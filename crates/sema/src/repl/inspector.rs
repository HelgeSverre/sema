//! `,inspect EXPR` — interactive arrow-key navigator for Sema values.
//!
//! Drops out of reedline, enters raw mode in the alternate screen, and
//! lets the user drill through maps / lists / vectors / closures with
//! the arrow keys. Esc / `q` / Ctrl-C returns to the REPL prompt with
//! the terminal restored.
//!
//! Sema values cannot form cycles (all containers hold immutable `Rc<T>`
//! contents), so traversal is unconditionally safe — no visited-set
//! needed.

use std::collections::BTreeMap;
use std::io::{self, Stdout, Write};

use crossterm::cursor::{Hide, MoveTo, Show};
use crossterm::event::{self, Event, KeyCode, KeyEvent, KeyEventKind, KeyModifiers};
use crossterm::style::{Color as CtColor, Print, ResetColor, SetForegroundColor};
use crossterm::terminal::{
    self, disable_raw_mode, enable_raw_mode, Clear, ClearType, EnterAlternateScreen,
    LeaveAlternateScreen,
};
use crossterm::{execute, queue};
use sema_core::{pretty_print, Value, ValueView};

/// Width given to the value pretty-print pane. Narrow on purpose so even
/// terminals around 60 cols read well.
const VALUE_PRETTY_WIDTH_DEFAULT: usize = 60;

/// Maximum visible characters of a child's preview value before clipping.
const CHILD_PREVIEW_WIDTH: usize = 48;

#[derive(Clone)]
enum PathSeg {
    /// `:key` style segment, displayed with a leading colon.
    MapKey(Value),
    /// `[index]` style segment, used for lists and vectors.
    Index(usize),
}

impl PathSeg {
    fn render(&self) -> String {
        match self {
            PathSeg::MapKey(k) => format!("{k}"),
            PathSeg::Index(i) => format!("[{i}]"),
        }
    }
}

struct InspectorState {
    root: Value,
    path: Vec<PathSeg>,
    selected: usize,
    scroll: usize,
}

impl InspectorState {
    fn new(root: Value) -> Self {
        Self {
            root,
            path: Vec::new(),
            selected: 0,
            scroll: 0,
        }
    }

    /// Resolve the current focus by walking `path` from `root`. Returns
    /// `None` if the path no longer makes sense (shouldn't happen given
    /// Sema's immutable values, but kept for paranoia).
    fn focus(&self) -> Option<Value> {
        let mut cur = self.root.clone();
        for seg in &self.path {
            cur = match seg {
                PathSeg::MapKey(k) => descend_map(&cur, k)?,
                PathSeg::Index(i) => descend_index(&cur, *i)?,
            };
        }
        Some(cur)
    }
}

/// Public entry point — show the inspector for `val`. Returns when the
/// user exits. The label is shown in the title bar.
pub fn run(val: Value, label: &str) -> io::Result<()> {
    let mut stdout = io::stdout();
    enable_raw_mode()?;
    execute!(stdout, EnterAlternateScreen, Hide)?;

    let result = drive(&mut stdout, val, label);

    // Always restore terminal state, even on error.
    let _ = execute!(stdout, LeaveAlternateScreen, Show, ResetColor);
    let _ = disable_raw_mode();
    result
}

fn drive(stdout: &mut Stdout, val: Value, label: &str) -> io::Result<()> {
    let mut state = InspectorState::new(val);

    loop {
        let (cols, rows) = terminal::size()?;
        render(stdout, &state, label, cols, rows)?;

        let Event::Key(key) = event::read()? else {
            continue;
        };
        if !matches!(key.kind, KeyEventKind::Press | KeyEventKind::Repeat) {
            continue;
        }

        match handle_key(key, &mut state, rows) {
            Action::Continue => {}
            Action::Exit => return Ok(()),
        }
    }
}

enum Action {
    Continue,
    Exit,
}

fn handle_key(key: KeyEvent, state: &mut InspectorState, rows: u16) -> Action {
    // Ctrl-C always exits, regardless of context.
    if key.modifiers.contains(KeyModifiers::CONTROL) && matches!(key.code, KeyCode::Char('c')) {
        return Action::Exit;
    }

    let children = collect_children(state.focus().as_ref());
    let child_count = children.len();
    let visible = visible_rows(rows);

    match key.code {
        KeyCode::Char('q') => return Action::Exit,
        KeyCode::Esc | KeyCode::Backspace | KeyCode::Left | KeyCode::Char('h') => {
            if state.path.pop().is_some() {
                state.selected = 0;
                state.scroll = 0;
            } else {
                return Action::Exit;
            }
        }
        KeyCode::Enter | KeyCode::Right | KeyCode::Char('l') => {
            if child_count > 0 {
                if let Some(child) = children.get(state.selected) {
                    state.path.push(child.path_segment.clone());
                    state.selected = 0;
                    state.scroll = 0;
                }
            }
        }
        KeyCode::Up | KeyCode::Char('k') => {
            if state.selected > 0 {
                state.selected -= 1;
                if state.selected < state.scroll {
                    state.scroll = state.selected;
                }
            }
        }
        KeyCode::Down | KeyCode::Char('j') => {
            if state.selected + 1 < child_count {
                state.selected += 1;
                if state.selected >= state.scroll + visible {
                    state.scroll = state.selected + 1 - visible;
                }
            }
        }
        KeyCode::Home | KeyCode::Char('g') => {
            state.selected = 0;
            state.scroll = 0;
        }
        KeyCode::End | KeyCode::Char('G') => {
            if child_count > 0 {
                state.selected = child_count - 1;
                state.scroll = state.selected.saturating_sub(visible.saturating_sub(1));
            }
        }
        _ => {}
    }

    Action::Continue
}

fn render(
    stdout: &mut Stdout,
    state: &InspectorState,
    label: &str,
    cols: u16,
    rows: u16,
) -> io::Result<()> {
    queue!(stdout, Clear(ClearType::All), MoveTo(0, 0))?;

    // Title bar.
    queue!(
        stdout,
        SetForegroundColor(CtColor::Cyan),
        Print(format!("Inspector — {label}")),
        ResetColor,
    )?;

    // Breadcrumb on the next line.
    let crumb = render_breadcrumb(&state.path);
    queue!(stdout, MoveTo(0, 1), Print(format!("Path: {crumb}")))?;

    // Pretty-printed current value, clipped to a few lines.
    let focus = state.focus().unwrap_or(Value::nil());
    let value_width = (cols as usize)
        .saturating_sub(2)
        .min(VALUE_PRETTY_WIDTH_DEFAULT.max(40));
    let pretty = pretty_print(&focus, value_width);
    let mut row: u16 = 3;
    let value_lines_budget: usize = 6;
    for (printed, line) in pretty.lines().enumerate() {
        if printed >= value_lines_budget {
            queue!(stdout, MoveTo(0, row), Print("..."))?;
            row += 1;
            break;
        }
        queue!(stdout, MoveTo(0, row), Print(line))?;
        row += 1;
    }

    // Separator + children list.
    row = row.saturating_add(1);
    let children = collect_children(Some(&focus));

    if children.is_empty() {
        queue!(
            stdout,
            MoveTo(0, row),
            SetForegroundColor(CtColor::DarkGrey),
            Print("(leaf — no children)"),
            ResetColor,
        )?;
    } else {
        let visible = visible_rows(rows);
        let end = (state.scroll + visible).min(children.len());
        let label_width = children
            .iter()
            .skip(state.scroll)
            .take(end - state.scroll)
            .map(|c| c.label.chars().count())
            .max()
            .unwrap_or(0)
            .min(24);

        for (offset, child) in children[state.scroll..end].iter().enumerate() {
            let idx = state.scroll + offset;
            let cursor = if idx == state.selected { ">" } else { " " };
            let line = format!(
                "{cursor} {:width$}  {}",
                child.label,
                clip(&child.preview, CHILD_PREVIEW_WIDTH),
                width = label_width,
            );
            queue!(stdout, MoveTo(0, row), Print(line))?;
            row += 1;
        }

        if state.scroll + visible < children.len() {
            let remaining = children.len() - end;
            queue!(
                stdout,
                MoveTo(0, row),
                SetForegroundColor(CtColor::DarkGrey),
                Print(format!("  ... {remaining} more")),
                ResetColor,
            )?;
        }
    }

    // Help bar pinned to the last row.
    let help_row = rows.saturating_sub(1);
    queue!(
        stdout,
        MoveTo(0, help_row),
        SetForegroundColor(CtColor::DarkGrey),
        Print("[↑/↓] move  [→/Enter] descend  [←/Esc] back  [q] quit"),
        ResetColor,
    )?;

    stdout.flush()
}

/// Compute how many child rows we can show. Reserve rows for the title
/// (1), breadcrumb (1), blank (1), value pretty-print (up to 6), blank
/// (1), help bar (1) — round numbers are easier than tracking exactly,
/// so we just subtract 12 and saturate.
fn visible_rows(rows: u16) -> usize {
    (rows as usize).saturating_sub(12).max(3)
}

fn render_breadcrumb(path: &[PathSeg]) -> String {
    if path.is_empty() {
        return ".".to_string();
    }
    let mut s = String::from(".");
    for seg in path {
        s.push_str(" > ");
        s.push_str(&seg.render());
    }
    s
}

fn clip(s: &str, max: usize) -> String {
    let mut out = String::new();
    for (count, ch) in s.chars().enumerate() {
        if count + 1 > max {
            out.push('…');
            return out;
        }
        out.push(ch);
    }
    out
}

struct Child {
    label: String,
    preview: String,
    path_segment: PathSeg,
}

/// Enumerate the children of the focused value. Returns an empty Vec for
/// leaves (numbers, strings, native fns, etc.).
fn collect_children(val: Option<&Value>) -> Vec<Child> {
    let Some(val) = val else {
        return vec![];
    };
    match val.view() {
        ValueView::List(items) => items_to_children(items.as_ref()),
        ValueView::Vector(items) => items_to_children(items.as_ref()),
        ValueView::Map(map) => map_to_children(map.iter().map(|(k, v)| (k.clone(), v.clone()))),
        ValueView::HashMap(map) => {
            // Sort by string-representation for deterministic display,
            // matching what pretty_print does.
            let mut entries: BTreeMap<String, (Value, Value)> = BTreeMap::new();
            for (k, v) in map.iter() {
                entries.insert(format!("{k}"), (k.clone(), v.clone()));
            }
            map_to_children(entries.into_values())
        }
        _ => vec![],
    }
}

fn items_to_children(items: &[Value]) -> Vec<Child> {
    items
        .iter()
        .enumerate()
        .map(|(i, v)| Child {
            label: format!("[{i}]"),
            preview: short_preview(v),
            path_segment: PathSeg::Index(i),
        })
        .collect()
}

fn map_to_children<I>(iter: I) -> Vec<Child>
where
    I: IntoIterator<Item = (Value, Value)>,
{
    iter.into_iter()
        .map(|(k, v)| Child {
            label: format!("{k}"),
            preview: short_preview(&v),
            path_segment: PathSeg::MapKey(k),
        })
        .collect()
}

fn descend_map(parent: &Value, key: &Value) -> Option<Value> {
    match parent.view() {
        ValueView::Map(m) => m.get(key).cloned(),
        ValueView::HashMap(m) => m.get(key).cloned(),
        _ => None,
    }
}

fn descend_index(parent: &Value, idx: usize) -> Option<Value> {
    match parent.view() {
        ValueView::List(items) => items.get(idx).cloned(),
        ValueView::Vector(items) => items.get(idx).cloned(),
        _ => None,
    }
}

fn short_preview(v: &Value) -> String {
    // Use the compact pretty-print at a small width so containers show
    // their type + length rather than dumping inline.
    match v.view() {
        ValueView::List(items) => format!("({} items)", items.len()),
        ValueView::Vector(items) => format!("[{} items]", items.len()),
        ValueView::Map(m) => format!("{{{} entries}}", m.len()),
        ValueView::HashMap(m) => format!("#{{{} entries}}", m.len()),
        ValueView::Lambda(_) => "<lambda>".to_string(),
        _ if sema_vm::extract_vm_closure(v).is_some() => "<closure>".to_string(),
        _ => format!("{v}"),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crossterm::event::KeyEvent;
    use sema_core::{intern, Value};
    use std::collections::BTreeMap;

    fn make_map(pairs: &[(&str, i64)]) -> Value {
        let mut m = BTreeMap::new();
        for (k, v) in pairs {
            m.insert(Value::keyword(k), Value::int(*v));
        }
        Value::map(m)
    }

    #[test]
    fn focus_returns_root_when_path_empty() {
        let state = InspectorState::new(Value::int(42));
        assert_eq!(state.focus().unwrap(), Value::int(42));
    }

    #[test]
    fn descend_into_map_key() {
        let mut state = InspectorState::new(make_map(&[("a", 1), ("b", 2)]));
        state.path.push(PathSeg::MapKey(Value::keyword("b")));
        assert_eq!(state.focus().unwrap(), Value::int(2));
    }

    #[test]
    fn descend_into_list_index() {
        let mut state = InspectorState::new(Value::list(vec![
            Value::int(10),
            Value::int(20),
            Value::int(30),
        ]));
        state.path.push(PathSeg::Index(1));
        assert_eq!(state.focus().unwrap(), Value::int(20));
    }

    #[test]
    fn children_of_leaf_is_empty() {
        let leaf = Value::int(7);
        assert!(collect_children(Some(&leaf)).is_empty());
    }

    #[test]
    fn children_of_list_enumerated() {
        let list = Value::list(vec![Value::int(1), Value::int(2)]);
        let kids = collect_children(Some(&list));
        assert_eq!(kids.len(), 2);
        assert_eq!(kids[0].label, "[0]");
        assert_eq!(kids[1].label, "[1]");
    }

    #[test]
    fn down_key_advances_selection() {
        let list = Value::list(vec![Value::int(1), Value::int(2), Value::int(3)]);
        let mut state = InspectorState::new(list);
        let _ = handle_key(
            KeyEvent::new(KeyCode::Down, KeyModifiers::NONE),
            &mut state,
            80,
        );
        assert_eq!(state.selected, 1);
    }

    #[test]
    fn esc_at_root_exits() {
        let mut state = InspectorState::new(Value::int(1));
        let action = handle_key(
            KeyEvent::new(KeyCode::Esc, KeyModifiers::NONE),
            &mut state,
            80,
        );
        assert!(matches!(action, Action::Exit));
        // We didn't intern anything that would leak, but keep this here so
        // an unused-import lint doesn't bite if intern is dropped later.
        let _ = intern("placeholder");
    }

    #[test]
    fn esc_with_path_pops() {
        let mut state = InspectorState::new(Value::list(vec![Value::int(1)]));
        state.path.push(PathSeg::Index(0));
        let action = handle_key(
            KeyEvent::new(KeyCode::Esc, KeyModifiers::NONE),
            &mut state,
            80,
        );
        assert!(matches!(action, Action::Continue));
        assert!(state.path.is_empty());
    }

    #[test]
    fn enter_descends_into_selected_child() {
        let list = Value::list(vec![Value::int(10), Value::int(20)]);
        let mut state = InspectorState::new(list);
        state.selected = 1;
        let _ = handle_key(
            KeyEvent::new(KeyCode::Enter, KeyModifiers::NONE),
            &mut state,
            80,
        );
        assert_eq!(state.path.len(), 1);
        assert_eq!(state.focus().unwrap(), Value::int(20));
    }
}
