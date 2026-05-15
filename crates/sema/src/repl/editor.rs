use reedline::{
    default_emacs_keybindings, ColumnarMenu, Emacs, KeyCode, KeyModifiers, MenuBuilder, Reedline,
    ReedlineEvent, ReedlineMenu,
};

use super::completer::SemaCompleter;
use super::highlighter::SemaHighlighter;
use super::hinter::SemaHinter;
use super::history::build_history;
use super::validator::SemaValidator;

const COMPLETION_MENU_NAME: &str = "completion_menu";

/// Build a fully-configured `Reedline` editor for the Sema REPL.
pub fn build() -> Reedline {
    let history = build_history();
    let completer = Box::new(SemaCompleter::new());
    let validator = Box::new(SemaValidator);
    let highlighter = Box::new(SemaHighlighter::new());
    let hinter = Box::new(SemaHinter::new());

    let completion_menu = Box::new(
        ColumnarMenu::default()
            .with_name(COMPLETION_MENU_NAME)
            .with_columns(4)
            .with_column_width(None)
            .with_column_padding(2),
    );

    let mut keybindings = default_emacs_keybindings();
    // Tab opens (or navigates) the columnar completion menu, matching the
    // muscle memory from the old rustyline REPL.
    keybindings.add_binding(
        KeyModifiers::NONE,
        KeyCode::Tab,
        ReedlineEvent::UntilFound(vec![
            ReedlineEvent::Menu(COMPLETION_MENU_NAME.to_string()),
            ReedlineEvent::MenuNext,
        ]),
    );

    let edit_mode = Box::new(Emacs::new(keybindings));

    Reedline::create()
        .with_history(history)
        .with_completer(completer)
        .with_validator(validator)
        .with_highlighter(highlighter)
        .with_hinter(hinter)
        .with_menu(ReedlineMenu::EngineCompleter(completion_menu))
        .with_edit_mode(edit_mode)
        .with_quick_completions(false)
        .with_partial_completions(true)
}
