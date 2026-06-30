//! `,apropos PATTERN` — find Sema bindings whose name matches a pattern.

use std::collections::BTreeMap;

use sema_core::Env;

use crate::docs;

pub fn run(env: &Env, pattern: &str) {
    let pattern = pattern.trim();
    if pattern.is_empty() {
        println!("Usage: ,apropos <pattern>");
        return;
    }

    let mut summaries: BTreeMap<String, String> = docs::doc_name_summaries(
        collect_env_names(env)
            .into_iter()
            .map(|name| {
                let summary = describe_env_name(env, &name);
                (name, summary)
            })
            .collect::<Vec<_>>(),
    );

    let hits = docs::search_name_summaries(pattern, &summaries);
    print!("{}", docs::render_apropos_hits(pattern, &hits));
    summaries.clear();
}

fn collect_env_names(env: &Env) -> Vec<String> {
    let mut names = Vec::new();
    env.iter_bindings(|spur, _| names.push(sema_core::resolve(spur)));
    if let Some(parent) = &env.parent {
        names.extend(collect_env_names(parent));
    }
    docs::dedupe_names(names)
}

fn describe_env_name(env: &Env, name: &str) -> String {
    let spur = sema_core::intern(name);
    match env.get(spur) {
        Some(val) if sema_vm::extract_vm_closure(&val).is_some() => "lambda".to_string(),
        Some(val) => val.type_name().to_string(),
        None => String::new(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn env_names_are_deduped_across_parent_chain() {
        let root = Env::new();
        root.set_str("alpha", sema_core::Value::int(1));
        let child = Env::with_parent(std::rc::Rc::new(root));
        child.set_str("alpha", sema_core::Value::int(2));
        let names = collect_env_names(&child);
        assert_eq!(names.iter().filter(|n| n.as_str() == "alpha").count(), 1);
    }
}
