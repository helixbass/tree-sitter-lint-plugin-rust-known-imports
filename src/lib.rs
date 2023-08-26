#![allow(non_upper_case_globals)]

use tree_sitter_lint::Plugin;

mod kind;
mod rules;

use rules::known_imports_rule;

pub type ProvidedTypes<'a> = ();

pub fn instantiate() -> Plugin {
    Plugin {
        name: "rust-known-imports".to_owned(),
        rules: vec![known_imports_rule()],
    }
}
