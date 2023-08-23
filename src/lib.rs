use tree_sitter_lint::Plugin;

mod rules;

use rules::no_undef_rule;

pub type ProvidedTypes<'a> = ();

pub fn instantiate() -> Plugin {
    Plugin {
        name: "rust-known-imports".to_owned(),
        rules: vec![no_undef_rule()],
    }
}
