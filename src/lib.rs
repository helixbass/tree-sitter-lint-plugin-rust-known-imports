#![allow(non_upper_case_globals)]

use tree_sitter_lint::{Plugin, PluginBuilder, FromFileRunContextInstanceProviderFactory, instance_provider_factory};

mod kind;
mod rules;

use rules::known_imports_rule;
use tree_sitter_lint_plugin_rust_scope_analysis::ScopeAnalyzer;

pub type ProvidedTypes<'a> = (ScopeAnalyzer<'a>,);

pub fn instantiate() -> Plugin {
    PluginBuilder::default()
        .name("rust-known-imports")
        .rules(vec![known_imports_rule()])
        .build()
        .unwrap()
}

pub fn get_instance_provider_factory() -> Box<dyn FromFileRunContextInstanceProviderFactory> {
    Box::new(instance_provider_factory!(ProvidedTypes))
}
