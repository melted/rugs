use std::collections::HashMap;

use crate::ast::Module;

pub struct Environment {
    modules: HashMap<String, Module>,
    current_module: String,
    resolved: Module,
}
