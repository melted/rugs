use std::collections::HashSet;

use crate::ast::{varid, Identifier};

struct NameGen {
    prefix: String,
    current: usize,
}

impl NameGen {
    fn new(prefix: &str) -> Self {
        NameGen {
            prefix: prefix.to_string(),
            current: 0,
        }
    }
}

impl Iterator for NameGen {
    type Item = Identifier;

    fn next(&mut self) -> Option<Self::Item> {
        let name = format!("{}{}", self.prefix, self.current);
        self.current += 1;
        Some(varid(&name))
    }
}

pub fn generate_fresh_name(prefix: &str, avoid: HashSet<Identifier>) -> Identifier {
    let gen = NameGen::new(prefix);
    for id in gen {
        if !avoid.contains(&id) {
            return id;
        }
    }
    panic!("This can't happen!");
}
