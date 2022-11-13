use std::{
    collections::HashMap,
    fs::File,
    io::{stdin, Read},
};

#[derive(Debug)]
pub struct Session {
    pub source: HashMap<String, String>,
}

impl Session {
    pub fn new() -> Session {
        Session {
            source: HashMap::new(),
        }
    }

    pub fn load(&mut self, files: Vec<String>) -> std::io::Result<()> {
        for path in files.iter() {
            let mut file = File::open(path)?;
            let mut code = String::new();
            file.read_to_string(&mut code)?;
            self.source.insert(path.clone(), code);
        }
        Ok(())
    }

    pub fn load_stdin(&mut self) -> std::io::Result<()> {
        let mut input = stdin().lock();
        let mut code = String::new();
        input.read_to_string(&mut code)?;
        self.source.insert("stdin".to_string(), code);
        Ok(())
    }
}
