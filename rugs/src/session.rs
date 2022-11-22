use std::{
    collections::HashMap,
    fs::File,
    io::{stdin, Read}, rc::Rc,
};

#[derive(Debug, Clone)]
pub struct Session {
    pub source: HashMap<String, Rc<String>>,
    pub options: Options
}

impl Session {
    pub fn new() -> Session {
        Session {
            source: HashMap::new(),
            options: Options::new()
        }
    }

    pub fn load(&mut self, files: Vec<String>) -> std::io::Result<()> {
        for path in files.iter() {
            let mut file = File::open(path)?;
            let mut code = String::new();
            file.read_to_string(&mut code)?;
            self.source.insert(path.clone(), Rc::from(code));
        }
        Ok(())
    }

    pub fn load_stdin(&mut self) -> std::io::Result<()> {
        let mut input = stdin().lock();
        let mut code = String::new();
        input.read_to_string(&mut code)?;
        self.source.insert("stdin".to_string(), Rc::from(code)) ;
        Ok(())
    }
}

impl  Default for Session {
    fn default() -> Self {
        Session::new()
    }
}


#[derive(Debug, PartialEq, Clone)]
pub struct Options {

}

impl Options {
    fn new() -> Options {
        Options {  }
    }
}