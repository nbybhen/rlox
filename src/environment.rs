use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::{
    interpreter::{Error, Object},
    token::Token,
};

#[derive(Debug, Clone, PartialEq)]
pub struct Environment {
    pub values: RefCell<HashMap<String, Object>>,
    pub enclosing: Option<Rc<Environment>>,
}

impl std::fmt::Display for Environment {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        if let Some(enc) = &self.enclosing {
            let _ = write!(f, "Environment values: ");
            for (key, _) in self.values.borrow().iter() {
                let _ = write!(f, "{} -> <value>, ", key);
            }
            let _ = write!(f, "enclosing: {}", enc);
            Ok(())
        } else {
            write!(f, "Environment values: ")?;
            for (key, _) in self.values.borrow().iter() {
                let _ = write!(f, "{} -> <value>, ", key);
            }
            Ok(())
        }
    }
}

impl Environment {
    pub fn new(env: Option<Rc<Environment>>) -> Environment {
        Environment {
            values: RefCell::new(HashMap::new()),
            enclosing: env,
        }
    }

    // Writes a variable into the global environment
    pub fn define(&self, name: String, value: Object) {
        self.values.borrow_mut().insert(name, value);
    }

    // Gets a variable from the global environment
    pub fn get(&self, name: Token) -> Result<Object, Error> {
        if self.values.borrow().contains_key(&name.lexeme) {
            return Ok(self.values.borrow().get(&name.lexeme).unwrap().clone());
        }
        if self.enclosing.is_some() {
            return self.enclosing.as_ref().unwrap().get(name);
        }

        Err(Error::Error(
            name.clone(),
            format!("Undefined variable: {name}"),
        ))
    }

    // Reassigns a variable within the global environment if it exists
    pub fn assign(&self, name: &Token, value: Object) -> Result<(), String> {
        if self.values.borrow().contains_key(&name.lexeme) {
            self.values.borrow_mut().insert(name.lexeme.clone(), value);
            return Ok(());
        }
        if self.enclosing.is_some() {
            return self
                .enclosing
                .as_ref()
                .expect("Enclosing was wrong type")
                .assign(name, value);
        }

        Err(String::from("Undefined variable."))
    }

    pub fn assign_at(&self, dist: usize, name: &Token, value: Object) {
        self.ancestor(dist)
            .values
            .borrow_mut()
            .insert(name.lexeme.clone(), value);
    }

    fn ancestor(&self, dist: usize) -> &Environment {
        let mut env = self;
        for _ in 0..dist {
            if let Some(inner) = env.enclosing.as_ref() {
                env = inner;
            }
        }

        env
    }

    pub fn get_at(&self, dist: usize, name: String) -> Object {
        // println!("Current Environment is {}", self);
        // println!("looking for {} at {}", name, dist);
        self.ancestor(dist)
            .values
            .borrow()
            .get(&name)
            .unwrap()
            .clone()
    }
}
