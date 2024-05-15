use crate::interpreter::Interpreter;

pub struct Resolver<'a> {
    interpreter: &'a mut Interpreter
}

impl<'a> Resolver<'a> {
    fn new(interpreter: &mut Interpreter) -> Resolver {
        Resolver {interpreter}
    }
}