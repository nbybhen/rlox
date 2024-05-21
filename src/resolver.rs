use crate::interpreter::Interpreter;

#[allow(dead_code)]
pub struct Resolver<'a> {
    interpreter: &'a mut Interpreter
}

#[allow(dead_code)]
impl<'a> Resolver<'a> {
    fn new(interpreter: &mut Interpreter) -> Resolver {
        Resolver {interpreter}
    }
}
