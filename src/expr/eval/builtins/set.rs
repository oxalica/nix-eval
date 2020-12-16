use crate::expr::eval::{eval, Error};

def_cont! {
    pub(crate) fn get_attr(e, name: string, set: set) {
        match set.get(&*name) {
            Some(v) => {
                e.push(v.clone());
                e.cont(eval);
                Ok(())
            }
            None => Err(Error::BuiltinError { reason: format!("Missing attribute: {:?}", name).into() }),
        }
    }
}
