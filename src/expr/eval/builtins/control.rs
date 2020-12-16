use crate::expr::eval::{eval, Error};

def_cont! {
    pub(crate) fn assert(e, cond: bool, body: thunk) {
        if !cond {
            return Err(Error::AssertionFailed);
        }
        e.push(body);
        e.cont(eval);
        Ok(())
    }

    pub(crate) fn if_then_else(e, cond: bool, then_body: thunk, else_body: thunk) {
        e.push(if cond { then_body } else { else_body });
        e.cont(eval);
        Ok(())
    }

    pub(crate) fn select_or_default(e, set: set, name: string, or_default: thunk) {
        match set.get(&*name) {
            Some(v) => e.push(v.clone()),
            None => e.push(or_default),
        }
        e.cont(eval);
        Ok(())
    }
}
