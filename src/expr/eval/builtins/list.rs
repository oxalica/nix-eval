use crate::expr::eval::{eval, Error, Value};

def_cont! {
    pub(crate) fn length(e, xs: list) {
        e.push_value(Value::Int(xs.len() as i64));
        Ok(())
    }

    pub(crate) fn head(e, xs: list) {
        if xs.is_empty() {
            return Err(Error::BuiltinError { reason: "List is empty".into() });
        }
        e.push(xs[0].clone());
        e.cont(eval);
        Ok(())
    }

    pub(crate) fn elem_at(e, xs: list, idx: int) {
        if idx < 0 || xs.len() as i64 <= idx {
            return Err(Error::BuiltinError { reason: format!("Index out of bound: {}", idx).into() });
        }
        e.push(xs[idx as usize].clone());
        e.cont(eval);
        Ok(())
    }
}
