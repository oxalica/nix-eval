use crate::expr::eval::{eval, Value};

def_cont! {
    pub(crate) fn and(e, a: bool, b: thunk) {
        if a {
            e.push(b);
            e.cont(|e| {
                let _ = e.get(0).unwrap_value_ref()?.as_bool()?;
                Ok(())
            });
            e.cont(eval);
        } else {
            e.push_value(Value::Bool(false));
        }
        Ok(())
    }

    pub(crate) fn or(e, a: bool, b: thunk) {
        if a {
            e.push_value(Value::Bool(true));
        } else {
            e.push(b);
            e.cont(|e| {
                let _ = e.get(0).unwrap_value_ref()?.as_bool()?;
                Ok(())
            });
            e.cont(eval);
        }
        Ok(())
    }

    pub(crate) fn concat(e, a: list, b: list) {
        let mut v = a.to_vec();
        v.extend_from_slice(b);
        e.push_value(Value::List(v));
        Ok(())
    }

    pub(crate) fn equal(e, a: any, b: any) {
        let b = match (a, b) {
            (Value::Bool(a), Value::Bool(b)) => a == b,
            (Value::Float(a), Value::Float(b)) => a == b,
            (Value::Int(a), Value::Int(b)) => a == b,
            (Value::String(a), Value::String(b)) |
            (Value::Path(a), Value::Path(b)) => a == b,
            (Value::AttrSet(a), Value::AttrSet(b)) if a.keys().eq(b.keys()) => {
                todo!();
            }
            (Value::List(a), Value::List(b)) if a.len() == b.len() => {
                todo!();
            }
            _ => false,
        };
        e.push_value(Value::Bool(b));
        Ok(())
    }
}
