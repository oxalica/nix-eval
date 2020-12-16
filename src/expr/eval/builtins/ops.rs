use crate::expr::eval::{eval, EvalState, Result, Value};
use std::ops::Bound;

def_cont! {
    pub(crate) fn not(e, a: bool) {
        e.push_value(Value::Bool(!a));
        Ok(())
    }

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

    pub(crate) fn add(e, a: any, b: any) {
        let a = a.as_int()?;
        let b = b.as_int()?;
        e.push_value(Value::Int(a.wrapping_add(b)));
        Ok(())
    }

    pub(crate) fn equal(e, a: (any), b: (any)) {
        let v = match (a.unwrap_value_ref()?, b.unwrap_value_ref()?) {
            (Value::Bool(a), Value::Bool(b)) => a == b,
            (Value::Float(a), Value::Float(b)) => a == b,
            (Value::Int(a), Value::Int(b)) => a == b,
            (Value::String(a), Value::String(b)) |
            (Value::Path(a), Value::Path(b)) => a == b,
            (Value::AttrSet(a_), Value::AttrSet(b_)) if a_.is_empty() && b_.is_empty() => true,
            (Value::List(a_), Value::List(b_)) if  a_.is_empty() && b_.is_empty() => true,
            (Value::AttrSet(a_), Value::AttrSet(b_)) if a_.keys().eq(b_.keys()) => {
                e.push(b.clone());
                e.push(a.clone());
                e.push_value(Value::String(a_.keys().next().unwrap().to_string()));
                e.cont(compare_set_next);
                e.push(b_.values().next().unwrap().clone());
                e.push(a_.values().next().unwrap().clone());
                e.cont(equal);
                return Ok(());
            }
            (Value::List(a_), Value::List(b_)) if a_.len() == b_.len() => {
                e.push(b.clone());
                e.push(a.clone());
                e.push_value(Value::Int(0));
                e.cont(compare_list_next);
                e.push(b_[0].clone());
                e.push(a_[0].clone());
                e.cont(equal);
                return Ok(());
            }
            _ => false,
        };
        e.push_value(Value::Bool(v));
        Ok(())
    }

    pub(crate) fn less_than(e, a: any, b: any) {
        e.push_value(Value::Bool(a.as_int()? < b.as_int()?));
        Ok(())
    }
}

// Stack: .. | b, a, last_key, last_cmp_result
fn compare_set_next(e: &mut EvalState<'_>) -> Result<()> {
    let last_ret = e.pop().unwrap_value_ref()?.as_bool()?;
    if !last_ret {
        e.pop_many(3);
        e.push_value(Value::Bool(false));
        return Ok(());
    }

    let last_key = e.pop();
    let last_key = last_key.unwrap_value_ref()?.as_string()?;
    let a = e.get(0).unwrap_value_ref()?.as_set()?;
    let (next_key, el1) = match a
        .range::<str, _>((Bound::Excluded(last_key), Bound::Unbounded))
        .next()
    {
        Some((key, el)) => (key.clone(), el.clone()),
        None => {
            e.pop_many(2);
            e.push_value(Value::Bool(true));
            return Ok(());
        }
    };
    let el2 = e.get(1).unwrap_value_ref()?.as_set()?[&*next_key].clone();
    e.push_value(Value::String(next_key.into()));
    e.cont(compare_set_next);
    e.push(el2);
    e.push(el1);
    e.cont(equal);
    Ok(())
}

// Stack: .. | b, a, last_idx, last_cmp_result
fn compare_list_next(e: &mut EvalState<'_>) -> Result<()> {
    let last_ret = e.pop().unwrap_value_ref()?.as_bool()?;
    if !last_ret {
        e.pop_many(3);
        e.push_value(Value::Bool(false));
        return Ok(());
    }

    let cur_idx = e.pop().unwrap_value_ref()?.as_int()? as usize + 1;
    let a = e.get(0).unwrap_value_ref()?.as_list()?;
    if a.len() == cur_idx {
        e.pop_many(2);
        e.push_value(Value::Bool(true));
        return Ok(());
    }
    let el1 = a[cur_idx].clone();
    let el2 = e.get(1).unwrap_value_ref()?.as_list()?[cur_idx].clone();
    e.push_value(Value::Int(cur_idx as i64));
    e.cont(compare_list_next);
    e.push(el2);
    e.push(el1);
    e.cont(equal);
    Ok(())
}
