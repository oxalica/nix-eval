use super::{Error, Evaluator, Result, SmolStr, Thunk, Value};
use crate::expr::Builtin;
use std::collections::BTreeMap;
use std::sync::Arc;

pub fn invoke(e: &Evaluator, b: Builtin, args: &[Arc<Thunk>]) -> Result<Value> {
    let err = Err(Error::BuiltinNotImplemented { builtin: b });
    let f = match b {
        Builtin::Abort => abort,
        Builtin::Add => add,
        Builtin::All => return err,
        Builtin::Any => return err,
        Builtin::AttrNames => return err,
        Builtin::AttrValues => return err,
        Builtin::BaseNameOf => return err,
        Builtin::BitAnd => return err,
        Builtin::BitOr => return err,
        Builtin::BitXor => return err,
        // FIXME: No clone.
        Builtin::Builtins => return Ok(e.builtins.eval(e)?.clone()),
        Builtin::CompareVersions => return err,
        Builtin::ConcatLists => return err,
        Builtin::ConcatStringsSep => return err,
        Builtin::CurrentSystem => return err,
        Builtin::DeepSeq => return err,
        Builtin::Derivation => return err,
        Builtin::DirOf => return err,
        Builtin::Div => div,
        Builtin::Elem => return err,
        Builtin::ElemAt => elem_at,
        Builtin::False => return Ok(Value::Bool(false)),
        Builtin::FetchGit => return err,
        Builtin::FetchTarball => return err,
        Builtin::Fetchurl => return err,
        Builtin::Filter => return err,
        Builtin::FilterSource => return err,
        Builtin::Foldl_ => return err,
        Builtin::FromJSON => return err,
        Builtin::FunctionArgs => return err,
        Builtin::GenList => return err,
        Builtin::GetAttr => return err,
        Builtin::GetEnv => return err,
        Builtin::HasAttr => return err,
        Builtin::HashFile => return err,
        Builtin::HashString => return err,
        Builtin::Head => head,
        Builtin::Import => return err,
        Builtin::IntersectAttrs => return err,
        Builtin::IsAttrs => return err,
        Builtin::IsBool => return err,
        Builtin::IsFloat => return err,
        Builtin::IsFunction => return err,
        Builtin::IsInt => return err,
        Builtin::IsList => return err,
        Builtin::IsNull => return err,
        Builtin::IsPath => return err,
        Builtin::IsString => return err,
        Builtin::Length => length,
        Builtin::LessThan => less_than,
        Builtin::ListToAttrs => return err,
        Builtin::Map => return err,
        Builtin::Match => return err,
        Builtin::Mul => mul,
        Builtin::ParseDrvName => return err,
        Builtin::Path => return err,
        Builtin::PathExists => return err,
        Builtin::Placeholder => return err,
        Builtin::ReadDir => return err,
        Builtin::ReadFile => return err,
        Builtin::RemoveAttrs => return err,
        Builtin::ReplaceStrings => return err,
        Builtin::Seq => return err,
        Builtin::Sort => return err,
        Builtin::Split => return err,
        Builtin::SplitVersion => return err,
        Builtin::StringLength => return err,
        Builtin::Sub => sub,
        Builtin::SubString => return err,
        Builtin::Tail => return err,
        Builtin::Throw => throw,
        Builtin::ToFile => return err,
        Builtin::ToJSON => return err,
        Builtin::ToPath => return err,
        Builtin::ToString => return err,
        Builtin::ToXML => return err,
        Builtin::Trace => return err,
        Builtin::True => return Ok(Value::Bool(true)),
        Builtin::TryEval => try_eval,
        Builtin::TypeOf => type_of,
    };
    f(e, args)
}

enum ArithArgs {
    Int(i64, i64),
    Float(f64, f64),
    String(SmolStr, SmolStr),
}

fn _arith_args(
    operation: &'static str,
    allow_str: bool,
    a: &Value,
    b: &Value,
) -> Result<ArithArgs> {
    Ok(match (a, b) {
        (Value::Int(a), Value::Int(b)) => ArithArgs::Int(*a, *b),
        (Value::Float(a), Value::Int(b)) => ArithArgs::Float(*a, *b as f64),
        (Value::Int(a), Value::Float(b)) => ArithArgs::Float(*a as f64, *b),
        (Value::Float(a), Value::Float(b)) => ArithArgs::Float(*a, *b),
        (Value::String(a), Value::String(b)) if allow_str => {
            ArithArgs::String(a.clone(), b.clone())
        }
        _ => {
            return Err(Error::BinOpTypeError {
                operation,
                lhs: a.type_name(),
                rhs: b.type_name(),
            })
        }
    })
}

pub fn _arith_op(op: Builtin, a: &Value, b: &Value) -> Result<Value> {
    Ok(match op {
        Builtin::Add => match _arith_args("add", true, a, b)? {
            ArithArgs::Int(a, b) => Value::Int(a.wrapping_add(b)),
            ArithArgs::Float(a, b) => Value::Float(a + b),
            ArithArgs::String(a, b) => Value::String((a.to_string() + &*b).into()),
        },
        Builtin::Sub => match _arith_args("subtract", false, a, b)? {
            ArithArgs::Int(a, b) => Value::Int(a.wrapping_sub(b)),
            ArithArgs::Float(a, b) => Value::Float(a - b),
            _ => unreachable!(),
        },
        Builtin::Mul => match _arith_args("multiply", false, a, b)? {
            ArithArgs::Int(a, b) => Value::Int(a.wrapping_mul(b)),
            ArithArgs::Float(a, b) => Value::Float(a * b),
            _ => unreachable!(),
        },
        Builtin::Div => match _arith_args("divide", false, a, b)? {
            ArithArgs::Int(_, b) if b == 0 => return Err(Error::DivisionByZero),
            ArithArgs::Int(a, b) => Value::Int(a.checked_div(b).ok_or(Error::DivisionOverflow)?),
            ArithArgs::Float(a, b) => Value::Float(a / b),
            _ => unreachable!(),
        },
        Builtin::LessThan => Value::Bool(match _arith_args("compare", true, a, b)? {
            ArithArgs::Int(a, b) => a < b,
            ArithArgs::Float(a, b) => a < b,
            ArithArgs::String(a, b) => a < b,
        }),
        _ => unreachable!(),
    })
}

fn abort(e: &Evaluator, args: &[Arc<Thunk>]) -> Result<Value> {
    let v = args[0].eval(e)?;
    let reason = e.eval_coerce_to_string(v)?;
    Err(Error::Abort { reason })
}

fn add(e: &Evaluator, args: &[Arc<Thunk>]) -> Result<Value> {
    _arith_op(Builtin::Add, args[0].eval(e)?, args[1].eval(e)?)
}

fn div(e: &Evaluator, args: &[Arc<Thunk>]) -> Result<Value> {
    _arith_op(Builtin::Div, args[0].eval(e)?, args[1].eval(e)?)
}

fn elem_at(e: &Evaluator, args: &[Arc<Thunk>]) -> Result<Value> {
    let xs = args[0].eval(e)?.as_list()?;
    let idx = args[1].eval(e)?.as_int()?;
    if 0 <= idx && idx < xs.len() as i64 {
        // FIXME: No clone?
        Ok(xs[idx as usize].eval(e)?.clone())
    } else {
        Err(Error::BuiltinError {
            reason: format!("List index {} out of bound (length is {})", idx, xs.len()).into(),
        })
    }
}

fn head(e: &Evaluator, args: &[Arc<Thunk>]) -> Result<Value> {
    let args = [args[0].clone(), Thunk::new_value(Value::Int(0))];
    elem_at(e, &args)
}

fn length(e: &Evaluator, args: &[Arc<Thunk>]) -> Result<Value> {
    let xs = args[0].eval(e)?.as_list()?;
    Ok(Value::Int(xs.len() as i64))
}

fn less_than(e: &Evaluator, args: &[Arc<Thunk>]) -> Result<Value> {
    _arith_op(Builtin::LessThan, args[0].eval(e)?, args[1].eval(e)?)
}

fn mul(e: &Evaluator, args: &[Arc<Thunk>]) -> Result<Value> {
    _arith_op(Builtin::Mul, args[0].eval(e)?, args[1].eval(e)?)
}

fn sub(e: &Evaluator, args: &[Arc<Thunk>]) -> Result<Value> {
    _arith_op(Builtin::Sub, args[0].eval(e)?, args[1].eval(e)?)
}

fn throw(e: &Evaluator, args: &[Arc<Thunk>]) -> Result<Value> {
    let reason = e.eval_coerce_to_string(args[0].eval(e)?)?;
    Err(Error::Throw { reason })
}

fn try_eval(e: &Evaluator, args: &[Arc<Thunk>]) -> Result<Value> {
    let mut set = BTreeMap::new();
    match args[0].eval(e) {
        Ok(_) => {
            let t = Thunk::new_value(Value::Bool(true));
            set.insert("success".into(), t);
            set.insert("value".into(), args[0].clone());
            Ok(Value::AttrSet(set))
        }
        Err(err) if err.is_soft_error() => {
            let f = Thunk::new_value(Value::Bool(false));
            set.insert("success".into(), f.clone());
            set.insert("value".into(), f);
            Ok(Value::AttrSet(set))
        }
        Err(err) => Err(err),
    }
}

fn type_of(e: &Evaluator, args: &[Arc<Thunk>]) -> Result<Value> {
    let v = args[0].eval(e)?;
    Ok(Value::String(v.type_name().into()))
}
