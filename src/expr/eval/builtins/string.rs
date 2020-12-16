use crate::expr::eval::{Thunk, Value};
use either::Either;

def_cont! {
    pub(crate) fn concat_str(e, a: (to_string), b: to_string) {
        let mut a = a;
        let s = match a.unwrap_value()? {
            Either::Left(a) => a.into_string()? + b,
            Either::Right(a) => {
                let a = a.as_string()?;
                let mut s = String::with_capacity(a.len() + b.len());
                s.push_str(a);
                s.push_str(b);
                s
            }
        };
        e.push(Thunk::new_value(Value::String(s.into())));
        Ok(())
    }
}
