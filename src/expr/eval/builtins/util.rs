use crate::expr::eval::Value;

def_cont! {
    pub(crate) fn true_(_e) {
        unreachable!("Already translated");
    }

    pub(crate) fn false_(_e) {
        unreachable!("Already translated");
    }

    pub(crate) fn builtins(e) {
        e.push(e.ctx.builtins.clone());
        Ok(())
    }

    pub(crate) fn type_of(e, v: any) {
        e.push_value(Value::String(v.type_name().into()));
        Ok(())
    }
}
