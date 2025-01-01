use crate::{
    val::{functions::NativeFunction, Val},
    vm::{Vm, VmResult},
};

pub fn register_builtins(vm: &mut Vm) -> &mut Vm {
    vm.register_native_function(NativeFunction::with_args("+", plus_fn))
        .register_native_function(NativeFunction::new("define", define_fn))
}

fn plus_fn(args: &[Val]) -> VmResult<Val> {
    let mut int_sum = 0;
    let mut float_sum = 0.0;
    for arg in args {
        match arg {
            Val::Int(x) => int_sum += *x,
            Val::Float(x) => float_sum += *x,
            v => todo!("{v:?} not handled in + operator"),
        }
    }
    let res = if float_sum == 0.0 {
        Val::Int(int_sum)
    } else {
        Val::Float(float_sum + int_sum as f64)
    };
    Ok(res)
}

fn define_fn(vm: &mut Vm) -> VmResult<Val> {
    let (sym, val) = match vm.args() {
        [Val::Symbol(sym), val] => (sym.clone(), val.clone()),
        _ => todo!(),
    };
    vm.globals.values.insert(sym, val);
    Ok(Val::Void)
}
