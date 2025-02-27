use std::process::Stdio;

use compact_str::{format_compact, CompactString};
use spore_vm::{
    val::native_function::NativeFunction,
    vm::{Vm, VmErrorInner},
};

pub fn register_shell(vm: &mut Vm) {
    vm.register_native_function(NativeFunction::new("shell-command!", |vm: &mut Vm| {
        let mut args = vm.args().iter().copied();
        let cmd_val = args.next().ok_or_else(|| VmErrorInner::WrongArity {
            name: "shell-command!".into(),
            expected: 1,
            actual: 0,
        })?;
        let mut cmd =
            std::process::Command::new(cmd_val.as_str(vm).ok_or(VmErrorInner::WrongType)?);
        cmd.stdout(Stdio::piped()).stderr(Stdio::piped());
        for arg in args {
            let arg_str = arg.as_str(vm).ok_or(VmErrorInner::WrongType)?;
            cmd.arg(arg_str);
        }
        let output = cmd
            .output()
            .map_err(|err| VmErrorInner::Custom(format_compact!("{err}")))?;
        if output.status.success() {
            let out = CompactString::from_utf8_lossy(&output.stdout);
            Ok(vm.make_string(out))
        } else {
            Err(VmErrorInner::Custom(CompactString::from_utf8_lossy(
                &output.stderr,
            )))?
        }
    }));
}
