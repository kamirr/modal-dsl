# modal-dsl
JIT-compiled language for writing modules in the software synthesiser
[modal](https://github.com/kamirr/modal) implemented using
[cranelift](https://cranelift.dev) and [chumsky](https://github.com/zesterer/chumsky).

## State
Very much work in progress.
- Barely has any features.
- No optimizations beyond what cranelift does.
- A few functions will panic on bad inputs.
- Terrible, but somewhat informative error reporting.