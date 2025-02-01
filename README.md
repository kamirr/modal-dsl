# modal-dsl
JIT-compiled language for writing modules in the software synthesiser
[modal](https://github.com/kamirr/modal) implemented using
[cranelift](https://cranelift.dev) and [chumsky](https://github.com/zesterer/chumsky).

## State
Very much work in progress.
- Barely has any features.
- No optimizations beyond what cranelift does.
- Will panic if you look at it wrong.
- It's likely to have safety bugs.
- Error reporting is non-existent.