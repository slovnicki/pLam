# λ-calculus interpreter
(on its way to become fun programming language)

## Prerequisites
This project builds using Haskell tool stack documented at https://docs.haskellstack.org/en/stable/README/.

## Install
On most Unix systems, you can get stack by typing:
```
curl -sSL https://get.haskellstack.org/ | sh
```
or:
```
wget -qO- https://get.haskellstack.org/ | sh
```
On Windows, you can download 64-bit installer given at https://docs.haskellstack.org/en/stable/README/.

## Build & Run
1. clone project repository:
```
git clone https://github.com/sandrolovnicki/lambda-calculus-interpreter.git
```
2. go to project directory:
```
cd lambda-calculus-interpreter
```
3. setup stack on isolated location
```
stack setup
```
4. use stack to build project:
```
stack build
```
5. use stack to run project executable
```
stack exec plam
```

## Examples:

NOTE: Output might be slightly different due to constant fixes and changes.
      Fully updated examples will be put each time they diverge too far from current.

### Fun with booleans
```
         _
        | |
    ____| |   ___  __  __
    | _ \ |__| _ \|  \/  |
    |  _/____|____\_\__/_| v0.1.0
    |_| pure λ-calculus interpreter
   =================================

pLam> :import booleans
pLam> 
pLam> and (not F) (or (xor F T) F)
----- reductions count : 14
----- β normal form    : λx.λy.x
----- α-equivalent     : T
pLam>
```

### Fun with arithmetic
```
         _
        | |
    ____| |   ___  __  __
    | _ \ |__| _ \|  \/  |
    |  _/____|____\_\__/_| v0.1.0
    |_| pure λ-calculus interpreter
   =================================

pLam> :import std
pLam> sub (add 3 2) 1
----- reductions count : 24
----- β normal form    : λf.λx.(f (f (f (f x))))
----- α-equivalent     : 4
pLam>
```

### Factorial
```
         _
        | |
    ____| |   ___  __  __
    | _ \ |__| _ \|  \/  |
    |  _/____|____\_\__/_| v0.1.0
    |_| pure λ-calculus interpreter
   =================================

pLam> :import std
pLam> :import comp
pLam> fact = PR0 1 mul
pLam> fact 3
----- reductions count : 647
----- β normal form    : λf.λx.(f (f (f (f (f (f x))))))
----- α-equivalent     : 6
pLam>
```

### Minimization
Detailed description of what is going on is given in programs/min.plam as comments.
#### interactive coding:
```
        _
        | |
    ____| |   ___  __  __
    | _ \ |__| _ \|  \/  |
    |  _/____|____\_\__/_| v0.1.0
    |_| pure λ-calculus interpreter
   =================================

pLam> :import std
pLam> :import comp
pLam> 
pLam> f = \x. sub 6 (mul 2 x)
pLam> cond = \x y. sub (f x) y
pLam> find = MIN cond
pLam> x0 = find 0
pLam> 
pLam> x0
----- reductions count : 292
----- β normal form    : λf.λx.(f (f (f x)))
----- α-equivalent     : 3
pLam>
```
#### running the existing program:
```
         _
        | |
    ____| |   ___  __  __
    | _ \ |__| _ \|  \/  |
    |  _/____|____\_\__/_| v0.1.0
    |_| pure λ-calculus interpreter
   =================================

pLam> :run programs/min.plam
zero of f(x) = 6-2x is...
----- reductions count : 292
----- β normal form    : λf.λx.(f (f (f x)))
----- α-equivalent     : 3
pLam>
```

### Binary numerals
```
         _
        | |
    ____| |   ___  __  __
    | _ \ |__| _ \|  \/  |
    |  _/____|____\_\__/_| v0.1.0
    |_| pure λ-calculus interpreter
   =================================

pLam> -- first, let's try sub with Church numerals to see the reductions count
pLam> :import std
pLam> sub 255 4
----- reductions count : 2051
----- β normal form    : λf.λx.(f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f x)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
----- α-equivalent     : 251
pLam> 
pLam> -- now the binary version
pLam> -- NOTE: operations on this representation of numerals have reduction complexity dependant just on the number of bits of larger operand
pLam> :import binary
pLam> bin4 = pair F (pair F (pair T end))
pLam> bin255 = make8b T T T T T T T T
pLam> subBv bin255 bin4
----- reductions count : 1950
----- β normal form    : λp.((p λx.λy.x) λp.((p λx.λy.x) λp.((p λx.λy.y) λp.((p λx.λy.x) λp.((p λx.λy.x) λp.((p λx.λy.x) λp.((p λx.λy.x) λp.((p λx.λy.x) λe.λx.λy.x))))))))
----- α-equivalent     : λp.((p T) λp.((p T) λp.((p F) λp.((p T) λp.((p T) λp.((p T) λp.((p T) λp.((p T) end))))))))
pLam> :quit
Goodbye!
```
