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

**NOTE:** Output might be slightly different due to constant fixes and changes. Fully updated examples will be put each time they diverge too far from current.  
All the examples can be found in `examples/` directory.

### Fun with booleans
```
         _
        | |
    ____| |   ___  __  __
    | _ \ |__| _ \|  \/  |
    |  _/____|____\_\__/_| v1.0.0
    |_| pure λ-calculus interpreter
   =================================

pLam> :import booleans
pLam> 
pLam> and (or F (not F)) (xor T F)
> reductions count              : 18
> uncurried β-normal form       : (λxy. x)
> curried (partial) α-equivalent: T
pLam>
```

### Fun with arithmetic
```
         _
        | |
    ____| |   ___  __  __
    | _ \ |__| _ \|  \/  |
    |  _/____|____\_\__/_| v1.0.0
    |_| pure λ-calculus interpreter
   =================================

pLam> :import std
pLam> 
pLam> mul (add 2 (Sc 2)) (sub (exp 2 3) (Pc 8))
> reductions count              : 762
> uncurried β-normal form       : (λfx. f (f (f (f (f x)))))
> curried (partial) α-equivalent: 5
pLam> 
```

### Factorial
```
         _
        | |
    ____| |   ___  __  __
    | _ \ |__| _ \|  \/  |
    |  _/____|____\_\__/_| v1.0.0
    |_| pure λ-calculus interpreter
   =================================

pLam> :import std
pLam> :import comp
pLam>
pLam> fact = PR0 1 (C22 mul (C2 S I12) I22)
pLam> fact 3
> reductions count              : 898
> uncurried β-normal form       : (λfx. f (f (f (f (f (f x))))))
> curried (partial) α-equivalent: 6
pLam> 
```

### Minimization
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
pLam> fun = \x. mul (sub 2 x) (sub 3 x)
pLam> MIN1 fun
> reductions count              : 114
> uncurried β-normal form       : (λfx. f (f x))
> curried (partial) α-equivalent: 2
pLam> 
```
#### running the existing program:
```
         _
        | |
    ____| |   ___  __  __
    | _ \ |__| _ \|  \/  |
    |  _/____|____\_\__/_| v1.0.0
    |_| pure λ-calculus interpreter
   =================================

pLam> :run examples/2.5.2
=================================
< zero
=================================
> reductions count              : 114
> uncurried β-normal form       : (λfx. f (f x))
> curried (partial) α-equivalent: 2
pLam>
```

### Binary numerals
```
         _
        | |
    ____| |   ___  __  __
    | _ \ |__| _ \|  \/  |
    |  _/____|____\_\__/_| v1.0.0
    |_| pure λ-calculus interpreter
   =================================

pLam> :import binary
pLam>
pLam> 0b
> reductions count              : 2
> uncurried β-normal form       : (λp.((p (λxy. y)) (λexy.x)))
> curried (partial) α-equivalent: 0b
pLam> 
pLam> 2048b
> reductions count              : 24
> uncurried β-normal form       : (λp.((p (λxy. y)) (λp.((p (λxy. y)) (λp.((p (λxy. y)) (λp.((p (λxy. y)) (λp.((p (λxy. y)) (λp.((p (λxy. y)) (λp.((p (λxy. y)) (λp.((p (λxy. y)) (λp.((p (λxy. y)) (λp.((p (λxy. y)) (λp.((p (λxy. y)) (λp.((p (λxy. x)) (λexy.x)))))))))))))))))))))))))
> curried (partial) α-equivalent: (λp. ((p F) 1024b))
pLam>
pLam>
pLam> addB 7b (subBs 2b 3b)
> reductions count              : 9458
> uncurried β-normal form       : (λp.((p (λxy. x)) (λp.((p (λxy. x)) (λp.((p (λxy. x)) (λexy.x)))))))
> curried (partial) α-equivalent: 7b
pLam>
pLam> :quit
Goodbye!
```
