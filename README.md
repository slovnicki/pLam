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
stack exec lci
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
----- β normal form : λx.λy.x
----- α-equivalent  : T
pLam> :quit
Goodbye!
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

pLam> :import arithmetic
pLam> 
pLam> eq 3 (add 2 1)
----- β normal form : λx.λy.x
----- α-equivalent  : T
pLam> :quit
Goodbye!
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
pLam> 
pLam> Y = \f. (\x. f(x x)) (\x. f(x x))
pLam> fact = Y (\f n. (isZero n) 1 (mult n (f (pred n))))
pLam> 
pLam> fact 3
----- β normal form : λf.λx.(f (f (f (f (f (f x))))))
----- α-equivalent  : 6
pLam> :quit
Goodbye!
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

pLam> :import comp
pLam> 
pLam> f = \x. sub 6 (mult 2 x)
pLam> condition = \x. eq (f x) 0
pLam> x0 = Min condition
pLam> 
pLam> x0
----- β normal form : λf.λx.(f (f (f x)))
----- α-equivalent  : 3
pLam>
pLam> :quit
Goodbye!
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
----- β normal form : λf.λx.(f (f (f x)))
----- α-equivalent  : 3
pLam> :quit
Goodbye!
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

pLam> :import binary
pLam>
pLam> n11
----- β normal form : λB.((((B λx.λy.x) λx.λy.x) λx.λy.y) λx.λy.x)
----- α-equivalent  : (λB. ((((B T) T) F) T))
pLam>
pLam>
pLam> BN4add n2 n3
----- β normal form : λB.((((B λx.λy.x) λx.λy.y) λx.λy.x) λx.λy.y)
----- α-equivalent  : (λB. ((((B T) F) T) F))
pLam>
pLam> BN4sub n13 n10
----- β normal form : λB.((((B λx.λy.x) λx.λy.x) λx.λy.y) λx.λy.y)
----- α-equivalent  : (λB. ((((B T) T) F) F))
pLam> 
pLam> :quit
Goodbye!
```
