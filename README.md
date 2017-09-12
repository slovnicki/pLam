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
### Fun with booleans
```
LCI> import booleans.txt
- imported all from "booleans.txt"
LCI> define !id = \x.x
LCI> -- this is a comment line
LCI> -------------------------------------
LCI> execute auto !foo
ERROR: undeclared variable "foo"
- type "review all" to see all environment variables you can use
- type "define <variable name> = <lambda expression>" to add new variables to environment
LCI> review all
 ENVIRONMENT:
--- "id" = λx.x
--- "xor" = λx.λy.((x (λx.((x λx.λy.y) λx.λy.x) y)) y)
--- "or" = λx.λy.((x true) y)
--- "and" = λx.λy.((x y) false)
--- "not" = λx.((x false) true)
--- "false" = λx.λy.y
--- "true" = λx.λy.x
LCI> --------------------------------------
LCI> define !foo = !id
LCI> execute (!foo (!or !false (!not !true)))
-- 0: (λx.x ((λx.λy.((x λx.λy.x) y) λx.λy.y) (λx.((x λx.λy.y) λx.λy.x) λx.λy.x)))
-- 1: ((λx.λy.((x λx.λy.x) y) λx.λy.y) (λx.((x λx.λy.y) λx.λy.x) λx.λy.x))
-- 2: (λy.((λx.λy.y λx.λy.x) y) (λx.((x λx.λy.y) λx.λy.x) λx.λy.x))
-- 3: ((λx.λy.y λx.λy.x) (λx.((x λx.λy.y) λx.λy.x) λx.λy.x))
-- 4: (λy.y (λx.((x λx.λy.y) λx.λy.x) λx.λy.x))
-- 5: (λx.((x λx.λy.y) λx.λy.x) λx.λy.x)
-- 6: ((λx.λy.x λx.λy.y) λx.λy.x)
-- 7: (λy.λx.λy.y λx.λy.x)
-- 8: λx.λy.y
-- fixed point reached!
-- fixed point reached!
----- result        : λx.λy.y
----- α-equivalent  : "false"
----- Church numeral: 0
LCI> :quit
you@your-computer your/path/to/lambda-calculus-interpreter
```

### Fun with arithmetic
```
LCI> import arithmetic.txt
- imported all from "arithmetic.txt"
LCI> execute manual (!succ (!plus 0 1))
-- 0: (λn.λf.λx.(f ((n f) x)) ((λm.λn.λf.λx.((m f) ((n f) x)) λf.λx.x) λf.λx.(f x)))
Continue? [Y/n]

-- 1: λf.λx.(f ((((λm.λn.λf.λx.((m f) ((n f) x)) λf.λx.x) λf.λx.(f x)) f) x))
Continue? [Y/n]

-- 2: λf.λx.(f (((λn.λf.λx.((λf.λx.x f) ((n f) x)) λf.λx.(f x)) f) x))
Continue? [Y/n]

-- 3: λf.λx.(f ((λf.λx.((λf.λx.x f) ((λf.λx.(f x) f) x)) f) x))
Continue? [Y/n]

-- 4: λf.λx.(f (λx.((λf.λx.x f) ((λf.λx.(f x) f) x)) x))
Continue? [Y/n]

-- 5: λf.λx.(f ((λf.λx.x f) ((λf.λx.(f x) f) x)))
Continue? [Y/n]

-- 6: λf.λx.(f (λx.x ((λf.λx.(f x) f) x)))
Continue? [Y/n]

-- 7: λf.λx.(f ((λf.λx.(f x) f) x))
Continue? [Y/n]

-- 8: λf.λx.(f (λx.(f x) x))
Continue? [Y/n]

-- 9: λf.λx.(f (f x))
Continue? [Y/n]

-- fixed point reached!
----- result        : λf.λx.(f (f x))
----- α-equivalent  : none
----- Church numeral: 2
LCI> :quit
you@your-computer your/path/to/lambda-calculus-interpreter
```

### Renaming
```
LCI> execute tree ((\f x. f x) (\f x. f x))
(λf.λx.(f x) λf.λx.(f x))
|
`- λx.(λf.λx.(f x) x)
   |
   `- λx.λf'.(x f')

LCI> :quit
you@your-computer your/path/to/lambda-calculus-interpreter
```
