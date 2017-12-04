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
Output might be slightly different due to constant fixes and changes.
Fully updated examples will be put each time they diverge too far from current.
(Check the newswest example (minimization) which is up to date)

### Fun with booleans
```
LCI> :import booleans
LCI> id = \x.x
LCI> -- this is a comment line
LCI> -------------------------------------
LCI> foo
ERROR: undeclared variable "foo"
- type ":review all" to see all environment variables you can use
- type "<variable name> = <lambda expression>" to add new variables to environment
LCI> foo = id
LCI> --------------------------------------
LCI> --------------------------------------
LCI> -- immediate solution --
LCI> --------------------------------------
LCI> (foo (or false (not true)))
----- β normal form : λx.λy.y
----- α-equivalent  : "false"
----- Church numeral: 0
LCI> --------------------------------------
LCI> --------------------------------------
LCI> -- detailed solution --
LCI> --------------------------------------
LCI> :d (foo (or false (not true)))
- type reduction option (a-auto, m-manual, t-tree, [DEFAULT-immediate]): a
-- 0: (λx.x ((λx.λy.((x λx.λy.x) y) λx.λy.y) (λx.((x λx.λy.y) λx.λy.x) λx.λy.x)))
-- 1: ((λx.λy.((x λx.λy.x) y) λx.λy.y) (λx.((x λx.λy.y) λx.λy.x) λx.λy.x))
-- 2: (λy.((λx.λy.y λx.λy.x) y) (λx.((x λx.λy.y) λx.λy.x) λx.λy.x))
-- 3: ((λx.λy.y λx.λy.x) (λx.((x λx.λy.y) λx.λy.x) λx.λy.x))
-- 4: (λy.y (λx.((x λx.λy.y) λx.λy.x) λx.λy.x))
-- 5: (λx.((x λx.λy.y) λx.λy.x) λx.λy.x)
-- 6: ((λx.λy.x λx.λy.y) λx.λy.x)
-- 7: (λy.λx.λy.y λx.λy.x)
-- 8: λx.λy.y
--- no beta redexes!
----- β normal form : λx.λy.y
----- α-equivalent  : "false"
----- Church numeral: 0
LCI> :quit
you@your-computer your/path/to/lambda-calculus-interpreter
```

### Fun with arithmetic
```
LCI> :import arithmetic
LCI> two = (succ (plus 0 1))
LCI> :d two
- type reduction option (a-auto, m-manual, t-tree, [DEFAULT-immediate]): m
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

--- no beta redexes!
-- 10: λf.λx.(f (f x))
Continue? [Y/n]
n
----- β normal form : λf.λx.(f (f x))
----- α-equivalent  : none
----- Church numeral: 2
LCI> :quit
you@your-computer your/path/to/lambda-calculus-interpreter
```

### Renaming
```
LCI> ((\f x. f x) (\f x. f x))
----- β normal form : λx.λf'.(x f')
----- α-equivalent  : none
----- Church numeral: 1
LCI> :quit
you@your-computer your/path/to/lambda-calculus-interpreter
```

### Factorial
```
LCI> :import booleans
LCI> :import arithmetic
LCI> :import predicates
LCI> Y = \f. (\x. f(x x)) (\x. f(x x))
LCI> fact = (Y (\f n. if (isZero n) 1 (mult n (f (pred n)))))
LCI> (fact 3)
----- β normal form : λf.λx.(f (f (f (f (f (f x))))))
----- α-equivalent  : none
----- Church numeral: 6
LCI> :quit
you@your-computer your/path/to/lambda-calculus-interpreter
```
### Running the existing program
```
LCI> :run programs/program0.txt
5 - 4 is
----- β normal form : λf.λx.(f x)
----- α-equivalent  : none
----- Church numeral: 1
is 3 equal to 0?
----- β normal form : λx.λy.y
----- α-equivalent  : "false"
----- Church numeral: 0
is 1 less than or equal to 2?
----- β normal form : λx.λy.x
----- α-equivalent  : "true"
----- Church numeral: none
second of first of ((0,7),2)
----- β normal form : λf.λx.(f (f (f (f (f (f (f x)))))))
----- α-equivalent  : "number7"
----- Church numeral: 7
LCI> :quit
you@your-computer your/path/to/lambda-calculus-interpreter
```

### Minimization
In programs folder there is min.txt which implements this example so 2 code samples will be presented here.
#### interactive coding
```
LCI> :import comp
LCI> f = \x. minus 6 (mult 2 x)
LCI> cond = \x. eq (f x) 0
LCI> x0 = M cond
LCI> x0
----- β normal form : λf.λx.(f (f (f x)))
----- α-equivalent  : 3
LCI>
```
#### running the program
```
LCI> :run programs/min.txt
zero of f(x) = 6-2x is...
----- β normal form : λf.λx.(f (f (f x)))
----- α-equivalent  : 3
LCI> 
```
