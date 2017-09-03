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
# Fun with booleans
```
LCI> import booleans.txt
- imported all from "booleans.txt"
LCI> define id = \x.x
LCI> -- this is a comment line
LCI> execute foo
Undeclared variable "foo"
Try typing
 define "foo" = <lambda expression>
to define it before use
LCI> review all
ENVIRONMENT:
   "id" = λx.x
   "or" = λx.λy.((x true) y)
   "and" = λx.λy.((x y) false)
   "not" = λx.((x false) true)
   "false" = λx.λy.y
   "true" = λx.λy.x
LCI> execute (id (or false (not true)))
----- result        : λx.λy.y
----- defined as    : "false"
----- natural number: none
LCI>
```
more complex examples are coming soon, after I improve evaluation algorithms.
