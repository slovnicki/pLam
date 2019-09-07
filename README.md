[![Version](https://img.shields.io/github/release/sandrolovnicki/pLam.svg)]()
[![License](https://img.shields.io/github/license/sandrolovnicki/pLam.svg)]()

<p align="center">
  <img width=100% height=auto src="https://raw.githubusercontent.com/sandrolovnicki/pLam/master/res/demo-v2.gif">
</p>

pLam (**p**ure **Lam**bda calculus) is a tool to explore, define and evaluate various λ-expressions. Code written in pLam can be executed interactively within pLam's shell or stored in a file with `.plam` extension and run anytime.

Inside `import/` directory, many useful λ-expressions are already implemented to be used as libraries.

---

### Table of contents

- [Install a package](#install)
  - [Arch Linux](#aur)
  - [Debian](#deb)
- [Build from source](#build)
  - [Prerequisites](#prerequisites)
  - [First time setup](#fts)
  - [Building](#build)
  - [Running (locally)](#runl)
  - [Running (globally)](#rung)
- [Syntax and Semantics](#synsem)
  - [λ-expressions](#expr)
    - [Variable](#var)
    - [Abstraction](#abs)
    - [Application](#app)
  - [Commands](#commands)
    - [Define](#def)
    - [Evaluate](#eval)
    - [Import](#imp)
    - [Export](#exp)
    - [Comment](#comm)
    - [Run](#run)
    - [Print](#print)
  - [Syntactic Sugars](#syns)
    - [Church numerals](#cn)
    - [Binary numerals](#bn)
    - [Lists](#lists)
- [Examples](#examples)
  - [Fun with booleans](#fwb)
  - [Fun with arithmetic](#fwa)
  - [Factorial](#fact)
    - [Standard way](#sw)
    - [Primitive recursive way](#prw)
  - [Binary numerals](#bin)
  - [Lists](#liex)
  - [Redex coloring](#rc)
  - [Running the existing program](#runex)
    - [Without entering pLam's shell](#runout)
- [Additional notes](#additional)

<a name="install"/>

# Install a Package

<a name="aur"/>

### Arch Linux

pLam's AUR package is at https://aur.archlinux.org/packages/plam thanks to @Xmgplays.   

Git Clone URL: https://aur.archlinux.org/plam.git

<a name="deb"/>

### Debian

(coming soon...)

<a name="build"/>

# Build from source

<a name="prerequisites"/>

## Prerequisites
This project builds using Haskell tool stack documented at https://docs.haskellstack.org/en/stable/README/.

On most Unix systems, you can get stack by typing:
```
curl -sSL https://get.haskellstack.org/ | sh
```
or:
```
wget -qO- https://get.haskellstack.org/ | sh
```
On Windows, you can download 64-bit installer given at https://docs.haskellstack.org/en/stable/README/.

<a name="fts"/>

## First time setup
1. clone project repository
```
git clone https://github.com/sandrolovnicki/pLam.git
```
2. go to project directory
```
cd pLam
```
3. setup stack on isolated location
```
stack setup
```
<a name="build"/>

## Building
4. use stack to build project
```
stack build
```
**note:** if build was not successful, it may be due to:

- curses library
  - the solution is to install it (on Ubuntu: `sudo apt-get install libncurses5-dev libncursesw5-dev`)

<a name="runl"/>

## Running (locally)
5.a) use stack to run project executable from project's directory
```
stack exec plam
```
<a name="rung"/>

## Running (globally (Unix systems))
5.b) use `make_global.sh` script to create a global command 'plam' that can be used to start pLam from anywhere in your system. The script will also change your import path in src/Config.hs so you need to build the project again.
```
sudo ./make_global.sh
stack build
```
Now, (and anytime in the future!), you can start pLam from anywhere in your system by just typing
```
plam
```
---
<a name="synsem"/>

# Syntax and semantics

<a name="expr"/>

## λ-expressions

<a name="var"/>

### Variable
λ-variable is required to be lowercase and a single letter. For example, `x` is a good λ-variable for pLam and `X`, `var`,... are not. There are also environment variables (names for defined λ-expressions) which are every string that is not parsed as λ-variable, λ-abstraction or λ-application.

<a name="abs"/>

### Abstraction
λ-abstraction is written the same as in the language of pure (untyped) λ-calculus, except that pLam treats a symbol `\` as `λ` and it is required to write a space after `.`. For example, `λx.λy.x` would be written `\x. \y. x` in pLam. One can also write λ-abstraction in the "uncurried" form: `\xy. x` or `\x y. x`.

<a name="app"/>

### Application
λ-application is written like 2 λ-expressions separated by a space, for example `(\x. x) (\xy.x)` or `(\x. x) MyExpression` or `myexp1 myexp2`. Brackets `(` and `)` are used as usual and are not required to be written for application association; the default association is to the left, so `M N P` is parsed as `(M N) P` and one only needs to specify with brackets if the intended expression should be `M (N P)`.

<a name="commands"/>

## Commands
A block of code in pLam is a line, and possible lines (commands) are the following:

<a name="def"/>

### Define

- syntax: `<string> = <λ-expression>`
- semantics: let the `<string>` be a name for `<λ-expression>`.
- examples: `T = \x y. x`, `myexpression = T (T (\x. x) T) T`
- restriction: `<string>` needs to be of length>1 or starting with uppercase letter

<a name="eval"/>

### Evaluate

- syntax: `?<evop> ?<evop> <λ-expression>` where `<evop>`s are evaluation options; `:d` and/or `:cbv`
- semantics: reduce the `<λ-expression>` to β-normal form. If `:d` is chosen as option, all the reduction steps will be shown. If `:cbv` is chosen as option, reductions will be performed in a call-by-value manner, first reducing the argument before substituting it for bound variable. That is, call-by-name is the default reduction option if `:cbv` is not chosen.
- example: `\x y. x`, `:d T (T (\x. x) T) T`, `:d :cbv T (T (\x. x) T) T`, `:cbv and (or T F) T`
- example 2: `F omega T` will reduce to `T`, but `:cbv F omega T` will run forever
- restriction: none

<a name="imp"/>

### Import

- syntax: `:import <string>`
- semantics: put all the expressions defined in the file `import/<string>.plam` into the list of environment variables.
- example: `:import std`
- restriction: `<string>.plam` has to be inside `import/` directory within the pLam project directory

<a name="exp"/>

### Export

- syntax `:export <string>`
- semantics: put all the expressions in the list of environment variables into the file `import/<string>.plam`
- example: `:export test`
- restriction: `<string>.plam` cannot already exist

<a name="comm"/>

### Comment

- syntax: `--<string>`
- semantics: a comment line
- example: `-- this is a comment`
- restriction: none

<a name="run"/>

### Run

- syntax: `:run <string>`
- semantics: runs a `.plam` file with relative path `<string>.plam`
- example: `:run <relative-path-to-plam>/examples/2.5.2`
- restrictions: `~` for home does not work

<a name="print"/>

### Print

- syntax: `:print <string>`
- semantics: prints `<string>` to a new line. It mostly makes sense to use it in .plam programs to be executed, not in interactive mode where a comment should do the job better.
- example: `:print this is a message`
- restrictions: none

<a name="syns"/>

## Syntactic Sugars

pLam is equipped with some (optional) shortcuts to work with often used expressions.  

<a name="cn"/>

### Church numerals

Church numerals can be typed as `0`, `1`, `2`,... and pLam parses those integers as `λfx. x`, `λfx. f x`, `λfx. f (f x)`, ...

<a name="bn"/>

### Binary numerals

Similar to handling Church numerals, pLam also handles binary numerals from `binary.plam` library. You can type them as `0b`, `1b`, `2b`, ...  which is them parsed as `λp. p (λxy. y) (λexy.x)`, `λp. p (λxy. x) (λexy.x)`, `λp. p (λxy. y) (λp. p (λxy. x) (λexy.x))`, ...  
Note that binary numerals are nothing standard, but something I implemented, so I suppose the only documentation for them is here.

<a name="lists"/>

### Lists

List encoding is pretty standard; `empty = T`, `append = λhtfl. l h t`, and you can use syntact sugar which parses `[1,2]` into `λfl. l 1 (λfl. l 2 empty)`, `[T,\x.x]` into `λfl. l T (λfl. l (λx.x) empty)` and so on...


---

<a name="examples"/>

# Examples

**NOTE:** Output might be slightly different due to constant fixes and changes. Fully updated examples will be put each time they diverge too far from current.  
All the examples can be found in `examples/` directory.

<a name="fwb"/>

### Fun with booleans
```
pLam> :import booleans
pLam> 
pLam> and (or F (not F)) (xor T F)
|> reductions count               : 18
|> uncurried β-normal form        : (λxy. x)
|> curried (partial) α-equivalent : T
```

<a name="fwa"/>

### Fun with arithmetic
```
pLam> :import std
pLam> 
pLam> mul (add 2 (S 2)) (sub (exp 2 3) (P 8))
|> reductions count               : 762
|> uncurried β-normal form        : (λfx. f (f (f (f (f x)))))
|> curried (partial) α-equivalent : 5
```

<a name="fact"/>

### Factorial

<a name="sw"/>

#### "standard" way
```
pLam> :import std
pLam> 
pLam> fFact = \f. \x. (isZ x) 1 (mul x (f (P x)))
pLam> Fact = Y fFact
pLam> 
pLam> Fact 3
|> reductions count               : 646
|> uncurried β-normal form        : (λfx. f (f (f (f (f (f x))))))
|> curried (partial) α-equivalent : 6
```

<a name="prw"/>

#### primitive recursive way
```
pLam> :import std
pLam> :import comp
pLam> 
pLam> fact = PR0 1 (C22 mul (C2 S I12) I22)
pLam> fact 3
|> reductions count               : 898
|> uncurried β-normal form        : (λfx. f (f (f (f (f (f x))))))
|> curried (partial) α-equivalent : 6
```

<a name="bin"/>

### Binary numerals
```
pLam> :import binary
pLam> 
pLam> 0b
|> reductions count               : 2
|> uncurried β-normal form        : (λp.((p (λxy. y)) (λexy.x)))
|> curried (partial) α-equivalent : 0b
pLam> 
pLam> 2048b
|> reductions count               : 24
|> uncurried β-normal form        : (λp.((p (λxy. y)) (λp.((p (λxy. y)) (λp.((p (λxy. y)) (λp.((p (λxy. y)) (λp.((p (λxy. y)) (λp.((p (λxy. y)) (λp.((p (λxy. y)) (λp.((p (λxy. y)) (λp.((p (λxy. y)) (λp.((p (λxy. y)) (λp.((p (λxy. y)) (λp.((p (λxy. x)) (λexy.x)))))))))))))))))))))))))
|> curried (partial) α-equivalent : (λp. ((p F) 1024b))
pLam> 
pLam> addB 7b (subBs 2b 3b)
|> reductions count               : 9458
|> uncurried β-normal form        : (λp.((p (λxy. x)) (λp.((p (λxy. x)) (λp.((p (λxy. x)) (λexy.x)))))))
|> curried (partial) α-equivalent : 7b
```

<a name="liex"/>

### Lists

```
pLam> :import list
pLam> 
pLam> list = Merge [3,1] [2]
pLam> rlist = Reverse list
pLam> 
pLam> Get 0 rlist
|> reductions count               : 243
|> uncurried β-normal form        : (λfx. f (f x))
|> curried (partial) α-equivalent : 2
pLam> Get 0 list
|> reductions count               : 50
|> uncurried β-normal form        : (λfx. f (f (f x)))
|> curried (partial) α-equivalent : 3
pLam> 
pLam> QSort list
|> reductions count               : 459
|> uncurried β-normal form        : (λfl. (l (λfx. f x)) (λfl. (l (λfx. f (f x))) (λfl. (l (λfx. f (f (f x)))) (λfl. f))))
|> curried (partial) α-equivalent : (λf. (λl. ((l 1) (λf. (λl. ((l 2) (λf. (λl. ((l 3) empty)))))))))

```

<a name="rc"/>

### Redex coloring
![redex_coloring.png](https://raw.githubusercontent.com/sandrolovnicki/pLam/master/res/redex_coloring.png "Redex Coloring")


<a name="runex"/>

### Running the existing program:
```
pLam> :run examples/2.5.2
=================================
< zero
=================================
|> reductions count               : 114
|> uncurried β-normal form        : (λfx. f (f x))
|> curried (partial) α-equivalent : 2
```

<a name="runout"/>

#### Without entering pLam's shell:
```
plam ~/Projects/pLam/examples/2.5.2.plam
=================================
< zero
=================================
|> reductions count               : 114
|> uncurried β-normal form        : (λfx. f (f x))
|> curried (partial) α-equivalent : 2
Done.
```

---

<a name="additional"/>

**Disclaimer for Haskell experts**  
I am not a Haskell expert. In fact, this is my first and only Haskell project. It is inevitable that existing code could be written better and I wish to do it in the upcoming future.  
The goal of the project was to create an environment for easy building of new expressions from previously defined ones, so that one could explore λ-calculus. It was a helper tool so I could define and test a new numeral system in λ-calculus, for my master thesis. Now, when this all went well, the time is coming for me to get back to Haskell code.
