---
title: A Ppx ReactiveML extension for OCaml
author: Constantin \textsc{Gierczak-Galle} \& Samuel \textsc{Vivien}
numbersections: True
header-includes:
    - \usepackage{tikz}
    - \usetikzlibrary{chains, decorations.pathreplacing}
abstract: This project aims at turning the current implementation of ReactiveML, a reactive language forked from OCaml around the 3.4 version, and whose pure-OCaml features did not evolve in sync with the main project, into an extension to the official OCaml compiler in order to gain access to all modern features and libraries as well as allow ReactiveML and OCaml code to interoperate transparently, even within a single program or module.
---

# Motivation

[ReactiveML](https://github.com/reactiveml/rml) is a synchronous, event-based language based on OCaml 3.4. It has been implemented using the original OCaml compiler v3.4, and the OCaml part has not been updated since then, as every commit to the OCaml repository related to the parser would have to get adapted and merged into RML. Thus ReactiveML only supports features that were present in this version, meaning that some things such as modules and object-oriented structures, as well as any third-party extern OCaml library, cannot be used in ReactiveML, thus depriving the user of the opportunity to use some features of the OCaml ecosystem from within ReactiveML code.

Our project is aimed at addressing this issue by turning ReactiveML into an extension, via the ppx protocol, of the OCaml compiler, such that anyone can use ReactiveML inside an OCaml program while being free of using any other feature from the language. This change also bring a much simpler compilation process as ReactiveML code would only get compiler once and not twice (rmlc + ocamlc). Another benefit comes from the upcoming $5$.x.x OCaml version, which will bring multicore support as well as effects, this is a good opportunity to update ReactiveML's compiler in order to support those two new features. As an extension to our work, the project [ppx-rml-5](https://git.eleves.ens.fr/svivien/ppx-rml-5) tests a new interpreter for RML that use the new multicore feature of OCaml 5.00.

An added benefit is that ReactiveML should become a lot more maintainable, as it doesn't need anymore its own parser, core libraries, etc. The only logic left in ReactiveML would be its core functionnalities and our ppx extension. All the rest comes from the stock OCaml distribution. This means that maintaining ReactiveML "only" involves keeping the ppx extension in sync with OCaml's syntax evolution, as well as updating the ReactiveML semantics themselves.

# How it is done

## What is PPX

The OCaml compiler provides a technology called PPX. A PPX rewriter is preprocess that modifies ocaml's code just after it has been parsed. This allow to generate many complex code automatically just like macros in other programming languages.

To use a PPX rewriter the user can add the following line into his `dune` file (exemple given for the `ppx_rml` PPX rewriter):

```lisp
(preprocess (pps ppx_rml))
```

On the other side, the developper writes a library called `ppx_rml` that register one or multiple flags.

After parsing an OCaml code, the compiler looks in the AST for flags. For every flag found, the
associated subtree is given to the handler function associated with the flag. This handler function takes an AST node as argument and returns a new AST node (the real type is actually a bit more complicated).

There are two types of PPX flags : attributes and extensions.

### Atributes

Attributes are writen with `@` and attached to a node of the AST. The most common exemple is the `ppx_deriving`.

The following code adds the attribute `deriving` with the argument `(show, eq)` to the node representing `type point3d = float * float * float` of the AST. Through preprocess, an attribute will add new nodes to the AST.

```ocaml
type point3d = float * float * float
[@@deriving show, eq]
```

This exemple implements function for testing equality and printing elements of type `point3d` automaticaly. Thus the developper won't need to change them when he changes the type, everything is handled by the preprocess.

An attribute is written with one to three `@` depending of the localisation inside the AST (one to associate it to a expression, two to associate it to a structure item and three to create a new independant structure item).

### Extension

Extensions are writen with `%`. In opposition to attributes, extensions are nodes of the AST. During preprocess, the AST node is rewritten by the handler function associated with the corresponding extension.

A good exemple of preprocess trough extension is `ppx_getenv2`. This extension replaces `[%getven "PPX_GETENV2"]` with a string corresponding to the value of `PPX_GENENV2` at compilation time.

An extension can also be written with a different syntax :

```ocaml
let print_1_to_n n =
    for%par i = 1 to n do
        print_int i;
    done;
    print_endline ()
```

Which is exactly the same as

```ocaml
let print_1_to_n n =
    [%par
        for i = 1 to n do
            print_int i;
        done
    ];
    print_endline ()
```

An extension is written with one or two `%` depending of the localisation inside the AST just like attributes.

## Our usage of PPX

In order to get all the tools from the existing ReactiveML compiler we just changed the front and the back end of the compiler rather then developping a brand new compiler.

The user encompasses his RML code with the `rml` extension. Thus the handler function registered with the extension flag `rml` is given the AST of the corresponding code by `ocamlc`. This AST first goes through the function `Ocaml2rml.main` that translate the OCaml's AST into a RML's AST. The AST obtained can now go through the whole typing and optimisation process inside the compiler. Lastly, at the backend of the compiler, instead of printing the OCaml code, the string representing the obtained code is given to OCaml's parser in order to obtain a new clean AST.

However the AST obtained throught this method is a list of structure item when the preprocess accepts only a structure item. In order to go around this problem we encompass the code generated in a module (who is called RML by default).

# Syntax and conversion

## Compiler arguments

As the compiler is not called like it usually is, the way to pass arguments had to be changed.

With the ppx version of the compiler the arguments are given as attributes in the first lines of the rml code. For exemple if someone wants to give the flag `-sampling 0.01` to the compiler than he just need to write `[@@@sampling 0.01]` in the first line of the rml code. Just like this (exemple taken from `test/darwin.ml`):

```ocaml
[%%rml

    [@@@sampling 0.01];;

    code
]
```

## Signal definition

In ReactiveML, a signal is defined as follows:

```ocaml
signal s default 0 gather (fun x y -> x + y)  in  ...
```

We replaced it by:

```ocaml
let s = Signal { default = 0; gather = fun x y -> x + y } in ...
```

However, the ReactiveML syntax does not allow for simultaneous declarations involving signals, while our syntax obviously does allow it (because we use the `let .. = .. in ..` syntax). Thus some thought went into deciding how to handle such constructions as

```ocaml
let sp4 = Signal {default = 0; gather = fun x _ -> x}
    and _useless_0 = 42
    and sp5 = Signal {default = 0; gather = fun x _ -> x}
    and _useless_1 = 69
    in ((emit sp4 _useless_0) || (emit sp5 _useless_1))
```

In term, each definition of a signal inside a simultaneous declaration is first converted to a

```ocaml
let .. = (let rml_var = Signal {...} in rml_var) in ..
```

such that a signal definition is always done alone.

## Process definition and signal emission

Signals can be emitted in ppx_rml the same way than in RML.

You just need to write `emit s` to emit the signal `s` or `emit s v` to emit the value `v` on the signal `s`.

When the ppx preprocess finds a function application node where the expression corresponding to the function is the identifier `emit` then we translate it to the corresponding node of the RML's AST.

In a similar way process can be defined in ppx_rml the same way than in RML. The syntax :

```ocaml
let process p a = expr
```

Defines a process `p` that takes an argument called `a`.

To do this, the preprocess looks for the name `process` in each let binding. In can it finds one, it takes the first argument of the function defined (because OCaml's parser understands `let process p = ...` as `let process = fun p -> ...`) and uses it as the process name and modifes the rest to transform it into a process.

## Await

There exist to different `await` in RML :

- `await s(l) /\ s2(l2) in expr` who binds the value `l` of the signal `s` and the value `l2` of signal `s2` in `expr` (this works like a `let l = ... in `)
- and `await s` that just stop the process until the signal `s` is received.

Both syntax have been handled in a different way.

### Await s(l) in expr

The `await s(l) /\ s2 = l2 in expr` has been rewritten as

```ocaml
let%await All = l = s && l2 = s2 in expr
```

Where `All` can be replaced by `Immediate One` or `One` depending of when you want the process to wake up (see RML's documentation for more information).

### Await s

On the other hand, `await s` is still written the same way, except that `/\` has been replace by `&&` and `\/` by `||`

### When condition

For both of then, the condition with a `when` can still be written, except that you write `When` and need to put parenthesis around the condition in order to avoid priority problems.

## Until

In RML you have the possibility to execute a block of code until as certain event occurs. This can be written as :

```ocaml
do
    block
until s(l) when l = 3 
```

In ppx_rml this can be written as

```ocaml
try%until
    block
with [%event l = s] when l = 3 -> ()
```

The reason we needed the `[%event ]` is that the parser wants a pattern at this place, when the rml syntax allowed any expression. Thus the extension wrapper allowed to write an expression while making the parser happy.

## Do when

The `do .. when ..` sutructure is implemented the same way as `until` :

```ocaml
try%when
    while !a do
    global_value := !global_value + 1;
    if%present s2 then a := false;
    done
with [%event s] -> print_endline "Not running"
```

However, contrarely to `do .. until ..`, this construct in ReactiveML does not allow to add a guard on the `with` branch and, as a result, the ppx preprocessing will fail if one is added, e.g. `with [%event a = s] when a = s -> print_endline "Not running"`.

## Control

The `control .. with ..` is also similar to `do .. until` but it does allow adding a guard on the `with` branch:

```ocaml
try%control
    while !a do
        global_value := !global_value + 1;
        if%present s2 then a := false;
    done
with [%event (a = s) || (a = s2)] when a = n -> print_endline "Not running"
```

## Present

The `present` operator uses the `if` syntax, as it has semantics kind of similar to a `if` construct.

We can use:

```ocaml
if%present s then a := false else print_endline "Failed";
```

## Last, default, pre

The `last`, `default` and `pre` operators were quite simple to implement and only involved detecting function calls with these keywords.

We can thus have:

```ocaml
let v_pre = pre s;;
let v_last = last s;;
let v_default = default s;;
```

## Ocaml expressions

# Our work

Most of the code in ReactiveML has been written by previous developpers of the projet. Our work resides in :

- changing the build chain to `dune` (every dune files)
- adding tests (`test` folder) using the `alcotest` opam library
- adding the ppx version of the compiler (`compiler/main/rmlc.ml` and `compiler/parsing/ocaml2rml.ml`)

# What next ?

Currently our code just started to be merge into RML repository :

- PR1 renamming files to avoid conflics with Ppxlib (merged)
- PR2 changing the build chain from Makefile to Dune (under review)
- PR3 removing the >500 warnings (awaiting review)

More PR are going to be send in order to merge our code step by step in RML while allowing Louis Mandel (the maintainer of RML) to review the full code.

# Future extensions

## Feature that need to be implemented

- change the ppx_rml backend in order to stop printing the code before parsing it

## Feature that would be nice to have

- support the effects from the next version of OCaml
- update the interpreter to enable multithreading with OCaml 5.00 (see [this project](https://git.eleves.ens.fr/svivien/ppx-rml-5) to see the tests towards such an interpreter)
- reduce the amount of structures and types that aren't supported in RML's AST.
