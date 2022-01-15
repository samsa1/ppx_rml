---
title: A Ppx ReactiveML extension for OCaml
author: Constantin \textsc{Gierczak-Galle} \& Samuel \textsc{Vivien}
numbersections: True
header-includes:
    - \usepackage{tikz}
    - \usetikzlibrary{chains, decorations.pathreplacing}
abstract: This project aims at turning the current implementation of ReactiveML, a reactive language forked from OCaml around the 3.4 version, and whose pure-OCaml features have not been updated since, into an extension to the official OCaml compiler in order to gain access to all modern features and libraries as well as allow ReactiveML and OCaml code to interoperate transparently, even within a single program or module.
---

# Motivation

[ReactiveML](https://github.com/reactiveml/rml) is a synchronous, even-based language based on OCaml 3.4. It has been implemented using the original OCaml compiler a while back, and the OCaml part as not been updated since then, as every commit to the OCaml repository would have to get adapted and merged into RML. Thus ReactiveML only supports features that were present in this version, meaning that some things as modules and object-oriented structures, as well as any third-party extern OCaml library, cannot be used in ReactiveML, making it therefore a closed ecosystem that cannot access the rest of OCaml's one.

Our project is aimed at addressing this issue by turning ReactiveML into an extension, via the ppx protocol, the the OCaml compiler, such that anyone can use ReactiveML inside an OCaml program while being free of using any other feature from the language. Another benefit comes from the upcoming $5$.x.x OCaml version, which will bring multicore support. 

An added benefit is that ReactiveML should become a lot more maintainable, as it now doesn't need its own parser, core libraries, etc. The only logic left in ReactiveML its core functionnalities and our ppx extension. All the rest comes from the stock OCaml distribution. This means that maintaining ReactiveML "only" involves keeping the ppx extension in sync with OCaml's syntax evolution, as well as updating the ReactiveML semantics themselves; no more custom legacy OCaml parsers, typers and codegen units!

As an extension to our work, a multicore intepreter for ReactiveML yields computation time benefits. 

# How it is done

## What is PPX

The OCaml compiler provides a technology called PPX. A PPX rewriter is preprocess that modifies the ocaml code just after it was parsed.

To use this technology the user can add the following line into his `dune`file :

```
(preprocess (pps ppx_rml))
```

On the other side, the developper writes a library called `ppx_rml` that register one or multiple flags.

After parsing an OCaml code, the compiler search for flags in the AST. For every flag found, the
associated subtree is given to the preprocess function associated with the flag. This function takes an AST node as argument and returns a new AST node.

There are two types of PPX flags : attributes and extensions.

### Atributes

Attributes are writen with `@`. The most common exemple is the `ppx_deriving`.

The following code adds the attribute `deriving` with the argument `(show, eq)` to the node representing `type point3d = float * float * float` of the AST. Throught preprocess, an attribute will add new nodes to the AST.

```ocaml
type point3d = float * float * float
[@@deriving show, eq]
```

This exemples implements function for testing equality and printing elements of type `point3d` automaticaly. Thus the developper won't need to change them when he changes the type, everything is handled by the preprocess.

An attribute is written with one to three `@` depending of the localisation inside the AST.

### Extension

Extensions are writen with `%`. During preprocess, the AST node is rewritten by the handler function associated with the corresponding extension extension.

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

In order to get all the tools from the existing ReactiveML compiler to just changed the front and the back end of the compiler.

The user encompasses his RML code with the `rml` extension. Thus the whole handler with register with the `rml` flag recieves the AST of the corresponding code. This AST first goes through the function `Ocaml2rml.main` that translate the OCaml's AST into a RML's AST. The AST obtained can now go through the whole typing and optimisation process inside the compiler. And at the backend of the compiler, instead of printing the OCaml code, the string representing the obtained code is given to OCaml's parser in order to obtain a new clean AST.

However the AST obtained throught this method is a list of structure item when the preprocess accepts only a structure item. In order to go around this problem we encompass the code generated in a module (who is called RML by default).

# Syntax and conversion

## Compiler arguments

As the compiler is not called like it usually is, the way to pass arguments had to be changed.

With the ppx version of the compiler the arguments are given as attributes in the first lines of the rml code.

For exemple if someone wants to give the flag `-sampling 0.01` to the compiler than he just need to write `[@@@sampling 0.01]` in the first line of the rml code. Just like this (exemple taken from `test/darwin.ml`):

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

When the ppx preprocess finds a function application node where the expression corresponding to the function is the  
identifier `emit` then we translate it to the corresponding node of the RML's AST.

In a similar way process can be defined in ppx_rml the same way than in  RML. The syntax :

```ocaml
let process p a = expr
```

Defines a process `p` that takes an argument called `a`.

To do this, the preprocess looks for the name `process` in each let binding. In can it finds one, it takes the first argument of the function defined (because OCaml's parser understands `let process p = ...` as `let process = fun p -> ...`), uses it as the process name and modifes the rest to transform it into a process.


## Await

## Until

## Control

## Present

## Do when

## Last, default

# Our work

Most of the code in ReactiveML has been written by previous developpers of the projet. Our work resides in :

- changing the build chain to `dune` (every dune files)
- adding tests (`test` folder) using the `alcotest` opam library
- adding the ppx version of the compiler (`compiler/main/rmlc.ml` and `compiler/parsing/ocaml2rml.ml`)

# Future extensions

Prepare for OCaml 5.00
