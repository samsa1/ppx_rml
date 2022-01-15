---
title: Towards a new interpreter for RML
author: Samuel \textsc{Vivien}
numbersections: True
header-includes:
    - \usepackage{tikz}
    - \usetikzlibrary{chains, decorations.pathreplacing}
toc: True
abstract: Des trucs blabla
---

# Disclaimer

The whole code provided is not mine. The source has been taken from the project https://github.com/samsa1/ppx_rml. This project is itself based on the project ReactiveML.

The project `ppx_rml` modified ReactiveML compiler in order to allow compilation thought PPX (see the report corresponding to this project in the folder `docs` of it's repository).

The project presented here aims to present the modification done to the interpreter to take advantage of the `domainslib` library that will come with OCaml 5.00.

# Motivation

When writing those lines, OCaml 5.00 (also known as OCaml multicore) is due to April 04, 2022 says https://github.com/ocaml/ocaml/milestone/40.

Even if this version has been promised for a long time, it is closer than ever. And demo versions are already available (`4.12.0+domains` and `4.12.0+domains+effects`).

As shown in the demo versions above, the two main additions that OCaml 5 will have in comparison to OCaml 4 are multithreading and algebraic effects. While multithreading is quite intuitive, it is a bit more complicated to understand what effects are.

## TODO

ReactiveML is a reactive language based on OCaml who is simulating multi-threading. The arrival of a multi-threading in OCaml means that ReactiveML could benefit from it in order to improve the performances.


## TODO
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

An attribute is written with one or two `@` depending of the localisation inside the AST (one for expressions and two for structure items).

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

# Our work

Most of the code in ReactiveML has been written by previous developpers of the projet. Our work resides in :

- changing the build chain to `dune` (every dune files)
- adding tests (`test` folder)
- adding the ppx version of the compiler (`compiler/main/rmlc.ml` and `compiler/parsing/ocaml2rml.ml`)

# Future extensions

Prepare for OCaml 5.00
