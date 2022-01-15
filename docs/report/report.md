---
title: A Ppx ReactiveML extension for OCaml
author: Constantin \textsc{Gierczak-Galle} \& Samuel \textsc{Vivien}
numbersections: True
header-includes:
    - \usepackage{tikz}
    - \usetikzlibrary{chains, decorations.pathreplacing}
toc: True
abstract: Des trucs blabla
---

# Motivation

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

An attribute is written with one or two `@` depending of the localisation inside the AST.

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



# Syntax and conversion

# Future extensions