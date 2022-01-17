---
title: A Ppx ReactiveML extension for OCaml
author: Constantin \textsc{Gierczak-Galle} \& Samuel \textsc{Vivien}
numbersections: True
advanced-maths: True
advanced-cs: True
theme: metropolis
header-includes:
    - \usepackage{setspace}
---

# Motivation

## RML is old

Unsupported structures :

- modules
- classes

# How it is done

## PPX

2 kinds :

- attributes

```ocaml
type point3d = float * float * float
[@@deriving eq, show]
```

- extension

```ocaml
[%getenv SOMEVAR]
```

## How to register an extension

```ocaml
let ext = Extension.declare_with_path_arg
    "rml"
    Extension.Context.structure_item
    Ast_pattern.(pstr __)
    handler in 
Ppxlib.Driver.register_transformation "rml" ~extensions:[ext]
```

**Usage :**

```ocaml
[%%rml.Exemple
    let process t n =
        if t > 0 
        then run (t - 1) || run (t - 1)  
]

let () = Exemple.run (Exemple.t 3)
```

# Syntax and conversion

Exemples

# Future extensions

- Multithreaded interpreter
- Support effects from OCaml 5.00

## Work distribution

- new build chain (S with bug correction by C)
- PPX interface (S)
- AST translation (S for every thing between OCaml and RML, C for RML-specific structures)
- Tests (mostly C)
