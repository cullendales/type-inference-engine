# Type Inference Engine

Funtype is a Haskell-based type inference engine for a minimalist functional language called Fun.  
It implements a constraint-based type inference system that can infer types for expressions containing integers, booleans, functions, and control flow constructs without any explicit type annotations.

The project demonstrates how static typing can be achieved through constraint generation and unification in a purely functional setting.

## The FUN Language

The FUN language supports the following expressions:

```haskell
e ::= c | b | x
    | e + e | e - e | e == e
    | if e then e else e
    | lambda x. e
    | app e e
    | let x = e in e
```

Example Haskell representations:

```haskell
Var "x1"                           -- Variable
Plus (CInt 1) (CInt 2)             -- 1 + 2
ITE (CBool True) (CInt 1) (CInt 2) -- if True then 1 else 2
Abs "x" (Var "x")                  -- lambda x. x
App (Abs "x" (Var "x")) (CInt 1)   -- (lambda x. x) 1
LetIn "x" (CInt 1) (Var "x")       -- let x = 1 in x
```

## Core Concepts

- **Typing Environment (`Env`)** – Maps variable identifiers to types.  
- **Constraints (`ConstraintSet`)** – Represents type relationships that must be unified.  
- **Substitution (`Substitution`)** – Maps type variables to concrete types.  
- **Unification** – Produces the most general substitution satisfying all constraints.  
- **Type Inference Monad (`InferState`)** – Manages the generation of fresh type variables.  

## To Run

```bash
ghc main.hs -o main
./main exps.txt
```
