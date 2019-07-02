# Notation

`compose` provides a single-letter abbreviations for the most heavily used functions.  Both Greek
and Latin letter abbreviations are provided, of which the author prefer the Greek variant because it
helps the functions stand out as operators.  The Latin variants are provided for users who either
prefer the Latin variant or do not have ewasy access to typing Greek letters on their keyboard.

The table below provides an overview of each abbreviated form (`core` refers to `clojure.core`,
`compose` to `io.simplect.compose`):

| Notation     | Abbreviates        | Intended mnemonic         |
|--------------|--------------------|---------------------------|
| γ, c         | `core/comp`        | 'c' (gamma) for *compose* |
| Γ, C         | `compose/rcomp`    | 'C' (Gamma) for *compose* |
| π, p         | `core/partial`     | 'p' (pi) for *partial*    |
| Π, P         | `compose/raptial`  | 'P' (Pi) for *partial*    |
| χ            | `compose/curry`    | 'k' (chi) for *kurry*     |
| λ            | `core/fn`          | 'lambda' for *lambda*     |
| μ, m         | `core/map`         | 'm' (mu) for *map*        |
| ρ, r         | `core/reduce`      | 'r' (rho) for *reduce*    |
 
Some people may prefer to not use these abbreviated forms, which require you (1) to remember the
meaning of each and (2) may not be straightforward to enter on most keyboards.  

Different computing environments offer different solutions to the character entry issue, below is a
sketch of how to use Abbrevs in Emacs.

## Using the notation

The notation characters work best if they can be used without a namespace qualifier, either by
simply requiring all of them

```
(ns myns
  (:require [io.simplect.compose.notation	:refer :all]))
```

or importing them individually as needed

```
(ns myns
  (:require [io.simplect.compose.notation	:refer [π Π γ Γ]]))
```
