(ns io.simplect.functional.notation
  (:require
   [cats.core					:as cats]
   [io.simplect.functional			:as iof]))

(defmacro λ
  "Abbreviation for `clojure.core/fn`."
  [& args]
  `(fn ~@args))

(def Π
  "Abbreviation for `clojure.core/partial`."			
  partial)

(def π
  "Abbreviation  for `com.harbo-enterprises.util.misc/partial>`."
  iof/partial>)

(def γ
  "Abbreviation for `comp`."
  comp)

(def Γ
  "Abbreviation for `rcomp` which is `clojure.core/comp` with arguments
  in reverse order."
  iof/rcomp)

(def μ
  "Abbreviation for `map`."
  map)

(def μμ
  "Abbreviation for `mapv`.")

(def ρ
  "Abbreviation for `clojure.core/reduce`."
  reduce)

(def conj-ρ
  "Abbreviation for `com.harbo-enterprises.util.misc/conjreduce`."
  iof/conjreduce)

(def assoc-ρ
  "Abbreviation for `com.harbo-enterprises.util.misc/assocreduce`"
  iof/assocreduce)

(defmacro Ξ
  [& args ]
  `(iof/curry ~@args))
