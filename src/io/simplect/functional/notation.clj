(ns io.simplect.functional.notation
  (:require
   [cats.core					:as cats]
   [clojure.core				:as core]
   [io.simplect.functional			:as iof]))

(defn- build-sym [nm] (symbol (str (ns-name *ns*)) (name nm)))

(defn- var-arglist-and-doc
  [fvar]
  (select-keys (meta fvar) [:arglists :doc]))

(defn- merge-meta
  [target-var m]
  (alter-meta! target-var (iof/partial> merge m)))

(defmacro fref
  [nm fname]
  `(let [m# (var-arglist-and-doc (var ~fname))]
     (def ~nm ~fname)
     (merge-meta (var ~nm) (update-in m# [:doc] #(str "\nAbbreviation for " '~fname "\n\n" %)))
     '~nm))

(alter-meta! #'fref #(assoc % :private true))

(fref Π	core/partial)
(fref π		iof/partial>)
(fref γ		core/comp)
(fref Γ	iof/rcomp)
(fref μ	core/map)
(fref μμ	core/mapv)
(fref ρ	core/reduce)
(fref conj-ρ	iof/conjreduce)
(fref assoc-ρ	iof/assocreduce)

(defmacro Ξ
  [& args]
  `(cats/curry ~@args))
(merge-meta #'Ξ (var-arglist-and-doc #'cats/curry))

(defmacro λ
  [& args]
  `(fn ~@args))
(merge-meta #'λ (var-arglist-and-doc #'fn))
