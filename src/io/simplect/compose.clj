;;  Copyright (c) Klaus Harbo. All rights reserved.
;;  The use and distribution terms for this software are covered by the
;;  Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;  which can be found in the file epl-v10.html at the root of this distribution.
;;  By using this software in any fashion, you are agreeing to be bound by
;;  the terms of this license.
;;  You must not remove this notice, or any other, from this software.

(ns io.simplect.compose
  (:require
   [cats.core					:as cats]
   [clojure.algo.generic.functor		:as functor]
   [clojure.core				:as core]
   [clojure.spec.alpha				:as s]
   ))

(defn- var-arglist-and-doc
  [fvar]
  (select-keys (meta fvar) [:arglists :doc]))

(defn- merge-meta
  [target-var m]
  (alter-meta! target-var #(merge % m)))

(defmacro fref
  [nm fname]
  `(let [m# (var-arglist-and-doc (var ~fname))]
     (def ~nm ~fname)
     (merge-meta (var ~nm) (update-in m# [:doc] #(str "Notation for [[" '~fname "]].\n\n" %)))
     '~nm))
(alter-meta! #'fref #(assoc % :private true))

(defn- build-sym
  [nm]
  (symbol (str (ns-name *ns*)) (name nm)))

(defn- def-*
  [nm docstring form]
  `(do ~(if docstring
          `(def ~nm ~docstring ~form)
          `(def ~nm ~form))
       (alter-meta! (var ~nm) #(assoc % :private true))
       '~nm))

(defmacro def-
  "Like [[clojure.core/def]] except `nm` is private."
  ([nm form]
   (def-* nm nil form))
  ([nm docstring form]
   (def-* nm docstring form)))

(defn- fdef*
  [nm specpred-or-spec docstring form]
  (let [sym (build-sym nm)
        M (meta nm)]
    `(do ~(if docstring
            `(def ~nm ~docstring ~form)
            `(def ~nm ~form))
         ~(when specpred-or-spec
            `(s/fdef ~nm :args ~(if (or (symbol? specpred-or-spec) (keyword? specpred-or-spec))
                                  `(s/cat ~(keyword (name (gensym))) ~specpred-or-spec)
                                  specpred-or-spec)))
         '~sym)))

(defmacro fdef
  "Defines a function with specification.  The resulting function is NOT instrumented.

  Example:

        user> (fdef my-add-3 int? (fn [x] (+ x 3)))
        user/my-add-3
        user> (clojure.spec.test.alpha/instrument `my-add-3)
        user> (my-add-3 2)
        5
        user> (my-add-3 2.0)
        Execution error - invalid arguments to io.simplect.compose/my-add-3 at (REPL:5).
        2.0 - failed: int? at: [:G__8490]
        user>"
  ([nm specpred-or-spec docstring form]
   (fdef* nm specpred-or-spec docstring form))
  ([nm specpred-or-docstring form]
   (if (string? specpred-or-docstring)
     (fdef* nm nil specpred-or-docstring form)
     (fdef* nm specpred-or-docstring nil form)))
  [[nm form]
   (fdef* nm nil nil form)])

(defn- fdef-*
  [nm specpred docstring form]
  (let [sym (build-sym nm)]
    `(do
       ~(fdef* nm specpred docstring form)
       (alter-meta! (var ~sym) #(assoc % :private true))
       '~sym)))

(defmacro fdef-
  "Like [[fdef]] except defines a private function."
  ([nm specpred docstring form]
   (fdef-* nm specpred docstring form))
  ([nm specpred-or-docstring form]
   (if (string? specpred-or-docstring)
     (fdef-* nm nil specpred-or-docstring form)
     (fdef-* nm specpred-or-docstring nil form)))
  [[nm form]
   (fdef-* nm nil nil form)])

(defn- sdefn*
  [nm docstring arglist spec body]
  (let [sym (build-sym nm)]
    `(do
       (defn ~nm
         ~@(when docstring [docstring])
         ~arglist
         ~@body)
       (s/fdef ~sym :args ~(if (or (symbol? spec)
                                   (keyword? spec))
                             `(s/cat ~(-> (gensym) name keyword) ~spec)
                             spec))
       '~sym)))

(defmacro sdefn
  "Defines a functions like [[clojure.core/defn]] but adds specification `spec`. Note that the
  function is NOT instrumented.

  Example: ```
    (sdefn add-to-int
      (s/cat :int int?)
      \"Add 2 to `v` which must be an integer.\"
      [v]
      (+ v 2))
    (clojure.spec.alpha/instrument `add-to-int)

  user> (add-to-int 4)
  6
  user> (try (add-to-int 4.0) (catch Exception _ :err))
  :err
  ```"
  [nm spec docstring arglist & body]
  (sdefn* nm docstring arglist spec body))

(defn- sdefn-*
  [nm docstring arglist spec body]
  (let [sym (build-sym nm)]
    `(do
       ~(sdefn* nm docstring arglist spec body)
       (alter-meta! (var ~sym) #(assoc % :private true))
       '~sym)))

(defmacro sdefn-
  "Defines private function like [[clojure.core/defn]] but adds specification `spec`.
  See [[sdefn]] for example of use.  Note that function is NOT instrumented."
  ([nm spec docstring arglist & body]
   (sdefn-* nm docstring arglist spec body)))

(defmacro redefn
  "Defines `name` to be `refname` and updates its meta-data keys `:arglists` and `:doc` to be those of
  `refname` thereby enabling tools to show its argument lists and documentation.  Useful for
  functions."
  [name refname]
  `(do (def ~name ~refname)
       (alter-meta! (var ~name)
                    #(merge % {:arglists (-> (var ~refname) meta :arglists)
                               :doc (-> (var ~refname) meta :doc)}))
       (var ~name)))

(defn >->>
  "Returns a function which calls `f` with arguments reordered such that the first argument is given
  to `f` as the last.

  The name of the function is meant to suggest that `f` is converted to fit into a '->' context by
  mapping argument order from `->`-style (missing arg inserted first) to '->>'-style (missing arg
  inserted last).

  Can be called without arguments in which case a function reordering arguments is
  returned (cf. `ex3` in the example below).

  Example:

        user> (-> {:a 1}
                  (assoc :b 9)
                  ((>->> map) str))
        (\"[:a 1]\" \"[:b 9]\")
        user>
        (let [f (fn [& args] args)
              map> (>->> map)]
          {:ex1 ((>->> f) 1 2 3 4 5),
           :ex2 (-> (range 5)
                    ((>->> map) (partial * 10)))
           :ex3 (-> (range 5)
                    (map> (partial * 1000)))})
        {:ex1 (2 3 4 5 1),
         :ex2 (0 10 20 30 40),
         :ex3 (0 1000 2000 3000 4000)}
        user>"
  [f]
  (fn [& args]
    (apply f (let [fst (first args)]
               (if fst
                 (concat (rest args) (list fst))
                 '())))))

(defn >>->
  "Returns a function which calls `f` with arguments reordered such that last argument is given to `f`
  as the first.

  The name of the function is meant to suggest that `f` is converted to fit into a '->>' context by
  mapping argument order from `->>`-style (arg last) to '->'-style (arg first).

  Can be called without arguments in which case a function reordering arguments is
  returned (cf. `ex3` in the example below).

  Example:

       user> (->> {:a 1}
                  ((>>-> assoc) :b 1))
       {:a 1, :b 1}
       user> (let [f (fn [& args] args)
                   assoc>> (>>-> assoc)]
               {:ex1 ((>>-> f) 1 2 3 4 5),
                :ex2 (->> {:a 1}
                          ((>>-> assoc) :b 2))
                :ex3 (->> {:a 1}
                         (assoc>> :b 2))})
       {:ex1 (5 1 2 3 4),
        :ex2 {:a 1, :b 2},
        :ex3 {:a 1, :b 2}}
        user>"
  [f]
  (fn [& args]
    (apply f (let [lst (last args)]
               (if lst
                 (cons lst (butlast args))
                 '())))))

(defmacro invoke
  "Call function `f`."
  [f & args]
  `(.invoke ^clojure.lang.IFn ~f ~@args))

(defn rcomp
  "Compose `fs` in order.  Like [[clojure.core/comp]] except applies `fs` in the order they appear
  (reverse order relative to [[comp]]). `io.simplect.compose` defines the short-hand notation [[Γ]]
  for [[rcomp]] and [[γ]] for [[clojure.core/comp]]."
  [& fs]
  (apply comp (reverse fs)))

(defn raptial
  "Like `partial` except it will insert the argument accepted by the returned function to the *front*
  of `args` (as opposed to [[partial]] which adds the argument *after* those given to it).

  ```
        user> (let [f (c/raptial assoc :a 1)
                     g #(assoc % :a 1)
                     m {:b 2}]
                 (= (f m) (g m)))
        true
        user> {:ex1 ((raptial assoc :x 2) {:a 1})
               :ex2 (->> [{:a 1} {:v -1}]
                         (map (raptial assoc :x 2)))}
        {:ex1 {:a 1, :x 2},
         :ex2 ({:a 1, :x 2} {:v -1, :x 2})}
        user>
  ```
  `io.simplect.compose` defines the short-hand notation `π` for `raptial` and `Π` for
  `clojure.core/partial`."
  [f & args]
  (fn [& args2]
    (if-let [fst (first args2)]
      (apply f (cons fst (concat args (rest args2))))
      (apply f args))))

(defn reorder
  "Returns a function which calls `f` with arguments reordered according to `v` which must be a
  sequential collection of integers all less than the number of arguments.

  Example:

        user> (let [f (fn [& args] args)
                    g (reorder [0 3 2 0] f)]
                (g :a :b :c :d :e))
        (:a :d :c :a)
        user>"
  [v f]
  (fn [& args]
    (let [mx (apply max v)]
      (if (< mx (count args))
        (apply f (map (partial nth args) v))
        (throw (ex-info (str "reorder: " mx " arg index too large ")
                 {:v v, :max-index mx, :args args}))))))

(defmacro curry
  "Same as `cats.core/curry` from the Funcool Cats library, its docstring reproduced below for
  convenience.

  Given either a fixed arity function or an arity and a function, return another which is curried.

  With inferred arity (function must have one fixed arity):

      (defn add2 [x y] (+ x y))
      (def cadd2 (curry add2))

      ((cadd2 1) 3)
      ;; => 4

      (cadd2 1 3)
      ;; => 4

  With given arity:

      (def c+ (curry 3 +))

      ((c+ 1 2) 3)
      ;; => 6

      ((((c+) 1) 2) 3)
      ;; => 6

  `io.simplect.compose` defines the short-hand notation `Χ` for [[curry]]."
  [& args]
  `(cats/curry ~@args))

;; Add fmap from clojure.algo.generic.functor
(fref fmap clojure.algo.generic.functor/fmap)

(defn call-if
  "Takes a single argument.  If applying `pred` to the argument yields a truthy value returns the
  result of applying `f` to the argument, otherwise returns the argument itself."
  [pred f]
  #(if (pred %) (f %) %))

;;; ----------------------------------------------------------------------------------------------------
;;; NOTATION - DEFINITIONS BELOW MERELY INTRODUCES ALTERNATIVE SHORT NAMES (NO NEW FUNCTIONALITY)
;;; ----------------------------------------------------------------------------------------------------

(fref π	core/partial)
(fref p	core/partial)

(fref Π	raptial)
(fref P	raptial)

(fref γ	core/comp)
(fref c	core/comp)

(fref Γ	rcomp)
(fref C	rcomp)

(fref μ	core/map)
(fref m	core/map)

(fref ρ	core/reduce)
(fref r	core/reduce)

(defmacro χ
  "Abbreviated form of [[io.simplect.compose/curry]]."
  [& args]
  `(io.simplect.compose/curry ~@args))
(merge-meta #'χ (var-arglist-and-doc #'io.simplect.compose/curry))
(alter-meta! #'χ (Π update-in [:doc] (π str "Abbreviated form of [[io.simplect.compose/curry]].\n\n")))

(defmacro λ
  "Same as [[clojure.core/fn]]."
  [& args]
  `(fn ~@args))
(merge-meta #'λ (var-arglist-and-doc #'fn))
(alter-meta! #'λ (Π update-in [:doc] (π str "Abbreviated form of [[clojure.core/fn]].\n\n")))

(comment
  (defn- guard
    [pred then else]
    #(if (pred %) (then %) (else %)))
  (defn guard-when
    [pred then]
    (guard pred then identity))
  (defn guard-when-not
    [pred then]
    (guard-when (complement pred) then)))
