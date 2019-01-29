(ns io.simplect.functional
  (:require
   [cats.core					:as cats]
   [clojure.algo.generic.functor		:as functor]
   [clojure.spec.alpha				:as s]
   ))

(defn- build-sym [nm] (symbol (str (ns-name *ns*)) (name nm)))

(defn- def-*
  [nm docstring form]
  `(do ~(if docstring
          `(def ~nm ~docstring ~form)
          `(def ~nm ~form))
       (alter-meta! (var ~nm) #(assoc % :private true))
       '~nm))

(defmacro def-
  "Like `clojure.core/def` except `nm` is private to namespace."
  ([nm form]
   (def-* nm nil form))
  ([nm docstring form]
   (def-* nm docstring form)))


(defn- fdef*
  [nm specpred docstring form]
  (let [sym (build-sym nm)]
    `(do ~(if docstring
            `(def ~nm ~docstring ~form)
            `(def ~nm ~form))
         ~(when specpred
            `(s/fdef ~nm :args (s/cat ~(keyword (name (gensym))) ~specpred)))
         ~(when specpred
            `(t/instrument '~sym))
         '~sym)))

(defmacro fdef
  "Defines a function accepting a single value using `def` (instead of
  `defn`).  The resulting function is instrumented to be checked
  against `specpred` which must be a predicate function taking a
  single argument."
  ([nm specpred docstring form]
   (fdef* nm specpred docstring form))
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
  "Like `fdef` except defines a private function."
  ([nm specpred docstring form]
   (fdef-* nm specpred docstring form))
  ([nm specpred-or-docstring form]
   (if (string? specpred-or-docstring)
     (fdef-* nm nil specpred-or-docstring form)
     (fdef-* nm specpred-or-docstring nil form)))
  [[nm form]
   (fdef-* nm nil nil form)])

(def ^:dynamic *instrument-defins*
  "Instruments functions defined using `defin*` if evaluating this var
  returns a truthy value, otherwise functions are `unstrument`ed.

  If `*instrument-defins*` is bound to a function, evaluation is
  performed by calling the function without arguments, otherwise the
  result of evaluation is the value of `*instrument-defins*."
  true)

(defn- sdefn*
  "Define instrumented function.  Like `clojure.core/defn` but
  instruments function to be checked against `spec`."
  [nm docstring arglist spec body]
  (let [sym (build-sym nm)]
    `(do
       (defn ~nm
         ~@(when docstring [docstring])
         ~arglist
         ~@body)
       (s/fdef ~sym :args ~spec)
       ~(if *instrument-defins*
          `(t/instrument '~sym)
          `(t/unstrument '~sym))
       '~sym)))

(defmacro sdefn
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
  "Liks `defin` except defines a private function."
  ([nm spec docstring arglist & body]
   (sdefn-* nm docstring arglist spec body)))

(defn >->>
  "Calls `f` with arguments reordered such that the first argument to
  `>->>` is given to `f` as the last.  The name of the function is
  meant to suggest that `f` is converted to fit into a '->' context by
  mapping argument order from `->`-style (arg first) to
  '->>'-style (arg last).

  Can be called without arguments in which case a function reordering
  arguments is returned (cf. `ex3` below).

  Example:

        user> (let [f (fn [& args] args)
                    map> (>->> map)]
                {:ex1 (>->> 1 f 2 3 4 5),
                 :ex2 (-> (range 5)
                          (>->> map (partial * 10)))
                 :ex3 (-> (range 5)
                          (map> (partial * 1000)))})
        {:ex1 (2 3 4 5 1),
         :ex2 (0 10 20 30 40),
         :ex3 (0 1000 2000 3000 4000)}
        user>"
  ([v & f-and-args]
   (let [f (first f-and-args), f-args (concat (rest f-and-args) [v])]
     (apply f f-args)))
  ([f]
   (fn [& args] (apply f (concat (rest args) [(first args)])))))

(defn >>->
  "Calls `f` with arguments reordered such that last argument to `>>->`
  is given to `f` as the first. The name of the function is meant to
  suggest that `f` is converted to fit into a '->>' context by mapping
  argument order from `->>`-style (arg last) to '->'-style (arg
  first).

  Can be called without arguments in which case a function reordering
  arguments is returned (cf. `ex3` below).

  Example:

        user> (let [f (fn [& args] args)
                    assoc>> (>>-> assoc)]
                {:ex1 (>>-> f 1 2 3 4 5),
                 :ex2 (->> {:a 1}
                           (>>-> assoc :b 2))
                 :ex3 (->> {:a 1}
                           (assoc>> :b 2))
                 :ex4 ((partial >>-> assoc :b 2) {:a 1})})
        {:ex1 (5 1 2 3 4),
         :ex2 {:a 1, :b 2},
         :ex3 {:a 1, :b 2}
         :ex4 {:a 1, :b 2}}
        user>"
  ([f & args]
   (let [f-args (list* (last args) (butlast args))]
     (apply f f-args)))
  ([f]
   (fn [& args] (apply f (concat (list (last args)) (butlast args))))))

(defn rcomp [& fs]
  "Like `clojure.core/comp` except applies `fs` in reverse order."
  (apply comp (reverse fs)))

(def chain
  "Like `clojure.core/comp` except applies `fs` in reverse
  order. Deprecated.  Use `rcomp` instead."
  rcomp)

(def fmap
  "Same as `clojure.algo.generic.functor/fmap`."
  functor/fmap)

(defn partial>
  "Like `partial` except it will insert the argument accepted by the
  returned function between first and second elements of `args` (as
  opposed to `partial` which adds the argument after those given to
  it).

  Example:

        user> {:ex1 ((partial> assoc :x 2) {:a 1})
               :ex2 (->> [{:a 1} {:v -1}]
                         (map (partial> assoc :x 2)))}
        {:ex1 {:a 1, :x 2},
         :ex2 ({:a 1, :x 2} {:v -1, :x 2})}
        user>"
  [& args]
  (apply partial >>-> args))

(defmacro curry
  [& args]
  `(cats/curry ~@args))

(defmacro call
  "Call `f` with `v` as an argument.  Essentially a no-op because
  `(= (f v) (call f v))` will hold for all pure, total functions. Use
  to accentuate the function call occurs in complex expressions."
  [f v]
  `(~f ~v))

(defmacro call-with
  "Call `f` with `v` as an argument.  Same as `call` with arguments
  switched.  Same purpose, to make clear where the function call
  occurs in complex expressions."
  [v f]
  `(~f ~v))

(defn callee
  "Returns a function which, when called with a function `f`, returns
  the result of applying `f` to the argument `v`.  Intended to be used
  mapping of over a collection of (config) functions.

  Example:

        navigate> (map (callee 11) [inc dec (partial * 10)])
        (12 10 110)"
  [v]
  (fn [f] (f v)))

(defn conjreduce
  "Reduces collection `coll` using `conj` of the result of applying `f`
  to each element.  If `incl?` is specified it must a single-argument
  function used as a predicate indicating whether the element should
  be included in the reduction.  `incl?` defaults to `(constantly
  true)`.

  Example:

        user> (conjreduce :a [{:a 1, :b 2} {:a 2, :b 1}])
        [1 2]
        user> (conjreduce {:incl? (comp odd? :b)} :a [{:a 1, :b 2} {:a 2, :b 1}])
        [2]
        user>
  "
  ([f coll]
   (conjreduce {} f coll))
  ([{:keys [incl? init] :or {init [], incl? (constantly true)}} f coll]
   (reduce (fn [α v] (let [r (f v)] (if (incl? v) (conj α r) α))) init coll)))

(defn assocreduce
  "Reduces collection `coll` applying `assoc` of the result of calling
  `f` to each element.  `f` must return a pair (vector of 2 elements):
  the key and val of the resulting entry in the returned map.  If
  `incl?` is specified it must a single-argument function used as a
  predicate indicating whether the element should be included in the
  reduction.  `incl?` defaults to `(constantly true)`.

  Example:

        user> 
        
        user> (assocreduce (fn [[k v]] [k (:a v)])
                           {:one {:a 1, :b 2} :two {:a 2, :b 1}})
        {:one 1, :two 2}
        user> (assocreduce {:incl? (comp odd? :b second)}
                           (fn [[k v]] [k (:a v)])
                           {:one {:a 1, :b 2} :two {:a 2, :b 1}})
        {:two 2}
        user>
  "
  ([f coll]
   (assocreduce {} f coll))
  ([{:keys [init incl?] :or {init {}, incl? (constantly true)}} f coll]
   (reduce (fn [α v] (let [r (f v)] (if (incl? v) (apply assoc α r) α))) init coll)))

