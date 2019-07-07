(ns io.simplect.compose.action-test
  ;; We want to test side-effects, but prefer to do so in a controlled manner.  To accomplish this
  ;; we use an atom, bound to `*state*`, containing the state that the tests affect, making it
  ;; straightforward control the scope of the effects and to inspect the impact of the side-effects.
  (:require
   [clojure.test				:as t		:refer [deftest is]]
   [clojure.test.check				:as tc]
   [clojure.test.check.generators		:as gen]
   [clojure.test.check.properties		:as prop]
   [io.simplect.compose						:refer [def- invoke γ Γ π Π]]
   [io.simplect.compose.action			:as action	:refer :all]))

(def- QC-ITERS 1000)

;;; ----------------------------------------------------------------------------------------------------
;;; SUPPORTING FORMS
;;; ----------------------------------------------------------------------------------------------------

(def ^{:dynamic true, :private true} *initial-state* {})
(def ^{:dynamic true, :private true} *state*)

(defmacro bind-initial-state
  "If not otherwise specified, tests start with the state atom contain the value of this dynamic var."
  [expr & body]
  `(binding [*initial-state* ~expr]
     ~@body))

(defmacro from-initial-state
  "Evaluate `body` with `*state*` containing the value of `*initial-state*` at the beginning of
  evaulation."
  [& body]
  `(binding [*state* (atom *initial-state*)]
     ~@body))

(defmacro state-after
  "Return the value contained in `*state*` after evaluating `body`."
  [& body]
  `(do ~@body @*state*))

(defmacro transforming-state
  "Return the value contained in `*state*` after evaluating `body` with an initial state value of
  `initial-state`."
  [initial-state & body]
  `(bind-initial-state ~initial-state
     (from-initial-state
       (state-after ~@body))))

(defmacro bind-post [[post-action post-state] {:keys [init-state action-input action]} expr]
  (let [init-state (or init-state {})
        action-input (or action-input {})
        action (or action (action/action))]
    `(let [[~post-action ~post-state]
           (bind-initial-state ~init-state
             (from-initial-state
               [(.invoke ~action ~action-input) @*state*]))]
      ~expr)))

(doseq [var [#'bind-initial-state #'bind-post #'from-initial-state #'state-after #'transforming-state]]
  (alter-meta! var (Π assoc :private true :style/indent :defn)))

;;; ----------------------------------------------------------------------------------------------------
;;; HELPER FUNCTIONS
;;; ----------------------------------------------------------------------------------------------------

(defn- update-state [f] (swap! *state* f))
(defn- get-state [] (deref *state*))

(defn- PUSH [v] (update-state #((fnil conj ()) % v)))
(defn- POP [& _] (update-state rest))
(defn- maptop
  [f]
  (update-state
   (fn [S]
     (let [top (or (first S) 0)]
       (conj (rest S) (f top))))))
(defn ADD [v] (maptop (Π + v)))
(defn SUB [v] (maptop (Π - v)))
(defn MUL [v] (maptop (Π * v)))
(defn DIV [v] (maptop (Π / v)))

(defmacro run-stack [& body]
  `(transforming-state () ~@body))

(defn run-action
  [action]
  (run-stack (.invoke action)))

(defn- random-partitioning
  "Returns a random partitioning of `coll` as a vector of vectors which contain at least `min-size`
  and at most `max-size` elements.  The last partition may be smaller than `min-size`, but
  contains at least one element.  `max-size` must be at least as big as `min-size`, both must be
  positive integers."
  ([coll max-size]
   (random-partitioning coll max-size 1))
  ([coll max-size min-size]
   (assert (pos? min-size))
   (assert (>= max-size min-size))
   (let [delta (inc (- max-size min-size))]
     (loop [partitions [], vs coll]
       (if (seq vs)
         (let [n (+ (rand-int delta) min-size)]
           (recur (conj partitions (vec (take n vs)))
                  (drop n vs)))
         partitions)))))

;;; ----------------------------------------------------------------------------------------------------
;;; GENERATORS
;;; ----------------------------------------------------------------------------------------------------

(def g-opval	(gen/tuple (gen/frequency
                            [[100 (gen/elements `[ADD SUB MUL DIV])]
                             [8   (gen/elements `[PUSH])] ;; more PUSH than POP, or we'll have empty stack often
                             [7   (gen/elements `[POP])]])
                           (gen/choose 1 100)))

(def g-mkstep	(gen/elements [ ;; ways to instantiate an Action step
                               (fn [[op val]] (side-effect #(op val) [op val]))
                               (fn [[op val]] (fn [S] (op val) S))
                               (fn [[op val]] (vector op val))
                               (fn [[op val]] (step (fn [S] (op val) S) [op val]))
                               (fn [[op val]] (step (vector op val) [op val]))
                               (fn [[op val]] (step (vector op val)))
                               (constantly nil)]))

(def g-step	(gen/let [opval	g-opval
                          op g-mkstep]
                  (gen/return (op opval))))

(def g-action	(gen/let [n (gen/choose 0 100)
                          steps	(gen/return (gen/sample g-step n))]
                  (gen/return (apply action steps))))

(def g-identity	(gen/elements [nil
                               identity ^:side-effects identity
                               (step nil nil) (step [] [])
                               (action nil)]))

;;; ----------------------------------------------------------------------------------------------------
;;; BASIC CHECK
;;; ----------------------------------------------------------------------------------------------------

(def actions (π apply action))
(def run-actions (Γ actions run-action))

(deftest basic-check
  (is (= '(2 1)	(run-actions `[[PUSH 1] [PUSH 2]])))
  (is (= '(4 1) (run-actions `[[PUSH 1] [PUSH 2] [MUL 2]])))
  (is (= '(1 1) (run-actions `[[PUSH 1] [PUSH 2] [DIV 2]])))
  (is (= '(1)	(run-actions `[[PUSH 1] [PUSH 2] [POP nil]])))
  (is (= '(2)	(run-actions `[[PUSH 2]])))
  (is (= '()	(run-actions `[[POP 9]])))
  (is (= '(0)	(run-actions `[[MUL 2]])))
  (is (= '(0)	(run-actions `[[DIV 99]]))))

(deftest action-output-correct
  (is (let [sym1 (gensym)
            sym2 (gensym)
            n (rand-int 1000000)]
        (bind-post [A' S'] {:init-state (),
                            :action-input {:sym1 sym1}
                            :action (action [`PUSH n]
                                            (Π assoc :sym2 sym2))}
          (and (= {:sym1 sym1, :sym2 sym2} (action/output A'))
               (= S' (list n)))))))

;;; ----------------------------------------------------------------------------------------------------
;;; MONOIDAL PROPERTIES
;;; ----------------------------------------------------------------------------------------------------

(def prop--action-is-associative
  "Side-effects are not affected by how the action is assembled."
  (prop/for-all [s1 g-action
                 s2 g-action
                 s3 g-action]
    (= (run-action (action (action s1         s2) s3))
       (run-action (action         s1 (action s2  s3))))))

(deftest action-is-associative
  (is (:pass? (tc/quick-check QC-ITERS prop--action-is-associative))))

(def prop--action-has-identity
  "If you combine an action with a null-action, the side-effects are unchanged."
  (prop/for-all [id g-identity
                 ac g-action]
    (= (run-action ac)
       (run-action (action ac id))
       (run-action (action id ac)))))

(deftest action-has-identity
  (is (:pass? (tc/quick-check QC-ITERS prop--action-has-identity))))

;;; ----------------------------------------------------------------------------------------------------
;;; OTHER ALGEBRAIC
;;; ----------------------------------------------------------------------------------------------------

(def prop--actions-combine-arbitrarily
  (prop/for-all [steps (gen/vector g-step)
                 max-size (gen/choose 1 10)]
    (let [p1 (random-partitioning steps max-size)
          p2 (random-partitioning steps max-size)
          a1 (vec (map actions p1))
          a2 (vec (map actions p2))
          r1 (run-action (actions a1))
          r2 (run-action (actions a2))]
      (= r1 r2))))

(deftest actions-combine-arbitrarily
  (is (:pass? (tc/quick-check QC-ITERS prop--actions-combine-arbitrarily))))

;;; ----------------------------------------------------------------------------------------------------
;;; STEP SPECS
;;; ----------------------------------------------------------------------------------------------------

(def g-action-with-specs
  (gen/let [n (gen/choose 5 100)
            opvals (gen/return (gen/sample g-opval n))
            specs (gen/return (gen/sample gen/any n))
            mkstep (gen/elements [step (fn [[op val] spec] (side-effect #(op val) spec))])
            steps (gen/return (map mkstep opvals specs))
            max-size (gen/choose 2 5)
            partitioning (gen/return (random-partitioning steps max-size))
            α (gen/return (map actions partitioning))]
    (gen/return {:partitioning partitioning, :specs specs, :action (actions α)})))

(def prop--actions-retain-specs
  "If you attach a spec to a step, it can be retrieved later."
  (prop/for-all [aws g-action-with-specs]
    (let [{:keys [specs action]} aws]
      (= specs (step-specs action)))))

(deftest actions-retain-specs
  (is (:pass? (tc/quick-check QC-ITERS prop--actions-retain-specs))))

