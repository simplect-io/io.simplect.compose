(ns io.simplect.compose.action
  (:require
   [clojure.pprint				:as pp]
   [io.simplect.compose						:refer [def- γ Γ π Π >>-> >->>]]))

;;; ----------------------------------------------------------------------------------------------------
;;; PROTOCOLS
;;; ----------------------------------------------------------------------------------------------------

(defprotocol step-proto
  (step-spec		[_])
  (step-fn		[_]))

(defprotocol action-proto
  (completed?		[_])
  (done?		[_])
  (halted?		[_])
  (reset		[_])
  (step-specs		[_])
  (steps		[_])
  (success?		[_]))

(defprotocol result-proto
  (completed-steps	[_])
  (failure		[_])
  (failure-step		[_])
  (failure-string	[_])
  (failure-value	[_])
  (output		[_]))

;;; ----------------------------------------------------------------------------------------------------
;;; STEP
;;; ----------------------------------------------------------------------------------------------------

(deftype Step [_fn _spec]
  step-proto
  (step-fn	[_]	_fn)
  (step-spec	[_]	_spec)
  Object
  (toString	[_]	(str "#step<" (with-out-str (print _spec)) ">"))
  clojure.lang.IFn
  (invoke	[_]	(.invoke _fn {}))
  (invoke	[_ S]	(.invoke _fn S)))
(alter-meta! #'->Step (Π assoc :private true))

(defmethod print-method Step
  [^Step st ^java.io.Writer w]
  (if pp/*print-pretty*
    (binding [*out* (pp/get-pretty-writer w)
              pp/*print-suppress-namespaces* true]
      (pp/pprint-logical-block
       (.write w "#step<")
       (pp/pprint-logical-block (pp/write (step-spec st)))
       (.write w ">")))
    (.write w (str st))))

(defn- vector-step? [v] (and (vector? v) (-> v first symbol?)))

(defn- _stepdata
  [f]
  (cond
    (vector-step? f)		f
    (vector? f)			f
    (symbol? f)			f
    (keyword? f)		f
    :else			(str f)))

(defn- _stepfn
  [f]
  (cond
    (= f [])		identity
    (nil? f)		identity
    (fn? f)		(let [{:keys [side-effect]} (meta f)]
                          (if side-effect (fn [S] (f) S) f))
    (symbol? f)		(ns-resolve *ns* f)
    (vector-step? f)	(let [[op & args] f
                              f' #(apply (ns-resolve *ns* op) args)]
                          (fn [S] (f') S))
    :else		   (throw
                            (Exception.
                             (str "Bad action step - "
                                  "Must be either a function or a vector "
                                  "whose first element is a symbol: " f)))))

;;; ----------------------------------------------------------------------------------------------------
;;; ACTION
;;; ----------------------------------------------------------------------------------------------------

(declare ->Action halt?)
(defn- invoke-action
  [ac S steps]
  (let [[res step failure completed-steps]
        ,, (reduce (fn [[SS _ _ completed-steps] step]
                     (let [f (step-fn step)
                           [fail? SS'] (try [false (f SS)]
                                            (catch Throwable e
                                              [true e]))]
                       (cond
                         fail? 		(reduced [SS step SS' completed-steps])
                         (halt? SS')	(reduced [SS' step nil completed-steps])
                         :else		[SS' nil nil (conj completed-steps step)])))
                   [S nil nil []]
                   steps)
        success? (not failure)
        halted? (halt? res)
        res (dissoc res :action/halt)]
    (->Action completed-steps failure step halted? res steps)))

(defn- _mkaction
  [steps]
  (->Action nil nil nil nil nil steps))

(deftype Action [_completed-steps _failure _failure-step _halted? _output _steps]
  action-proto
  (completed?		[_]	(= (count _completed-steps) (count _steps)))
  (done?		[a]	(boolean (or (completed? a) _halted? _failure)))
  (halted?		[_]	(boolean _halted?))
  (reset		[_]	(_mkaction _steps))
  (step-specs		[_]	(mapv step-spec _steps))
  (steps		[_]	_steps)
  (success?		[a]	(boolean (and (done? a) (not _failure))))
  result-proto
  (completed-steps	[_]	_completed-steps)
  (failure		[_]	_failure)
  (failure-step		[_]	_failure-step)
  (failure-string	[_]	(str _failure))
  (output		[_]	_output)
  clojure.lang.IFn
  (invoke	[action]	(invoke-action action {} _steps))
  (invoke	[action S]	(invoke-action action S _steps)))

(alter-meta! #'->Action (Π assoc :private true))

(defmethod print-method Action
  [^Action ac ^java.io.Writer w]
  (if pp/*print-pretty*
    (binding [*out* (pp/get-pretty-writer w)
              pp/*print-suppress-namespaces* true]
      (.write w "#action")
      (pp/pprint-logical-block
       (pp/write (merge {:steps (steps ac),
                         :success? (success? ac),
                         :completed? (completed? ac),
                         :done? (done? ac)}
                        (when (output ac)
                          {:output (output ac)})
                        (when (failure ac)
                          {:failure-step (failure-step ac), :failure-string (failure-string ac),})
                        (when (halted? ac)
                          {:halted? true, :halt-step (failure-step ac)})
                        (when (done? ac)
                          {:completed-steps (count (completed-steps ac))})))))
    (.write w (str "#action[" ac "]"))))

(declare step step? action?)
(defn- _steps
  [v]
  (vec (cond
         (nil? v)		[]
         (step? v)		[v]
         (action? v)		(steps v)
         (fn? v)		[(step v (str v))]
         (vector-step? v)	[(step v v)]
         :else			(throw (Exception. (str "Bad Action-step: " v))))))

;;; ----------------------------------------------------------------------------------------------------
;;; INTERFACE
;;; ----------------------------------------------------------------------------------------------------

(defn step
  "Returns a Step, which is an atomic element in an Action and must be a Step Function: A function
  which accepts and returns a map which represents the state of the Action.  Actions consistent of a
  sequence of zero or more Steps.

  `step-fn` must be one of:

  - A Step Function: A function accepting and returning a map
  - A symbol referencing Step Function
  - A vector whose first element is a symbol which resolves to a function which will be called with
    the remaining members of the vector
  - `nil` - indicating a no-operation Step
  - `[]` - the empty vector, indicating a no-operation Step

  `step-spec` is the data that is optionally associated with Steps, it can be any value."
  ([vstep]
   (if (vector-step? vstep)
     (step vstep vstep)
     #_(step vstep (_stepdata vstep))
     (throw (Exception. (str "Must be a vector-spec: " vstep)))))
  ([step-fn step-spec]
   (->Step (_stepfn step-fn) step-spec)))

(def step?
  (π instance? Step))

(defn side-effect
  "Returns a Step Function which acts as the identity function with respect to the map when called,
  but calling `step-fn` before the map is returned.  `step-fn` must be a thunk (a zero-argument
  function)."
  [step-fn step-spec]
  (if (fn? step-fn)
    (step (fn [S] (step-fn) S) step-spec)
    (throw (Exception. (str "Must be a function: " step-fn)))))

(defn action
  ([]
   (action nil))
  ([v]
   (_mkaction (_steps v)))
  ([v1 v2]
   (_mkaction (vec (concat (_steps v1) (_steps v2)))))
  ([v1 v2 & vs]
   (apply action (action v1 v2) vs)))

(def action?
  (π instance? Action))

(def halt!
  "Modifies the state to indicate stop further processing."
  (Π assoc :action/halt true))

(def halt?
  "Given a state, returns `true` iff `halt!` has been used to indicate processing stop and `false`
  otherwise."
  (Γ (Π get :action/halt) boolean))
