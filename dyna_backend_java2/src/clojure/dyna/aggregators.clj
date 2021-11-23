(ns dyna.aggregators
  (:require [dyna.utils :refer :all])
  (:require [dyna.base-protocols :refer :all])
  (:require [dyna.rexpr :refer :all])
  (:require [dyna.rexpr-builtins :refer [make-lessthan make-lessthan-eq
                                         make-add make-times make-min make-max make-lor make-land]])
  (:require [dyna.term :refer :all])
  (:import (dyna UnificationFailure DynaTerm DynaUserError))
  (:import [dyna.rexpr aggregator-rexpr])
  (:require [dyna.context :as context]))

(def aggregators (atom {}))

(defn def-aggregator [name & args]
  (let [kw-args (apply hash-map args)
        args2 (if (nil? (:identity kw-args))
                (assoc kw-args :identity ((:combine kw-args))) ;; the zero argument call to the combine function should return the identity
                kw-args)
        args3 (assoc args2 :name name)]
    ;; this should construct the other functions if they don't already exist, so that could mean that there are some defaults for everything
    ;; when the aggregator is created, it can have whatever oeprations are
    (swap! aggregators assoc name args3)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-aggregator "+="
  :combine +
  :many-items *
  :rexpr-binary-op make-add)

(comment
  (def-aggregator "*="
    :combine *
    :many-items (fn [x mult] (Math/pow x mult))
    :rexpr-binary-op make-times))


;; we can define 'advanced' aggregators which will allow this to pass through
;; some information, something like all of the values will be >= 0, in which
;; case we can take whatever is the current value as an upper bound.  Could then
;; propose that there could be rewrites which annotate that the result of some
;; expression will be positive, in which case it could "convert" from one
;; aggregator to another or something.  This would allow for it to somehow focus
;; its time on branches which are going to be the most useful.
(comment (def-aggregator "prob+="
           :combine +))

(comment
  (def-aggregator "="
    ;; in the case that there are two different, then that is an error, though not 100% sure where the unification failure is going to pop up in this case???
    :combine (fn [a b]
               (if (not= a b)
                 (throw (UnificationFailure. "The equals aggregator (=) requires only one contribution"))
                 a))))

;; used if there are multiple aggregators on a single rule which need to get combined together
;; this will throw an exception if we attempt to combine multiple expressions together at once
;; though maybe this should just be some error state that propagates around as a user value or something instead of an exception
(comment
  (def-aggregator "only_one_contrib"
    :combine (fn [a b]
               (throw (DynaUserError. "multiple aggregators on the same rule")))))

(defn- get-aggregated-value [v]
  (if (and (instance? DynaTerm v)
           (= (.name ^DynaTerm v) "$with_key_pair"))
    (get (.arguments ^DynaTerm v) 1)
    v))


;; maybe the add-to-rexpr-incoming should take an R-expr rather than just taking
;; whatever is the current value to the expression.  In the case that there is
;; an R-expr, that would make the representation of the expression a bit easier
;; for the compiler to work with rather than an opaque function which it can't
;; observe?  Though with the function, it could just use that to construct the
;; R-expr and then it can just keep the R-expr around?  So that would allow it
;; to avoid having to have some lambda names or something.  I suppoose that it
;; could have some current value expression with this looking through

(comment
  (def-aggregator "max="
    :combine (fn [a b] (if (> (get-aggregated-value a) (get-aggregated-value b)) a b))
    :allows-with-key true
    ;; this will let us add expressions to the R-expr so that this can eleminate branches which are not useful
    :add-to-rexpr-incoming (fn [current-value incoming-variable]  ;; this would mean that there needs to be some optional stuff for these, or some way in which assumptions can be tracked for these branches.  We don't want to store these expressions in the case that
                             (make-lessthan-eq (make-constant current-value) incoming-variable))
    :add-to-rexpr-result (fn [current-value result-variable]
                           (make-lessthan-eq (make-constant current-value) result-variable))
    :rexpr-binary-op make-max)


  (def-aggregator "min="
    :combine (fn [a b] (if (< (get-aggregated-value a) (get-aggregated-value b)) a b))
    :allows-with-key true
    ;; this add-to-rexpr will have to know if with-key is included in the expression.
    ;; this would mean that it somehow removes the unification in the case
    :add-to-rexpr-incoming (fn [current-value incoming-variable]
                             (make-lessthan-eq incoming-variable (make-constant current-value)))
    ;; adding some information to the result of the expression would allow for
    ;; this to indicate that the resulting value will at least be greater than
    ;; what the current expression is.  having some lessthan expression added on
    ;; the result side will allow for this to replicate alpha-beta pruning as a
    ;; strategy These lessthan expressions should then be able to combine together
    ;; to eleminate branches
    :add-to-rexpr-result (fn [current-value result-variable]
                           (make-lessthan-eq result-variable (make-constant current-value)))
    :rexpr-binary-op make-min))

(comment
  (def-aggregator ":-"
    :combine (fn [a b] (or a b))
    :saturate #(= true %))
  ;; this aggregator is already going to have that the incoming variable is unified with the expression which corresponds with

  (def-aggregator "|="
    :combine (fn [a b] (or a b))
    :saturate #(= true %)
    :add-to-rexpr (fn [current-value incoming-variable]
                    (if (= current-value false)
                      (make-unify incoming-variable (make-constant true))))
    :rexpr-binary-op make-lor)

  (def-aggregator "&="
    :combine (fn [a b] (and a b))
    :saturate #(= false %)
    :add-to-rexpr (fn [current-value incoming-variable]
                    (if (= current-value true)
                      ;; then this could add some unify the result with false as that is the only thing which can change the value of this expression
                      ;; but could that causes this expression to run something that wouldn't have run otherwise?
                      (make-unify incoming-variable (make-constant false))))
    :rexpr-binary-op make-land))

;; a global counter which is used for all := across the entire program in the
;; case of nested dynabases, this will not necessarily allow for overriding a
;; value with another expression as this will not know the "order" in which expressions are contributed
(def ^:private colon-equals-counter (atom 0))
(defn get-colon-equals-count [] (swap! colon-equals-counter inc))

(comment
  (def-aggregator ":="
    :combine (fn [a b]
               (let [[la va] (.arguments ^DynaTerm a)
                     [lb vb] (.arguments ^DynaTerm b)]
                 (if (> lb la) b a)))
    :check-input (fn [x]
                   (and
                    (instance? DynaTerm x)
                    (= "$colon_line_tracking" (.name ^DynaTerm x))))
    :add-to-rexpr (let [linevar (make-variable (gensym))
                        valvar (make-variable (gensym))]
                    (fn [current-value incoming-variable]
                      (let [[line val] (.arguments ^DynaTerm current-value)]
                        (make-proj-many [linevar valvar] (make-conjunct [
                                                                         (make-unify-structure incoming-variable (make-constant nil)
                                                                                               "$colon_line_tracking" [linevar valvar])
                                                                         (make-lessthan-eq (make-constant line) linevar)]))))))


                                        ; this should merge a map together.  This would not have which of the expressions would
  ;; (def-aggregator "merge="
  ;;   :combine merge
  ;;   :check-input (partial instance? DynaMap)
  ;;   )

  (def-aggregator "?="
    :combine (fn [a b] a) ;; we just have to choose something
    :saturate (fn [x] true)))  ;; this saturates once it gets something, so we can
    ;; just ignore whatever value is selected, we don't
    ;; have to keep running this value.  Having this as a
    ;; function means that we can make this do whatever
    ;; we want with the representation


(comment
  (def-rewrite
    :match (aggregator (:unchecked operator) (:any result-variable) (:any incoming-variable)
                       (:unchecked body-is-conjunctive) (:rexpr R))
    :run-at :construction
    (when (and (is-unify? R)
               (= (:a R) incoming-variable)
               (is-bound? (:b R)))  ;; I think that we don't need to care if the other side of the unfication is ground, though maybe only if the body is conjunctive, or we can always remove the aggregator
      (make-unify result-variable (:b R))))

  (def-rewrite
    :match (aggregator (:unchecked operator) (:any result-variable) (:any incoming-variable)
                       (:unchecked body-is-conjunctive) (:rexpr R))
    :run-at :construction
    (when (and (is-multiplicity? R)
               (= (:mult R) 0))
      (if body-is-conjunctive
        (make-multiplicity 0)
        (make-unify result-variable (:identity (get @aggregators operator))))))

  (comment
    (def-rewrite
      :match (aggregator (:unchecked operator) (:any result-variable) (:any incoming-variable)
                         (:unchecked body-is-conjunctive) (:rexpr R))
      :run-at :construction
      (let [aop (get @aggregators operator)
            agg-val (atom nil)
            combine-op (fn [x]
                         (if (nil? @agg-val)
                           (reset! agg-val x)
                           (swap! agg-val (:combine aop) x)))]

        (and (is-disjunct? R)
             ;; if there is some branch which just has the assignment to the
             ;; incoming variable, then we should attempt to perform

             (let [body1 (doall (remove (for [disj (:args R)]
                                          (if (and (is-unify? disj)
                                                   (= (:a disj) incoming-variable))
                                            (let [val (get-value (:b disj))]
                                              (if-not (nil? val)
                                                (do
                                                  (combine-op val)
                                                  nil))
                                              disj)
                                            disj))))

                   ;; this could do the add to incoming R-exprs thing here
                   body2 (if-not (nil? @agg-val) (conj body1 (make-unify incoming-variable @agg-val)))]
               (if (empty? body1)
                 (make-unify result-variable @agg-val)  ;; there is nothing else
                 ;; that remains, so just
                 ;; return the result of aggregation
                 ;; make a new aggregator with the body which has combined expressions together
                 (make-aggregator operoator result-variable incoming-variable
                                  body-is-conjunctive body2)))))))



  (def-rewrite
    :match (aggregator (:unchecked operator) (:any result-variable)
                       (:any incoming-variable) (:unchecked body-is-conjunctive)
                       (:rexpr R))
    (do (debug-repl)
        (print "--------------------------------------------"))))


(def-rewrite
  :match (aggregator (:unchecked operator) (:any result-variable) (:any incoming-variable) (:unchecked body-is-conjunctive) (:rexpr R))
  (let [aop (get @aggregators operator)
        ctx (context/make-nested-context-aggregator R incoming-variable body-is-conjunctive)]
        ;agg-val (atom nil)

    ;; (if (is-disjunct? R))
    ;; (let [new-disjunct-children (doall (for [child (:args R)]
    ;;                                      (let [lctx (context/make-nested-context-disjunct child)
    ;;                                            new-rexpr (context/bind-context-raw lctx child)]

    ;;                                        [new-rexpr lctx])))])
    (let [nR (context/bind-context ctx (simplify R))]
      (assert (= true body-is-conjunctive))
      (if (is-bound-in-context? incoming-variable ctx)
        (do
          (if (is-multiplicity? nR)
            ;; then we need to multiply in the result
            (do
                (case (:mult nR)
                  0 (make-multiplicity 0)
                  1 (make-unify result-variable (make-constant (get-value-in-context incoming-variable ctx)))
                  (make-unify result-variable (make-constant
                                               ((:many-items aop)
                                                (get-value-in-context incoming-variable ctx)
                                                (:mult nR))))))
            (make-aggregator operator result-variable incoming-variable body-is-conjunctive
                             (make-conjunct [(make-unify incoming-variable
                                                         (get-value-in-context incoming-variable ctx))
                                             nR]))))
        (make-aggregator operator result-variable incoming-variable body-is-conjunctive nR)))))