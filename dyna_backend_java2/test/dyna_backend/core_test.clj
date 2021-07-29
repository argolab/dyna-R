(ns dyna-backend.core-test
  (:require [clojure.test :refer :all]
            [dyna-backend.core :refer :all]
            [dyna-backend.rexpr :refer :all]
            [dyna-backend.context :as context]))


(deftest basic-rexpr
  (let [rexpr (make-add (make-constant 2) (make-constant 3) (make-constant 5))
        r2 (simplify rexpr)
        r3 (make-multiplicity 1)]
    (is (= r2 r3))))


(deftest basic-rexpr2
  (let [rexpr (make-add (make-constant 2) (make-constant 3) (make-variable 'var1))
        ctx (context/make-empty-context rexpr)
        r2 (context/bind-context ctx (simplify rexpr))
        r3 (make-unify (make-variable 'var1) (make-constant 5))]
    (is (= r2 r3))
    (println ctx)))



(deftest basic-conjunct
  (let [rexpr (make-conjunct
                [(make-add (make-constant 2) (make-variable 'v1) (make-variable 'v2))
                 (make-add (make-constant 2) (make-constant 3) (make-variable 'v1'))])
        ctx (context/make-empty-context rexpr)
        r2 (context/bind-context ctx (simplify-fully rexpr))
        r3 (make-multiplicity 1)]


    (is (= r2 r3))))







;; (deftest basic-rexpr
;;   (let [expr '(if expression )])
;;   )
