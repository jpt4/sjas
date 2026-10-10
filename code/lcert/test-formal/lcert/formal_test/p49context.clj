(ns lcert.formal-test.p49context
  (:require [clojure.test :refer [deftest is]]
            [lcert.formal.base :as b]
            [lcert.formal.p49context]))

(deftest context-substitution
  (doseq [nm '[p49plug_subst p49plug_lift p49plug_repl]]
    (is (b/has? nm) (str nm)))
  ;; A hole environment is essential: the context must not capture an
  ;; ambient variable when it is moved under a binder.
  (is (b/rejects? '[]
        '(Eq Exp
           (lift 1 0 (p49plug (P49C.hole 0) (fn [i :- Nat] (Exp.var i))))
           (p49plug (P49C.hole 0) (fn [i :- Nat] (Exp.var i))))
        '[(rfl)])))
