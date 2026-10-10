(ns lcert.formal-test.p49tele
  (:require [clojure.test :refer [deftest is]]
            [lcert.formal.base :as b]
            [lcert.formal.p49tele]))

(deftest telescope-indexing
  (is (b/has? 'p49tele_one))
  (is (b/has? 'p49tele_two))
  ;; The two code holes must stay distinct after both depth evidence binders.
  (is (b/rejects? '[B :- (=> Nat Exp Exp), j :- Nat, k :- Nat]
        '(Eq Exp
           (p49tele B (P49C.node 7 (P49C.hole 0) (P49C.hole 1))
             (List.cons (Prod Nat Nat) (Prod.mk 0 j)
               (List.cons (Prod Nat Nat) (Prod.mk 1 k) (List.nil (Prod Nat Nat))))
             (fn [h :- Nat] (Exp.var h)))
           (p49tele B (P49C.node 7 (P49C.hole 0) (P49C.hole 0))
             (List.cons (Prod Nat Nat) (Prod.mk 0 j)
               (List.cons (Prod Nat Nat) (Prod.mk 1 k) (List.nil (Prod Nat Nat))))
             (fn [h :- Nat] (Exp.var h))))
        '[(rfl)])))
