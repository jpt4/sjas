(ns lcert.formal-test.arrfamily
  "Formal suite (ADR-0006), lcert.formal.arrfamily: the paper's §4.4 family
  A_j = 1 ⊸ ⋯ ⊸ 1, its closedness, code size and certifiability at Check."
  (:require [clojure.test :refer [deftest is testing]]
            [lcert.formal.base :as b]
            [lcert.formal.arrfamily]))

(deftest arr-family-shape
  (testing "A_j is closed and its F7 code has at least j nodes"
    (is (not (b/rejects? '[j :- Nat]
          '(Eq Bool (closedTy (arrN j)) Bool.true)
          '[(exact (arrN_closed j))])))
    (is (not (b/rejects? '[j :- Nat]
          '(LE.le j (cnodes (encE (arrN j))))
          '[(exact (arrN_size j))])))
    (is (b/rejects? '[]
          '(Eq Exp (arrN 2) (Exp.tPi U.u1 Exp.tUnit Exp.tUnit))
          '[(rfl)]))))

(deftest arr-family-certifiable
  (testing "A_j has a label-safe accepted certificate at Check, at every j"
    (doseq [nm '[lamTree_check arrForm_check dataOk_lamTree dataOk_arrForm]]
      (is (b/has? nm) (str nm)))
    (is (not (b/rejects? '[j :- Nat] '(Certifiable (arrN j))
                         '[(exact (arrN_certifiable j))])))))
