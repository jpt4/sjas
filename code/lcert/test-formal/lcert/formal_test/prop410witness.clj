(ns lcert.formal-test.prop410witness
  "A17: a closed identity derivation yields an accepted certificate and K."
  (:require [clojure.test :refer [deftest is testing]]
            [lcert.formal.base :as b]
            [lcert.formal.prop410witness]))

(deftest a17-proposition-410-witness
  (testing "the paper's closed identity and fixed-budget witness are kernel declarations"
    (is (b/has? 'id00_closed))
    (is (b/has? 'prop410_witness)))
  (testing "a bare leaf is not an accepted identity certificate"
    (is (b/rejects? '[]
                    '(Eq Bool
                         (Check decCert (Code.sl 0)
                           (encE (Exp.tPi U.u1 Exp.tEmpty Exp.tEmpty)))
                         Bool.true)
                    '[(rfl)]))))
