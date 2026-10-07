(ns lcert.formal-test.patransport
  (:require [lcert.formal.patransport]
            [clojure.test :refer [deftest is testing]]
            [lcert.formal.base :as b]))
(deftest packaged-transport
  (doseq [nm '[paC_TRANSPORT paTransportTy paC_TRANSPORT_rt paTransport_form]]
    (is (b/has? nm) (str nm)))
  (is (not (b/rejects? '[chkf :- (=> Code Code Bool)]
    '(Rt chkf (List.nil Exp) (List.nil U) paC_TRANSPORT paTransportTy)
    '[(exact (paC_TRANSPORT_rt chkf))])))
  (testing "the type is exactly x y f. EQ x y -> T(f x) -> T(f y)"
    (is (not (b/rejects? []
      '(Eq Exp paTransportTy
        (Exp.tPi U.uw Exp.tNat
          (Exp.tPi U.uw Exp.tNat
            (Exp.tPi U.uw (Exp.tPi U.uw Exp.tNat Exp.tBool)
              (Exp.tPi U.uw (Exp.tT (Exp.app (Exp.app trEQ (Exp.var 2)) (Exp.var 1)))
                (Exp.tPi U.uw (Exp.tT (Exp.app (Exp.var 1) (Exp.var 3)))
                  (Exp.tT (Exp.app (Exp.var 2) (Exp.var 3)))))))))
      '[(rfl)]))))
  (testing "transport is a function requiring equality evidence, not a refutation"
    (is (b/rejects? '[chkf :- (=> Code Code Bool)]
      '(Rt chkf (List.nil Exp) (List.nil U) paC_TRANSPORT Exp.tEmpty)
      '[(exact (paC_TRANSPORT_rt chkf))]))))
