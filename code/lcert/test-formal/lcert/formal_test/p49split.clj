(ns lcert.formal-test.p49split
  "The node-case evidence splitter, not the all-depth Proposition 4.9."
  (:require [clojure.test :refer [deftest is]]
            [lcert.formal.base :as b]
            [lcert.formal.p49split]))

(deftest dependent-boolean-split
  (is (b/has? 'p49split_typed))
  ;; Dropping the conjunction hypothesis must not be justified by the
  ;; splitter: it is not a proof of both arbitrary Boolean propositions.
  (is (b/rejects? '[chkf :- (=> Code Code Bool)]
        '(Rt chkf (List.nil Exp) (List.nil U) p49split_term
           (Exp.tPi U.uw Exp.tBool
             (Exp.tPi U.uw Exp.tBool
               (Exp.tSig U.u1 (Exp.tT (Exp.var 1))
                 (Exp.tT (Exp.var 1))))))
        '[(exact (p49split_typed chkf))])))
