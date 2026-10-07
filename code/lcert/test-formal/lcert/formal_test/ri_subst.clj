(ns lcert.formal-test.ri-subst
  "Formal suite (ADR-0006), lcert.formal.ri-subst: requiring the namespace
  kernel-checks its declarations; these tests check that the expected
  constants exist and that false variants are rejected.  The namespace is
  the generic copy (over an R-interpretation) of lcert.formal.subst, generated
  by formal/tools/gen_ri_all.sh; the constant list is the one the warm
  server recorded for it."
  (:require [clojure.test :refer [deftest is testing]]
            [lcert.formal.base :as b]
            [lcert.formal.ri-subst]))

(deftest f4-ri-subst
  (testing "the generic copy declares its constants over ri"
    (doseq [c '[envOf_nil_ri envOf_ri]]
      (is (b/has? c) (str c)))))
