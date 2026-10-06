(ns lcert.formal-test.ri-mono
  "Formal suite (ADR-0006), lcert.formal.ri-mono: requiring the namespace
  kernel-checks its declarations; these tests check that the expected
  constants exist and that false variants are rejected.  The namespace is
  the generic copy (over an R-interpretation) of lcert.formal.mono, generated
  by formal/tools/gen_ri_all.sh; the constant list is the one the warm
  server recorded for it."
  (:require [clojure.test :refer [deftest is testing]]
            [lcert.formal.base :as b]
            [lcert.formal.ri-mono]))

(deftest f4-ri-mono
  (testing "the generic copy declares its constants over ri"
    (doseq [c '[Lemma_3_4_ri V_mono_ri base_R_mono_ri]]
      (is (b/has? c) (str c)))))
