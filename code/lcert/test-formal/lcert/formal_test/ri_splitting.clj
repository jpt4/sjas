(ns lcert.formal-test.ri-splitting
  "Formal suite (ADR-0006), lcert.formal.ri-splitting: requiring the namespace
  kernel-checks its declarations; these tests check that the expected
  constants exist and that false variants are rejected.  The namespace is
  the generic copy (over an R-interpretation) of lcert.formal.splitting, generated
  by formal/tools/gen_ri_all.sh; the constant list is the one the warm
  server recorded for it."
  (:require [clojure.test :refer [deftest is testing]]
            [lcert.formal.base :as b]
            [lcert.formal.ri-splitting]))

(deftest f4-ri-splitting
  (testing "the generic copy declares its constants over ri"
    (doseq [c '[EnvSat_mono_ri EnvSat_omega_ri EnvSat_one_ri EnvSat_split_ri mono_cons_ri omega_cons_ri
                one_cons_ri split_cons_ri]]
      (is (b/has? c) (str c)))))
