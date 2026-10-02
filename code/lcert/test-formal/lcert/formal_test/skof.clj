(ns lcert.formal-test.skof
  "Formal suite (ADR-0006), lcert.formal.skof: requiring the namespace
  kernel-checks its declarations; these tests check that the expected
  constants exist and that false variants are rejected."
  (:require [clojure.test :refer [deftest is testing]]
            [lcert.formal.base :as b]
            [lcert.formal.skof]))
(deftest f3k-skof-completeness
  (testing "skOf is complete on runtime terms at clean types; skeleton-typed expressions are clean"
    (doseq [c '[clean_tPi clean_app skj_clean cv_wf_left skOf_const skOf_rt skOf_tl]]
      (is (b/has? c) (str c))))
  (testing "a branch list has no inferred skeleton, which is why the clean premise is needed"
    (is (not (b/rejects? '[G :- (List Sk)] '(Eq (Option Sk) (skOf G Exp.bnil) (Option.none Sk)) '[(rfl)])))))
