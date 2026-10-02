(ns lcert.formal-test.skeletons
  "Formal suite (ADR-0006), lcert.formal.skeletons: requiring the namespace
  kernel-checks its declarations; these tests check that the expected
  constants exist and that false variants are rejected."
  (:require [clojure.test :refer [deftest is testing]]
            [lcert.formal.base :as b]
            [lcert.formal.skeletons]))
(deftest f2e-lemma-2-5
  (testing "Lemma 2.5: conversion preserves skeletons; derivations are skeleton-typed"
    (doseq [c '[skel_subst step_skel cv_skel lemma25_tl lemma25_tl_term lemma25_tl_type lemma25_rt]]
      (is (b/has? c) (str c))))
  (testing "the hypotheses are necessary (kernel-checked counterexamples)"
    (is (b/has? 'skel_subst_needs_unit))
    (is (b/has? 'step_skel_needs_wf)))
  (testing "a derivation's skeleton is not arbitrary"
    (is (b/rejects? '[] '(SkJ Bool.false (List.nil Sk) Exp.tt Sk.nat) '[(constructor)]))))
