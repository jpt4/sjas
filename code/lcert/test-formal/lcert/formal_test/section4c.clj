(ns lcert.formal-test.section4c
  "Formal suite (ADR-0006), lcert.formal.section4c: Propositions 4.11, 4.12 (1)
  and 4.9 in part.  Requiring the namespace kernel-checks its declarations;
  these tests check that the expected constants exist and that false variants
  are rejected."
  (:require [clojure.test :refer [deftest is testing]]
            [lcert.formal.base :as b]
            [lcert.formal.section4c]))

(deftest f4-section4c
  (testing "Proposition 4.11: the sharing chain, its denotation, and the node count"
    (doseq [c '[sh4_pow_succ sh4_bush_succ sh4_grow_zero sh4_grow_succ
                sh4_open_zero sh4_open_succ sh4_cn_unfold
                sh4_lenU_cons sh4_lenE_cons sh4_lenU_vzero sh4_len_head
                sh4_vadd_zz sh4_vscale_z sh4_vadd_uw sh4_vadd_u0uw sh4_vscale_uw
                sh4_vadd_node sh4_vadd_app sh4_nthE0 sh4_nthU0 sh4_nonzero_w
                sh4_var sh4_lbl sh4_snode sh4_open_step sh4_open_typed prop411_typed
                sh4_sk_sleaf sh4_sk_snode sh4_den_var0 sh4_den_snode
                sh4_open_den_step sh4_open_den sh4_grow_double sh4_grow_bush prop411_den
                sh4_twice sh4_bush_size sh4_sub_succ sh4_sub_add1 prop411_nodes prop411]]
      (is (b/has? c) (str c))))
  (testing "three doublings are seven internal nodes, and the chain is a Syn term"
    (is (not (b/rejects? '[] '(Eq Nat (cnodes (sh4_bush 3)) 7) '[(exact (prop411_nodes 3))])))
    (is (not (b/rejects? '[chkf :- (=> Code Code Bool)]
                         '(Rt chkf (List.nil Exp) (List.nil U) (sh4_cn 2) Exp.tSyn)
                         '[(exact (prop411_typed chkf 2))]))))
  (testing "the leaf chain is not a node, and it is not a term of R"
    (is (b/rejects? '[chkf :- (=> Code Code Bool),
                      dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
                      encTy :- (=> Exp Code),
                      cap :- Nat]
                    '(Eq Code (den chkf dec encTy cap (sh4_cn 0) (List.nil Sk) Sk.syn Unit.unit)
                            (Code.sn 0 (Code.sl 0) (Code.sl 0)))
                    '[(exact (prop411_den chkf dec encTy cap 0))]))
    (is (b/rejects? '[chkf :- (=> Code Code Bool)]
                    '(Rt chkf (List.nil Exp) (List.nil U) (sh4_cn 0) Exp.tR)
                    '[(exact (prop411_typed chkf 0))])))
  (testing "Proposition 4.12 (1): the doubling recursor, packaged, at budget 0"
    (doseq [c '[sh4_M sh4_motive_step sh4_motive_sub sh4_star_ty sh4_zero_ty sh4_nz1
                sh4_len_nil sh4_lenL sh4_lenC sh4_lenS sh4_lenP sh4_nth_syn sh4_nthu_w
                sh4_lbl_body sh4_code_var sh4_star_body sh4_snode_body sh4_pair_body
                sh4_acc_var sh4_let_step sh4_base sh4_num_typed sh4_formP prop412_typed
                sh4_den_var1 sh4_den_star sh4_sk_var0 sh4_den_acc sh4_fst sh4_snd
                sh4_den_snode1 sh4_den_body sh4_den_step sh4_den_base sh4_den_num
                sh4_den_doubler prop412_den prop412_nodes prop412]]
      (is (b/has? c) (str c))))
  (testing "at 3 the package has seven internal nodes; at 0 it is a leaf, not a node, and not a Syn term"
    (is (not (b/rejects? '[chkf :- (=> Code Code Bool),
                           dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
                           encTy :- (=> Exp Code), cap :- Nat]
                         '(Eq Nat (cnodes (Prod.fst (den chkf dec encTy cap (sh4_doubler (sh4_num 3))
                                                          (List.nil Sk) (Sk.prod Sk.syn Sk.unit) Unit.unit)))
                                 7)
                         '[(exact (prop412_nodes chkf dec encTy cap 3))])))
    (is (b/rejects? '[chkf :- (=> Code Code Bool),
                      dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
                      encTy :- (=> Exp Code), cap :- Nat]
                    '(Eq (Prod Code Unit)
                         (den chkf dec encTy cap (sh4_doubler (sh4_num 0))
                              (List.nil Sk) (Sk.prod Sk.syn Sk.unit) Unit.unit)
                         (Prod.mk (Code.sn 0 (Code.sl 0) (Code.sl 0)) Unit.unit))
                    '[(exact (prop412_den chkf dec encTy cap 0))]))
    (is (b/rejects? '[chkf :- (=> Code Code Bool)]
                    '(Rt chkf (List.nil Exp) (List.nil U) (sh4_doubler (sh4_num 0)) Exp.tSyn)
                    '[(exact (prop412_typed chkf 0))])))
  (testing "Proposition 4.9, the uniform arm: T(ff) inhabits 0, and depthLeq at 0 is a closed Bool function"
    (doseq [c '[sh4_nbr_ff sh4_nbr_empty sh4_nbr_tff sh4_tt_ty sh4_ff_ty
                sh4_step_hd sh4_cv_ff sh4_from_ff
                sh4_depth_leaf sh4_depth_node sh4_depth_rec sh4_depth0_typed]]
      (is (b/has? c) (str c))))
  (testing "T(ff) is not Unit, and the depth predicate is not a Syn term"
    (is (b/rejects? '[chkf :- (=> Code Code Bool)]
                    '(Rt chkf (List.cons Exp (Exp.tT Exp.ff) (List.nil Exp))
                         (List.cons U U.u1 (List.nil U)) (Exp.var 0) Exp.tUnit)
                    '[(exact (sh4_from_ff chkf))]))
    (is (b/rejects? '[chkf :- (=> Code Code Bool)]
                    '(Rt chkf (List.nil Exp) (List.nil U) (sh4_depth0) Exp.tSyn)
                    '[(exact (sh4_depth0_typed chkf))]))))

