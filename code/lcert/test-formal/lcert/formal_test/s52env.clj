(ns lcert.formal-test.s52env
  "Theorem 5.2: all-entry and erasing environments, restriction, and lookup."
  (:require [clojure.test :refer [deftest is testing]]
            [lcert.formal.base :as b]
            [lcert.formal.s52env]))

(deftest s52-environments
  (testing "environment construction, restriction, formation, and lookup"
    (doseq [c '[entry52_all entry52_zero entry52_nonzero entry52_or entry52_mono
                env52_nil env52_cons env52_cons_rel env52_sub
                wfs_cons wfs_tail wfs_head wfs_of_wfctx wfs_theta wfs_var
                s52_lift_fam s52_lift2_fam var52]]
      (is (b/has? c) (str c))))
  (testing "erasure permits an unrelated zero entry, but cannot promote it"
    (is (not (b/rejects? [] '(Entry52 Bool.true U.u0 False)
                         '[(exact True.intro)])))
    (is (b/rejects? '[h :- (Entry52 Bool.true U.u0 False)]
                    '(Entry52 Bool.true U.u1 False) '[(exact h)])))
  (testing "non-erasing environments still constrain zero entries"
    (is (b/rejects? [] '(Entry52 Bool.false U.u0 False)
                    '[(exact True.intro)]))))
