(ns lcert.formal-test.extract-counts
  "ADR-0008: the extracted F7 encoder must reproduce kernel-checked counts.

  The counts and closed fixtures were established independently in
  prop411size and prop412size; this test compiles their checked fixture data,
  passes it to the extracted encCert, then counts the generated Code nodes."
  (:require [clojure.test :refer [deftest is testing]]
            [lcert.extracted.runtime :as rt]
            [lcert.formal.prop411poly]
            [lcert.formal.prop412poly]
            [lcert.formal.extract :as extract]))

(defn- checked-value [name]
  (extract/compile-closure! name))

(deftest extracted-encoder-matches-proposition-411
  (let [cn (checked-value 'sh4_cn)
        tree (checked-value 'sh4_cn_tree)
        form (checked-value 'sh4_c0_form)
        bush (checked-value 'sh4_bush)
        code-tree (checked-value 'codeTree)
        code-term (checked-value 'codeTerm)]
    (doseq [[n shared literal] [[2 331 152] [4 717 780]
                                [6 1231 3772] [8 1873 17660]]]
      (testing (str "n=" n)
        (is (= shared (rt/cnodes (rt/encCert 0 (cn n) [5] (tree n) (form)))))
        (let [b (bush n)]
          (is (= literal
                 (rt/cnodes (rt/encCert 0 (code-term b) [5]
                                       (code-tree b) (form))))))))))

(deftest extracted-encoder-matches-proposition-412
  ;; The nullary components are checked definitions too. Compile them first
  ;; so every bare reference in the extracted doubler is a data-producing call.
  (doseq [name '[sh4_M sh4_dbl_body sh4_dbl_base sh4_dbl_step]]
    (checked-value name))
  (let [doubler (checked-value 'sh4_doubler)
        numeral (checked-value 'sh4_num)
        motive (checked-value 'sh4_M)
        tree (checked-value 'sh4_doubler_tree)
        form (checked-value 'sh4_doubler_form_tree)]
    (doseq [[n nodes] [[4 400] [8 450] [12 516]]]
      (testing (str "n=" n)
        (is (= nodes
               (rt/cnodes (rt/encCert 0 (doubler (numeral n)) (motive)
                                     (tree n) (form)))))))))
