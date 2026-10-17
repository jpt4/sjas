(ns lcert.f7-shell-test
  "The untrusted parser bridge must preserve the term checked by F7."
  (:require [clojure.test :refer [deftest is testing]]
            [lcert.core :as lc]
            [lcert.examples :as ex]
            [lcert.extracted.runtime :as rt]
            [lcert.f7-shell :as f7]
            [lcert.syntax :as syntax]
            [lcert.typing :as typing]))

(def ^:private examples
  ;; Each expected constructor number comes from formal/syntax.clj's Exp
  ;; declaration, independently of the bridge's implementation.
  [[[:Empty] 0] [[:Unit] 1] [[:Bool] 2] [[:Nat] 3]
   [[:Lbl] 4] [[:Syn] 5] [[:Dia] 6] [[:R] 7]
   [[:T [:tt]] 8] [[:Pi 1 [:Bool] [:Bool]] 9]
   [[:Sigma :w [:Nat] [:Nat]] 10] [[:var 2] 11]
   [[:star] 12] [[:abort [:Nat] [:var 0]] 13]
   [[:tt] 14] [[:ff] 15] [[:if [:tt] [:zero] [:zero]] 16]
   [[:elimBool [:Nat] [:tt] [:zero] [:zero]] 17]
   [[:zero] 18] [[:succ [:zero]] 19]
   [[:recN [:Nat] [:zero] [:var 0] [:zero]] 20]
   [[:lbl :a] 21] [[:caseLbl [:Nat] [:lbl :a] [[:zero] [:zero]]] 22]
   [[:sleaf [:lbl :a]] 25]
   [[:snode [:lbl :a] [:sleaf [:lbl :a]] [:sleaf [:lbl :b]]] 26]
   [[:recSyn [:Nat] [:zero] [:zero] [:sleaf [:lbl :a]]] 27]
   [[:leaf [:lbl :a]] 28]
   [[:node [:var 0] [:lbl :a] [:leaf [:lbl :b]] [:leaf [:lbl :c]]] 29]
   [[:itR [:Nat] [:var 0] [:var 0] [:leaf [:lbl :a]]] 30]
   [[:print [:var 0]] 31] [[:lam 0 [:Unit] [:star]] 32]
   [[:app [:var 1] [:var 0]] 33]
   [[:pair [:Sigma 1 [:Unit] [:Unit]] [:star] [:star]] 34]
   [[:let [:Unit] [:var 0] [:star]] 35]
   [[:chk [:var 0] [:var 1]] 36]
   [[:h1 [:var 0] [:var 1] [:var 2] [:var 3] [:var 4]] 37]
   [[:reflect [:Unit] [:var 0] [:var 1]] 38]
   [[:inspect [:Nat] [:var 0] [:var 1] [:zero] [:zero]] 39]])

(deftest parsed-expressions-are-the-checked-expressions
  (doseq [[source tag] examples]
    (testing (pr-str source)
      (let [value (f7/exp source)]
        (is (= tag (first value)))
        (is (= [value] (rt/decE (rt/encE value)))))))
  (testing "usage and labels have their canonical indices"
    (is (= [32 [0] [1] [12]] (f7/exp [:lam 0 [:Unit] [:star]])))
    (is (= [32 [1] [1] [12]] (f7/exp [:lam 1 [:Unit] [:star]])))
    (is (= [32 [2] [1] [12]] (f7/exp [:lam :w [:Unit] [:star]])))
    (is (= [21 97] (f7/exp [:lbl :a]))))
  (testing "malformed expressions cannot be silently recast as a checked term"
    (is (thrown? clojure.lang.ExceptionInfo (f7/exp [:unknown])))
    (is (thrown? clojure.lang.ExceptionInfo (f7/exp [:lam 1 [:Unit]])))
    (is (thrown? clojure.lang.ExceptionInfo (f7/exp [:lbl :not-a-label])))))

(deftest closed-constant-derivations-produce-f7-certificates
  (doseq [[term type] [[[:star] [:Unit]] [[:tt] [:Bool]]
                       [[:ff] [:Bool]] [[:zero] [:Nat]]
                       [[:lbl :a] [:Lbl]]]]
    (testing (pr-str term)
      (let [d (typing/check-top 0 term)
            cert (f7/certificate 0 d)]
        (is (= type (:type d)))
        (is (= [31 nil nil (f7/exp term) (f7/exp type)] (f7/dt d)))
        (is (= [0 nil (f7/exp type)] (f7/dt (typing/tf [] type))))
        (is (true? (rt/Check rt/decCert cert (rt/encE (f7/exp type))))))))
  (let [d (typing/check-top 0 [:star])
        cert (f7/certificate 0 d)]
    (is (false? (rt/Check rt/decCert cert (rt/encE (f7/exp [:Nat])))))
    (is (false? (rt/Check rt/decCert (assoc cert 1 0)
                          (rt/encE (f7/exp [:Unit])))))))

(deftest lambda-derivations-preserve-context-and-usage
  (doseq [term [[:lam 0 [:Unit] [:star]]
                [:lam 1 [:Unit] [:var 0]]
                [:lam :w [:Unit] [:var 0]]]]
    (testing (pr-str term)
      (let [d (typing/check-top 0 term)
            cert (f7/certificate 0 d)
            type (:type d)]
        (is (= :Lam (:rule d)))
        (is (= [9 (case (second term) 0 [0] 1 [1] :w [2]) [1] [1]]
               (f7/exp type)))
        (is (true? (rt/Check rt/decCert cert (rt/encE (f7/exp type)))))
        (is (false? (rt/Check rt/decCert cert (rt/encE (f7/exp [:Unit]))))))))
  (testing "the bound variable records the innermost context entry"
    (let [d (second (:prems (typing/check-top 0 [:lam 1 [:Unit] [:var 0]])))]
      (is (= [30 (list [1]) (list [1]) 0 [1] [1]] (f7/dt d)))))
  (testing "two binders keep the outer variable at de Bruijn index one"
    (let [term [:lam :w [:Unit] [:lam 0 [:Unit] [:var 1]]]
          d (typing/check-top 0 term)
          cert (f7/certificate 0 d)]
      (is (true? (rt/Check rt/decCert cert
                            (rt/encE (f7/exp (:type d)))))))))

(deftest applications-carry-checked-premises
  (doseq [term [[:app [:lam 1 [:Unit] [:var 0]] [:star]]
                [:app [:lam :w [:Unit] [:var 0]] [:star]]
                [:app [:lam :w [:Unit] [:lam 1 [:Unit] [:var 0]]]
                 [:star]]]]
    (testing (pr-str term)
      (let [d (typing/check-top 0 term)
            cert (f7/certificate 0 d)]
        (is (= :App (:rule d)))
        (is (true? (rt/Check rt/decCert cert
                              (rt/encE (f7/exp (:type d))))))
        (is (false? (rt/Check rt/decCert cert
                               (rt/encE (f7/exp [:Bool])))))))))

(deftest erased-applications-have-a-type-level-argument
  (let [term [:app [:lam 0 [:Unit] [:star]] [:star]]
        d (typing/check-top 0 term)
        argument (second (:prems d))
        cert (f7/certificate 0 d)]
    (is (= :has0 (:j argument)))
    (is (= 33 (first (f7/dt d))))
    (is (= 5 (first (f7/dt argument))))
    (is (true? (rt/Check rt/decCert cert (rt/encE (f7/exp [:Unit])))))
    (is (false? (rt/Check rt/decCert cert (rt/encE (f7/exp [:Bool])))))))

(deftest pairs-preserve-sigma-usage
  (doseq [usage [0 1 :w]]
    (testing (str "usage " usage)
      (let [type [:Sigma usage [:Unit] [:Unit]]
            term [:pair type [:star] [:star]]
            d (typing/check-top 0 term)
            cert (f7/certificate 0 d)]
        (is (= :Pair (:rule d)))
        (is (= (if (= usage 0) 35 36) (first (f7/dt d))))
        (is (true? (rt/Check rt/decCert cert
                              (rt/encE (f7/exp type)))))
        (is (false? (rt/Check rt/decCert cert
                               (rt/encE (f7/exp [:Unit])))))))))

(deftest arithmetic-and-branches-have-f7-derivations
  (doseq [[term constructor] [[[:succ [:zero]] 42]
                              [[:if [:tt] [:zero] [:succ [:zero]]] 40]]]
    (testing (pr-str term)
      (let [d (typing/check-top 0 term)
            cert (f7/certificate 0 d)]
        (is (= constructor (first (f7/dt d))))
        (is (= [:Nat] (:type d)))
        (is (true? (rt/Check rt/decCert cert (rt/encE (f7/exp [:Nat])))))
        (is (false? (rt/Check rt/decCert cert (rt/encE (f7/exp [:Bool])))))))))

(deftest dependent-boolean-elimination-has-f7-premises
  (let [term [:elimBool [:Nat] [:tt] [:zero] [:succ [:zero]]]
        d (typing/check-top 0 term)
        cert (f7/certificate 0 d)]
    (is (= :ElimBool (:rule d)))
    (is (= 41 (first (f7/dt d))))
    (is (= [:Nat] (:type d)))
    (is (true? (rt/Check rt/decCert cert (rt/encE (f7/exp [:Nat])))))
    (is (false? (rt/Check rt/decCert cert (rt/encE (f7/exp [:Bool])))))))

(deftest empty-elimination-has-an-f7-derivation
  (let [term [:lam 1 [:Empty] [:abort [:Nat] [:var 0]]]
        d (typing/check-top 0 term)
        cert (f7/certificate 0 d)]
    (is (= :Abort (:rule (second (:prems d)))))
    (is (= 38 (first (f7/dt (second (:prems d))))))
    (is (true? (rt/Check rt/decCert cert (rt/encE (f7/exp (:type d))))))
    (is (false? (rt/Check rt/decCert cert (rt/encE (f7/exp [:Nat])))))))

(deftest natural-number-recursion-has-f7-step-usage
  (let [term [:recN [:Nat] [:zero] [:zero] [:succ [:zero]]]
        d (typing/check-top 0 term)
        cert (f7/certificate 0 d)]
    (is (= :RecN (:rule d)))
    (is (= 43 (first (f7/dt d))))
    (is (true? (rt/Check rt/decCert cert (rt/encE (f7/exp [:Nat])))))
    (is (false? (rt/Check rt/decCert cert (rt/encE (f7/exp [:Bool])))))))

(deftest syntax-tree-recursion-has-f7-method-premises
  (let [term [:recSyn [:Nat] [:zero] [:zero] [:sleaf [:lbl :a]]]
        d (typing/check-top 0 term)
        cert (f7/certificate 0 d)]
    (is (= :RecSyn (:rule d)))
    (is (= 49 (first (f7/dt d))))
    (is (true? (rt/Check rt/decCert cert (rt/encE (f7/exp [:Nat])))))
    (is (false? (rt/Check rt/decCert cert (rt/encE (f7/exp [:Bool])))))))

(deftest certificate-tree-iteration-has-f7-method-premises
  (let [leaf-method [:lam :w [:Lbl] [:zero]]
        node-method [:lam 1 [:Dia]
                     [:lam :w [:Lbl]
                      [:lam 1 [:Nat] [:lam 1 [:Nat] [:var 0]]]]]
        term [:itR [:Nat] leaf-method node-method [:leaf [:lbl :a]]]
        d (typing/check-top 0 term)
        check (fn [type]
                (rt/dtCheck
                 (fn [c expected] (rt/Check rt/decCert c expected))
                 (f7/dt d)
                 [1 nil nil (f7/exp term) (f7/exp type)]))]
    (is (= :ItR (:rule d)))
    (is (= 52 (first (f7/dt d))))
    (is (true? (check [:Nat])))
    (is (false? (check [:Bool])))))

(deftest label-case-branches-have-f7-derivations
  (let [term [:caseLbl [:Nat] [:lbl :a]
              (vec (repeat (count syntax/labels) [:zero]))]
        d (typing/check-top 0 term)
        tree (f7/dt d)
        check (fn [type]
                (rt/dtCheck
                 (fn [c expected] (rt/Check rt/decCert c expected))
                 tree [1 nil nil (f7/exp term) (f7/exp type)]))]
    (is (= :CaseLbl (:rule d)))
    (is (= 44 (first tree)))
    (is (= 46 (first (nth tree 9))))
    (is (true? (check [:Nat])))
    (is (false? (check [:Bool])))))

(deftest h1-uses-the-two-distinct-proof-premises
  (let [r [:leaf [:lbl :a]]
        s [:leaf [:lbl :b]]
        c [:sleaf [:lbl :a]]
        ctx [[1 [:T [:chk [:print r] c]]]
             [1 [:T [:chk [:print s] (syntax/neg-term c)]]]]
        term [:h1 r s c [:var 1] [:var 0]]
        d (typing/synth 1 ctx term)
        tree (f7/dt d)
        judgment [1 (list (f7/exp (second (second ctx)))
                          (f7/exp (second (first ctx))))
                  (list [1] [1]) (f7/exp term) (f7/exp [:Empty])]]
    (is (= :H1 (:rule d)))
    (is (= 55 (first tree)))
    (is (true? (rt/dtCheck
                (fn [code expected] (rt/Check rt/decCert code expected))
                tree judgment)))))

(deftest reflect-uses-the-extracted-base-type-code
  (let [r [:leaf [:lbl :a]]
        target [:Unit]
        code [:sleaf [:lbl :t1]]
        evidence [:T [:chk [:print r] code]]
        ctx [[1 evidence]]
        term [:reflect target r [:var 0]]
        d (typing/synth 1 ctx term)
        tree (f7/dt d)
        judgment [1 (list (f7/exp evidence)) (list [1])
                  (f7/exp term) (f7/exp target)]]
    (is (= :Reflect (:rule d)))
    (is (= (first (rt/baseCode (f7/exp target))) (nth tree 5)))
    (is (true? (rt/dtCheck
                (fn [cert expected] (rt/Check rt/decCert cert expected))
                tree judgment)))))

(deftest inspect-checks-both-branch-assumptions
  (let [r [:leaf [:lbl :a]]
        code [:sleaf [:lbl :t1]]
        claim [:chk [:print [:var 0]] (syntax/shift code 1)]
        positive-function [:Pi 1 [:R] [:Pi 1 [:T claim] [:Unit]]]
        negative-function [:Pi 1 [:R]
                           [:Pi 1 [:T (syntax/not-term claim)] [:Unit]]]
        ctx [[:w positive-function] [:w negative-function]]
        term [:inspect [:Unit] r code
              [:app [:app [:var 3] [:var 1]] [:var 0]]
              [:app [:app [:var 2] [:var 1]] [:var 0]]]
        d (typing/synth 1 ctx term)
        tree (f7/dt d)
        judgment [1 (list (f7/exp negative-function)
                          (f7/exp positive-function))
                  (list [2] [2]) (f7/exp term) (f7/exp [:Unit])]]
    (is (= :Inspect (:rule d)))
    (is (= 57 (first tree)))
    (is (true? (rt/dtCheck
                (fn [cert expected] (rt/Check rt/decCert cert expected))
                tree judgment)))))

(deftest accepted-certificates-bind-caller-type-budget-and-term
  (let [parsed [:zero]
        cert (f7/certificate 0 (typing/check-top 0 parsed))
        accept #(f7/accept-certificate %1 %2 %3 %4)]
    (is (= {:budget 0 :term (f7/exp parsed) :type (f7/exp [:Nat])}
           (select-keys (accept 0 [:Nat] parsed cert)
                        [:budget :term :type])))
    (is (thrown? clojure.lang.ExceptionInfo
                 (accept 0 [:Bool] parsed cert)))
    (is (thrown? clojure.lang.ExceptionInfo
                 (accept 1 [:Nat] parsed cert)))
    (is (thrown? clojure.lang.ExceptionInfo
                 (accept 0 [:Nat] [:succ [:zero]] cert)))
    (is (thrown? clojure.lang.ExceptionInfo
                 (accept 0 [:Nat] parsed (assoc cert 1 0))))))

(deftest code-and-certificate-constructors-use-f7
  (let [a [:lbl :a]
        b [:lbl :b]
        code-leaf [:sleaf a]
        cert-leaf [:leaf a]]
    (doseq [[n term type constructor]
            [[0 code-leaf [:Syn] 47]
             [0 [:snode a code-leaf [:sleaf b]] [:Syn] 48]
             [0 cert-leaf [:R] 50]
             [0 [:print cert-leaf] [:Syn] 53]
             [0 [:chk code-leaf [:sleaf b]] [:Bool] 54]
             [1 [:node [:var 0] a cert-leaf [:leaf b]] [:R] 51]]]
      (testing (pr-str term)
        (let [d (typing/check-top n term)
              cert (f7/certificate n d)]
          (is (= constructor (first (f7/dt d))))
          (is (= type (:type d)))
          (is (true? (rt/Check rt/decCert cert (rt/encE (f7/exp type)))))
          (is (false? (rt/Check rt/decCert cert
                                 (rt/encE (f7/exp [:Nat]))))))))))

(deftest truth-type-formation-is-checked
  (let [proof-type [:T [:tt]]
        term [:lam 1 proof-type [:var 0]]
        d (typing/check-top 0 term)
        cert (f7/certificate 0 d)]
    (is (= :Pi (first (:type d))))
    (is (= 1 (first (f7/dt (typing/tf [] proof-type)))))
    (is (true? (rt/Check rt/decCert cert
                          (rt/encE (f7/exp (:type d))))))
    (is (false? (rt/Check rt/decCert cert
                           (rt/encE (f7/exp [:Bool])))))))

(deftest dependent-truth-type-uses-the-type-level-variable-rule
  (let [type [:Pi 0 [:Bool] [:T [:var 0]]]
        formation (typing/tf [] type)
        tree (f7/dt formation)
        judgment [0 true nil (f7/exp type) [1]]]
    (is (= 2 (first tree)))
    (is (= 1 (first (nth tree 6))))
    (is (= 4 (first (nth (nth tree 6) 3))))
    (is (true? (rt/dtCheck
                (fn [c d] (rt/Check rt/decCert c d)) tree judgment)))))

(deftest dependent-pair-elimination-has-the-two-bound-entries
  (let [pair-type [:Sigma 1 [:Unit] [:Unit]]
        term [:let [:Unit] [:pair pair-type [:star] [:star]] [:var 0]]
        d (typing/check-top 0 term)
        cert (f7/certificate 0 d)]
    (is (= :Let (:rule d)))
    (is (= 37 (first (f7/dt d))))
    (is (true? (rt/Check rt/decCert cert (rt/encE (f7/exp [:Unit])))))
    (is (false? (rt/Check rt/decCert cert (rt/encE (f7/exp [:Bool])))))))

(deftest conversion-derivations-become-f7-chains
  ;; The typer retypes ★ : 1 at T(tt) by a Conv node whose chain is
  ;; [1, T(tt)]: one backward tTT step. F7 records the chain as cvRefl then
  ;; cvBwd, with a skeleton tree for each type and the step's path and head
  ;; record; extracted Check decides the result.
  (let [program [:app [:lam 1 [:T [:tt]] [:var 0]] [:star]]
        derivation (typing/check-top 0 program)
        conv (first (filter #(= :Conv (:rule %)) (tree-seq map? :prems derivation)))
        cert (f7/certificate 0 derivation)]
    (is (= [[:Unit] [:T [:tt]]] (:chain conv)))
    (is (= 39 (first (f7/dt conv))) "a runtime Conv is F7's rConv")
    (is (true? (rt/Check rt/decCert cert (rt/encE (f7/exp (:type derivation))))))
    ;; The application's type is the codomain, T(tt).
    (is (= {:budget 0 :term (f7/exp program) :type (f7/exp [:T [:tt]])}
           (select-keys (f7/accept-certificate 0 [:T [:tt]] program cert) [:budget :term :type]))))
  (testing "skeleton trees follow F7's skeleton rules"
    ;; Π(x : Bool). T(x) is well formed at skeletons; its tree is wPi over
    ;; wBool and wT, whose premise types the variable at Sk.bool.
    (is (= [9 [1] [2] [8 [11 0]] [2] [8 [11 0] [11 0 [1]]]]
           (f7/skeleton-type nil (f7/exp [:Pi 1 [:Bool] [:T [:var 0]]]))))
    (is (thrown? clojure.lang.ExceptionInfo
                 (f7/skeleton-type nil (f7/exp [:T [:zero]])))))
  (testing "a step is found at its redex's path, in either direction"
    (let [chkf (fn [_ _] false)
          a (f7/exp [:Pi 1 [:Bool] [:T [:tt]]])
          b (f7/exp [:Pi 1 [:Bool] [:Unit]])]
      (is (= [0 (list 1) [16] a b] (f7/find-step chkf a b)))
      (is (nil? (f7/find-step chkf b a)))))
  (testing "a tampered chain is rejected by extracted Check"
    (let [derivation (typing/check-top 0 [:app [:lam 1 [:T [:tt]] [:var 0]] [:star]])
          tamper (fn tamper [d]
                   (if (= :Conv (:rule d))
                     (assoc d :chain [[:Unit] [:T [:ff]]])
                     (update d :prems #(mapv tamper %))))]
      (is (thrown? clojure.lang.ExceptionInfo
                   (f7/certificate 0 (tamper derivation)))))))

(deftest conversion-steps-across-head-rules
  ;; find-step identifies the redex and its head rule (check_hd.clj's
  ;; hd-rules tags) inside T(·); extracted stepDTCheck accepts each record.
  (let [chkf (fn [_ _] false)
        step (fn [a b] (f7/find-step chkf (f7/exp a) (f7/exp b)))
        head-tag (fn [a b] (first (nth (step a b) 2)))]
    (is (= 0 (head-tag [:T [:app [:lam 1 [:Bool] [:var 0]] [:tt]]] [:T [:tt]])) "beta")
    (is (= 2 (head-tag [:T [:if [:tt] [:ff] [:tt]]] [:T [:ff]])) "iteT")
    (is (= 3 (head-tag [:T [:if [:ff] [:ff] [:tt]]] [:T [:tt]])) "iteF")
    (is (= 16 (head-tag [:T [:tt]] [:Unit])) "tTT")
    (is (= 17 (head-tag [:T [:ff]] [:Empty])) "tTF")
    (is (= (list 0) (second (step [:T [:if [:tt] [:ff] [:tt]]] [:T [:ff]]))) "the redex is T's child 0")
    ;; Not one step: two contractions apart.
    (is (nil? (step [:T [:if [:tt] [:tt] [:ff]]] [:Unit]))))
  (testing "a two-step chain: T(if tt then tt else ff) ≡ T(tt) ≡ 1"
    (let [program [:app [:lam 1 [:T [:if [:tt] [:tt] [:ff]]] [:var 0]] [:star]]
          derivation (typing/check-top 0 program)
          conv (first (filter #(= :Conv (:rule %)) (tree-seq map? :prems derivation)))
          cert (f7/certificate 0 derivation)]
      (is (= 3 (count (:chain conv))))
      (is (true? (rt/Check rt/decCert cert (rt/encE (f7/exp (:type derivation)))))))))

(deftest type-level-derivations-become-f7-trees
  ;; Type-level (:has0) derivations translate to F7's z* rules. Both
  ;; programs below were rejected by the bridge before: a type-level
  ;; application inside a type, and the tutorial's bounded consistency
  ;; statement at budget 0.
  (let [accepted? (fn [n derivation]
                    (f7/check (f7/certificate n derivation) (f7/exp (:type derivation))))]
    (testing "a type-level β-redex inside T, reached by a two-step chain"
      (let [program [:app [:lam 1 [:T [:app [:lam 0 [:Bool] [:var 0]] [:tt]]] [:var 0]] [:star]]]
        (is (accepted? 0 (typing/check-top 0 program)))))
    (testing "the bounded consistency statement at budget 0"
      (is (accepted? 0 (lc/check-program 0 (ex/bounded-con 0)))))))

;; --- Rule coverage -------------------------------------------------------
;;
;; One closed program per F7 derivation rule, each certified by the bridge
;; and decided by extracted Check through the full certificate path. The
;; coverage test below asserts that, together, they reach every one of
;; F7's 61 rules (check_dt.clj's dt-rules), so a rule the bridge emits but
;; no program reaches cannot hide behind the other tests.

(def ^:private f7-rule-names
  "F7's derivation rules in tag order, as declared in check_dt.clj (tl-rules,
  then rt-rules, then cv-rules). Written out here, independently of the
  bridge, so a renumbering on either side fails the coverage test."
  [:fBase :fT :fPi :fSig :zVar :zConst :zConv :zLam :zApp :zPair :zLet
   :zAbort :zIte :zElimB :zSucc :zRecN :zCaseL :zBnil :zBcons :zSleaf
   :zSnode :zRecS :zLeaf :zNode :zItR :zPrn :zChk :zH1 :zRefl :zInsp
   :rVar :rConst :rLam :rApp0 :rApp :rPair0 :rPair :rLet :rAbort :rConv
   :rIte :rElimB :rSucc :rRecN :rCaseL :rBnil :rBcons :rSleaf :rSnode
   :rRecS :rLeaf :rNode :rItR :rPrn :rChk :rH1 :rRefl :rInsp
   :cvRefl :cvFwd :cvBwd])

(defn- used-at-type-level
  "A closed runtime program that uses `arg`, of type `A`, only as the
  erased argument of an application, so the typer derives it at type level
  (a :has0 derivation, F7's z* rules). `binders` are the types of erased
  binders around it, outermost first; `A` and `arg` may refer to them."
  [binders A arg]
  (reduce (fn [body P] [:lam 0 P body])
          [:app [:lam 0 A [:star]] arg]
          (reverse binders)))

(def ^:private r-a [:leaf [:lbl :a]])
(def ^:private r-b [:leaf [:lbl :b]])
(def ^:private code-a [:sleaf [:lbl :a]])
(def ^:private label-branches (vec (repeat (count syntax/labels) [:zero])))
(def ^:private leaf-method [:lam :w [:Lbl] [:zero]])
(def ^:private node-method
  [:lam 1 [:Dia] [:lam :w [:Lbl] [:lam 1 [:Nat] [:lam 1 [:Nat] [:var 0]]]]])
;; H1's two hypotheses: r proves code-a and s proves its negation.
(def ^:private h1-positive [:T [:chk [:print r-a] code-a]])
(def ^:private h1-negative [:T [:chk [:print r-b] (syntax/neg-term code-a)]])
;; Reflect's evidence: r proves the code of the base type 1 (Unit).
(def ^:private unit-evidence [:T [:chk [:print r-a] [:sleaf [:lbl :t1]]]])

(def ^:private rule-programs
  "[F7 rule name, budget, closed program]. The rule named is the one the
  program was written to reach; coverage is measured, not assumed."
  [;; Type-level rules, reached through an erased argument.
   [:zVar 0 (used-at-type-level [[:Nat]] [:Nat] [:var 0])]
   [:zConst 0 (used-at-type-level [] [:Bool] [:tt])]
   [:zConv 0 (used-at-type-level [[:Unit]] [:T [:tt]] [:var 0])]
   [:zLam 0 (used-at-type-level [] [:Pi 1 [:Nat] [:Nat]] [:lam 1 [:Nat] [:var 0]])]
   [:zApp 0 (used-at-type-level [] [:Nat] [:app [:lam 1 [:Nat] [:var 0]] [:zero]])]
   [:zPair 0 (used-at-type-level [] [:Sigma 1 [:Nat] [:Nat]]
                                 [:pair [:Sigma 1 [:Nat] [:Nat]] [:zero] [:zero]])]
   [:zLet 0 (used-at-type-level [] [:Nat]
                                [:let [:Nat] [:pair [:Sigma 1 [:Nat] [:Nat]] [:zero] [:zero]]
                                 [:var 0]])]
   [:zAbort 0 (used-at-type-level [[:Empty]] [:Nat] [:abort [:Nat] [:var 0]])]
   [:zIte 0 (used-at-type-level [] [:Nat] [:if [:tt] [:zero] [:succ [:zero]]])]
   [:zElimB 0 (used-at-type-level [] [:Nat] [:elimBool [:Nat] [:ff] [:zero] [:zero]])]
   [:zSucc 0 (used-at-type-level [] [:Nat] [:succ [:zero]])]
   [:zRecN 0 (used-at-type-level [] [:Nat] [:recN [:Nat] [:zero] [:var 0] [:succ [:zero]]])]
   [:zCaseL 0 (used-at-type-level [] [:Nat] [:caseLbl [:Nat] [:lbl :b] label-branches])]
   [:zSleaf 0 (used-at-type-level [] [:Syn] code-a)]
   [:zSnode 0 (used-at-type-level [] [:Syn] [:snode [:lbl :a] code-a [:sleaf [:lbl :b]]])]
   [:zRecS 0 (used-at-type-level [] [:Nat] [:recSyn [:Nat] [:zero] [:zero] code-a])]
   [:zLeaf 0 (used-at-type-level [] [:R] r-a)]
   [:zNode 0 (used-at-type-level [[:Dia]] [:R] [:node [:var 0] [:lbl :a] r-a r-b])]
   [:zItR 0 (used-at-type-level [] [:Nat] [:itR [:Nat] leaf-method node-method r-a])]
   [:zPrn 0 (used-at-type-level [] [:Syn] [:print r-a])]
   [:zChk 0 (used-at-type-level [] [:Bool] [:chk code-a [:sleaf [:lbl :b]]])]
   [:zH1 0 (used-at-type-level [h1-positive h1-negative] [:Empty]
                               [:h1 r-a r-b code-a [:var 1] [:var 0]])]
   [:zRefl 0 (used-at-type-level [unit-evidence] [:Unit] [:reflect [:Unit] r-a [:var 0]])]
   [:zInsp 0 (used-at-type-level [] [:Unit] [:inspect [:Unit] r-a code-a [:star] [:star]])]
   ;; Runtime rules.
   [:rVar 0 [:lam 1 [:Unit] [:var 0]]]
   [:rConst 0 [:star]]
   [:rApp0 0 [:app [:lam 0 [:Unit] [:star]] [:star]]]
   [:rApp 0 [:app [:lam 1 [:Unit] [:var 0]] [:star]]]
   [:rPair0 0 [:pair [:Sigma 0 [:Unit] [:Unit]] [:star] [:star]]]
   [:rPair 0 [:pair [:Sigma 1 [:Unit] [:Unit]] [:star] [:star]]]
   [:rLet 0 [:let [:Unit] [:pair [:Sigma 1 [:Unit] [:Unit]] [:star] [:star]] [:var 0]]]
   [:rAbort 0 [:lam 1 [:Empty] [:abort [:Nat] [:var 0]]]]
   ;; ★ : 1 retyped at T(tt): a backward step.
   [:cvBwd 0 [:app [:lam 1 [:T [:tt]] [:var 0]] [:star]]]
   ;; x : T(tt) retyped at 1: a forward step.
   [:cvFwd 0 [:lam 1 [:T [:tt]] [:app [:lam 1 [:Unit] [:var 0]] [:var 0]]]]
   [:rIte 0 [:if [:tt] [:zero] [:succ [:zero]]]]
   [:rElimB 0 [:elimBool [:Nat] [:tt] [:zero] [:succ [:zero]]]]
   [:rRecN 0 [:recN [:Nat] [:zero] [:zero] [:succ [:zero]]]]
   [:rCaseL 0 [:caseLbl [:Nat] [:lbl :a] label-branches]]
   [:rSnode 0 [:snode [:lbl :a] code-a [:sleaf [:lbl :b]]]]
   [:rRecS 0 [:recSyn [:Nat] [:zero] [:zero] code-a]]
   [:rNode 1 [:node [:var 0] [:lbl :a] r-a r-b]]
   [:rItR 0 [:itR [:Nat] leaf-method node-method r-a]]
   [:rPrn 0 [:print r-a]]
   [:rChk 0 [:chk code-a [:sleaf [:lbl :b]]]]
   [:rH1 0 [:lam 1 h1-positive [:lam 1 h1-negative [:h1 r-a r-b code-a [:var 1] [:var 0]]]]]
   [:rRefl 0 [:lam 1 unit-evidence [:reflect [:Unit] r-a [:var 0]]]]
   [:rInsp 0 [:inspect [:Unit] r-a code-a [:star] [:star]]]])

(defn- tree-rules
  "The F7 rule tags of an F7 DT node that dt builds without a recursive
  call: the steps of a conversion chain (the last field of rConv and zConv)
  and the bcons/bnil chain of a label case (the last field of rCaseL and
  zCaseL)."
  [tree]
  (let [chain (fn [node tail-of]
                (loop [n node acc []]
                  (if (nil? n) acc
                      (recur (tail-of n) (conj acc (first n))))))]
    (case (first tree)
      (6 39) (chain (peek tree) #(when (#{59 60} (first %)) (nth % 5)))
      44 (chain (peek tree) #(when (= 46 (first %)) (nth % 8)))
      16 (chain (peek tree) #(when (= 18 (first %)) (nth % 7)))
      [])))

(defn- certify-recording-rules
  "Certify `program` at `budget` and return [derivation certificate tags],
  where tags are the F7 rules of every DT node in the certificate's trees.
  dt is called once per node, including the formation premises it rebuilds,
  so wrapping it observes every node."
  [budget program]
  (let [tags (atom #{})
        dt f7/dt
        derivation (typing/check-top budget program)
        cert (with-redefs [f7/dt (fn [d]
                                   (let [tree (dt d)]
                                     (swap! tags into (cons (first tree) (tree-rules tree)))
                                     tree))]
               (f7/certificate budget derivation))]
    [derivation cert @tags]))

(deftest every-f7-rule-is-reached-and-decided-by-extracted-check
  (let [results (vec (for [[rule budget program] rule-programs]
                  (let [[derivation cert tags] (certify-recording-rules budget program)
                        type (:type derivation)
                        wrong (if (= type [:Bool]) [:Nat] [:Bool])]
                    (testing (str rule " " (pr-str program))
                      (is (true? (f7/check cert (f7/exp type))))
                      (is (false? (f7/check cert (f7/exp wrong))))
                      (is (contains? tags (.indexOf ^java.util.List f7-rule-names rule))
                          "the program reaches the rule it was written for"))
                    tags)))]
    (testing "together the programs reach every F7 rule"
      (is (= (set (range (count f7-rule-names))) (reduce into #{} results))))))
