(ns lcert.formal-test.certenc
  "Formal suite (ADR-0006), lcert.formal.certenc: the encoding of the data
  certificates hold (derivation trees as codes), its decoders and round
  trips, raw codes as literal terms, and the padded certificate format with
  its completeness (F7 5c; kept as the historical counterexample of
  enc46f7.clj — the canonical format is certcanon.clj's)."
  (:require [clojure.test :refer [deftest is testing]]
            [lcert.formal.base :as b]
            [lcert.formal.certenc]))

(def ^:private nilE '(List.nil Exp))
(def ^:private zc (list 'DT.zConst nilE 'Exp.star 'Exp.tUnit))
(def ^:private rc (list 'DT.rConst nilE '(List.nil U) 'Exp.star 'Exp.tUnit))
(def ^:private fb (list 'DT.fBase nilE 'Exp.tUnit))

(defn- holds? [prop] (not (b/rejects? [] prop '[(rfl)])))

(deftest f7-encoding-roundtrip
  (testing "encoders, decoders and round trips for every type a certificate holds"
    (doseq [nm '[encN decN rtN encB decB rtB encU decU rtU decE rtE encS decS rtS
                 encLN decLN rtLN encLE decLE rtLE encLU decLU rtLU encLS decLS rtLS
                 encHd decHd rtHd encSt decSt rtSt
                 encSkDT decSkDT htSkDT rtSkDT encDT decDT htDT rtDT]]
      (is (b/has? nm) (str nm))))
  (testing "a derivation tree decodes back from its encoding"
    (is (holds? (list 'Eq '(Option DT) (list 'decDT 3 (list 'encDT zc)) (list 'Option.some 'DT zc))))
    (is (holds? (list 'Eq '(Option DT) (list 'decDT 3 (list 'encDT fb)) (list 'Option.some 'DT fb)))))
  (testing "a code with an unknown constructor label decodes to nothing"
    (is (holds? '(Eq (Option DT) (decDT 3 (Code.sn 999 (Code.sl 0) (Code.sl 0))) (Option.none DT)))))
  (testing "without fuel nothing decodes"
    (is (holds? (list 'Eq '(Option DT) (list 'decDT 0 (list 'encDT zc)) '(Option.none DT)))))
  (testing "raw codes are written as their literal terms and read back"
    (doseq [nm '[encC decC rtC lblC]] (is (b/has? nm) (str nm)))
    (is (holds? '(Eq (Option Code) (decC (encC (Code.sn 96 (Code.sl 0) (Code.sl 5)))) (Option.some Code (Code.sn 96 (Code.sl 0) (Code.sl 5))))))
    ;; not the identity: the label 96 of the raw code becomes a leaf under lbl
    (is (not (holds? '(Eq Code (encC (Code.sn 96 (Code.sl 0) (Code.sl 5))) (Code.sn 96 (Code.sl 0) (Code.sl 5))))))
    (is (holds? '(Eq Code (encC (Code.sl 5)) (encE (Exp.sleaf (Exp.lbl 5))))))))

(deftest f7-completeness-padded
  (testing "the padded certificate format (historical), its decoder, and its completeness"
    (doseq [nm '[encCertPad decCertPad decCertPad_enc padC pad_nodes check_cert_ok_pad check_complete_pad]]
      (is (b/has? nm) (str nm))))
  (testing "the padded certificate of ⋆ : Unit is accepted for Unit, not for Bool"
    (let [cert (list 'encCertPad 1 0 'Exp.star 'Exp.tUnit rc fb '(padC 3))]
      (is (holds? (list 'Eq 'Bool (list 'Check 'decCertPad cert '(encE Exp.tUnit)) 'Bool.true)))
      (is (holds? (list 'Eq 'Bool (list 'Check 'decCertPad cert '(encE Exp.tBool)) 'Bool.false)))))
  (testing "completeness of the padded format: an accepted typing tree and formation tree make an accepted certificate"
    (is (not (b/rejects? '[m :- Nat, t :- Exp, A :- Exp, T1 :- DT, T2 :- DT,
                           h1 :- (Eq Bool (dtCheck (Check decCertPad) T1 (DTJ.rt (thetaD m) (thetaU m) t A)) Bool.true),
                           h2 :- (Eq Bool (dtCheck (Check decCertPad) T2 (DTJ.tl Bool.true (List.nil Exp) A Exp.tUnit)) Bool.true),
                           hA :- (Eq Bool (closedTy A) Bool.true)]
      '(Exists (fn [c :- Code] (Eq Bool (Check decCertPad c (encE A)) Bool.true)))
      '[(exact (check_complete_pad m t A T1 T2 h1 h2 hA))])))))

(deftest f7-certificate-labels
  (testing "numbers in certificates are unary, so a certificate's labels stay below 100"
    (doseq [nm '[dataOkDT lblBelow_encCertPad check_complete_lbl_pad]] (is (b/has? nm) (str nm)))
    (is (holds? '(Eq Bool (lblBelow 97 (encN 150)) Bool.true)))
    (is (holds? '(Eq (Option Nat) (decN (encN 7)) (Option.some Nat 7)))))
  (testing "a padded certificate with fuel and budget above 100 has every label below 100"
    (is (holds? (list 'Eq 'Bool (list 'lblOk (list 'encCertPad 150 120 'Exp.star 'Exp.tUnit rc fb '(padC 3))) 'Bool.true)))))

