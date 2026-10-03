(ns lcert.formal-test.certenc
  "Formal suite (ADR-0006), lcert.formal.certenc: the encoding of
  certificates (derivation trees as codes), its decoder and round trip, and
  completeness — the checker accepts the encoding of every tree it accepts
  as a derivation (F7 5c)."
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
    (is (holds? (list 'Eq '(Option DT) (list 'decDT 0 (list 'encDT zc)) '(Option.none DT))))))

(deftest f7-completeness
  (testing "the certificate decoder, and completeness"
    (doseq [nm '[encCert decCert decCert_enc padC pad_nodes check_complete]]
      (is (b/has? nm) (str nm))))
  (testing "the encoded certificate of ⋆ : Unit is accepted for Unit, not for Bool"
    (let [cert (list 'encCert 1 0 'Exp.star 'Exp.tUnit rc fb '(padC 3))]
      (is (holds? (list 'Eq 'Bool (list 'Check 'decCert cert '(encE Exp.tUnit)) 'Bool.true)))
      (is (holds? (list 'Eq 'Bool (list 'Check 'decCert cert '(encE Exp.tBool)) 'Bool.false)))))
  (testing "completeness: an accepted typing tree and formation tree make an accepted certificate"
    (is (not (b/rejects? '[m :- Nat, t :- Exp, A :- Exp, T1 :- DT, T2 :- DT,
                           h1 :- (Eq Bool (dtCheck (Check decCert) T1 (DTJ.rt (thetaD m) (thetaU m) t A)) Bool.true),
                           h2 :- (Eq Bool (dtCheck (Check decCert) T2 (DTJ.tl Bool.true (List.nil Exp) A Exp.tUnit)) Bool.true),
                           hA :- (Eq Bool (closedTy A) Bool.true)]
      '(Exists (fn [c :- Code] (Eq Bool (Check decCert c (encE A)) Bool.true)))
      '[(exact (check_complete m t A T1 T2 h1 h2 hA))])))))
