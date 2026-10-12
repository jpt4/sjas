(ns lcert.formal.paequality
  "F6.3 — closed equality combinators built from R4 §6.2's TRANSPORT."
  (:require [ansatz.core :as a]
            [lcert.formal.base :as b]
            [lcert.formal.patransport :as transport]
            [lcert.formal.paaxioms]
            [lcert.formal.paclosed :as pc]
            [lcert.formal.dtgen :as g]))
;; Abstract TRANSPORT before checking each small closure. The final closed
;; combinator applies that abstraction to paC_TRANSPORT using rApp and its
;; already proved typing. This keeps its recursive implementation out of
;; every new reflection tree; its exact type remains kernel-checked.
(def combinators
  '[[SYM
     (PF.pall (PF.pall
       (PF.pimp (PF.peq (PT.pv 1) (PT.pv 0)) (PF.peq (PT.pv 0) (PT.pv 1)))))
     (λ :uw [a Nat] (λ :uw [b Nat] (λ :uw [e (T (ap EQ a b))]
       (ap transport a b (λ :uw [z Nat] (ap EQ z a)) e (ap REFL a)))))]
    [TRANS
     (PF.pall (PF.pall (PF.pall
       (PF.pimp (PF.peq (PT.pv 2) (PT.pv 1))
         (PF.pimp (PF.peq (PT.pv 1) (PT.pv 0)) (PF.peq (PT.pv 2) (PT.pv 0)))))))
     (λ :uw [a Nat] (λ :uw [b Nat] (λ :uw [c Nat]
       (λ :uw [e (T (ap EQ a b))] (λ :uw [h (T (ap EQ b c))]
         (ap transport b c (λ :uw [z Nat] (ap EQ a z)) h e))))))]
    [CONGP
     (PF.pall (PF.pall (PF.pall (PF.pall
       (PF.pimp (PF.peq (PT.pv 3) (PT.pv 2))
         (PF.pimp (PF.peq (PT.pv 1) (PT.pv 0))
           (PF.peq (PT.padd (PT.pv 3) (PT.pv 1)) (PT.padd (PT.pv 2) (PT.pv 0)))))))))
     (λ :uw [a Nat] (λ :uw [b Nat] (λ :uw [c Nat] (λ :uw [d Nat]
       (λ :uw [e (T (ap EQ a b))] (λ :uw [h (T (ap EQ c d))]
         (ap transport a b (λ :uw [z Nat] (ap EQ (ap PLUS a c) (ap PLUS z d))) e
           (ap transport c d (λ :uw [z Nat] (ap EQ (ap PLUS a c) (ap PLUS a z))) h
             (ap REFL (ap PLUS a c))))))))))]
    [CONGT
     (PF.pall (PF.pall (PF.pall (PF.pall
       (PF.pimp (PF.peq (PT.pv 3) (PT.pv 2))
         (PF.pimp (PF.peq (PT.pv 1) (PT.pv 0))
           (PF.peq (PT.pmul (PT.pv 3) (PT.pv 1)) (PT.pmul (PT.pv 2) (PT.pv 0)))))))))
     (λ :uw [a Nat] (λ :uw [b Nat] (λ :uw [c Nat] (λ :uw [d Nat]
       (λ :uw [e (T (ap EQ a b))] (λ :uw [h (T (ap EQ c d))]
         (ap transport a b (λ :uw [z Nat] (ap EQ (ap TIMES a c) (ap TIMES z d))) e
           (ap transport c d (λ :uw [z Nat] (ap EQ (ap TIMES a c) (ap TIMES a z))) h
             (ap REFL (ap TIMES a c))))))))))]] )

(doseq [[nm formula body] combinators]
  (let [term (symbol (str "paC_" nm)), fname (symbol (str "paF_" nm))
        helper (symbol (str term "_using_transport"))
        helper-rt (symbol (str helper "_rt"))
        theorem (symbol (str term "_rt")), form-name (symbol (str term "_form"))
        cs (assoc pc/constants 'TransportTy transport/transport-type)
        e (g/compile-term cs (list 'λ :uw '[transport TransportTy] body))
        ty (g/pa-formula pc/constants formula identity)
        A (list 'trF fname 'ρ), nilD '(List.nil Exp), nilU '(List.nil U)]
    (b/kdef! fname 'PF formula)
    (binding [g/*names* (assoc pc/names transport/transport-type 'paTransportTy)]
      (b/kdef! helper 'Exp (g/sx e))
      (b/kdef! term 'Exp (list 'Exp.app helper 'paC_TRANSPORT))
      (let [proof (transport/split-proof! (str "paEquality_" nm)
                    (g/rt [] [] e [:tPi :uw transport/transport-type ty]))]
        (a/prove-theorem helper-rt '[chkf :- (=> Code Code Bool), ρ :- (=> Nat Nat)]
          (list 'Rt 'chkf nilD nilU helper (list 'Exp.tPi 'U.uw 'paTransportTy A))
          [(list 'exact proof)]))
      (let [proof (transport/split-proof! (str "paEquality_form_" nm) (g/tl-form [] ty))]
        (a/prove-theorem form-name '[chkf :- (=> Code Code Bool), ρ :- (=> Nat Nat)]
          (list 'Tl 'chkf 'Bool.true nilD A 'Exp.tUnit) [(list 'exact proof)])))
    (a/prove-theorem theorem '[chkf :- (=> Code Code Bool), ρ :- (=> Nat Nat)]
      (list 'Rt 'chkf nilD nilU term A)
      [(list 'exact
         (list 'Rt.rApp 'chkf nilD nilU nilU 'U.uw helper 'paC_TRANSPORT 'paTransportTy A
           'rfl (list helper-rt 'chkf 'ρ) '(paC_TRANSPORT_rt chkf) '(paTransport_form chkf)
           (list 'paClosed_tl 'chkf 'Bool.true A 'Exp.tUnit 'rfl 'rfl
             (list form-name 'chkf 'ρ) '(List.cons Exp paTransportTy (List.nil Exp)))) )])
    (a/prove-theorem (symbol (str term "_at"))
      '[chkf :- (=> Code Code Bool), D :- (List Exp), ρ :- (=> Nat Nat)]
      (list 'Rt 'chkf 'D '(allW D) (list 'paPad A term) A)
      [(list 'exact (list 'pa_pad_closed 'chkf 'D term A 'rfl 'rfl 'rfl
         (list form-name 'chkf 'ρ) (list theorem 'chkf 'ρ)))])))
