(ns lcert.formal.s52reflect
  "Theorem 5.2: reflected programs run safely at a strictly smaller budget."
  (:require [ansatz.core :as a]
            [clojure.walk :as walk]
            [lcert.formal.base :refer [thm kdef kdef! lv]]
            [lcert.formal.s52fund :refer [prove! pars ps ctx den rel trace result]]
            [lcert.formal.s52data]))

;; The original evaluator runs t itself; the erasing evaluator runs any
;; erasure of a runtime derivation. This is only a proposition selecting
;; syntax, not a new evaluator or an axiom about derivation uniqueness.
(kdef Select52 (=> (=> Code Code Bool) Bool Nat Exp Exp Exp Prop)
  (fn [chkf :- (=> Code Code Bool), erasing :- Bool, m :- Nat, t :- Exp, A :- Exp, e :- Exp]
    (Bool.rec$1 (fn [_ :- Bool] Prop) (Eq Exp e t)
      (Er chkf (thetaD m) (thetaU m) t A e) erasing)))

(thm select52_total [chkf :- (=> Code Code Bool), erasing :- Bool, m :- Nat, t :- Exp, A :- Exp,
                     hd :- (Rt chkf (thetaD m) (thetaU m) t A)]
  (Exists (fn [e :- Exp] (Select52 chkf erasing m t A e)))
  (cases erasing)
  (constructor) (exact t) (rfl)
  (exact (er_total chkf (thetaD m) (thetaU m) t A hd)))

;; The outer induction needs just the closed token-context instance. Its
;; erasing clause quantifies over every erasure, including reflected runs.
(kdef! 'Closed52
  (reduce (fn [q [x _ ty]] (list 'forall [x ty] q)) 'Prop (reverse (partition 3 pars)))
  (list 'fn pars
    '(forall [t Exp] (forall [A Exp]
       (=> (Rt chkf (thetaD cap) (thetaU cap) t A)
         (forall [e Exp] (=> (Select52 chkf erasing cap t A e)
           (Result52 chkf dec encTy cap erasing (skels (thetaD cap)) (tokEnvD cap) (rtokens cap) t A e))))))))

;; All tokens satisfy S, with no footprint reasoning required.
(prove! 'tok52 '[m :- Nat]
  '(Env52 chkf dec encTy cap erasing (thetaD m) (thetaU m) (rtokens m) (tokEnvD m))
  '[(induction m) (exact (env52_nil chkf dec encTy cap erasing))
    (exact (env52_cons_rel chkf dec encTy cap erasing Exp.tDia (thetaD n) U.u1 (thetaU n)
      RV.token (rtokens n) Unit.unit (tokEnvD n) (Or.inr (Eq.refl Bool.true)) (Eq.refl RV.token) ih_n))])

;; Only successful reflect differs between the trace tables. Its nested
;; trace is required explicitly, at the decoded budget and selected syntax.
(prove! 'trace52_refl_ok
  '[rho :- (List RV), X :- Exp, r :- Exp, e :- Exp, c :- Code,
    m :- Nat, t2 :- Exp, A :- Exp, e2 :- Exp, ve :- RV, w :- RV]
  '(=> (Trace52 chkf dec encTy cap erasing (EvSrc.tm rho r) (RV.cert c))
       (Trace52 chkf dec encTy cap erasing (EvSrc.tm rho e) ve)
       (Eq Bool (Bool.and (Nat.ble (cnodes c) cap) (chkf c (encTy X))) Bool.true)
       (Eq (Option (Prod Nat (Prod Exp Exp))) (dec c)
         (Option.some (Prod Nat (Prod Exp Exp)) (Prod.mk m (Prod.mk t2 A))))
       (Select52 chkf erasing m t2 A e2)
       (Trace52 chkf dec encTy m erasing (EvSrc.tm (rtokens m) e2) w)
       (Eq Bool (isH X) Bool.false)
       (Trace52 chkf dec encTy cap erasing (EvSrc.tm rho (Exp.refl X r e)) w))
  '[(cases erasing) (intro hr he hb hd hs ht hH) (subst hs)
    (exact (Ok.eReflOk chkf dec encTy cap rho X r e c m t2 A ve w hr he hb hd ht hH))
    (intro hr he hb hd hs ht hH)
    (exact (OkE.eReflOk chkf dec encTy cap rho X r e c m t2 A e2 ve w hr he hb hd hs ht hH))])

;; S at base data types is independent of the budget and environment.
;; Empty stays empty; unlike E it never admits a default there.
(def fields @#'lcert.formal.syntactic/exp-fields)
(def base-ctors '#{tEmpty tUnit tBool tNat tLbl tSyn tR})
(prove! 's52_base_transfer
  '[X :- Exp, m :- Nat, G :- (List Sk), en :- (HEnv G), G2 :- (List Sk), en2 :- (HEnv G2)]
  '(=> (Eq Bool (isBaseTy X) Bool.true)
    (forall [v RV] (forall [alpha (Car (skel X))]
      (Eq Prop (S52 chkf dec encTy m erasing X G en (skel X) v alpha)
        (S52 chkf dec encTy cap erasing X G2 en2 (skel X) v alpha)))))
  (into ['(cases X)] (mapcat (fn [[c _]]
    (if (base-ctors c) '[(intro hb v alpha) (rfl)] '[(intro hb) (exact (Bool.noConfusion hb))])) fields)))

(prove! 's52_base_default (concat ctx '[X :- Exp])
  '(=> (Eq Bool (isBaseTy X) Bool.true) (Eq Bool (isH X) Bool.false)
    (S52 chkf dec encTy cap erasing X G en (skel X) (rdflt (skel X)) (dflt (skel X))))
  (into ['(cases X)] (mapcat (fn [[c _]]
    (cond
      (= c 'tEmpty) '[(intro hb hh) (exact (Bool.noConfusion hh))]
      (#{'tLbl} c) '[(intro hb hh) (constructor) (rfl) (change (LT.lt 0 100)) (omega)]
      (#{'tSyn 'tR} c) '[(intro hb hh) (constructor) (rfl) (rfl)]
      (base-ctors c) '[(intro hb hh) (rfl)]
      :else '[(intro hb) (exact (Bool.noConfusion hb))])) fields)))

(thm s52_isH [X :- Exp]
  (=> (Eq Bool (isH X) Bool.true) (Eq Exp X Exp.tEmpty))
  (cases X)
  (intro hh) (rfl)
  (all_goals (intro hh))
  (all_goals (exact (Bool.noConfusion hh))))

(def CR (den 'r 'Sk.cert))
(def guard (list 'Bool.and (list 'Nat.ble (list 'cnodes CR) 'cap) (list 'chkf CR '(encTy X))))
(def refl-pars '[X :- Exp, cd :- Exp, r :- Exp, e :- Exp, re :- Exp, ee :- Exp,
                 hb :- (Eq (Option Exp) (baseCode X) (Option.some Exp cd)),
                 hH :- (Eq Bool (isH X) Bool.false)])
(def below '(forall [m Nat] (=> (LT.lt m cap) (Closed52 chkf dec encTy m erasing))))
(defn- run [m t A] (list 'den 'chkf 'dec 'encTy m t (list 'skels (list 'thetaD m))
                         (list 'skel A) (list 'tokEnvD m)))

;; Failed cap/check guard: the safe operand traces precede the default.
(prove! 's52_refl_no
  (concat ctx refl-pars '[ve :- RV]
    ['her :- (trace 're (list 'RV.cert CR)) 'hee :- (trace 'ee 've)
     'hc :- (list 'Eq 'Bool guard 'Bool.false)])
  (result '(Exp.refl X r e) 'X '(Exp.refl X re ee))
  ['(rw [(den_refl_false chkf dec encTy cap X r e G en hc)])
   '(constructor) '(exact (rdflt (skel X))) '(constructor)
   (list 'exact (list 'trace52_eReflNo 'chkf 'dec 'encTy 'erasing 'cap 'rho 'X 're 'ee CR 've 'her 'hee 'hc 'hH))
   '(exact (s52_base_default chkf dec encTy cap erasing G en rho X (isBase_of_code X cd hb) hH))])

;; Successful guard: CheckSpec provides the runtime derivation, decoding,
;; strict budget decrease, and injective type encoding. The outer IH then
;; supplies the nested safe run; no token-size or footprint premise is used.
(prove! 's52_refl_yes
  (concat ctx refl-pars '[hcs :- (CheckSpec chkf dec encTy)] ['below :- below 've :- 'RV
    'her :- (trace 're (list 'RV.cert CR)) 'hee :- (trace 'ee 've)
    'hc :- (list 'Eq 'Bool guard 'Bool.true)])
  (result '(Exp.refl X r e) 'X '(Exp.refl X re ee))
  (walk/postwalk-replace {'CR CR 'RUN (run 'm 't2 'X)}
   '[(have hbase (Eq Bool (isBaseTy X) Bool.true) (isBase_of_code X cd hb))
     (have hchk (Eq Bool (chkf CR (encTy X)) Bool.true)
       (andb_right (Nat.ble (cnodes CR) cap) (chkf CR (encTy X)) hc))
     (refine' (exT Nat _ _ ((And.left hcs) CR (encTy X) hchk) _)) (intro m hm)
     (refine' (exT Exp _ _ hm _)) (intro t2 ht2)
     (refine' (exT Exp _ _ ht2 _)) (intro A hA)
     (have hd (Eq (Option (Prod Nat (Prod Exp Exp))) (dec CR)
       (Option.some (Prod Nat (Prod Exp Exp)) (Prod.mk m (Prod.mk t2 A)))) (And.left hA))
     (have hrt (Rt chkf (thetaD m) (thetaU m) t2 A) (And.left (And.right hA)))
     (have hclA (Eq Bool (closedTy A) Bool.true) (And.left (And.right (And.right (And.right hA)))))
     (have henc (Eq Code (encTy A) (encTy X)) (And.left (And.right (And.right (And.right (And.right hA))))))
     (have hlt0 (LT.lt m (cnodes CR)) (And.right (And.right (And.right (And.right (And.right hA))))))
     (have hle (Nat.le (cnodes CR) cap)
       (Nat.le_of_ble_eq_true (andb_left (Nat.ble (cnodes CR) cap) (chkf CR (encTy X)) hc)))
     (have hlt (LT.lt m cap) (lt_le_omega m (cnodes CR) cap hlt0 hle))
     (have eAX (Eq Exp A X)
       ((And.right (And.right (And.right hcs))) A X hclA (closed_of_base X hbase) henc))
     (refine' (exT Exp _ _ (select52_total chkf erasing m t2 A hrt) _)) (intro e2 hs2)
     (refine' (exT RV _ _ (below m hlt t2 A hrt e2 hs2) _)) (intro w pw)
     (have hx (S52 chkf dec encTy m erasing X (skels (thetaD m)) (tokEnvD m) (skel X) w RUN)
       (Eq.mp (congrArg (fn [Z :- Exp]
         (S52 chkf dec encTy m erasing Z (skels (thetaD m)) (tokEnvD m) (skel Z) w
           (den chkf dec encTy m t2 (skels (thetaD m)) (skel Z) (tokEnvD m)))) eAX) (And.right pw)))
     (have hn (S52 chkf dec encTy cap erasing X G en (skel X) w RUN)
       (Eq.mp (s52_base_transfer chkf dec encTy cap erasing X m (skels (thetaD m)) (tokEnvD m) G en hbase w RUN) hx))
     (constructor) (exact w) (constructor)
     (exact (trace52_refl_ok chkf dec encTy cap erasing rho X re ee CR m t2 A e2 ve w
       her hee hc hd hs2 (And.left pw) hH))
     (exact (Eq.mp (congrArg (fn [alpha :- (Car (skel X))]
       (S52 chkf dec encTy cap erasing X G en (skel X) w alpha))
       (Eq.trans (Eq.symm (tok_transfer m (den chkf dec encTy m t2) (skel X)))
         (Eq.symm (den_refl_ok_eq chkf dec encTy cap G X r e en m t2 A hc hd hlt)))) hn))]))

(prove! 's52_refl_nonH
  (concat ctx refl-pars '[hcs :- (CheckSpec chkf dec encTy)] ['below :- below
    'ihr :- (result 'r 'Exp.tR 're) 'ihe :- (result 'e '(chkT r cd) 'ee)])
  (result '(Exp.refl X r e) 'X '(Exp.refl X re ee))
  [(list 'have 'her (trace 're (list 'RV.cert CR))
     '(s52_value_cert chkf dec encTy cap erasing G en rho r re ihr))
   '(refine' (exT RV _ _ ihe _)) '(intro ve pe)
   (list 'by_cases guard)
   '(exact (s52_refl_no chkf dec encTy cap erasing G en rho X cd r e re ee hb hH ve her (And.left pe) hc))
   '(exact (s52_refl_yes chkf dec encTy cap erasing G en rho X cd r e re ee hb hH hcs below ve her (And.left pe) hc))])

;; H (reflect at 0) is vacuous by Corollary 3.7 at every code size. Thus
;; every inhabited case has isH X=false, as the safe trace rules require.
(prove! 's52_refl
  (concat ctx (drop-last 3 refl-pars) '[hcs :- (CheckSpec chkf dec encTy)] ['below :- below
    'ihr :- (result 'r 'Exp.tR 're) 'ihe :- (result 'e '(chkT r cd) 'ee)])
  (result '(Exp.refl X r e) 'X '(Exp.refl X re ee))
  '[(by_cases (isH X))
    (exact (s52_refl_nonH chkf dec encTy cap erasing G en rho X cd r e re ee hb hc hcs below ihr ihe))
    (have hx (Eq Exp X Exp.tEmpty) (s52_isH X hc)) (subst hx)
    (exact (s52_H chkf dec encTy cap erasing G en rho hcs r cd e ee hb _ ihe))])
