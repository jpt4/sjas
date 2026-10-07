"""Generate lcert/formal/den_gen.clj: the denotation ⟦t⟧ of R4-metatheory.md
§3.2, one kernel definition per constructor of Exp, assembled by Exp.rec.

With --ri, generate ri_den_gen.clj instead: the same denotation over an
R-interpretation ri : RInt (rint.clj; Theorem 4.6 design note §2).  Every
clause and denAt take ri after encTy and carry the suffix _ri; five clauses
read it — leaf (its tree's label is riLf ri ⟦a⟧), itR (a leaf passes riIt ri l
to g), print (riPr ri), inspect (checks riPr ri ⟦r⟧) and reflect (decodes
riPr ri ⟦r⟧, and only when riPf ri ⟦r⟧ holds).  At stdRI each reduces to the
standard clause."""
import re, sys
RI = '--ri' in sys.argv
args = [a for a in sys.argv[1:] if a != '--ri']
base = args[0] if args else 'formal/lcert/formal'
syn = open(base + '/syntax.clj').read()
blk = syn[syn.index("(a/inductive Exp []"):syn.index(";; lift k c e")]
blk = re.sub(r";[^\n]*", "", blk)
spec = [(n, re.findall(r"\[(\w+) (\w+)\]", f)) for n, f in re.findall(r"\((\w+)((?: \[\w+ \w+\])*)\)", blk)]
MOT = "(forall [G (List Sk)] (forall [sk Sk] (=> (HEnv G) (Car sk))))"
PARAMS = [("chkf", "(=> Code Code Bool)"), ("dec", "(=> Code (Option (Prod Nat (Prod Exp Exp))))"),
          ("encTy", "(=> Exp Code)")] + ([("ri", "RInt")] if RI else []) + [("prev", "DenFn"), ("cap", "Nat")]
V = "(i_r G Sk.cert en)"
bodies = {
 'var': "(lookup G i sk en)",
 'star': "(coe Sk.unit sk Unit.unit)",
 'tt': "(coe Sk.bool sk Bool.true)", 'ff': "(coe Sk.bool sk Bool.false)",
 'ite': "(Bool.rec$1 (fn [_ :- Bool] (Car sk)) (i_e G sk en) (i_t G sk en) (i_b G Sk.bool en))",
 'elimB': "(Bool.rec$1 (fn [_ :- Bool] (Car sk)) (i_e G sk en) (i_t G sk en) (i_b G Sk.bool en))",
 'zero': "(coe Sk.nat sk 0)",
 'succ': "(coe Sk.nat sk (Nat.succ (i_n G Sk.nat en)))",
 'recN': ("(Nat.rec$1 (fn [_ :- Nat] (Car sk)) (i_z G sk en) (fn [k :- Nat, acc :- (Car sk)] "
          "(i_s (sk2 sk Sk.nat G) sk (Prod.mk acc (Prod.mk k en)))) (i_n G Sk.nat en))"),
 'lbl': "(coe Sk.lbl sk l)",
 'caseL': "((i_bs G (Sk.arr Sk.lbl sk) en) (i_a G Sk.lbl en))",
 'bcons': ("(arrCase sk (fn [x :- Sk, y :- Sk] (fn [v :- (Car x)] (Nat.rec$1 (fn [_ :- Nat] (Car y)) (i_h G y en) "
           "(fn [k :- Nat, w :- (Car y)] ((i_t G (Sk.arr x y) en) (coe Sk.lbl x k))) (coe x Sk.lbl v)))))"),
 'sleaf': "(coe Sk.syn sk (Code.sl (i_a G Sk.lbl en)))",
 'snode': "(coe Sk.syn sk (Code.sn (i_a G Sk.lbl en) (i_c1 G Sk.syn en) (i_c2 G Sk.syn en)))",
 'recS': ("(Code.rec$1 (fn [_ :- Code] (Car sk)) (fn [l :- Nat] (i_tl (List.cons Sk Sk.lbl G) sk (Prod.mk l en))) "
          "(fn [l :- Nat, a :- Code, b :- Code, ya :- (Car sk), yb :- (Car sk)] (i_tn (sk2 sk sk (sk2 Sk.syn Sk.syn (List.cons Sk Sk.lbl G))) sk "
          "(Prod.mk yb (Prod.mk ya (Prod.mk b (Prod.mk a (Prod.mk l en))))))) (i_c G Sk.syn en))"),
 'leaf': "(coe Sk.cert sk (Code.sl (i_a G Sk.lbl en)))",
 'node': "(coe Sk.cert sk (Code.sn (i_a G Sk.lbl en) (i_r1 G Sk.cert en) (i_r2 G Sk.cert en)))",
 'itR': ("(Code.rec$1 (fn [_ :- Code] (Car sk)) (fn [l :- Nat] ((i_g G (Sk.arr Sk.lbl sk) en) l)) "
         "(fn [l :- Nat, a :- Code, b :- Code, ya :- (Car sk), yb :- (Car sk)] "
         "((i_h G (Sk.arr Sk.dia (Sk.arr Sk.lbl (Sk.arr sk (Sk.arr sk sk)))) en) Unit.unit l ya yb)) (i_r G Sk.cert en))"),
 'prn': "(coe Sk.syn sk (i_r G Sk.cert en))",
 'lam': "(arrCase sk (fn [x :- Sk, y :- Sk] (fn [v :- (Car x)] (i_t (List.cons Sk x G) y (Prod.mk v en)))))",
 'app': ("(Option.rec$1$0 Sk (fn [_ :- (Option Sk)] (Car sk)) (dflt sk) "
         "(fn [su :- Sk] ((i_f G (Sk.arr su sk) en) (i_u G su en))) (skOf G u))"),
 'pair': "(prodCase sk (fn [x :- Sk, y :- Sk] (Prod.mk (i_a G x en) (i_b G y en))))",
 'letp': ("(Option.rec$1$0 Sk (fn [_ :- (Option Sk)] (Car sk)) (dflt sk) "
          "(fn [sp :- Sk] (splitProd sk sp (i_p G sp en) (fn [a :- Sk, b :- Sk, va :- (Car a), vb :- (Car b)] "
          "(i_t (sk2 b a G) sk (Prod.mk vb (Prod.mk va en)))))) (skOf G p))"),
 'chk': "(coe Sk.bool sk (chkf (i_c G Sk.syn en) (i_d G Sk.syn en)))",
 'refl': ("(Bool.rec$1 (fn [_ :- Bool] (Car sk)) (dflt sk) "
          "(Option.rec$1$0 (Prod Nat (Prod Exp Exp)) (fn [_ :- (Option (Prod Nat (Prod Exp Exp)))] (Car sk)) (dflt sk) "
          "(fn [tr :- (Prod Nat (Prod Exp Exp))] (coe (skel D) sk (prev (Prod.fst tr) (Prod.fst (Prod.snd tr)) "
          "(thetaSk (Prod.fst tr)) (skel D) (tokenEnv (Prod.fst tr))))) (dec " + V + ")) "
          "(Bool.and (Nat.ble (cnodes " + V + ") cap) (chkf " + V + " (encTy D))))"),
 'insp': ("(Bool.rec$1 (fn [_ :- Bool] (Car sk)) "
          "(i_t2 (sk2 Sk.unit Sk.cert G) sk (Prod.mk Unit.unit (Prod.mk " + V + " en))) "
          "(i_t1 (sk2 Sk.unit Sk.cert G) sk (Prod.mk Unit.unit (Prod.mk " + V + " en))) "
          "(chkf " + V + " (i_c G Sk.syn en)))"),
}
if RI:
    # the five clauses that read the R-interpretation
    PV = "(riPr ri " + V + ")"
    bodies['leaf'] = "(coe Sk.cert sk (Code.sl (riLf ri (i_a G Sk.lbl en))))"
    bodies['itR'] = ("(Code.rec$1 (fn [_ :- Code] (Car sk)) (fn [l :- Nat] ((i_g G (Sk.arr Sk.lbl sk) en) (riIt ri l))) "
                     "(fn [l :- Nat, a :- Code, b :- Code, ya :- (Car sk), yb :- (Car sk)] "
                     "((i_h G (Sk.arr Sk.dia (Sk.arr Sk.lbl (Sk.arr sk (Sk.arr sk sk)))) en) Unit.unit l ya yb)) (i_r G Sk.cert en))")
    bodies['prn'] = "(coe Sk.syn sk " + PV + ")"
    bodies['refl'] = ("(Bool.rec$1 (fn [_ :- Bool] (Car sk)) (dflt sk) "
          "(Option.rec$1$0 (Prod Nat (Prod Exp Exp)) (fn [_ :- (Option (Prod Nat (Prod Exp Exp)))] (Car sk)) (dflt sk) "
          "(fn [tr :- (Prod Nat (Prod Exp Exp))] (coe (skel D) sk (prev (Prod.fst tr) (Prod.fst (Prod.snd tr)) "
          "(thetaSk (Prod.fst tr)) (skel D) (tokenEnv (Prod.fst tr))))) (dec " + PV + ")) "
          "(Bool.and (Nat.ble (cnodes " + V + ") cap) (Bool.and (riPf ri " + V + ") (chkf " + PV + " (encTy D)))))")
    bodies['insp'] = ("(Bool.rec$1 (fn [_ :- Bool] (Car sk)) "
          "(i_t2 (sk2 Sk.unit Sk.cert G) sk (Prod.mk Unit.unit (Prod.mk " + V + " en))) "
          "(i_t1 (sk2 Sk.unit Sk.cert G) sk (Prod.mk Unit.unit (Prod.mk " + V + " en))) "
          "(chkf " + PV + " (i_c G Sk.syn en)))")
SFX = "_ri" if RI else ""
out = [";; GENERATED by formal/tools/gen_den.py%s — do not edit by hand.\n" % (" --ri" if RI else "")]
names = []
for n, fs in spec:
    body = bodies.get(n, "(dflt sk)")
    binders = [(p, t) for p, t in PARAMS] + [(f, t) for f, t in fs] + [("i_" + f, MOT) for f, t in fs if t == "Exp"]
    ty = MOT
    for b, t in reversed(binders):
        ty = "(forall [%s %s] %s)" % (b, t, ty)
    lam = "(fn [%s] (fn [G :- (List Sk), sk :- Sk, en :- (HEnv G)] %s))" % (", ".join("%s :- %s" % bt for bt in binders), body)
    out.append("(kdef den_%s%s\n  %s\n  %s)\n" % (n, SFX, ty, lam))
    names.append(n)
minors = " ".join("(den_%s%s chkf dec encTy%s prev cap)" % (n, SFX, " ri" if RI else "") for n in names)
params_fn = ", ".join("%s :- %s" % bt for bt in PARAMS)
ptype = MOT
out.append("""(kdef denAt%s
  (forall [chkf (=> Code Code Bool)] (forall [dec (=> Code (Option (Prod Nat (Prod Exp Exp))))] (forall [encTy (=> Exp Code)%s]
    (forall [prev DenFn] (forall [cap Nat] (=> Exp %s))))))
  (fn [%s, t :- Exp]
    (Exp.rec$1 (fn [_ :- Exp] %s)
      %s
      t)))
""" % (SFX, " ri RInt" if RI else "", MOT, params_fn, MOT, minors))
open(base + ('/ri_den_gen.clj' if RI else '/den_gen.clj'), 'w').write("\n".join(out))
print(len(names), "clauses")
