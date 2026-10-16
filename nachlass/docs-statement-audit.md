# Statement audit: R4 and ADR-0006

Scope: every numbered result in [`R4-metatheory.md`](refinement/R4-metatheory.md), every claim in its §0 and §7, and the numbered draft claims within ADR-0006's scope. A formal statement is the **elaborated type actually stored in the Ansatz environment**, printed from the warm server after loading all `lcert.formal.*` namespaces. Where a proposition constant has type `Prop`, its stored definition is also printed. Definitions used as statements remain part of the trust base. `exact`, `stronger`, `weaker`, `different`, and `open` compare the paper's assertion with the listed kernel declarations, not with the plausibility of their proofs. “Weaker” lists extra premises; “different” marks changed subject or scope.

The type printer converts string-named nodes in stored kernel expressions to Ansatz `Name` objects for display, then calls `ansatz.surface.pp/pp`; it does not elaborate or alter the stored declaration. This works around a pretty-printer cast error on declarations created by some source macros. The raw kernel expressions and these printed types were compared through the same warm server.

The paper's unqualified consistency assertions are read **relative to the Ansatz kernel, its trusted bundled Init export, and the surrounding metatheory**. The checker-specific statements refer to `Check decCert`. An implication with an unproved arithmetic hypothesis is a conditional theorem, not a proof of its unconditioned conclusion.

## Numbered statements

### Lemma 2.1

**Formal constants:** `tl_weaken` (`code/lcert/formal/lcert/formal/derivations.clj:1202`), `rt_weaken` (`code/lcert/formal/lcert/formal/derivations.clj:1480`).

**Kernel-held elaborated type:**

`tl_weaken`:

```lean
∀ (chkf : Code → Code → Bool), ∀ (w0 : Bool), ∀ (D0 : List Exp), ∀ (t0 : Exp), ∀ (A0 : Exp), ∀ (der : Tl chkf w0 D0 t0 A0), ∀ (cc : Nat), ∀ (XX : Exp), Tl chkf w0 (insD cc XX D0) (lift 1 cc t0) (lift 1 cc A0)
```

`rt_weaken`:

```lean
∀ (chkf : Code → Code → Bool), ∀ (D0 : List Exp), ∀ (us0 : List U), ∀ (t0 : Exp), ∀ (A0 : Exp), ∀ (der : Rt chkf D0 us0 t0 A0), ∀ (cc : Nat), ∀ (XX : Exp), Rt chkf (insD cc XX D0) (insU cc U.u0 us0) (lift 1 cc t0) (lift 1 cc A0)
```

**Verdict: exact (A01, 2026-10-15).** Insertion of usage-0 entries with de Bruijn lifting formalizes weakening. R4 no longer claims exchange of independent entries: no result uses it, and it is not formalized.

**Extra-premise satisfiability:** No extra premise for insertion. Exchange is no longer claimed.

**Suite coverage:** `tl_weaken` → `lcert.formal-test.derivations/f2f-runtime-weakening`; `rt_weaken` → `lcert.formal-test.derivations/f2f-runtime-weakening`.

### Lemma 2.2

**Formal constants and kernel type:** none.

**Verdict: exact by definition (A02, 2026-10-15).** R4 now states only the de Bruijn case: renaming bound variables is syntactic identity, and renaming two contexts apart is Lemma 2.1's shift. General injective renaming, which would include exchange, is no longer claimed.

**Extra-premise satisfiability:** No counterpart; no extra premise to discharge.

**Suite coverage:** none (no counterpart).

### Lemma 2.3

**Formal constants:** `rt_strengthen` (`code/lcert/formal/lcert/formal/strengthen.clj:223`).

**Kernel-held elaborated type:**

`rt_strengthen`:

```lean
∀ (chkf : Code → Code → Bool), ∀ (D0 : List Exp), ∀ (us0 : List U), ∀ (t0 : Exp), ∀ (A0 : Exp), ∀ (der : Rt chkf D0 us0 t0 A0), ∀ (cz : Nat), ∀ (h : @Eq.{1} Bool (freshF t0 cz) Bool.true), Rt chkf D0 (zeroUF us0 cz) t0 A0
```

**Verdict: exact.** Freshness is expressed by freshF and pointwise usage lowering.

**Extra-premise satisfiability:** freshF is the paper's x-not-free condition, not an added assumption; `rt_mask` gives simultaneous strengthening.

**Suite coverage:** `rt_strengthen` → `lcert.formal-test.strengthen/f2-strengthening`.

### Lemma 2.4

**Formal constants:** `lemma24` (`code/lcert/formal/lcert/formal/derivations.clj:1585`).

**Kernel-held elaborated type:**

`lemma24`:

```lean
∀ (chkf : Code → Code → Bool), ∀ (m1 : Nat), ∀ (m2 : Nat), ∀ (t1 : Exp), ∀ (t2 : Exp), ∀ (A : Exp), ∀ (hc : @Eq.{1} Bool (closedTy A) Bool.true), ∀ (hA : Tl chkf Bool.true (List.nil Exp) A Exp.tUnit), ∀ (h1 : Rt chkf (thetaD m1) (thetaU m1) t1 A), ∀ (h2 : Rt chkf (thetaD m2) (thetaU m2) t2 (Exp.tPi U.u1 A Exp.tEmpty)), Rt chkf (thetaD (Nat.add m1 m2)) (thetaU (Nat.add m1 m2)) (Exp.app (lift m1 0 t2) (lift m2 m1 t1)) Exp.tEmpty
```

**Verdict: exact after paper correction.** The paper now explicitly requires the closed formation derivation `∅ ⊢ A :⁰ 1` alongside closedness. This matches `lemma24` without changing the typing definitions.

**Extra-premise satisfiability:** The formation derivation is now a stated premise. Closedness alone does not imply formation: `closedTy` checks only variable scope. For the uses in the consistency proof, the checker soundness specification supplies the decoded type's closed formation derivation.

**Suite coverage:** `lemma24` → `lcert.formal-test.derivations/f2f-composition` (focused warm-server namespace run: 3 tests, 45 assertions, 0 failures, 0 errors).

### Lemma 2.5

**Formal constants:** `lemma25_tl` (`code/lcert/formal/lcert/formal/skeletons.clj:659`), `lemma25_rt` (`code/lcert/formal/lcert/formal/skeletons.clj:777`), `cv_skel` (`code/lcert/formal/lcert/formal/skeletons.clj:379`).

**Kernel-held elaborated type:**

`lemma25_tl`:

```lean
∀ (chkf : Code → Code → Bool), ∀ (w0 : Bool), ∀ (D0 : List Exp), ∀ (t0 : Exp), ∀ (A0 : Exp), ∀ (der : Tl chkf w0 D0 t0 A0), SkJ w0 (skels D0) t0 (skel A0)
```

`lemma25_rt`:

```lean
∀ (chkf : Code → Code → Bool), ∀ (D0 : List Exp), ∀ (us0 : List U), ∀ (t0 : Exp), ∀ (A0 : Exp), ∀ (der : Rt chkf D0 us0 t0 A0), SkJ Bool.false (skels D0) t0 (skel A0)
```

`cv_skel`:

```lean
∀ (chkf : Code → Code → Bool), ∀ (G : List Sk), ∀ (A : Exp), ∀ (B : Exp), ∀ (h : Cv chkf G A B), @Eq.{1} Sk (skel A) (skel B)
```

**Verdict: exact.** The two conclusions state the paper's typing-to-skeleton property, and `cv_skel` states conversion preservation. The branch-list restrictions are already part of the formal source judgments, rather than new theorem premises.

**Extra-premise satisfiability:** No added theorem premise. `Tl`, `Rt`, and `Cv` are the judgments defined for the formal calculus.

**Suite coverage:** `lemma25_tl` → `lcert.formal-test.skeletons/f2e-lemma-2-5`; `lemma25_rt` → `lcert.formal-test.skeletons/f2e-lemma-2-5`; `cv_skel` → `lcert.formal-test.skeletons/f2e-lemma-2-5`.

### Lemma 2.6

**Formal constants:** `CheckSpec` (`code/lcert/formal/lcert/formal/model.clj:106`), `check_spec` (`code/lcert/formal/lcert/formal/check_spec.clj:280`), `check_full` (`code/lcert/formal/lcert/formal/check_spec.clj:237`).

**Kernel-held elaborated type:**

`CheckSpec` (transparent definition):

```lean
(Code → Code → Bool) → (Code → Option (Prod Nat (Prod Exp Exp))) → (Exp → Code) → Prop
```

Kernel-stored definition (printed after normalizing its string-named constants for Ansatz’s pretty-printer):

```lean
fun (chkf : Code → Code → Bool) => fun (dec : Code → Option (Prod Nat (Prod Exp Exp))) => fun (encTy : Exp → Code) => And (∀ (c : Code), ∀ (d : Code), ∀ (f : @Eq.{1} Bool (chkf c d) Bool.true), @Exists.{1} Nat (fun (m : Nat) => @Exists.{1} Exp (fun (t : Exp) => @Exists.{1} Exp (fun (A : Exp) => And (@Eq.{1} (Option (Prod Nat (Prod Exp Exp))) (dec c) (Option.some (Prod Nat (Prod Exp Exp)) (Prod.mk Nat (Prod Exp Exp) m (Prod.mk Exp Exp t A)))) (And (Rt chkf (thetaD m) (thetaU m) t A) (And (Tl chkf Bool.true (List.nil Exp) A Exp.tUnit) (And (@Eq.{1} Bool (closedTy A) Bool.true) (And (@Eq.{1} Code (encTy A) d) (Nat.lt m (cnodes c)))))))))) (And (∀ (D : Exp), ∀ (cd : Exp), ∀ (f : @Eq.{1} (Option Exp) (baseCode D) (Option.some Exp cd)), @Eq.{1} (Option Code) (codeOf cd) (Option.some Code (encTy D))) (And (∀ (A : Exp), ∀ (e : @Eq.{1} Bool (closedTy A) Bool.true), @Eq.{1} Code (encTy (Exp.tPi U.u1 A Exp.tEmpty)) (Code.sn 25 (encTy A) (Code.sl 15))) (∀ (A : Exp), ∀ (B : Exp), ∀ (f : @Eq.{1} Bool (closedTy A) Bool.true), ∀ (g : @Eq.{1} Bool (closedTy B) Bool.true), ∀ (h : @Eq.{1} Code (encTy A) (encTy B)), @Eq.{1} Exp A B)))
```

`check_spec`:

```lean
∀ (decD : Code → Option (Prod Nat (Prod Exp (Prod Exp (Prod DT DT))))), CheckSpec (Check decD) (decOf decD) encE
```

`check_full`:

```lean
∀ (decD : Code → Option (Prod Nat (Prod Exp (Prod Exp (Prod DT DT))))), ∀ (c : Code), ∀ (d : Code), ∀ (h : @Eq.{1} Bool (Check decD c d) Bool.true), @Exists.{1} Nat (fun (m : Nat) => @Exists.{1} Exp (fun (t : Exp) => @Exists.{1} Exp (fun (A : Exp) => And (@Eq.{1} (Option (Prod Nat (Prod Exp Exp))) (decOf decD c) (Option.some (Prod Nat (Prod Exp Exp)) (Prod.mk Nat (Prod Exp Exp) m (Prod.mk Exp Exp t A)))) (And (Rt (Check decD) (thetaD m) (thetaU m) t A) (And (Tl (Check decD) Bool.true (List.nil Exp) A Exp.tUnit) (And (@Eq.{1} Bool (closedTy A) Bool.true) (And (@Eq.{1} Code (encE A) d) (And (Nat.lt m (cnodes c)) (And (LT.lt Nat instLTNat (Nat.add (cntU (maskUF (thetaU m) (freshF t))) (cntU (maskUF (thetaU m) (freshF t)))) (cnodes c)) (LT.lt Nat instLTNat (cnodes (encE A)) (cnodes c)))))))))))
```

**Verdict: exact (fidelity pass, 2026-10-15).** R4's Lemma 2.6 now states everything `CheckSpec` gives: the decoded derivation, a closed and formed type, `⌜A⌝ = d`, and the strict budget bound of Lemma 2.7. `check_full` and `check_spec` establish this for `Check decCert`.

**Extra-premise satisfiability:** `check_spec decCert` (proved via `check_full`) satisfies generic `CheckSpec`; `checkspec_sat` also shows abstract consistency.

**Suite coverage:** `CheckSpec` → `lcert.formal-test.model/f3e-statements-and-t2`; `check_spec` → `lcert.formal-test.check_spec/f7-concrete-checker`; `check_full` → `lcert.formal-test.check_spec/f7-concrete-checker`.

### Lemma 2.7

**Formal constants:** `check_spec` (`code/lcert/formal/lcert/formal/check_spec.clj:280`), `budget_canon` (`code/lcert/formal/lcert/formal/certcanon.clj:390`), `strict_overhead` (`code/lcert/ansatz/lcert/verified.clj:126`).

**Kernel-held elaborated type:**

`strict_overhead`:

```lean
∀ (l : Nat), ∀ (j : CT), ∀ (p : CT), LT.lt Nat instLTNat (budget (CT.cell l Bool.true j p)) (nodes (CT.cell l Bool.true j p))
```

`budget_canon`:

```lean
∀ (c : Code), ∀ (y : Prod Nat (Prod Exp (Prod Exp (Prod DT DT)))), ∀ (h : @Eq.{1} (Option (Prod Nat (Prod Exp (Prod Exp (Prod DT DT))))) (decCert c) (Option.some (Prod Nat (Prod Exp (Prod Exp (Prod DT DT)))) y)), LT.lt Nat instLTNat (Prod.fst Nat (Prod Exp (Prod Exp (Prod DT DT))) y) (cnodes c)
```

`check_spec`:

```lean
∀ (decD : Code → Option (Prod Nat (Prod Exp (Prod Exp (Prod DT DT))))), CheckSpec (Check decD) (decOf decD) encE
```

**Verdict: exact at the concrete checker.** `check_spec decCert` supplies the paper's accepted-code inequality through `CheckSpec`; `budget_canon` proves it independently from canonicity of the certificate encoding. The older `strict_overhead` is a narrower CT-tree theorem, rather than the main counterpart.

**Extra-premise satisfiability:** `check_spec decCert` is a theorem and leaves no checker premise; `budget_canon` establishes the encoding fact. `strict_overhead` has no premise but concerns CT derivation-shaped trees.

**Suite coverage:** `check_spec` → `lcert.formal-test.check_spec/f7-concrete-checker`; `budget_canon` → `lcert.formal-test.certcanon/f7-canon-sizes`. `strict_overhead` → `lcert.verified-test/the-theorems-are-kernel-checked` (separate legacy suite; no `test-formal` assertion).

### Lemma 2.8

**Formal constants:** `stdRI_pr` (`code/lcert/formal/lcert/formal/rint.clj:154`), `lemma28_print` (`code/lcert/formal/lcert/formal/section4b.clj`).

**Kernel-held elaborated type:**

`stdRI_pr`:

```lean
∀ (c : Code), @Eq.{1} Code (riPr stdRI c) c
```

`lemma28_print`:

```lean
∀ (chkf : Code → Code → Bool) (v : Code) (off extra : Nat),
  WRed chkf (skels (thetaD (off + (cnodes v + extra))))
    (Exp.prn (litAt v off)) (codeTerm v)
  ∧ codeOf (codeTerm v) = some v
```

**Verdict: exact for canonical certificate literals.** `lemma28_print` says that the object-language `print (litAt v off)` reduces to `codeTerm v` by the calculus’s conversion rules, and that reading the resulting canonical term returns exactly `v`. Tree equality preserves every label and the internal-node count. The token context supplies `cnodes v` entries, with arbitrary offset and extra entries. `stdRI_pr` remains the semantic counterpart.

**Extra-premise satisfiability:** No extra premise for `lemma28_print` or `stdRI_pr`.

**Suite coverage:** `lemma28_print` → `lcert.formal-test.section4b/f4-section4b`; `stdRI_pr` → `lcert.formal-test.rint/f4-rint`.

### Lemma 3.1

**Formal constants:** `lemma31` (`code/lcert/formal/lcert/formal/substitution.clj:1399`).

**Kernel-held elaborated type:**

`lemma31`:

```lean
∀ (chkf : Code → Code → Bool), ∀ (dec : Code → Option (Prod Nat (Prod Exp Exp))), ∀ (encTy : Exp → Code), ∀ (n : Nat), ∀ (w0 : Bool), ∀ (Gp : List Sk), ∀ (t : Exp), ∀ (s : Sk), ∀ (der : SkJ w0 Gp t s), ∀ (G : List Sk), ∀ (sg : Nat → Exp), ∀ (en : HEnv G), ∀ (hs : SubOK Gp sg G), @Eq.{1} (Car s) (den chkf dec encTy n (subst sg t) G s en) (den chkf dec encTy n t Gp s (envOf chkf dec encTy n Gp sg G en))
```

**Verdict: exact (fidelity pass, 2026-10-15).** R4's Lemma 3.1 now states its typing hypothesis: `t` skeleton-typed, and each replacement skeleton-typed at its variable's skeleton, which is `SubOK`. Without it the equation fails (the branch-list counterexample of substitution.clj §5), and the paper's denotation `⟦u⟧` presupposes it.

**Extra-premise satisfiability:** `subOK_id` satisfies SubOK for identity substitution; `subOK_cons` extends it when a replacement is skeleton-typed. Arbitrary untyped replacement need not satisfy it.

**Suite coverage:** `lemma31` → `lcert.formal-test.substitution/f3h-lemma-3-1`.

### Lemma 3.2

**Formal constants:** `conv_all` (`code/lcert/formal/lcert/formal/convcase.clj:40`).

**Kernel-held elaborated type:**

`conv_all`:

```lean
ConvAll
```

**Verdict: exact for the paper's syntax (fidelity pass, 2026-10-15).** `Cv`'s `nbr` condition excludes branch lists in argument position. Branch lists are the formalization's representation of `caseLbl`'s branch family and occur only in branch position; the paper's syntax has no branch-list terms, so `nbr` holds of every paper expression. The counterexample without `nbr` uses a term the paper cannot write.

**Extra-premise satisfiability:** `cv_wf_left` and `conv_all` discharge typed conversion for calculus derivations; there is a counterexample without `nbr`.

**Suite coverage:** `conv_all` → `lcert.formal-test.convcase/f3m-convcase-path`.

### Lemma 3.3

**Formal constants:** `V_mono` (`code/lcert/formal/lcert/formal/mono.clj:89`), `cv_V` (`code/lcert/formal/lcert/formal/convcase.clj:3939`), `lemma33_subst` (`code/lcert/formal/lcert/formal/substitution.clj:1555`).

**Kernel-held elaborated type:**

`V_mono`:

```lean
∀ (chkf : Code → Code → Bool), ∀ (dec : Code → Option (Prod Nat (Prod Exp Exp))), ∀ (encTy : Exp → Code), ∀ (n : Nat), ∀ (A : Exp), ∀ (G : List Sk), ∀ (en : HEnv G), ∀ (k : Nat), ∀ (k2 : Nat), ∀ (s : Sk), ∀ (v : Car s), ∀ (l : LE.le Nat instLENat k k2), ∀ (m : LE.le Nat instLENat k2 n), ∀ (n : V chkf dec encTy n A G en k s v), V chkf dec encTy n A G en k2 s v
```

`cv_V`:

```lean
∀ (chkf : Code → Code → Bool), ∀ (dec : Code → Option (Prod Nat (Prod Exp Exp))), ∀ (encTy : Exp → Code), ∀ (cap : Nat), ∀ (G : List Sk), ∀ (A : Exp), ∀ (B : Exp), ∀ (der : Cv chkf G A B), ∀ (g : ∀ (s : Sk), Car s), ∀ (en : HEnv G), ∀ (k : Nat), @Eq.{1} Prop (V chkf dec encTy cap A G en k (skel A) (g (skel A))) (V chkf dec encTy cap B G en k (skel B) (g (skel B)))
```

`lemma33_subst`:

```lean
∀ (chkf : Code → Code → Bool), ∀ (dec : Code → Option (Prod Nat (Prod Exp Exp))), ∀ (encTy : Exp → Code), ∀ (n : Nat), ∀ (Gp : List Sk), ∀ (A : Exp), ∀ (der : SkJ Bool.true Gp A Sk.unit), ∀ (G : List Sk), ∀ (sg : Nat → Exp), ∀ (en : HEnv G), ∀ (hs : SubOK Gp sg G), ∀ (k : Nat), ∀ (v : Car (skel A)), Iff (V chkf dec encTy n (subst sg A) G en k (skel A) v) (V chkf dec encTy n A Gp (envOf chkf dec encTy n Gp sg G en) k (skel A) v)
```

**Verdict: exact (fidelity pass, 2026-10-15).** Monotonicity is exact. Substitution has Lemma 3.1's typing hypothesis, which R4 now states. Conversion has Lemma 3.2's `nbr`, which every paper expression satisfies.

**Extra-premise satisfiability:** `subOK_id`/`subOK_cons` witness substitution premises; `conv_all` handles derivation conversion; monotonicity has no extra premise.

**Suite coverage:** `V_mono` → `lcert.formal-test.mono/f3g-monotonicity`; `cv_V` → `lcert.formal-test.convcase/f3m-convcase-path`; `lemma33_subst` → `lcert.formal-test.substitution/f3h-lemma-3-1`.

### Lemma 3.4

**Formal constants:** `Lemma_3_4` (`code/lcert/formal/lcert/formal/mono.clj:113`).

**Kernel-held elaborated type:**

`Lemma_3_4`:

```lean
∀ (chkf : Code → Code → Bool), ∀ (dec : Code → Option (Prod Nat (Prod Exp Exp))), ∀ (encTy : Exp → Code), ∀ (D : Exp), ∀ (m : Nat), ∀ (n : Nat), ∀ (k : Nat), ∀ (G : List Sk), ∀ (G2 : List Sk), ∀ (en : HEnv G), ∀ (en2 : HEnv G2), ∀ (s : Sk), ∀ (v : Car s), ∀ (n : @Eq.{1} Bool (isBaseTy D) Bool.true), ∀ (o : Nat.le m k), ∀ (p : V chkf dec encTy m D G en m s v), V chkf dec encTy n D G2 en2 k s v
```

**Verdict: exact.** Base data types are explicitly enumerated by the formal predicate.

**Extra-premise satisfiability:** No extra premise beyond base-type classification and budget inequalities in the paper.

**Suite coverage:** `Lemma_3_4` → `lcert.formal-test.mono/f3g-monotonicity`.

### Lemma 3.5

**Formal constants:** `EnvSat_split` (`code/lcert/formal/lcert/formal/splitting.clj:171`), `EnvSat_omega` (`code/lcert/formal/lcert/formal/splitting.clj:206`).

**Kernel-held elaborated type:**

`EnvSat_split`:

```lean
∀ (chkf : Code → Code → Bool), ∀ (dec : Code → Option (Prod Nat (Prod Exp Exp))), ∀ (encTy : Exp → Code), ∀ (n : Nat), ∀ (D : List Exp), ∀ (us1 : List U), ∀ (us2 : List U), ∀ (en : HEnv (skels D)), ∀ (k : Nat), ∀ (j : EnvSat chkf dec encTy n D (vadd us1 us2) en k), @Exists.{1} Nat (fun (k1 : Nat) => @Exists.{1} Nat (fun (k2 : Nat) => And (Nat.le (Nat.add k1 k2) k) (And (EnvSat chkf dec encTy n D us1 en k1) (EnvSat chkf dec encTy n D us2 en k2))))
```

`EnvSat_omega`:

```lean
∀ (chkf : Code → Code → Bool), ∀ (dec : Code → Option (Prod Nat (Prod Exp Exp))), ∀ (encTy : Exp → Code), ∀ (n : Nat), ∀ (D : List Exp), ∀ (us : List U), ∀ (en : HEnv (skels D)), ∀ (k : Nat), ∀ (i : EnvSat chkf dec encTy n D (vscale U.uw us) en k), EnvSat chkf dec encTy n D us en 0
```

**Verdict: exact.** Context splitting and omega scaling are formalized.

**Extra-premise satisfiability:** No extra premise beyond EnvSat and the paper's context split/scaling conditions.

**Suite coverage:** `EnvSat_split` → `lcert.formal-test.splitting/f3h-lemma-3-5`; `EnvSat_omega` → `lcert.formal-test.splitting/f3h-lemma-3-5`.

### Lemma 3.6

**Formal constants:** `Lemma_3_6` (`code/lcert/formal/lcert/formal/model.clj:138`), `lemma36` (`code/lcert/formal/lcert/formal/lemma36.clj:144`), `Lemma_3_6_holds` (`code/lcert/formal/lcert/formal/convcase.clj:41`).

**Kernel-held elaborated type:**

`Lemma_3_6`:

```lean
Prop
```

Stored proposition definition:

```lean
∀ (chkf : Code → Code → Bool), ∀ (dec : Code → Option (Prod Nat (Prod Exp Exp))), ∀ (encTy : Exp → Code), ∀ (d : CheckSpec chkf dec encTy), ∀ (n : Nat), ∀ (D : List Exp), ∀ (us : List U), ∀ (t : Exp), ∀ (A : Exp), ∀ (j : WFCtx chkf D), ∀ (k : Rt chkf D us t A), ∀ (en : HEnv (skels D)), ∀ (k : Nat), ∀ (n : Nat.le k n), ∀ (o : EnvSat chkf dec encTy n D us en k), V chkf dec encTy n A (skels D) en k (skel A) (den chkf dec encTy n t (skels D) (skel A) en)
```

`lemma36`:

```lean
∀ (chkf : Code → Code → Bool), ∀ (dec : Code → Option (Prod Nat (Prod Exp Exp))), ∀ (encTy : Exp → Code), ∀ (hcs : CheckSpec chkf dec encTy), ∀ (hconv : ∀ (n : Nat), ConvCase chkf dec encTy n), ∀ (n : Nat), ∀ (D : List Exp), ∀ (us : List U), ∀ (t : Exp), ∀ (A : Exp), ∀ (der : Rt chkf D us t A), ∀ (hw : WFCtx chkf D), Sound chkf dec encTy n D us t A
```

`Lemma_3_6_holds`:

```lean
Lemma_3_6
```

**Verdict: exact at `Check` (fidelity pass, 2026-10-15).** `lemma36_F7` (concrete.clj) is the lemma at `Check decCert`, with `CheckSpec` and typed conversion discharged (`check_spec`, `conv_all`). R4 now states the well-formed context (`WFCtx`) the paper presupposed; token contexts satisfy it (`wf_theta`). The `Syn` and `R` label bounds are part of `V`, as R4 §1.2 now says.

**Extra-premise satisfiability:** `check_spec decCert` discharges `CheckSpec`; `wf_theta` discharges `WFCtx` at token roots; `conv_all` discharges conversion. `check_complete_lbl` gives label-safe concrete certificates under its typed premises. Arbitrary contexts and raw codes are outside the stated domain.

**Suite coverage:** `Lemma_3_6` → `lcert.formal-test.model/f3e-statements-and-t2`; `lemma36` → `lcert.formal-test.lemma36/f3n-lemma36-and-main-results`; `Lemma_3_6_holds` → `lcert.formal-test.convcase/f3m-convcase-path`.

### Theorem 1 / T1

**Formal constants:** `Theorem_1` (`code/lcert/formal/lcert/formal/model.clj:148`), `theorem1` (`code/lcert/formal/lcert/formal/lemma36.clj:154`), `consistent_concrete` (`code/lcert/formal/lcert/formal/selfjust.clj:31`).

**Kernel-held elaborated type:**

`Theorem_1`:

```lean
Prop
```

Stored proposition definition:

```lean
∀ (chkf : Code → Code → Bool), ∀ (dec : Code → Option (Prod Nat (Prod Exp Exp))), ∀ (encTy : Exp → Code), ∀ (d : CheckSpec chkf dec encTy), ∀ (n : Nat), ∀ (t : Exp), Not (Rt chkf (thetaD n) (thetaU n) t Exp.tEmpty)
```

`theorem1`:

```lean
∀ (chkf : Code → Code → Bool), ∀ (dec : Code → Option (Prod Nat (Prod Exp Exp))), ∀ (encTy : Exp → Code), ∀ (hcs : CheckSpec chkf dec encTy), ∀ (hconv : ∀ (n : Nat), ConvCase chkf dec encTy n), ∀ (n : Nat), ∀ (t : Exp), Rt chkf (thetaD n) (thetaU n) t Exp.tEmpty → False
```

`consistent_concrete`:

```lean
∀ (n : Nat), ∀ (t : Exp), Not (Rt (Check decCert) (thetaD n) (thetaU n) t Exp.tEmpty)
```

**Verdict: exact.** Consistency is relative to Ansatz and bundled Init; CheckSpec is discharged at Check decCert.

**Extra-premise satisfiability:** `check_spec decCert`, `wf_theta`, and `conv_all` discharge the generic checker/context/conversion premises in `consistent_concrete`.

**Suite coverage:** `Theorem_1` → `lcert.formal-test.model/f3e-statements-and-t2`; `theorem1` → `lcert.formal-test.lemma36/f3n-lemma36-and-main-results`; `consistent_concrete` → `lcert.formal-test.selfjust/self-justification`.

### Theorem 2 / T2

**Formal constants:** `Theorem_2_H` (`code/lcert/formal/lcert/formal/model.clj:168`), `Theorem_2_H1` (`code/lcert/formal/lcert/formal/model.clj:235`), `self_justification` (`code/lcert/formal/lcert/formal/selfjust.clj:60`).

**Kernel-held elaborated type:**

`Theorem_2_H`:

```lean
∀ (chkf : Code → Code → Bool), Rt chkf (List.nil Exp) (List.nil U) Hterm Hcirc
```

`Theorem_2_H1`:

```lean
∀ (chkf : Code → Code → Bool), Rt chkf (List.nil Exp) (List.nil U) H1term H1circ
```

`self_justification`:

```lean
And (∀ (n : Nat), ∀ (t : Exp), Not (Rt (Check decCert) (thetaD n) (thetaU n) t Exp.tEmpty)) (And (Rt (Check decCert) (List.nil Exp) (List.nil U) Hterm Hcirc) (Rt (Check decCert) (List.nil Exp) (List.nil U) H1term H1circ))
```

**Verdict: exact.** Closed budget-0 inhabitants of Hcirc and H1circ; relative self-justification scope only.

**Extra-premise satisfiability:** The inhabitants use only the object-calculus rules; `self_justification` is concrete and has no checker-spec premise.

**Suite coverage:** `Theorem_2_H` → `lcert.formal-test.model/f3e-statements-and-t2`; `Theorem_2_H1` → `lcert.formal-test.lemma36/f3n-lemma36-and-main-results`; `self_justification` → `lcert.formal-test.selfjust/self-justification`.

### Theorem 3 / T3

**Formal constants:** `theorem3` (`code/lcert/formal/lcert/formal/lemma36.clj:205`), `theorem3_tokens` (`code/lcert/formal/lcert/formal/lemma36.clj:234`).

**Kernel-held elaborated type:**

`theorem3`:

```lean
∀ (chkf : Code → Code → Bool), ∀ (dec : Code → Option (Prod Nat (Prod Exp Exp))), ∀ (encTy : Exp → Code), ∀ (hcs : CheckSpec chkf dec encTy), ∀ (hconv : ∀ (n : Nat), ConvCase chkf dec encTy n), ∀ (n : Nat), ∀ (D : List Exp), ∀ (us : List U), ∀ (t : Exp), ∀ (der : Rt chkf D us t Exp.tR), ∀ (hw : WFCtx chkf D), ∀ (en : HEnv (skels D)), ∀ (k : Nat), ∀ (hk : Nat.le k n), ∀ (hs : EnvSat chkf dec encTy n D us en k), LE.le Nat instLENat (cnodes (den chkf dec encTy n t (skels D) Sk.cert en)) k
```

`theorem3_tokens`:

```lean
∀ (chkf : Code → Code → Bool), ∀ (dec : Code → Option (Prod Nat (Prod Exp Exp))), ∀ (encTy : Exp → Code), ∀ (hcs : CheckSpec chkf dec encTy), ∀ (hconv : ∀ (n : Nat), ConvCase chkf dec encTy n), ∀ (m : Nat), ∀ (n : Nat), ∀ (t : Exp), ∀ (der : Rt chkf (thetaD m) (thetaU m) t Exp.tR), ∀ (hn : Nat.le (cntU (maskUF (thetaU m) (freshF t))) n), LE.le Nat instLENat (cnodes (den chkf dec encTy n t (skels (thetaD m)) Sk.cert (tokEnvD m))) (cntU (maskUF (thetaU m) (freshF t)))
```

**Verdict: exact at `Check` (fidelity pass, 2026-10-15).** `theorem3_F7` and `theorem3_tokens_F7` (concrete.clj) are T3 and its token refinement at `Check decCert`, with no checker premise. R4's T3 now states the well-formed context and, for the refinement, the cap `n ≥ c`, with `c` the number of distinct tokens free in `t`; these are exactly the remaining premises.

**Extra-premise satisfiability:** `check_spec decCert` and `conv_all` discharge model premises; `wf_theta` and `tok_sat_mask` witness token environments. The cap condition is met by choosing n at least the context size.

**Suite coverage:** `theorem3` → `lcert.formal-test.lemma36/f3n-lemma36-and-main-results`; `theorem3_tokens` → `lcert.formal-test.strengthen/f3-theorem3-tokens`.

### Corollary 3.7

**Formal constants:** `Corollary_3_7` (`code/lcert/formal/lcert/formal/model.clj:154`), `cor37_refutation` (`code/lcert/formal/lcert/formal/lemma36.clj:171`), `cor37_contradiction` (`code/lcert/formal/lcert/formal/lemma36.clj:186`).

**Kernel-held elaborated type:**

`Corollary_3_7`:

```lean
Prop
```

Stored proposition definition:

```lean
∀ (chkf : Code → Code → Bool), ∀ (dec : Code → Option (Prod Nat (Prod Exp Exp))), ∀ (encTy : Exp → Code), ∀ (d : CheckSpec chkf dec encTy), And (∀ (c : Code), Not (@Eq.{1} Bool (chkf c (encTy Exp.tEmpty)) Bool.true)) (∀ (c1 : Code), ∀ (c2 : Code), ∀ (d : Code), Not (And (@Eq.{1} Bool (chkf c1 d) Bool.true) (@Eq.{1} Bool (chkf c2 (Code.sn 25 d (Code.sl 15))) Bool.true)))
```

`cor37_refutation`:

```lean
∀ (chkf : Code → Code → Bool), ∀ (dec : Code → Option (Prod Nat (Prod Exp Exp))), ∀ (encTy : Exp → Code), ∀ (hcs : CheckSpec chkf dec encTy), ∀ (hconv : ∀ (n : Nat), ConvCase chkf dec encTy n), ∀ (c : Code), @Eq.{1} Bool (chkf c (encTy Exp.tEmpty)) Bool.false
```

`cor37_contradiction`:

```lean
∀ (chkf : Code → Code → Bool), ∀ (dec : Code → Option (Prod Nat (Prod Exp Exp))), ∀ (encTy : Exp → Code), ∀ (hcs : CheckSpec chkf dec encTy), ∀ (hconv : ∀ (n : Nat), ConvCase chkf dec encTy n), ∀ (c1 : Code), ∀ (c2 : Code), ∀ (d : Code), ∀ (h1 : @Eq.{1} Bool (chkf c1 d) Bool.true), @Eq.{1} Bool (chkf c2 (Code.sn 25 d (Code.sl 15))) Bool.true → False
```

**Verdict: exact.** Both all-code checker exclusions follow from CheckSpec; concrete checker specialization discharges it.

**Extra-premise satisfiability:** `check_spec decCert` and `conv_all` discharge the generic assumptions; concrete checker exclusions hold for every code.

**Suite coverage:** `Corollary_3_7` → `lcert.formal-test.model/f3e-statements-and-t2`; `cor37_refutation` → `lcert.formal-test.lemma36/f3n-lemma36-and-main-results`; `cor37_contradiction` → `lcert.formal-test.lemma36/f3n-lemma36-and-main-results`.

### Proposition 4.1

**Formal constants:** `prop41_closed` (`code/lcert/formal/lcert/formal/section4.clj:36`), `prop41_fun` (`code/lcert/formal/lcert/formal/section4.clj:45`).

**Kernel-held elaborated type:**

`prop41_closed`:

```lean
∀ (chkf : Code → Code → Bool), ∀ (dec : Code → Option (Prod Nat (Prod Exp Exp))), ∀ (encTy : Exp → Code), ∀ (hcs : CheckSpec chkf dec encTy), ∀ (hconv : ∀ (n : Nat), ConvCase chkf dec encTy n), ∀ (n : Nat), ∀ (t : Exp), ∀ (der : Rt chkf (List.nil Exp) (List.nil U) t Exp.tR), @Exists.{1} Nat (fun (l : Nat) => @Eq.{1} Code (den chkf dec encTy n t (List.nil Sk) Sk.cert Unit.unit) (Code.sl l))
```

`prop41_fun`:

```lean
∀ (chkf : Code → Code → Bool), ∀ (dec : Code → Option (Prod Nat (Prod Exp Exp))), ∀ (encTy : Exp → Code), ∀ (hcs : CheckSpec chkf dec encTy), ∀ (hconv : ∀ (n : Nat), ConvCase chkf dec encTy n), ∀ (n : Nat), ∀ (f : Exp), ∀ (der : Rt chkf (List.nil Exp) (List.nil U) f (Exp.tPi U.u1 Exp.tSyn Exp.tR)), ∀ (c : Code), ∀ (hc : @Eq.{1} Bool (lblOk c) Bool.true), @Exists.{1} Nat (fun (l : Nat) => @Eq.{1} Code (den chkf dec encTy n f (List.nil Sk) (Sk.arr Sk.syn Sk.cert) Unit.unit c) (Code.sl l))
```

**Verdict: exact at `Check` (fidelity pass, 2026-10-15).** `prop41_closed_F7` and `prop41_fun_F7` (concrete.clj) discharge `CheckSpec` and typed conversion. The function clause's `lblOk c` says `c` is a code over `L` (R4 §1.2 now records the label representation), so it is the paper's quantification over codes.

**Extra-premise satisfiability:** `check_spec decCert`, `conv_all`, `wf_theta` discharge generic assumptions. The label condition is met by `check_complete_lbl` for typed certificates, not arbitrary codes.

**Suite coverage:** `prop41_closed` → `lcert.formal-test.section4/f4-section4`; `prop41_fun` → `lcert.formal-test.section4/f4-section4`.

### Proposition 4.2

**Formal constants:** `prop42` (`code/lcert/formal/lcert/formal/section4b.clj:1439`).

**Kernel-held elaborated type:**

`prop42`:

```lean
∀ (chkf : Code → Code → Bool), ∀ (dec : Code → Option (Prod Nat (Prod Exp Exp))), ∀ (encTy : Exp → Code), ∀ (A : Exp), ∀ (v : Code), ∀ (hok : @Eq.{1} Bool (lblOk v) Bool.true), ∀ (hA : @Eq.{1} Bool (lblOk (encTy A)) Bool.true), ∀ (hck : @Eq.{1} Bool (chkf v (encTy A)) Bool.true), Rt chkf (thetaD (cnodes v)) (thetaU (cnodes v)) (certTerm v (codeTerm (encTy A))) (boxTy (codeTerm (encTy A)))
```

**Verdict: exact (fidelity pass, 2026-10-15).** `prop42` holds for every checker, so for `Check decCert`. Its `lblOk` premises say that the certificate and the type's code are over `L` (R4 §1.2), which every paper code is.

**Extra-premise satisfiability:** `check_complete_lbl` supplies a label-safe accepted certificate for a derivable closed judgment; arbitrary accepted code does not automatically establish `lblOk` without the canonical encoding theorem.

**Suite coverage:** `prop42` → `lcert.formal-test.section4b/f4-section4b`.

### Proposition 4.3

**Formal constants:** `prop43` (`code/lcert/formal/lcert/formal/prop434.clj:85`).

**Kernel-held elaborated type:**

`prop43`:

```lean
∀ (chkf : Code → Code → Bool), ∀ (dec : Code → Option (Prod Nat (Prod Exp Exp))), ∀ (encTy : Exp → Code), ∀ (hcs : CheckSpec chkf dec encTy), ∀ (hes : TokSize chkf dec), ∀ (A : Exp), ∀ (cA : Exp), ∀ (hcA : @Eq.{1} (Option Code) (codeOf cA) (Option.some Code (encTy A))), ∀ (hbc : @Eq.{1} Bool (closedTy (Exp.tSig U.u1 Exp.tR (chkT (Exp.var 0) cA))) Bool.true), ∀ (w : Code), ∀ (hw : @Eq.{1} Bool (chkf w (encTy (Exp.tSig U.u1 Exp.tR (chkT (Exp.var 0) cA)))) Bool.true), ∀ (M : Nat), ∀ (hM : ∀ (v : Code), ∀ (n : @Eq.{1} Bool (chkf v (encTy A)) Bool.true), LE.le Nat instLENat M (cnodes v)), LT.lt Nat instLTNat (Nat.add M M) (cnodes w)
```

**Verdict: exact at F7 through `prop43_mu_F7`.** The generic `prop43` has the displayed hypotheses; `muRaw_witness` selects the least accepted size and `prop43_mu_F7` specializes the bound at `Check decCert`.

**Extra-premise satisfiability:** `check_toksize decCert` and `check_spec decCert` supply the checker premises. Choose `cA = cTerm (encTy A)`: `cterm_code` supplies its decoding equation and `box_closed` supplies closedness. `raw_min_exists` and `muRaw_witness` supply the minimum for raw certifiability.

**Suite coverage:** `prop43` → `lcert.formal-test.prop434/f4-prop43-44`; `prop43_mu_F7` → `lcert.formal-test.thm46f7/f4-minimum-certificate`.

### Proposition 4.4

**Formal constants:** `prop44_1` (`code/lcert/formal/lcert/formal/prop434.clj:191`), `prop44_2` (`code/lcert/formal/lcert/formal/prop434.clj:311`), `prop44_3` (`code/lcert/formal/lcert/formal/prop434.clj:275`).

**Kernel-held elaborated type:**

`prop44_1`:

```lean
∀ (chkf : Code → Code → Bool), ∀ (dec : Code → Option (Prod Nat (Prod Exp Exp))), ∀ (encTy : Exp → Code), ∀ (hcs : CheckSpec chkf dec encTy), ∀ (hes : TokSize chkf dec), ∀ (hts : TypeSize chkf dec encTy), ∀ (k : Nat), ∀ (q : Exp), ∀ (der : Rt chkf (List.cons Exp Exp.tR (thetaD k)) (List.cons U U.u1 (thetaU k)) q Exp.tR), ∀ (hmap : ∀ (n : Nat), ∀ (A : Exp), ∀ (v : Code), ∀ (m : @Eq.{1} Bool (closedTy A) Bool.true), ∀ (n : @Eq.{1} Bool (chkf v (encTy A)) Bool.true), ∀ (o : @Eq.{1} Bool (lblOk v) Bool.true), @Eq.{1} Bool (chkf (den chkf dec encTy n q (List.cons Sk Sk.cert (skels (thetaD k))) Sk.cert (Prod.mk Code (HEnv (skels (thetaD k))) v (tokEnvD k))) (encTy (Exp.tSig U.u1 Exp.tR (chkT (Exp.var 0) (cTerm (encTy A)))))) Bool.true), ∀ (v : Code), ∀ (hv : @Eq.{1} Bool (chkf v (encTy (negN (Nat.add k 1)))) Bool.true), ∀ (hvok : @Eq.{1} Bool (lblOk v) Bool.true), (∀ (v2 : Code), ∀ (o : @Eq.{1} Bool (chkf v2 (encTy (negN (Nat.add k 1)))) Bool.true), LE.le Nat instLENat (cnodes v) (cnodes v2)) → False
```

`prop44_2`:

```lean
∀ (chkf : Code → Code → Bool), ∀ (dec : Code → Option (Prod Nat (Prod Exp Exp))), ∀ (encTy : Exp → Code), ∀ (hcs : CheckSpec chkf dec encTy), ∀ (hes : TokSize chkf dec), ∀ (hts : TypeSize chkf dec encTy), ∀ (k : Nat), ∀ (q : Exp), ∀ (der : Rt chkf (thetaD k) (thetaU k) q (Exp.tPi U.u1 (Exp.tSig U.u1 Exp.tR (chkT (Exp.var 0) (cTerm (encTy (negN (Nat.add k 1)))))) (Exp.tSig U.u1 Exp.tR (chkT (Exp.var 0) (cTerm (encTy (Exp.tSig U.u1 Exp.tR (chkT (Exp.var 0) (cTerm (encTy (negN (Nat.add k 1)))))))))))), ∀ (v : Code), ∀ (hv : @Eq.{1} Bool (chkf v (encTy (negN (Nat.add k 1)))) Bool.true), ∀ (hvok : @Eq.{1} Bool (lblOk v) Bool.true), (∀ (v2 : Code), ∀ (n : @Eq.{1} Bool (chkf v2 (encTy (negN (Nat.add k 1)))) Bool.true), LE.le Nat instLENat (cnodes v) (cnodes v2)) → False
```

`prop44_3`:

```lean
∀ (chkf : Code → Code → Bool), ∀ (dec : Code → Option (Prod Nat (Prod Exp Exp))), ∀ (encTy : Exp → Code), ∀ (hcs : CheckSpec chkf dec encTy), ∀ (hts : TypeSize chkf dec encTy), ∀ (k : Nat), ∀ (q : Exp), ∀ (der : Rt chkf (thetaD k) (thetaU k) q (Exp.tPi U.u1 (Exp.tSig U.u1 Exp.tR (chkT (Exp.var 0) (cTerm (encTy (negN (Nat.add k 1)))))) (Exp.tSig U.u1 (Exp.tSig U.u1 Exp.tR (chkT (Exp.var 0) (cTerm (encTy (negN (Nat.add k 1)))))) (Exp.tSig U.u1 Exp.tR (chkT (Exp.var 0) (cTerm (encTy (negN (Nat.add k 1))))))))), ∀ (v : Code), ∀ (hv : @Eq.{1} Bool (chkf v (encTy (negN (Nat.add k 1)))) Bool.true), ∀ (hvok : @Eq.{1} Bool (lblOk v) Bool.true), (∀ (v2 : Code), ∀ (m : @Eq.{1} Bool (chkf v2 (encTy (negN (Nat.add k 1)))) Bool.true), LE.le Nat instLENat (cnodes v) (cnodes v2)) → False
```

**Verdict: exact at `Check` for the paper's family (fidelity pass, 2026-10-15).** `prop44_paper_1`–`3` (prop44paper.clj) state the three parts as R4 does, at `Check decCert` and every `k`, with `A = A_{k+1} = 1 ⊸ ⋯ ⊸ 1`; `arrN_certifiable` (arrfamily.clj) supplies its certificates. **Finding:** `prop44_1`–`3` used `¬ᵏ⁺¹1`, which has no certificate for even `k` (`1 ⊸ 0` would refute `1`), so their certificate hypotheses were unsatisfiable and the theorems vacuous at every even `k`. `prop44g_1`–`3` restate the proofs for any closed `A` whose code exceeds `k` nodes, with minimality over label-safe certificates, which is all the proofs use.

**Extra-premise satisfiability:** `check_toksize decCert`, `check_typesize decCert`, and `check_spec decCert` discharge generic size/checker premises; the A_j witnesses come from the concrete checker construction.

**Suite coverage:** `prop44_1` → `lcert.formal-test.prop434/f4-prop43-44`; `prop44_2` → `lcert.formal-test.prop434/f4-prop43-44`; `prop44_3` → `lcert.formal-test.prop434/f4-prop43-44`.

### Proposition 4.5

**Formal constants:** `prop45_d3` (`code/lcert/formal/lcert/formal/section4b.clj:2204`), `prop45_contraction` (`code/lcert/formal/lcert/formal/section4b.clj:2836`).

**Kernel-held elaborated type:**

`prop45_d3`:

```lean
∀ (chkf : Code → Code → Bool), ∀ (dec : Code → Option (Prod Nat (Prod Exp Exp))), ∀ (encTy : Exp → Code), ∀ (A : Exp), ∀ (w : Code), ∀ (hok : @Eq.{1} Bool (lblOk w) Bool.true), ∀ (hA : @Eq.{1} Bool (lblOk (encTy A)) Bool.true), ∀ (hB : @Eq.{1} Bool (lblOk (encTy (boxTy (codeTerm (encTy A))))) Bool.true), ∀ (hck : @Eq.{1} Bool (chkf w (encTy (boxTy (codeTerm (encTy A))))) Bool.true), Rt chkf (thetaD (cnodes w)) (thetaU (cnodes w)) (Exp.lam U.u1 (boxTy (codeTerm (encTy A))) (Exp.pair (boxTy (codeTerm (encTy (boxTy (codeTerm (encTy A)))))) (litAt w 1) Exp.star)) (Exp.tPi U.u1 (boxTy (codeTerm (encTy A))) (boxTy (codeTerm (encTy (boxTy (codeTerm (encTy A)))))))
```

`prop45_contraction`:

```lean
∀ (chkf : Code → Code → Bool), ∀ (dec : Code → Option (Prod Nat (Prod Exp Exp))), ∀ (encTy : Exp → Code), ∀ (A : Exp), ∀ (v : Code), ∀ (hok : @Eq.{1} Bool (lblOk v) Bool.true), ∀ (hA : @Eq.{1} Bool (lblOk (encTy A)) Bool.true), ∀ (hck : @Eq.{1} Bool (chkf v (encTy A)) Bool.true), Rt chkf (thetaD (Nat.add (cnodes v) (cnodes v))) (thetaU (Nat.add (cnodes v) (cnodes v))) (Exp.lam U.u1 (boxTy (codeTerm (encTy A))) (Exp.pair (Exp.tSig U.u1 (boxTy (codeTerm (encTy A))) (boxTy (codeTerm (encTy A)))) (Exp.pair (boxTy (codeTerm (encTy A))) (litAt v (Nat.add (cnodes v) 1)) Exp.star) (Exp.pair (boxTy (codeTerm (encTy A))) (litAt v 1) Exp.star))) (Exp.tPi U.u1 (boxTy (codeTerm (encTy A))) (Exp.tSig U.u1 (boxTy (codeTerm (encTy A))) (boxTy (codeTerm (encTy A)))))
```

**Verdict: exact at the paper's budgets (fidelity pass, 2026-10-15).** `prop45_contraction_mu` and `prop45_d3_mu` (concrete.clj) build the terms at budgets `2 μ_L(A)` and `μ_L(□A)`, from a least label-safe certificate (`least_cert_witness`). `prop45_d3` and `prop45_contraction` remain the any-certificate forms.

**Extra-premise satisfiability:** `check_complete_lbl` gives accepted label-safe certificate witnesses for derivable closed A; the explicit label-bound premises are met by that theorem. A least-number argument can choose a minimum among such certificates, but no named kernel theorem makes that selection, so the bridge to `μ` is a finding. `prop45_d3` and `prop45_contraction` themselves have no `CheckSpec` premise.

**Suite coverage:** `prop45_d3` → `lcert.formal-test.section4b/f4-section4b`; `prop45_contraction` → `lcert.formal-test.section4b/f4-section4b`.

### Theorem 4.6

**Formal constants:** `thm46_types` (`code/lcert/formal/lcert/formal/theorem46.clj:340`), `thm46_types_F7` (`code/lcert/formal/lcert/formal/thm46f7.clj:184`).

**Kernel-held elaborated type:**

`thm46_types`:

```lean
∀ (chkf : Code → Code → Bool), ∀ (dec : Code → Option (Prod Nat (Prod Exp Exp))), ∀ (encTy : Exp → Code), ∀ (hcs : CheckSpec chkf dec encTy), ∀ (sz : Exp → Nat), ∀ (henc : Enc46 chkf dec sz), ∀ (r : Nat), ∀ (sg : Nat → Code), ∀ (hsg : PhOk (Nat.add r 1) sg), ∀ (As : Nat → Exp), ∀ (B : Exp), ∀ (k : Nat), ∀ (t : Exp), ∀ (hclA : ∀ (i : Nat), ∀ (o : LT.lt Nat instLTNat i (Nat.add r 1)), @Eq.{1} Bool (closedTy (As i)) Bool.true), ∀ (hclB : @Eq.{1} Bool (closedTy B) Bool.true), ∀ (hc : ∀ (i : Nat), ∀ (q : LT.lt Nat instLTNat i (Nat.add r 1)), @Eq.{1} Bool (chkf (sg i) (encTy (As i))) Bool.true), ∀ (hbig : ∀ (i : Nat), ∀ (r : LT.lt Nat instLTNat i (Nat.add r 1)), ∀ (m : Nat), ∀ (u : Exp), ∀ (A : Exp), ∀ (v : @Eq.{1} (Option (Prod Nat (Prod Exp Exp))) (dec (sg i)) (Option.some (Prod Nat (Prod Exp Exp)) (Prod.mk Nat (Prod Exp Exp) m (Prod.mk Exp Exp u A)))), LT.lt Nat instLTNat k (sz u)), ∀ (hne : ∀ (i : Nat), ∀ (s : LT.lt Nat instLTNat i (Nat.add r 1)), @Eq.{1} Exp B (As i) → False), ∀ (der : Rt chkf (thetaD k) (thetaU k) t (Exp.tPi U.u1 (tensB (fun (i : Nat) => encTy (As i)) 0 r) (boxTy (codeTerm (encTy B))))), @Exists.{1} Code (fun (v : Code) => And (@Eq.{1} Bool (chkf v (encTy B)) Bool.true) (And (@Eq.{1} Bool (lblOk v) Bool.true) (LE.le Nat instLENat (cnodes v) k)))
```

`thm46_types_F7`:

```lean
∀ (r : Nat), ∀ (sg : Nat → Code), ∀ (hsg : PhOk (Nat.add r 1) sg), ∀ (As : Nat → Exp), ∀ (B : Exp), ∀ (k : Nat), ∀ (t : Exp), ∀ (hclB : @Eq.{1} Bool (closedTy B) Bool.true), ∀ (hc : ∀ (i : Nat), ∀ (j : LT.lt Nat instLTNat i (Nat.add r 1)), @Eq.{1} Bool (Check decCert (sg i) (encE (As i))) Bool.true), ∀ (hne : ∀ (i : Nat), ∀ (k : LT.lt Nat instLTNat i (Nat.add r 1)), @Eq.{1} Exp B (As i) → False), ∀ (der : Rt (Check decCert) (thetaD k) (thetaU k) t (Exp.tPi U.u1 (tensB (fun (i : Nat) => encE (As i)) 0 r) (boxTy (codeTerm (encE B))))), @Exists.{1} Code (fun (v : Code) => And (@Eq.{1} Bool (Check decCert v (encE B)) Bool.true) (And (@Eq.{1} Bool (lblOk v) Bool.true) (LE.le Nat instLENat (cnodes v) k)))
```

**Verdict: exact at `Check` (fidelity pass, 2026-10-15).** `thm46_mu_F7` (concrete.clj) concludes `μ(B) ≤ k`, the paper's form, from `thm46_types_F7` and `muRaw_lower`. The certifiable `A_i` come with chosen certificates `sg i`, whose labels lie in `L` (`PhOk`).

**Extra-premise satisfiability:** `check_spec decCert` and `enc46_canon` discharge generic CheckSpec/Enc46. `thm46_hyps_sat` jointly witnesses CheckSpec, Enc46 and LargeCert in a toy checker. `phok_safe` supplies `PhOk J safePhantoms` at every J. For the intended arbitrary list of certifiable premises, take `sg i` to be each accepted label-safe certificate; `check_complete_lbl` constructs such a certificate from the corresponding closed derivation. Its `lblOk` conclusion supplies the `PhOk` hypothesis pointwise.

**Suite coverage:** `thm46_types` → `lcert.formal-test.theorem46/f4-theorem46`; `thm46_types_F7` → `lcert.formal-test.thm46f7/f4-thm46f7-theorems`.

### Lemma 4.6a

**Formal constants:** `lemma46a` (`code/lcert/formal/lcert/formal/lemma46a.clj:86`), `lemma36_ri` (`code/lcert/formal/lcert/formal/ri_lemma36.clj:155`), `phok_safe` (`code/lcert/formal/lcert/formal/rint.clj`).

**Kernel-held elaborated type:**

`phok_safe`: `∀ (J : Nat), PhOk J safePhantoms`, where `safePhantoms i = Code.sl 0`.

`lemma46a`:

```lean
∀ (chkf : Code → Code → Bool), ∀ (dec : Code → Option (Prod Nat (Prod Exp Exp))), ∀ (encTy : Exp → Code), ∀ (hcs : CheckSpec chkf dec encTy), ∀ (J : Nat), ∀ (sg : Nat → Code), ∀ (hsg : PhOk J sg), ∀ (n : Nat), ∀ (D : List Exp), ∀ (us : List U), ∀ (t : Exp), ∀ (A : Exp), ∀ (der : Rt chkf D us t A), ∀ (hw : WFCtx chkf D), Sound_ri chkf dec encTy (phRI J sg hsg) n D us t A
```

`lemma36_ri`:

```lean
∀ (chkf : Code → Code → Bool), ∀ (dec : Code → Option (Prod Nat (Prod Exp Exp))), ∀ (encTy : Exp → Code), ∀ (ri : RInt), ∀ (hcs : CheckSpec chkf dec encTy), ∀ (hrl : RLaws ri), ∀ (hph : PhCons chkf encTy ri), ∀ (hconv : ∀ (n : Nat), ConvCase_ri chkf dec encTy ri n), ∀ (n : Nat), ∀ (D : List Exp), ∀ (us : List U), ∀ (t : Exp), ∀ (A : Exp), ∀ (der : Rt chkf D us t A), ∀ (hw : WFCtx chkf D), Sound_ri chkf dec encTy ri n D us t A
```

**Verdict: exact at `Check` (fidelity pass, 2026-10-15).** `lemma46a_F7` (concrete.clj) discharges `CheckSpec`; its remaining premises are the well-formed context R4 now states and phantom labels in `L` (`PhOk`).

**Extra-premise satisfiability:** `check_spec decCert` discharges CheckSpec and `wf_theta` supplies `WFCtx` at token contexts. `phok_safe` supplies `PhOk J safePhantoms` at every J, including nonzero J. For the intended certificate family, each label-safe witness from `check_complete_lbl` supplies one `lblOk` premise of `PhOk`. `stdRI_laws` separately witnesses the standard interpretation.

**Suite coverage:** `lemma46a` → `lcert.formal-test.lemma46a/f4-lemma46a`; `lemma36_ri` → `lcert.formal-test.ri_lemma36/f4-ri-lemma36`; `phok_safe` → `lcert.formal-test.rint/f4-rint` (focused warm-server run: 1 test, 63 assertions, 0 failures, 0 errors).

### Corollary 4.6′

**Formal constants:** `cor46_prime_iff_F7` (`code/lcert/formal/lcert/formal/thm46f7.clj:273`), `d2_no_uniform` (`code/lcert/formal/lcert/formal/cor46.clj:597`).

**Kernel-held elaborated type:**

`cor46_prime_iff_F7`:

```lean
∀ (A : Exp), ∀ (B : Exp), ∀ (hclB : @Eq.{1} Bool (closedTy B) Bool.true), ∀ (hne : @Eq.{1} Exp B A → False), ∀ (c0 : Code), ∀ (h0 : @Eq.{1} Bool (Check decCert c0 (encE (Exp.tPi U.u1 A B))) Bool.true), ∀ (l0 : @Eq.{1} Bool (lblOk c0) Bool.true), ∀ (c1 : Code), ∀ (h1 : @Eq.{1} Bool (Check decCert c1 (encE A)) Bool.true), ∀ (l1 : @Eq.{1} Bool (lblOk c1) Bool.true), ∀ (kk : Nat), Iff (@Exists.{1} Exp (fun (tt : Exp) => Rt (Check decCert) (thetaD kk) (thetaU kk) tt (Exp.tPi U.u1 (Exp.tSig U.u1 (boxTy (codeTerm (encE (Exp.tPi U.u1 A B)))) (boxTy (codeTerm (encE A)))) (boxTy (codeTerm (encE B)))))) (@Exists.{1} Code (fun (v : Code) => And (@Eq.{1} Bool (Check decCert v (encE B)) Bool.true) (And (@Eq.{1} Bool (lblOk v) Bool.true) (LE.le Nat instLENat (cnodes v) kk))))
```

`d2_no_uniform`:

```lean
∀ (chkf : Code → Code → Bool), ∀ (dec : Code → Option (Prod Nat (Prod Exp Exp))), ∀ (encTy : Exp → Code), ∀ (hcs : CheckSpec chkf dec encTy), ∀ (hts : TypeSize chkf dec encTy), ∀ (sz : Exp → Nat), ∀ (henc : Enc46 chkf dec sz), ∀ (k : Nat), ∀ (hLAB : LargeCert chkf dec sz (encTy (Exp.tPi U.u1 Exp.tUnit (negN (Nat.add k 1))))), ∀ (hLA : LargeCert chkf dec sz (encTy Exp.tUnit)), ∀ (t : Exp), Rt chkf (thetaD k) (thetaU k) t (Exp.tPi U.u1 (Exp.tSig U.u1 (boxTy (codeTerm (encTy (Exp.tPi U.u1 Exp.tUnit (negN (Nat.add k 1)))))) (boxTy (codeTerm (encTy Exp.tUnit)))) (boxTy (codeTerm (encTy (negN (Nat.add k 1)))))) → False
```

**Verdict: exact at F7 for the corrected `μ_L` threshold.** `cor46_prime_mu_F7` proves the iff when the two input certificates are label-safe, `B` has a label-safe certificate, `B` is closed and `A ≠ B`. The paper now names `μ_L` separately from the least size among all accepted codes; the no-uniform-budget part uses TypeSize and E5.

**Extra-premise satisfiability:** `check_spec decCert`, `enc46_canon`, `check_typesize decCert` discharge checker/encoding/size assumptions. Certifiability of A and A-lolli-B is the paper's premise; `check_complete_lbl` constructs witnesses from derivations. No single all-input witness theorem is expected.

**Suite coverage:** `cor46_prime_iff_F7` → `lcert.formal-test.thm46f7/f4-thm46f7-theorems`; `cor46_prime_mu_F7` → `lcert.formal-test.thm46f7/f4-minimum-certificate`; `d2_no_uniform` → `lcert.formal-test.cor46/f4-cor46`.

### Corollary 4.6″

**Formal constants:** `cor46_dprime_iff_F7` (`code/lcert/formal/lcert/formal/thm46f7.clj:322`), `d3_gap_F7` (`code/lcert/formal/lcert/formal/thm46f7.clj:342`).

**Kernel-held elaborated type:**

`cor46_dprime_iff_F7`:

```lean
∀ (A : Exp), ∀ (c : Code), ∀ (hc : @Eq.{1} Bool (Check decCert c (encE A)) Bool.true), ∀ (lc : @Eq.{1} Bool (lblOk c) Bool.true), ∀ (kk : Nat), Iff (@Exists.{1} Exp (fun (tt : Exp) => Rt (Check decCert) (thetaD kk) (thetaU kk) tt (Exp.tPi U.u1 (boxTy (codeTerm (encE A))) (boxTy (codeTerm (encE (boxTy (codeTerm (encE A))))))))) (@Exists.{1} Code (fun (v : Code) => And (@Eq.{1} Bool (Check decCert v (encE (boxTy (codeTerm (encE A))))) Bool.true) (And (@Eq.{1} Bool (lblOk v) Bool.true) (LE.le Nat instLENat (cnodes v) kk))))
```

`d3_gap_F7`:

```lean
∀ (A : Exp), ∀ (w : Code), ∀ (hw : @Eq.{1} Bool (Check decCert w (encE (boxTy (codeTerm (encE A))))) Bool.true), ∀ (M : Nat), ∀ (hM : ∀ (v : Code), ∀ (f : @Eq.{1} Bool (Check decCert v (encE A)) Bool.true), LE.le Nat instLENat M (cnodes v)), LT.lt Nat instLTNat (Nat.add M M) (cnodes w)
```

**Verdict: exact at F7 for the corrected pair of minima.** `cor46_dprime_mu_F7` gives the `μ_L(□A)` threshold with label-safe certificates of `A` and `□A`; `d3_gap_mu_F7` gives the gap against `2μ(A)` for the least accepted size. A bridge proving label-safe certifiability of `□A` solely from that of `A` is not among these theorems.

**Extra-premise satisfiability:** `check_spec decCert`, `enc46_canon`, `check_toksize decCert` discharge checker/encoding/size assumptions. `check_complete_lbl` supplies witnesses for certifiable A.

**Suite coverage:** `cor46_dprime_iff_F7` → `lcert.formal-test.thm46f7/f4-thm46f7-theorems`; `cor46_dprime_mu_F7` and `d3_gap_mu_F7` → `lcert.formal-test.thm46f7/f4-minimum-certificate`.

### Proposition 4.7

**Formal constants:** `prop47` (`code/lcert/formal/lcert/formal/section4.clj:104`).

**Kernel-held elaborated type:**

`prop47`:

```lean
∀ (chkf : Code → Code → Bool), Rt chkf (List.nil Exp) (List.nil U) p47term (Exp.tPi U.u1 ConP Hcirc)
```

**Verdict: exact.** ConP-to-Hcirc term at budget zero.

**Extra-premise satisfiability:** No extra premise; the term is closed at budget zero for any checker.

**Suite coverage:** `prop47` → `lcert.formal-test.section4/f4-section4`.

### Proposition 4.8

**Formal constants:** `prop48` (`code/lcert/formal/lcert/formal/p5.clj:141`), `prop48_lolli` (`code/lcert/formal/lcert/formal/p5.clj:141`).

**Kernel-held elaborated type:**

`prop48`:

```lean
∀ (conPA : PF), ∀ (conL : PF), ∀ (EPAw : PF → Prop), ∀ (s1 : ∀ (d : Not (PPrv (PF.peq PT.pz (PT.ps PT.pz)))), Not (PPrv conPA)), ∀ (s3 : ∀ (σ : PF), ∀ (f : EPAw σ), PPrv σ), ∀ (i61 : PPrv (PF.pimp conL conPA)), ∀ (i64 : ∀ (n : Nat), ∀ (t : Exp), ∀ (i : Rt (Check decCert) (thetaD n) (thetaU n) t ConOmega), EPAw conL), ∀ (aL : ∀ (ρ : Nat → Nat), Iff (paHolds ρ conL) (∀ (c : Code), @Eq.{1} Bool (Check decCert c (encE Exp.tEmpty)) Bool.false)), (∀ (ρ : Nat → Nat), Iff (paHolds ρ conPA) (Not (PPrv (PF.peq PT.pz (PT.ps PT.pz))))) → ∀ (n : Nat), ∀ (t : Exp), Not (Rt (Check decCert) (thetaD n) (thetaU n) t (Exp.tPi U.uw Hcirc ConOmega))
```

`prop48_lolli`:

```lean
∀ (conPA : PF), ∀ (conL : PF), ∀ (EPAw : PF → Prop), ∀ (s1 : ∀ (d : Not (PPrv (PF.peq PT.pz (PT.ps PT.pz)))), Not (PPrv conPA)), ∀ (s3 : ∀ (σ : PF), ∀ (f : EPAw σ), PPrv σ), ∀ (i61 : PPrv (PF.pimp conL conPA)), ∀ (i64 : ∀ (n : Nat), ∀ (t : Exp), ∀ (i : Rt (Check decCert) (thetaD n) (thetaU n) t ConOmega), EPAw conL), ∀ (aL : ∀ (ρ : Nat → Nat), Iff (paHolds ρ conL) (∀ (c : Code), @Eq.{1} Bool (Check decCert c (encE Exp.tEmpty)) Bool.false)), (∀ (ρ : Nat → Nat), Iff (paHolds ρ conPA) (Not (PPrv (PF.peq PT.pz (PT.ps PT.pz))))) → ∀ (n : Nat), ∀ (t : Exp), Not (Rt (Check decCert) (thetaD n) (thetaU n) t (Exp.tPi U.u1 Hcirc ConOmega))
```

**Verdict: weaker.** Depends on named phase-2 S1, S3, Int-6.1, Int-6.4 hypotheses and anchors through Proposition 5.

**Extra-premise satisfiability:** The S1/S3/Int-6.1/Int-6.4 and anchor premises are not proved in this formalization; they are phase-2 findings, not satisfiability theorems.

**Suite coverage:** `prop48` → `lcert.formal-test.p5/f6-prop5`; `prop48_lolli` → `lcert.formal-test.p5/f6-prop5`.

### Proposition 4.9

**Formal constants:** `prop49_concrete` (`code/lcert/formal/lcert/formal/p49all.clj:557`).

**Kernel-held elaborated type:**

`prop49_concrete`:

```lean
∀ (k : Nat), Rt (Check decCert) (List.nil Exp) (List.nil U) (t49 k) (P49 k)
```

**Verdict: exact.** For every depth k, a closed budget-zero term of P49 k at Check decCert.

**Extra-premise satisfiability:** `prop49_concrete` specializes `prop49` using `check_spec decCert`; no remaining premise.

**Suite coverage:** `prop49_concrete` → `lcert.formal-test.p49all/all-depth-proposition-49`.

### Proposition 4.10

**Formal constants:** `prop410` (`code/lcert/formal/lcert/formal/section4b.clj:1957`), `id00_closed` and `prop410_witness` (`code/lcert/formal/lcert/formal/prop410witness.clj`).

**Kernel-held elaborated type:**

`prop410`:

```lean
∀ (chkf : Code → Code → Bool), ∀ (dec : Code → Option (Prod Nat (Prod Exp Exp))), ∀ (encTy : Exp → Code), ∀ (hspec : CheckSpec chkf dec encTy), ∀ (v : Code), ∀ (hok : @Eq.{1} Bool (lblOk v) Bool.true), ∀ (hck : @Eq.{1} Bool (chkf v (encTy (Exp.tPi U.u1 Exp.tEmpty Exp.tEmpty))) Bool.true), Rt chkf (thetaD (cnodes v)) (thetaU (cnodes v)) (Exp.lam U.u1 Exp.tR (Exp.lam U.u1 (chkT (Exp.var 0) cbot) (Exp.h1 (Exp.var 1) (litAt v 2) cbot (Exp.var 0) Exp.star))) Hcirc
```

`id00_closed` is the closed runtime derivation of `λ(x :₁ 0). x :¹ 0 ⊸ 0` at `Check decCert`. `prop410_witness` has the readable kernel type:

```lean
∃ (v : Code), lblOk v = true ∧ Check decCert v (encE id00Type) = true ∧
  Rt (Check decCert) (thetaD (cnodes v)) (thetaU (cnodes v))
    (Exp.lam U.u1 Exp.tR
      (Exp.lam U.u1 (chkT (Exp.var 0) cbot)
        (Exp.h1 (Exp.var 1) (litAt v 2) cbot (Exp.var 0) Exp.star))) Hcirc
```

**Verdict: exact at the concrete checker.** `id00_closed` proves the paper's named closed identity derivation. Kernel-checked derivation trees for it and its type formation feed `check_complete_lbl`, which supplies an accepted label-safe certificate. `prop410_witness` applies `prop410` at the fixed budget `K = cnodes v` for that certificate.

**Extra-premise satisfiability:** `check_spec decCert` discharges `CheckSpec`; `check_complete_lbl` supplies the certificate from the closed identity and formation trees. No remaining premise.

**Suite coverage:** `prop410` → `lcert.formal-test.section4b/f4-section4b`; `id00_closed`, `prop410_witness` → `lcert.formal-test.prop410witness/a17-proposition-410-witness` (1 test, 3 assertions, 0 failures, 0 errors).

### Proposition 4.10′

**Formal constants:** `prop410_prime` (`code/lcert/formal/lcert/formal/prop410.clj:405`).

**Kernel-held elaborated type:**

`prop410_prime`:

```lean
∀ (chkf : Code → Code → Bool), ∀ (dec : Code → Option (Prod Nat (Prod Exp Exp))), ∀ (encTy : Exp → Code), ∀ (hcs : CheckSpec chkf dec encTy), ∀ (k : Nat), ∀ (t : Exp), ∀ (hnh : @Eq.{1} Bool (noH1 t) Bool.true), Rt chkf (thetaD k) (thetaU k) t H1circ → False
```

**Verdict: exact.** `prop410_prime` states nonderivability of `H₁°` at every token budget for terms without `H₁`, under the paper's checker hypotheses. Its proof uses a modified checker that accepts all sufficiently large codes, whereas the paper's proof sketch changes two pairs. That proof choice does not change the stored theorem statement.

**Extra-premise satisfiability:** `check_spec decCert` discharges the generic checker premise for the concrete checker. The finite agreement is constructed inside the proof, not assumed by the theorem. `noH1 t = true` is the paper's restriction on the language, not an added premise.

**Suite coverage:** `prop410_prime` → `lcert.formal-test.prop410/f4-prop410-prime`.

### Proposition 4.11

**Formal constants:** `prop411` (`code/lcert/formal/lcert/formal/section4c.clj:461`), `prop411_nodes` (`code/lcert/formal/lcert/formal/section4c.clj:455`), `prop411_redex_typed`, `prop411_redex_cert`, `cert_term_size`, and `prop411_literal_lower` (`code/lcert/formal/lcert/formal/prop411cert.clj`); `sh4_cn_tree_check`, `prop411_family_cert`, `sh4_cn_cert`, `prop411_explicit_cert`, and seven generated `sh4_encDT_*_nodes` equations (`code/lcert/formal/lcert/formal/prop411poly.clj`).

**Kernel-held elaborated type:**

`prop411`:

```lean
∀ (chkf : Code → Code → Bool), ∀ (dec : Code → Option (Prod Nat (Prod Exp Exp))), ∀ (encTy : Exp → Code), ∀ (cap : Nat), ∀ (n : Nat), And (Rt chkf (List.nil Exp) (List.nil U) (sh4_cn n) Exp.tSyn) (@Eq.{1} Nat (cnodes (den chkf dec encTy cap (sh4_cn n) (List.nil Sk) Sk.syn Unit.unit)) (Nat.sub (sh4_pow n) 1))
```

`prop411_nodes`:

```lean
∀ (n : Nat), @Eq.{1} Nat (cnodes (sh4_bush n)) (Nat.sub (sh4_pow n) 1)
```

**Verdict: exact under F7 (A10 reopened, 2026-10-15).** `prop411_explicit_cert` accepts the explicit beta-redex certificate `sh4_cn_cert n` at every `n`; `prop411_cert_size` gives its exact size `16n² + 97n + 73`, and `prop411_cert_quadratic` the quadratic bound; `prop411_literal_lower` bounds every certificate of the normal literal below by `2ⁿ − 1`, and `prop411_exponential_gap` makes the paper's comparison exact from `n = 12`. R4's table now shows kernel-evaluated F7 counts (`prop411_cert_*`, `prop411_lit_cert_*`).

**Extra-premise satisfiability:** No extra premise for prop411. `check_complete` can turn a concrete typed derivation into an accepted certificate, but no indexed theorem connects it to the omega-let chain or proves the certificate-size comparison.

**Suite coverage:** `prop411` and `prop411_nodes` → `lcert.formal-test.section4c/f4-section4c`; `prop411_redex_cert` → `lcert.formal-test.section4c/a10-redex-certificate`; `prop411_literal_lower` → `lcert.formal-test.section4c/a10-normal-literal-size`; `prop411_family_cert` → `lcert.formal-test.section4c/a10-certificate-family`; `prop411_explicit_cert` → `lcert.formal-test.section4c/a10-explicit-certificate`.

### Proposition 4.12(1)

**Formal constants:** `prop412` (`code/lcert/formal/lcert/formal/section4c.clj:826`), `prop412_nodes` (`code/lcert/formal/lcert/formal/section4c.clj:819`).

**Kernel-held elaborated type:**

`prop412`:

```lean
∀ (chkf : Code → Code → Bool), ∀ (dec : Code → Option (Prod Nat (Prod Exp Exp))), ∀ (encTy : Exp → Code), ∀ (cap : Nat), ∀ (n : Nat), And (Rt chkf (List.nil Exp) (List.nil U) (sh4_doubler (sh4_num n)) sh4_M) (@Eq.{1} Nat (cnodes (Prod.fst (@Sk.rec.{2} (fun (_ : Sk) => Type) Unit Bool Nat Nat Code Unit Code (fun (x : Sk) => fun (y : Sk) => fun (cx : Type) => fun (cy : Type) => ∀ (j : cx), cy) (fun (x : Sk) => fun (y : Sk) => fun (cx : Type) => fun (cy : Type) => Prod cx cy) Sk.syn) (@Sk.rec.{2} (fun (_ : Sk) => Type) Unit Bool Nat Nat Code Unit Code (fun (x : Sk) => fun (y : Sk) => fun (cx : Type) => fun (cy : Type) => ∀ (j : cx), cy) (fun (x : Sk) => fun (y : Sk) => fun (cx : Type) => fun (cy : Type) => Prod cx cy) Sk.unit) (den chkf dec encTy cap (sh4_doubler (sh4_num n)) (List.nil Sk) (Sk.prod Sk.syn Sk.unit) Unit.unit))) (Nat.sub (sh4_pow n) 1))
```

`prop412_nodes`:

```lean
∀ (chkf : Code → Code → Bool), ∀ (dec : Code → Option (Prod Nat (Prod Exp Exp))), ∀ (encTy : Exp → Code), ∀ (cap : Nat), ∀ (n : Nat), @Eq.{1} Nat (cnodes (Prod.fst (@Sk.rec.{2} (fun (_ : Sk) => Type) Unit Bool Nat Nat Code Unit Code (fun (x : Sk) => fun (y : Sk) => fun (cx : Type) => fun (cy : Type) => ∀ (j : cx), cy) (fun (x : Sk) => fun (y : Sk) => fun (cx : Type) => fun (cy : Type) => Prod cx cy) Sk.syn) (@Sk.rec.{2} (fun (_ : Sk) => Type) Unit Bool Nat Nat Code Unit Code (fun (x : Sk) => fun (y : Sk) => fun (cx : Type) => fun (cy : Type) => ∀ (j : cx), cy) (fun (x : Sk) => fun (y : Sk) => fun (cx : Type) => fun (cy : Type) => Prod cx cy) Sk.unit) (den chkf dec encTy cap (sh4_doubler (sh4_num n)) (List.nil Sk) (Sk.prod Sk.syn Sk.unit) Unit.unit))) (Nat.sub (sh4_pow n) 1)
```

**Verdict: exact under F7 (A06 reopened, 2026-10-15).** Budget-0 typing, the `2ⁿ − 1` denotation and an accepted F7 certificate at every `n` were checked before; `prop412_cert_size` now gives its exact size (`2|c| = n² + 13n + 732`), the polynomial statement, and `prop412_cert_4`/`_8`/`_12` the kernel-evaluated counts R4 shows.

**Extra-premise satisfiability:** No extra semantic premise for `prop412`; the polynomial certificate-growth assertion has no kernel statement or satisfiability theorem.

**Suite coverage:** `prop412` and `prop412_nodes` → `lcert.formal-test.section4c/f4-section4c`; `sh4_num_enc_linear` → `lcert.formal-test.section4c/a06-numeral-encoding-size`; `prop412_explicit_cert` → `lcert.formal-test.section4c/a06-certificate-family`; `sh4_num_tree_bound` → `lcert.formal-test.section4c/a06-numeral-tree-size`.

### Proposition 4.12(2)

**Formal constants:** `theorem3` (`code/lcert/formal/lcert/formal/lemma36.clj:205`), `theorem4e_nodes` (`code/lcert/formal/lcert/formal/theorem4e.clj:1145`), `prop41_closed` (`code/lcert/formal/lcert/formal/section4.clj:36`).

**Kernel-held elaborated type:**

`theorem3`:

```lean
∀ (chkf : Code → Code → Bool), ∀ (dec : Code → Option (Prod Nat (Prod Exp Exp))), ∀ (encTy : Exp → Code), ∀ (hcs : CheckSpec chkf dec encTy), ∀ (hconv : ∀ (n : Nat), ConvCase chkf dec encTy n), ∀ (n : Nat), ∀ (D : List Exp), ∀ (us : List U), ∀ (t : Exp), ∀ (der : Rt chkf D us t Exp.tR), ∀ (hw : WFCtx chkf D), ∀ (en : HEnv (skels D)), ∀ (k : Nat), ∀ (hk : Nat.le k n), ∀ (hs : EnvSat chkf dec encTy n D us en k), LE.le Nat instLENat (cnodes (den chkf dec encTy n t (skels D) Sk.cert en)) k
```

`theorem4e_nodes`:

```lean
∀ (chkf : Code → Code → Bool), ∀ (dec : Code → Option (Prod Nat (Prod Exp Exp))), ∀ (encTy : Exp → Code), ∀ (hcs : CheckSpec chkf dec encTy), ∀ (n : Nat), ∀ (t : Exp), ∀ (hd : Rt chkf (thetaD n) (thetaU n) t Exp.tR), ∀ (e : Exp), ∀ (i : Er chkf (thetaD n) (thetaU n) t Exp.tR e), @Exists.{1} Code (fun (c : Code) => And (EvalE chkf dec encTy n (rtokens n) e (RV.cert c)) (LE.le Nat instLENat (cnodes c) n))
```

`prop41_closed`:

```lean
∀ (chkf : Code → Code → Bool), ∀ (dec : Code → Option (Prod Nat (Prod Exp Exp))), ∀ (encTy : Exp → Code), ∀ (hcs : CheckSpec chkf dec encTy), ∀ (hconv : ∀ (n : Nat), ConvCase chkf dec encTy n), ∀ (n : Nat), ∀ (t : Exp), ∀ (der : Rt chkf (List.nil Exp) (List.nil U) t Exp.tR), @Exists.{1} Nat (fun (l : Nat) => @Eq.{1} Code (den chkf dec encTy n t (List.nil Sk) Sk.cert Unit.unit) (Code.sl l))
```

**Verdict: exact.** For results of type R, T3 bounds the denoted certificate by k; Theorem 4-prime bounds the erased runtime R value by k, and Proposition 4.1 covers closed budget-zero terms.

**Extra-premise satisfiability:** `check_spec decCert`, `conv_all`, and `wf_theta` discharge the model assumptions; `theorem4e_nodes` is for an Er witness, supplied by `er_total`.

**Suite coverage:** `theorem3` → `lcert.formal-test.lemma36/f3n-lemma36-and-main-results`; `theorem4e_nodes` → `lcert.formal-test.theorem4e/f5-theorem4e`; `prop41_closed` → `lcert.formal-test.section4/f4-section4`.

### Theorem 4 / T4

**Formal constants:** `theorem4` (`code/lcert/formal/lcert/formal/theorem4.clj:1781`).

**Kernel-held elaborated type:**

`theorem4`:

```lean
∀ (chkf : Code → Code → Bool), ∀ (dec : Code → Option (Prod Nat (Prod Exp Exp))), ∀ (encTy : Exp → Code), ∀ (hcs : CheckSpec chkf dec encTy), ∀ (n : Nat), Adeq chkf dec encTy n
```

**Verdict: exact at `Check` (fidelity pass, 2026-10-15).** `theorem4_F7` (concrete.clj) gives adequacy at every budget with no checker premise. `argsOK` excludes branch-list pseudo-terms, which the paper's syntax does not have (see Lemma 3.2); `rt_argsOK` derives it from any runtime derivation.

**Extra-premise satisfiability:** `rt_argsOK` derives `argsOK` from a runtime typing derivation, and `skOf_complete` supplies denotations for skeleton-typed arguments; arbitrary branch-list pseudo-terms are excluded. The budgeted evaluator's CheckSpec assumption is met by `check_spec decCert`.

**Suite coverage:** `theorem4` → `lcert.formal-test.theorem4/f5-skof-agree`.

### Corollary 5.1

**Formal constants:** `corollary51_conv` (`code/lcert/formal/lcert/formal/theorem4.clj:1839`), `corollary51_nodes` (`code/lcert/formal/lcert/formal/theorem4.clj:1862`).

**Kernel-held elaborated type:**

`corollary51_conv`:

```lean
∀ (chkf : Code → Code → Bool), ∀ (dec : Code → Option (Prod Nat (Prod Exp Exp))), ∀ (encTy : Exp → Code), ∀ (hcs : CheckSpec chkf dec encTy), ∀ (n : Nat), ∀ (t : Exp), ∀ (D : Exp), ∀ (hd : Rt chkf (thetaD n) (thetaU n) t D), ∀ (hb : @Eq.{1} Bool (isBaseTy D) Bool.true), @Exists.{1} RV (fun (v : RV) => And (Eval chkf dec encTy n (rtokens n) t v) (And (rel chkf dec encTy n (skel D) v (den chkf dec encTy n t (thetaSk n) (skel D) (tokenEnv n))) (And (V chkf dec encTy n D (skels (thetaD n)) (tokEnvD n) n (skel D) (den chkf dec encTy n t (skels (thetaD n)) (skel D) (tokEnvD n))) (@Eq.{1} (Car (skel D)) (den chkf dec encTy n t (thetaSk n) (skel D) (tokenEnv n)) (den chkf dec encTy n t (skels (thetaD n)) (skel D) (tokEnvD n))))))
```

`corollary51_nodes`:

```lean
∀ (chkf : Code → Code → Bool), ∀ (dec : Code → Option (Prod Nat (Prod Exp Exp))), ∀ (encTy : Exp → Code), ∀ (hcs : CheckSpec chkf dec encTy), ∀ (n : Nat), ∀ (t : Exp), ∀ (hd : Rt chkf (thetaD n) (thetaU n) t Exp.tR), LE.le Nat instLENat (cnodes (den chkf dec encTy n t (skels (thetaD n)) Sk.cert (tokEnvD n))) n
```

**Verdict: exact at `Check` for the node bound (fidelity pass, 2026-10-15).** `corollary51_nodes_F7` (concrete.clj) bounds a typed `R` program's value by its budget, with no checker premise. `corollary51_conv` states the canonical-result clause through the value relation `rel`, the formal reading of "computes the model's value".

**Extra-premise satisfiability:** `check_spec decCert`, `conv_all`, and `wf_theta` discharge the typed root hypotheses; base-data restriction is the paper's condition.

**Suite coverage:** `corollary51_conv` → `lcert.formal-test.theorem4/f5-skof-agree`; `corollary51_nodes` → `lcert.formal-test.theorem4/f5-skof-agree`.

### Theorem 4′

**Formal constants:** `theorem4e` (`code/lcert/formal/lcert/formal/theorem4e.clj:1074`), `theorem4e_agree` (`code/lcert/formal/lcert/formal/theorem4e.clj:1120`), `theorem4e_nodes` (`code/lcert/formal/lcert/formal/theorem4e.clj:1145`).

**Kernel-held elaborated type:**

`theorem4e`:

```lean
∀ (chkf : Code → Code → Bool), ∀ (dec : Code → Option (Prod Nat (Prod Exp Exp))), ∀ (encTy : Exp → Code), ∀ (hcs : CheckSpec chkf dec encTy), ∀ (n : Nat), ∀ (t : Exp), ∀ (X : Exp), ∀ (hd : Rt chkf (thetaD n) (thetaU n) t X), ∀ (hb : @Eq.{1} Bool (isBaseTy X) Bool.true), ∀ (e : Exp), ∀ (k : Er chkf (thetaD n) (thetaU n) t X e), @Exists.{1} RV (fun (v : RV) => And (EvalE chkf dec encTy n (rtokens n) e v) (And (Eval chkf dec encTy n (rtokens n) t v) (rel chkf dec encTy n (skel X) v (den chkf dec encTy n t (thetaSk n) (skel X) (tokenEnv n)))))
```

`theorem4e_agree`:

```lean
∀ (chkf : Code → Code → Bool), ∀ (dec : Code → Option (Prod Nat (Prod Exp Exp))), ∀ (encTy : Exp → Code), ∀ (hcs : CheckSpec chkf dec encTy), ∀ (n : Nat), ∀ (t : Exp), ∀ (X : Exp), ∀ (hd : Rt chkf (thetaD n) (thetaU n) t X), ∀ (hb : @Eq.{1} Bool (isBaseTy X) Bool.true), @Exists.{1} Exp (fun (e : Exp) => And (Er chkf (thetaD n) (thetaU n) t X e) (@Exists.{1} RV (fun (v : RV) => And (EvalE chkf dec encTy n (rtokens n) e v) (Eval chkf dec encTy n (rtokens n) t v))))
```

`theorem4e_nodes`:

```lean
∀ (chkf : Code → Code → Bool), ∀ (dec : Code → Option (Prod Nat (Prod Exp Exp))), ∀ (encTy : Exp → Code), ∀ (hcs : CheckSpec chkf dec encTy), ∀ (n : Nat), ∀ (t : Exp), ∀ (hd : Rt chkf (thetaD n) (thetaU n) t Exp.tR), ∀ (e : Exp), ∀ (i : Er chkf (thetaD n) (thetaU n) t Exp.tR e), @Exists.{1} Code (fun (c : Code) => And (EvalE chkf dec encTy n (rtokens n) e (RV.cert c)) (LE.le Nat instLENat (cnodes c) n))
```

**Verdict: exact at `Check` (fidelity pass, 2026-10-15).** `theorem4e_nodes_F7` and `theorem4e_agree_F7` (concrete.clj) discharge `CheckSpec`. The paper's erasure is a function; the formal `Er` is a relation that is total (`er_total`), and `theorem4e` holds for every erasure, so the statements agree.

**Extra-premise satisfiability:** `er_total` supplies an erasure witness; `check_spec decCert` discharges checker properties. The base-data restriction is in the paper.

**Suite coverage:** `theorem4e` → `lcert.formal-test.theorem4e/f5-theorem4e`; `theorem4e_agree` → `lcert.formal-test.theorem4e/f5-theorem4e`; `theorem4e_nodes` → `lcert.formal-test.theorem4e/f5-theorem4e`.

### Theorem 5.2

**Formal constants:** `theorem52_all_concrete` (`code/lcert/formal/lcert/formal/s52univ.clj:117`), `theorem52e_all` (`code/lcert/formal/lcert/formal/s52univ.clj:104`).

**Kernel-held elaborated type:**

`theorem52_all_concrete`:

```lean
∀ (n : Nat), ∀ (t : Exp), ∀ (X : Exp), ∀ (hd : Rt (Check decCert) (thetaD n) (thetaU n) t X), And (∀ (v : RV), ∀ (f : Eval (Check decCert) (decOf decCert) encE n (rtokens n) t v), Ok (Check decCert) (decOf decCert) encE n (EvSrc.tm (rtokens n) t) v) (∀ (e : Exp), ∀ (f : Er (Check decCert) (thetaD n) (thetaU n) t X e), ∀ (v : RV), ∀ (h : EvalE (Check decCert) (decOf decCert) encE n (rtokens n) e v), OkE (Check decCert) (decOf decCert) encE n (EvSrc.tm (rtokens n) e) v)
```

`theorem52e_all`:

```lean
∀ (chkf : Code → Code → Bool), ∀ (dec : Code → Option (Prod Nat (Prod Exp Exp))), ∀ (encTy : Exp → Code), ∀ (hcs : CheckSpec chkf dec encTy), ∀ (n : Nat), ∀ (t : Exp), ∀ (X : Exp), ∀ (hd : Rt chkf (thetaD n) (thetaU n) t X), ∀ (e : Exp), ∀ (j : Er chkf (thetaD n) (thetaU n) t X e), ∀ (v : RV), ∀ (l : EvalE chkf dec encTy n (rtokens n) e v), OkE chkf dec encTy n (EvSrc.tm (rtokens n) e) v
```

**Verdict: exact (fidelity pass, 2026-10-15).** R4's Theorem 5.2 now states the general form, for every result type, which `theorem52_all_concrete` proves; the base-data case is the case of interest.

**Extra-premise satisfiability:** `check_spec decCert` is discharged by `theorem52_all_concrete`; `er_total` supplies erasure witnesses. No extra premise remains at the concrete checker.

**Suite coverage:** `theorem52_all_concrete` → `lcert.formal-test.s52univ/f5-s52univ`; `theorem52e_all` → `lcert.formal-test.s52univ/f5-s52univ`.

### Proposition 5 / P5

**Formal constants:** `prop5` (`code/lcert/formal/lcert/formal/p5.clj:133`).

**Kernel-held elaborated type:**

`prop5`:

```lean
∀ (conPA : PF), ∀ (conL : PF), ∀ (EPAw : PF → Prop), ∀ (s1 : ∀ (d : Not (PPrv (PF.peq PT.pz (PT.ps PT.pz)))), Not (PPrv conPA)), ∀ (s3 : ∀ (σ : PF), ∀ (f : EPAw σ), PPrv σ), ∀ (i61 : PPrv (PF.pimp conL conPA)), ∀ (i64 : ∀ (n : Nat), ∀ (t : Exp), ∀ (i : Rt (Check decCert) (thetaD n) (thetaU n) t ConOmega), EPAw conL), ∀ (aL : ∀ (ρ : Nat → Nat), Iff (paHolds ρ conL) (∀ (c : Code), @Eq.{1} Bool (Check decCert c (encE Exp.tEmpty)) Bool.false)), (∀ (ρ : Nat → Nat), Iff (paHolds ρ conPA) (Not (PPrv (PF.peq PT.pz (PT.ps PT.pz))))) → ∀ (n : Nat), ∀ (t : Exp), Not (Rt (Check decCert) (thetaD n) (thetaU n) t ConOmega)
```

**Verdict: weaker.** Conditional on S1, S3, Int-6.1, Int-6.4 and representation anchors; no unconditional internal second-incompleteness claim.

**Extra-premise satisfiability:** S1, S3, Int-6.1, Int-6.4 and the two anchors lack kernel-checked discharge; `P5-hypotheses.md` gives only informal evidence. Phase 2.

**Suite coverage:** `prop5` → `lcert.formal-test.p5/f6-prop5`.

### Proposition 6.1

**Formal constants:** `pa_lcert` (`code/lcert/formal/lcert/formal/palcert.clj:38`), `pa_refutation` (`code/lcert/formal/lcert/formal/palcert.clj:46`), `pa_lcert_closed` (`code/lcert/formal/lcert/formal/paclosure.clj:478`), `paClose_type_agree` (`code/lcert/formal/lcert/formal/paclosure.clj:492`), `paClose_zero` (`code/lcert/formal/lcert/formal/paclosure.clj:505`).

**Kernel-held elaborated type:**

`pa_lcert`:

```lean
∀ (chkf : Code → Code → Bool), ∀ (p : PF), ∀ (hp : PPrv p), ∀ (D : List Exp), ∀ (ρ : Nat → Nat), ∀ (f : Good D ρ), @Exists.{1} Exp (fun (e : Exp) => Rt chkf D (allW D) e (trF p ρ))
```

`pa_refutation`:

```lean
∀ (chkf : Code → Code → Bool), ∀ (hp : PPrv (PF.peq PT.pz (PT.ps PT.pz))), @Exists.{1} Exp (fun (t : Exp) => Rt chkf (thetaD 0) (thetaU 0) t Exp.tEmpty)
```

**Verdict: exact for the external closed-line form (A05, 2026-10-15); internal part phase 2.** `pa_lcert_closed` gives every H_PA proof of φ a closed budget-0 derivation of `paCloseF φ`, the paper's ⟦∀ᶜˡφ⟧: the translated formula under one ω-Nat binder for each de Bruijn index below its support bound. R4 §6.2 now allows the closure to bind variables that do not occur in φ, so this support-bound closure and the implementation's exact-free-variable closure are both instances of its definition. `paClose_type_agree` identifies the body of `paCloseF φ` with the open translation `pa_lcert` uses at the canonical finite environment, and `pa_finite_lcert_pos` obtains the closed line from that open derivation. The converse, from a closed line back to an open derivation in an arbitrary `Good` context, is not stated in the paper and is not formalized. The paper's primitive-recursive proof-code `tr` and PA-internal CHECK correctness are phase 2 (Int-6.1).

**Extra-premise satisfiability:** `Good D ρ` is an additional context premise; `paRefute_good` supplies it for the fixed refutation translation, and `paGood_up` extends a `Good` context under a binder. There is no `Good` witness in the empty context for an arbitrary total assignment; `pa_lcert_closed` needs none, because `paEnv_good` supplies one in the finite Nat context `paNatD s` that its closure removes. The external theorem does not supply Int-6.1's PA-internal theorem.

**Suite coverage:** `pa_lcert` → `lcert.formal-test.palcert/all-pa-proofs-translate`; `pa_refutation` → `lcert.formal-test.palcert/all-pa-proofs-translate`. `pa_lcert_closed` → `lcert.formal-test.palcert/a05-all-proofs-close`; `paClose_type_agree`, `paClose_zero` → `lcert.formal-test.palcert/a05-translation-agreement`.

### Lemma 6.2

**Formal constants and kernel type:** none.

**Verdict: open (phase 2).** E-PA^omega-internal monotonicity, substitution and conversion for Delta are phase 2; meta-level analogues do not state E-PA^omega provability.

**Extra-premise satisfiability:** No formal E-PA^omega statement; S2 and E-PA^omega syntax/proof predicate are phase-2 prerequisites.

**Suite coverage:** none (no counterpart).

### Lemma 6.3

**Formal constants and kernel type:** none.

**Verdict: open (phase 2).** PA proofs of BF0 and BF1 for every numeral bound are phase 2; meta-level Corollary 3.7 is not their internalization.

**Extra-premise satisfiability:** No formal PA proof family of BF0/BF1; S2 and finite-support enumeration are phase-2 prerequisites.

**Suite coverage:** none (no counterpart).

### Lemma 6.4

**Formal constants:** `lemma64_theta` (`code/lcert/formal/lcert/formal/dflt64.clj:239`), `lemma64_concrete` (`code/lcert/formal/lcert/formal/dflt64.clj:300`).

**Kernel-held elaborated type:**

`lemma64_theta`:

```lean
∀ (chkf : Code → Code → Bool), ∀ (dec : Code → Option (Prod Nat (Prod Exp Exp))), ∀ (encTy : Exp → Code), ∀ (hcs : CheckSpec chkf dec encTy), ∀ (n : Nat), ∀ (t : Exp), ∀ (A : Exp), ∀ (der : Rt chkf (thetaD n) (thetaU n) t A), V_ri chkf dec encTy riDflt n A (skels (thetaD n)) (tokEnvD n) n (skel A) (den_ri chkf dec encTy riDflt n t (skels (thetaD n)) (skel A) (tokEnvD n))
```

`lemma64_concrete`:

```lean
∀ (n : Nat), ∀ (t : Exp), ∀ (A : Exp), ∀ (der : Rt (Check decCert) (thetaD n) (thetaU n) t A), V_ri (Check decCert) (decOf decCert) encE riDflt n A (skels (thetaD n)) (tokEnvD n) n (skel A) (den_ri (Check decCert) (decOf decCert) encE riDflt n t (skels (thetaD n)) (skel A) (tokEnvD n))
```

**Verdict: different.** Fixed-budget default-model soundness is proved externally; the paper asks for each Delta node to be proved sound inside E-PA^omega, phase 2 Int-6.4.

**Extra-premise satisfiability:** `check_spec decCert` supplies the external model's checker premise; no theorem discharges the E-PA^omega internalization (Int-6.4).

**Suite coverage:** `lemma64_theta` → `lcert.formal-test.dflt64/f6-4-default-model`; `lemma64_concrete` → `lcert.formal-test.dflt64/f6-4-default-model`.

### Corollary 6.5

**Formal constants:** `cor65_concrete` (`code/lcert/formal/lcert/formal/dflt64.clj:294`).

**Kernel-held elaborated type:**

`cor65_concrete`:

```lean
∀ (n : Nat), ∀ (t : Exp), Rt (Check decCert) (thetaD n) (thetaU n) t ConOmega → ∀ (c : Code), ∀ (hc : @Eq.{1} Bool (lblOk c) Bool.true), @Eq.{1} Bool (Check decCert c (encE Exp.tEmpty)) Bool.false
```

**Verdict: different.** External default-model consequence is proved; E-PA^omega derivability of Con_lambda from Delta remains phase 2.

**Extra-premise satisfiability:** `cor65_concrete` is external and premise-free at Check decCert; no theorem discharges E-PA^omega provability from Delta (Int-6.4).

**Suite coverage:** `cor65_concrete` → `lcert.formal-test.dflt64/f6-4-default-model`.

## §0 Results and unnumbered claims

| Claim | Counterpart and finding |
| --- | --- |
| T1 consistency | Theorem 1 above; relative metatheorem, with `consistent_concrete` at the concrete checker. |
| T2 self-justification | Theorem 2 above: closed object-level inhabitants of `H°` and `H₁°`. Against Willard2016 Definition 3.4: `willard_def34` proves both clauses at `Check` (clause (ii) also as "Check accepts no certificate of `0`"), and `Hcirc_reads` gives `H°`'s type literally; that `H°` states consistency is a reading of that type, as R4 §3.9 now says (A14). |
| T3 size soundness | Theorem 3 above, including the distinct-free-token refinement. |
| T4 termination/adequacy and safe evaluation | Theorem 4, Theorem 4′, Corollary 5.1 and universal Theorem 5.2 above. |
| P5 code-form second incompleteness | Proposition 5 above is conditional on named arithmetic internalizations and representation anchors; phase 2 must discharge them. |
| D1, D2, D3, boxed contraction | Propositions 4.2, 4.4–4.5, Theorem 4.6, Corollaries 4.6′/4.6″ above. |
| Certificates not normal; codes compress | Propositions 4.11–4.12 above; the numerical certificate sizes are unformalized measurements. |
| `H₁°` yields `H°`; `H` does not give `H₁°` | Propositions 4.10 and 4.10′ above. The latter changes the checker more broadly than the paper. |
| `Con′ → H°`; bounded `Con′` | Propositions 4.7 and 4.9 above. |
| Certified evaluation for base data types | Reflect case of Lemma 3.6, Theorem 4′ and Theorem 5.2. The §4.7 typed parser is outside the formalization (A07; R4 §4.7 records its status). |

## §7 What is mechanized

§7 describes an earlier ADR-0005 state. Its six `lcert.verified` constants live in `code/lcert/ansatz/lcert/verified.clj` and are loaded by the separate `lcert.verified-test`, **not by `bin/test-formal`**. `lcert.verified` calls `a/load-init!` and resets the warm server's environment; the types below were captured after the complete formal environment inventory, in the same JVM. The current formal tree supplies later replacements for the strict-budget result (`budget_canon`) and model descent; the historical numerical `not` certificate checks remain a separate artifact. The §7 claim that the remainder is only tested has become stale: ADR-0006 now kernel-checks the deeper formal counterparts catalogued above. The §7 claim that `lcert.kernel` renders every implementation code as a `CT` tree with matching measures remains a test-level bridge, without a formal theorem relating the two representations (A16).

| §7 constant and source | Kernel-held elaborated type | Verdict and premise satisfiability | `test-formal` coverage |
| --- | --- | --- | --- |
| `ctxlen_le_nodes` (`code/lcert/ansatz/lcert/verified.clj:107`) | <code>∀ (t : CT), LE.le Nat instLENat (ctxlen t) (nodes t)</code> | **Exact** for the §7 code-tree first-child-spine inequality; no extra premise. | No `test-formal` assertion; `lcert.verified-test/the-theorems-are-kernel-checked` is separate. |
| `fstlen_le_nodes` (`code/lcert/ansatz/lcert/verified.clj:119`) | <code>∀ (t : CT), LE.le Nat instLENat (fstlen t) (nodes t)</code> | **Exact** for the §7 first-child version; no extra premise. | No `test-formal` assertion; `lcert.verified-test/the-theorems-are-kernel-checked` is separate. |
| `strict_overhead` (`code/lcert/ansatz/lcert/verified.clj:126`) | <code>∀ (l : Nat), ∀ (j : CT), ∀ (p : CT), LT.lt Nat instLTNat (budget (CT.cell l Bool.true j p)) (nodes (CT.cell l Bool.true j p))</code> | **Exact** for §7's *derivation-shaped CT* claim. It does not say that every accepted `Check` certificate has strict overhead; `budget_canon` and `check_spec decCert` are that later bridge. No extra premise. | No `test-formal` assertion; `lcert.verified-test/the-theorems-are-kernel-checked` is separate. |
| `h1_budget` (`code/lcert/ansatz/lcert/verified.clj:133`) | <code>∀ (m1 : Nat), ∀ (k1 : Nat), ∀ (m2 : Nat), ∀ (k2 : Nat), ∀ (n : Nat), ∀ (h1 : LT.lt Nat instLTNat m1 k1), ∀ (h2 : LT.lt Nat instLTNat m2 k2), ∀ (h3 : LE.le Nat instLENat (Nat.add k1 k2) n), LT.lt Nat instLTNat (Nat.add m1 m2) n</code> | **Exact** for §7's three numerical hypotheses; they are the paper's assumptions, not added ones. | No `test-formal` assertion; `lcert.verified-test/the-theorems-are-kernel-checked` is separate. |
| `not_cert_nodes` (`code/lcert/ansatz/lcert/verified.clj:161`) | <code>@Eq.{1} Nat (nodes (CT.cell 64 Bool.true (CT.cell 8 Bool.true (CT.cell 4 Bool.false CT.tend CT.tend) (CT.cell 10 Bool.true (CT.cell 50 Bool.true (CT.cell 17 Bool.false CT.tend CT.tend) (CT.cell 34 Bool.true (CT.cell 14 Bool.true (CT.cell 2 Bool.false CT.tend CT.tend) (CT.cell 0 Bool.false CT.tend CT.tend)) (CT.cell 13 Bool.true (CT.cell 33 Bool.false CT.tend CT.tend) (CT.cell 32 Bool.false CT.tend CT.tend)))) (CT.cell 26 Bool.true (CT.cell 17 Bool.false CT.tend CT.tend) (CT.cell 17 Bool.false CT.tend CT.tend)))) (CT.cell 1 Bool.true (CT.cell 88 Bool.true (CT.cell 11 Bool.true (CT.cell 4 Bool.false CT.tend CT.tend) (CT.cell 17 Bool.false CT.tend CT.tend)) (CT.cell 0 Bool.false CT.tend CT.tend)) (CT.cell 1 Bool.true (CT.cell 70 Bool.true (CT.cell 8 Bool.true (CT.cell 7 Bool.true (CT.cell 4 Bool.false CT.tend CT.tend) (CT.cell 17 Bool.false CT.tend CT.tend)) (CT.cell 10 Bool.true (CT.cell 34 Bool.true (CT.cell 14 Bool.true (CT.cell 2 Bool.false CT.tend CT.tend) (CT.cell 0 Bool.false CT.tend CT.tend)) (CT.cell 13 Bool.true (CT.cell 33 Bool.false CT.tend CT.tend) (CT.cell 32 Bool.false CT.tend CT.tend))) (CT.cell 17 Bool.false CT.tend CT.tend))) (CT.cell 1 Bool.true (CT.cell 58 Bool.true (CT.cell 8 Bool.true (CT.cell 7 Bool.true (CT.cell 4 Bool.false CT.tend CT.tend) (CT.cell 17 Bool.false CT.tend CT.tend)) (CT.cell 10 Bool.true (CT.cell 14 Bool.true (CT.cell 2 Bool.false CT.tend CT.tend) (CT.cell 0 Bool.false CT.tend CT.tend)) (CT.cell 17 Bool.false CT.tend CT.tend))) (CT.cell 0 Bool.false CT.tend CT.tend)) (CT.cell 1 Bool.true (CT.cell 61 Bool.true (CT.cell 8 Bool.true (CT.cell 7 Bool.true (CT.cell 4 Bool.false CT.tend CT.tend) (CT.cell 17 Bool.false CT.tend CT.tend)) (CT.cell 10 Bool.true (CT.cell 33 Bool.false CT.tend CT.tend) (CT.cell 17 Bool.false CT.tend CT.tend))) (CT.cell 0 Bool.false CT.tend CT.tend)) (CT.cell 1 Bool.true (CT.cell 60 Bool.true (CT.cell 8 Bool.true (CT.cell 7 Bool.true (CT.cell 4 Bool.false CT.tend CT.tend) (CT.cell 17 Bool.false CT.tend CT.tend)) (CT.cell 10 Bool.true (CT.cell 32 Bool.false CT.tend CT.tend) (CT.cell 17 Bool.false CT.tend CT.tend))) (CT.cell 0 Bool.false CT.tend CT.tend)) (CT.cell 0 Bool.false CT.tend CT.tend))))) (CT.cell 0 Bool.false CT.tend CT.tend))))) 35</code> | **Exact** for the particular draft certificate's 35-node calculation; no extra premise. | No `test-formal` assertion; `lcert.verified-test/the-theorems-are-kernel-checked` is separate. |
| `not_cert_budget` (`code/lcert/ansatz/lcert/verified.clj:163`) | <code>@Eq.{1} Nat (budget (CT.cell 64 Bool.true (CT.cell 8 Bool.true (CT.cell 4 Bool.false CT.tend CT.tend) (CT.cell 10 Bool.true (CT.cell 50 Bool.true (CT.cell 17 Bool.false CT.tend CT.tend) (CT.cell 34 Bool.true (CT.cell 14 Bool.true (CT.cell 2 Bool.false CT.tend CT.tend) (CT.cell 0 Bool.false CT.tend CT.tend)) (CT.cell 13 Bool.true (CT.cell 33 Bool.false CT.tend CT.tend) (CT.cell 32 Bool.false CT.tend CT.tend)))) (CT.cell 26 Bool.true (CT.cell 17 Bool.false CT.tend CT.tend) (CT.cell 17 Bool.false CT.tend CT.tend)))) (CT.cell 1 Bool.true (CT.cell 88 Bool.true (CT.cell 11 Bool.true (CT.cell 4 Bool.false CT.tend CT.tend) (CT.cell 17 Bool.false CT.tend CT.tend)) (CT.cell 0 Bool.false CT.tend CT.tend)) (CT.cell 1 Bool.true (CT.cell 70 Bool.true (CT.cell 8 Bool.true (CT.cell 7 Bool.true (CT.cell 4 Bool.false CT.tend CT.tend) (CT.cell 17 Bool.false CT.tend CT.tend)) (CT.cell 10 Bool.true (CT.cell 34 Bool.true (CT.cell 14 Bool.true (CT.cell 2 Bool.false CT.tend CT.tend) (CT.cell 0 Bool.false CT.tend CT.tend)) (CT.cell 13 Bool.true (CT.cell 33 Bool.false CT.tend CT.tend) (CT.cell 32 Bool.false CT.tend CT.tend))) (CT.cell 17 Bool.false CT.tend CT.tend))) (CT.cell 1 Bool.true (CT.cell 58 Bool.true (CT.cell 8 Bool.true (CT.cell 7 Bool.true (CT.cell 4 Bool.false CT.tend CT.tend) (CT.cell 17 Bool.false CT.tend CT.tend)) (CT.cell 10 Bool.true (CT.cell 14 Bool.true (CT.cell 2 Bool.false CT.tend CT.tend) (CT.cell 0 Bool.false CT.tend CT.tend)) (CT.cell 17 Bool.false CT.tend CT.tend))) (CT.cell 0 Bool.false CT.tend CT.tend)) (CT.cell 1 Bool.true (CT.cell 61 Bool.true (CT.cell 8 Bool.true (CT.cell 7 Bool.true (CT.cell 4 Bool.false CT.tend CT.tend) (CT.cell 17 Bool.false CT.tend CT.tend)) (CT.cell 10 Bool.true (CT.cell 33 Bool.false CT.tend CT.tend) (CT.cell 17 Bool.false CT.tend CT.tend))) (CT.cell 0 Bool.false CT.tend CT.tend)) (CT.cell 1 Bool.true (CT.cell 60 Bool.true (CT.cell 8 Bool.true (CT.cell 7 Bool.true (CT.cell 4 Bool.false CT.tend CT.tend) (CT.cell 17 Bool.false CT.tend CT.tend)) (CT.cell 10 Bool.true (CT.cell 32 Bool.false CT.tend CT.tend) (CT.cell 17 Bool.false CT.tend CT.tend))) (CT.cell 0 Bool.false CT.tend CT.tend)) (CT.cell 0 Bool.false CT.tend CT.tend))))) (CT.cell 0 Bool.false CT.tend CT.tend))))) 0</code> | **Exact** for that certificate's zero budget; no extra premise. | No `test-formal` assertion; `lcert.verified-test/the-theorems-are-kernel-checked` is separate. |

The rest of §7 reports an ADR-0005 implementation and its tests, rather than six more kernel statements. Its present status is:

| §7 claim | Current counterpart and verdict | Suite coverage |
| --- | --- | --- |
| `code->ct`/`lcert.kernel` rendering preserves `nodes`, `ctxlen`, and `budget` on implementation codes | No kernel declaration relates the implementation code type to `CT`. **Open as a formal bridge:** `code/lcert/ansatz/lcert/verified.clj:141` defines the Clojure renderer, and the §7 assertion is a finite test, not a quantified theorem. No extra premise applies. | No `test-formal` assertion; `lcert.verified-test/verified-and-plain-measures-agree` checks the generated examples. |
| Every §1.4 rule is checked twice, and the two checkers agree | `dtCheck_agree` (`code/lcert/formal/lcert/formal/check_agree.clj:180`) proves equal `dtCheck` outcomes for a fixed derivation tree under `AgreeF` on the two checker functions. **Different:** this is conditional agreement of two formal checker parameters, not a theorem equating the historical implementation's two independent checkers. The historical claim remains test evidence. `agreeF_zero` witnesses `AgreeF` on a tree without δ records; identical functions also meet it. The kernel type is printed below. | `lcert.formal-test.check_agree/f7-check-agreement` asserts `dtCheck_agree`; the historical two-checker comparison is outside `test-formal`. |
| Encoding E3–E5 is tested | `budget_canon`, `toksize_canon`, `typesize_canon`, and `encE_inj` are kernel-checked counterparts. **Stronger** than the old test-only status; the elaborated types of `budget_canon` and `CheckSpec` are above. | `lcert.formal-test.certcanon/f7-canon-sizes`; `lcert.formal-test.encode/f7-encoding`. |
| Erasing evaluator and Theorem 5.2 are probed on three blocked constructs | `theorem52_all_concrete` and `theorem52e_all` prove safety for all runs at typed roots; their kernel types are under Theorem 5.2. **Stronger** than a finite probe. | `lcert.formal-test.s52univ/f5-s52univ`. |
| Propositions 4.2, 4.9 at depth 0, 4.10 and §4.7 destructor type check and run | `prop42`, all-depth `prop49_concrete`, and `prop410` have kernel statements above. **Stronger** for the first three. The §4.7 typed parser is outside the formalization; R4 §4.7 records why (A07). | `lcert.formal-test.section4b/f4-section4b`; `lcert.formal-test.p49all/all-depth-proposition-49`; no theorem assertion for the §4.7 equation. |

The historical tests' agreement with the draft worked example remains test evidence, not a general theorem about the whole §3 model. Lemma 3.6 and its consequences now have independent kernel counterparts listed above.

`dtCheck_agree` (kernel-held elaborated type):

```lean
∀ (chk1 : Code → Code → Bool), ∀ (chk2 : Code → Code → Bool), ∀ (tree : DT), ∀ (d : AgreeF (dtB tree) chk1 chk2), ∀ (JJ : DTJ), @Eq.{1} Bool (dtCheck chk1 tree JJ) (dtCheck chk2 tree JJ)
```

`dtCheck_sound` (`code/lcert/formal/lcert/formal/check_der.clj:286`; kernel-held elaborated type):

```lean
∀ (chkf : Code → Code → Bool), ∀ (tree : DT), ∀ (JJ : DTJ), ∀ (d : @Eq.{1} Bool (dtCheck chkf tree JJ) Bool.true), Holds chkf JJ
```

`encE_inj` (`code/lcert/formal/lcert/formal/encode.clj:271`; kernel-held elaborated type):

```lean
∀ (a : Exp), ∀ (b : Exp), ∀ (h : @Eq.{1} Code (encE a) (encE b)), @Eq.{1} Exp a b
```

## Draft `R4-certificate-calculus.md`

ADR-0006 takes the corrected `R4-metatheory.md` as its normative statement source. The draft explicitly marks L1–L4 and its conditional theorem as a superseded proof route (`R4-certificate-calculus.md` §3). For completeness, the numbered draft assertions have these counterparts:

| Draft assertion | Formal constants and kernel type | Verdict; premise satisfiability; suite coverage |
| --- | --- | --- |
| L1, strong normalization of every well-typed token-context term | None for strong normalization. `theorem4` (`code/lcert/formal/lcert/formal/theorem4.clj:1781`) has the elaborated type printed under Theorem 4 above. | **Different:** totality of the budgeted evaluator is a statement about evaluation, not strong normalization of the reduction relation. `argsOK` is an additional domain condition in the formal evaluator; typed roots satisfy it. `lcert.formal-test.theorem4/f5-skof-agree` asserts `theorem4`. The draft L1 itself is **open** if retained as a claim. |
| L2, subject reduction preserving usages | None. | **Open** as a draft assertion; no theorem says every reduction step preserves `Rt` and its usage vector. No suite coverage. It is not an ADR-0006 proof obligation because the corrected T1 proof does not use reduction. |
| L3, canonicity of normal forms in token contexts | None for the three stated normal-form clauses. `corollary51_conv` (`code/lcert/formal/lcert/formal/theorem4.clj:1839`) and `theorem4e_agree` (`code/lcert/formal/lcert/formal/theorem4e.clj:1120`) have their elaborated types above. | **Different:** they give canonical evaluator results and agreement on data, not a syntactic classification of all normal forms. Their checker/conversion hypotheses are met by `check_spec decCert` and `conv_all`, and erasure by `er_total`. Covered by `lcert.formal-test.theorem4/f5-skof-agree` and `lcert.formal-test.theorem4e/f5-theorem4e`. Draft L3 is **open** if retained literally. |
| L4, affine mass of a normal canonical subtree | `theorem3` (`code/lcert/formal/lcert/formal/lemma36.clj:205`) and `theorem3_tokens` (`code/lcert/formal/lcert/formal/lemma36.clj:234`); their elaborated types are printed under Theorem 3 above. | **Different:** semantic node and distinct-token bounds for all runtime R values replace the draft's syntactic normal-subtree statement. `check_spec decCert` and `conv_all` satisfy the generic premises. `lcert.formal-test.lemma36/f3n-lemma36-and-main-results` asserts both. |
| A, accepted `print r` decodes to a derivation whose budget is strictly below the nodes of `r` | `CheckSpec` (`code/lcert/formal/lcert/formal/model.clj:106`), `check_spec` (`code/lcert/formal/lcert/formal/check_spec.clj:280`), and `budget_canon` (`code/lcert/formal/lcert/formal/certcanon.clj:390`, generated in the named theorem loop); elaborated types appear under Lemmas 2.6–2.7 above. | **Exact for the canonical concrete checker:** `check_spec decCert` establishes soundness and the strict size clause, with `budget_canon` deriving that clause from the encoding. `lcert.formal-test.check_spec/f7-concrete-checker` and `lcert.formal-test.certcanon/f7-canon-sizes` assert the respective constants. |
| Theorem conditional on L1–L4 and A; consistency corollary | `consistent_concrete` (`code/lcert/formal/lcert/formal/selfjust.clj:31`) has its elaborated type under Theorem 1 above; `self_justification` (`code/lcert/formal/lcert/formal/selfjust.clj:60`) is under Theorem 2. | **Stronger at the corrected scope:** T1 needs none of L1–L4. The consistency conclusion is relative to the Ansatz kernel, bundled Lean Init export and metatheory. `lcert.formal-test.selfjust/self-justification` asserts the constants. |

The draft's L2 and L3 are historical assertions, not additional promised phase-1 kernel theorems. Their literal forms remain open. The corrected metatheory supplies the results used by the current consistency argument.

## Trust-base audit

The ready warm environment contains **29,306 declarations** and exactly three axioms: `Classical.choice`, `Quot.sound`, and `propext`. A source scan with `rg -n 'ansatz-env|Env\.addConstant|add-constant|axiom|opaque|sorry|implemented_by|extern|unsafe' code/lcert/formal/lcert/formal -g '*.clj'` found one direct update of `a/ansatz-env`: `base.clj:50` calls `env/check-constant` on an elaborated definition. Other declarations use `a/prove-theorem`, `a/inductive`, or `a/defn`'s kernel path. The hits for `axiom`, `opaque`, and `unsafe` are prose or object-calculus terminology; `sorry`, `implemented_by`, and `extern` yielded no formal declarations. No formal source calls `Env.addConstant` or installs an axiom, and none uses Ansatz's `^:partial` or foreign definers, which would install trusted axioms. Ansatz may add a temporary self-reference axiom to a *forked* environment while elaborating a recursive `a/defn`; it checks the resulting definition against the real environment before installation (`ansatz/core.clj`, `define-verified`). On first formal load, each persistent new declaration goes through a kernel check. On a warm-server *recheck*, `formal_server.clj` can copy previously checked constants back with `Env.addConstant` before checking changed namespaces; that is a cache operation, not a fresh proof check. The bundled Init export is the separate trusted exception described next. This source scan is not a proof of kernel soundness.

In Ansatz 0.2.115's jar, `ansatz/core.clj:257–278` defines `load-init!`; line 271 calls `replay/replay` on `ansatz/init-medium.ndjson.gz` with `:verify? false` (the alternate bundled loader at lines 234–243 does likewise). Metadata inside that export says Lean 4.29.0-rc2; the warm loader reports 3,653 declarations. These Init declarations are **trusted by Ansatz as an export checked by Lean's kernel, not rechecked by Ansatz**. A transitive dependency walk over each declaration’s type and value found all three axioms in every sampled headline theorem: `consistent_concrete`, `self_justification`, `theorem3`, `theorem4`, `theorem4e`, `theorem52_all_concrete`, `prop5`, `pa_consistent_via_lcert`, `prop412`, `lemma64_concrete`, and `cor65_concrete`. The environment contains no other axioms. This is an Ansatz dependency-closure calculation, not a claim that each axiom is logically essential to each result.

## Open items (phase 1)

- [x] A03 — Lemma 2.8 — give an explicit theorem for closed canonical `print` preserving labels and internal-node count, connected to the object-language term, beyond `stdRI_pr`'s identity interpretation.
- [x] A09 — Theorem 4.6 / Lemma 4.6a premise satisfiability — add a named witness theorem for `PhOk J σ` (for example the constant safe-leaf family) at arbitrary J and document how concrete certifiability witnesses fill the hypothesis.
- [x] A17 — Proposition 4.10 witness — prove a named closed derivation of `λ(x :₁ 0). x :¹ 0 ⊸ 0`, apply `check_complete_lbl` to obtain an accepted label-safe concrete certificate v, and specialize `prop410` at the fixed budget `cnodes v`.
- [x] A11 — Lemma 2.4 formation premise — prove that a closed well-formed type A in the paper's two typing hypotheses supplies `Tl chkf true [] A 1`, or amend the paper to require that formation derivation explicitly.
- [x] A13 — Proposition 4.3 and Corollaries 4.6′/4.6″ — `muRaw` is the paper's least accepted size, with `raw_min_exists`/`muRaw_witness` and `prop43_mu_F7`. The runtime upper bounds require label-safe codes: `muCert` selects their minimum, `least_cert_witness`/`mu_threshold` prove the threshold, and `cor46_prime_mu_F7`/`cor46_dprime_mu_F7` specialize it. R4 now calls this second quantity `μ_L`; no equality of the two minima is claimed.
A08 — §7 legacy kernel facts: **superseded by ADR-0008** (implementation extraction). The six historical constants have separate legacy coverage; any implementation bridge belongs to ADR-0008.
A16 — §7 code-to-CT rendering: **superseded by ADR-0008** (implementation extraction and measure preservation). Current §7 bridge remains test evidence.
A15 — §7 independent-checker agreement: **superseded by ADR-0008** (extracted implementation and checker agreement). `dtCheck_agree` only compares formal checker functions under `AgreeF`.
- [x] A10 — Proposition 4.11 — reopened 2026-10-15 and closed: exact F7 size `16n² + 97n + 73` (`prop411_cert_size`, prop411size.clj), quadratic bound, kernel-evaluated table, and the exponential gap from `n = 12` (`prop411_exponential_gap`).
- [x] A06 — Proposition 4.12(1) — reopened 2026-10-15 and closed: exact F7 size `2|c| = n² + 13n + 732` (`prop412_cert_size`, prop412size.clj) and kernel-evaluated counts at `n = 4, 8, 12`.
- [x] A05 — F6.3 / Proposition 6.1 external translation — `paCloseF` is the finite-support universal closure ⟦∀ᶜˡφ⟧, and `pa_lcert_closed` proves the paper's closed-line form for every H_PA proof. `paClose_type_agree` and `pa_finite_lcert_pos` connect it to the open-context translation `pa_lcert`. R4 §6.2's closure now allows vacuous binders, so the formal closure is an instance of the paper's definition; the closed-to-open converse is not a paper claim and is not formalized.
- [x] A07 — §4.7 typed parser — out of formalization scope, recorded in R4 §4.7 (amended 2026-10-15): the parser's typing and adequacy are paper arguments checked by the implementation's tests; no result depends on them, since Proposition 4.9's formalized case analysis proves the bounded instances. ADR-0008's runner will certify the parser's typing with the extracted `Check`.
- [x] A01 — Lemma 2.1 exchange — R4 amended: Lemma 2.1 is weakening only, which `tl_weaken` and `rt_weaken` prove. Exchange is not used by any result and is not claimed.
- [x] A02 — Lemma 2.2 renaming — R4 amended to the de Bruijn identity case, as this item allowed. General injective renaming is not used and is not claimed.
- [x] A14 — §0 T2 / Willard2016 Definition 3.4 — `willard_def34` (willard.clj) proves both clauses at `Check`: clause (i)'s derivations (T2) and clause (ii) as T1 and as Corollary 3.7 (no accepted certificate of `0`). `Hcirc_reads` shows `H°`'s type is literally `Π(r :₁ R). T(chk′ (print r) ⌜0⌝) ⊸ 0`. That `H°` *states* consistency is the reading of that syntax; Definition 3.4 is informal, and R4 §3.9 now says so.

## Phase 2 items

- S1–S3: internal Gödel second theorem, PA Σ₁ completeness, and E-PA^ω conservativity, as used in the paper's Proposition 5.
- Int-6.1: Proposition 6.1's primitive-recursive proof-code translation and PA-internal correctness of `CHECK`; the meta-level `pa_lcert` does not assert this.
- Lemma 6.2: E-PA^ω-internal monotonicity, substitution and conversion for Δ, with an actual E-PA^ω syntax and derivability predicate.
- Lemma 6.3: PA-internal BF₀ and BF₁ at each numeral bound, including finite enumeration.
- Int-6.4: Lemma 6.4's E-PA^ω-internal default-model proof for every Δ node and Corollary 6.5's E-PA^ω derivation of Con_λ from Δ. `EPAw` is currently abstract.
- Proposition 5 and Proposition 4.8 become unconditional only after their named arithmetic hypotheses and representation anchors are discharged.
