# Tab and Xtab as inductive families in dependent type theory

Date: 2026-09-05 (America/Indiana/Indianapolis).

[CLI-readable ASCII edition](2026-09-05-tab-xtab-dependent-types-cli.txt).

## Request and scope

User request, following the question how a semantic/analytic tableau might be
defined in dependent type theory:

> Write these please.

This note writes Willard's **Tab** and **Xtab** as inductive families indexed by
signed contexts (equivalently: formula lists along a branch), with well-formedness
enforced by the indices. It is a paper construction in the same register as the
[PTS translation](2026-09-05-sjas-pure-type-systems.md): not a mechanized Lean/Agda
development, and not an implementation ADR. Codification/Refinement registries
and Proflog's arithmeticized `tableau-proof/3` are left untouched.

Primary textual sources for the rule tables:

- six elimination rules plus bounded hybrids (a),(b), and the Xtab LEM clause:
  Willard 2020 Appendix (printed pp. 22–23);
- eight-rule presentation with bounded ∃/∀ as separate numbered rules:
  Willard 2001 §4 / Willard 2018 §3.2;
- Z-Enriched / Z-Base-Variable-Enriched: Willard 2020 Definitions 7.1–7.2.

## 1. Object language

Fix a first-order language \(L\) with the connectives and quantifiers Willard
uses: \(\land\), \(\lor\), \(\to\), \(\lnot\), \(\forall\), \(\exists\), and the
bounded abbreviations

\[
\begin{aligned}
\forall v\le s\,\Upsilon(v)
  &\equiv \forall v\,(v\le s\to\Upsilon(v)),\\
\exists v\le s\,\Upsilon(v)
  &\equiv \exists v\,(v\le s\land\Upsilon(v)).
\end{aligned}
\tag{1}
\]

Let \(\mathrm{Form}\) be the set of \(L\)-sentences (closed formulas). Open
formulas appear only after quantifier instantiation, as sentences in an
expanded language \(L[\vec p]\) with finitely many **parameter symbols**
\(\vec p\). Write \(\mathrm{Form}[\vec p]\) for that set.

An axiom system \(\alpha\) is a set of sentences of \(L\). Parameters may appear
in deductions; they do not appear in members of \(\alpha\).

This matches Willard's candidate-tree discipline: axioms are proper sentences;
parameters are introduced only by existential rules.

## 2. Branches as dependent indices

A **branch context** is a finite list of sentences

\[
\Gamma = (\varphi_0,\ldots,\varphi_{n-1})\in\mathrm{List}(\mathrm{Form}[\vec p]).
\tag{2}
\]

Membership \(\varphi\in\Gamma\) means some entry equals \(\varphi\). Extension is
cons at the head (current tip of the branch):

\[
\varphi{::}\Gamma.
\]

Willard's printed trees store **one sentence per node**, with deduction
justified by an **ancestor**. The branch-list presentation is definitionally
equivalent for the inductive proof predicate: the list is the sequence of
node-labels from tip to root (or root to tip; orientation is conventional).
Sibling splits become pairs of child derivations. This is the usual translation
between Fitting–Smullyan branch calculi and Willard's candidate trees.

**Closure** of a branch is complementary pair membership:

\[
\mathrm{Closed}(\Gamma)
  :\equiv
  \exists\varphi.\;
  \varphi\in\Gamma\;\land\;(\lnot\varphi)\in\Gamma.
\tag{3}
\]

No separate polarity type is required: Willard closes on \(\varphi\) and
\(\lnot\varphi\) as sentences, with root \(\lnot\Psi\).

## 3. Tab as an inductive family (six-rule / 2020 profile)

Fix \(\alpha\). The judgment

\[
\mathrm{Tab}_\alpha(\Gamma)
\tag{4}
\]

means: every root-to-leaf completion of the current branch \(\Gamma\) can be
closed by analytic tableau expansion from ancestors and axioms of \(\alpha\).
It is an inductive family \(\mathrm{List}(\mathrm{Form}[\vec p])\to\mathrm{Type}\)
(or \(\mathrm{Prop}\), if only inhabitation matters).

Constructors, named to match Willard 2020 Appendix items 1–6 and hybrids
(a),(b):

### 3.1 Closure and axioms

\[
\begin{aligned}
\mathrm{close}&:
  \varphi\in\Gamma \to
  (\lnot\varphi)\in\Gamma \to
  \mathrm{Tab}_\alpha(\Gamma),
\\
\mathrm{ax}&:
  \varphi\in\alpha \to
  \mathrm{Tab}_\alpha(\varphi{::}\Gamma) \to
  \mathrm{Tab}_\alpha(\Gamma).
\end{aligned}
\tag{5}
\]

\(\mathrm{ax}\) is the non-analytic proper-axiom step: any axiom may be written
onto the branch. Analyticity of the connective rules is unaffected.

### 3.2 Conjunctive / negation normal steps (Rules 1–2)

\[
\begin{aligned}
\mathrm{and}_1&:
  (\Upsilon\land\Gamma)\in\Delta \to
  \mathrm{Tab}_\alpha(\Upsilon{::}\Delta) \to
  \mathrm{Tab}_\alpha(\Delta),
\\
\mathrm{and}_2&:
  (\Upsilon\land\Gamma)\in\Delta \to
  \mathrm{Tab}_\alpha(\Gamma{::}\Delta) \to
  \mathrm{Tab}_\alpha(\Delta),
\\
\mathrm{dnn}&:
  (\lnot\lnot\Upsilon)\in\Delta \to
  \mathrm{Tab}_\alpha(\Upsilon{::}\Delta) \to
  \mathrm{Tab}_\alpha(\Delta).
\end{aligned}
\tag{6}
\]

Rule 2's remaining negation pushes are definitional abbreviations before
applying the inductive rules, or else are separate constructors with the same
shape. Written out:

\[
\begin{aligned}
\mathrm{neg}\lor&:
  \lnot(\Upsilon\lor\Gamma)\in\Delta \to
  \mathrm{Tab}_\alpha((\lnot\Upsilon\land\lnot\Gamma){::}\Delta) \to
  \mathrm{Tab}_\alpha(\Delta),
\\
\mathrm{neg}\to&:
  \lnot(\Upsilon\to\Gamma)\in\Delta \to
  \mathrm{Tab}_\alpha((\Upsilon\land\lnot\Gamma){::}\Delta) \to
  \mathrm{Tab}_\alpha(\Delta),
\\
\mathrm{neg}\land&:
  \lnot(\Upsilon\land\Gamma)\in\Delta \to
  \mathrm{Tab}_\alpha((\lnot\Upsilon\lor\lnot\Gamma){::}\Delta) \to
  \mathrm{Tab}_\alpha(\Delta),
\\
\mathrm{neg}\exists&:
  \lnot\exists v\,\Upsilon(v)\in\Delta \to
  \mathrm{Tab}_\alpha((\forall v\,\lnot\Upsilon(v)){::}\Delta) \to
  \mathrm{Tab}_\alpha(\Delta),
\\
\mathrm{neg}\forall&:
  \lnot\forall v\,\Upsilon(v)\in\Delta \to
  \mathrm{Tab}_\alpha((\exists v\,\lnot\Upsilon(v)){::}\Delta) \to
  \mathrm{Tab}_\alpha(\Delta).
\end{aligned}
\tag{7}
\]

Bounded variants of the last two (Willard 2001 Rule 2 extras) are optional
constructors; under (1) they are redundant with \(\mathrm{neg}\exists\) /
\(\mathrm{neg}\forall\) plus the hybrids below.

### 3.3 Disjunctive splits (Rules 3–4)

\[
\begin{aligned}
\mathrm{or}&:
  (\Upsilon\lor\Gamma)\in\Delta \to
  \mathrm{Tab}_\alpha(\Upsilon{::}\Delta) \to
  \mathrm{Tab}_\alpha(\Gamma{::}\Delta) \to
  \mathrm{Tab}_\alpha(\Delta),
\\
\mathrm{imp}&:
  (\Upsilon\to\Gamma)\in\Delta \to
  \mathrm{Tab}_\alpha((\lnot\Upsilon){::}\Delta) \to
  \mathrm{Tab}_\alpha(\Gamma{::}\Delta) \to
  \mathrm{Tab}_\alpha(\Delta).
\end{aligned}
\tag{8}
\]

Both premises are required: a Tab proof closes **every** sibling branch. In
dependent type theory this is a pair of subderivations, i.e. a product of
proofs—not a sum.

### 3.4 Quantifiers (Rules 5–6) and bounded hybrids (a),(b)

Let \(\mathrm{Tm}[\vec p]\) be the terms of the current parameter language.

\[
\begin{aligned}
\mathrm{all}&:
  (\forall v\,\Upsilon(v))\in\Delta \to
  (t:\mathrm{Tm}[\vec p]) \to
  \mathrm{Tab}_\alpha(\Upsilon(t){::}\Delta) \to
  \mathrm{Tab}_\alpha(\Delta),
\\
\mathrm{ex}&:
  (\exists v\,\Upsilon(v))\in\Delta \to
  (p\notin\vec p) \to
  \mathrm{Tab}_{\alpha}^{[\vec p,p]}(\Upsilon(p){::}\Delta) \to
  \mathrm{Tab}_\alpha(\Delta),
\\
\mathrm{ball}&:
  (\forall v\le s\,\Upsilon(v))\in\Delta \to
  (t:\mathrm{Tm}[\vec p]) \to
  \mathrm{Tab}_\alpha((t\le s\to\Upsilon(t)){::}\Delta) \to
  \mathrm{Tab}_\alpha(\Delta),
\\
\mathrm{bex}&:
  (\exists v\le s\,\Upsilon(v))\in\Delta \to
  (p\notin\vec p) \to
  \mathrm{Tab}_{\alpha}^{[\vec p,p]}((p\le s\land\Upsilon(p)){::}\Delta) \to
  \mathrm{Tab}_\alpha(\Delta).
\end{aligned}
\tag{9}
\]

Freshness \(p\notin\vec p\) is an index constraint: the existential constructors
extend the parameter context. That is the dependent-type content of Willard's
“newly introduced parameter symbol.” The eight-rule 2001/2018 profile is the
same family with \(\mathrm{ex}\)/\(\mathrm{bex}\)/\(\mathrm{all}\)/\(\mathrm{ball}\)
as Rules 5–8 and with U-Grounded / parameter-term side conditions recorded as
additional indices on \(t\) (see §5).

### 3.5 Proofs of theorems

A **Tab-proof of \(\Psi\) from \(\alpha\)** is an inhabitant

\[
\mathrm{TabProof}_\alpha(\Psi)
  :\equiv
  \mathrm{Tab}_\alpha\bigl((\lnot\Psi)\bigr).
\tag{10}
\]

Root \(\lnot\Psi\); every completed branch closed. Prenex\* normalisation of the
root is **not** required in the 2020 Appendix profile (contrast Willard 2001
§4). Treat prenex\* as an optional preprocessing constructor if a selected
engine demands it; do not silently identify the two profiles.

## 4. Xtab = Tab + cut / LEM constructor

Willard 2020: Xtab is identical to Tab “except that any node may optionally
store a sentence of the form \(\mho\lor\lnot\mho\) (for arbitrary \(\mho\)).”

\[
\begin{aligned}
\mathrm{Xtab}_\alpha(\Gamma)
&\quad\text{has every constructor of }\mathrm{Tab}_\alpha,
\\
\mathrm{lem}&:
  (\mho:\mathrm{Form}[\vec p]) \to
  \mathrm{Xtab}_\alpha\bigl((\mho\lor\lnot\mho){::}\Gamma\bigr) \to
  \mathrm{Xtab}_\alpha(\Gamma).
\end{aligned}
\tag{11}
\]

Equivalently, define a one-constructor extension

\[
\mathrm{Xtab}_\alpha
  :\equiv
  \mathrm{Tab}_\alpha
  +
  \mathrm{lem}.
\tag{12}
\]

This is Definition 4.2 / Appendix “Added Comment” of Willard 2020, and is the
TR's “tableaux proof with cuts” when cut is identified with writing a LEM
instance as a logical axiom (not as a derived Tab theorem).

**Z-Enriched** (Def. 7.1) restricts the cut formula:

\[
\mathrm{lem}_Z:
  (\Upsilon\in Z) \to
  \mathrm{Tab}^Z_\alpha\bigl((\Upsilon\lor\lnot\Upsilon){::}\Gamma\bigr) \to
  \mathrm{Tab}^Z_\alpha(\Gamma).
\tag{13}
\]

**Z-Base-Variable-Enriched** (Def. 7.2) admits

\[
\forall x\,\Upsilon(x)\;\lor\;\lnot\Upsilon(x)
\quad(\Upsilon(x)\in Z\text{ free in }x)
\tag{14}
\]

as a further constructor. Ordinary Xtab is the case \(Z=\) all sentences.

There is no claim here that \(\mathrm{lem}\) is analytic. It is the intentional
efficiency / Linear-Sum resource that Willard isolates when comparing
\(\mathrm{IS}_{\mathrm{Tab}}\) with \(\mathrm{IS}_{\mathrm{Xtab}}\).

## 5. Eight-rule profile as the same family

Willard 2001/2018 number bounded quantifiers separately. The inductive family
is unchanged; only the constructor inventory is partitioned differently:

| 2001/2018 | 2020 Appendix | Constructor |
| --- | --- | --- |
| 1 | 1 | \(\mathrm{and}_{1,2}\) |
| 2 | 2 | \(\mathrm{dnn}\), \(\mathrm{neg}\ast\) |
| 3 | 3 | \(\mathrm{or}\) |
| 4 | 4 | \(\mathrm{imp}\) |
| 5 | 6 | \(\mathrm{ex}\) |
| 6 | (b) | \(\mathrm{bex}\) |
| 7 | 5 | \(\mathrm{all}\) (term side-condition varies) |
| 8 | (a) | \(\mathrm{ball}\) |

U-Grounded / parameter-term restrictions on \(t\) in Rules 7–8 are expressed by
indexing \(\mathrm{all}\)/\(\mathrm{ball}\) by a predicate
\(\mathrm{Admissible}(\vec p,t)\) rather than by changing the shape of the
family. Different papers' admissible-term dialects are therefore different
instances of one inductive signature.

## 6. Reading in a pure type system

Relative to the [PTS note](2026-09-05-sjas-pure-type-systems.md), there are
three distinct objects again:

| Object | Type-theoretic status |
| --- | --- |
| Encoded proof number \(p:\iota\) | Arithmetic data for \(\mathrm{Prf}_S\) |
| Closed candidate tree | Inhabitant of \(\mathrm{TabProof}_\alpha(\Psi)\) or \(\mathrm{XtabProof}_\alpha(\Psi)\) |
| Translated theorem | Inhabitant of \(\llbracket\Psi\rrbracket\) in \(\lambda P\) |

The inductive families of §§3–4 are the **middle** row: a dependently typed
presentation of the deduction apparatus \(d\in\{\mathrm{Tab},\mathrm{Xtab}\}\).
They are not the Gödelized \(\mathrm{Prf}\) relation, and they are not yet the
negative translation \(\llbracket-\rrbracket\).

Paper-level bridges, not established here as theorems of \(S\):

1. **Soundness of the family.** An inhabitant of
   \(\mathrm{TabProof}_\alpha(\Psi)\) yields classical consequence
   \(\alpha\models\Psi\) (equivalently: no model of \(\alpha\cup\{\lnot\Psi\}\)).
2. **Arithmeticization.** A conventional encoding of finite trees gives a
   \(\Delta_0\)-style predicate \(\mathrm{SemPrf}^d_\alpha\) whose standard-model
   extension coincides with inhabitation of the inductive family for finite
   \(\alpha,\Psi\).
3. **PTS transport.** By the inhabitation equivalence (8) of the PTS note,
   \(\alpha\vdash_d\Psi\) iff \(\Sigma\vdash t:\llbracket\Psi\rrbracket\) for some
   \(t\), once \(d\) is fixed to Tab or Xtab and completeness/soundness of that
   apparatus for classical FOL are assumed externally.

Cut-freeness of Tab is visible in the constructor set: there is no
\(\mathrm{lem}\). Under Curry–Howard, Tab proof objects are already fully
expanded refutations; Xtab proof objects may mention LEM lemmas and therefore
correspond to proofs with non-analytic cuts. That is the dependent-type
rendering of the observation that cut-free SJAS apparatuses resist “evaluate
during proof search, then quote” interpretations unless the quoted object is
allowed to be the Tab inhabitant itself (or its arithmetic code).

## 7. Minimal Lean-shaped sketch

Illustrative only; not checked by a kernel.

```lean
inductive Form : Type where
  | atom : Nat → Form
  | and  : Form → Form → Form
  | or   : Form → Form → Form
  | imp  : Form → Form → Form
  | neg  : Form → Form
  | all  : (Nat → Form) → Form   -- v ↦ Υ(v), schematic
  | ex   : (Nat → Form) → Form

/-- Tab_α Γ : the branch Γ can be completed to a closed Tab tree. -/
inductive Tab (α : Form → Prop) : List Form → Type where
  | close {Γ φ} :
      φ ∈ Γ → Form.neg φ ∈ Γ → Tab α Γ
  | ax {Γ φ} :
      α φ → Tab α (φ :: Γ) → Tab α Γ
  | and₁ {Δ Υ Γ} :
      Form.and Υ Γ ∈ Δ → Tab α (Υ :: Δ) → Tab α Δ
  | and₂ {Δ Υ Γ} :
      Form.and Υ Γ ∈ Δ → Tab α (Γ :: Δ) → Tab α Δ
  | dnn {Δ Υ} :
      Form.neg (Form.neg Υ) ∈ Δ → Tab α (Υ :: Δ) → Tab α Δ
  | or {Δ Υ Γ} :
      Form.or Υ Γ ∈ Δ →
      Tab α (Υ :: Δ) → Tab α (Γ :: Δ) → Tab α Δ
  | imp {Δ Υ Γ} :
      Form.imp Υ Γ ∈ Δ →
      Tab α (Form.neg Υ :: Δ) → Tab α (Γ :: Δ) → Tab α Δ
  | all {Δ Υ t} :
      Form.all Υ ∈ Δ → Tab α (Υ t :: Δ) → Tab α Δ
  | ex {Δ Υ p} :
      Form.ex Υ ∈ Δ →
      -- freshness of p is a side condition on the ambient parameter context
      Tab α (Υ p :: Δ) → Tab α Δ
  -- ball, bex, neg* : same shape; omitted for brevity

inductive Xtab (α : Form → Prop) : List Form → Type where
  | ofTab {Γ} : Tab α Γ → Xtab α Γ
  | lem {Γ mho} :
      Xtab α (Form.or mho (Form.neg mho) :: Γ) → Xtab α Γ

def TabProof (α : Form → Prop) (Ψ : Form) : Type :=
  Tab α [Form.neg Ψ]

def XtabProof (α : Form → Prop) (Ψ : Form) : Type :=
  Xtab α [Form.neg Ψ]
```

A production formalization should replace schematic `Nat → Form` quantifiers by
a locally nameless or named binder discipline, and should index `Tab` by the
parameter context so that `ex`/`bex` freshness is judged, not commented.

## 8. What this does and does not settle

Provided:

- an inductive-family definition of Willard Tab (six- and eight-rule readings);
- Xtab as that family plus a LEM/cut constructor;
- Z-enrichments as restrictions of the cut formula;
- the separation among encoded \(\mathrm{Prf}\), Tab/Xtab inhabitants, and
  PTS theorem witnesses.

Not provided:

- a mechanized proof of soundness/completeness;
- an arithmeticization theorem tying the family to Proflog `tableau-proof/3`;
- an internal bridge from Tab inhabitants to \(\Box_S\) in the PTS sense;
- a claim that Xtab's LEM constructor restores uniform HBL-(2) inside an SJAS.

## 9. Verification record

Rule inventories were checked against the local text layers
`target/sjas-pdf-text/willard2020.txt` (Appendix) and
`willard2018.txt` / `willard2001_self_verifying_axiom_systems_author_jsl1.txt`
(§3.2 / §4). Cross-links to Codification extractions
`willard2020.md` (Appendix row; Defs 7.1–7.2) and the drift note that 2020 drops
prenex\* on the root while 2001 requires it. No assistant kernel checked the
Lean sketch. No registry status labels were changed.
