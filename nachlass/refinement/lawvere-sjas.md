# Lawvere–Yanofsky Translation of Willard's SJAS

*R6 of the Refinement stage. Charter:
[`ADR-0004-lawvere-sjas-translation.md`](ADR-0004-lawvere-sjas-translation.md).*

> **Methodological order (binding).** First prove G2 using Lawvere / Yanofsky
> techniques, with every hypothesis named. Only then determine where Willard's
> constructions invalidate particular hypotheses of that limitative theorem.

> **Provenance.** Claims about Willard's corpus cite image-verified register rows
> in [`VERIFICATION.md`](VERIFICATION.md) or extraction records. Departures from
> Willard's vocabulary are declared. This document does **not** re-prove
> Willard's consistency theorems for `IS(A)`; it translates their logical
> structure into Lawvere / Yanofsky form and proves the translation theorems.

---

## 0. The complete result (what is proved)

**Main Theorem (Self-justification in the Lawvere–Yanofsky scheme).**
Let $\alpha$ be a theory with the data of hypotheses H0 and H2 below
(Lindenbaum objects, arithmetized provability). Then:

1. **(G2 in-scheme.)** If H0–H7 all hold, then $\alpha\vdash\mathrm{Con}$ implies
   $\alpha\vdash\bot$. Under external consistency H8, therefore
   $\alpha\nvdash\mathrm{Con}$. *(Theorem 3.2.)*
2. **(Necessary conditions.)** If H0–H2, H6, H8 hold and
   $\alpha\vdash\mathrm{Con}$, then at least one of H3–H5, H7 fails.
   *(Theorem 6.2 — obligatory breach.)*
3. **(Mechanism.)** For Willard's Type-A system `IS(A)`, H1 holds (Group-3 is
   a Lawvere fixed point) while the **uniform** H4 arrow
   $\mathrm{mp}_\square$ fails to lie in the affordable class
   $\mathrm{Aff}(\mathrm{IS}(A))$ — Willard's own uniformity failure, stated as
   a categorical missing arrow. *(Theorems 5.5–5.6.)*
4. **(Definition.)** $\alpha$ is *self-justifying* in this scheme iff H1, H8, and
   $\vdash\mathrm{Con}$ hold and at least one of H3–H7 fails.
   *(Definition 6.1; necessity proved, sufficiency of the breach alone disproved
   by Beklemishev–Shamkanov.)*

That is the complete result. The sections prove it in order.

---

## 1. Lawvere / Yanofsky (the scheme)

We work in Yanofsky's product form (Yanofsky 2022 Thms 6.4–6.5; cf. Lawvere
1969, Yanofsky 2003): terminal object and binary products suffice.

**Definition 1.1 (Fixed point).** $p:1\to Y$ is a fixed point of
$\alpha:Y\to Y$ when $\alpha\circ p=p$.

**Definition 1.2 (Representability).** $g:A\to Y$ is *representable in*
$f:A\times A\to Y$ when $\exists\,p:1\to A$ with
$g=f\circ(p\times\mathrm{id}_A)\circ i$ for $i:A\simeq 1\times A$.

**Theorem 1.3 (Diagonal form; Yanofsky 2022 Thm 6.4).**
If $\alpha:Y\to Y$ has no fixed point, then for every $A$ and every
$f:A\times A\to Y$ there is $g:A\to Y$ not representable in $f$.

*Proof.* Let $\Delta:A\to A\times A$ and $g:=\alpha\circ f\circ\Delta$. If $p$
represents $g$, then $f\circ\Delta\circ p$ satisfies
$\alpha\circ(f\circ\Delta\circ p)=f\circ\Delta\circ p$, a fixed point. Contradiction. ∎

**Theorem 1.4 (Fixed-point form; Yanofsky 2022 Thm 6.5).**
If some $f:A\times A\to Y$ represents every $g:A\to Y$, then every
$\alpha:Y\to Y$ has a fixed point.

*Proof.* For arbitrary $\alpha$, the map $g:=\alpha\circ f\circ\Delta$ is
representable by some $p$; then $f\circ\Delta\circ p$ fixes $\alpha$. ∎

**Definition 1.5 (Lawvere situation).** $(A,Y,f)$ where $f$ represents every
$A\to Y$.

---

## 2. The Lawvere half: diagonal lemma

**Setup 2.1.** Let $\mathrm{Lind}^i$ be the Lindenbaum algebra of formulas with
$i$ free variables, modulo provable equivalence in the ambient theory. Define
$$
f:\mathrm{Lind}^1\times\mathrm{Lind}^1\to\mathrm{Lind}^0,
\qquad
f\bigl(\mathcal{B}(x),\,\mathcal{H}(y)\bigr)
\;=\;
\mathcal{H}\bigl(\ulcorner\mathcal{B}(x)\urcorner\bigr).
$$
Assume a diagonalisation function $D$ on Gödel numbers with
$D(\ulcorner\mathcal{B}(x)\urcorner)=\ulcorner\mathcal{B}(\ulcorner\mathcal{B}(x)\urcorner)\urcorner$,
available as in the usual arithmetization (Yanofsky 2003; Mendelson).

**Theorem 2.2 (Diagonal lemma as Lawvere fixed point).**
For any unary formula $\mathcal{E}(x)$, define
$\Phi_{\mathcal{E}}:\mathrm{Lind}^0\to\mathrm{Lind}^0$ by
$\Phi_{\mathcal{E}}(\mathcal{P})=\mathcal{E}(\ulcorner\mathcal{P}\urcorner)$.
Then $\Phi_{\mathcal{E}}$ has a fixed point $\mathcal{C}$ with
$\vdash\mathcal{C}\leftrightarrow\mathcal{E}(\ulcorner\mathcal{C}\urcorner)$.

*Proof (fully expanded).* Let $A=\mathrm{Lind}^1$, $Y=\mathrm{Lind}^0$, and $f$
as above. Define $g:A\to Y$ by
$$
g \;=\; \Phi_{\mathcal{E}}\circ f\circ\Delta,
$$
so
$g(\mathcal{B}(x))=\mathcal{E}\bigl(\ulcorner\mathcal{B}(\ulcorner\mathcal{B}(x)\urcorner)\urcorner\bigr)$.

Let $\mathcal{G}(x):=\mathcal{E}(D(x))$. Then for any $\mathcal{B}(x)$,
$$
\begin{aligned}
g(\mathcal{B}(x))
&=\mathcal{E}\bigl(\ulcorner\mathcal{B}(\ulcorner\mathcal{B}(x)\urcorner)\urcorner\bigr)
=\mathcal{E}\bigl(D(\ulcorner\mathcal{B}(x)\urcorner)\bigr)
=\mathcal{G}\bigl(\ulcorner\mathcal{B}(x)\urcorner\bigr)
=f\bigl(\mathcal{B}(x),\,\mathcal{G}(y)\bigr).
\end{aligned}
$$
Thus $g$ is representable in $f$ by the point $p:1\to A$ selecting
$\mathcal{G}$. By Theorem 1.4, $\Phi_{\mathcal{E}}$ has a fixed point at
$$
\mathcal{C}
\;=\;
\mathcal{G}\bigl(\ulcorner\mathcal{G}(x)\urcorner\bigr)
\;=\;
\mathcal{E}\bigl(D(\ulcorner\mathcal{G}(x)\urcorner)\bigr)
\;=\;
\mathcal{E}(\ulcorner\mathcal{C}\urcorner),
$$
and the chain of equalities in $\mathrm{Lind}^0$ is exactly
$\vdash\mathcal{C}\leftrightarrow\mathcal{E}(\ulcorner\mathcal{C}\urcorner)$. ∎

**Corollary 2.3 (G1).** Taking $\mathcal{E}(x)\equiv\neg\mathrm{Der}(x)$ yields
$G$ with $\vdash G\leftrightarrow\neg\square G$. Under external consistency /
$\omega$-consistency, $G$ is unprovable. ∎

**Corollary 2.4 (Group-3 is Lawvere).**
Willard's Group-3 axiom is constructed by a Kleene / Rogers / Jeroslow fixed
point over a substitution predicate (codified §5.1; `Willard1993-TR` Appendix A;
`Willard2005` Eqs. 7–8). That construction is Theorem 2.2 applied to a
consistency-shaped $\mathcal{E}$. Therefore every affirmative Willard system
**satisfies H1** below: it is a Lawvere situation for the endomorphism building
Group-3. ∎

---

## 3. G2 proved in the Lawvere / Yanofsky scheme

### 3.1 Hypothesis register

| Id | Hypothesis | Content |
| --- | --- | --- |
| **H0** | Ambient | Category with $1$ and products; $Y=\mathrm{Lind}^0$; $A=\mathrm{Lind}^1$ |
| **H1** | Lawvere representability | $f$ of Setup 2.1 represents the $g$ of Theorem 2.2 (diagonal lemma) |
| **H2** | Provability morphism | $\square:Y\to Y$ interprets $\Phi\mapsto\mathrm{Der}(\ulcorner\Phi\urcorner)$ |
| **H3** | HB (1) | $\alpha\vdash\Phi\;\Rightarrow\;\alpha\vdash\square\Phi$ |
| **H4** | HB (2) | $\alpha\vdash\bigl(\square\Phi\land\square(\Phi\supset\Psi)\bigr)\supset\square\Psi$ — **uniformly in $\Phi,\Psi$** |
| **H5** | HB (3) | $\alpha\vdash\square\Phi\supset\square\square\Phi$ |
| **H6** | Π₁-strength | $\alpha$ proves all $\Pi^-_1$ theorems of PA (`Willard2001` Thm A.1) |
| **H7** | Internal propositional packaging | $\alpha$ proves the propositional / first-order steps listed as lemmas in §3.2 |
| **H8** | External consistency | $\alpha\nvdash\bot$ |

**Definition 3.1.** A *Lawvere–HB G2-situation* satisfies H0–H7.

H4 is stated with Willard's uniformity: `Willard2001` Thm A.1 says "Suppose
**for any two sentences** $\Phi$ and $\Psi$" (VERIFICATION.md, img at p. 44).
The TR's breach is exactly the failure of that uniformity
(`Willard1993-TR` printed p. 12, img).

### 3.2 Packaging lemmas (making H7 finite and checkable)

Write $\mathrm{Con}:=\neg\square\bot$. Assume (FP):
$\alpha\vdash G\leftrightarrow\neg\square G$, from Theorem 2.2 with
$\mathcal{E}(x)=\neg\mathrm{Der}(x)$.

**Lemma 3.2.1. [H7, FP]**
$\alpha\vdash G\supset\neg\square G$ and $\alpha\vdash\neg\square G\supset G$. ∎

**Lemma 3.2.2. [H7, FP]**
$\alpha\vdash\square G\supset\neg G$.

*Proof.* From (FP), $\alpha\vdash\square G\supset\neg(\neg\square G)$. But
$\alpha\vdash\neg\square G\leftrightarrow G$, so
$\alpha\vdash\square G\supset\neg G$. ∎

**Lemma 3.2.3. [H3, H4, H7]**
If $\alpha\vdash\Phi\supset\Psi$, then $\alpha\vdash\square\Phi\supset\square\Psi$.

*Proof.* H3 gives $\alpha\vdash\square(\Phi\supset\Psi)$. H4 with
$\square\Phi$ and $\square(\Phi\supset\Psi)$ yields $\square\Psi$. ∎

**Lemma 3.2.4. [H3, H4, H5, H7, Lemmas 3.2.2–3.2.3]**
$\alpha\vdash\square G\supset\square\bot$.

*Proof.*
From Lemma 3.2.2, $\alpha\vdash\square G\supset\neg G$.
By Lemma 3.2.3, $\alpha\vdash\square\square G\supset\square\neg G$.
By H5, $\alpha\vdash\square G\supset\square\square G$.
Compose: $\alpha\vdash\square G\supset\square\neg G$.
From $\alpha\vdash\square G$ and $\alpha\vdash\square\neg G$, H4 applied to
$\square G$ and $\square(G\supset\bot)$ (since $\neg G$ is $G\supset\bot$)
gives $\alpha\vdash\square\bot$. Discharging the assumption
$\square G$ yields $\alpha\vdash\square G\supset\square\bot$. ∎

**Lemma 3.2.5. [H7, FP, Lemma 3.2.4]**
$\alpha\vdash\mathrm{Con}\supset G$.

*Proof.* Lemma 3.2.4 contraposes to
$\alpha\vdash\mathrm{Con}\supset\neg\square G$. With
$\alpha\vdash\neg\square G\supset G$, conclude
$\alpha\vdash\mathrm{Con}\supset G$. ∎

### 3.3 The theorem

**Theorem 3.2 (G2, Lawvere–HB form).**
Assume H0–H7. If $\alpha\vdash\mathrm{Con}$, then $\alpha\vdash\bot$.
Hence under H8, $\alpha\nvdash\mathrm{Con}$.

*Proof.*

**(L) [H0, H1, H2]** By Theorem 2.2, obtain (FP):
$\alpha\vdash G\leftrightarrow\neg\square G$. *(Sole use of Lawvere / Yanofsky.)*

**(1)–(5)** Lemmas 3.2.1–3.2.5 give $\alpha\vdash\mathrm{Con}\supset G$ and
$\alpha\vdash\square G\supset\square\bot$.

**(6) [H3]** Assume $\alpha\vdash\mathrm{Con}$. Then
$\alpha\vdash\square\mathrm{Con}$ and, from Lemma 3.2.5,
$\alpha\vdash\square(\mathrm{Con}\supset G)$.

**(7) [H4]** From $\square\mathrm{Con}$ and $\square(\mathrm{Con}\supset G)$,
H4 yields $\alpha\vdash\square G$.

**(8)** From (7) and Lemma 3.2.4, $\alpha\vdash\square\bot$, i.e.
$\alpha\vdash\neg\mathrm{Con}$. With $\alpha\vdash\mathrm{Con}$,
$\alpha\vdash\bot$.

**(9) [H8]** Therefore $\alpha\vdash\mathrm{Con}$ contradicts H8. ∎

**Corollary 3.3 (Factorisation).**
$$
(\mathrm{H0{+}H1{+}H2})
\;+\;
(\mathrm{H3{+}H4{+}H5{+}H6{+}H7})
\;\;\Longrightarrow\;\;
(\vdash\mathrm{Con}\Rightarrow\vdash\bot).
$$
Any single failure among H3–H7 blocks a tagged step. Failure of H1 blocks (L). ∎

**Corollary 3.4.** Theorem 1.4 alone does not entail $\nvdash\mathrm{Con}$.
G2 in this scheme is Theorem 3.2. ∎

**Remark 3.5 (Relation to Willard2001 Thm A.1).**
Theorem 3.2 is the Lawvere-factored form of Willard's Theorem A.1
(status `stated-only` in the registry; justification by reduction to classical
HB). H6 is Willard's Π₁-calibration; H3–H5 are his conditions (1)–(3)
verbatim; H1 is the diagonal half he isolates as needing PA-strength for the
fixed-point identity (`Willard2001` footnote on the fixed-point / HB split,
recorded in `refined-sjas.md` §2.1).

---

## 4. Alternate factorisations (declared, not primary)

**Jeroslow + □-contraction.** Fixed point $G\leftrightarrow\square(G\to\bot)$ plus
$\mathrm{copy}_\square$ yields formalized G2 (Beklemishev–Shamkanov; ALSJAS
`AbstractG2`). Different post-Lawvere package; Willard comparison uses §3's
register, not this one.

**Kreisel model-chain.** Yanofsky 2003 flags it; not developed here — Willard's
breach maps cleanly onto H4, not onto Kreisel's truth-path diagonal.

---

## 5. Affordable arrows and Willard's H4 failure

### 5.1 Ambient maps and affordability

**Definition 5.1 (Ambient category $\overline{\mathcal{A}}$).**
Objects: Gödel numbers of formulas / proofs (as $\mathbb{N}$ where convenient),
and the Lindenbaum objects $A,Y$ of H0. Morphisms: partial recursive
(arithmetically definable) maps on codes. Classical HB arguments are written
externally in $\overline{\mathcal{A}}$.

**Definition 5.2 (Affordable class $\mathrm{Aff}(\alpha)$).**
$\mathrm{Aff}(\alpha)$ is a **wide class of arrows** of $\overline{\mathcal{A}}$
(same objects; a designated subclass of morphisms). It is **not** assumed to be
a subcategory: composition of affordable arrows need not be affordable, because
that composition is essentially $\mathrm{mp}_\square$ (Definition 5.3), which
Theorem 5.5 removes. Membership:

- An arrow $m$ of $\overline{\mathcal{A}}$ lies in $\mathrm{Aff}(\alpha)$ when
  $\alpha$ proves the **uniform** existence / totality statement defining $m$.
- If $\alpha$ proves only *instances* for fixed closed terms $\bar c$, the
  instance arrows $m_{\bar c}:1\to Z$ may lie in $\mathrm{Aff}(\alpha)$ while
  the uniform $m$ does not.
- Identity arrows are affordable ($\alpha\vdash\forall x\,x=x$).

This is the categorical reading of Willard's fixed-constants vs uniform
quantification (`Willard1993-TR` printed p. 12, img; same pattern at
`SUBST_i(\bar k,y)` for fixed $\bar k$).

**Definition 5.3 (Uniform H4 arrow).**
$$
\mathrm{mp}_\square:
\;
\square\Phi \times \square(\Phi\supset\Psi)
\;\longrightarrow\;
\square\Psi
$$
in $\overline{\mathcal{A}}$ sends a pair of proof-codes $(x,y)$ to a
proof-code $z$ of $\Psi$ when such a $z$ exists. **H4 holds relative to
$\mathrm{Aff}(\alpha)$** iff $\mathrm{mp}_\square\in\mathrm{Aff}(\alpha)$,
i.e. iff $\alpha$ proves
$$
\forall\Phi\forall\Psi\;
\bigl(\square\Phi\land\square(\Phi\supset\Psi)\bigr)\supset\square\Psi.
$$

**Definition 5.4 (Degenerate / instance H4 arrows).**
For fixed closed proof-codes $\bar x,\bar y$, the instance arrow
$\mathrm{mp}_{\bar x,\bar y}:1\to\square\Psi$ may lie in $\mathrm{Aff}(\alpha)$
even when $\mathrm{mp}_\square$ does not.

### 5.2 The missing-arrow theorem for `IS(A)`

**Theorem 5.5 (H4 fails as a missing Aff-arrow).**
Let $\alpha=\mathrm{IS}(A)$ for nice $A$, in Willard's Type-A /
semantic-tableaux configuration. Then:

1. $\mathrm{mp}_\square\notin\mathrm{Aff}(\alpha)$ (uniform H4 fails);
2. for fixed constants $\bar x,\bar y$, instance arrows of Definition 5.4 may
   lie in $\mathrm{Aff}(\alpha)$ (degenerate H4 holds);
3. H1 holds for $\alpha$ (Group-3 / Lawvere fixed point is an axiom).

*Proof.*
(1)–(2) are Willard's statement, image-verified at `Willard1993-TR` printed
p. 12 (VERIFICATION.md; extraction `willard1993-tr.md` §3.7c):

> "For the case of `IS(A)`, the difficulty is that **only in the *degenerate
> case* where `x` and `y` are *fixed constants*** can `IS(A)` prove the
> Hilbert-Bernays condition that if `x` is a proof of $\alpha$ and $y$ is a
> proof of $\alpha\supset\beta$ then there exists some $z$ that proves $\beta$."

Under Definitions 5.2–5.4 that sentence is exactly (1)–(2). No inference beyond
the translation into those definitions is used.

(3) is Corollary 2.4. ∎

**Theorem 5.6 (Evasion mechanism).**
Relative to $\mathrm{Aff}(\mathrm{IS}(A))$, Theorem 3.2 fails at H4: Lemmas
3.2.3–3.2.4 and step (7) invoke $\mathrm{mp}_\square\notin\mathrm{Aff}(\alpha)$.
The Lawvere half (L) remains. Therefore
$\vdash\mathrm{Con}\Rightarrow\vdash\bot$ is not forced by Theorem 3.2
inside $\mathrm{Aff}(\mathrm{IS}(A))$.

*Proof.* Theorem 5.5(1) + Corollary 3.3. ∎

**Theorem 5.7 (Obligatory breach, Aff-form).**
Suppose $\alpha$ satisfies H0–H2, H6, H8 and $\alpha\vdash\mathrm{Con}$, with
the affirmative corpus theorems applying. Then $\alpha$ is not a Lawvere–HB
G2-situation relative to $\mathrm{Aff}(\alpha)$: at least one of H3–H5, H7
fails there. For Type-A `IS(A)`, the failed hypothesis is H4.

*Proof.* If H0–H7 all held relative to $\mathrm{Aff}(\alpha)$, Theorem 3.2 would
give $\alpha\vdash\bot$, contradicting H8. For `IS(A)`, Theorem 5.5 names H4. ∎

### 5.3 Filled hypothesis register for Type-A `IS(A)`

| Id | Step(s) | Status relative to $\mathrm{Aff}(\mathrm{IS}(A))$ | Provenance |
| --- | --- | --- | --- |
| H0 | ambient | holds | Setup |
| H1 | (L) | **holds** | Cor. 2.4; codified §5.1 |
| H2 | □ | holds | arithmetization in corpus |
| H3 | Lem. 3.2.3, Thm 3.2(6) | not the named breach | TR names condition (2) specifically |
| H4 | Lem. 3.2.3–3.2.4, Thm 3.2(7) | **fails (uniform)** | Thm 5.5; TR p. 12 img |
| H5 | Lem. 3.2.4 | not the named breach | — |
| H6 | Π₁ | **holds** by design | Thm A.1 calibration; TR Remark 7 |
| H7 | packaging | fails insofar as it invokes uniform H4 | Def. 5.2 |
| H8 | consistency | **holds** | TR Prop. 1 / 2001 Thm 3.4 (`full`) |

### 5.4 Margin as quantitative affordability

**Definition 5.8 (Margin; Willard2011 Def. 4.5, img).**
$M(\xi)=\inf_\beta\bigl(\mathrm{Log}(q_\beta)-\sharp(\beta)\bigr)$.

**Theorem 5.9 (Margin witnesses failure of the G2 composite).**
If $M(\xi)\ge 2$, then by `Willard2011` Lemma 4.6 (status `sketch`, proof
"kept brief and informal"; img at pp. 14–15) the configuration with its
self-referential axiom is a consistent self-justifying system. In the present
vocabulary: $M(\xi)\ge 2$ implies that the composite arrow of Theorem 3.2
producing $\bot$ from $\mathrm{Con}$ is not in $\mathrm{Aff}(\xi)$, while H1
and $\vdash\mathrm{Con}$ can hold.

*Proof content.* Eqs. (19)–(20) place the fixed point at margin exactly $1$;
demanding margin $\ge 2$ excludes every inconsistent $\beta$ from matching that
cost (`R3-the-margin.md` §6, page-image verified). That exclusion is the
absence of the contradiction-producing arrow from $\mathrm{Aff}(\xi)$. ∎

*Status note.* Lemma 4.6 is `sketch` in the registry; Theorem 5.9 inherits that
status for the quantitative half. Theorem 5.5 (uniform H4 failure for `IS(A)`)
does **not** inherit it — it rests on Willard's explicit statement, not on the
margin lemma.

---

## 6. Necessary conditions for self-justification

**Definition 6.1 (Self-justifying situation).**
A theory $\alpha$ with H0, H2 is *self-justifying* when:

| | Condition |
| --- | --- |
| (SJ1) | **H1** — Lawvere / Group-3 available |
| (SJ2) | **H8** — consistent |
| (SJ3) | $\alpha\vdash\mathrm{Con}$ — self-assertion of consistency |
| (SJ4) | at least one of **H3–H7** fails in $\mathrm{Aff}(\alpha)$ |

**Theorem 6.2 (Necessity of (SJ4)).**
Assume H0–H2, H6, and (SJ1)–(SJ3). Then (SJ4) holds.

*Proof.* Suppose not: H3–H7 all hold in $\mathrm{Aff}(\alpha)$. Then H0–H7 hold,
so Theorem 3.2 yields $\alpha\vdash\bot$, contradicting (SJ2). ∎

**Theorem 6.3 (Insufficiency of (SJ4) alone).**
(SJ4) does not imply (SJ3). Beklemishev–Shamkanov's system $S$ fails formalized
G2 (a post-Lawvere structural hypothesis in the §4.1 packaging) but does not
prove $\neg\square\bot$ (their §6, img in VERIFICATION.md). ∎

**Theorem 6.4 (Type-A instance).**
For $\alpha=\mathrm{IS}(A)$ (nice $A$), (SJ1) holds by Corollary 2.4; (SJ2) and
(SJ3) hold by Willard's Proposition 1 / Theorem 3.4 (`full`); (SJ4) holds by
Theorem 5.5 (H4 fails). Therefore $\mathrm{IS}(A)$ is self-justifying in the
sense of Definition 6.1, and the evasion mechanism is Theorem 5.6. ∎

**Theorem 6.5 (Conventional configurations are not self-justifying).**
If $\xi$ is a conventional configuration satisfying H0–H7 in
$\mathrm{Aff}(\xi)$ (`Willard2011` p. 21 fn. 11, img: "will satisfy the
Hilbert-Bernays derivability conditions… automatically inconsistent"), then
(SJ3) is incompatible with (SJ2) by Theorem 3.2. ∎

---

## 7. Completion audit

| Requirement | Evidence | Status |
| --- | --- | --- |
| Prove G2 using Lawvere/Yanofsky techniques | §3: Thm 2.2 (expanded) + Lemmas 3.2.1–3.2.5 + Thm 3.2 | **proved** |
| Named hypothesis register | §3.1 H0–H8; filled for Type-A in §5.3 | **done** |
| Translate Willard into that framework | §§5–6: Aff, missing Aff-arrow, SJ conditions | **proved** |
| Necessary conditions for self-justification | Def 6.1; Thm 6.2 (necessity of breach) | **proved** |
| Mechanism of G2 evasion (category-theoretic) | Defs 5.1–5.4; Thms 5.5–5.7 | **proved** |
| Type-A instance | Thm 6.4, citing corpus `full` consistency | **proved** (translation; consistency cited not re-derived) |
| Image-verified Willard H4 quote | VERIFICATION.md row TR p. 12 | **verified** |
| Image-verified Thm A.1 uniformity | VERIFICATION.md row 2001 p. 44 | **verified** |
| Re-prove Willard's consistency of `IS(A)` from scratch | out of scope (codification owns it) | **cited, not re-proved** |
| Kreisel secondary register | §4 | **deferred** (not required for the Main Theorem) |
| Mechanised Lean of Thm 3.2 | optional | **not required** for the Main Theorem |

**AAR.** The complete result stated in §0 is proved. Lawvere supplies only the
fixed point; Willard's Type-A evasion is the absence of the uniform H4 arrow
$\mathrm{mp}_\square$ from $\mathrm{Aff}(\mathrm{IS}(A))$, while H1 remains.
Self-justification is exactly (SJ1)–(SJ4). The quantitative margin (Thm 5.9)
is an additional sufficient witness at registry status `sketch` and is not
required for the Main Theorem's Type-A case, which rests on Willard's explicit
uniformity statement.

---

## 8. Three comparable presentations: AU, Beklemishev–Shamkanov, Willard

*Purpose.* Give categorical presentations of **(1) Beklemishev–Shamkanov** and
**(2) Willard's SJAS** in the same vocabulary as Joyal / van Dijk–Oldenziel, so
that the difference between “G2’s argument fails” and “the system is
self-justifying” is a difference of **which arrows / structural rules exist**,
not a difference of dialect.

### 8.0 Common interface (the Löb / G2 package)

A *G2-relevant situation* is data against which van Dijk–Oldenziel’s Löb
argument (their §6) can be stated or blocked:

$$
\mathcal{G}
\;=\;
\bigl(\mathcal{C},\;\square,\;\Delta_{\mathrm{FP}},\;
\mathrm{nec},\;\mathrm{mp}_\square,\;\mathrm{boxit},\;
\mathrm{copy}_\square,\;\mathrm{Con}\bigr)
$$

| Slot | Meaning | van Dijk–Oldenziel locus |
| --- | --- | --- |
| $\mathcal{C}$ | ambient of propositions / contexts / codes | AU $U$ (objects, monos into $1$, $\mathrm{Hom}$) |
| $\square$ | provability operator | $\square=\Gamma\circ R$ / $\mathrm{Hom}(1_0,-)$ |
| $\Delta_{\mathrm{FP}}$ | Lawvere / Gödelian fixed-point availability | Lemma 6.12 |
| $\mathrm{nec}$ | HBL (D1): $\vdash\varphi\Rightarrow\vdash\square\varphi$ | Prop. 6.5 |
| $\mathrm{mp}_\square$ | HBL (D2): internal □-MP | Prop. 6.8 |
| $\mathrm{boxit}$ | HBL (D3): $\square\varphi`\square\square\varphi$ | Prop. 6.6 |
| $\mathrm{copy}_\square$ | □-contraction: reuse a boxed assumption | implicit in AU structural rules; explicit in B–S Def. 3.7 / Rem. 3.9 |
| $\mathrm{Con}$ | consistency point / judgement $\neg\square\bot$ | Con-instance of Löb ($\varphi=\bot$) |

**Theorem 8.0 (Full package implies no consistent self-Con).**
If $\Delta_{\mathrm{FP}}$, $\mathrm{nec}$, $\mathrm{mp}_\square$, $\mathrm{boxit}$, and
$\mathrm{copy}_\square$ are all available in $\mathcal{C}$, then inhabited
$\mathrm{Con}$ yields inconsistency (van Dijk–Oldenziel Thm 6.13 with
$\varphi=\bot$; equivalently Theorem 3.2 plus the structural licence B–S isolate). ∎

*Note on “three HBL conditions”.* van Dijk–Oldenziel §6 prove Props. 6.5, 6.6,
6.8 (the three HBL rôles) inside the AU; they do not separately axiomatize
$\mathrm{copy}_\square$. Beklemishev–Shamkanov show that G2’s argument also
consumes that structural arrow. The common interface therefore carries **four**
post-diagonal resources, of which three are HBL and one is structural.

---

### 8.1 Joyal / van Dijk–Oldenziel (reference cell)

**Definition 8.1 (AU situation).** The initial arithmetic universe $U_0$ (and
its internal copy) realises $\mathcal{G}$ with every slot filled: Lemma 6.12
supplies $\Delta_{\mathrm{FP}}$; Props. 6.5–6.8 supply HBL; the AU’s ordinary
context discipline supplies $\mathrm{copy}_\square$. Affordance is total among
AU-constructible arrows.

**Consequence.** $\mathrm{Con}$ cannot be both inhabited and consistent
(Thm 8.0 / their §§5–6).

---

### 8.2 Categorical presentation of Beklemishev–Shamkanov

**Definition 8.2 (B–S situation).** A *Beklemishev–Shamkanov situation* is a
tuple
$$
\mathcal{B}
\;=\;
\bigl(\mathcal{S},\;\square,\;\Delta_{\mathrm{FP}},\;
\mathrm{L\ddot{o}b}_\square,\;\mathrm{copy}_\square,\;\mathrm{Con}\bigr)
$$
where:

1. **$\mathcal{S}$** is an abstract provability structure: a consequence
   relation on contexts with a modal operator $\square$ (B–S §§2–3).
2. **$\Delta_{\mathrm{FP}}$** asserts existence of Gödelian (and, in their toy,
   Henkinian) fixed points — the Lawvere half, kept.
3. **$\mathrm{L\ddot{o}b}_\square$** packages the Löb / HBL conditions on
   $\square$ as B–S state them for their Theorems 3–4 (the modal analogues of
   $\mathrm{nec}$, $\mathrm{mp}_\square$, $\mathrm{boxit}$).
4. **$\mathrm{copy}_\square$** is the structural rule
   $$
   \Gamma,\,\square\varphi,\,\square\varphi\;\vdash\;\psi
   \quad\Longrightarrow\quad
   \Gamma,\,\square\varphi\;\vdash\;\psi
   $$
   (□-contraction; Rem. 3.9), or the plain contraction of Def. 3.7.
5. **$\mathrm{Con}$** is the judgement $\Rightarrow\neg\square\bot$ (or
   $\neg\square\bot$ as a theorem).

**Definition 8.3 (copy as an arrow).**
In a category of contexts-and-derivations, $\mathrm{copy}_\square$ is the
arrow (natural in $\varphi$)
$$
\mathrm{copy}_\square:
\;
\square\varphi
\;\longrightarrow\;
\square\varphi\otimes\square\varphi
$$
(cf. ALSJAS `copyBox`; Jeroslow packaging). Its absence is the B–S breach.

**Theorem 8.4 (B–S toy $S$ as a B–S situation).**
Let $S$ be Beklemishev–Shamkanov’s contraction-free K4 with fixed-point
operators. Then the associated $\mathcal{B}$ has:

| Slot | Status in $S$ | Provenance |
| --- | --- | --- |
| $\Delta_{\mathrm{FP}}$ | **present** (Gödelian / Henkinian FPs, not unique) | B–S construction; cut-elimination |
| $\mathrm{L\ddot{o}b}_\square$ / HBL-on-$\square$ | as needed for their abstract setup; not the named breach | Thms 3–4 hypotheses |
| $\mathrm{copy}_\square$ | **absent** | contraction-free fragment; Rem. 3.9 |
| Formalized G2 | **fails** | cut-elimination |
| $\mathrm{Con}$ as theorem | **absent** — $S\nvdash\neg\square\bot$ | §6, p. 14, img |

*Proof.* Direct from the image-verified assessment in
`R2-beklemishev-shamkanov-assessment.md` §§1–2 and VERIFICATION.md rows for
B–S pp. 8, 14. ∎

**Theorem 8.5 (What B–S blocks, categorically).**
In $\mathcal{B}$ for $S$, Theorem 8.0 fails because $\mathrm{copy}_\square$ is
missing. Hence formalized G2 does not go through, even though $\Delta_{\mathrm{FP}}$
holds. This does **not** entail that $\mathrm{Con}$ is a theorem of $S$. ∎

**Corollary 8.6 (B–S ≠ self-justifying).**
A B–S situation with $\Delta_{\mathrm{FP}}$, no $\mathrm{copy}_\square$, and
uninhabited $\mathrm{Con}$ realises “G2’s argument fails” without realising
self-justification (Def. 6.1). ∎

---

### 8.3 Categorical presentation of Willard’s SJAS

**Definition 8.7 (Willard situation).** A *Willard situation* is a tuple
$$
\mathcal{W}
\;=\;
\bigl(\overline{\mathcal{A}},\;\mathrm{Aff}(\alpha),\;\square,\;
\Delta_{\mathrm{FP}},\;
\mathrm{nec},\;\mathrm{mp}_\square,\;\mathrm{boxit},\;
\mathrm{copy}_\square,\;\mathrm{Con}\bigr)
$$
extending Def. 8.0’s interface by the affordance cut of §5:

1. **$\overline{\mathcal{A}}$** — ambient codes / Lindenbaum (Def. 5.1),
   comparable to $\mathrm{Ext}(U_0)$.
2. **$\mathrm{Aff}(\alpha)$** — affordable arrows (Def. 5.2): those whose
   *uniform* existence statement is a theorem of $\alpha$.
3. **$\Delta_{\mathrm{FP}}$** — Lawvere / Group-3 (H1; Lem. 6.12 analogue).
4. **$\mathrm{nec},\mathrm{mp}_\square,\mathrm{boxit}$** — HBL arrows; they
   “hold in Aff” iff they lie in $\mathrm{Aff}(\alpha)$.
5. **$\mathrm{copy}_\square$** — retained on the Type-A / classical-tableaux
   line (not Willard’s named breach).
6. **$\mathrm{Con}$** — Group-3’s consistency assertion as a global point.

**Definition 8.8 (HBL-internal).**
$\mathcal{W}$ *satisfies HBL in Aff* when
$\{\mathrm{nec},\,\mathrm{mp}_\square,\,\mathrm{boxit}\}\subseteq\mathrm{Aff}(\alpha)$.

**Theorem 8.9 (Type-A Willard).**
For $\alpha=\mathrm{IS}(A)$,

| Slot | Status in $\mathrm{Aff}(\alpha)$ | AU / B–S analogue |
| --- | --- | --- |
| $\Delta_{\mathrm{FP}}$ | **present** (Group-3) | Lem. 6.12 / B–S FP |
| $\mathrm{nec}$ (D1) | not the named breach | Prop. 6.5 |
| $\mathrm{mp}_\square$ (D2) | **absent** | Prop. 6.8 fails as Aff-arrow |
| $\mathrm{boxit}$ (D3) | not the named breach | Prop. 6.6 |
| $\mathrm{copy}_\square$ | **present** (classical context reuse) | B–S’s *kept* resource |
| $\mathrm{Con}$ inhabited | **yes** | would force inconsistency in $U_0$ |
| consistency ($0\neq 1$) | **yes** | AU Def. 4.12 |

*Proof.* Theorems 5.5–5.6 and Corollary 2.4; Willard1993-TR p. 12 img for the
D2 uniformity failure. ∎

**Theorem 8.10 (What Willard blocks, categorically).**
In Type-A $\mathcal{W}$, Theorem 8.0 fails because $\mathrm{mp}_\square\notin\mathrm{Aff}(\alpha)$.
Löb/G2 do not fire. Unlike B–S, $\mathrm{Con}$ is inhabited and consistency is
retained, so Def. 6.1 (self-justifying) holds. ∎

---

### 8.4 The identified differences

**Theorem 8.11 (Three-way comparison).**

| | $\Delta_{\mathrm{FP}}$ | HBL in Aff | $\mathrm{copy}_\square$ | $\mathrm{Con}$ + consistent |
| --- | --- | --- | --- | --- |
| **AU $U_0$ (vDO)** | yes | **all three** | **yes** | **impossible** |
| **B–S $S$** | yes | (modal Löb package as stated) | **no** | **no** ($\nvdash\neg\square\bot$) |
| **Willard $\mathrm{IS}(A)$** | yes | **fails D2** ($\mathrm{mp}_\square$) | **yes** | **yes** (self-justifying) |

*Proof.* Assemble Defs. 8.1, 8.2, 8.7 and Thms 8.4, 8.9; B–S §6 img for the
Con column of row 2; Willard affirmative corpus for row 3. ∎

**Theorem 8.12 (Why SJAS is self-justifying and B–S is not).**
Both B–S and Willard remove a post-diagonal resource required by Theorem 8.0,
hence both evade G2’s *argument*. They remove **different** resources:

1. **B–S** removes $\mathrm{copy}_\square$ (structural). Formalized G2 fails;
   $\mathrm{Con}$ is not obtained as a theorem (Cor. 8.6).
2. **Willard Type-A** removes $\mathrm{mp}_\square$ from Aff (derivability /
   affordance). G2 fails *and* Group-3 supplies inhabited $\mathrm{Con}$ while
   consistency is preserved (Thm 8.10; Def. 6.1).

Therefore: evasion of G2 is necessary for self-justification but not
sufficient; the missing arrow’s identity, together with whether $\mathrm{Con}$
is inhabited in Aff, distinguishes the two cells. ∎

**Corollary 8.13 (Non-identification).**
$\mathrm{copy}_\square\notin\mathcal{B}$ is not the same failure as
$\mathrm{mp}_\square\notin\mathrm{Aff}(\alpha)$. The former is a rule of the
consequence relation on contexts; the latter is failure of uniform internal
modus ponens for $\square$ as an Aff-arrow. (`R2-…assessment.md` §2.3.) ∎

### 8.5 What this is not

- Not a claim that $\mathrm{IS}(A)$ or $S$ is an arithmetic universe.
- Not a claim that dropping plain contraction (affine object logic) yields the
  B–S cell — □-contraction can survive in affine PA (B–S §3; obligation RO1).
- Not a re-proof of Willard’s consistency theorems or of B–S cut-elimination;
  those are cited at their registry / image-verified status.

---

## References

- Lawvere, F. W. (1969/2006). TAC Reprints 15.
- Yanofsky, N. S. (2003). BSL 9(3); arXiv:math/0305282.
- Yanofsky, N. S. (2022). *Working Category Theorist*, Ch. 6.
- van Dijk, J., and Oldenziel, A. G. (2020). *Gödel incompleteness through
  Arithmetic Universes after A. Joyal.* arXiv:2004.10482. §6 Props. 6.5–6.8,
  Lemma 6.12, Thm 6.13. Held at `../../lit/2004.10482.pdf`.
- Beklemishev, L., and Shamkanov, D. (2016). *Some abstract versions of Gödel’s
  second incompleteness theorem based on non-classical logics.* arXiv:1602.05728.
  Def. 3.7, Rem. 3.9, Thms 3–4, §6. Assessed in
  `R2-beklemishev-shamkanov-assessment.md`; quotes in VERIFICATION.md.
- Willard1993-TR printed p. 12 (H4 uniformity failure); Willard2001 Thm A.1
  p. 44; Willard2011 Def. 4.5 / Lemma 4.6 — as in VERIFICATION.md.
- `refined-sjas.md`, `R3-the-margin.md`, ADR-0002, ADR-0004.
