# The essence of self-justification, revised after the Löb paper

Date: 2026-10-29

Revises the [2026-10-28 note](2026-10-28-essence-of-self-justification.md).

## Request (verbatim)

> This paper should inform your answer to my question:
> https://jasongross.github.io/lob-paper/nightly/lob.pdf

The question is the one from 2026-10-28: "In your own words, try to articulate
the essence of self-justification, in a way as generally applicable as
Lawvere's Fixed Point Theorem."

## Source

J. Gross, J. Gallagher, B. Fallenstein, "Löb's Theorem: A functional pearl of
dependently typed quining" (ICFP '16 draft, nightly build of 2016-03-19).
All 13 pages were read from page images. The passages used:

- §2: "self-reference with truth fails to terminate"; the `Tarski` program
  loops. Table 1 pairs □X → X with interpreters.
- §3, end: *locality*. "the interpretation of any given constructor does not
  require doing case analysis on any of its arguments".
- §6: `Löb : □ (‘□’ X ‘→’ X) → □ X` with ⟦ Löb f ⟧ᵗ = ⟦ f ⟧ᵗ (Löb f). A local
  explanation makes Löb consistent to add.
- §7: `soundness x = ⟦ x ⟧ᵗ`; `incompleteness : ¬ □ (‘¬’ (‘□’ ‘⊥’))` is `löb`;
  `no-interpreters interp = löb (interp {‘⊥’})`. Also: "you might be able to
  prove that your syntax has no proofs of ⊥ (by interpreting any such proof).
  But you'll be unable to quote that proof."
- §8: Löb derived from `Quine` plus internal quotation and application of a
  variable:
  `‘□‘H’→□‘X’’ = ‘λ’ (w ⌜‘fromH’⌝ᵗ w‘‘’’ₐ ‘VAR₀’ w‘‘’’ₐ ‘⌜‘VAR₀’⌝ᵗ’)`,
  `‘h’ = ‘toH’ ‘’ₐ (‘f’ ‘∘’ ‘□‘H’→□‘X’’)`,
  `Löb = ‘fromH’ ‘’ₐ ‘h’ ‘’ₐ ⌜‘h’⌝ᵗ`. The quine and its isomorphisms are
  interpreted locally (⟦ Quine φ ⟧ = ⟦ φ ⟧ (Quine φ); `quine→`, `quine←` are
  identities).
- §10: Löb from `repr`, Σ types, function types, and decidable equality on
  contexts.
- Footnote 2: the F_ω self-interpreter (Brown and Palsberg) has no separate
  syntax for types, so Löb's type there is either not strictly positive or
  a general fixpoint.
- Introduction: the first listed application is "an induction rule for program
  semantics involving a 'later' operator (Appel et al. 2007)".
- §11, third moral: "If believing in something is enough to make it true,
  then it already is."

## The articulation

**Self-justification is trust by well-founded recursion.**

Syntax can be given meaning in two ways: handed over, or run. Löb's theorem
hands it over. In §6, ⟦ Löb f ⟧ = ⟦ f ⟧ (Löb f): `f` receives, uninterpreted,
the syntax of the very term it sits in. That is safe whenever the other
constructors have local explanations.

The trust axiom runs syntax. It is the interpreter at ‘⊥’, an inhabitant of
‘¬’ (‘□’ ‘⊥’): the type that §7's `incompleteness = löb` empties. Its
explanation cannot be local, because it has to call the whole interpreter.
That is safe when the call descends: everything trust is asked to run must be
strictly smaller than the term asking.

Each is consistent alone. Together they are §2's loop:
⟦ Löb con ⟧ = ⟦ con ⟧ (Löb con) = ⟦ Löb con ⟧ = ….

§7 says you can prove your syntax has no proofs of ⊥ by interpreting them, but
cannot quote that proof. A self-justifying system quotes it: `con` is
`soundness` brought inside. It survives because, inside, the interpreter only
ever runs proofs smaller than the one running it. The 2026-10-28 note's
"infinite descent" is this recursion's termination proof.

## The theorem: the trust-side counterpart of §6

Read each type at a level n drawn from a well-founded order, with ‘□’ A at
level n meaning syntax of A below n. An explanation is **guarded** if every
constructor's clause at level n is local, except that it may call the whole
interpretation at levels below n.

> **Theorem.** If a syntax has a guarded explanation, adding
> `con : ‘¬’ (‘□’ ‘⊥’)` with ⟦ con ⟧ₙ s = ⟦ s ⟧_{size s} keeps it consistent.
>
> *Proof.* Strong induction on n. The `con` clause is the induction
> hypothesis.

Löb's clause is local but unguarded. At level n = size (Löb f) it needs
Löb f to lie below its own size.

So the paper's theorem and this one differ in a single line, the reading of
‘□’:

```
paper §6:  ⟦ ‘□’ T ⟧  = Term T                         -- Löb's clause is fine
guarded:   ⟦ ‘□’ T ⟧ₙ = { t : Term T // size t < n }   -- trust's clause descends
```

Under the first reading, trust's clause is an unbounded call to the
interpreter. Under the second, Löb's clause fails its bound.

Two instances:

- **Willard (arithmetic).** The 2026-10-28 theorem (true universal axioms + T
  + guard ⟹ consistent) is this theorem for IS(A). The level is the Gödel
  number and `con` is T. The least-refutation argument is the strong
  induction. The guard is Willard's growth bound, enforced by arithmetic
  weakness rather than by leaving constructors out.
- **λᶜᵉʳᵗ₀ (calculus).** T1, checked in Ansatz (2026-10-06 note, §4), is the
  same theorem for that calculus. Its budget-stratified model (Lemma 3.6) is
  the level-indexed reading, and `reflect`'s bound m < ‖v‖ is the guard. The
  2026-10-06 note already said that "a certificate cannot be held by the
  program it certifies". The paper adds the contrast class: Löb's explanation
  is local, and trust's cannot be.

## The check

[`2026-10-29-guarded-trust.lean`](2026-10-29-guarded-trust.lean) uses Lean 4
core, v4.32.1, and needs no Mathlib. It was checked with
`lean 2026-10-29-guarded-trust.lean`.

- **Guarded syntax `Tm`.** It has λ, application, ⊤, ⊥, quotation of closed
  terms (D1), `con`, and the Gödel sentence G ≅ (‘□’ G ‘→’ ‘⊥’) as a naming
  fixed point. G is the paper's ‘H’ at ‘X’ = ‘⊥’. The content is two lines:

  ```lean
  | .box A => { t : Tm [] A // size t < n }        -- ‘□’ A at level n
  | _, _, .con, _, _ => fun ⟨s, hs⟩ => below (size s) hs s (Nat.le_refl _)
  ```

  `soundness : Tm [] .bot → False` checks, using propext and Quot.sound.
  `selfJustification : Tm [] (.arr (.box .bot) .bot) := .con`.
- **Negative checks.** Each was run on an edited copy, and each failed where it
  should:
  1. Dropping the size bound from ‘□’: the `con` clause no longer typechecks.
  2. Adding internal quotation of a variable: `size s + 1 < n` does not follow
     from `size s < n`.
  3. Adding the paper's `Löb` with the paper's clause: `size f + 1 < n` does
     not follow from `size f + 1 ≤ n`.
- **Unguarded syntax `TmU`.** The same `con` proves ⊥ in two ways:
  - `viaLob = lob con`;
  - `viaQuine`, which is §8's construction with ‘f’ := `con` and ‘X’ := ‘⊥’.

  `no_sound_reading` uses no axioms.

## What the paper pins down

**Where Löb needs self-simulation.** In §8 the only step that builds syntax
from a variable is ‘□‘H’→□‘X’’. It uses its variable twice, once raw and once
quoted, under internal application: D2 and D3 at a variable.

Trace the paper's own interpretation through the final term,
‘fromH’ ‘’ₐ ‘h’ ‘’ₐ ⌜‘h’⌝ᵗ. Given ⌜‘h’⌝ᵗ, that λ returns the syntax
‘fromH’ ‘’ₐ ‘h’ ‘’ₐ ⌜‘h’⌝ᵗ, which is the term itself, and hands it to ‘f’. So
the derived Löb is the §6 primitive.

Applying h to its own quotation is harmless: it is D1, and `Tm` admits it
(`app (unfold h) (quo h)` is guarded). What a guarded system cannot do is
rebuild, from that quotation, the term it sits in. That is the 2026-10-28
"evidence diagonal" with a concrete term attached. It is also why IS(A) keeps
modus-ponens closure only "in the degenerate case where x and y are fixed
constants" (TR 93-10, p. 12). At constants, D2 is D1 of an application, which
is guarded.

**Three exits.** Trust plus Löb is ⊥, so a system gives up one of three things:

- **Trust.** The paper's languages: Löb is admissible and ‘¬’ (‘□’ ‘⊥’) is
  empty.
- **Naming.** Brown–Palsberg's F_ω has a self-interpreter at every type. It has
  no syntax for its own types (footnote 2), so there is no type-level quine to
  feed that interpreter. Davies–Pfenning's λ□ exits the same way: its code is
  typed, not data with a checker (2026-10-06 note, §5).
- **Self-simulation.** Willard's systems and `Tm` keep the Gödel sentence and
  trust at ⊥, and lose the step above.

**Lawvere, located.** Lawvere's diagonal is what builds the paper's `Quine`;
§10 builds it from `repr`. Its clause, ⟦ Quine φ ⟧ = ⟦ φ ⟧ (Quine φ), is local:
it hands over a type and never runs it. That is why naming is free.

**The modal reading, made literal.** The guarded reading is a step-indexed
model of the system's own provability. Its soundness proof is Löb's rule for
"later", the first application on the paper's list. Trust excludes the other
reading, Löb's rule for provability.

**The third moral, counterpart.** "If believing in something is enough to make
it true, then it already is." The counterpart: believing in yourself is safe
as long as you only ever vouch for proofs smaller than yourself.

## Status

- **Checked in Lean.**
  - The trust-side theorem for `Tm`.
  - The corollary's type-theoretic form: naming + D1 + D2 and D3 at a variable
    + `con` ⟹ ⊥.
- **Paper (Agda).** §6–8 are taken as the authors report them; the artifact was
  not rerun.
- **Reading, not theorem.** `Tm`'s guard is qualitative: no constructor hands
  on more syntax than it consumed. Willard's guard is quantitative: a step adds
  at most a bit, and the encoding charges more. That they are the same guard is
  this note's reading, not a theorem.
- **Arithmetic.** The arithmetic corollary (Σ₁-complete base) is still a
  translation, not checked. The guard in Willard's systems rests on his `full`
  proofs: `Willard2001` Thm 4.3; `Willard2005` Thms 1, 2 and 4.
- **Open.** The converse: every consistent self-justifying configuration with
  an honest proof predicate is guarded. The corpus's nearest claim
  (`Willard2005` Remark 4) still rests on `Willard2002a` Thm 6.4, a `sketch`.

## Possible uses

- SJexM, "Refactoring": the sentence "What costs something is self-simulation"
  can now point to a term (§8's ‘□‘H’→□‘X’’) and a two-line checked
  counterpart.
- λᶜᵉʳᵗ₀: the draft's informal argument that LP's free `·` and `!` cannot
  coexist with internal uniform reflection (2026-10-06 note, §5) has a
  checked toy instance in `viaQuine`, with `·` = `iapp` and `!` = `iquo`.
  Whether it transfers to λᶜᵉʳᵗ₀'s own syntax is unchecked. The same note's
  recommended "no certified Ω" theorem is, in `Tm`, `soundness` itself.
