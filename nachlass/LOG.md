# Nachlass Log

## 2026-09-04 - Adversarial round seventeen: the two engines do not split by line

One finding, and it is a repair introducing a fresh error of its own kind - the
third time this session.

Round five corrected section 6's claim that "every consistency-preservation
proof in the corpus" is a minimal-counterexample argument, and correctly
identified Willard2005 Theorem 1 as the separate theta-compactification engine.
But the replacement filed the least-number method under **"the Hilbert line"**,
citing Willard2001 Theorems 3.4 and 4.3 - and **Theorem 4.3 proves consistency
preservation for IS(A)**, which is a *semantic tableaux* system (registry
SJAS-Origin, codified section 3.1's Type-A row).

So a tableaux system's preservation is proved by minimal counterexample in 2001
and covered again by the apparatus-neutral engine in 2005. The division is
between **proof methods**, not between the two families of system. Codified
section 6.9's title, "The Hilbert line's engine", is looser than its own table,
which lists IS(A) among the four.

Fixed in section 6, in R3 section 1.1, and in the correction record that had
asserted it - the entry for the round-five repair now carries its own
withdrawal.

The pattern is now well attested: round eight's anchor repair introduced a wrong
codified section number, round sixteen's four-row table broke the row reference
in the next paragraph, and round five's engine repair mis-assigned the method.
A repair is an edit like any other and needs the same check the original claim
got.

audit.sh green.

## 2026-09-04 - Adversarial round sixteen: the provenance records were mis-dated

An end-to-end read of refined-sjas.md, hunting the one class the kind-organised
sweeps keep missing - a claim inconsistent with its own document - plus an audit
of this log's own bookkeeping. Seven findings.

**Section 2.1 said Willard "uses Kleene's recursion theorem directly".** He
cannot. TR 93-10 printed p. 37: IS(A) "will clearly be too weak to prove that
for-all x exists y SUBST_i(x,y)", and proves only exists-y SUBST_i(k-bar,y)
"for any fixed integer k-bar" - the recursion theorem in general form is exactly
what is not internally available, and the fixed-numeral instance is what defines
Group-3. This is R6 section 4.1's two-levels distinction, which section 2.1 had
elided even after today's other repairs.

**Section 2.5b listed three ingredients where R3 section 5.3 lists four**,
folding Pakhomov's semantic step into the derivability row. Fixing that promptly
broke a cross-reference in the next paragraph ("the third row" became row (iv)),
which the end-to-end read caught two paragraphs later - the same fix-introduces-
defect pattern as round eight's anchor.

**And the provenance records were mis-dated.** A system notice early in the
session said the date was 2026-09-02; it later became 2026-09-04, and I carried
the stale date through eight commits. Nine entries in this log, ten pass
headings in VERIFICATION.md, and five registry and concordance entries claimed
2026-09-02 for work whose commits are timestamped 2026-09-04. Corrected against
git log, which is the only authority available for it. For a project whose whole
discipline is provenance, dating its own corrections wrongly is not a small
thing.

**VERIFICATION.md's passes were out of order** - the third preceded the second,
because insertions anchored on different strings.

**And round nine had no entry in this log.** Commit 8199a22 went in without one.
The log's own per-round structure is what made the gap visible; the entry is now
written and inserted in sequence, marked as back-filled.

Also, R-F caught my own new correction entry for citing bare filenames - the
fifth time a check has flagged the record of the defect it checks for, and the
first time I applied the documented remedy (plain-text naming) instead of
discovering the interaction afresh.

audit.sh green.

## 2026-09-04 - Adversarial rounds fourteen and fifteen: R6 read in full; R-F's blind spot

**Round fifteen finished reading R6** - sections 2 and 4.2-4.3, the last
unread material in the stage - and checked its remaining claims. All correct:

- Theorem 1.1's derivation. The contraction use falls exactly where Four gives
  Box-G -> Box-Box-G and BoxMP must then consume Box-G a second time; that is
  the same step Beklemishev-Shamkanov Proposition 3.8 identifies. R6 also marks
  the theorem a sufficient package, not a biconditional.
- Section 4.2's quantifier equivalence between the witness form of HBL (2) and
  the terminal-fibre BoxMP entailment.
- Section 4.3's rendering of Willard2001 footnote 16's
  Theta(z) := forall x,y (Subst(z,x) -> not Derive(x,y)) - verbatim against the
  page image - and its citation of TR printed p. 37 for the SUBST_i
  fixed-versus-uniform split, which the extraction record confirms word for
  word ("Point of Clarification").

R6 carries Willard2001 Theorem A.1's stated-only status itself. And every
"Theorem A.1" reference in the stage is attributed to its paper, which matters
more than it looks: Willard1993-TR's Theorem A.1 (majorization, full) and
Willard2001's (generalized Hilbert-Bernays, stated-only) are different theorems
with different statuses.

**One finding, and it is R-F's blind spot.** Seventeen references were written
as bare filenames - the audit scripts, the results registry, the codified
statement, the nachlass log, the repo README and org file - each of which
resolves to nothing from refinement/. Round ten had fixed exactly this class for
slashed paths and built R-F to catch it; R-F's pattern required a slash, so it
never saw a bare filename. Extended and re-tested: it now checks 59 paths where
it checked 20.

**And a standing interaction, now recorded because it has happened four times.**
Every check that scans prose eventually flags VERIFICATION.md itself, because
the corrections table names the defective artifact it records - a stripped box,
an elided path, a retracted sentence, a bare filename. R-C, R-D and R-F have
each been caught by it. The two remedies in use are a line-local guard word and
naming the artifact in plain text rather than backticks. Written down so the
next check chooses one deliberately instead of discovering it as a failure.

audit.sh green.

## 2026-09-04 - Adversarial round thirteen: a contradiction that survived eight rounds

Two findings, both in R3 section 5.3, and both instructive.

**"Covering all five of Willard's dials."** That sentence sat in section 5.3
while sections 3.3, 3.4, 4 and 6 of the same document said three of five, with
Level(n) retracted and Hybrid(H) undetermined. It survived rounds five through
twelve because every sweep I ran was organised by *kind* - counts, then
cross-references, then quotations, then statuses, then anchors - and a claim
that is internally inconsistent with its own document falls between those
categories. It was caught this round only because a status check flagged the
adjacent sentence and I read the paragraph around it.

**And the adjacent sentence was my own fresh rule-2 violation.** The passage I
added in round eleven, about the selected G2 diagonal, cites Willard2001 Theorem
A.1 without its stated-only status - the exact rule round seven had swept the
whole corpus for. Introduced while writing carefully about care, six rounds
after the sweep, with the informational check R-E running.

Both are now fixed. The lesson is the one this session keeps repeating in
different forms: a check that has never been seen to fail should not be
believed, and a document is not consistent because its parts were each checked.

audit.sh green.

## 2026-09-04 - Adversarial round twelve: R6 reviewed against its sources

Read R6's sections 0-6 and checked its claims against the papers rather than
taking them on trust. **R6 holds up well** - better, on the discipline this
session has been enforcing, than refined-sjas.md did.

Spot-checks, all correct:

- Willard2001 Theorem A.1's footnote 16 does supply the selected diagonal
  Theta(N) <-> not-Der(quote Theta(N)) from Pi^-_1 strength, which is what R6
  section 0 claims and what separates it from a universal fixed-point theorem.
- Willard1993-TR Proposition 1 is full, and R6 carries that status in its table.
- Beklemishev-Shamkanov Lemma 5.1 is the weakening rule, as section 3.2 says;
  Remark 3.9, the affine-PA caution, and the HBL (2) reading all match the
  image-verified record.
- R6 carries proof statuses at each point of use, including "the paper calls
  this easy from cut-freeness and does not display a proof" for Proposition 4.1
  and the AU source's sketch-level section 6 Lob derivation. That is rule 2
  observed, in the component that never had to be told.

**One completion gap: Pakhomov is absent from R6, undeclared.** R1-R3 establish
H_<omega as one of only two demonstrated routes to a self-verifying theory, and
R6's section 5 square compares AU, B-S and Willard only, closing section 0 with
a contrast that reads as a claim about the field. I have not tried to supply the
missing row - it would need a categorical presentation of a set theory whose
witness is a finite model on the superexponential cut, and no Lawvere-side
treatment of that is held. Recorded as a declared scope limit instead, in
lawvere-sjas.md section 6 and ADR-0004's status.

**And R6's square is sharper than refined-sjas section 2.5b's table.** Two
independent axes - structural diagonal on boxed hypotheses, uniform internal HBL
evaluation - plus a separate fixed-point-strength axis, with the warning that
they "should not be collapsed into a single weakness ordering". Section 2.5b now
points at it.

audit.sh green.

## 2026-09-04 - Adversarial round eleven: the review had not covered the whole stage

Chasing a strange-looking expression in the quotation register - `Box = Gamma-R`,
attributed to "van Dijk-Oldenziel", a paper I had never read - turned up
something larger than a bad row.

**There is a sixth component, and twelve passes of review had never opened it.**
lawvere-sjas.md (642 lines) and ADR-0004 (138 lines) are R6, the categorical
comparison of AU incompleteness, Beklemishev-Shamkanov and Willard SJAS,
complete as of today. My checks R-A through R-F do cover those files - they glob
the whole directory - but my *reading* had been scoped to refined-sjas.md and
its three immediate siblings throughout. The register row was legitimate; my
picture of the stage was not.

**And R6 corrects the main statement on a point that matters.** Its section 1.3
separates two selected diagonals that refined-sjas section 2 had merged:

  G2 diagonal:              G <-> not-Box-G          (what the limitative
                                                      argument consumes)
  self-consistency diagonal: H <-> Con(B + H, D)     (how a self-referential
                                                      consistency assertion is
                                                      formed)

"Neither instance by itself implies the other." Willard's Group-3 axiom is H,
not G. Section 2 had said "they contain the fixed point as an axiom", singular,
which reads as though his systems contain the Godel sentence. They do not.
Sections 2 and 2.5b now carry the distinction.

**Section 9's Lawvere paragraph was stale in an interesting way.** It said the
framing "does not give a categorical proof of any Willard result ... a research
direction, not a construction" - and ADR-0004 quotes exactly that sentence as
the gap R6 was written to fill. R6 has since built the construction: a common
coded modal proof doctrine, the three systems presented in it, and the missing
morphism identified in each case. Its Theorem 1.1 adds a coordinate this
document did not have - formalized G2 needs, beyond a fixed point and the three
HBL conditions, a boxed diagonal copy_A : Box-A -> Box-A tensor Box-A, and
B-S's Lindenbaum construction is symmetric monoidal rather than cartesian
precisely there. Section 9 now records that, together with R6's own list of
non-claims.

ADR-0002's status line also still read "R1 done...; R4-R5 pending" with no
mention of R6, though its own component table has an R6 row marked complete.

Checked and found consistent: R6's account of B-S (satisfies L1-L3, lacks the
boxed diagonal) agrees with refined-sjas section 2.5 and R2 section 2.3; its
identification of Willard's Type-A breach as HBL (2) agrees with section 2.3;
its limits section carries the AU source's sketch-level section 6 Lob
derivation; and its declared witness lit/2004.10482.pdf is held.

audit.sh green.

## 2026-09-04 - Adversarial round nine: B6 declared met while rounds seven and eight falsified it

*(Entry written 2026-09-04 in round sixteen; the round-nine commit 8199a22 went
in without one, which the log's own per-round record made visible.)*

The charter's own bookkeeping was the last thing unaudited.

**B6 was asserted met at exactly the moment rounds seven and eight were breaking
it.** B6 requires every quotation image-verified and transcribed verbatim, and
every corpus claim status-carrying. Round seven found the status half broken in
ten places, including Willard2001 Theorem A.1 and Willard2011 Lemma 4.6. That is
the second time B6 has been declared satisfied and then falsified - the first
was the non-verbatim quotations of corrections 23 and 32.

Neither half of B6 is machine-checkable: R-B tests register hygiene, not
fidelity, and R-E is informational because no positional heuristic captures "at
the point of use". B6 now records both falsifications and says that any future
assertion that it is met is a claim requiring a fresh sweep, not a standing
fact.

The charter's "what remains open" list also omitted the two largest open items
found this session - that the margin's sufficiency rests on a sketch, and that
R5 is unstarted with Pudlak unheld - and the status line still said "reviewed
twice" after ten passes.

audit.sh green.

## 2026-09-04 - Adversarial round ten: a stripped box inside the document that records box-stripping

Four findings, and the first is almost comic.

**R2 still had a box-stripped quotation - in the document whose correction #3
records box-stripping.** R2 section 2.3 described Proposition 3.8's decisive step
as "the step from Gamma |- phi and Gamma |- not-phi to Gamma |- not-top". The
page image has boxes on all three: Gamma |- box-phi, Gamma |- box-not-phi,
therefore Gamma |- box-not-top. And the boxes are the entire point - under
boxtimes-phi := box(phi -> bottom), box-not-top IS boxtimes-top, the formalized
inconsistency assertion, which is exactly why dropping contraction costs only
*formalized* G2. refined-sjas section 2.5 had it right; R2 did not. I had edited
that passage twice this session without checking it against the image.

**A citation to a file that does not exist.** R2 section 3 pointed at
docs/design/affine-tree-sjas.md as "the C4 pilot's fidelity target". It was an
untracked working-tree file - the Codification plan's own hygiene flag said so -
and it is not present. code/alsjas/ exists but is untracked. So the caution R2
carries for the affine-tree line points at a line of work the repository does not
hold in any committed form. Said plainly now.

**Nine repository paths a reader could not follow.** The codification registries
were cited bare from refinement/, where such a path resolves to nothing.

**New check R-F**: every backticked repository path must resolve, from the citing
file or from the repo root. Red-green tested; nineteen paths currently cited, all
resolving. It caught the dangling design-doc path immediately, and then caught my
own correction entries twice for quoting bad paths inside the record of their
being bad - the same self-trip R-C and R-D each had, handled the same way.

audit.sh green. Six checks now: R-A corpus keys, R-B image-verified register,
R-C witnesses and elisions, R-D retracted claims, R-E non-full results
(informational), R-F repository paths.

## 2026-09-04 - Adversarial round eight: the margin's sufficiency has no full proof

Four findings. The first is the most consequential status omission in the whole
Refinement, and it governs R3's central claim.

**Willard2011 Lemma 4.6 is sketch, and R3 never carried it - in six places.**
Lemma 4.6 is what says Tightness implies B^xi + SelfRef is a consistent
self-justifying system. It is the reason the margin is worth defining at all.
Its proof is, in Willard's words, "kept brief and informal ... because the same
topic will be visited more rigorously during Section 5's discourse" - and
Section 5's rigorous engine, Theorem 5.9 (full), consumes **EA-stability, not
Tightness**.

So the margin's sufficiency has **no full proof anywhere in the corpus**. R3
section 6 had claimed "the corpus already proves the thing R3 wanted proved about
it". It states it; it does not prove it.

**And R3's "shorter route" is the less rigorous one.** Compare:

  Willard:  A-Stable -> (Lemma C.1, full) -> 0-Stable -> (Thm 5.11, full)
            -> self-justifying
  R3:       A-Stable -> (section 2.2's argument) -> Tight -> (Lemma 4.6, SKETCH)
            -> self-justifying

Willard's chain is full end to end; R3's terminates in the sketch. Section 2.2
had called its route a simplification without that qualification. It is still
worth having - a short route through a named inequality is easier to check and
to generalise - but it is not a strictly better proof, and the earlier wording
implied it was.

**Two anchor defects, one of them self-inflicted.** R3 section 4 pointed at
"section 4's second paragraph" for the dial statuses; that paragraph is "Argued,
not proved" and holds none. And round three, repairing section 2.3c's dangling
"as tabulated above", replaced it with "codified section 3.2" - which is *The
grounding functions* and tabulates no systems at all. The Type-A row, "the whole
tableaux line", is codified section 3.1. A fix that introduced a fresh defect of
the same class it was fixing.

Verified sound this round: every codified anchor the Refinement cites resolves
to a section with the expected content (ten checked); every cross-document
section reference into R1-review, R2 and R3 resolves; and section 4's claim that
Willard names the dying lemma "three times, in three papers" is verbatim-accurate
against codified section 7.2.

audit.sh green.

## 2026-09-04 - Adversarial round seven: rule 2 was being broken systematically

**Every claim about a Willard result carries that result's Proof status, at the
point of use.** That is rule 2 of VERIFICATION.md, written after the first
review. A deliberate sweep this round found it broken in four places, one of
them structural.

**Willard2001 Theorem A.1 is stated-only, and section 2.2 did not say so.** That
is the theorem the entire Hilbert-Bernays frame rests on - "Willard's systems
prove all of PA's Pi^-_1 theorems, they therefore must breach one of (1)-(3)".
Willard writes "we will not give a formal proof of Theorem A.1 in this very
short appendix" and offers footnote 16's reduction to Hilbert and Bernays'
construction instead. The Refinement's central framing device inherits that
status and never said so.

Also: Willard2002a Theorem 2.2 is sketch (headed "Proof Sketch" on the page) and
was cited bare three times; Willard2006a Theorem 6 is sketch, and R1-review
leant on it as the corpus analogue of H_<omega without flagging it.

**A third check that passed by never matching.** I wrote a prototype check for
this very rule. It stripped backticks from the citations it extracted, then
grepped the files for the stripped form - which never occurs - so it reported
every status as carried. Removing a status I had just added produced no failure,
which is how it was caught. That is the same failure as R-D's first guard window
and R-C's subshell bug: a check that is green because its own matching is
broken. Three times in one session. Red-green testing is the only reason any of
them was caught, and a check that has never been seen to fail should not be
believed.

**R-E is therefore informational.** No positional heuristic captures rule 2 - a
status may sit in a following paragraph, or in the prose around a table. Both a
+/-2 line window and a paragraph window were tried and both produced three false
positives out of four. So R-E lists every non-full result the Refinement cites,
with its status and citation count, and leaves the judgement to a reader. It
does fail on the one thing it can decide: a citation resolving to no row in
results.md. Six distinct non-full results are currently cited, all sketch.

audit.sh green.

## 2026-09-04 - Adversarial round six: a conditional result used bare, and section 8 against itself

Four findings. The first is the one the project's own machinery already forbade.

**Section 7 used a conditional result without carrying the condition.** Willard's
reply to his own strongest self-objection - section 8's "Is it not almost
cheating...?" - is Definition 8.1's Platonic Stability plus Corollary 8.2. And
Corollary 8.2 depends on Theorem 6.7, which is conditional on Conjecture 6.6,
which is stated-only. So Willard's answer to the deepest objection against his
own programme is itself conditional, and section 7 presented it flat.

Obligation **O66** exists for exactly this: "Flag Willard2016's conditionality
wherever its result is used." Section 9 carried it; section 7, where the result
is actually used, did not. Now it does.

**Section 8 contradicted itself two paragraphs apart.** It said "the mechanism is
the same as Willard's" and then, two paragraphs later, that the transfer is an
"analogy - it is not yet a correspondence, since no formal mapping has been
established". Willard blocks by cost, Brown-Palsberg by typing. One shape, two
mechanisms; the section now says so.

**And it overstated what Brown-Palsberg block.** Section 8 said "they do not
block the diagonal". Their abstract, image-verified, says static type checking
"can exclude the proof's diagonalization gadget". The gadget is precisely what
is blocked; what survives is the self-representation. The distinction is the
whole content of the analogy, so getting it backwards mattered.

**A Codification gap, found by leaning on it.** Willard2016 Definition 8.1's
registry digest was empty - the bare words "Platonic Stability" and nothing
else. The Refinement discovered it by reaching for the definition and finding
nothing to cite. Content supplied from Corollary 8.2, with the definition's own
statement flagged for the next visual pass.

Also checked this round and found sound: every numbered-item citation in the
document resolves to a registry row (fourteen checked); Willard2005 Theorem 5 is
indeed the Tab-U*_1-List preservation result section 2.3c says it is; and the
remaining uses of Theorem 6.7 elsewhere in the Refinement do carry the
conditionality.

audit.sh green.

## 2026-09-04 - Adversarial round five: the corpus has two engines, and two documents had one

Three findings. The first is a genuine misreading of the Codification.

**Section 6 said "every consistency-preservation proof in the corpus" is a
minimal-counterexample argument on Theta(P), and cited Willard2005 Theorem 1 as
an instance.** Codified section 6.9 denies both in its opening sentence: "The
Hilbert line does not use theta-Compactification. Its preservation results are
proved directly, by minimal counterexample." Willard2005 Theorem 1 IS the
theta-compactification engine (codified 6.4) - apparatus-neutral, saying nothing
about tableaux, and explicitly the *other* method.

So the corpus has two engines, not one:

- Hilbert line, by minimal counterexample: Willard2001 Thms 3.4 and 4.3,
  Willard2006a Thm 3, Willard2016 Thm 6.7 (conditional on Conjecture 6.6).
- Tableaux line, by theta-compactification: Willard2005 Thm 1, with Thm 4
  discharging the hypothesis for Tab-U*_1-List at theta = 1/4.

R3 section 1.1 carried the same error, and there it was load-bearing: it was the
argument that "shortest proof" in Definition 4.5 means least Godel number. The
reading survives - Def 4.5 says "shortest" and the Hilbert-line usage fixes the
sense - but its support is one line's practice, not the whole corpus's. Section
6's Rosser parallel also survives, in a narrower form: the size-relativity is on
both lines, but it lives in the minimised Theta(P) on one and in
U-Height(p) < (1/5)Log_2(p) and the theta bound on the other.

**Two stale claim statements.** R3 section 3's heading still asserted "every dial
moves one of the two terms" - the claim section 3.3 retracts for Level(n) - so a
reader skimming headings got the withdrawn version. And R1-review section 4 still
stated B5 as "prove the five dials are instances of the general form", superseded
both by three-of-five and by R3 section 5's refutation of the premise that a
third instance exists at all.

audit.sh green.

## 2026-09-04 - Adversarial round four: what the document omits about its own stage

Three completion findings and one precision note. The middle one is the
substantial one.

**Section 9 never mentioned R5.** ADR-0002's secondary-literature pass -
Artemov, Pudlak, Visser, Adamowicz, Kolodziejczyk, Salehi - is a pending
component of this very stage, and the document's own account of what is
unsettled omitted it. Artemov, Visser and Salehi are held in works-citing-dew/;
Adamowicz and Kolodziejczyk are not.

**And Pudlak is not held at all.** The Refinement invokes "Pudlak's theorem"
fourteen times across six files. The claim that his cut-shortening technique
*requires successor totality* is what sections 1 and 2.5b use to explain how
both Willard's Hilbert line and Pakhomov's H_<omega evade him - it is the hinge
of the whole comparison. Every one of those invocations rests on **Willard's or
Pakhomov's description** of a paper that a find over papers/, lit/ and
works-citing-dew/ shows nobody here holds.

The descriptions agree with each other and there is no reason to doubt them.
But "agrees with the two authors who need it to be true" is a weaker warrant
than this stage applies to Willard's own corpus, and the asymmetry was silent.
Recorded as gap **G39**, with a standing condition: until Pudlak is acquired, no
Refinement document may state what his proof requires except as reported by
Willard or Pakhomov, and each such claim must say so.

**The header overclaimed, and this session is what falsified it.** It said
"every claim it makes about the literature is anchored in the codification's
registries". False in two directions: the secondary literature was never in the
Codification's scope, and the Pudlak claims are anchored in nothing held. It now
says which claims rest on what - registries plus page images for Willard, page
images alone for the secondary literature, and second-hand report for Pudlak.

**One precision note.** Section 2.3 identified the TR's condition-(2) phrasing
with Theorem A.1's without saying how they line up, though section 2.3a's whole
argument turns on where the quantifiers sit: A.1 is schematic in Phi and Psi
with the proof quantifier hidden inside Der, and the TR unpacks the same content
over proof codes. Now stated.

audit.sh green; gaps 39 rows.

## 2026-09-04 - Adversarial round three: counts, argument order, and an elided path

Six findings, none of them about the mathematics and all of them the kind a
reader would trip over.

**Two counts contradicted the Codification.** Section 1 said "eleven formula
classes"; codified section 3.3 and drift D50 both say **ten** - D50 is titled
"ten formula-class notations for one idea". Section 1 also said "five apparatus
families" for what codified section 4.1 catalogues as five *apparatuses*, four
of the five being Hilbert-style. Both corrected.

**Section 6 used two argument orders four paragraphs apart.** The Rosser
sentence was written Prf(y, quote-R) - proof first, the textbook convention -
while SemPrf^K_alpha(x, y, z) below it and Willard2020 Eq. (5)'s
Prf_{IS_D(beta)}(x,p) are theorem-first. That is in the one section whose whole
point is that the two predicates are the same move, where a reader cannot see
the parallel if the arguments swap sides. Rewritten in the corpus's order, with
the clash noted.

**Section 4's "ten occurrences" was wrong, and fixing it exposed a Codification
gap.** The replication map listed nine - and omitted the origin text. TR 93-10
printed p. 6 carries the doubling/squaring device (a_0 >= 2, a_{i+1} = (a_i)^2,
"a_n requires at least 2^n bits", then b_{i+1} = b_i + b_i), framed as a
philosopher's hedge: asked to build the squaring sequence the expected reply is
that "multiplication is a function in theory but not always in practice",
whereas "the philosopher would not give such a quick answer about addition".
The map had cited only the 12-page published abbreviation. Row added,
image-verified; the count is now genuinely ten.

**An elided witness path, and then a bug in the check written to catch it.**
Section 2.1 cited Yanofsky as `lit/...Working Category Theorist...pdf` -
unfollowable by a reader, and R-C's path pattern did not match an elision, so it
passed in silence. Real path given and R-C now rejects elided paths. But that
new clause read its hits from a **pipeline**, so the `while` ran in a subshell,
`err`'s assignment to FAIL was lost, and the run printed AUDIT-FAIL lines while
still exiting green. Caught in the red-green test, fixed with a process
substitution, re-tested. Then it fired on VERIFICATION.md's own record of the
defect, so the check is scoped to the prose documents - the same exemption R-D
already has, for the same reason.

**Section 2.3b's hypothesis-count criticism was incomplete.** Willard2006a
Remark 1 supplies Theorem 4*, "a stronger form of Theorem 4", which drops the
Concise Encoding assumption - so the control is available with two hypotheses,
not three. The logical point stands unaffected: it affirms the consequent either
way.

**Verified sound this round**, and recorded as verified: section 2.1's statement
of Lawvere's Fixed Point Theorem (the uncurried weakly-point-surjective form,
with Cantor/Russell/Tarski/Turing/Godel-I as its contrapositive - correct as
given); section 8's account of Brown-Palsberg, whose abstract states exactly the
mechanism section 8 attributes to them, "static type checking in F-omega can
exclude the proof's diagonalization gadget"; section 2.3a's paraphrase of
Continuous Expansion; section 2.3b's statement of Theorem 4's three hypotheses;
and every internal cross-reference in the document.

Register 43 rows, all image-verified. audit.sh green.

## 2026-09-04 - Refinement review, deep pass: the nine unverified quotations read

The previous pass flagged nine quotations in refined-sjas.md as having no
register row, and left them. Under the standing rule that extracted text is not
a source, flagging is not discharging. This pass read all nine against page
images, plus eleven more pages of corroboration.

**Eight of the nine were accurate. One was not.**

Willard2016 section 9 says the evasions rested on arithmetics "weaker than
traditional arithmetics **in, at least, some respects**". Section 7 quoted only
"weaker than traditional arithmetics", dropping the qualifier and making his
concession absolute. Section 7 now carries the sentence whole.

That is the third elision of this exact class - after Willard2005 Remark 4's
"in [68] implies that" and Willard2020 Theorem 4.5's hypothesis on beta - and
the pattern is worth naming: the failures here are not mistranscribed symbols
but **dropped hedges and dropped attributions**. Extracted text was never the
cause of those three; compression was. The images-only rule does not reach this
failure mode, and section 9 and VERIFICATION.md now say so.

**Verified correct, and worth recording as verified:**

- Section 2.2's claim that the TR states the governing Hilbert-Bernays fact
  **four times**. Checked page by page: printed pp. 1, 2, 6 and 12. It is right.
- Section 2.3c's claim that the tableaux line keeps successor and addition
  total. Willard2011 Remark 6.16 has xi*, xi** and xi^R as "Type-A systems that
  recognize addition as a total function", and TR p. 2 gives Solovay's two
  conditions - (a) Gentzen with a cut rule, (b) recognizing Addition (or even
  successor) as total - with the tableaux line relaxing (a), not (b).
- Willard2020 Theorem 4.5 verbatim, with its sketch status visible on the page.
- Willard2006a section 6's ISINF(A), "Infinitely Far-Reaching without sustaining
  an ability to prove successor is a total function" - the uncited corpus
  analogue of Pakhomov's H_<omega.
- Willard2001 Theorem A.1's footnote 16 and the "differs from the classic
  Hilbert-Bernays theorem only by..." passage, both verbatim.

**Two smaller corrections.** Section 5.1 glossed Willard2002a Theorem 2.2 as
"cut elimination preserves theoremhood while the proof can be longer"; the
blowup is in **building the composite** of three proofs under a cut-free method,
not in eliminating cuts from one already built - section 2.3 had it right all
along. And Hybrid(H)'s domain restriction i >= 3 was missing everywhere, as was
Willard's own framing that the convention "can unify the formalisms of Theorems
3 and 4" - which supports R3's project and no draft had cited.

**Drift D82 understated the problem, and is rewritten.** It recorded two omega
families; there are **three**. Willard2001 p. 45 defines both the Hajek-Pudlak
omega_i and the Wilkie-Paris varpi_i - and **varpi_1 is exactly
Willard2007-APAL's omega_1** under a different letter, while Willard2001's
Hajek-Pudlak omega_1 is about x^2, essentially Willard2020-LFCS's omega_0. The
hierarchies are offset by an index as well as differing in definition, so a bare
"omega_1" is ambiguous even once the paper is named. Willard himself notes the
prior literature "has found it mostly unnecessary to distinguish between the two
slightly different notations", which is how the letters drifted.

Register: 41 rows, all image-verified. Notation: 227 rows. audit.sh green, and
M5 again caught the README count before I did.

## 2026-09-04 - R6 completion review: two fixed points, one positive difference

The categorical repair still conflated two selected Willard diagonals. The G2
proof in Theorem 1.1 consumes `G <-> not Box G`, displayed under the
`Pi^-_1`-strength hypothesis in `Willard2001` Theorem A.1 footnote 16. The 1993
Group-3 construction instead produces `H <-> Con(B+H,D)`, whose being adjoined
as an axiom supplies the internal consistency point. Both use fixed-numeral
substitution, but they are not the same proposition or the same logical role.
`refinement/lawvere-sjas.md`, ADR-0004, and the verification row now separate
them.

The review also closes an understated B-S fact. Their toy `S` is externally
consistent in Definition 2.1's sense: otherwise I4, the bottom initial sequent,
and Theorem 5's admissible cut would make Proposition 4.1's expressly
underivable sequent derivable. Thus external consistency is shared by B-S and
Willard. The positive difference is exactly the internal consistency point:
B-S has none (§6), while Willard's Group-3 supplies one and Proposition 1
(`full`) proves that its theory remains consistent.

Theorem 1.1 now states the exact selected fixed point and `BoxContr_G` instance
it uses, rather than suggesting a general family is necessary. The AU section
also carries the source's own qualification that its §6 Löb derivation is a
sketch.

## 2026-09-04 - R6 categorical repair: two independent missing maps

The 2026-09-02 R6 completion claim did not survive categorical review. Its
`Aff(alpha)` was expressly allowed to fail closure under composition, so it was
not a category; it also promoted Willard's selected Group-3 fixed point to an
unsupported universal Lawvere situation.

Replaced `refinement/lawvere-sjas.md` with a three-way proof-doctrine
comparison, after reading the relevant source pages as rendered images:

1. van Dijk-Oldenziel: `Box = Gamma R`; HBL maps are Props. 6.5, 6.6 and
   6.8; Prop. 6.8 is internal-Hom evaluation; finite products supply the
   normally implicit contraction diagonal; Lem. 6.12 is universal fixed point.
2. Beklemishev-Shamkanov: the APS is a thin category and the implication
   calculus a symmetric monoidal proof category (cut is admissible). Their toy
   `S` satisfies all three Lob conditions and has fixed points; it lacks the
   boxed diagonal. It blocks formalized G2 but does not prove `not Box bottom`.
3. Willard Type-A `IS(A)`: the base syntactic category is cartesian, but the
   uniform HBL-(2) proof-composition entailment is absent; fixed proof-code
   instances exist. The same pointwise/uniform split gives the selected
   Group-3 substitution instance without total substitution. Group-3 supplies
   an internal consistency point and Prop. 1 (`full`) supplies external
   consistency.

The two evasions are therefore orthogonal: B-S remove structural duplication
under `Box`; Willard removes internal uniform evaluation while retaining the
positive and semantic data required for self-justification. The exact HBL
breach remains unassigned for Willard routes other than the Type-A `IS(A)` case
that the 1993 report names.

## 2026-09-04 - Review of refined-sjas.md for completion and correctness

Seven findings. The first is the one worth keeping.

**The document contradicted a verdict committed the same day.** R3's re-reading
retracted the Level(n) instance and cut the count from four dials to three, and
withdrew the claim that the margin "generalises" Willard's Def 4.5. Sections 5.1
and 9 of refined-sjas.md still carried the old text: four dials, Level(n) on the
cost side, "generalises". A stage document and its own supporting analysis
disagreed, in the same repository, hours apart. Both sections are rewritten;
section 9 now also carries what R3 found and this document had missed - the
d-independence lemma, the forced threshold, and the withdrawal of the stability
bridge's novelty claim.

**Section 2.5b overclaimed about Pakhomov.** It said he "does not remove" a
derivability condition, contrasting him with Willard. He is silent on the point;
p. 3 says HBL conditions "do not necessary hold in the case of some weaker
arithmetical c.e. theories". What he blocks is the step after them: Pudlak works
by carrying a G2 failure in T into "a different theory T' (interpretable in T),
where HBL are satisfied", and that interpretation needs successor totality,
which H_<omega denies. Same door as Willard, not a different one.

**Two more dangling cross-references, both in section 2.3c.** "The tableaux line
as tabulated above" - there is no table above that line. "The distinction section
8 is simultaneously trying to draw" - section 8 is the computational analogue and
draws no such distinction; it is 5.1's. The previous pass fixed one mis-numbered
reference in 2.3b and did not sweep the rest of the subsection for others.

**Nine quotations in the document have no register row.** Under the standing
images-only rule they are unverified in this stage. Each is backed by an
image-verified row in registry/results.md, so none is suspected wrong - but none
has been checked against the page here. R-B validates the rows that exist and
structurally cannot detect a quotation that has no row, so register completeness
joins verbatim transcription as a human obligation the audit supports and does
not discharge. Recorded in section 9 and in VERIFICATION.md.

**A Codification gap, surfaced by the review and not a Refinement error.**
Section 3 rests on the classical bound omega_1^K(p) with omega_1(x) = x^Log(x),
cited to Willard2007-APAL section 4. That function was recorded NOWHERE in the
registries - while R1-review.md listed section 3 as "verified against
Willard2007-APAL section 4". I read printed pp. 18-19 as images: the section is
**correct as written**, verbatim, including the (*) bound and the fact that
ISigma_0 cannot prove omega_1 total (Eq. 32), which is the totality failure the
programme turns on. Now registered in notation.md.

And it collides. Willard2020-LFCS defines a different omega_j - omega_0(x) = x^2,
omega_1(x) >= x^x - under the same name. x^Log(x) is quasi-polynomial; the other
is exponential. Neither paper cross-references the other. Recorded as **drift
D82**, the ledger's seventieth entry.

**Two process notes.** The audit caught my own README drift (224 notation rows
against an actual 225) without my noticing I had introduced it - M5 earning its
keep. And a batch-edit script aborted mid-way twice in this session, discarding
edits that had already reported success, because the write happens at the end.
That is process rule P1 recurring for the third time; the remaining edits were
redone with a write after every single replacement.

Verified sound and unchanged: sections 1, 2.1-2.4, 2.5-2.5a, 3, 4, 5's
withdrawal and the Hybrid arithmetic, 6, 7's five costs, 8's declared conjecture.

audit.sh green: 29 register rows, 225 notation rows, 70 drift entries.

## 2026-09-02 - R6 complete: G2 in-scheme, Aff missing-arrow, SJ conditions

Completed `refinement/lawvere-sjas.md` Main Theorem:

1. Thm 3.2 — G2 as Lawvere (step L only) + HB transport H3–H7, with
   packaging Lemmas 3.2.1–3.2.5.
2. Defs 5.1–5.4 — Aff(α) as wide *class of arrows* (not a subcategory:
   composition is mp_□). Thm 5.5 — uniform H4 missing for IS(A), by
   Willard's TR p. 12 img statement translated into Aff.
3. Thms 5.6–5.7, 6.2–6.5 — evasion mechanism and necessary SJ conditions
   (SJ1)–(SJ4); Type-A instance; conventional configs collapse.

Audit table in §7. Consistency of IS(A) cited from corpus `full`, not
re-derived. Margin Thm 5.9 remains `sketch`-status optional witness.

## 2026-09-02 - R6 Lawvere–SJAS: prove G2 in-scheme first, then break hypotheses

Goal: translate Willard's SJAS into Lawvere / Yanofsky so the evasion of G2
is a theorem about named hypotheses, not a framing.

Methodological correction (user): do **not** lead with Willard as an
external witness that "G2 needs more than LFPT". First prove G2 using
Lawvere/Yanofsky techniques; then locate where Willard invalidates that
proof.

Delivered:

- `refinement/ADR-0004-lawvere-sjas-translation.md` — R6 charter; binding
  order; acceptance L0–L6.
- `refinement/lawvere-sjas.md` — Yanofsky Thms 6.4–6.5; diagonal lemma as
  Lawvere; **Theorem 3.2** Hilbert–Bernays–Löb G2 with hypothesis register
  **H0–H8** (Lawvere used only at step (L)); Willard mapped as breaker of
  post-Lawvere Hi (principally **H4** on the tableaux line); self-justification
  conditions read off the register.

Open: image-verify §5 quotations; construct an explicit affordable
subcategory where H4 fails as a missing Hom; optional Kreisel secondary
register (§4.2).

## 2026-09-02 - Does R3 hold? Re-read of Willard2011 sections 4-5 from page images

Standing instruction, and it now governs both charters: "Using OCR'd text
clearly does not work; all reading must be of the original documents
themselves." Extracted text may LOCATE a passage and may never be quoted from.
The former exemption for prose containing no mathematics is withdrawn - it
failed twice, on two page numbers and one non-verbatim prose quotation carried
under an image-verified tag. VERIFICATION.md's txt status is retired; all 28
register rows are now img, and check R-B fails anything else (red-green tested).

Then the question itself. I re-read Willard2011 printed pp. 12-16 and 21 as
rendered images, plus Willard2001 p. 44 and Pakhomov pp. 3, 4, 22, 24.

**R3 partly holds. Three of five, not five of five.**

WHAT HOLDS.

- The definition. M(xi) = inf { Log(q_beta) - sharp(beta) } with threshold 2 IS
  Willard's Definition 4.5, and Lemma 4.6 is exactly the theorem that it
  suffices. Verified verbatim from the page.
- Three dials, now PROVED rather than argued. Defs 4.1-4.4 define sharp without
  any reference to the deduction method d: E(n) denotes the fixed value 2^n,
  Scope_E is syntactic, Good(N) is truth in Standard-M. So sharp is
  d-independent, and any dial varying only d moves Log(q_beta) and cannot move
  sharp. That covers R, Herb-k and Z. This one-line lemma is what the whole of
  section 3 needed and no draft had.
- The A-Stability implies Tightness bridge completes. The r.e.-versus-arbitrary
  gap closes by passing to the finite subset of beta that the shortest
  refutation actually uses: that subset is r.e., has the same q, and has
  sharp >= sharp(beta).

WHAT DOES NOT HOLD.

- The Level(n) instance is RETRACTED. Two errors. Group-3 is not part of B^xi -
  Def 4.5 has B^xi as the base and Lemma 4.6 adds SelfRef on top. And Level(n)
  varies a SENTENCE, so both terms move; at the critical beta = B^xi + Psi,
  Equation (20) makes them move in exact lockstep, so the margin there is
  identically 1 whatever the level.
- Hybrid(H) remains unplaced, for the same structural reason.
- Section 3's whole framing was not well-posed. M is a DIFFERENCE; "dial X
  lowers Log(q_beta)" says nothing about it unless the other term is checked,
  and no draft checked it. For the three apparatus dials the omission was
  harmless - but by luck, not by argument.

WITHDRAWN CLAIMS ABOUT R3 ITSELF.

- "Generalising Def 4.5". It is a restatement: for-all-beta A >= 2 and
  inf A >= 2 are the same statement.
- Novelty for the stability bridge. Remark 5.2 announces "Theorem 5.11 will show
  the presence of A-stability, alone, is sufficient for constructing
  self-justifying systems", and Theorem 6.3 uses it. R3's route is shorter, not
  new. A simplification is the stage's remit; a new theorem it is not.
- Authorship of the sharp-is-model-theoretic argument used to withdraw the
  Hybrid side-assignment. It is Willard's own justification for Equation (20):
  sharp(B^xi + Psi) = sharp(Psi) "because all of B^xi's axioms are true under
  the Standard-M model".

WHAT R3 MISSED, AND IS THE BEST THING HERE.

The threshold 2 is forced, and the corpus says why. When the self-referential
axiom is false, Eq (19) gives Log(q) = sharp(Psi) + 1 exactly, and Eq (20)
carries it to sharp(B^xi + Psi) + 1. The fixed point sits at margin EXACTLY
ONE. Tightness's +2 is precisely the demand that every inconsistent beta have
strictly more margin than the fixed point achieves - and Lemma 4.6 is that
one-line contradiction, nothing more. R3 carried the 2 as a given for three
drafts. It is the content.

R3 section 6 states the verdict; sections 1-4 are rewritten around it. B5 is
partly met at three of five. audit.sh green.

## 2026-09-02 - Omnibus bibliography of works Willard cites (works-cited-by-dew)

Created `works-cited-by-dew/` as the reverse of `works-citing-dew/`: every
non-self bibliographic citation in the SJAS Willard witnesses, with the full
set of citing DEW documents per entry.

- Regenerator: `works-cited-by-dew/build-bibliography.py` (parses reference
  sections in `codification/sources-text/`, drops Willard self-cites, clusters
  by author/year/title).
- Human omnibus: `works-cited-by-dew/BIBLIOGRAPHY.md`; machine indexes:
  `bibliography.json`, `bibliography.tsv`; notes in `README.md`.
- Tally after quality filters: **157** external works from **712** rows across
  **23** DEW documents (**174** self-citation rows excluded).
- Coverage gaps recorded in the omnibus: `Willard2006b` has no bibliography in
  its text layer (use `Willard2005-TAB`); `Willard1998` is thin OCR with a
  manual Solovay 1994 row; local `[n]` keys are paper-local, not global IDs.

## 2026-09-02 - Independent review of the repair commit: ten more corrections, two in the checks themselves

An adversarial review of 4eb63c7 (record at refinement/REVIEW-2026-09-02.md,
which the agent wrote incrementally against a progress checklist so an
interrupted run could resume - the first attempt died on a rate limit having
written nothing). Ten findings, all verified independently before repair. Four
matter.

**The repair commit misquoted its own evidence.** Two Beklemishev-Shamkanov
sentences, reproduced in R2 and R3 inside blockquotes tagged "(image-verified)",
were not verbatim: the paper's own abbreviation "G2" was silently expanded to
"Godel's second incompleteness theorem", a clause "in it" was added that is not
in the source, and a mid-section sentence was described as the section's close.
Re-rendered p. 14 at 200 dpi and confirmed. The same passage is quoted correctly
in VERIFICATION.md and in refined-sjas.md section 2.5a - so three of five
transcriptions were right and two were wrong, in the two documents doing the
fixing, in text this commit had just added. The substance (that BS's S is not
self-verifying) is unaffected; the fidelity claim was false.

That is the whole lesson of the previous entry recurring one commit later: the
discipline was written down, the register was built, the checks were added, and
the thing the discipline exists to prevent happened anyway in the act of
preventing it.

**Two defects in the new checks, both found by construction, not by reading.**

- R-B tested only the *stripping* family of pdftotext corruption (box,
  turnstiles, corners) and missed the *substitution* family - U+2127 rendered as
  digit 0, Fraktur I as "=", flattened floors and towers (drift D71). That is
  the more dangerous family, because the output reads as ordinary text rather
  than an obvious gap. Constructed txt rows carrying those symbols passed as
  well-formed. The hazard set now covers both; adding Greek letters as well
  overshot into ordinary prose and was reverted.
- R-D's context window was defeated by a withdrawal word in neighbouring,
  unrelated prose: three lines where line 1 retracts an encoding convention and
  line 2 bare-reasserts the retracted Pakhomov claim passed green. The window
  had already been narrowed once, six lines to one, for this exact failure -
  narrowing raised the bar without closing the hole. R-D is now line-local: a
  line is guarded iff it quotes the retracted text or itself carries a
  retraction verb. Both of the review's defeat cases are now caught, and the
  control still fires.

R-D is also recorded, in the script and in VERIFICATION.md, for what it is: a
regression guard on fixed strings. It cannot catch a reworded reassertion, and
it does not verify that a marker is about the same claim as the text near it.
Green means "these exact sentences have not returned unmarked", nothing more.

**And B6 overclaimed.** "Every mathematical quotation image-verified ...
audit.sh green" was asserted while the misquotations stood - and audit.sh cannot
check quotation fidelity at all, since nothing diffs a quotation against its
source. B6 now says so: verbatim transcription is a human obligation the audit
supports and does not discharge.

Six smaller corrections: the "abstract's" quote on uniqueness and weakening is
from the Introduction p. 2, and the abstract mentions neither; R3 section 1.1's
closing claim that the encoding requirement "keeps [the three size measures]
within constant factors" contradicted its own correction two paragraphs earlier
and is withdrawn (found by me, before the review returned); Pakhomov page
numbers - the "both H and H_<omega" sentence does span pp. 3-4, but the second
citation is p. 22 not 21 and is about H alone, and footnote 3 with the
Acknowledgments is on p. 24 not 25; refined-sjas section 2.3b's cross-reference
to "section 4 below" cites nothing; R1-review finding 7 still called G38 an
"acquisition target for R5" while two files in the same commit recorded it
closed; and VERIFICATION correction 3's location list read as one quote in three
documents when it was two instances in two.

The review also checked and found sound: the Hybrid arithmetic, recomputed
independently; the condition-(2) identification against the TR page image; all
of Beklemishev-Shamkanov p. 8; the substance of the Pakhomov correction;
Willard2011's stability definitions and Theorem 5.9's dependencies; the encoding
requirement's direction; R-A and R-C by construction; and - going past what was
asked - R3's new A-Stability-implies-Tightness sketch, which it judged
essentially complete rather than a sketch. I separately verified the five-dial
status table against codified section 8.1 and results.md: all five cells match,
Willard2004 Theorem 3 is the positive half and Theorem 2 the negative, both
permanently sketch per G35.

audit.sh green: 26 register rows, 13 corpus keys, 3 witnesses, 5 retraction
patterns.

## 2026-09-02 - Refinement review: 22 corrections, and audit.sh extended across the stage boundary

A subagent review of R1-R3 returned about thirty findings. All Tier-1 items were
verified independently before repair; four held, and each was of a kind the
Codification's machinery exists to prevent, committed in the one stage that
machinery did not reach. That is the finding worth keeping.

**The four Tier-1 defects.**

1. The claim that Willard never names which derivability condition his systems
   breach. He names condition (2) for IS(A) at TR 93-10 printed p. 12, as a
   *uniformity* failure - the implication holds "only in the degenerate case
   where x and y are fixed constants". The repository's own extraction record
   has a section titled "Which derivability condition fails". The claim was made
   without consulting it.
2. The claim that Pakhomov's H_<omega proves successor totality and so falsifies
   R1's criterion. He states twice that neither H nor H_<omega can prove it -
   H_<omega evades Pudlak by Willard's own door. Retracted in R1-review section
   2.1, refined-sjas section 1, and ADR-0002.
3. A Beklemishev-Shamkanov block quote with every box stripped by pdftotext,
   reproduced in three documents. Theorem 4's conclusion lost its box the same
   way: it is boxtimes-top =_S box-bottom, not boxtimes-top =_S bottom.
4. Beklemishev-Shamkanov's S listed as a third route to self-verification. Their
   section 6 says the opposite: S "does not provide a counterexample to the
   non-formalized version of G2, since => not-box-bottom is not provable".

**The largest downstream consequence.** Finding 4 refutes the biconditional the
classification was built on. Breaking G2's argument is *necessary* for
self-verification, not sufficient - a theory must additionally prove Con. R3
section 5 is rewritten in three subsections around that distinction.

**A defect found by computing rather than reading.** refined-sjas section 5
stated the criterion as a growth rate: no proof of length L may denote an
integer whose encoding exceeds O(L). Summing the Hybrid recurrence gives
Log_2 C_n = Theta(n (log n)^H). Hybrid(1) is the *positive* case and is not
O(n); Hybrid(2) is the negative case and is also quasi-linear. The criterion
misclassifies the positive case and cannot locate the transition in either
direction - the real boundary is a single logarithmic factor. Willard's own
"midway between the additive and multiplicative conventions" is loose, and the
draft had taken it as exact. The rate form is withdrawn; R3's margin, an
additive comparison of two sizes, replaces it.

Twenty-two corrections in all, tabulated in refinement/VERIFICATION.md. The
others of substance: EA-stability is Definition 5.5 (both halves, quantified
over possibly-false R-Views), not Definition 5.1; the Conventional Encoding
Requirement is a *lower* bound, so it excludes over-compressed encodings, not
wasteful ones - the hazard the draft claimed it guarded remains unguarded; the
Hybrid dial's side-assignment is withdrawn, since sharp is model-theoretic and
naming axioms are true; Level(n)'s mechanism restated as strengthening the base
rather than changing what counts as a refutation; the R/Z merge removed, as
codified section 8.1 expressly forbids it (D34, O46, G22); BS Theorem 4 needs
contraction *and* weakening; the dial-status accounting corrected (only Level(n)
is full on both settings, not "two of five are sketch").

**The root-cause fix, which is the durable part.**

- refinement/VERIFICATION.md now carries five rules (four inherited from
  ADR-0001, plus "arithmetic is computed, never read off an informal gloss"), a
  25-row quotation register with img/txt status per quote, a Computed claims
  table, and the 22-correction table.
- codification/audit-r.sh adds four checks, sourced by audit.sh:
    R-A  every paper key cited in refinement/*.md resolves in corpus.md
    R-B  the quotation register uses only img|txt, and no txt row carries
         mathematical notation
    R-C  every cited secondary-literature witness exists
    R-D  no Refinement document reasserts a retracted claim without an
         adjacent withdrawal marker
- ADR-0002 gains constraint 4 (the verification discipline binds here) and
  acceptance criterion B6.

All four checks were red-green tested. Two bugs in my own checks were caught
that way and would not have been caught otherwise: R-D's first version used a
six-line guard window, which let an unrelated retraction elsewhere in the
section act as a spurious guard - it passed when it should have failed; and a
silent no-match in a batch edit left $docs referenced but never assigned. The
second is process rule P1 recurring, in the tool built to enforce P1.

Image-verified this session: Beklemishev-Shamkanov p. 8 (Definition 3.7,
Proposition 3.8 with boxes intact, Theorems 3 and 4, Remark 3.9's box-contraction
and box-weakening) and p. 14; Pakhomov p. 4 (superexponentiation, the
superexponential cut, the finite-model bound, Con^pred).

Stage status: R1-R3 done and repaired; R4-R5 pending. audit.sh green.

## 2026-09-02 - R3: the margin, and the refutation of the cross-route unification

R3 delivered in two halves, one positive and one negative.

**Positive.** The criterion now has a definition, built on Willard's own measure
rather than new notation. With Scope_E, Good(N) and sharp(Upsilon) from
Willard2011 section 4, and q_beta the shortest proof of 0=1:

  M(xi) = inf { Log(q_beta) - sharp(beta) : beta inconsistent, beta above B^xi }

xi is Willard2011's Tight exactly when M(xi) >= 2. The margin measures how much
further a refutation must reach than the envelope its own length affords.

All five boundary dials move one of the two terms, and they split into two
families by which side they push. Hybrid(H) is the only one that raises the
envelope sharp(beta) - more reach per symbol - which is why it always looked
unlike the other four. The other four lower the refutation cost Log(q_beta):
R, Herb-k and Z do it by making composition linear-sum on a wider class, which
is one mechanism (D30) with three parameterisations; Level(n) does it by
strengthening what Group-3 asserts, so less needs establishing to refute it.

This answers drift D38, which recorded five parameterisations "none of which
Willard relates to the others" and asked for a single presentation. One
inequality, two terms, five ways of writing on it.

Honestly labelled: the instances are argued from cited results, not derived from
the inequality. Two of the five rest on sketch results (G35) and two are
stated-only on both halves. And the engine's actual hypothesis in Willard2011
Theorem 5.9 is EA-stability, not Tightness; Stability implies Tightness is the
natural bridge but Willard does not state it and neither does this.

**Negative.** The cross-route unification R2 had made the target is refuted, and
for a structural reason rather than a gap in the attempt. The margin is a
quantitative condition on a fixed logic with fixed semantics. Pakhomov varies
the semantics - H_{<omega} proves every hereditarily finite set exists, so its
envelope is unbounded and no margin is positive; self-verification comes from
every finite subtheory having a finite model. Beklemishev-Shamkanov vary the
structural rules - their K4 has no size parameter at all.

So self-verification is a classification, not a criterion: G2's argument is
over-determined, needing a fixed point, the derivability conditions, and a
structural licence to use a context twice, and the three known routes are in
bijection with the last two plus the semantic step.

The classification has an empty cell, which is the useful part of a negative
result. Nobody has evaded G2 by attacking the fixed point itself, because
Lawvere makes it free in any cartesian closed setting. A system whose internal
hom is not cartesian closed would be a fourth mechanism and the first to make
self-reference rather than its cost the thing that fails. Whether it could still
be an arithmetic is the natural successor question.

B5 partly met. What remains open is smaller and sharper: derive each dial's
transition from the inequality rather than reading it off Willard, with
Hybrid(H) the tractable case.

## 2026-09-02 - Reuse is condition (3); the section 2.4 unification withdrawn

User asked in what way conditions (1) and (2) are "permissions to use a proof
again", noting that proof reuse is the third derivability criterion. Correct on
both counts.

Taken one at a time: (1) licenses internalisation once - from having a proof,
assert one exists. (2) licenses composition - combine proofs of Phi and
Phi -> Psi. (3) licenses iteration - from Der(Phi), assert Der(Der(Phi)), which
is the same proof reflected one level up. Reuse is (3). The earlier gloss
flattened three distinct licences into one and was wrong.

Also recorded: (3) is the strongest of the three, since proving Der(Der(Phi))
requires internalising (1), so a system failing (1) fails (3) a fortiori.
Section 2.3's assignment is therefore a claim about which of the WEAKER
conditions already gives way - which is what the design parameter controls - not
a claim that (3) survives.

The second error was bigger. Section 2.4 had claimed Beklemishev-Shamkanov's
box-contraction and the cost account were "the same condition described twice".
Checked where contraction is actually consumed in their Proposition 3.8: it is
the step from Gamma |- phi and Gamma |- not-phi to Gamma |- not-top, which
reuses the CONTEXT, not a proof. It is a rule of the ambient consequence
relation, more elementary than Loeb's conditions and lying underneath them, and
it is not a restatement of any of the three.

What survives is weaker and worth keeping: G2 is over-determined. It needs a
fixed point, the derivability conditions, and a structural licence to use a
context twice, and the literature now removes each independently - Willard a
derivability condition, Pakhomov the semantic step, Beklemishev-Shamkanov the
structural rule. A system escapes by failing any one. That is a catalogue of
independent failure points, not a unifying condition.

Section 2.4 replaced by 2.4 (what each condition licenses) and 2.5 (contraction
as a structural rule, with the quoted proof step). The slogan is restated
accordingly: G2 is a theorem about systems that meet all of its several
prerequisites at once, and the corpus is the catalogue of ways to fail one.

R3 is correspondingly harder, and the charter now says so: whether a single
condition subsumes the three routes, or whether self-verification is simply the
disjunction of G2's failure modes, is the stage's open question.

## 2026-09-01 - refined-sjas section 2 rewritten from the full evidence

Section 2 had been rewritten twice in quick succession, each time from whichever
paper had just been read, without integrating. User called it flailing, which it
was. Rewritten once, from Willard's own frame.

The governing evidence was in the corpus the whole time. Willard1993-TR says
three times that "every self verifying system must breach in some way one of the
three fundamental Hilbert-Bernays conditions". Willard2001 Appendix A proves
Theorem A.1, a version of the Hilbert-Bernays theorem sharpened to apply below
PA - it needs only that alpha prove all of PA's Pi^-_1 theorems, which Willard
notes "is actually the core fact that is needed by the classic proof".
Willard2011 adds that conventional configurations satisfy the conditions and are
"thus automatically inconsistent". So HBL is exactly the operative frame, and the
claim that "no derivability condition is the pivot" was wrong.

What the TR actually says is better than either earlier draft: the breach "may
arise because of either the axiom system employed or the choice of deduction
method, but not necessarily due to both". Feferman keeps all of PA's axioms and
breaches via the apparatus; Willard keeps a natural apparatus and breaches via
the axioms. The breach is obligatory; its location is the design choice, which is
why the programme trades parameters rather than converging.

Three corrections the user identified, all correct:

- Not all Willard systems deny successor totality. The tableaux line is Type-A -
  successor AND addition total, multiplication a 3-way relation. Only the Hilbert
  line is Type-NS. Which totality is surrendered is a design parameter.
- "silently consumes" is wrong about Beklemishev-Shamkanov; "implicitly" is the
  accurate word. Their word is "presupposes", and the point is that nobody had
  isolated the rule, not that anyone concealed it.
- "separately" was ambiguous. Fixed-point uniqueness failing, and needing the
  restricted weakening, is a second result about their toy system, not a further
  prerequisite of the G2 argument.

New in section 2.3, labelled as this Refinement's inference rather than Willard's
claim, since he says "one of the three" without assigning it: the tableaux line
breaches condition (2), internalised modus ponens, because the apparatus is
cut-free and Willard2002a Theorem 2.2 says the combined proof exists but may be
super-exponentially longer; the Hilbert line breaches condition (1), because
asserting a proof exists means naming it and no totality axiom is available.

Each half is checked against the control that fails. Restore cut to the tableaux
line - which is what Xtab does - and condition (2) returns; Willard2020 Theorem
4.5 says IS_Xtab is then automatically inconsistent. Make naming cheap on the
Hilbert line - the multiplicative convention - and condition (1) returns;
Willard2006a Theorem 4 says the system then cannot prove its own consistency.
That the two controls fail in the two predicted ways is the evidence for the
assignment.

Conditions (1) and (2) are both permissions to use a proof again, once as an
object and once as a step, which is why box-contraction is their abstract
counterpart rather than a competitor.

## 2026-09-01 - G38 acquired; R1 section 2 corrected on D3

Two things, both arising from a question about which derivability condition D3 is.

**The numbering was right, the attribution was wrong.** D3 is
`Prv(phi) -> Prv(Prv(phi))`, exactly as Pakhomov lists the three HBL conditions.
But refined-sjas section 2 named it as what Willard's systems lose, and that is
wrong twice over. HBL is not the operative frame - Pakhomov: "HBL conditions do
not necessary hold in the case of some weaker arithmetical c.e. theories" - and
what Willard actually evades is Pudlak's theorem, whose essential step is
cut-shortening, which requires successor totality. And at the abstract level the
pivot is not a derivability condition at all. Section 2 corrected.

**G38 acquired**: Beklemishev and Shamkanov, "Some abstract versions of Godel's
second incompleteness theorem based on non-classical logics", arXiv:1602.05728,
the Liber Amicorum Alberti chapter. Held with checksum.

They generalise Loeb's conditions to weaker-than-classical logics and isolate
what Godel's argument silently consumes: admissibility of the contraction rule
restricted to boxed formulas - "any hypothesis can be used several times".
Fixed-point uniqueness separately needs the restricted weakening. Their K4 over
contraction-free classical logic, with fixed-point operators, has Godelian fixed
points and no formalized G2, by cut-elimination. They call it a toy system.

Effect on the Refinement, three ways.

It corrects section 2, as above. It confirms section 2's central thesis from an
independent direction, with no arithmetic at all: a system with fixed points and
no G2 is the strongest available evidence that self-reference is not what does
the work. And it supplies the unification R1 was missing - contraction is reuse,
and reuse is what costs, so "bounded witness" and "no box-contraction" are the
same condition described twice. Three routes now: Willard denies the second use
by naming, Pakhomov bounds it semantically, Beklemishev-Shamkanov deny it
structurally.

A caution for this repository, recorded as obligation RO1. The paper notes that
box-contraction "actually holds for some meaningful arithmetical systems lacking
general contraction rule, for example, for a version of Peano arithmetic based
on affine predicate logic". So this repo's affine-tree line cannot infer from
affineness that G2 is evaded; it must show box-contraction fails, not merely
that general contraction does.

R2 done. R3 is now sharply specified: a definition adequate to all three routes
must cover a bounded valuation, a bounded finite model, and a structural absence
of box-contraction, the last having no size parameter. Whether those are one
condition or two that coincide in the arithmetic case is the stage's open
question.

## 2026-09-01 - R1 reviewed; G7 resolved as unacquirable; Pakhomov 2019 acquired

Searched for Beklemishev's "simplified presentation of SJAS" (gap G7, the
Refinement's named inspiration). It does not exist as an acquirable document.

The trail terminates in a footnote. Pakhomov 2019 (arXiv:1907.00877) footnote 3:
"Note that Beklemishev have developed a still unpublished simplification of
Willard's construction (which is different from the approach used in the present
paper)." Corroborated three ways: Willard's own Beklemishev citations are all
reflection-principle papers, none a survey of his systems; the current Wikipedia
Self-verifying theories article no longer mentions Beklemishev at all, and the
earlier revision's wording matches Pakhomov's footnote; and no published
Beklemishev work on Willard's construction surfaced in search. G7 closed
`accepted` - unacquirable, not unlocated. ADR-0002's criterion B4 is unmeetable
as written and is replaced by B4'.

The search succeeded anyway, and better than intended. It produced Fedor
Pakhomov, "A weak set theory that proves its own consistency" - published, on
arXiv, and a genuine external refinement of the SJAS idea. Acquired to
nachlass/refinement/lit/ with checksum.

It falsifies R1's headline claim. R1 said a system can assert its own
consistency "exactly when" naming an integer is not cheaper than proving
something about it. Pakhomov's H_{<omega} proves its own Hilbert-style
consistency AND proves the existence of every hereditarily finite set - it is
not growth-restricted in Willard's sense at all. On R1's criterion as written it
should be inconsistent.

The mechanisms are different but the shape is the same, and that shape is the
better statement of the essential idea: a system is self-verifying when, for
every proof it can write, a bounded semantic witness that the proof is not a
refutation can be constructed within its own means. Willard bounds a partial
valuation on one tableau branch (Willard2005 Lemmas 1-2, Val(u) <= b*2^Delta(s));
Pakhomov bounds a full finite model of the axioms actually used (size <= 2^0_p),
and relativises to the superexponential cut. R1's naming criterion is the
arithmetic instance, not the general form. Corrected.

Four further review findings corrected: the failing derivability condition is
specifically D3, not "the derivability conditions"; the incremental convention
does reach every integer and is merely not Continuously Expanding; Pakhomov's
external critique that Willard's axioms "are not completely natural in the sense
that some of axioms are constructed using Diagonal Lemma" is now carried in the
costs section; and the Brown-Palsberg computational analogue is labelled an
analogy rather than a correspondence.

New gap G38: Beklemishev-Shamkanov 2016, "Some abstract versions of Godel's
second incompleteness theorem based on non-classical logics", which Pakhomov
describes as giving an abstract G2 that FAILS for a contraction-free logic.
Directly relevant to refined-sjas section 2's claim about what G2 needs beyond
the fixed point. Not held; acquisition target for R5.

Sections of R1 checked and found sound: 3, 4, 5.1, 6, 9.

## 2026-09-01 - Refinement R1: the essential idea

Opened the Refinement stage at `nachlass/refinement/` (ADR-0002) and completed
R1, its stated deliverable: the essential idea of SJAS identified and
explicated, in `refined-sjas.md`.

The claim, in one sentence: a formal system can consistently assert its own
consistency exactly when naming an integer is not cheaper than proving
something about it.

The argument has four moves.

1. The diagonal is not the obstacle. Lawvere's Fixed Point Theorem gives the
   fixed point unconditionally; Willard's systems contain it as an axiom and are
   consistent. What G2 needs beyond the fixed point is the derivability
   conditions - the system must verify its own proof-checking - and that is a
   statement about cost, not about self-reference. This also explains the
   apparatus-identity thesis: the apparatuses agree on what is provable and
   differ on what proving costs, and G2 is a theorem about cost.
2. The cost is naming. Certifying a length-L proof requires naming a number
   below omega_1^K(p). A system that can do that closes the diagonalisation; one
   that cannot, stalls.
3. The criterion: naming must be non-compressive - no proof of length L may
   denote an integer whose binary encoding exceeds O(L). Checked against all
   five naming conventions in the corpus, and matching Willard2011 Definition
   4.5's Tight condition, Log(q_beta) >= sharp(beta) + 2, which is the same
   inequality written as arithmetic.
4. The five boundary dials of D38 are one dial - how much compression is
   permitted - measured five ways. That the five agree is the best evidence the
   criterion is the right invariant.

The Rosser hook is discharged, and the finding is that the trick is already
there, undeclared: SemPrf^K bounds the diagonal by magnitude where Rosser bounds
it by comparison, and every consistency-preservation proof in the corpus is a
least-number-principle argument on Godel number. The Lawvere hook is downgraded
to a framing - it separates existence of a fixed point from derivability of its
consequences, which is clarifying, but gives no categorical proof of any result.

The computational analogue is concrete: Brown-Palsberg's F_omega
self-interpreter keeps the diagonal and denies the composite by typing, which is
the same move Willard makes by growth restriction. Both are "keep the
self-reference; make the use of it unaffordable".

Found while writing: drift D81 - SemPrf^K's bound is strict (y < Log(z,K)) in
Willard2000-TAB Definition 1 and non-strict (y <= Log(z,K)) in Willard2007-APAL
Definition 2. Nothing turns on it, but a refinement licensed to reformulate must
not unify it silently.

The stage is NOT complete. B4 (compare Beklemishev) is blocked on gap G7 - the
survey that is the stage's named inspiration is not held, so whether this
identification agrees with his simplification is undeterminable. B5 (make the
criterion a definition and prove the five dials are instances) is open, and is
where the Refinement would become mathematics rather than exposition.

## 2026-09-01 - C16 image queue Q1-Q26 closed; review of the C16 work

Reviewed the C16 component and cleared the residue it had left open.

The mechanical half holds. I red-tested M1 against the real historical failure
rather than the synthetic one: deleting Willard2000-TAB#Lem8 produces
"AUDIT-FAIL: M1[Willard2000-TAB]: swept label 'lemma 8' has no results.md row",
and audit.sh sources audit-m.sh, so it fails the merge gate. The eight waivers
are all Tier D scans with no text layer; 25 papers are checked for real. R5
found six new cited-only obligations of exactly the O60 class (O2, O23, O52,
O53, O59, O93), and A-D's count of 67 drift entries with D52 and D58-D69 never
issued is accurate.

The objection was the disposition of Q1-Q26 - the mathematics quotations and
pre-rule glyph rows - as "carried to Refinement". That is wrong twice over:
Refinement is the stage that departs from Willard's presentation, so fidelity
verification cannot live there, and 26 image checks is a few hours, not a stage.
Cleared instead.

Result: 24 confirmed, 2 defects.

- Q21: Willard2006a Definition 1's Concise Encoding constraint is q < 2^(p^R)
  for a constant R > 0, not q < 2^p. Both results.md and the extraction record
  had dropped the exponent, which is load-bearing - Willard calls even the
  correct bound "somewhat excessive".
- Q25: the record claimed Willard2020-LFCS Appendix A transposes rules 5 and 6
  relative to Willard2020's Appendix. Both were image-verified and order them
  identically - rule 5 universal, rule 6 existential. The erratum is false and
  is withdrawn. The queue's own page anchor was also wrong: pp. 18-19 are the
  reference list, Appendix A is p. 15.

Two smaller notes. Q8's second half ("substantially more than 1,000 bits") is
not on Willard2002a p. 12 and remains unanchored. And Q12 turned up an
apparatus-generality claim earlier than any recorded: Willard2001 Remark 5.6
extends Theorem 5.1 to Herbrand deduction, Resolution and the cut-free sequent
calculus - 2001, before the 2002 claim O80 records as the earliest.

A-R is now met with no carried R1/R7 residue.

## 2026-09-01 - C16 AAR (Audits, acceptance, After Action)

Component C16 complete per [`codification/C16-verification-goal.md`](codification/C16-verification-goal.md).
Review record: [`codification/concordance/C16-review-record.md`](codification/concordance/C16-review-record.md).

### Acceptance

| Criterion | Result |
| --- | --- |
| **A-M** | `codification/audit.sh` **AUDIT PASS** with M1–M8; each red-green tested (`audit-tests.sh` 13/13) |
| **A-R** | Review record filed; every R1–R7 finding dispositioned (`corrected` / `accepted, with reason` / `carried to Refinement`) |
| **A-O** | All 87 obligations dispositioned (52 discharged / 6 cited-only with reason / 29 carried) — [`C16-R5-dispositions.md`](codification/concordance/C16-R5-dispositions.md); Status column written back |
| **A-D** | All 67 existing drift entries dispositioned (31 resolved / 31 remains-open / 5 carried) — [`C16-R6-dispositions.md`](codification/concordance/C16-R6-dispositions.md); D52, D58–D69 never issued |
| **A-T** | T1–T5 **PASS** against `codified-sjas.md` alone after correction pass — [`C16-T1-T5.md`](codification/concordance/C16-T1-T5.md) |
| **A-A** | This entry |

### Deficiency table (§1 of the goal) — what each hardening caught

| # | Deficiency | What C16 hardening caught / did |
| --- | --- | --- |
| 1 | Coverage ≠ inventory | **M1** green only after exclusions declared + Lem4.8/4.9, Def4.2/4.4 added; variant papers (WoLLIC, LFCS) must declare journal-key exclusions rather than silently omit |
| 2 | Overstated `full` statuses | **M7** advisory queue; R4 priority list; incidental G.2/`full` vs G.3/`sketch` correction in §5.6 during T2 pass |
| 3 | Text-layer corruption in registries | R1 math → image-queue (Q1–Q26); P3/M1 exclusions; charter Visual control restated |
| 4 | Stale derived claims | **M8** stale-dependency queue; D42 overclaim in §7.3 **corrected**; O49 text noted stale vs G35 |
| 5 | Cross-paper misattribution | Dominant Stage 5 finding (batches 1–2): 10 corrections including IQFS expansion, `[45]`≠Willard2009, Almost-M attribution, conference/journal swaps |
| 6 | Non-verbatim quotation | Sampled deficiency quotes already repaired in C15 draft; APAL "One of these systems" form verified; batch 1–2 found 7 not-verbatim (mostly attribution, not wording) |
| 7 | Elision moving ground | §6.7 Remark 4 ground clause present; batch 2 found first deficiency-7 instance in `willard1993.md` growth caveat — **carried** |
| 8 | Near-duplicate rows | **M2** enforces Paper+Label uniqueness (red-green) |
| 9 | Counts drift | **M5** caught README 514→528; obligation citation 57→53; drift denominator added to §10.2 |
| 10 | Obligations cited-only | **R5**: six cited-only with reasons (O2, O23, O52, O53, O59, O93); **O60 itself discharged** in §8.1 |
| 11 | Components leave obligations open | **R6**: D29 verified resolved; 25 remains-open with findings documented |
| 12 | Batch edits abort silently | **P1** observed; audit-tests mutate-then-restore pattern |
| 13 | Pre-rule registry content | R7 worklist = image-queue + pre-2026-08-27 formula rows — **carried to Refinement** (size exceeds one component; declared) |

### Artifacts added

- `codification/audit-m.sh` — M1–M8
- `codification/audit-tests.sh` — red-green harness
- `codification/concordance/C16-review-record.md` and supporting R5/R6/T1–T5/R1-batch files
- M1 exclusions/waivers in extraction records; four new `results.md` rows

### Carried to Refinement (explicit)

- Image-queue Q1–Q26 (math quotations; pre-rule glyphs)
- R6 remains-open document fixes beyond D42
- 29 carried obligations (tangibility cluster, floating-point cluster, 1993 mechanism block)
- Goal §6 cosmetic rename of "ADR-0001's Willard column"

### Mechanical note

`results.md` now **528** rows (was 524 at C15 close; +4 from M1). Registries:
528 results / 62 systems / 224 notation / 33 coverage / 37 gaps / 45 corpus /
87 obligations.

---

## 2026-08-31 - C14/C15 review corrections

A subagent review of the codified statement found 22 findings; I verified the
severe ones independently against page images before acting. All are corrected,
in the document and in the upstream concordance entries three of them came from.

Severe, all confirmed:

- The sentence explaining why Ax-1/Ax-2/Ax-3 have opposite incompleteness
  properties was attributed to Willard2009. It is Willard2007-APAL p. 3 - two
  occurrences there, zero anywhere in Willard2009.
- Willard2007-APAL Theorem 6 is about semantic tableaux only, but sat in both
  the Herbrand and tableaux columns of section 7.4 and of the result matrix, at
  `full`. The Ax-1/Ax-2 Herbrand cell therefore had no Willard source at all;
  Willard2009 p. 18 credits it to Adamowicz-Zbierski and Salehi.
- Section 9.1 called Willard2004 Theorem 3 "proved". It is `sketch`, and G35
  accepts that permanently. The frontier argument rested on the word. D34, D38
  and O46 carried the same stale claim - they predate the pass that produced
  G35 - and are corrected too.
- Section 6.7 quoted Willard2005 Remark 4's "no useful analog ... under any
  possible deduction method" after the words "and consequently", deleting the
  clause that attributes it to a separately cited theorem in [68]. The
  apparatus-independence is inherited from Willard2002a, not read off the
  compactification machinery, and carries that paper's sketch status.
- "nearly 100% certain" was not a quotation; the source says "essentially 100 %
  confident", as our own G21 already recorded.
- Section 6.1 said "None implies another" and then that 2005's hypothesis is
  weaker than 2020's, which contradicts it. Corrected with the direction stated.

Two further reported items, both verified and both real:

- D29 required the codification to state where Type-Almost-M sits, and it did
  not. It is now placed: the four-class lattice is Axis 1 and Almost-M sits
  beside it as an independent coordinate, being the axiom-versus-theorem
  distinction rather than a totality-strength one. Willard2011 Appendix D says
  in terms that xi^R is Almost-M, so it has been moved out of the Type-A row in
  both section 8 and the result matrix, alongside Ax-3. D29 discharged.
- Willard2000-TAB Lemma 8 had no results.md row. The defect was larger than
  reported: the paper has 17 numbered items and the registry held 8. All 11
  missing items are now recorded from the source - Definitions 1-3, Lemmas 1-4,
  7-9, Theorem 2 - including Lemma 8, which is the lemma section 5 says the
  negative proof "collapses entirely" without, and the subject of O76.
  results.md 513 -> 524.

Also fixed: the Kleene/Rogers/Jeroslow quote was orphaned and is Willard2005's;
Willard2007-APAL Lemma 1(i) restricts LEM to Delta_0 sentences, which is a point
on the Z dial and was flattened to "arbitrary"; the NS inconsistency claim was
over-quantified; the 1993 Main Conjecture's refutation is a codifier's inference
(O8), not a Willard result, and now says so; Root rounds up in four papers and
down in two, not one; three counts and two internal cross-references; a
quotation convention now distinguishes verbatim Willard from editorial
restatement.

Sections 6.3-6.6 - the engine and every constant in it - were checked against
page images and found faultless, including the direction of the density
inequality.

## 2026-08-31 - C15: codified-sjas.md full draft

All ten chapters written; ~1250 lines. Sections 6-9 composed from the C13
concordance and the registries, with every cross-reference verified to resolve
(57 obligations, 12 drift entries, 6 gaps, 5 relative links - no dangling ids).

Section 6, the preservation engine, states Willard2005's machinery in full: the
three inequivalent "consistency preserving" hypotheses; the envelope rebuilt
five times from Willard2001's Phi^i_j through Willard2011's Scope_E/sharp;
Normed(a,b) and theta-Compactification; Theorem 1 as an apparatus-NEUTRAL
engine, with everything apparatus-specific pushed into a single hypothesis;
Lemmas 1-2 with the Probe algorithm and its nine subcases; and the window
1/5 <= theta < 1/3 with theta = 1/4 proved, correcting ADR-0001's inverted
"theta <= 1/5". Remark 4 closes the circle: the engine is impossible once
multiplication is admitted, under ANY apparatus.

Section 7, the negative side, gives the Paris-Wilkie question answered in two
opposite directions by two Willard papers; the three places he names the exact
lemma that dies when multiplication stops being total; the negative controls;
the Ax-3 row where one theory under three axiomatizations has opposite
properties; the Level dial; and an explicit subsection on what the negative side
does NOT establish - four deferred proofs, four sketch statuses, one unproved
generality claim.

Section 8 states the boundary once and gives its five coordinatisations, with
the warning not to merge the R dial with the Z dial. Section 9 shows that
distinction is exactly what makes the Pi*_1 conjecture statable: it is the claim
that Willard2004's proved positive transfers from one dial to the other. It also
records why Conjecture 6.6 is hard - its analog is false when Successor replaces
theta, and ++ is proved by using that falsity.

Section 10 carries the provenance apparatus and the two extraction hazards,
including the U+2127-as-digit-zero case that forced the unconditional
visual-control rule.

Two corrections made from my own page reads while drafting: Lemma 1's induction
is nine subcases = one axiom case plus one per elimination rule, not nine rules;
and results.md carried two rows for Willard2005 Remark 1, now merged (513 rows).

Next: C16 - acceptance tests T1-T5, the results/statement cross-map (criterion
A2), the obligation register walk, and the AAR.

## 2026-08-31 - C14: codified-sjas.md skeleton and core statement

Ten-chapter skeleton laid down; sections 1-5 written in full - the core
statement of what an SJAS is. Sections 6-9 carry their sources, their
obligations and their gap markers, ready for C15.

Written:

1. Motivation and the historical arc - framed by Willard's own Q-1/Q-2 split of
   Hilbert's Second Problem (Q-1 is answered no; the whole corpus answers Q-2),
   Hilbert's and Godel's statements * and **, the two independent chains of
   testimony about Godel's late private view (Sacks direct, Nerode via
   Tennenbaum), and the four places Willard states his own limits.
2. Preliminaries - generalized arithmetic, self-justification with the
   axiom-or-theorem disjunction, threshold/anti-threshold with axiom
   containment, Solovay's April 1994 theorem, and the derivation of the corpus's
   two branches from it.
3. Language profiles - the Type-M/A/S/NS classification, why Type-S is empty,
   the six/seven/eight grounding-function sets, and the eleven formula-class
   relativisations with the only two relations ever stated in print.
4. Deductive apparatuses - the five named apparatuses, the apparatus-identity
   thesis in Willard's own voice (extensionally degenerate, intensionally
   decisive), the two unrelated senses of "cut", a fixed tableau definition with
   its two travelling conventions, the three devices that recover the cut, and
   the five unproved apparatus-generality claims.
5. The construction - the four axiom groups, the consistency-level axis, the
   growth restriction as the mechanism of the entire corpus (with the negative
   proofs shown to consume the same device), axiom-versus-theorem, the Hilbert
   line as a study of naming conventions, and the three finiteness reductions
   with the price each charges.

Also recorded: obligations O81-O89 were never issued. Eighty-seven obligations,
ids unique, none lost; the block is left unallocated rather than renumbered,
because the ids are cited from results.md, the drift ledger and the extraction
records.

Next: C15, the full draft - sections 6 (preservation engine), 7 (boundary and
negative side), 8 (result matrix) and 9 (frontier).

## 2026-08-31 - C13 Concordance complete

Sanity check of the C11/C12 work first (committed as f2b1789): audit green,
coverage and corpus keys agree 33 = 33, no duplicate ledger ids, the U+2127
hazard did not recur, and two load-bearing rows spot-checked verbatim against
rendered pages (Willard2001 Remark 4.5 footnote 7; Willard2006a Hybrid(H)).
Three corrections: audit.sh trimmed table fields with xargs, which truncates at
an apostrophe; Willard2001's coverage Read range was 2-67 against Images 1-67;
a stray empty page-images/willard1998/ directory.

C13 delivers four concordance files:

- `axes.md` - the five variation axes (language profile, apparatus, consistency
  level, axiomatization finiteness, admission condition), with every system
  placed and the reason Type-S is empty.
- `genealogy.md` - canonical names FROZEN, with the map from every paper's
  notation. The headline finding is that 62 registry rows collapse to about
  twenty systems: SJAS-Tableaux-Level1 alone has five notations across five
  papers spanning 1998-2020, and no paper says so. Also a name-collision table
  ("cut", Level-k, Q_0, "Consistency Preserving", IS-1*, U+2127).
- `result-matrix.md` - the (profile x apparatus) grid with consistency level and
  proof status per cell. Reading it honestly: the affirmative side is mostly
  full, the negative side is mostly sketch, and the two dials with exactly
  located thresholds are stated-only on both halves.
- `replication-map.md` - nine blocks that account for most of the repetition
  across ~600 pages, each with the source to present it from once.

Registry alignment: 322 results rows retopiced into 24 canonical topic slugs
derived from the axes; audit.sh now rejects any other (red-green verified).
Canonical system names frozen in systems.md. Proof-status counts unchanged
through the migration (137 full / 74 sketch / 40 cited / 58 stated-only /
205 n/a).

Next per charter: C14-C15, composition of codified-sjas.md.

## 2026-08-29 - Components C11 and C12 complete (checkpoint)

- **C10** was already done (2026-08-27; G31 closed). No unfinished Tier B work.
- **C11** done: all witnessed Tier C core + motivation items extracted;
  `Willard1997` remains blocked (G2). `audit.sh` **AUDIT PASS**.
  Registries at **514 results / 62 systems / 224 notation**; coverage
  **33 complete**. ADR-0001 C11/C12 lines updated.
- **C12** done: nine Tier D witness records. Gap searches: G30/G35/G37
  accepted (no longer versions in nachlass); G3/G36 remain open (ASL-2005 TR).
- Next per charter: **C13 Concordance** (axes, result matrix, freeze
  canonical system names). User checkpoint before commit/push.
- Working `page-images/` renders gitignored (regenerable); do not commit
  binaries. Ask before committing the C11/C12 logical unit.

## 2026-08-29 - Willard2007-APAL C11 extraction (closes G32)

- Finished the incomplete Tier C extraction: full-document visual pass
  (Images **1-48**), 44 results / 3 systems / 11 notation rows, corpus
  `extracted:2026-08-29`. Recovered **`℧`** (text layer digit `0`); Fact 2
  added. **G32 closed**; O70 discharged. `Willard2009` Theorem 5 remains
  `sketch` in its own paper. Drift **D51**: `Q₀` is eight axioms here, nine
  in Willard2009.

## 2026-08-29 - Willard1998 C11 extraction (last pending C11 item)

- Fully extracted **Willard1998** ("Self-Reflection Principles and NP-Hardness",
  DIMACS vol 39, pp. 297–320) at
  `nachlass/codification/extraction/willard1998.md` (core template §§1–8).
- Witness: image-only volume `lit/Dimacs1996.pdf` (170 pp., two-page spreads).
  Chapter on PDF **158–170** = printed **297–320**. Visual control: every
  chapter spread rendered `pdftoppm -r 130` and read as images (both halves).
- OCR search aid: `sources-text/willard1998.txt` (chapter only; never formal
  anchor). **G10 closed/accepted** as chapter-only OCR aid; full-volume
  collation not required once the chapter is extracted.
- Content focus recorded: Self-Justifying (two conditions); TangPred /
  TangDiv_k / TangRoot_k; Tangibility Reflection Eq. (2); ISREF(A) + 4-tape
  UTM; NP / P=?NP via ISREF^R and Thms 9.5–9.6; dropping Mult/Add/Successor
  totality. Related carefully to Willard2001 (ISTR proofs cite Willard1997,
  not this paper).
- Drifts **D79** (TangPred 1998 vs 2001), **D80** (Root ceiling vs floor);
  obligation **O96**. Registries: systems / results / notation rows;
  corpus `extracted:2026-08-29` (Pages stay 170 = volume); coverage Pages=13
  (chapter PDF spreads, State complete, chapter-only clarified).
- ADR-0001 C11 → **done 2026-08-29**; Willard1997 remains blocked/G2.
- Do not commit (task constraint). Prior C11/C12 uncommitted work left intact.

## 2026-08-29 - Tier D C12: nine witness-grade nachlass extractions

- Extracted all nine Tier D witness items under `nachlass/codification/extraction/`:
  `DEW-Resume.md`, `dew-2020-incompleteness-notes.md`, `dew-2008-zcf-group.md`,
  `dew-2014-notarized.md`, `dew-hajek-correspondence.md`,
  `dew-2025-boundary-draft.md`, `dew-2025-tab-xtab-notes.md`,
  `dew-2025-hilbert-draft.md`, `dew-1991-nsf-report.md`. Visual control:
  every page of every witness rendered `pdftoppm -r 130 -png` under
  `page-images/` and read as an image. Coverage rows all `complete`.
- **ZCF (O36 cited).** March 2008 notarized ZF/ZFC-inconsistency drafts
  (Boolean-4, Paradox-ZF(C), WZF, Statement ++). Unpublished draft claims —
  registered as `stated-only` with explicit "do not treat as established".
  Finite-set SJAS reading remains `Willard2001` Rem 3.8 (obligation **O94**).
- **Willard2021 pipeline scraps (G1 / O95 / D78).** `dew-2025-tab-xtab-notes`
  p.9 is a printed JLC-2021 **"Fine Line"** title page; `dew-2025-boundary-draft`
  opens with the JLC referee report preferring fine/delicate/subtle over
  "tender". Full JLC text still missing.
- **Hajek packet (D77).** Letter *to* Hájek dated 1976-08-17, signed **"Bill …"**
  — third-party interpretability note, not Willard-authored.
- **Gap searches G30/G35/G36/G37 (and G3):** not found in `nachlass/papers`,
  `collected_dew_materials`, or `lit`. **G30 accepted**; **G35 accepted**
  (Thms 2–3 permanently sketch); **G37 accepted** (deferred proofs stand);
  **G3/G36 remain open** for estate / SUNY Albany CS TR series.
- ADR-0001: C12 → done 2026-08-29; C11 Willard1998 completed separately (see entry above).
- Do not commit (task constraint). Prior uncommitted C11 work left intact.

## 2026-08-29 - Coverage Images catch-up (Willard2011 / 2001 / 2006a)

- Pre-existing audit failures: three `complete` rows still listed only the
  early formula-page Images subsets after the 2026-08-27 unconditional
  visual-pass amendment. Rendered all pages (`pdftoppm -r 130 -png`) and
  spot-checked previously unimaged formula pages (Fraktur / overbars / mho /
  towers). **No registry corrections** — Hybrid(H) `⌈2^{[Log i]^H}⌉`, Club ♣,
  varpi, hats, and towers match existing extraction/notation. Updated
  `coverage.md` Images to `1-64`, `1-67`, `1-45`. Do not commit.

## 2026-08-29 - Motivation-tier C11 batch (Willard2013/2014/2018/2007-KGS6/2006-Talk)

- Extracted all five motivation-tier items: `willard2013.md`, `willard2014.md`,
  `willard2018.md`, `willard2007-kgs6.md`, `willard2006-talk.md`. Visual
  control: every page of every witness rendered `pdftoppm -r 130 -png` and
  read as an image. Coverage rows all `complete`.
- **G33 / O73 resolved (hybrid).** Kept `Willard2014` as `motivation` for
  framing §§1-4, 7-10; extracted §§5-6 at **full core depth**. The finite
  system is printed **`IS_D^#(β)`** (superscript `#`); `Willard2016` Rem 7.3's
  "`ISD(β)`" is a naming conflation — drift **D76**. Theorem 3 + GlobSim_i
  give the kernel-image finite simulation of Group-2; §6 L-fold generalizes.
  Systems row `SJAS-Add-Level1-Finite`; full results rows for Defs 1-3, Thms
  2-3, GlobSim, Remarks 1-2.
- **G13 closed (accepted).** Dartmouth `willardtalk.pdf` is byte-identical to
  the nachlass 1-page abstract; no fuller Dartmouth slides found. WoLLIC 2006
  slides are a different talk (IΣ₀).
- Motivation distillates: `Willard2013` Miniaturized Finitism; `Willard2018`
  Rank-enriched tableaux preview of Tab/Xtab (author later calls it
  preliminary — Willard2020#Ref56); `Willard2007-KGS6` fourteen-year A-G map.
- Do not commit (task constraint).

## 2026-08-29 - Tier C Extraction: Willard2005-TAB (component C11)

- Extracted `Willard2005-TAB` (TABLEAUX 2005 / LNCS 3702; 15 pp. author copy)
  at full core depth: `extraction/willard2005-tab.md`. Visual control: all 15
  pages rendered `pdftoppm -r 130 -png` and read as images before formulas
  entered registries. Coverage row `complete` (Read 1-15, Images 1-15).
  Saturation re-pass: zero new numbered items.
- **Systems / results.** Positive: Theorem 3 — `IS-1(A')` with five `Pi*_1`
  simulated-real totality axioms recognizes Level(1) tableaux consistency;
  Theorem 6 strengthens to `IS_D(A')` under Tab-1. Negatives: Theorem 4
  (Hilbert vs `AddComp`) and Theorem 5 (Level(0-) vs `LongMult`). Theorem 7
  translates Tier(1)^⊕ → Tier(1) (stated-only; LinH deferred). Imported:
  Theorem 1 (Solovay), Theorem 2 (`IS`/`IS-1` summary).
- **Relationship to Willard2006b.** Conference precursor of the journal form
  already extracted the same day (drift **D70**): shared Defs. 1–4 and the
  truncated-vs-LongMult dial; theorem numbers diverge; journal is shorter.
- **Drifts / obligations / gaps.** D72 (JSL page-range typo 1189–1999), D73
  (Definition numbering restart), D74 (Thm 4 `Pi_1^-` vs `Pi_1*` slip), D75
  (`ISD`/`IS_D`/`IS-1*` naming). D70/D71 amended for the TAB witness.
  Obligations **O92** (Thm 6 ≠ restatement of Thm 3), **O93** (deferred
  proofs not discharged by journal form); O90/O91 amended to cite TAB.
  Gap **G37** opened for the three "longer version" deferrals; G3/G36
  amended (both published successors now extracted).
- Registries appended for this paper; left uncommitted for parent agent.

## 2026-08-29 - Tier C Extraction: Willard2006b (component C11)

- Extracted `Willard2006b` (JSL 71 pp. 1189–1199; 11 pp. author copy) at full
  core depth: `extraction/willard2006b.md`. Visual control: all 11 pages
  rendered `pdftoppm -r 130 -png` and read as images before formulas entered
  registries. Coverage row `complete` (Read 1-11, Images 1-11).
- **Systems / results.** Main positive: Theorem 2 — `IS_D(A')` with five
  `Pi*_1` simulated-real totality axioms (truncated `+ × - ÷` and `Expand`)
  is Tab-1 self-justifying. Main negatives: Theorem 5 (Hilbert vs simulated
  addition) and Theorem 6 (`LongMult` vs tableaux). Theorem 1 is cited from
  `Willard2005`; Theorem 3 translates Tier(1)^oplus to Tier(1).
- **Relationship to Willard2005-TAB.** Journal form of the TABLEAUX-2005
  announcement (footnote 1), but **not a renumbering or strict expansion**
  (drift **D70**): 11 vs 15 pp., theorem numbers diverge after Def. 4, Expand
  totality folded into Lemma 2, reference list collapsed to `[??]`.
- **Drifts / obligations / gaps.** D70 (TAB/journal inventory), D71 (Def. 3
  value formula destroyed by pdftotext — image governs). Obligations **O90**
  (truncated-vs-LongMult dial — corpus's only affirmative multiplication
  totality for an SJAS) and **O91** (Numerical Analysis vs Number Theory
  framing). G3/G36 amended: journal form now extracted; TR still sought.
- Registries appended; `audit.sh` run after this entry.

## 2026-08-27 - Tier B Extraction (component C10)

- Extracted all five Tier B items: `willard2002c.md`, `willard2000-tab.md`,
  `willard2006-wollic.md`, `willard2009.md`, `willard2016.md`. Registries at
  **311 results / 44 systems / 135 notation rows**; drift D45-D46; obligations
  O59-O66; **G21 closed**, G30-G31 opened. `audit.sh` green.
- **Willard2002c was badly under-ruled and was taken first**, as flagged at the
  last checkpoint. It is the source of the **Level hierarchy**, of **IS-1(A)**
  and its Group-3 `Pair`-form axiom (verbatim the shape of Willard2011 Eq. 37
  and Willard2020 Eq. 5), of **PROBE**, **(L,M)-Conservative Branches**,
  **Top**, **Check**, **Constraint** and **MaxVal** - the machinery Willard2004's
  appendix invokes while telling the reader to keep this paper "on his desk".
- **O59 - the rule that governs the whole corpus**: "it is desirable to use the
  **weakest** possible definition ... when one is seeking to **generalize** the
  Second Incompleteness Theorem. On the other hand, the opposite is true when one
  seeks to develop **boundary-case exceptions**." That single sentence explains
  why the negative papers sit at Level(0-) and the positive ones at Level(1).
- **The 1/2 constant is Willard2002c's.** Check(X,Y,P,Q) is
  `∀a [a <= (1/2)Max(P,Q)] ⊃ phi(a)`, and Lemmas 2 and 3 set L and M around it -
  which is where Willard2011's A-/E-Stability constant comes from.
- **D46/O60 - the corpus's sharpest located threshold is in a Tier B paper.**
  `R(i,j)` Tableaux-Hierarchy deduction carries **two** indices, and Willard2002c
  reports the collapse at **R(1,1) -> R(2,1)**, raising the Pi index alone. Every
  successor - Tab_1List, Tab-U\*_1-List, Tab-1, and Willard2004's
  Tab-Pi\*_2/Tab-Sigma\*_2 pair - collapses the pair to one class and cannot
  express it. The R dial of D38 is a projection.
- **D45**: four papers use four different truncation operators for one job -
  Willard2002c's Psi^M bounds universals only, Willard2001's bounds all unbounded
  quantifiers, Willard2005 splits the two bounds, Willard2011 re-merges them.
- **Willard2000-TAB** is the conference form of Willard2002a. The journal version
  is a strict superset, but four constants differ: V_2 has four clauses not six,
  Lemma 5 uses 2^3000 not 2^1000, Lemma 6 bounds O(s·n^m) not O(s·n^2m), and
  Lemma 10 is a sketch. It also cites an "**unabridged version** ... which the
  author can mail to any interested readers" that is neither this chapter nor
  Willard2002a - gap **G30**.
- **Willard2009 needed a new axis** (**O61**). Ax-1, Ax-2 and Ax-3 prove
  **exactly the same theorems** (Theorem 1), yet Ax-3\* is consistent and
  recognises its own **Herbrand** consistency (Theorem 4). The evasion comes from
  the **axiomatization**, not from weakening the theory - via **Split
  Representations**, base-(a_0+1) digit tuples that let every quantifier bound be
  a Max. Its **Threshold / Anti-Threshold** vocabulary (**O63**) is the corpus's
  cleanest for boundary results, and its "contains" means **axiom** containment -
  the same axiom-versus-theorem distinction behind Type-Almost-M and
  Willard2020's LEM result.
- **O62 - the Paris-Wilkie 1981 question gets a split answer**: Willard2002a
  Theorem 6.4 affirms the tableaux half, Willard2009 Theorem 4 refutes the
  Herbrand half. **O64** records the counterweight: Willard2009's own Theorem 5
  shows the same Ax-3 **obeys** the tableaux version, so Theorem 4 must never be
  quoted alone. Provenance: the paper answers an email from **L. A.
  Kolodziejczyk, 16 November 2005**, out of conversations with Adamowicz and
  Zdanowski.
- **G21 closed by extracting Willard2016.** The claim is **Conjecture 6.6**:
  `Log_2 K / Log_2 P < 1/6` for a Size-K Breaking Point and a proof P of 0=1.
  **Theorem 6.7 - IQFS's consistency preservation - is conditional on it**,
  making this the only affirmative result in the corpus resting on an unproved
  lemma. Willard is "essentially 100% confident" and calls the appendix's
  justification "one tiny iota short of a formal proof"; the iota is that it
  bounds the **canonical** route to large integers, not every route (**O66**).
  The `1/6` is the corpus's recurring density constant again.
- **O65 - Willard's own summary judgement**, from Willard2016 section 9:
  evasions "rested on using arithmetics that were **weaker than traditional
  arithmetics**", and the Second Incompleteness Theorem's significance in
  refuting Hilbert's original objectives "is thus, simply, **undeniable**". With
  Remark 8.3's admission that an "I am consistent" proof is "a **quite skinny
  form of proof**", this joins O24 and O35 as the three places Willard states the
  programme's limits in his own voice.
- **Honest scope**: `Willard2009` pp. 18-33, the WoLLIC chapter, and
  `Willard2016` sections 1-5 and 7 were read at statement level, and none of the
  three has had a visual pass. Recorded as **G31** with the specific items at
  risk named - Appendix B's proof statuses, Eq. (28)'s `1/6`, and Proposition
  4.3's `O{[Log n]^3}`.

## 2026-08-27 - Correction to D42's supporting citation

- The G29 entry below, drift **D42** and obligation **O55** each said that TR
  93-10's printed p. 36 "states the identification directly" that admitting the
  Law of the Excluded Middle as logical axioms *is* admitting cuts. **It does
  not.** The passage is a hedged parenthetical about a single system: "(Curiously,
  the ZF *half* of this 2-part system **can be viewed as** performing deduction
  *with Gentzen-style cuts*, **if** ZF is defined to include the axiom
  `Phi v not-Phi` for each sentence `Phi`.)" That is one-directional, system-
  specific, and not asserted as a theorem.
- Two of the three places also quoted it with "[it]" substituted for "ZF", which
  generalised a claim Willard made about ZF alone - a paraphrase presented as a
  quotation.
- **D42's substance is unaffected**: it rests on printed p. 33, which *defines*
  "a tableaux proof with cuts" as semantic tableaux plus `Phi v not-Phi`
  admissible at any node - verbatim Willard2020's `Xtab`. That is a definition,
  and it carries the claim on its own. Printed p. 36 is now recorded as
  corroborating rather than as stating the identification.

## 2026-08-27 - Visual Pass over TR 93-10 (closing G29)

- Rendered and read the 23 pages of the collated TR that had never been seen as
  images: PDF 1-4, 7-10, 18, 23-24, 27-29, 32-33, 35, 37-38, and the four
  unnumbered **Figure pages** 58-61, which no earlier pass had covered either.
  Every page of the witness is now visually read; `registry/coverage.md` reads
  **10 complete, 0 partial, 0 unrecorded**. **G29 closed.**
- The pass was **not** confirmatory. It produced three drift entries, four
  obligations, and a decisive strengthening of D1.
- **Drift D1 is settled.** PDF 2's front matter: "A 12-page abbreviated version
  of this paper ... was published on **25 August 1993** in the Proceedings of the
  Third Kurt Godel Symposium ... **This new longer draft has been issued as
  SUNY-Albany Comp. Science TR 93-10.**" With printed p. 1's "Robert Solovay
  recently proved a new version of Godel's Incompleteness Theorem, **shortly
  after learning of our work**", the order is fixed: chapter August 1993 ->
  Solovay April 1994 -> this report, which carries a 1993 series number.
- **=> `Willard2020`'s `Xtab` is the TR's "tableaux proof with cuts", verbatim**
  (**D42**, **O55**). Printed p. 33: "Define a tableaux proof with cuts to be a
  deduction formalism identical to semantic tableaux, except that it is
  permissible to include the sentence `Phi v not-Phi`, for any sentence Phi, in
  any node of a proof tree." That definition is the whole basis of the claim.
  Printed p. 36 adds only a hedged parenthetical about **ZF** - it "can be viewed
  as performing deduction *with Gentzen-style cuts*, if ZF is defined to include
  the axiom `Phi v not-Phi` for each sentence Phi" - which corroborates the
  association but does not state an identification. So Willard2020's
  contribution is the negative theorem about an apparatus the corpus defined at
  the outset, not the apparatus itself.
- **=> The TR supplies the argument `Willard2020` section 7 omits** (**D43**,
  **O56**), which materially improves **G22**. Printed p. 35's footnote performs
  exactly Definition 7.1's Z-enrichment at Z = Delta\*_0 - "add to IS(A) an axiom
  `Phi v not-Phi` for each Delta_0 sentence Phi" - and explains why it is
  harmless: IS(A) already "can simulate a Delta_0 cut rule with only a
  **polynomial increase in proof length**", because Group-2 supplies
  `Prf_A(<Phi v not-Phi>) => Phi v not-Phi`. That is the no-additional-
  compression condition the Linear-Sum analysis needs, and it sharpens the open
  Pi\*_1 question to whether Group-2 still delivers those instances cheaply one
  level up.
- **=> The corpus's two branches are derived on printed p. 2** (**D44**,
  **O57**). Solovay's theorem forbids (a) a Gentzen method with a cut rule and
  (b) Addition-totality **together**; "Solovay's Theorem is the almost direct
  complement to our Propositions 1 & 7, in that these theorems show that
  self-verifying axioms systems can be constructed when **either** condition (a)
  **or** (b) is relaxed." Proposition 1 relaxes (a) - the tableaux line;
  Proposition 7 relaxes (b) - the Hilbert line that becomes ISREF and ISCE.
- **Machinery recovered**: `IS^s_d(A,G)` parameterises by deduction method,
  Group-1 axioms **and** the Group-3 self-verification class - three of xi's five
  components, with `s` the ancestor of SelfCons_k's index; a **five-way
  reflection taxonomy** whose strong-uniform Sigma_1 form uses the bounded
  `Phi^y_v`, ancestor of Willard2001 Definition 1.1; **Positive Normal Form** and
  the **Positive(Omega)** rewrite over **broad** quantifiers, the origin of the
  prenex\* convention behind O27 and O42; **ANCESTOR(N)**, **ancestor-consistent**
  and **maximal node**, the root of the (k,m)-Affirmative / G-good / Normed
  lineage; **primary/secondary deductions** and **sharp/intermediate
  contradictions**, the source of 2001's terminology.
- **Delta_0 cuts are safe and unbounded-quantifier cuts fatal**, stated twice
  (printed pp. 7 and 35) - the K-deduction threshold avant la lettre - with the
  reason cuts change everything: "Addition can then be employed to **implicitly
  define** a sequence that grows actually faster than a_0, a_1, a_2 ...".
- **"Tangible" is a 1993/94 word** (printed p. 30, "only very tangible Delta_0
  sentences"), so Willard2011's zero occurrences of "tangib" is the abandonment
  of a founding notion, not the fading of a mid-period device (**O58**). The same
  page cites **[Wi94]**, confirming gap G14.
- The four **Figures** were never previously read. Figure 1 lists **twelve**
  tableaux rules (8 in 2001/2005, 6 elsewhere); Figure 2 names the **terse
  proof** and the **pivotal branch**; Figures 3 and 4 give INVALID.1's and
  INVALID.2's inconsistency proofs. All four instantiate the branching-axiom
  construction, moving **D30**'s origin from Willard2002a back to this report.
- Also: floating-point multiplication as a total function is already in the TR
  (printed p. 6 footnote), so it does not originate in the unlocated ASL-2005
  companion; PA+ is said to carry **eight** Group-1 functions where the chapter
  lists **eleven**; and printed p. 1 states that **every** self-verifying system
  must breach a Hilbert-Bernays condition, "because of **either** the axiom
  system employed **or** the choice of deduction method".

## 2026-08-27 - Verifying the C4-C6 Coverage (closing G25)

- The four items extracted before `registry/coverage.md` existed are resolved.
  Coverage now reads **9 complete, 1 partial, 0 unrecorded**.
- **Inventory verification first.** An uncapped, **case-insensitive** sweep of
  all four confirmed **no numbered item is missing from any extraction record**.
  The earlier sweeps could not have shown this: hazard H1 joins "D EFINITION"
  into all-caps DEFINITION, which a case-sensitive `^Definition` test rejects.
  The sweep must be uncapped *and* case-insensitive.
- **Nine `Willard2011` items had no `results.md` row** - Facts B.1-B.3, Lemmas
  C.1-C.2, Definition D.2, Fact D.3, Remark E.2, Remark G.4 - because the
  extraction record carries them as *combined* inventory rows. Same defect class
  as the six `Willard2006a` items found in phase 1. All added.
- **`Willard1993` read to completion, and it is the richest surprise of the
  component.** Three findings reassign credit backwards: (a) the growth device
  that O44 and O50 attribute to `Willard2006a` and `Willard2020` is stated in
  full on p. 329 - additive sequences have proofs *longer* than the numbers they
  construct, multiplicative ones *exponentially shorter*, with the "atoms in the
  universe" illustration; (b) **"cut" is defined there as arising from "an
  unnecessary application of the Law of the Excluded Middle"**, so
  `Willard2020`'s result is the formalisation of a 1993 definition; (c)
  `Willard2011`'s generic configuration is foreshadowed by 1993's `IS(A,g,d)`
  "IS-like systems", whose `g` and `d` are two of xi's five components. The
  **apparatus-identity thesis (O38) is also stated in 1993**. Separately: the
  1993 Group-1 functions are **eleven bit-manipulation primitives**, not
  grounding functions, and its `Pi_1` is a genuinely different definition
  (bounded by the max of enclosing universals), making D9/O12 a definitional
  rather than notational drift. The **ZF result** is the 1993 root of the 2008
  ZCF/ZF nachlass material.
- **`Willard2005` read to completion.** Its own framing is **limited modus
  ponens** (K-deduction, K=1 feasible and K=2 provably not) - a fifth boundary
  dial. `Tab-R-List` is defined here too, and **its endpoints are the two
  apparatuses**: R empty gives tableaux, R = all sentences gives
  Hilbert-equivalent efficiency, which is the structural reason the R and Z
  dials behave alike. Section 7 gives a sharp `Sigma\*_1`/`Pi\*_1` dichotomy on
  adding a valid sentence as an axiom - the ancestor of `Willard2011` Lemma 6.1.
  Remark 7 identifies the unlocated ASL-2005 companion TR's content: on
  floating-point arithmetic `IS_D(A)` **can** recognise multiplication as total.
  The **Prenex\* root requirement** is present in 2001 and 2005 and absent from
  2002a, 2004 and 2020, which settles obligation **O42**'s scope.
- **`Willard2011`**: coverage taken from its own saturation record's full
  text-layer read, corroborated by the sweep and a re-read of sections 1 and 4
  and Appendices A-C. **D29 sharpened**: Type-Almost-M is not a totality-strength
  class at all but the **theorem-versus-axiom** distinction - alpha proves the
  two totality statements *as theorems while treating none as axioms* - which is
  exactly the axis `Willard2020`'s result turns on. Appendix A's "Reminder about
  Equation (37)" explains the `k <= 1` restriction: SelfCons_k is **false** under
  Standard-M for nearly all (beta,d) once k >= 2 - 2011's version of the ceiling
  that 2004 calls Level(2+) and 2005 calls K=2.
- **`Willard1993-TR` remains `partial`, deliberately** (gap **G29**). It is an
  OCR witness where page images govern; C5's record enumerates which pages had a
  visual pass, and about seventeen printed main-body pages were covered only at
  OCR-sweep level. Every numbered item and both appendices were visually
  verified at C5 and the sweep found nothing missing, so the risk is low - but
  `complete` would be a false claim.

## 2026-08-27 - C9 Completion Reads (closing G23)

- Read to completion the ~92 pages left unread at C9: `Willard2002a` pp. 15-19
  and 29-33, `Willard2004` pp. 1-3 and 10-32, `Willard2006a` pp. 1-2 and 9-45,
  `Willard2020-LFCS` pp. 1-14, `Willard2020` pp. 1-3. `registry/coverage.md` now
  shows six items `complete`, none `partial`. **G23 closed.** Registries at 262
  results / 39 systems / 116 notation rows.
- **The C9 sweep had a hole.** Its awk recipe filtered items with
  `length(l)<95`, silently dropping any whose first line ran longer. The
  uncapped re-sweep found `Willard2002a` **Claim A.2** and **Definition 6.5** and
  `Willard2020-LFCS` **Example 1**; a registry cross-check found six
  `Willard2006a` items in the extraction record but not in `results.md`. All now
  recorded. Gap **G28**, closed. The sweep recipe must never carry a length cap.
- **Ten proof statuses were wrong**, all in the direction of overstating what
  was proved: Willard2004 Theorem 2 -> sketch, Theorems 4 and 5 -> cited,
  Lemmas 5 and 7 -> sketch; Willard2006a Theorem 3\* -> stated-only, Corollary 1
  -> sketch, Lemma 3 -> sketch, Theorem 5 -> cited, Theorem 6 -> sketch.
- **`Willard2004` defers four things to "a longer version of this article"**
  that appears never to have been published - including the details that would
  complete **Theorem 2**. Since Willard2020 section 7 cites exactly that theorem
  for the Pi\*_2 failure, **the Pi\*_1 conjecture is flanked by a Delta\*_0
  positive that is stated-only in 2020 and a Pi\*_2 negative that is sketch-only
  in 2004.** Gap **G26**.
- **The boundary has three dials** (**D38**): Willard2004's R (which intermediate
  theorems a TabList proof may use), Willard2006a's **Hybrid(H)** (naming growth
  rate, C_i = ceil(2^([Log i]^H))·C_(i-1)) and Willard2020's Z (which LEM
  instances are logical axioms). Willard never relates them. **Hybrid(H) is the
  only one whose threshold is stated exactly** - H = 1 positive, H > 1 negative.
- **Willard2020's LEM-as-axiom device is already in Willard2004** (**D39**):
  W_4 and W_5 are `∀..[Prf ∨ ¬Prf]` and `∀..[Subst ∨ ¬Subst]`, exactly Definition
  7.2's shape for two Sigma\*_0 predicates, and Lemma 5 uses them to generate its
  two ∨-Elimination splits. Sixteen years earlier, in restricted form.
- **Willard2004's appendix already contains the envelope machinery** (**O54**),
  under the name **G-good**: a Sigma\*_1 sentence is G-good iff its witnesses are
  bounded by 2^I(H), a Pi\*_1 sentence iff its universal bounds are G·2^(-I(H)),
  and "it is impossible for both X and Y to have G-good proofs". That is
  Willard2005's Normed(a,b) and Willard2011's Good(N), plus obligation O27's
  impossibility step, dated 2004.
- **Willard2002a section 7 is the mirror of Willard2001 Remark 4.5**: the
  tableaux G2 collapses under multiplication-as-relation "essentially [because]
  Lemma 4.7 ... would then become no longer valid". 2001 localises the *positive*
  engine's collapse to Lemma 4.2; 2002a localises the *negative* theorem's to
  Lemma 4.7 - **and both lemmas are about the squaring sequence** (O44).
- **Willard2006a Remark 3 is the cleanest statement of the growth restriction in
  the corpus** (**O50**): a proof of N's existence has a *larger* Godel number
  than N under additive naming and a *sharply smaller* one under multiplicative
  naming, "and this difference in magnitude is the reason the Second
  Incompleteness Theorem will generalize under the multiplicative naming
  paradigm".
- **A second frontier problem** (**G27**, **O53**): Pudlak's private-communication
  question of 2001, whether an **Extender Function** can build a self-justifying
  system of Infinite Far Reach. Willard defines IS.Extender(A), conjectures it
  consistent, and shows monotonicity provably fatal (Theorem 7). Carry it
  alongside Willard2020's Pi\*_1 conjecture.
- **Density is load-bearing for tableaux, merely clarifying for Hilbert**
  (**D41**, **O51**): Willard2006a twice disclaims the necessity of compressed
  encodings for its own results, where the tableaux engine genuinely fails
  without it.
- **Willard2006a explains why the corpus carries two toolkits** (**D40**,
  **O52**): definable cuts suit Hilbert, generalised Godel sentences suit
  cut-free methods. Its 11-item literature survey also identifies **Solovay's
  never-published thinning construction, proved at Hajek-Pudlak pp. 172-173** -
  the exact citation Willard2020 gives for statement (circled dot), completing
  obligation O48's chain.
- **Willard2002c is more load-bearing than its Tier B ruling suggests**: it
  holds IS-1(A), PROBE, (L,M)-Conservative Branches, Constraint(p,beta), Lemmas
  1-t/2-t/3-t and Theorem 2-t, and introduced the Level hierarchy. Flag for C10.
- **One of my own C9 claims was wrong and is corrected**: I had recorded that the
  Declarative / Infinite-Ranged distinction was in the LFCS chapter and absent
  from Willard2020. It is on Willard2020 pp. 2-3, which I had not read - an
  inference drawn from an unread range, which is precisely what the coverage
  discipline exists to prevent. Willard2020's record gains a section 2a for it.

## 2026-08-27 - Coverage Discipline: making "what was read" machine-checkable

- **The C9 coverage claim was false and is corrected.** `registry/gaps.md` G23
  had said `Willard2002a` and `Willard2020-LFCS` "were read in full". Neither
  was. Actual ranges are now in the new `registry/coverage.md`: Willard2002a
  pp. 1-14 and 20-28 read (**pp. 15-19 and 29-33 unread** - the second half of
  section 4's proof of Theorem 3.4 including Lemmas 4.8-4.9, plus the Appendix
  body); Willard2020-LFCS pp. 15-17 read (pp. 1-14 not read directly, but
  item-for-item correspondent to Willard2020 sections 3-5, read in full at C8);
  Willard2004 pp. 4-9; Willard2006a pp. 3-8; Willard2020 pp. 4-28 plus a partial
  opening. The two overstated saturation lines were corrected too.
- **Root cause**: coverage lived only in narration, where nothing could check
  it. Working agreement 5 - once an item is ruled core, every part is extracted
  - already forbade the underlying error and did not prevent it. So the fix is a
  mechanical check, not a restated rule.
- **New `registry/coverage.md`**: one row per extracted item with Read / Swept /
  Images / State (`complete` | `partial` | `unrecorded`). `audit.sh` now fails
  when an item marked `extracted:` in `corpus.md` has no coverage row, and when
  `complete` is claimed over unrecorded ranges. Both checks verified red-green.
  Current tally: 1 complete, 5 partial, 4 unrecorded.
- **New `unverified` proof status** in the results registry enum. The previous
  enum (full | sketch | cited | stated-only | n/a) had no way to say "I have not
  read this proof", which is part of why plausible statuses were entered for
  unread ranges instead of honest ones.
- **Charter amended** with a Coverage discipline section; criterion **A1** now
  requires a coverage row in state `complete` rather than a prose assertion.
- **Gap G25** opened and accepted: the four C4-C6 items predate this registry
  and their ranges are not reconstructible, so they are marked `unrecorded`
  rather than guessed.
- Working agreements 6-9 added to persistent memory: record coverage as ranges
  and never as "read in full"; split an oversized component at the start rather
  than compressing depth silently; record ignorance rather than a plausible
  guess; never report an edit before the tool call returns.

## 2026-08-26 - Tier A Extraction: the Negative Side (component C9)

- Extracted four items to `nachlass/codification/extraction/`: `willard2002a.md`
  (33 pp.), `willard2004.md` (32 pp.), `willard2006a.md` (45 pp.) and
  `willard2020-lfcs.md` (19 pp.). Registries now at 249 results / 36 systems /
  104 notation rows; gaps G23-G24; drift D30-D37; obligations O43-O48.
  `audit.sh` green.
- **The single best structural finding of the component**: the doubling/squaring
  pair `x_i = x_(i-1)+x_(i-1)` versus `y_i = y_(i-1)*y_(i-1)` is the corpus's
  one unifying device, appearing on **both sides** of the boundary -
  Willard2001 Remark 4.5 footnote 7 (squaring breaks Lemma 4.2, so the positive
  engine collapses), Willard2002a Lemma 4.7 (the same squaring makes the
  *negative* proof's subtree short), Willard2006a Eqs. (2)-(3) (the additive and
  multiplicative **naming conventions**, an explicit design axis), and
  Willard2020 section 5 (only y_n's encoding is long enough to hold its own
  derivation). Obligation **O44**.
- **The Linear-Sum construction is eighteen years older than its name.**
  Willard2002a Lemma 6.3's five-step tableau construction is Willard2020 section
  6's four-step one with an *implication* axiom in place of *LEM*. The same fact
  recurs five times across the corpus, including Willard2002a's V_4
  CLARIFICATION - "a redundant axiom can super-exponentially shorten the length
  of some cut-free and/or Semantic Tableaux proofs" - written in 2002. Drift
  **D30**, obligation **O43**.
- **Willard2002a locates the boundary explicitly**: "it is the very act of
  changing Multiplication from a 3-variable relation to a total function which
  is the exact juncture point where the Semantic-Tableaux version of Godel's
  Second Incompleteness Theorem becomes valid." It also **answers a Paris-Wilkie
  open question of 1981** (Theorem 6.4). Remark 5.6 supplies the complementary
  positive: omega-consistent extensions of Q\* (multiplication as a relation)
  can prove what extensions of Q cannot, and Willard claims IS^lambda(A)
  **internally recognizes its own omega-consistency** - a strong, unsupported
  claim recorded as gap **G24**.
- **Willard2004 supplies the corpus's canonical Level hierarchy** (Level(0-),
  Level(n), Level(n+) over Q\*_n sentences) with the reason it is a real axis:
  the levels coincide in strong models but weak systems cannot prove the
  equivalence (**O45**). Its `Tab-R-List` deduction is the origin of
  Willard2020's `Tab-1` (**D33**).
- **The open conjecture is sharper than recorded at C8.** Willard2004
  parameterizes by R - which *intermediate theorems* a TabList proof may use -
  and **proves** the Pi\*_1 union Sigma\*_1 case at Level(1) (Theorem 3), while
  proving the Pi\*_2 and Sigma\*_2 cases fail (Theorem 2). Willard2020
  parameterizes by Z - which *LEM instances* are logical axioms - and leaves
  Pi\*_1 open. **Different dials.** So Willard2020's conjecture is precisely the
  claim that 2004's proved positive result transfers from the R dial to the Z
  dial. Drift **D34**, obligation **O46**, appended to G22.
- **Willard2006a opens an axis found nowhere else**: the **naming convention**
  for constant symbols. ISCE(A) is a **Hilbert-apparatus** self-justifying
  system using the additive convention, and Willard states it is Willard2001's
  Theorem 3.4 (ISREF(A)) upgraded from incremental naming - the upgrade buying
  the **Continuous Expansion** property that the incremental convention fails
  because "it grows too slowly" (**O47**). Section 6's ISINF(A) is
  simultaneously Infinitely Far-Reaching, self-verifying and Pi^-_1-complete
  for PA.
- **The C6-deferred Willard2006a subsumption check is closed, negatively.**
  Willard2011 absorbs ISCE(A) but drops the naming-convention axis, Continuous
  Expansion and Infinite Far-Reach entirely. This is the **second** element-wise
  failure of 2011's unification claim, alongside D21. Drift **D36**.
- **Willard2020-LFCS is not superseded by the arXiv expansion** - which reverses
  part of D27. Its Appendix B carries Definition 4 (**Locally-J-Closed**), the
  omega_j hierarchy, and **Theorem 3**, none of which appear in Willard2020, plus
  the only explanation in the corpus of how statement (circled dot) reaches
  Hajek-Pudlak's definable-cut machinery: LEM -> Linear-Sum Effect -> short
  proofs of the Locally-J-Closed conditions -> the cut diagonalization
  (**O48**). And its Theorem 3's proof is "postponed until a longer version of
  this article" - which the arXiv paper is not, since it drops the theorem. That
  is **direct textual support for G1's raised priority**, previously inferred
  only from Willard2021's title and date. Drift **D37**.
- Provenance corroborated: the LFCS chapter thanks only Seth Chaiken and still
  reads "mapping are" at Theorem 1 - the exact error at item 24 of
  `prose/willard2020draftreview.txt`, corrected in the arXiv version, whose
  acknowledgments add James P. Torre, IV. The D28 timeline holds.
- **Other drift**: "Conventional Deciphering Property" names two inequivalent
  conditions in 2002a and 2004 (**D31**); Definable Cut now has **four**
  inequivalent definitions across 2001, 2002a, 2006a and the LFCS chapter
  (**D32**), which means obligation O29's "tangibility is a definable cut minus
  successor-closure" must be re-checked against whichever definition C13 adopts;
  and the Non-Growth condition itself differs, 2006a alone using
  `Maximum(2, ...)` (**D35**).
- **Visual control passes** on `willard2002a` p. 14 (confirmed C ~= 12, the
  underbrace numerals, the squaring sequence), `willard2004` p. 4 (six grounding
  functions, 2·Log_2 N, non-growth without the 2), `willard2006a` p. 6 (seven
  functions, Logarithm = 1 + floor(Log_2 x), non-growth **with** the 2), and
  `willard2020-lfcs` p. 16 (omega_0(x)=x^2, omega_(j+1)(x)=2^(omega_j(2·Log_2(x+1))),
  omega_1(x) >= x^x, Definition 4's three clauses).
- **Honest scope note**: `Willard2004` sections 4-6 and `Willard2006a` sections
  3-6 with Appendices A-D were swept for numbered items and read at the level of
  statements, not line by line, so some proof statuses there are provisional.
  Recorded as gap **G23** with the action assigned to C13. `Willard2002a` and
  `Willard2020-LFCS` were read in full.

## 2026-08-26 - Two Boundary-Result Elaborations (C8 follow-up)

- Recorded at `nachlass/codification/discussion/2026-08-26-boundary-results-and-the-pi1-conjecture.md`
  (summary + verbatim transcript), covering two checkpoint questions about what
  the corpus's negative results actually establish.
- **`XIS^lambda(PAX)` is genuinely syntactically inconsistent.** Theorem 7.3
  establishes (75) unconditionally; Theorem 7.2's proof then derives both
  alpha |- Theta and alpha |- not-Theta for Theta = Gamma(n-bar). Three
  qualifications: the witness step is metatheoretic (two derivations exist, no
  size bound on either - obligation **O37**), inconsistency is of the axiom set
  and so apparatus-independent, and no step covertly assumes consistency. New
  observation: Eq. (71)'s consequent is Theta^x where Eq. (9)'s is Psi^x_z, so
  the schema is weaker than the literal analogue and the theorem correspondingly
  stronger (erratum 11).
- **The Pi\*_1 conjecture is one rung of a parameterized family.** Definitions
  7.1/7.2 make Theorems 4.4 and 4.5 the endpoints (Z empty, Z everything) of a
  single spectrum: Delta\*_0 holds, Pi\*_2 fails, Pi\*_1 open. Z is the right
  dial because section 6's four-step construction licenses the Linear-Sum Effect
  exactly for middle terms in Z - so the question is how complex the simulated
  modus-ponens middle terms must be before compression suffices to run the
  diagonalization.
- Two structural observations recorded in the extraction record, flagged as
  inference rather than Willard's statement: **Pi\*_1 is the class of
  IS_D(beta)'s own axioms** (Groups 1, 2 and 3 are all Pi\*_1) *and* of its
  Level-1 consistency claim; and `Upsilon v not-Upsilon` for Pi\*_1 Upsilon is
  already **Pi\*_2 in prenex form**, so the two indexings are offset by one
  level. Also: Definitions 7.1 and 7.2 must not be collapsed - 7.2 at Delta\*_0
  already admits Pi\*_1 *axioms*, just not Pi\*_1 *disjuncts*.
- **Section 7 asserts a boundary rather than establishing one.** The Delta\*_0
  positive and the Pi\*_2 negative are both unproved in `Willard2020`, so the
  conjecture cannot be assessed until those extensions are checked (gap G22,
  now cross-linked to G1).
- **G1's priority raised.** `Willard2021` is titled "About the characterization
  of a **fine line** that separates generalizations and boundary-case
  exceptions..." and appeared online 2020-12-30, ~7 months after
  arXiv:2006.01057. The title shift from the LFCS chapter's "tender line
  separating" to "characterization of a fine line that separates" reads as a
  move from *there is a line* to *here is where it is*, making it the likely
  location of G22's missing proofs and possibly of the conjecture's answer.
  Inference from title and date only - the paper is unseen. Acquisition is no
  longer merely a completeness item.
- Process note, recorded in the discussion file: in both exchanges I stated that
  records had been updated before the edits were made, and the edits followed a
  turn later. The registries are the source of truth; a claim of recording is
  worth only the commit behind it.

## 2026-08-26 - Extraction of Willard 2020, the Terminal Statement (component C8)

- **Extracted `Willard2020`** (arXiv:2006.01057v1, "How the Law of Excluded
  Middle Pertains to the Second Incompleteness Theorem and its Boundary-Case
  Exceptions") to `nachlass/codification/extraction/willard2020.md`. 16 numbered
  items plus 5 unnumbered results; registries now at 187 results / 31 systems /
  88 notation rows. `audit.sh` green, `example` added to the type enum.
  `nachlass/papers/willard2020.pdf` and the `lit/` copy are byte-identical.
- **`Willard2020` and `Willard2020-LFCS` are one paper at two lengths.** Willard
  states the "initial 19-page draft of this article" was the LFCS-2020 chapter
  [57]. Terminal lineage: `Willard2018` (arXiv, self-described as roughly
  written) -> `Willard2020-LFCS` (LNCS 11972) -> `Willard2020` (arXiv, 28 pp.)
  -> `Willard2021` (JLC, still gap G1). Drift **D27**: C9 should extract the
  LFCS chapter as a variant witness recording deltas only, not as an
  independent paper.
- **This repository sits inside the paper's provenance.**
  `prose/willard2020draftreview.txt` (2020-05-21, signed jpt4) copy-edits a
  looser-set manuscript of this paper - its page references map onto the
  witness at a consistent ~2.2x ratio - and several of its corrections are
  visibly applied in arXiv v1 ("mappings are", "fails to be *a*
  consistency-preserving mapping", "Type-NS", the comma in Theorem ++, the
  added YouTube URLs at refs. 18 and 39). One was not: p. 7 still reads "Self
  Justifying" unhyphenated. Willard's Acknowledgments thank Seth Chaiken and
  **James P. Torre, IV**. Drift **D28**; gap **G19** accepted rather than
  chased, since the arXiv version supersedes the manuscript.
- **Three things this paper states more clearly than any other in the corpus.**
  (a) The **apparatus-identity thesis** (Example 3.1, restated section 7): all
  common apparatuses prove the same theorems from a common basis, at different
  proof lengths - so the codification's apparatus axis is extensionally
  degenerate and intensionally decisive (**O38**). (b) The **growth mechanism
  on objects rather than proof-encodings** (section 5): with x_i doubling and
  y_i squaring from 2, Log(x_n)=n+1 but Log(y_n)=2^n, so y_n's encoding is long
  enough to hold the derivation defining it and x_n's is not (**O39**). (c) The
  **Linear-Sum Effect** (section 6): modus ponens bounds a proof of psi by the
  sum of the lengths for phi and phi->psi; Tab lacks it; Xtab recovers it via
  LEM in four tree steps - so **proof compression, not expressive power, is
  what activates the Second Incompleteness Effect** (**O40**).
- **The headline result**: Theorem 4.4 (IS_Tab and IS_{Tab-1} are consistency
  preserving) versus Theorem 4.5 (IS_Xtab is not - it is outright inconsistent
  once beta proves the usual Pi\*_1 arithmetic identities). Neither is proved
  here; Theorem 4.4's Tab-1 case is credited to Willard2005 and its **Tab case
  is unsourced** (gap **G20**), while Theorem 4.5 is summarised via the new
  statement (circled dot), which itself rests on Hajek-Pudlak pp. 172-174 and
  is explicitly not given a short proof.
- **The boundary is now exact and names an open problem.** Section 7's
  Z-Enriched and Z-Base-Variable-Enriched tableau: the evasions survive at
  Delta\*_0, **fail at and above Pi\*_2**, and the **Pi\*_1 case is an explicit
  open conjecture** - "this fact has not yet been formally proven". All three
  section 7 results and section 8's Res/Xres generalization are stated-only
  (gap **G22**).
- **Willard grades his own two decades** (p. 22): the 1993/2001 `IS(A)` was
  "simpler, but substantially weaker because it only recognized the
  non-existence of a proof of 0=1 from itself", where `IS_{Tab-1}(beta)`
  corroborates that no two proofs exist for a Rank-1\* sentence and its
  negation - the distinction mattering because the **First** Incompleteness
  Theorem denies any decision procedure separating true from false Rank-1\*
  sentences. Recorded as **O41**, the justification for treating Level-0 versus
  Level-1 as a codification axis rather than a presentational variation.
- **New unification device.** `GenAC` = (alpha, D) with D decomposed as logical
  axioms L_D union rules of inference. Where Willard2011's generic configuration
  parameterizes language and coding, GenAC parameterizes the apparatus - which
  is what the LEM result needs, since the theorem is precisely about migrating
  sentences between L_D and the derived theorems. Definition 3.2's
  Self-Justifying is a **fifth** definition of the term across the corpus.
- **Other drift.** The growth taxonomy has four classes here against
  Willard2011's five - Type-Almost-M is not a point on the axiom-counting
  lattice (**D29**). Willard2001's title is mis-cited *again* in ref. [47],
  the same error Willard2005 made, so it is a systematic self-citation error
  persisting 19 years (strengthens **D20**). The tableau Appendix drops
  Willard2001's prenex\* normalisation of the root, which Willard2001's
  footnote 8 relied on - **O42** requires the codified engine to fix one
  convention and re-establish the closing-pair fact under it.
- **Also resolved**: `nachlass/collected_dew_materials/2020Notes.pdf` is
  Willard's "Quick Notes on the Incompleteness Theorem (Oct. 4, 2020)",
  pedagogical lecture notes supplementing Mendelson with a March 2004 -> April
  2010 -> October 2020 lineage. Image-only, no text layer. It is not the
  reviewed draft, and its Tier D ruling stands.
- **Visual control pass** (charter requirement) on pp. 10, 12, 15, 17. It caught
  `pdftotext` rendering the statement symbol (circled dot) as `J` throughout
  section 6, and confirmed erratum 2 (p. 15's "2^(n+1)" should be "2^n + 1",
  contradicting Log(y_n)=2^n three lines above) as genuine rather than a
  text-layer artifact. Also noted: page markers need form-feed stripping before
  a `^=== PAGE` grep will match.

## 2026-08-26 - Extraction of Willard 2001, the JSL Foundation (component C7)

- **Extracted `Willard2001` in full** to
  `nachlass/codification/extraction/willard2001.md` (all 67 witness pages,
  including Appendices A-D and Tables I-II). 41 numbered items inventoried;
  registries now at 167 results / 25 systems / 73 notation rows. `audit.sh`
  green. Added `cor` to the audit's type enum - 2001 contains a genuine
  Corollary (6.2), which the enum did not admit.
- **The witness is not the JSL 2001 text.** Willard's cover note claims
  identity apart from type size, but reference `[45]` cites an APAL **2006**
  paper (cited nowhere in the body) while `[44]` still calls the JSL version
  forthcoming - a partial post-publication bibliography update. Drift **D22**,
  gap **G15**: authoritative for the mathematics, not for JSL pagination.
- **Nine systems, where 2005 and 2011 have far fewer**: `IS(A)`, `IS^lambda(A)`,
  `ISREF(A)`, plus `ISTR(A)` (Bitwise-Or), `ISM(A)`, `ISMULT(A)`,
  `XIS^lambda(PAX)`, `ISREF^U(A)` and `IS^(lambda U)(A)`. "IS" abbreviates
  **Introspective Semantics** - stated only here.
- **The multiplication picture is not what the later papers suggest.**
  `ISM(A)` recognises Multiplication as total and *is* consistency-preserving;
  what it loses is the ability to state "+++" ("I am unable to produce a
  Semantic Tableaux proof of 0=1"), because its Group-3 gap is too wide.
  Meanwhile `XIS^lambda(PAX)` - multiplication *plus* tangibility reflection -
  is inconsistent for every lambda (Theorem 7.3). So multiplication is fatal to
  reflection, not to consistency-preservation. Obligation **O31**.
- **Two apparent drifts dissolved on inspection.** (a) Seven grounding
  functions here versus six in 2005/2011, with opposite roundings on Logarithm
  and Root - but p. 10 says any finite Pi^-_1-axiomatised set of non-growth
  functions works, and Remark C.5 says such extensions still keep Group-3
  Pi^-_1. Presentational (**D23**). (b) The coding-density constants 6, 5, 6/5
  and 32 across four papers are one scheme measured differently: a byte is six
  bits, five of them base-32 payload plus a tag (**D25**, obligation **O33**).
- **The density argument is at its clearest here.** Case 11 of Lemma 4.8: the
  addition axiom's tertiary deduction at most doubles a parameter's bound, and
  encoding the sentence that does it costs at least six bits - exactly one unit
  of the `2^(beta/6)` budget. The budget covers addition and nothing faster.
  Remark 4.5 then localises the multiplication failure to the loss of Lemma 4.2
  alone (`u_(i+1) = u_i^2` gives `2^(2^n)`). Obligation **O30**.
- **Lineage confirmed.** Lemma 3.2 (Pi^-_1 transfers down into the finite model
  `M_i`) with Remark 3.3 (Sigma^-_1/Delta^-_0 transfers up) is the ancestor of
  2005's `Normed(a,b)` and 2011's A-/E-Stable pair; `Phi^i_j` is the ancestor of
  `App-forall/App-exists` and `Scope_E`; the pseudo-formula machinery (club
  placeholders, `PseudoTransform`, `ExSemPrf`) is the ancestor of 2011's
  `SubstPrf`/`Gamma^k(n-bar)`. Willard states that TR 93-10 and the 1993
  abstract "contained intermediate results that were essentially equivalent to
  the Assertion ++", confirming the C5 reading of the TR's Lemma 6.2.
- **Willard forecloses the ideal system.** Section 1 lists seven comparison
  criteria, credits the definable-cut literature with I-IV and his own systems
  with V-VII, and concludes it is "futile to seek an idealized form of
  self-verifying system". Recorded as obligation **O35** - a direct constraint
  on the Refinement stage's ambition, asserted by Willard about his own work.
  Related: tangibility is precisely a definable cut minus closure under
  successor (**O29**), which makes the relation to Nelson/Pudlak/Wilkie-Paris
  exact rather than atmospheric.
- **A claim from the 2026-08-21 discussion is corrected.** I had told the user
  that Willard's size threshold "lives entirely in the metatheory" in contrast
  to Rosser's trick. True of 2005, 2011 and 2001's `IS(A)`; **false** of 2001's
  schematic systems, whose Group-3 axioms carry the size comparison in the
  object language (Eqs. 9, 10, 16, 61). Recorded as drift **D26**; the verbatim
  transcript is left intact with a correction addendum appended. The Rosser
  contrast itself survives in corrected form.
- **New gaps.** G15 (witness/JSL divergence), G16 (five stated-only results,
  including the lambda range the system is *defined* over versus the single
  lambda = 3/4 actually proven, and section 8's computable-variant boundary
  catalogue), G17 (`Willard1997` also holds `ISTR(A)`'s proofs - raising G2's
  priority a second time; note 2001 misprints its venue as the "Third" rather
  than Fifth Kurt Godel Colloquium), G18 (Solovay's full theorem is stronger
  than the printed Appendix A and was never published - accepted).
- Eight further errata recorded in the extraction record section 7, including a
  genuine repeated `<`/`<=` mismatch between the Group-3 pseudo-formulae and
  their instantiations (Eqs. 89/90 and 95/96), image-verified on pp. 54 and 60.
- **Visual control pass** (charter requirement) on pp. 9, 10, 19, 22, 28, 51,
  54, 60. It caught three text-layer corruptions: `pdftotext` rendered the
  fraction `1/3` as `31` (p. 19), dropped every overbar on the Group-Zero
  constant symbols (p. 10, where `2n` is twice `n`, not `2^n`), and rendered
  the valuation symbol `varpi` as `$`. Hazard H2 as charted.

## 2026-08-21 - Two Corrections from Checkpoint Review (C4/C6 follow-up)

- **Erratum E6 recorded against `Willard2005`.** Section 4 Item D (p. 16) cites
  `[55]` twice - "Hajek, Svejdar and Vopenka [55, 63]" and "Svejdar [55] has
  generalized this phenomena with regards to interpretability" - but `[55]` is
  Svejdar's "Modal analysis of generalized Rosser sentences" (JSL 48, 1983),
  whereas `[54]` is "Degrees of interpretability" (Comm. Math. Univ. Carolinae
  19, 1978), which is the paper the claim describes. `[54]` is cited correctly
  in Item E's list. Reading: both Item D occurrences are slips for `[54]`,
  recorded as **probable** rather than certain since confirming it requires the
  1983 paper's contents, which are not in the corpus. The codified statement
  should cite Svejdar's interpretability result by title, not by 2005's
  reference number.
- **Obligation O26 recorded, correcting a formulation of mine.** In discussion
  I contrasted Rosser-style self-verification ("changes what provable means")
  with Willard's ("changes what total function means"). The second half is
  wrong and the user corrected it: **Willard redefines nothing.** "Total
  function" keeps its standard meaning throughout; what varies is *which
  totality axioms a system takes* and *which function symbols its language
  carries*. `Willard1993-TR` p. 3 is explicit that IS(A) "will recognize
  non-zero Division as a total function (and hence can view Multiplication as a
  well-defined **partial** function)", and Willard2011's Type-M / Almost-M /
  A / S / NS taxonomy is purely a question of which of axioms (1)-(3) are
  present. The obligation records that the codified statement must never imply
  SJAS uses a weaker *notion* of totality or of provability - that reading
  converts an honest weakening of the theory into a definitional trick, and is
  exactly the Feferman-style "numerically correct but intensionally incorrect"
  route Willard rules out in the 1993 chapter's opening page.
- Context: the corpus cites Rosser only historically (the 1936 removal of
  omega-consistency) and carries Arai's "Derivability Conditions on Rosser's
  Proof Predicates" and Svejdar's "Modal Analysis of Rosser Sentences" in the
  bibliographies of both spine papers **without engaging either in the body**.
  A Rosser-comparison for the Refinement stage is therefore open rather than
  foreclosed; O26 records the constraint it must respect.

## 2026-08-21 - Spine Extraction: Willard 2011 and the Subsumption Audit (component C6)

- Extracted `Willard2011` (arXiv 1108.6330v8, 64 pp.), the unification paper:
  `codification/extraction/willard2011.md`, 47 numbered items. Registries now
  hold 126 results, 16 systems, 53 notation rows; audit green.
- The charter's distinct C6 deliverable, the **subsumption audit**, is
  `codification/extraction/willard2011-subsumption-audit.md`. Verdicts:
  **unifying** verified, **simplifying** verified, **extending** verified -
  with three qualifications recorded below. Two of the four claimed paradigms
  (`Willard2006a`, `Willard2009`) are not yet extracted, so those halves are
  explicitly **deferred to C9/C10** with the specific checks named, rather
  than assumed.
- **The unification device** is Definition 3.4's **generic configuration**, a
  5-tuple `(L^xi, Delta^xi_0, B^xi, d, g)`. Language, formula class, base
  system, deduction method **and the Godel coding method** all become
  parameters, and each earlier system becomes one choice of xi. Verified
  verbatim matches to `Willard2005`: the U-Grounding eight, the Mult graph
  (2011 Eq. 45 = 2005 Eq. 4), the eight tableau rules, the Group-2 schema,
  and the fixed point (2011 Eq. 35 = 2005 Eq. 7; Eq. 36 = 2005 Eq. 8 with
  Neg^k for Pair and the level parameter k added).
- **But the correspondence is not element-wise** (drift D21). Willard's own
  attributions map xi-star *and* xi-star-star to `Willard2005` (sections 5 and
  5.3), xi-minus to `Willard2006a` Theorem 3, and xi-R to `Willard2009` -
  leaving **`Willard1993` without a configuration of its own**. And 2011 is
  **not self-contained**: it declines to repeat the Tab-U\*1 definition
  ("It will be unnecessary to repeat here") and imports the Normed and
  Fact D.3 machinery from 2005 for its own stability proof.
- **The simplification is identifiable and partly relocation.** 2005's
  Definition 5 (theta-compactification, parts A and B, with theta < 1/3 free)
  **splits into two named properties** - Definition 5.1 **A-Stable** (the Pi
  half) and Definition 5.3 **E-Stable** (the Sigma half) - each with the fixed
  constant 1/2. 2005's App-forall/App-exists pair collapses into the single
  measure `sharp`. Theorem 5.9 has the same skeleton as 2005's Theorem 1 but
  a sharper ending: both a sentence and its negation land on the *same*
  Good{half sharp}, so the contradiction is immediate - which is why 2005's
  Theorem 1 needs the small-m side condition (obligation O1) and 2011's does
  not. **O1 is now scoped as a 2005-only obligation.** What does not vanish is
  the per-configuration combinatorics: 2005's section 5.2 nine-case induction
  reappears as Appendix D-2's four-page Theorem D.4.
- **Four genuinely new results** with no antecedent in the extracted 1993 or
  2005 material: Theorem 6.12's **Translational Reflection Principle** (the
  abstract's headline - reflection into a Standard-M-equivalent Pi^xi_1
  translation, which Loeb does not block because Psi and Psi-translated are
  *not* equivalent from the system's own perspective); the **Global Simulation
  Sentence**; Appendix G's three-axiom result; and Theorem E.1, which shows
  the translational principle is inoperative for conventional logics.
- **Willard 2011 answers a question asked in this repository.**
  `prose/to-dw-20201206z.txt` question 2 asks whether Group-2's infinitary
  schema can be replaced by a finite set sufficient for all Pi\*1 theorems.
  The answer is two-stage: a single **Global Simulation Sentence** replaces
  the schema but yields only the Test-relativized form (Definition 6.8,
  Remark 6.9); **three** added sentences yield the theorems *purely*, but only
  for a quantifier-count bound c fixed in advance (Theorems G.2, G.3).
  Recorded as composition obligation **O23**.
- **Willard's own limitation, recorded** (Remark 6.16b, obligation O24):
  `SelfCons` "causes axiom systems to produce essentially a **1-line proof**
  of their own consistency… an **instinctive faith** in its own consistency
  (rather than it supporting a full-length proof-justification of this fact)".
  Any presentation omitting this overstates what SJAS delivers.
- New drift: **D18** (2011's "Self Justifying" is 2005's "Introspectively
  Unified Logic"; with 1993's "self-verifying" the corpus now carries three
  closely-named non-equivalent predicates - obligation O25); **D19** (a third,
  materially different formulation of Solovay's unpublished theorem);
  **D20** (see below); **D21** (the paradigm/configuration mismatch).
- **Bibliographic correction to our own inventory (D20).** `Willard2001`'s
  actual title is "Self-Verifying **Axiom** Systems, the Incompleteness
  Theorem and **Related Reflection Principles**". Willard's 2005 self-citation
  renders the subtitle as "the tangibility reflection principle", and
  `nachlass/paperlist` had inherited that; the phrase names a construct inside
  the paper and the title of the 1997 KGC-5 chapter, not this paper.
  `paperlist` and `registry/corpus.md` corrected, with the provenance recorded.
- Obligations refined: **O2** (the coding condition's mature form is the
  **Conventional Tableaux Encoding Requirement** - Godel number at least 32^J
  for J function symbols - justified in 2011 as "at least 2J logical symbols
  and thus at least 5J bits", a different derivation of the same 5 from
  1993's base-32-in-6-bit-bytes route); **O11** discharged in substance by
  Definition D.1(ii), which writes addition-totality as
  `for all x,y exists z <= x+y (z = x+y)` and calls it "a very precise Pi\*1
  styled declaration" - the function symbol is what makes the bound
  expressible, and the bounded existential is what keeps it Pi\*1.
- Also recorded: the **Type-M / Almost-M / A / S / NS** growth taxonomy (the
  origin of "Type-NS" as used in this repository's README); `SelfCons_k` as
  the origin of the Proflog line's `SelfCons1`; Appendix F's **Epistemological
  Bundle Theory**, in which A- and E-stability formalize how a thinking agent
  can treat short proofs from a technically inconsistent view as useful,
  provided its proofs are shorter than the errors.
- Verification: text-layer read of all 64 pages, plus visual control of
  printed pp. 14, 16, 18, 32 and 39 (Tight; A-/E-Stable; Theorem 5.9;
  Appendix A's equations; the coding condition). Pagination convention for
  this witness: **printed page = PDF page - 1**.

## 2026-08-21 - Origin Extraction: TR 93-10 and the KGC 1993 Chapter (component C5)

- Extracted both origin witnesses: `codification/extraction/willard1993-tr.md`
  (full template) and `codification/extraction/willard1993.md`
  (delta-focused companion, since the chapter is the TR's 12-page
  abbreviation). Registries now hold 52 results, 11 systems, 41 notation rows;
  audit green.
- **Drift D1 resolved, gap G9 closed.** The TR's reference list (printed p. 55,
  read from the page image) cites `[So94] R. Solovay, private communications
  (April 1994)`, cites the published chapter as `[Wi93]`, and forward-
  references `[Wi94]`. A report citing April-1994 communications cannot
  predate April 1994, so TR 93-10 is a **1994 text carrying a 1993-series
  report number** - exactly why Willard cites it in JSL 2001 as a "SUNY-Albany
  Technical Report, March 1994". One artifact, two accurate descriptions.
- **New gap G14**: `[Wi94]` D. Willard, "The ETR Reflection Principles for
  Self-Verifying Axiom Systems", forthcoming report - cited as containing the
  proof of Proposition 8. Absent from paperlist, DBLP, arXiv and the resume,
  and not found by the C2 coverage pass. Its title closely matches the
  unlocated KGC-5 chapter `Willard1997` (G2), whose priority is raised
  accordingly.
- Structure recovered: the TR proves Propositions 1-7 (chapter proves only
  Proposition 1 and states 2-4 as "not proven in this 12-page Extended
  Abstract"), defers Proposition 8 to [Wi94], sketches Proposition 9, and
  carries Lemmas 5.1-5.3, 6.1-6.2, 7.1, 12.1, Remarks 1-7 and Appendix A.
- **The origin boundary, in Willard's own tabulation** (TR printed p. 3):
  systems fall into three categories by growth capacity - no growth functions,
  addition total, or both addition and multiplication total - and only the
  first "will be capable of permitting Gentzen-style deductive cuts without
  becoming inconsistent". Sharpest form (p. 11): Solovay's inconsistent
  `IS_+(PA+,G_s)` becomes consistent by removing *either* cuts *or* the
  successor function. One inconsistent system, two independent repairs.
- **The engine's informal ancestor** (chapter p. 329): proofs of `b_n`'s
  existence under addition require more bits than `b_n`'s binary encoding,
  whereas under multiplication `a_n`'s existence proof is exponentially
  shorter than its encoding. This is `Willard2005` Definition 5 and Eq. (20)
  in embryo. The 1993 proof skeleton (minimal inconsistency proof `p`, witness
  `p*` at least `p`, `INT(c_j) <= 2^j * m_max` because addition is the only
  increasing function) maps step for step onto 2005's Theorem 1 opening and
  Eq. (19); `INT` is `VAL`, "s-consistent" is `Positive(a,b)`. What 2005 adds
  is the abstraction barrier of Definition 5, separating the engine from the
  per-apparatus analysis - which is why Willard says it "did not exist in our
  prior papers".
- **The Main Conjecture and its refutation** (chapter section 6): Willard
  conjectured that the cut-permitting `IS_+(A)` and `IS*_+(A)` are also
  self-verifying. His own later work refutes it - TR Proposition 7 buys cuts
  only by dropping addition-totality, Solovay's Theorem kills the
  successor-carrying case, `Willard2004` closes Tab-Sigma\*2/Pi\*2, and
  `Willard2005` Theorem 5 recovers only a Level-1 restricted modus ponens.
  Recorded as composition obligation O8: the codified statement must carry the
  arc, not just the mature result.
- **Provenance correction (drift D13, obligation O9)**: the affine-tree
  documents present the identification of the multiplication boundary with
  *variable duplication* as a finding of their tree transposition. Willard
  states it himself at chapter p. 328 - `x*x` needs "two occurrences of the
  same variable", and "the proof of Godel's Incompleteness Theorem used
  essentially the same double appearance of a variable". The insight is
  corroborated rather than novel, which strengthens it; the transposition's
  real contribution is its carrier-independence.
- Further drift recorded: D7 (the deduction method is notation in 1993 but
  definitional by 2005; "self-verifying" and "self-justifying" are different
  predicates), D8 (`IS*(A)` vs `IS^{Sigma1}(A)` between the two 1993
  witnesses), D9 (the two witnesses define Pi1/Sigma1/Delta0 differently, and
  1993's unstarred classes are 2005's starred ones), D10 (the chapter lists
  fourteen Group-1 functions, the TR eight), D11 (the 1993 and 2005 "eight
  function" signatures share only Addition, Subtraction, Division - the
  shared cardinality is a trap), D12 (addition-totality is Pi2 in 1993 but
  Pi\*1 in 2005, which is *why* the U-Grounding function language exists),
  D14 (Willard permits pointer-sharing proof compression and says it
  strengthens his results, where the NC-SJAS plan forbids all sharing).
- Also recorded from the origin: `SUBST` demoted to a Delta0 relation with
  unprovable totality but per-instance verifiability (chapter p. 332); the
  "subcomponent" relation defining the cut-free property, absent from 2005;
  resolution and cut-free sequent calculus named as covered by the same
  analysis at TR pp. 4 and 10 and chapter p. 332 - **stated-only in every
  place**; the ZF thread ("IS(ZF) is self-verifying and affirms the
  consistency of ZF") that resurfaces in the 2008 ZCF nachlass drafts; and the
  floating-point remark (chapter p. 329) anticipating the ASL-2005 line by
  twelve years.
- **Visual verification pass over the TR main body (same day, after
  checkpoint review).** Nothing had been blocking it - the deferral was a
  scoping call, not an obstruction - so printed pp. 14, 15, 20, 23, 28, 29, 32
  and 34 were rendered and read. **Three proof-status judgements taken from the
  OCR sweep were wrong**: Lemmas 5.1, 6.1 and 12.1 carry *Proof Sketch* labels,
  not full proofs, and Proposition 9's is a "Very Informal Proof Sketch". All
  three sketched lemmas are load-bearing (5.1 underwrites the Delta0 encoding,
  6.1 is half of Proposition 1's proof, 12.1 is the entire mechanism of
  Proposition 6), so the TR's real proof profile is weaker than recorded.
  Registry now: 58 results rows, 25 full / 5 sketch / 2 cited / 16 stated-only.
- **`ETR` decoded**: TR printed p. 29 defines the **E-Tree Reflection
  Principle** and states Proposition 8 about a system `IS^{ETR}(A)`, saying
  "In [Wi94], we generalize the techniques of Sections 7 & 8 to prove" it. So
  gap G14's `[Wi94]` "The **ETR** Reflection Principles for Self-Verifying
  Axiom Systems" is precisely the paper proving Proposition 8; Proposition 9
  (the *Bounded* form) is the fragment Willard could sketch without it.
- **Remark 6 relativizes the signature drift** (printed p. 34): `G_0`'s
  definition "was quite arbitrary", and Propositions 1-4, 8, 9 hold when *any*
  set of **slowly growing** functions (bounded by i*2^j*Max) is added, with
  Proposition 7 taking any non-growth set. The differing 1993/2005 function
  lists (D10, D11) therefore do not compete - the invariant is the growth
  class, not the signature. Obligation O13; this also independently
  corroborates the affine-tree design's decision to abstract to a growth
  discipline.
- **Remark 4 sharpens D12** (printed p. 28): in 1993 a total function may be
  given *either* by a function symbol *or* by a relation symbol plus a
  totality axiom, interchangeably. By 2005 they are not interchangeable - a
  Pi2 totality axiom is inadmissible in a Normed(a,b) system - so the
  function-symbol presentation becomes obligatory. A real tightening that is
  easy to misread as agreement.
- **The pointer notation is a totality device** (printed p. 15): the encoding's
  `u#`/`v#` symbols "act as pointers to constants", and Lemma 5.2's
  parenthetical gives the reason - the analogous mappings onto the expanded
  constant form "are unknown by IS(PA+) to be total functions". Expanding a
  long constant in place is exactly what the system cannot prove total. This
  sharpens D14 considerably: Willard's sharing is load-bearing, not a cost
  optimisation, so any transposition that bans sharing outright owes an answer
  to the problem the pointers solve.
- **New drift D15**: multiplication is fatal for two different stated reasons -
  1993 (Lemma 12.1) because multiplication-totality makes local Pi1 reflection
  derivable, which Proposition 5 shows inconsistent; 2005 (Remark 4) because
  growth overwhelms Definition 5's envelope arithmetic. Two routes to one
  boundary; obligation O14.
- Also recovered: the proofs "were conceived using Gentzen's cut-free sequent
  calculus notation" and only later transposed to tableaux (p. 14) - stronger
  evidence for apparatus generality than the bare "trivially generalizes"
  claims; terse proofs, `Glue(t,p)` and Lemma 7.1's internalization of a
  meta-theorem (p. 23); the INVALID.1/INVALID.2 naming behind Figures 1-4; and
  the phrase "limited but **tangible** power" (p. 34), the lexical origin of
  the Tangibility Reflection Principle (obligation O15).
- **Appendix pass (same day, after user correction).** The plan excludes only
  material irrelevant to Willard's development of SJAS, and the exclusion test
  applies to whole corpus items - `Willard1993-TR` is ruled `core`, so
  sub-selecting within it was wrong. Every page of Appendix A and Appendix B
  was read (printed pp. 37-54). Results:
  - **An entire Appendix B had been missed** by the OCR-derived structure note.
    It is the worked seven-level tableaux derivation of INVALID.2's
    inconsistency that Figure 4 draws, and its Summarizing Comment states the
    axiom-versus-theorem mechanism (drift D16, obligation O18): a cut-free
    proof cannot cite a theorem as an intermediate step, so INVALID.2 must
    reconstruct LPi1 reflection's "essential cut-free implications" - the same
    device that separates Willard2005's IS_D from NS^{k,m}_D.
  - Appendix A defines `UNION(A)`, `ExPrf` and `SUBST` - the three constructs
    Willard2005 section 3 reuses verbatim - and its equations (A.1)-(A.3) are
    Willard2005's Eq. (7) with the diagonal argument fixed to a constant rather
    than abstracted to a parameter. A precise, checkable lineage claim.
  - **Corollary A.15 is the arithmetized proof-checker in origin form** (root
    check, node-justification check, closure check), the direct ancestor of
    Proflog's tableau-proof predicate and the affine-tree Deriv.
  - **Observation A.8 explains the 5-versus-6 constants** flagged as obligation
    O2 in C4: a base-32 digit is stored in a 6-bit byte, so the godelized form
    costs 6/5 of the natural one - five bits of content per six-bit symbol.
    That is where Willard2011's "at least 5J bits, Godel number at least 32^J"
    comes from, the 32 being the base-32 alphabet.
  - **Observation A.10: IS(A) cannot prove concatenation total**, and handles
    it by the same demote-to-relation move. The affine-tree design argues
    concatenation-strength from the external literature; Willard states the
    system-side fact himself (obligation O16).
  - **Scalar multiplication always was total**: `Scalar_k` is a function for
    fixed k, and Willard2005's `Double` is exactly 1993's `Scalar_2`. "Multi-
    plication is not total" means *variable* multiplication (obligation O17).
  - **The Added Comment (p. 52) reduces the working signature to four
    functions** - Addition, StringCount, Shift, Extract - the other four being
    presentational. With Remark 6 this largely dissolves drift D10/D11: the
    1993 and 2005 signatures are related by promotion and derivation within one
    growth class, not by disagreement. (Willard2005's primitive `Maximum` is
    derived in 1993 as x + (y-x).)
  - Three appendix items are sketch-grade: A.5 (Proof Sketch), A.13 (Proof
    Outline), A.16 (Proof Sketch). Registry now: 76 results rows, 38 full /
    8 sketch / 2 cited / 16 stated-only / 12 n-a.
  - Lemma A.16 also records that `G_0` is finitely many axioms **plus two
    infinite schemata** for the integer constants - schemata the 2005
    U-Grounding language eliminates by making numerals terms, which sharpens
    obligation O11 about why that language exists.
- **Completion pass (same day).** Cleared the two items left unverified, and
  the clearing turned up three more:
  - **Lemma 5.4 had been missed entirely** (printed p. 19). It defines
    `Prf2_B(x,y1,y2)` - a proof `y1` paired with **a Turing-machine trace `y2`
    verifying `y1` is well defined** - and the system `IS2` built on it.
    Willard adds that "Appendix A is in some sense **unnecessary**" because
    every theorem about IS translates to IS2 by replacing `y` with the pair.
    The chapter offers the same escape under `Prf-star`. A trace-carrying proof
    predicate is therefore Willard's own device from 1993, not a
    native-computational innovation of the NC-SJAS line (obligation O20). Its
    proof is labeled "**Quite Informal Proof Sketch**", the weakest label in
    the document.
  - **Remark 2 is on p. 24, not p. 22**, and is a global methodological
    caveat: "our proofs shall *apply a cut rule at the meta-theoretical
    level*… we will *often omit constructing formally the cut-free proofs*…
    Instead, *their theoretical existence* will be established implicitly via
    the Cut Elimination Theorem." The object systems are cut-free; the
    metatheory uses cut freely (drift D17, obligation O19).
  - **Lemma 5.3 is a sketch**, not a full proof, and Willard states that it
    **fails without the `u#` pointer notation** - "Lemma 5.3 would not follow,
    had this section employed slightly different definitions (such as omitting
    the symbol u#)" - while scoping the loss: Propositions 1, 2a, 3, 5 and 7
    survive without it, so only 2b, 2c, 4 and 6 depend on it. This settles
    drift D14: Willard's constant-pointer compression is **necessary to four
    of the origin results**, so a no-sharing cost model must replace it or
    give those results up.
- Two further passages recovered from printed p. 12 (still section 3, not
  section 4 as the OCR structure had it):
  - **Which derivability condition fails**: "only in the *degenerate case*
    where x and y are *fixed constants*" can IS(A) prove the Hilbert-Bernays
    condition that from proofs of alpha and alpha-implies-beta a proof of beta
    exists. Fixed-parameter totality versus uniform totality is the corpus's
    recurring device - the same shape as SUBST for fixed k-bar, Lemma 5.3's
    T(p,k), and Scalar_k (obligation O22).
  - **Willard states the delicacy thesis himself**: "a modest change in a
    formalism's definition will cause Propositions 1 thru 7 and Solovay's
    Theorem to produce starkly different results… it appears premature to
    develop hasty conclusions". That is the premise of
    `prose/sjas_synthesis.txt`, asserted by Willard about his own results in
    1993 - the Codification's governing caution is the corpus's own lesson
    (obligation O21).
- E1993-1 resolved and not an erratum: section 4 does carry a display heading,
  "Intuition Behind the Proof of Proposition 1" (p. 13); the OCR sweep missed
  it because the running page number ran into the heading (`-134. Intuition`).
  Lemma 5.4 and Appendix B were missed the same way, so the pass-2 obligation
  is now specifically to re-sweep for items the heading passes could not see.
- Registry: 77 results rows (37 full, 10 sketch, 2 cited, 16 stated-only).
- Verification discipline: everything marked `V` in the extraction records was
  read from page images; OCR-only items are marked `O` and may not be quoted
  in the codified statement until pass 2. The TR's OCR layer renders
  `IS^{Sigma1}(A)` as `is-TM (A)` and `G_0` as `Gg`, so the visual control was
  not optional. Appendix A's Lemma A.\* inventory is deferred to pass 2.

## 2026-08-21 - Pilot Extraction of Willard 2005 and Fidelity Gate (component C4)

- Completed the pilot extraction of `Willard2005` (JSL 70, pp. 1171-1209, the
  engine paper) per the charter template:
  `codification/extraction/willard2005.md`. Full sequential read of all 43
  pages; 26 numbered items inventoried (Definitions 1-6, Theorems 1-6 plus the
  cited Theorem *, Lemmas 1-4, Corollary 1, Remarks 1-8), each with page
  anchor, statement digest, proof status, and dependencies.
- Registries populated: 26 results rows, 4 systems rows (`IS_D(A)`,
  `NS^{k,m}_D(A)`, `N_D(A,Psi)`, `PA+`), 27 notation rows. `audit.sh` extended
  to report registry sizes and proof-status counts; verified red-green against
  a deliberately corrupted table (bad enum and unknown corpus key both caught).
- **Proof-status profile of the paper**: 10 full, 1 sketch (Lemma 3), 1 cited
  (Theorem *), **6 stated-only**, 8 definitional. The stated-only set is
  load-bearing: Remark 1 (cut-free sequent calculus, Herbrand, Tab-Q\*1-List
  and Q\*1-restricted Hilbert all preserve consistency - "we will not have the
  page space"), Remark 6 (tangibility hybridization), Remark 8 (three stronger
  forms of the negative result). Gap G8 widened accordingly.
- **Fidelity gate passed** (`extraction/willard2005-fidelity-check.md`).
  Agreement with both known-good re-derivations - the affine-tree design
  document sections 2-3 and the 2026-07-27 apparatus survey - on every
  load-bearing definition, theorem statement, and proof structure, including
  the nine-case induction in Lemma 1 and the verbatim sense of Remark 4.
- **Divergence found in the affine-tree re-derivation** (drift ledger D6):
  coding density bounds the compactification exponent theta from *below*
  (`theta >= 1/d` via Lemma 1's hypothesis, footnote 5), while Definition 5
  bounds it from above (`theta < 1/3`); Willard proves `theta = 1/4` with
  density 5, i.e. selects a value inside the window `[1/5, 1/3)`. The ADR's
  "theta <= 1/5" inverts the inequality and the design document's "theta =
  1/c" collapses the window to a point; the design document states the
  relation correctly in section 8 and incorrectly in sections 3-4. The design
  conclusions survive (density 5 gives a non-empty window; "density 3 is the
  theta = 1/3 boundary" is correct as the `d > 3` condition), but the stated
  derivation does not. Also recorded: three distinct constants must not be
  conflated - 6 bits per language symbol (Appendix A), the constant 5 in
  Eq. (20) bounding U-Height, and theta = 1/4 (drift ledger D3).
- Other drift entries seeded: D2 (`Willard2006a` cited under a pre-publication
  title naming the Hilbert-styled framing), D4 (the Solovay April-1994
  private-communication thread, unpublished Theorem *, expositions only by
  Willard - the same thread as `Willard2020-LFCS` ref. 41), D5 (`IS(A)` vs
  `IS_D(A)`).
- Gap G3 provenance confirmed: Remark 7 identifies the unlocated ASL-2005
  technical report as the only full-length source for the floating-point
  result in which `IS_D(A)` recognizes addition, multiplication, subtraction
  and division as total over computer reals.
- **Corpus-wide extraction hazard discovered**: `pdftotext` renders JSL
  small-caps headings with a space after the initial letter (`D EFINITION 4.`,
  `T HEOREM 1.`), so a naive `^Definition` grep matches only prose
  cross-references and silently misses every real heading. Reproducible sweep
  and warning recorded in the extraction record section 8; affects
  `Willard2001`, `Willard2002a`, `Willard2006b`, and other venues must be
  probed per-publisher.
- Template verdict: no change required for the eight sections themselves; two
  method additions were made in response to checkpoint review (below).
- **Visual control pass added and run** (extraction record section 8.1). Read
  the PDF page images for the pages carrying the load-bearing formalism
  (pp. 11, 19, 22, 26, 33) to control for text-layer rendering. `pdftotext`
  fails *silently* on mathematical typesetting: it rendered Fraktur `Im`
  (Definition 1's consistency-preserving map) as `=`, the Godel-sentence
  script glyph as `f`, and dropped numeral overbars, tower super/subscripts,
  iterated-log exponents, and floor/ceiling delimiters. The overbar loss is
  the dangerous one - `Gamma(n-bar)` versus `Gamma(n)` is numeral- versus
  variable-substitution, i.e. the fixed point itself. Corrected two notation
  rows and Eq. (9) in the extraction record; confirmed Definitions 4/5,
  Theorem 1, Eqs. (18)/(19)/(20), and Theorem 2's theta = 1/4 unchanged, so
  the D6 divergence survives visual verification. Also established that
  errata E1 and E2 are genuine features of the typeset source rather than
  extraction artifacts - a distinction the text layer cannot support. The
  charter now requires a visual control pass for every page carrying a system
  definition, a quotable definition or theorem, or a displayed equation whose
  constants matter, for all tiers and not only OCR'd scans.
- **Composition-obligations register created**
  (`codification/concordance/composition-obligations.md`, O1-O7). Findings the
  codified statement must act on were previously recorded only inside drift
  entries and the fidelity check, where they would not surface reliably at
  C14-C15; they are now also collected in one checklist: the Theorem 1 small-m
  side condition, the three-constants distinction, the corrected density/theta
  window, the stated-only apparatus cells, Theorem *'s unpublished provenance,
  the destroyed glyphs, and the `IS(A)` versus `IS_D(A)` distinction. The
  charter makes appending to it a standing obligation.

## 2026-08-20 - Codification Charter, Scaffold, and Corpus Ruling (component C3)

- Wrote the charter ADR (`codification/ADR-0001-codification-charter.md`):
  layered method (corpus → extraction → concordance → synthesis), fixed
  extraction template, component roadmap C1–C16 with status, acceptance
  criteria A1–A7 and sufficiency tests T1–T5, and the standing user
  directions (nachlass-scoped, bash tooling, human-readable registries,
  throughline test, obligatory checkpoints).
- Completed the corpus ruling pass (`codification/registry/corpus.md`):
  45 rows — 19 core (pilot Willard2005; origin TR 93-10 + KGC 1993; spine
  2011/2001/2020; tiers A/B/C), 5 motivation, 9 witness-grade (nachlass +
  resume), 6 out (data-structures/database line, biology line, teaching
  exam, duplicate alias), 4 gaps (Willard1997, Willard2021, ASL-2005 TR,
  TABLEAUX-2003 unconfirmed), 2 accepted micro-item classes (BSL abstracts,
  St. Petersburg abstract). Every in/out ruling carries a throughline
  rationale.
- Seeded the registries (`systems.md`, `results.md`, `notation.md` schemas;
  `gaps.md` populated G1–G13) and the concordance drift ledger (D1: TR 93-10
  vs "March 1994" citation identity).
- Added `codification/audit.sh` (portable bash+awk): parses the registry
  tables; checks key uniqueness, enum validity, ruling–extraction
  consistency, witness-file existence, and cross-registry key references.
  Green on the seeded state (45 corpus rows, 13 gap rows).
- Added `codification/regen-sources-text.sh` and generated
  `codification/sources-text/`: 25 plain-text extractions (~336K words) of
  the text-bearing in-corpus witnesses, as grep/anchor aids. Excluded by
  design: the image-only DIMACS volume (gap G10), the LNCS 11972 volume
  (chapter extract suffices), and nachlass scans (OCR lives under
  `collected_dew_materials/ocr/`).
- Added `codification/README.md` as the layer map / entry point.

## 2026-08-20 - LFCS 2020 Volume Acquisition and Chapter Witness (C2 follow-up)

- The user downloaded the dokumen.pub upload of the LFCS 2020 proceedings by
  browser (automation was Cloudflare-blocked; see the coverage report
  addendum). Verified as the genuine Springer LNCS 11972 volume (297 pp.,
  Artemov–Nerode eds., Acrobat-Distiller-produced).
- Renamed to `papers/willard2020_tender_line_lfcs2020_lncs11972_volume.pdf`
  and extracted the Willard chapter, "On the Tender Line Separating
  Generalizations and Boundary-Case Exceptions for the Second Incompleteness
  Theorem Under Semantic Tableaux Deduction", LNCS 11972 pp. 268–286
  (pdf-pages 278–296), DOI `10.1007/978-3-030-36755-8_17`, as
  `papers/willard2020_tender_line_lfcs2020_lncs11972_chapter.pdf`
  (ghostscript page-range extraction, 19 pp., 8,945 words searchable; the
  pdfseparate+pdfunite route duplicated shared resources to 12.4 MB and was
  discarded).
- Cataloged: `papers/SHA256SUMS` entries for volume and chapter
  (`sha256sum -c` clean), `papers/README.md` witness rows and gap-note
  update, `paperlist` entries `[36-LFCS]`/`Willard2020-LFCS` (witnessed) and
  `[37]`/`Willard2021` (metadata-only; the JLC 31(1) 2021 journal version
  remains the sole unacquired terminal item — no OA, no known shadow copy).
- Observed for later extraction: the chapter's ref. 41 cites private
  April-1994 telephone conversations between Willard and Robert M. Solovay.

## 2026-08-20 - Willard Literature Coverage Verification (SJAS Codification, component C2)

- Completed the precautionary web coverage pass over arXiv (author API with
  version numbers), DBLP (full 1978–2021 record), targeted web searches,
  Willard's own September-2015 résumé (`lit/dewresume.pdf`), and an internal
  sweep of every technical-report and BSL-abstract mention in the witnessed
  corpus. Full report: `codification/coverage-report-2026-08-20.md`.
- **Two escapes found**, Willard's terminal publication pair, both
  unwitnessed: the LFCS 2020 chapter "On the Tender Line Separating
  Generalizations and Boundary-Case Exceptions for the Second Incompleteness
  Theorem Under Semantic Tableaux Deduction" and its journal version in the
  Journal of Logic and Computation 31(1) (2021), pp. 375–392. Both actioned
  as acquisitions (no legal OA located; shadow-library copies excluded per
  the 2026-05-10 pass precedent). The JLC paper may bear on the apparatus
  audit's unprinted resolution-stability-proof finding.
- **Closed a recorded gap from local holdings**: `lit/danwillard1993.pdf` is
  the published 12-page KGC 1993 chapter itself; symlinked into `papers/` as
  `willard1993_self_verifying_axiom_systems_kgc_lncs713.pdf`, checksummed,
  README rows amended. `Willard1993` is now doubly witnessed (published
  chapter + TR 93-10 full-length origin).
- All six arXiv witnesses verified to be the latest versions (1108.6330v8,
  1307.0150v2, 1612.08071v5, 1707.00039v2, 1807.04717v2, 2006.01057v1).
- Technical-report class bounded: TR 93-10 identified with JSL 2001 ref.
  [41]'s "March 1994, 50-page" citation (same artifact, loose description;
  drift-ledger note); one unlocated companion TR behind JSL 2005 ref. [71]
  (ASL-2005 Athens floating-point talk; published successors witnessed).
- Micro-item classes recorded with ruling proposals (~25 BSL 300-word ASL
  abstracts 1995–2012; the 2003 St. Petersburg 200-word abstract): enumerate
  lazily, do not chase. TABLEAUX 2003 position paper remains metadata-only
  and unconfirmed by any queried source.
- Biographical anchor recorded: Willard 1948-09-19 – 2023-01-21.

## 2026-08-20 - TR 93-10 Searchable Collation (SJAS Codification, component C1)

- First component of the SJAS Codification stage (`prose/sjas_synthesis.txt`;
  plan approved 2026-08-20; branch `sjas-codification`). All codification work
  is conducted within `nachlass/`; audit tooling is portable bash/CLI; user
  feedback is collected after each definite component until autonomous
  execution is authorized.
- Collated `papers/1993technicalreport/tr1993-{0,1,2}.pdf` (three image-only
  scan parts, 61 pages) into
  `papers/1993technicalreport/willard1993_self_verifying_axiom_systems_tr93_10_searchable.pdf`:
  SUNY-Albany TR 93-10, "Self-Verifying Axiom Systems and the Incompleteness
  Theorem" — the full-length original of the published `Willard1993` KGC
  chapter, per its own preface page. Foundational witness for Codification.
- Pipeline (present tools only): pdftoppm 2550px grayscale renders → ImageMagick
  JPEG q55 → tesseract 5.3.4 per-page `pdf txt` at `--dpi 300` → pdfunite in
  scan order. Letter-size output, ~23.6 MB, aligned invisible text layer
  (searchable/highlightable), 24,221 words extracted.
- Verified: part order 0→1→2 with seam continuity (printed pp. 25→26, 41→42);
  printed-page completeness by OCR header sweep (gap-free lattices: part 0 =
  title + preface + pp. 1–25, part 1 = pp. 26–41, part 2 = pp. 42–55 +
  Figures 1–4); per-page PDF integrity via `pdfinfo` (one page damaged by an
  interrupted first OCR run was detected and regenerated before uniting);
  `pdftotext` content spot checks at collated pp. 1/28/44; `-bbox`
  word-coordinate alignment check. OCR layer is a search aid; page images
  remain authoritative for formulas.
- Recorded: `papers/SHA256SUMS` entries for the three parts and the collation
  (`sha256sum -c` clean over the full file), `papers/README.md` witness row
  and gap-note amendment, `paperlist` entry `[16-TR]`, and
  `papers/1993technicalreport/README.md` (provenance, structure map,
  reproducible pipeline).
- Checkpoint follow-up (same day): the first collation left part-2 pages at
  1569×2033–2049 pt versus letter for parts 0/1 — tesseract honors JFIF
  density metadata over `--dpi`, and part 2's low effective density (~117 dpi)
  reproduced its oversized, per-page-varying source dimensions. Regularized by
  aspect-preserving fit + white-pad of the 18 part-2 renders to exactly
  2550×3300 px with declared 300 dpi, re-OCR, and re-collation. All 61 pages
  now verify uniformly 612×792 pt; content, word count (24,221), and
  text-layer alignment re-verified; collation checksum replaced in
  `papers/SHA256SUMS` (sources unchanged).

## 2026-07-28 - Willard Deductive-Apparatus Audit

- Completed a corpus-controlled review of Willard's published logic papers,
  preprints, talks, reachable repository history, bibliographic inventory, and
  all 17 distinct high-fidelity nachlass OCR targets for resolution, sequent
  calculus, and other candidate SJAS deductive apparatuses. The full
  occurrence ledger and implementation analysis is recorded in Proflog commit
  `a7af9f7`.
- Resolution is an affirmative SJAS candidate in two distinct author-stated
  forms: the 2011 Skolemizing `xi_R`/`Level(0R)` route and the 2020 direct
  Level-1 `ISRes` route. The latter has a sharp negative control:
  LEM-as-logical-axioms changes the apparatus to `Xres`, and `ISXres` is not
  consistency-preserving.
- First-order cut-free sequent calculus is also affirmative for the
  total-addition/no-total-multiplication profile. Gentzen sequents with
  unrestricted cuts belong to a separate Hilbert-like, no-total-addition
  profile and cannot be substituted into the cut-free result.
- Any implementation must expose and arithmetize the exact proof objects of
  the selected apparatus, regenerate system identity and `SelfCons` from that
  proof predicate, and discharge its proof-growth invariant. A tableau proof
  predicate cannot be relabeled as resolution or sequent deduction.
- The audit retains all false positives, duplicate witnesses, and exact
  publication-version gaps. In particular, the TABLEAUX 2003 position paper
  remains metadata-only, and the accessible papers state but do not print a
  complete resolution-specific stability or compactification proof.

## 2026-05-21 - Collected DEW Materials OCR Pass

- Completed first OCR, assessment, and organization pass for
  `nachlass/collected_dew_materials/`: 19 original PDF scans (225 pages),
  18 unique OCR targets (one exact duplicate alias skipped).
- Added inventory artifacts: `SHA256SUMS`, `manifest.tsv`, `README.md`,
  `catalog/duplicates.md`, topic indexes under `catalog/by-topic/`, and
  reproducible scripts `scripts/ocr_dew_materials.sh` and
  `scripts/update_manifest_from_qa.sh`.
- OCR pipeline: `pdftoppm` at 200dpi, PIL downscale to 850px grayscale,
  `tesseract` (eng, PSM 6 with PSM 3 retry), `ocrad` fallback on low-yield
  pages. Merged text under `ocr/text/` with per-document `*.qa.tsv` QA files.
- Quality outcome: tesseract produced little text on most typewriter/fax-era
  scans; ocrad fallback dominates. Sixteen documents rated `needs_review`,
  `Correspondence_Hajek.pdf` rated `poor`, duplicate alias skipped. The 2020
  incompleteness notebook and ZCF drafts are partially searchable but not
  authoritative for formulas.
- Classified Dec 2025 scans: boundary-case incompleteness draft (Dec 22 a),
  Tab/Xtab deduction notes (Dec 22 b), Trivers-Willard biology article
  (Dec 24 a), Hilbert consistency-program draft (Dec 24 b).
- See [`collected_dew_materials/README.md`](collected_dew_materials/README.md).

## 2026-05-21 - Hi-Fi Re-OCR (600 DPI / formula fidelity)

- Added `--hifi` profile to `scripts/ocr_dew_materials.sh` and wrapper
  `scripts/ocr_dew_materials_hifi.sh` (exclusive flock lock).
- Settings: 600 DPI render, grayscale resize to 2550px max width,
  tesseract PSM 4 (fallbacks 3/1/6 only when primary yields little text),
  7200s per-pass timeout, TSV confidence QA. Output under `ocr/hifi/`.
- Pilot on `ZFnote.pdf` at 2550px: ~42 min/page, excellent formula text
  (`Decipher`, `ENUM`, `Support-ZFC`, etc.) vs garbage from the fast pass.
- Root cause of empty pilot: prior 900s timeout killed tesseract mid-run;
  concurrent tesseract jobs also starve each other — batch must run serially.
- Second bug: script used `$LANG` for tesseract `-l`, clobbering the locale
  (`en_US.UTF-8`); renamed to `TESS_LANG` (defaults `eng`).
- Preprocessing: grayscale resize only at hi-fi width (autocontrast/sharpen
  disabled — it did not improve tesseract on these scans).
- `update_manifest_from_qa.sh --hifi` reads `ocr/hifi/text/*.qa.tsv`.
- Full hi-fi batch completed 2026-05-22: 17/17 documents, ~225 pages under
  `ocr/hifi/text/`; manifest refreshed from hi-fi QA.
- Limitations unchanged: `eng` only (no `equ` math pack); OCR remains a search
  aid, not an authoritative transcription.


- Logged the Proflog completion audit for the finite ordinary-tableau
  `IS#_D(beta)` substrate. The audited scope now includes arithmetized
  formula/system/proof codes, structural syntax predicates, Level-1
  substitution-proof vocabulary, fixed-point substitution, structural
  theorem-code proof targets, and passing slow/fast/extended gates. The
  remaining non-goals are Tab-1/proof-list theorem reuse, general non-identity
  substitution beyond the generated fixed-point entry, and open proof-code
  synthesis.
- Logged the Proflog ADR-0068 follow-up: `tableau-proof/3` and `subst-prf/4`
  now build proof targets from structurally decoded theorem-code bytes when the
  theorem code is not part of the generated Group axiom registry. The promoted
  example is `lt(1,2)`: Proflog proves it through the SJAS arithmetic profile,
  encodes that theorem as a compact formula code, and then checks the supplied
  proof certificate against the decoded theorem target. Both proof predicates
  reject the same certificate when the theorem code is changed to `lt(2,1)`.
  The remaining implementation boundary is proof-list/Tab-1 theorem reuse over
  code terms alone; Proflog still validates decoded targets by calling its core
  tableau kernel.
- Logged the Proflog ADR-0067 follow-up: the Willard SJAS profile now parses
  formula-code byte streams structurally for `wff/1`, formula-class predicates,
  `neg-pair/2`, and identity `subst-code/2`. The red characterization used the
  code for `lt(1,2)`, a valid formula in the active SJAS language that was not
  generated as a Group axiom. Before the decoder, `wff`, `delta-star-0-code`,
  `neg-pair`, and identity `subst-code` all failed because the code was absent
  from the finite generated formula registry. The remaining Proflog boundary is
  that `tableau-proof/3` still bridges arbitrary theorem codes to kernel AST
  formulas instead of checking every theorem formula wholly at the code level.
- Logged the ADR-0066 follow-up: Proflog now exposes the finite generated
  substitution boundary as `subst-code/2`, separating Willard's `Subst(g,h)`
  relation from `SubstPrf(g,t,p)`. The active implementation still generates
  the relation for one finite `IS#_D(beta)` system rather than parsing arbitrary
  formula codes, but `subst-prf/4` no longer couples the substitution code to
  the theorem code being proved.
- Logged the ADR-0065 follow-up: Proflog's Level-1 `SelfCons1` construction now
  follows Willard 2011 Appendix A's fixed-point shape by generating a skeleton
  `Gamma_1(g)`, encoding that skeleton, and using the skeleton code as the
  `subst-prf/4` substitution argument in the final Group-3 sentence. The
  implementation remains a finite `IS#_D(beta)` substrate: it adds the required
  skeleton-to-Group-3 substitution entry and an object-level `sjas-axiom`
  certificate checked through generated `axiom-member/2`, while still leaving a
  general arbitrary-code `Subst` relation for later work.
- Logged the follow-up Proflog implementation boundary after ADR-0063: a
  `tableau-proof/3` predicate over code terms is necessary but not sufficient for
  Willard Level-1 self-justification. The `SelfCons_k(beta,d)` formulation also
  needs substitution-aware proof vocabulary. Proflog ADR-0064 therefore adds
  `subst-prf/4` and changes generated `SelfCons1` to cite it, while explicitly
  retaining the remaining gap that a general code-level `Subst` relation is not
  yet implemented. The focused SJAS and regression gates passed for ADR-0064.
- Logged the Proflog implementation boundary exposed after ADR-0062:
  hash-derived formula symbols can serve as finite generated codebook labels,
  but they are not Willard-style arithmetic Godel codes and cannot support a
  faithful object-language `tableau-proof` predicate by themselves.
- The next Proflog ADR must review Willard's own descriptions of syntax and
  semantic-tableau proof coding, identify compatible coding options, and replace
  or supplement host-side opaque labels with inspectable arithmetized codes for
  formulas, proof objects, complement relations, formula classes, substitution,
  and tableau proof checking.
- Completed the first Proflog-side research pass over local Willard witnesses
  and public arXiv records. The key citation is Willard 2011, Definition D.1,
  part iv: the Godelized method for encoding semantic-tableau proofs may be
  essentially any natural method satisfying a lower bound of at least `5J` bits
  for a proof with `J` function symbols; footnote 23 reformulates this as a
  Godel number at least `32^J`. This rules out fixed-width hashes as faithful
  formal codes.
- Proflog ADR-0063 implemented the replacement boundary using compact base-64
  code terms `(code-N b0 ... bN-1)`, matching the byte/base-64 direction in the
  Willard witnesses while avoiding a deep binary numeral stack overflow. The
  implementation still represents a finite `IS#_D(beta)` executable substrate:
  generated decode tables make codes inspectable to `tableau-proof/3`, but full
  arbitrary-code Delta-0 parsing/substitution remains a later fidelity step.

## 2026-05-10 - Willard Public-Witness Aggregation

- Completed a public-Internet aggregation pass for the Willard bibliography in
  `paperlist`, using arXiv, DBLP, OpenAlex, Crossref/DOI metadata, publisher
  landing pages, archived SUNY Albany author PDFs, and other public repositories.
- Added full-text witnesses under `papers/` where public copies were reachable,
  including author-side archive copies for many SJAS and second-incompleteness
  papers that were otherwise behind publisher controls.
- Added `papers/README.md` to map paperlist keys to local witnesses, public
  sources, and known public-full-text gaps.
- Added `papers/SHA256SUMS` and verified every listed local paper witness with
  `sha256sum -c`.
- Reviewed `works-citing-dew/` for second-order material citing Willard's work
  specifically on self-verifying/self-justifying axiom systems and related
  incompleteness-boundary logic, excluding biology and data-structure
  second-order material.
- Added public full-text witnesses for SJAS-relevant citing works by Salehi,
  Artemov, Beklemishev/Shamkanov, Cheng, Chow, Dvorkin, Pakhomov, Kant et al.,
  Sebti, Visser, Yudkowsky/Herreshoff, and related existing items.
- Captured Penchev's public WordPress page corresponding to the existing
  `Penchev_V` note and slide witness.
- Added `works-citing-dew/README.md` documenting inclusion criteria, archived
  public witnesses, excluded false positives, and blocked/non-OA citing records.
- Added `works-citing-dew/openalex-oa-citer-records.tsv` to preserve the
  OpenAlex OA/public-URL candidate set used during triage.
- Added `works-citing-dew/SHA256SUMS` and verified every listed second-order
  witness with `sha256sum -c`.
- Verified all archived PDFs with `pdfinfo`; all were readable.
- Added `.gitattributes` in the SJAS repo so PDF/PPTX/HTML archive payloads are
  treated as binary by Git.
- Committed and pushed the SJAS archive update as
  `dffbc38 Aggregate Willard SJAS paper witnesses`.
- In the parent Proflog repository only, added an ignore rule for `sjas/` and
  pushed it as `0801cb1 Ignore nested sjas repository`, so the nested SJAS clone
  is not uploaded twice by Proflog.

## 2026-09-04 - §8 expanded: B–S and Willard situations vs AU

Categorical presentations of Beklemishev–Shamkanov (Def 8.2) and Willard SJAS
(Def 8.7) on a common G2-relevant interface with vDO §6 HBL props plus
`copy_□`. Thms 8.11–8.12 identify: B–S drops copy_□ (mere G2-evasion,
no ⊢Con); Willard drops mp_□ from Aff (self-justifying).
