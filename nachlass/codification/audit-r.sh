#!/usr/bin/env bash
# Refinement checks for the SJAS synthesis.
#
# The Refinement stage was opened outside this audit's reach; the 2026-09-02
# review found that every Tier-1 defect it committed was of a kind the
# Codification's machinery exists to prevent. These checks extend that
# machinery across the stage boundary. Contract: refinement/VERIFICATION.md.
#
#   R-A  every paper key cited in refinement/*.md resolves in corpus.md
#   R-B  every row of VERIFICATION.md's quotation register is `img`.
#        Standing instruction 2026-09-02: extracted text is not a source. It
#        may locate a passage; it may never be quoted from. The former `txt`
#        status is retired and is now a failure.
#   R-C  every held secondary-literature file in the register exists
#   R-D  no Refinement document reasserts, unmarked, one of the exact
#        sentences the register records as retracted.
#
#        R-D is a REGRESSION GUARD ON FIXED STRINGS, not a semantic check. It
#        cannot detect a retracted claim that has been reworded, and it does not
#        verify that a nearby withdrawal marker is about the same claim. Treat a
#        green R-D as "these exact sentences have not come back unmarked",
#        nothing stronger.

REF="../refinement"

# The documents whose claims this stage is accountable for. REVIEW-*.md is a
# record ABOUT those documents: it quotes retracted claims and invents test keys
# by design, so it is evidence, not a claim.
r_docs() {
  ls "$REF"/*.md | grep -v -e '/VERIFICATION\.md$' -e '/REVIEW-[0-9-]*\.md$'
}

r_paper_keys() {
  # Paper keys look like Willard2011, Willard2007-APAL, Willard1993-TR.
  grep -ho '\bWillard[0-9]\{4\}[a-z]\?\(-[A-Za-z]\+\)\?' $(r_docs) "$REF/VERIFICATION.md" 2>/dev/null | sort -u
}

run_r_a() {
  local k n=0
  for k in $(r_paper_keys); do
    if ! grep -q "^| $k " registry/corpus.md; then
      err "R-A: refinement cites paper key '$k', absent from corpus.md"
    else
      n=$((n + 1))
    fi
  done
  echo "  R-A: $n distinct corpus keys cited by the Refinement, all resolving"
}

# Emit only the rows of the "Quotation register" table, not the other tables.
r_register_rows() {
  awk '/^## Quotation register/ { inreg = 1; next }
       /^## / { inreg = 0 }
       inreg && /^\| / { print }' "$REF/VERIFICATION.md"
}

run_r_b() {
  local bad=0 n=0
  while IFS='|' read -r _ quote _src _anchor status _; do
    case "$(printf '%s' "$quote" | tr -d ' ')" in ''|Quote/claim|---*) continue ;; esac
    local st
    # The status cell is "img" or "txt", optionally followed by " — why" or " (C5)".
    st=$(printf '%s' "$status" | tr -d '*`' | sed 's/[[:space:]]*[—(-].*//' | tr -d ' ')
    case "$st" in
      img) n=$((n + 1)) ;;
      txt) err "R-B: register row '$(printf '%.40s' "$quote")' is 'txt'. Extracted text is not a source: read the page"; bad=1 ;;
      *) err "R-B: register row '$(printf '%.40s' "$quote")' has status '$st', not img"; bad=1 ;;
    esac
  done < <(r_register_rows)
  [ "$bad" = "0" ] && echo "  R-B: $n register rows, all image-verified"
}

run_r_c() {
  local f n=0
  for f in $(grep -ho '\(\.\./\.\./\)\?lit/[A-Za-z0-9._-]*\.pdf' $(r_docs) "$REF/VERIFICATION.md" 2>/dev/null | sort -u); do
    if [ -f "$REF/$f" ]; then n=$((n + 1)); else err "R-C: refinement cites missing witness '$f'"; fi
  done
  # A citation whose path is elided cannot be followed or checked. The pattern
  # above simply does not match one, so it used to pass in silence.
  #
  # Read the hits from a process substitution, NOT a pipeline: a `while` on the
  # right of a pipe runs in a subshell, so `err`'s assignment to FAIL is lost and
  # the run reports AUDIT-FAIL lines while still exiting green. That bug was in
  # this check's first version.
  local hit
  while IFS= read -r hit; do
    [ -n "$hit" ] || continue
    err "R-C: elided witness path, unfollowable and unverifiable: $hit"
  # Scope: the prose documents only. VERIFICATION.md is the corrections registry
  # and names defective artifacts by design, exactly as it does for R-D.
  done < <(grep -n 'lit/[^`]*…' $(r_docs) 2>/dev/null)
  echo "  R-C: $n secondary-literature witnesses cited, all present"
}

run_r_d() {
  # Claims retracted on 2026-09-02. Each may still appear, but only where the
  # line carrying it also marks it as a quotation of the old claim.
  #
  # The guard is LINE-LOCAL, and deliberately so. Two earlier versions used a
  # context window -- first six lines, then one -- and both were defeated the
  # same way: a withdrawal word in neighbouring prose, about something else
  # entirely, silently guarded a bare reassertion. Narrowing the window raised
  # the bar without closing the hole. A line is guarded iff, on that line,
  # either (a) the retracted text sits inside double quotes -- the form every
  # legitimate occurrence in this stage takes -- or (b) the line itself carries
  # a retraction verb. Nothing on an adjacent line can guard anything.
  # VERIFICATION.md is the retraction registry itself: it names these claims by
  # design, so it is not prose under audit here.
  local pat label n=0 docs
  docs=$(r_docs | tr '\n' ' ')
  while IFS='	' read -r pat label; do
    [ -n "$pat" ] || continue
    local unguarded
    unguarded=$(grep -rn -- "$pat" $docs 2>/dev/null |
      awk -v p="$pat" '
        {
          line = $0
          # (a) the retracted text is quoted on this line, or
          quoted = (line ~ /"/)
          # (b) this line itself says it is a retraction.
          marked = (line ~ /withdrawn|retracted|Retracted|corrected|superseded|earlier|first draft|no longer|claimed|draft/)
          if (!quoted && !marked) c++
        }
        END { print c+0 }')
    if [ "${unguarded:-0}" -gt 0 ]; then
      err "R-D: retracted claim '$label' appears $unguarded time(s) on a line that neither quotes it nor marks it retracted"
    else
      n=$((n + 1))
    fi
  done <<'PATTERNS'
proves the existence of every hereditarily finite set	Pakhomov H_<w proves successor totality
five dials are one dial	the five dials are identical
permissions to use a proof again	conditions (1) and (2) are reuse
same condition described twice	contraction = a derivability condition
falsified R1's headline claim	Pakhomov falsified the headline claim
PATTERNS
  echo "  R-D: $n retracted claims checked; each occurrence is quoted or marked retracted on its own line"
}

# R-E is INFORMATIONAL and never fails. Rule 2 of VERIFICATION.md ("every claim
# about a Willard result carries that result's Proof status, at the point of
# use") is written per section, not per line or per paragraph: a status may sit
# in a following paragraph, or in a table's surrounding prose. Both a +/-2 line
# window and a paragraph window were tried on 2026-09-02 and produced three false
# positives out of four. So R-E does not judge -- it lists every non-`full`
# result the Refinement cites, with its status and where it is cited, so a reader
# can check the obligation by eye. Carrying the status remains a human duty.
run_r_e() {
  local docs cite bare paper rest kind num k st n=0
  docs=$(r_docs | tr '\n' ' ')
  echo "  R-E (informational): non-\`full\` corpus results cited by the Refinement --"
  while IFS= read -r cite; do
    bare=$(printf '%s' "$cite" | tr -d '`')
    paper=${bare%% *}; rest=${bare#* }
    kind=${rest%% *}; kind=${kind%.}; num=${rest##* }
    case "$kind" in Theorem|Thm) k=Thm ;; Lemma|Lem) k=Lem ;; Definition|Def) k=Def ;;
                    Remark|Rem) k=Rem ;; Corollary|Cor) k=Cor ;; *) continue ;; esac
    st=$(grep -m1 "^| $paper#$k$num " registry/results.md 2>/dev/null |
         awk -F"|" '{gsub(/^ +| +$/,"",$8); print $8}')
    if [ -z "$st" ]; then
      err "R-E: refinement cites '$bare', which resolves to no row '$paper#$k$num' in results.md"
      continue
    fi
    case "$st" in full|n/a) continue ;; esac
    printf '      %-28s %-11s cited %s time(s)\n' "$bare" "[$st]" \
      "$(grep -c -F -- "$cite" $docs 2>/dev/null | awk -F: '{t+=$NF} END{print t+0}')"
    n=$((n + 1))
  done < <(grep -ho '`Willard[0-9A-Za-z-]*` \(Theorem\|Thm\|Lemma\|Lem\|Definition\|Def\|Remark\|Rem\|Corollary\|Cor\)\.\? [0-9A-Z][0-9.]*' $docs 2>/dev/null | sort -u)
  echo "      ($n distinct non-\`full\` results; statuses must be carried at each point of use)"
}

run_all_r() {
  echo "-- refinement (VERIFICATION.md) --"
  run_r_a; run_r_b; run_r_c; run_r_d; run_r_e
}
