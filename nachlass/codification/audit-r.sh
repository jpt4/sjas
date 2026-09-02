#!/usr/bin/env bash
# Refinement checks for the SJAS synthesis.
#
# The Refinement stage was opened outside this audit's reach; the 2026-09-02
# review found that every Tier-1 defect it committed was of a kind the
# Codification's machinery exists to prevent. These checks extend that
# machinery across the stage boundary. Contract: refinement/VERIFICATION.md.
#
#   R-A  every paper key cited in refinement/*.md resolves in corpus.md
#   R-B  VERIFICATION.md's quotation register uses only img|txt statuses,
#        and no `txt` row's quote contains mathematical notation
#   R-C  every held secondary-literature file in the register exists
#   R-D  no Refinement document reasserts a claim the register records as
#        corrected (stale-retraction check)

REF="../refinement"

r_paper_keys() {
  # Paper keys look like Willard2011, Willard2007-APAL, Willard1993-TR.
  grep -ho '\bWillard[0-9]\{4\}[a-z]\?\(-[A-Za-z]\+\)\?' "$REF"/*.md 2>/dev/null | sort -u
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
      img|txt) n=$((n + 1)) ;;
      *) err "R-B: register row '$(printf '%.40s' "$quote")' has status '$st', not img|txt"; bad=1; continue ;;
    esac
    if [ "$st" = "txt" ] && printf '%s' "$quote" | grep -q '[□⌜⌝♯⊢≥≤∧⊃¬∀∃⊠⊤⊥]'; then
      err "R-B: register row '$(printf '%.40s' "$quote")' is txt-only but carries mathematics"
      bad=1
    fi
  done < <(r_register_rows)
  [ "$bad" = "0" ] && echo "  R-B: $n register rows well-formed; no txt row carries mathematics"
}

run_r_c() {
  local f n=0
  for f in $(grep -ho '\(\.\./\.\./\)\?lit/[A-Za-z0-9._-]*\.pdf' "$REF"/*.md 2>/dev/null | sort -u); do
    if [ -f "$REF/$f" ]; then n=$((n + 1)); else err "R-C: refinement cites missing witness '$f'"; fi
  done
  echo "  R-C: $n secondary-literature witnesses cited, all present"
}

run_r_d() {
  # Claims retracted on 2026-09-02. Each may still appear, but only inside a
  # passage that marks it withdrawn. The marker must sit on the matching line
  # or the line immediately adjacent -- a wider window lets an unrelated
  # retraction elsewhere in the section act as a spurious guard, which is how
  # this check first passed when it should not have.
  # VERIFICATION.md is the retraction registry itself: it names these claims by
  # design, so it is not prose under audit here.
  local pat label n=0 docs
  docs=$(ls "$REF"/*.md | grep -v '/VERIFICATION\.md$' | tr '\n' ' ')
  while IFS='	' read -r pat label; do
    [ -n "$pat" ] || continue
    local unguarded
    unguarded=$(grep -rn -C1 -- "$pat" $docs 2>/dev/null |
      awk -v p="$pat" '
        function flush() {
          if (block != "") {
            if (block ~ p && block !~ /withdrawn|retracted|corrected|superseded|earlier|first draft|no longer|claimed|That draft|that draft/) c++
          }
          block = ""
        }
        /^--$/ { flush(); next }
        { block = block "\n" $0 }
        END { flush(); print c+0 }')
    if [ "${unguarded:-0}" -gt 0 ]; then
      err "R-D: retracted claim '$label' appears $unguarded time(s) without an adjacent withdrawal marker"
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
  echo "  R-D: $n retracted claims checked; each occurrence carries an adjacent withdrawal marker"
}

run_all_r() {
  echo "-- refinement (VERIFICATION.md) --"
  run_r_a; run_r_b; run_r_c; run_r_d
}
