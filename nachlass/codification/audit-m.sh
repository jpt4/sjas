#!/usr/bin/env bash
# C16 mechanical checks M1–M8. Sourced by audit.sh (or run standalone after
# the baseline registry checks). Portable bash + awk; optional python3 for
# the OCR-tolerant M1 label sweep (falls back to a stricter awk sweep).
#
# M1 Inventory completeness     — fail
# M2 Near-duplicate Paper+Label — fail
# M3 Cross-ref / link resolution — fail
# M4 Internal § references      — fail
# M5 Derived prose counts       — fail
# M6 Id continuity report       — report only
# M7 Status-word proximity      — advisory queue
# M8 Stale-dependency queue     — advisory queue (AMENDED_AFTER=YYYY-MM-DD)
#
# Exclusion lists for M1 live in extraction/<paper>.md under a section
# headed exactly "## M1 exclusions" with a markdown table whose first
# data column is the swept label and whose second is the reason. A paper
# with no sources-text must declare "## M1 inventory" containing either
# `waiver:` plus a reason, or the same exclusions table.

set -u
LC_ALL=C

# When sourced from audit.sh these are already set; when run alone, set them.
: "${FAIL:=0}"
: "${ROOT:=$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)}"
cd "$(dirname "${BASH_SOURCE[0]}")"

err() { echo "AUDIT-FAIL: $*"; FAIL=1; }
warn() { echo "AUDIT-ADVISORY: $*"; }
info() { echo "AUDIT-INFO: $*"; }

trim() { local v="$1"; v="${v#"${v%%[![:space:]]*}"}"; v="${v%"${v##*[![:space:]]}"}"; printf '%s' "$v"; }

# ---------- helpers ----------

# Corpus keys marked extracted: or saturated:
extracted_keys() {
  awk -F'|' '
    /^\|/ {
      key=$2; gsub(/^[ \t]+|[ \t]+$/,"",key)
      extr=$8; gsub(/^[ \t]+|[ \t]+$/,"",extr)
      if (key=="" || key=="Key" || key ~ /^-+$/) next
      if (extr ~ /^(extracted|saturated):/) print key
    }
  ' registry/corpus.md
}

# Map corpus key -> sources-text path (relative), or empty if none.
sources_text_for() {
  local key="$1" sk
  sk=$(echo "$key" | tr 'A-Z' 'a-z')
  if [ -f "sources-text/${sk}.txt" ]; then
    echo "sources-text/${sk}.txt"
  fi
}

# Map corpus key -> extraction record path.
extraction_for() {
  local key="$1" sk
  sk=$(echo "$key" | tr 'A-Z' 'a-z')
  if [ -f "extraction/${sk}.md" ]; then
    echo "extraction/${sk}.md"
  elif [ -f "extraction/DEW-Resume.md" ] && [ "$key" = "DEW-Resume" ]; then
    echo "extraction/DEW-Resume.md"
  fi
}

# Normalize a label for comparison: lower-case, collapse space, unify stars.
norm_label() {
  # Unify Unicode mho-star to ASCII *; keep the star so "Theorem ∗" matches.
  # Do not put * inside a sed character class (quantifier hazard under -E).
  printf '%s' "$1" | tr 'A-Z' 'a-z' | sed -e 's/∗/*/g' -e 's/[[:space:]]\{1,\}/ /g' -e 's/^ //' -e 's/ $//'
  printf '\n'
}

# Parse M1 exclusions / waiver from an extraction record.
# Prints either: WAIVER\t<reason>
#            or: EXCL\t<norm-label>\t<reason>
parse_m1_exclusions() {
  local file="$1"
  [ -f "$file" ] || return 0
  awk -F'|' '
    BEGIN { inx=0; inv=0 }
    /^## M1 exclusions/ { inx=1; inv=0; next }
    /^## M1 inventory/  { inv=1; inx=0; next }
    /^## / {
      if (inx || inv) { inx=0; inv=0 }
      next
    }
    inv && /waiver:/ {
      line=$0
      sub(/.*waiver:[[:space:]]*/, "", line)
      gsub(/^[ \t]+|[ \t]+$/, "", line)
      print "WAIVER\t" line
      next
    }
    inx && /^\|/ {
      lab=$2; reason=$3
      gsub(/^[ \t]+|[ \t]+$/, "", lab)
      gsub(/^[ \t]+|[ \t]+$/, "", reason)
      if (lab=="" || lab=="Label" || lab ~ /^-+$/) next
      print "EXCL\t" lab "\t" reason
    }
  ' "$file" | while IFS=$'\t' read -r kind a b; do
    case "$kind" in
      WAIVER) printf 'WAIVER\t%s\n' "$a" ;;
      EXCL)   printf 'EXCL\t%s\t%s\n' "$(norm_label "$a")" "$b" ;;
    esac
  done
}

# ---------- M1: inventory completeness ----------

m1_sweep_labels() {
  # Emit normalized labels found in a sources-text file.
  # Prefer python3 for OCR-tolerant join of "D EFINITION" / "R EMARK".
  local txt="$1"
  if command -v python3 >/dev/null 2>&1; then
    python3 - "$txt" <<'PY'
import re, sys
path = sys.argv[1]
text = open(path, errors="replace").read()
# Hazard H1: leading letter split ("D EFINITION", "R EMARK", "L EMMA", ...)
text = re.sub(r"\b([A-Za-z]) ([A-Z]{2,})\b", r"\1\2", text)
TYPES = r"Definition|Theorem|Lemma|Corollary|Proposition|Remark|Conjecture|Claim|Example|Comment|Fact|Observation"
pat = re.compile(rf"(?i)\b({TYPES})\s+((?:[∗*]\s*)?[0-9]+(?:\.[0-9]+)?[A-Za-z]?|[∗*])")
seen = set()
for m in pat.finditer(text):
    typ, num = m.group(1), m.group(2)
    num = re.sub(r"\s+", "", num).replace("∗", "*")
    if num.startswith("0") and not re.match(r"0\.[1-9]", num):
        continue
    after = text[m.end(): m.end() + 60]
    # Citations / possessives of other results — not inventory headers
    if re.match(r"(?i)^(\s*[’']s|\s+of\s+\[|\s+from\s+\[|\s+in\s+\[|\s+of\s+[A-Z]|\s*=)", after):
        continue
    # "Theorem 2.3 from Pudlak" / "Theorem 3.4 of [46]" already partly caught;
    # also skip bare external-looking "Theorem N.M" when followed by " of "
    if re.match(r"(?i)^\s+of\s+", after):
        continue
    lab = f"{typ.lower()} {num.lower().replace('∗','*')}"
    lab = re.sub(r"\s+", " ", lab).strip()
    seen.add(lab)
for lab in sorted(seen):
    print(lab)
PY
  else
    # Stricter awk fallback: line-start headers only, no OCR join.
    awk 'BEGIN{IGNORECASE=1}
      {
        line=$0; gsub(/[ \t]+/," ",line); sub(/^ /,"",line)
        if (match(line,/^(Definition|Theorem|Lemma|Corollary|Proposition|Remark|Conjecture|Claim|Example|Comment|Fact|Observation)[ ]+([∗*] )?([0-9]+([.][0-9]+)?[A-Za-z]?|[∗*])/)) {
          m=substr(line,RSTART,RLENGTH)
          gsub(/[∗*]/,"*",m); gsub(/ +/," ",m); sub(/ $/,"",m)
          print tolower(m)
        }
      }' "$txt" | sort -u
  fi
}

m1_results_labels() {
  local paper="$1"
  awk -F'|' -v p="$paper" '
    /^\|/ {
      id=$2; paper=$3; lab=$4
      gsub(/^[ \t]+|[ \t]+$/,"",paper)
      gsub(/^[ \t]+|[ \t]+$/,"",lab)
      if (paper!=p) next
      if (id=="Id" || id ~ /^-+$/) next
      print lab
    }
  ' registry/results.md | while IFS= read -r lab; do norm_label "$lab"; done
}

# Does results-label set cover a swept label?
m1_covered() {
  local lab="$1"
  local resfile="$2"
  grep -qxF "$lab" "$resfile" && return 0
  # Prefix / containment: "definitions 6.1-6.2" covers "definition 6.1"
  local typ num
  typ=$(echo "$lab" | awk '{print $1}')
  num=$(echo "$lab" | awk '{print $2}')
  [ -n "$num" ] || return 1
  # singular/plural type stem
  local stem="$typ"
  case "$typ" in
    definition) stem='definition' ;;
    theorem) stem='theorem' ;;
    lemma) stem='lemma' ;;
  esac
  awk -v stem="$stem" -v num="$num" '
    {
      n=$0
      # strip trailing prose after comma for token match
      if (index(n, stem) && index(n, num)) found=1
    }
    END { exit !found }
  ' "$resfile"
}

run_m1() {
  echo "--- M1 inventory completeness ---"
  local key txt ext tmp_res tmp_sw n_miss=0 n_papers=0 n_waiver=0
  tmp_res=$(mktemp)
  tmp_sw=$(mktemp)
  while IFS= read -r key; do
    n_papers=$((n_papers + 1))
    txt=$(sources_text_for "$key")
    ext=$(extraction_for "$key")
    : > "$tmp_res"
    : > "$tmp_sw"

    # Load exclusions / waiver
    local waiver="" 
    declare -A excl=()
    if [ -n "$ext" ]; then
      while IFS=$'\t' read -r kind a b; do
        case "$kind" in
          WAIVER) waiver="$a" ;;
          EXCL) excl["$a"]="$b" ;;
        esac
      done < <(parse_m1_exclusions "$ext")
    fi

    if [ -z "$txt" ]; then
      if [ -n "$waiver" ]; then
        info "M1[$key]: waived — $waiver"
        n_waiver=$((n_waiver + 1))
      else
        err "M1[$key]: no sources-text and no '## M1 inventory' waiver in extraction record"
      fi
      unset excl
      continue
    fi

    m1_sweep_labels "$txt" > "$tmp_sw"
    m1_results_labels "$key" > "$tmp_res"

    while IFS= read -r lab; do
      [ -z "$lab" ] && continue
      if [ -n "${excl[$lab]:-}" ]; then
        continue
      fi
      if m1_covered "$lab" "$tmp_res"; then
        continue
      fi
      err "M1[$key]: swept label '$lab' has no results.md row (declare under ## M1 exclusions with reason, or add a row)"
      n_miss=$((n_miss + 1))
    done < "$tmp_sw"
    unset excl
  done < <(extracted_keys)
  info "M1: checked $n_papers extracted papers; $n_waiver waivers; missing labels reported above if any"
  rm -f "$tmp_res" "$tmp_sw"
}

# ---------- M2: near-duplicate Paper+Label ----------

run_m2() {
  echo "--- M2 near-duplicate Paper+Label ---"
  local dups
  dups=$(mktemp)
  awk -F'|' '
    /^\|/ {
      id=$2; paper=$3; lab=$4
      gsub(/^[ \t]+|[ \t]+$/,"",id)
      gsub(/^[ \t]+|[ \t]+$/,"",paper)
      gsub(/^[ \t]+|[ \t]+$/,"",lab)
      if (id=="" || id=="Id" || id ~ /^-+$/) next
      key=paper "\t" lab
      if (key in seen) {
        print seen[key] "\t" id "\t" paper "\t" lab
      } else seen[key]=id
    }
  ' registry/results.md > "$dups"
  while IFS=$'\t' read -r a b paper lab; do
    [ -z "${a:-}" ] && continue
    err "M2: Paper+Label duplicate: '$paper' / '$lab' as $a and $b"
  done < "$dups"
  rm -f "$dups"
}

# ---------- M3: cross-reference resolution ----------

# Collect declared O / D / G ids
collect_odg_ids() {
  # O from composition-obligations
  awk -F'|' '
    /^\|/ {
      id=$2; gsub(/^[ \t*]+|[ \t*]+$/,"",id)
      if (id ~ /^O[0-9]+$/) print id
    }
  ' concordance/composition-obligations.md
  # D from drift-ledger headings
  awk '
    /^## D[0-9]+/ {
      match($0, /D[0-9]+/)
      print substr($0, RSTART, RLENGTH)
    }
  ' concordance/drift-ledger.md
  # G from gaps.md
  awk -F'|' '
    /^\|/ {
      id=$2; gsub(/^[ \t]+|[ \t]+$/,"",id)
      if (id ~ /^G[0-9]+$/) print id
    }
  ' registry/gaps.md
}

run_m3() {
  echo "--- M3 cross-reference and link resolution ---"
  local idfile reffile linkfile
  idfile=$(mktemp)
  reffile=$(mktemp)
  linkfile=$(mktemp)
  collect_odg_ids | sort -u > "$idfile"

  local docs=(
    codified-sjas.md
    ADR-0001-codification-charter.md
    README.md
    C16-verification-goal.md
    concordance/axes.md
    concordance/genealogy.md
    concordance/result-matrix.md
    concordance/replication-map.md
    concordance/drift-ledger.md
    concordance/composition-obligations.md
    registry/results.md
    registry/systems.md
    registry/notation.md
    registry/coverage.md
    registry/gaps.md
    registry/corpus.md
  )
  local f
  for f in extraction/*.md; do docs+=("$f"); done

  : > "$reffile"
  : > "$linkfile"
  for f in "${docs[@]}"; do
    [ -f "$f" ] || continue
    # bold refs **O12** etc.
    grep -oE '\*\*[ODG][0-9]+\*\*' "$f" 2>/dev/null | tr -d '*' | sort -u | while read -r id; do
      printf '%s\t%s\n' "$f" "$id"
    done >> "$reffile"
    # Full markdown links [text](path) — not bare function-call (x̄) false positives
    grep -oE '\[[^]]+\]\([^)]+\)' "$f" 2>/dev/null | sed -E 's/^\[[^]]+\]\((.*)\)$/\1/' | while read -r link; do
      printf '%s\t%s\n' "$f" "$link"
    done >> "$linkfile"
  done

  while IFS=$'\t' read -r f id; do
    [ -z "${id:-}" ] && continue
    grep -qxF "$id" "$idfile" || err "M3[$f]: unresolved reference **${id}**"
  done < "$reffile"

  while IFS=$'\t' read -r f link; do
    [ -z "${link:-}" ] && continue
    case "$link" in
      http://*|https://*|mailto:*|\#*) continue ;;
    esac
    local path="${link%%#*}"
    [ -z "$path" ] && continue
    # Skip math/function false positives like [i,j](x̄): require path-ish targets
    case "$path" in
      *.md|*.md.*|*/*|../*|./*|[A-Za-z0-9_-]*.md) ;;
      *) continue ;;
    esac
    local dir
    dir=$(dirname "$f")
    if [ ! -e "$dir/$path" ] && [ ! -e "$path" ]; then
      err "M3[$f]: broken relative link ($link)"
    fi
  done < "$linkfile"

  rm -f "$idfile" "$reffile" "$linkfile"
}

# ---------- M4: internal § references in codified-sjas.md ----------

run_m4() {
  echo "--- M4 internal section references ---"
  local heads secs
  heads=$(mktemp)
  secs=$(mktemp)
  awk '
    /^#{2,3} / {
      line=$0
      sub(/^#+ /, "", line)
      if (match(line, /^[0-9]+(\.[0-9]+)*/)) {
        print substr(line, RSTART, RLENGTH)
      }
    }
  ' codified-sjas.md | sort -u > "$heads"
  grep -oE '§[0-9]+(\.[0-9]+)*' codified-sjas.md | tr -d '§' | sort -u > "$secs"
  while IFS= read -r sec; do
    [ -z "$sec" ] && continue
    case "$sec" in
      0|0.*) continue ;;
    esac
    if ! grep -qxF "$sec" "$heads"; then
      err "M4: §${sec} in codified-sjas.md has no matching heading"
    fi
  done < "$secs"
  rm -f "$heads" "$secs"
}

# ---------- M5: derived prose counts ----------

run_m5() {
  echo "--- M5 derived registry counts ---"
  local n_results n_systems n_notation n_obligations n_coverage n_gaps n_corpus
  n_results=$(awk -F'|' 'NR>1 && /^\|/ {id=$2; gsub(/^[ \t]+|[ \t]+$/,"",id); if(id!="" && id!="Id" && id !~ /^-+$/) c++} END{print c+0}' registry/results.md)
  n_systems=$(awk -F'|' 'NR>1 && /^\|/ {id=$2; gsub(/^[ \t]+|[ \t]+$/,"",id); if(id!="" && id!="Canonical" && id !~ /^-+$/) c++} END{print c+0}' registry/systems.md)
  n_notation=$(awk -F'|' 'NR>1 && /^\|/ {p=$2; gsub(/^[ \t]+|[ \t]+$/,"",p); if(p!="" && p!="Paper" && p !~ /^-+$/) c++} END{print c+0}' registry/notation.md)
  n_coverage=$(awk -F'|' 'NR>1 && /^\|/ {id=$2; gsub(/^[ \t]+|[ \t]+$/,"",id); if(id!="" && id!="Item" && id !~ /^-+$/ && id !~ /^\*\*/) c++} END{print c+0}' registry/coverage.md)
  n_gaps=$(awk -F'|' 'NR>1 && /^\|/ {id=$2; gsub(/^[ \t]+|[ \t]+$/,"",id); if(id ~ /^G[0-9]+$/) c++} END{print c+0}' registry/gaps.md)
  n_corpus=$(awk -F'|' 'NR>1 && /^\|/ {id=$2; gsub(/^[ \t]+|[ \t]+$/,"",id); if(id!="" && id!="Key" && id !~ /^-+$/) c++} END{print c+0}' registry/corpus.md)
  n_obligations=$(awk -F'|' 'NR>1 && /^\|/ {id=$2; gsub(/^[ \t*]+|[ \t*]+$/,"",id); if(id ~ /^O[0-9]+$/) c++} END{print c+0}' concordance/composition-obligations.md)

  info "M5 actuals: results=$n_results systems=$n_systems notation=$n_notation coverage=$n_coverage gaps=$n_gaps corpus=$n_corpus obligations=$n_obligations"

  # Narrow patterns only — deficiency 9 forms, not every "N systems" in prose.
  # Accept: "528 results,", "528 numbered items", "62 systems,", "224 notation rows",
  # "87 obligations" when it is the registry tally (not "57 of 87 obligations").
  check_one() {
    local file="$1" regex="$2" actual="$3" label="$4"
    local hit num
    hit=$(grep -nEo "$regex" "$file" 2>/dev/null || true)
    [ -z "$hit" ] && return 0
    while IFS= read -r line; do
      num=$(echo "${line#*:}" | grep -oE '^[0-9]+')
      [ -z "$num" ] && continue
      if [ "$num" != "$actual" ]; then
        err "M5[$file]: prose '${line#*:}' ($label) != actual $actual"
      fi
    done <<< "$hit"
  }

  for f in README.md codified-sjas.md C16-verification-goal.md; do
    [ -f "$f" ] || continue
    check_one "$f" '[0-9]+ results,' "$n_results" results
    check_one "$f" '[0-9]+ numbered items' "$n_results" results
    check_one "$f" '[0-9]+ notation rows' "$n_notation" notation
    # systems: require a digit-run immediately before " systems," not preceded by ~
    hit=$(grep -nE '[^~0-9][0-9]+ systems,|^[0-9]+ systems,' "$f" 2>/dev/null || true)
    while IFS= read -r line; do
      [ -z "$line" ] && continue
      body="${line#*:}"
      num=$(echo "$body" | grep -oE '[0-9]+ systems,' | head -1 | grep -oE '^[0-9]+')
      # Prefer the last match on the line when "~20" and "62" both appear — take matches not after ~
      num=$(echo "$body" | python3 -c "import re,sys; s=sys.stdin.read(); ms=[m.group(1) for m in re.finditer(r'(?<!~)(\d+) systems,', s)]; print(ms[-1] if ms else '')")
      [ -z "$num" ] && continue
      if [ "$num" != "$n_systems" ]; then
        err "M5[$f]: prose '$num systems,' != actual $n_systems"
      fi
    done <<< "$hit"
  done
  # Standalone "87 obligations" tally (word before must not be "of")
  for f in README.md codified-sjas.md concordance/composition-obligations.md ADR-0001-codification-charter.md; do
    [ -f "$f" ] || continue
    hit=$(grep -nE '(^|[^f] )[0-9]+ obligations' "$f" 2>/dev/null || true)
    # Filter: drop "of N obligations"
    while IFS= read -r line; do
      [ -z "$line" ] && continue
      body="${line#*:}"
      echo "$body" | grep -qE 'of[[:space:]]+[0-9]+[[:space:]]+obligations' && continue
      num=$(echo "$body" | grep -oE '[0-9]+ obligations' | head -1 | grep -oE '^[0-9]+')
      [ -z "$num" ] && continue
      if [ "$num" != "$n_obligations" ]; then
        err "M5[$f]: prose '$num obligations' != actual $n_obligations"
      fi
    done <<< "$hit"
  done
}

# ---------- M6: id continuity report (non-fatal) ----------

run_m6() {
  echo "--- M6 id continuity report ---"
  report_gaps() {
    local prefix="$1"
    shift
    local ids=("$@")
    [ "${#ids[@]}" -eq 0 ] && return 0
    local sorted min max i
    sorted=$(printf '%s\n' "${ids[@]}" | sort -n)
    min=$(echo "$sorted" | head -1)
    max=$(echo "$sorted" | tail -1)
    local missing=""
    for i in $(seq "$min" "$max"); do
      echo "$sorted" | grep -qxF "$i" || missing="$missing $i"
    done
    if [ -n "$missing" ]; then
      info "M6: ${prefix} sequence gaps (deliberate or accidental):$missing"
    else
      info "M6: ${prefix} sequence $min–$max contiguous (${#ids[@]} ids)"
    fi
  }
  local d_ids o_ids g_ids
  d_ids=$(awk '/^## D[0-9]+/ { match($0,/D[0-9]+/); print substr($0,RSTART+1,RLENGTH-1) }' concordance/drift-ledger.md)
  o_ids=$(awk -F'|' '/^\|/ {id=$2; gsub(/^[ \t*]+|[ \t*]+$/,"",id); if(id ~ /^O[0-9]+$/) print substr(id,2) }' concordance/composition-obligations.md)
  g_ids=$(awk -F'|' '/^\|/ {id=$2; gsub(/^[ \t]+|[ \t]+$/,"",id); if(id ~ /^G[0-9]+$/) print substr(id,2) }' registry/gaps.md)
  # bash arrays from lines
  local da oa ga
  mapfile -t da <<< "$d_ids"
  mapfile -t oa <<< "$o_ids"
  mapfile -t ga <<< "$g_ids"
  report_gaps "D" "${da[@]}"
  report_gaps "O" "${oa[@]}"
  report_gaps "G" "${ga[@]}"
}

# ---------- M7: status-word proximity (advisory) ----------

run_m7() {
  echo "--- M7 status-word proximity queue ---"
  local n=12
  # Build map: result-id fragment -> proof status for sketch/cited/stated-only
  local soft
  soft=$(mktemp)
  awk -F'|' '
    /^\|/ {
      id=$2; lab=$4; proof=$8
      gsub(/^[ \t]+|[ \t]+$/,"",id)
      gsub(/^[ \t]+|[ \t]+$/,"",lab)
      gsub(/^[ \t]+|[ \t]+$/,"",proof)
      if (id=="" || id=="Id" || id ~ /^-+$/) next
      if (proof=="sketch" || proof=="cited" || proof=="stated-only") {
        print id "\t" lab "\t" proof
      }
    }
  ' registry/results.md > "$soft"

  local f
  for f in codified-sjas.md concordance/*.md; do
    [ -f "$f" ] || continue
    # Sentences containing a soft-status result id near a strong verb.
    # Heuristic: same line has both a WillardYYYY#Id (or Label) and a verb.
    grep -nE 'proved|proves|establishes|shows|demonstrates' "$f" 2>/dev/null | while IFS= read -r hit; do
      local line lineno
      lineno=$(echo "$hit" | cut -d: -f1)
      line=$(echo "$hit" | cut -d: -f2-)
      while IFS=$'\t' read -r id lab proof; do
        if echo "$line" | grep -qF "$id"; then
          warn "M7[$f:$lineno]: '$id' is $proof, near strong status verb — review queue"
        fi
      done < "$soft"
    done
  done
  rm -f "$soft"
}

# ---------- M8: stale-dependency queue (advisory) ----------

run_m8() {
  echo "--- M8 stale-dependency queue ---"
  local since="${AMENDED_AFTER:-2026-08-28}"
  info "M8: scanning gap/drift entries amended after $since"

  # Gaps with closed:YYYY-MM-DD or status lines mentioning a date >= since
  local g
  while IFS=$'\t' read -r gid status; do
    local d=""
    if echo "$status" | grep -qE 'closed:[0-9]{4}-[0-9]{2}-[0-9]{2}'; then
      d=$(echo "$status" | grep -oE 'closed:[0-9]{4}-[0-9]{2}-[0-9]{2}' | head -1 | cut -d: -f2)
    fi
    [ -z "$d" ] && continue
    if [[ "$d" > "$since" || "$d" == "$since" ]]; then
      warn "M8: gap $gid amended $d — dependents:"
      grep -nH -E "\\*\\*${gid}\\*\\*|\\b${gid}\\b" \
        codified-sjas.md concordance/*.md registry/*.md extraction/*.md 2>/dev/null \
        | grep -v "^registry/gaps.md" \
        | head -40 | while IFS= read -r dep; do
          info "  cites: $dep"
        done
    fi
  done < <(awk -F'|' '
    /^\|/ {
      id=$2; st=$5
      gsub(/^[ \t]+|[ \t]+$/,"",id)
      gsub(/^[ \t]+|[ \t]+$/,"",st)
      if (id ~ /^G[0-9]+$/) print id "\t" st
    }
  ' registry/gaps.md)

  # Drift entries whose Status line carries explained:YYYY-MM-DD >= since,
  # or whose body mentions an amendment date. Parse "## Dnn" blocks.
  awk -v since="$since" '
    /^## D[0-9]+/ {
      if (cur != "" && date != "" && date >= since) {
        print cur "\t" date
      }
      match($0, /D[0-9]+/)
      cur = substr($0, RSTART, RLENGTH)
      date = ""
      next
    }
    cur != "" && /explained:[0-9]{4}-[0-9]{2}-[0-9]{2}/ {
      if (match($0, /explained:[0-9]{4}-[0-9]{2}-[0-9]{2}/)) {
        date = substr($0, RSTART+10, 10)
      }
    }
    cur != "" && /Status:.*[0-9]{4}-[0-9]{2}-[0-9]{2}/ {
      if (date == "" && match($0, /[0-9]{4}-[0-9]{2}-[0-9]{2}/)) date = substr($0, RSTART, 10)
    }
    END {
      if (cur != "" && date != "" && date >= since) print cur "\t" date
    }
  ' concordance/drift-ledger.md | while IFS=$'\t' read -r did d; do
    warn "M8: drift $did amended $d — dependents:"
    grep -nH -E "\\*\\*${did}\\*\\*|\\b${did}\\b" \
      codified-sjas.md concordance/*.md registry/*.md extraction/*.md 2>/dev/null \
      | grep -v "^concordance/drift-ledger.md" \
      | head -40 | while IFS= read -r dep; do
        info "  cites: $dep"
      done
  done
}

#   M-N  (informational) drift headings that state a count, against the row
#        count of their own table. Hand-maintained counts in headings went
#        stale twice: D38 read "three dials" over a five-row table (four stale
#        numbers in one entry, F-L2), and D50 read "ten formula-class
#        notations" over six rows (F-L3). Neither is machine-decidable -- a
#        heading number need not be a row count (D25's "6 / 5 / 6-over-5 / 32"
#        is a constant, not a tally) -- so this lists rather than fails, and a
#        reader checks the pairs.
run_m_n() {
  local led="concordance/drift-ledger.md"
  echo "  M-N (informational): drift headings stating a count, vs their own table rows --"
  awk '
    BEGIN { n = split("one two three four five six seven eight nine ten eleven twelve", w, " ") }
    /^## D[0-9]+ / {
      cur = $2; heads[cur] = $0; rows[cur] = 0; wantn[cur] = 0
      low = tolower($0)
      for (i = 1; i <= n; i++)
        if (low ~ ("(^|[^a-z])" w[i] "([^a-z]|$)")) { want[cur] = w[i]; wantn[cur] = i }
      next
    }
    /^\|/ { if (cur != "") rows[cur]++ }
    END {
      for (id in wantn) {
        if (wantn[id] == 0) continue
        r = (rows[id] >= 2) ? rows[id] - 2 : 0
        if (r == 0) continue
        if (wantn[id] != r)
          printf("      %-5s heading says %-6s table has %d row(s)  -- %s\n",
                 id, want[id], r, substr(heads[id], 1, 62))
      }
    }
  ' "$led" | sort
  echo "      (a heading number need not be a row count -- D25 tallies constants,"
  echo "       not entries. Check the pairs listed; each is a claim to verify.)"
}

run_all_m() {
  run_m1
  run_m2
  run_m3
  run_m4
  run_m5
  run_m6
  run_m7
  run_m8
  run_m_n
}

# If executed directly (not sourced), run and exit.
if [[ "${BASH_SOURCE[0]}" == "$0" ]]; then
  FAIL=0
  run_all_m
  if [ "$FAIL" -eq 0 ]; then echo "M-CHECKS PASS"; exit 0; else echo "M-CHECKS FAIL"; exit 1; fi
fi
