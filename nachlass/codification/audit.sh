#!/usr/bin/env bash
# Registry integrity audit for the SJAS Codification.
# Charter: ADR-0001-codification-charter.md. Portable bash + awk only.
#
# Checks:
#   corpus.md    - key uniqueness; ruling/tier/extraction enums;
#                  ruling<->extraction consistency; witness files exist.
#   results.md   - every Paper value is a corpus key; Id uniqueness.
#   systems.md   - every Paper value is a corpus key.
#   notation.md  - every Paper value is a corpus key.
#   gaps.md      - id uniqueness; status enum.
# Exit 0 with "AUDIT PASS" iff no check fails.

set -u
LC_ALL=C
cd "$(dirname "$0")"
ROOT="$(cd ../.. && pwd)"   # repository root
FAIL=0
err() { echo "AUDIT-FAIL: $*"; FAIL=1; }

# Emit data rows of the markdown tables in a file as tab-separated cells.
# Skips separator rows (cells of dashes/colons) and header rows, which are
# recognized by their first cell being one of the schema header tokens.
rows() {
  awk -F'|' '
    /^\|/ {
      sep = 1
      for (i = 2; i < NF; i++) {
        c = $i
        gsub(/^[ \t]+|[ \t]+$/, "", c)
        if (c !~ /^[-: ]*$/) sep = 0
        cell[i] = c
      }
      if (sep) next
      if (cell[2] == "Key" || cell[2] == "Id" || cell[2] == "Paper" || cell[2] == "Canonical") next
      out = ""
      for (i = 2; i < NF; i++) out = out (i > 2 ? "\t" : "") cell[i]
      print out
      delete cell
    }
  ' "$1"
}

for f in registry/corpus.md registry/results.md registry/systems.md \
         registry/notation.md registry/gaps.md; do
  [ -f "$f" ] || err "missing registry file: $f"
done
[ "$FAIL" -eq 1 ] && { echo "AUDIT FAIL"; exit 1; }

# ---------- corpus.md ----------
CORPUS_KEYS=""
n_rows=0
declare -A seen ruling_count extr_count
while IFS=$'\t' read -r key witness pages ruling tier rationale extraction; do
  [ -z "$key" ] && { err "corpus: empty key in a data row"; continue; }
  n_rows=$((n_rows + 1))
  if [ -n "${seen[$key]:-}" ]; then err "corpus: duplicate key '$key'"; fi
  seen[$key]=1
  CORPUS_KEYS="$CORPUS_KEYS $key"

  case "$ruling" in
    core|motivation|witness|out|gap|gap-accepted) ;;
    *) err "corpus[$key]: bad ruling '$ruling'" ;;
  esac
  case "$tier" in
    pilot|origin|spine|A|B|C|D|-) ;;
    *) err "corpus[$key]: bad tier '$tier'" ;;
  esac
  if ! echo "$extraction" | grep -qE '^(pending|n/a|blocked|accepted|(extracted|saturated):[0-9]{4}-[0-9]{2}-[0-9]{2})$'; then
    err "corpus[$key]: bad extraction '$extraction'"
  fi
  case "$ruling" in
    out)          [ "$extraction" = "n/a" ]      || err "corpus[$key]: ruling out requires extraction n/a" ;;
    gap)          [ "$extraction" = "blocked" ]  || err "corpus[$key]: ruling gap requires extraction blocked" ;;
    gap-accepted) [ "$extraction" = "accepted" ] || err "corpus[$key]: ruling gap-accepted requires extraction accepted" ;;
    *) echo "$extraction" | grep -qE '^(pending|(extracted|saturated):)' \
         || err "corpus[$key]: ruling $ruling requires pending/extracted/saturated" ;;
  esac
  [ -z "${ruling_count[$ruling]:-}" ] && ruling_count[$ruling]=0
  ruling_count[$ruling]=$(( ${ruling_count[$ruling]} + 1 ))
  ex_class="${extraction%%:*}"
  [ -z "${extr_count[$ex_class]:-}" ] && extr_count[$ex_class]=0
  extr_count[$ex_class]=$(( ${extr_count[$ex_class]} + 1 ))

  # witness paths
  if [ "$witness" = "(none)" ] || [ -z "$witness" ]; then
    case "$ruling" in
      gap|gap-accepted) ;;
      *) err "corpus[$key]: ruling $ruling requires at least one witness path" ;;
    esac
  else
    IFS=';' read -ra paths <<< "$witness"
    for p in "${paths[@]}"; do
      p="$(echo "$p" | sed 's/^[ \t]*//; s/[ \t]*$//')"
      [ -e "$ROOT/$p" ] || err "corpus[$key]: witness path not found: $p"
    done
  fi
done < <(rows registry/corpus.md)
[ "$n_rows" -ge 1 ] || err "corpus: no data rows parsed"

in_corpus_key() { echo "$CORPUS_KEYS" | grep -qw "$1"; }

# ---------- results.md ----------
declare -A rid_seen proof_count
n_results=0
while IFS=$'\t' read -r id paper label type page topic proof depends notes; do
  [ -z "$id" ] && continue
  n_results=$((n_results + 1))
  [ -n "${rid_seen[$id]:-}" ] && err "results: duplicate id '$id'"
  rid_seen[$id]=1
  in_corpus_key "$paper" || err "results[$id]: unknown corpus key '$paper'"
  case "$type" in def|thm|lemma|cor|prop|remark|conj|example) ;; *) err "results[$id]: bad type '$type'" ;; esac
  case "$proof" in
    full|sketch|cited|stated-only|unverified|n/a) ;;
    *) err "results[$id]: bad proof status '$proof'" ;;
  esac
  [ -z "${proof_count[$proof]:-}" ] && proof_count[$proof]=0
  proof_count[$proof]=$(( ${proof_count[$proof]} + 1 ))
done < <(rows registry/results.md)

# ---------- systems.md ----------
n_systems=0
while IFS=$'\t' read -r canonical paper rest; do
  [ -z "$canonical" ] && continue
  n_systems=$((n_systems + 1))
  in_corpus_key "$paper" || err "systems[$canonical]: unknown corpus key '$paper'"
done < <(rows registry/systems.md)

# ---------- notation.md ----------
n_notation=0
while IFS=$'\t' read -r paper symbol rest; do
  [ -z "$paper" ] && continue
  n_notation=$((n_notation + 1))
  in_corpus_key "$paper" || err "notation: unknown corpus key '$paper' (symbol '$symbol')"
done < <(rows registry/notation.md)

# ---------- gaps.md ----------
declare -A gid_seen
n_gaps=0
while IFS=$'\t' read -r gid item kind status action; do
  [ -z "$gid" ] && continue
  n_gaps=$((n_gaps + 1))
  [ -n "${gid_seen[$gid]:-}" ] && err "gaps: duplicate id '$gid'"
  gid_seen[$gid]=1
  if ! echo "$status" | grep -qE '^(open|accepted|refinement-prep|closed:[0-9]{4}-[0-9]{2}-[0-9]{2})$'; then
    err "gaps[$gid]: bad status '$status'"
  fi
done < <(rows registry/gaps.md)

# ---------- summary ----------
echo "corpus rows: $n_rows"
for r in core motivation witness out gap gap-accepted; do
  [ -n "${ruling_count[$r]:-}" ] && echo "  ruling $r: ${ruling_count[$r]}"
done
for e in pending extracted saturated n/a blocked accepted; do
  [ -n "${extr_count[$e]:-}" ] && echo "  extraction $e: ${extr_count[$e]}"
done
echo "results rows: $n_results"
for p in full sketch cited stated-only unverified n/a; do
  [ -n "${proof_count[$p]:-}" ] && echo "  proof $p: ${proof_count[$p]}"
done
echo "systems rows: $n_systems"
echo "notation rows: $n_notation"
trim() { local v="$1"; v="${v#"${v%%[![:space:]]*}"}"; v="${v%"${v##*[![:space:]]}"}"; printf '%s' "$v"; }
# --- coverage registry: every extracted corpus item must declare what was read ---
COVSEEN=""
cov="registry/coverage.md"
[ -f "$cov" ] || err "coverage: registry/coverage.md is missing"
cov_rows=0
while IFS='|' read -r _ item pages read_r swept images state _rest; do
  item=$(trim "$item"); state=$(trim "$state"); read_r=$(trim "$read_r")
  case "$item" in ''|Item|---|'**Open'*) continue ;; esac
  cov_rows=$((cov_rows+1))
  case "$state" in
    complete|partial|unrecorded) ;;
    *) err "coverage[$item]: bad state '$state'" ;;
  esac
  [ -n "$read_r" ] || err "coverage[$item]: empty Read column"
  if [ "$state" = "complete" ] && [ "$read_r" = "unrecorded" ]; then
    err "coverage[$item]: state 'complete' with unrecorded Read ranges"
  fi
  # the visual control pass is mandatory for all tiers (ADR-0001 "Visual control"),
  # so a row that records reading must also record which pages were rendered
  images=$(trim "$images")
  if [ "$read_r" != "unrecorded" ] && [ -z "$images" ]; then
    err "coverage[$item]: empty Images column; the visual control pass is mandatory for all tiers"
  fi
  case "$(echo "$images" | tr 'A-Z' 'a-z')" in
    some|several|a\ few|few|various|many|most|yes|partial|done|n/a|none|tbd)
      err "coverage[$item]: Images '$images' is an adjective, not page numbers" ;;
  esac
  # every page of every document, both modalities (ADR-0001 "Visual control",
  # amended 2026-08-27): a `complete` row must image the whole witness
  if [ "$state" = "complete" ]; then
    pages=$(trim "$pages")
    missing=$(echo "$images" | awk -v n="$pages" '
      { gsub(/[^0-9,\-]/, " "); split($0, parts, /[ ,]+/)
        for (i in parts) {
          if (parts[i] ~ /^[0-9]+-[0-9]+$/) { split(parts[i], r, "-"); for (k = r[1]; k <= r[2]; k++) seen[k] = 1 }
          else if (parts[i] ~ /^[0-9]+$/) seen[parts[i]] = 1
        } }
      END { miss = ""; c = 0
            for (k = 1; k <= n + 0; k++) if (!(k in seen)) { c++; if (c <= 6) miss = miss " " k }
            if (c > 0) print c "  (e.g." miss " )" }')
    [ -z "$missing" ] || err "coverage[$item]: state 'complete' but $missing pages are not imaged"
  fi
  COVSEEN="$COVSEEN $item"
done < <(grep '^| ' "$cov")

# every corpus item marked extracted: must have a coverage row
while IFS='|' read -r _ key _rest; do
  key=$(echo "$key" | xargs)
  case "$key" in ''|Key|---) continue ;; esac
  if grep -q "^| $key .*| extracted:" registry/corpus.md 2>/dev/null; then
    case " $COVSEEN " in
      *" $key "*) ;;
      *) err "coverage: '$key' is marked extracted in corpus.md but has no coverage row" ;;
    esac
  fi
done < <(grep '^| ' registry/corpus.md)

echo "coverage rows: $cov_rows"
for st in complete partial unrecorded; do
  n=$(awk -F'|' -v s="$st" 'NR>2 && $0 ~ /^\| / {gsub(/^ +| +$/,"",$7); if ($7==s) c++} END{print c+0}' "$cov")
  echo "  coverage $st: $n"
done

echo "gaps rows: $n_gaps"

if [ "$FAIL" -eq 0 ]; then echo "AUDIT PASS"; exit 0; else echo "AUDIT FAIL"; exit 1; fi
