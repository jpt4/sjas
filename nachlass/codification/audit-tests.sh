#!/usr/bin/env bash
# Red-green tests for C16 mechanical checks M1–M8.
# Each check is shown to FAIL on a deliberately injected defect, then the
# clean tree is confirmed green for that check's fatal path.
#
# Usage: from codification/, ./audit-tests.sh
# Exit 0 iff every red case fails as expected and the green baseline for
# fatal M-checks (M1–M5) is clean. M6–M8 are advisory (report-only).

set -u
cd "$(dirname "$0")"
ROOT="$(cd ../.. && pwd)"
PASS=0
FAILN=0
ok() { echo "OK  $*"; PASS=$((PASS+1)); }
bad() { echo "BAD $*"; FAILN=$((FAILN+1)); }

TMP=$(mktemp -d)
trap 'rm -rf "$TMP"' EXIT

# Snapshot paths we may mutate; always restore.
backup() { cp -a "$1" "$TMP/$(echo "$1" | tr '/' '_')"; }
restore() { cp -a "$TMP/$(echo "$1" | tr '/' '_')" "$1"; }

# ---------- helpers to run a single M-check ----------
run_one() {
  local fn="$1"
  FAIL=0
  # shellcheck source=audit-m.sh
  . ./audit-m.sh
  "$fn" >/tmp/audit-tests-out.txt 2>&1
  echo "$FAIL"
}

expect_fail() {
  local name="$1" fn="$2" needle="$3"
  local rc
  rc=$(run_one "$fn")
  if [ "$rc" = "1" ] && grep -q "$needle" /tmp/audit-tests-out.txt; then
    ok "$name red (caught: $needle)"
  else
    bad "$name red — expected FAIL with /$needle/; got FAIL=$rc"
    sed -n '1,20p' /tmp/audit-tests-out.txt
  fi
}

expect_pass() {
  local name="$1" fn="$2"
  local rc
  rc=$(run_one "$fn")
  if [ "$rc" = "0" ]; then
    ok "$name green"
  else
    bad "$name green — expected PASS; got FAIL=$rc"
    grep 'AUDIT-FAIL' /tmp/audit-tests-out.txt | head -20
  fi
}

echo "=== C16 audit-tests (M1–M8 red-green) ==="

# ----- M1 red: remove an exclusion so a swept label goes missing -----
backup extraction/willard2000-tab.md
# Drop Lemma 7A from exclusions
python3 - <<'PY'
from pathlib import Path
p = Path('extraction/willard2000-tab.md')
t = p.read_text()
t2 = t.replace('| Lemma 7A | Internal case label within Lemma 7 |\n', '')
p.write_text(t2)
PY
expect_fail "M1" run_m1 "lemma 7a"
restore extraction/willard2000-tab.md

# ----- M2 red: duplicate Paper+Label -----
backup registry/results.md
echo '| Willard2005#Rem1DUP | Willard2005 | Remark 1 | remark | 20 | proof-apparatus | stated-only | — | deliberate M2 fixture |' >> registry/results.md
expect_fail "M2" run_m2 "Paper+Label duplicate"
restore registry/results.md

# ----- M3 red: unresolved **O999** -----
backup codified-sjas.md
echo 'Deliberate unresolved ref **O999**.' >> codified-sjas.md
expect_fail "M3" run_m3 "O999"
restore codified-sjas.md

# ----- M4 red: fake §99.9 -----
backup codified-sjas.md
echo 'See §99.9 for nothing.' >> codified-sjas.md
expect_fail "M4" run_m4 "§99.9"
restore codified-sjas.md

# ----- M5 red: wrong results count in README -----
backup README.md
python3 - <<'PY'
from pathlib import Path
p = Path('README.md')
t = p.read_text()
import re
t2 = re.sub(r'[0-9]+ results,', '999 results,', t, count=1)
p.write_text(t2)
PY
expect_fail "M5" run_m5 "999 results"
restore README.md

# ----- M6: report-only — just ensure it runs -----
FAIL=0; . ./audit-m.sh; run_m6 >/tmp/audit-tests-out.txt 2>&1
if grep -q 'M6:' /tmp/audit-tests-out.txt; then
  ok "M6 report emitted"
else
  bad "M6 produced no report"
fi

# ----- M7 / M8: advisory — ensure they run -----
FAIL=0; . ./audit-m.sh; run_m7 >/tmp/audit-tests-out.txt 2>&1
ok "M7 ran (advisory)"
FAIL=0; . ./audit-m.sh; AMENDED_AFTER=2026-08-28 run_m8 >/tmp/audit-tests-out.txt 2>&1
if grep -q 'M8:' /tmp/audit-tests-out.txt; then
  ok "M8 report emitted"
else
  bad "M8 produced no report"
fi

# ----- Green baseline for fatal checks -----
expect_pass "M1" run_m1
expect_pass "M2" run_m2
expect_pass "M3" run_m3
expect_pass "M4" run_m4
expect_pass "M5" run_m5

echo "=== $PASS passed, $FAILN failed ==="
[ "$FAILN" -eq 0 ]
