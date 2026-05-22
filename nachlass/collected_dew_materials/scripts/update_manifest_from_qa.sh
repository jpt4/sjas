#!/usr/bin/env bash
# Refresh ocr_status and ocr_quality columns in manifest.tsv from *.qa.tsv files.
set -euo pipefail

ROOT="$(cd "$(dirname "$0")/.." && pwd)"
MANIFEST="$ROOT/manifest.tsv"
PROFILE="fast"

usage() {
  cat <<EOF
Usage: $(basename "$0") [--hifi]

  --hifi   Read QA from ocr/hifi/text/*.qa.tsv (high-fidelity pass)
  default  Read QA from ocr/text/*.qa.tsv (fast pass)
EOF
}

while [[ $# -gt 0 ]]; do
  case "$1" in
    --hifi) PROFILE="hifi"; shift ;;
    -h|--help) usage; exit 0 ;;
    *) echo "Unknown option: $1" >&2; usage; exit 1 ;;
  esac
done

if [[ "$PROFILE" == "hifi" ]]; then
  QA_DIR="$ROOT/ocr/hifi/text"
else
  QA_DIR="$ROOT/ocr/text"
fi

quality_from_avg() {
  local avg="$1"
  awk -v a="$avg" 'BEGIN {
    if (a >= 75) print "good";
    else if (a >= 50) print "fair";
    else if (a > 0) print "poor";
    else print "poor-no-conf";
  }'
}

python3 - "$MANIFEST" "$QA_DIR" <<'PY'
import csv
import sys
from pathlib import Path

manifest_path = Path(sys.argv[1])
qa_dir = Path(sys.argv[2])

rows = []
with manifest_path.open(newline="") as f:
    reader = csv.DictReader(f, delimiter="\t")
    fieldnames = reader.fieldnames
    for row in reader:
        rows.append(row)

for row in rows:
    if row.get("ocr_status") == "skipped":
        continue
    orig = row["original_file"]
    stem = orig.rsplit(".", 1)[0].replace(" ", "_").lower()
    qa = qa_dir / f"{stem}.qa.tsv"
    if not qa.exists():
        continue
    confs = []
    charss = []
    with qa.open() as f:
        next(f, None)
        for line in f:
            parts = line.rstrip("\n").split("\t")
            if len(parts) >= 2:
                try:
                    confs.append(float(parts[1]))
                except ValueError:
                    pass
            if len(parts) >= 3:
                try:
                    charss.append(int(parts[2]))
                except ValueError:
                    pass
    if confs and any(c > 0 for c in confs):
        avg = sum(confs) / len(confs)
    elif charss:
        avg_chars = sum(charss) / len(charss)
        if avg_chars >= 500:
            avg = 80
        elif avg_chars >= 150:
            avg = 55
        elif avg_chars > 0:
            avg = 25
        else:
            avg = 0
    else:
        row["ocr_status"] = "complete"
        row["ocr_quality"] = "poor-no-conf"
        continue
    row["ocr_status"] = "complete"
    if avg >= 75:
        row["ocr_quality"] = "good"
    elif avg >= 50:
        row["ocr_quality"] = "fair"
    elif avg > 0:
        row["ocr_quality"] = "poor"
    else:
        row["ocr_quality"] = "poor-no-conf"

with manifest_path.open("w", newline="") as f:
    writer = csv.DictWriter(f, fieldnames=fieldnames, delimiter="\t", lineterminator="\n")
    writer.writeheader()
    writer.writerows(rows)
PY

echo "Updated $MANIFEST from QA files in $QA_DIR."
