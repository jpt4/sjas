#!/usr/bin/env bash
# Re-OCR all non-duplicate collected materials with the high-fidelity profile.
# Uses an exclusive lock so only one hi-fi batch runs at a time (tesseract is
# very slow at 600dpi / 2550px and concurrent runs starve each other).
set -euo pipefail
ROOT="$(cd "$(dirname "$0")/.." && pwd)"
LOCK="$ROOT/ocr/hifi/.ocr-hifi.lock"
mkdir -p "$(dirname "$LOCK")"
if pgrep -x tesseract >/dev/null 2>&1; then
  echo "Refusing to start: another tesseract process is already running." >&2
  echo "Stop competing OCR jobs first (e.g. pkill -x tesseract)." >&2
  exit 1
fi
exec 9>"$LOCK"
if ! flock -n 9; then
  echo "Another hi-fi OCR batch is already running (lock: $LOCK)" >&2
  exit 1
fi
exec "$ROOT/scripts/ocr_dew_materials.sh" --hifi --force --all "$@"
