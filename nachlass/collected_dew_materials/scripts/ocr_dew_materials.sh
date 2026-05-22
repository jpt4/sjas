#!/usr/bin/env bash
# OCR batch driver for collected_dew_materials scanned PDFs.
# Profiles:
#   default (fast): 200dpi, 850px width cap, ocrad fallback
#   --hifi:         600dpi, 2550px width cap, grayscale resize, multi-PSM
#                   tesseract with TSV confidence; ocrad only as last resort
set -euo pipefail

ROOT="$(cd "$(dirname "$0")/.." && pwd)"
PDF_DIR="$ROOT"
OCR_ROOT="$ROOT/ocr"
PROFILE="fast"
FORCE=0
USER_DPI=0
USER_MAX_WIDTH=0

# Defaults (fast profile)
CONF_THRESHOLD="${CONF_THRESHOLD:-60}"
DPI="${DPI:-200}"
PSM="${PSM:-6}"
PSM_FALLBACKS="${PSM_FALLBACKS:-3}"
TESS_LANG="${TESS_LANG:-eng}"
OEM="${OEM:-1}"
MAX_WIDTH="${MAX_WIDTH:-850}"
TESS_TIMEOUT="${TESS_TIMEOUT:-240}"
GENERATE_TSV="${GENERATE_TSV:-0}"
PREPROCESS="scale"
ALLOW_OCRAD_FALLBACK="${ALLOW_OCRAD_FALLBACK:-1}"
OCR_SUBDIR=""

usage() {
  cat <<EOF
Usage: $(basename "$0") [options] [pdf-file ...]

Options:
  --hifi          High-fidelity profile (600dpi, 2550px, multi-PSM tesseract)
  --force         Re-OCR even if output already exists
  --all           OCR every non-duplicate PDF in the collection root
  --pilot         OCR Phase A pilot set only
  --phase-b       OCR Phase B logic/teaching set
  --phase-c       OCR Phase C admin/unclassified set
  --dpi N         Render resolution (overrides profile default)
  --conf N        TSV confidence threshold for fallback (default: $CONF_THRESHOLD)
  -h, --help      Show this help

Hi-fi output is written under ocr/hifi/; fast output under ocr/text/ (etc.).
EOF
}

apply_profile() {
  case "$PROFILE" in
    hifi)
      if [[ "$USER_DPI" -eq 0 ]]; then DPI=600; fi
      if [[ "$USER_MAX_WIDTH" -eq 0 ]]; then MAX_WIDTH=2550; fi
      PSM=4
      PSM_FALLBACKS="3 1 6"
      # Full-width 2550px pages can exceed 40 minutes per tesseract pass.
      TESS_TIMEOUT=7200
      GENERATE_TSV=1
      PREPROCESS="scale"
      ALLOW_OCRAD_FALLBACK=1
      OCR_SUBDIR="hifi"
      ;;
    fast)
      OCR_SUBDIR=""
      ;;
  esac
}

set_paths() {
  if [[ -n "$OCR_SUBDIR" ]]; then
    OCR_DIR="$OCR_ROOT/$OCR_SUBDIR"
  else
    OCR_DIR="$OCR_ROOT"
  fi
  TEXT_DIR="$OCR_DIR/text"
  PAGE_DIR="$OCR_DIR/pages"
  TSV_DIR="$OCR_DIR/tsv"
  IMG_DIR="$OCR_DIR/images"
  SCALED_DIR="$OCR_DIR/scaled"
}

mean_confidence() {
  local tsv="$1"
  awk -F'\t' '
    NR > 1 && $11 != "" && $11 != "-1" {
      sum += $11; n++
    }
    END {
      if (n > 0) printf "%.1f", sum / n; else print "0"
    }
  ' "$tsv"
}

preprocess_png() {
  local src="$1"
  local dst="$2"
  python3 - "$src" "$dst" "$MAX_WIDTH" "$PREPROCESS" <<'PY'
import sys
from PIL import Image, ImageOps, ImageFilter

src, dst, max_w_raw, mode = sys.argv[1], sys.argv[2], sys.argv[3], sys.argv[4]
max_w = int(max_w_raw) if max_w_raw not in ("0", "") else 0
img = Image.open(src).convert("L")
if mode == "enhance":
    img = ImageOps.autocontrast(img)
    img = img.filter(ImageFilter.UnsharpMask(radius=2, percent=150, threshold=3))
w, h = img.size
if max_w > 0 and w > max_w:
    scale = max_w / w
    img = img.resize((int(w * scale), int(h * scale)), Image.LANCZOS)
img.save(dst)
PY
}

run_tesseract_psm() {
  local psm="$1"
  local scaled="$2"
  local txt_base="$3"
  local tsv_base="$4"
  local with_tsv="$5"

  timeout "$TESS_TIMEOUT" tesseract "$scaled" "$txt_base" \
    -l "$TESS_LANG" --psm "$psm" --oem "$OEM" \
    -c preserve_interword_spaces=1 2>/dev/null || true

  if [[ "$with_tsv" == "1" ]]; then
    timeout "$TESS_TIMEOUT" tesseract "$scaled" "$tsv_base" \
      -l "$TESS_LANG" --psm "$psm" --oem "$OEM" \
      -c preserve_interword_spaces=1 tsv 2>/dev/null || true
  fi
}

ocr_page() {
  local png="$1"
  local page_txt="$2"
  local page_tsv="$3"
  local scaled="$4"
  local work_dir="${page_txt%.txt}.work"
  local best_txt="$work_dir/best.txt"
  local best_tsv="$work_dir/best.tsv"
  local best_chars=0
  local best_conf=0
  local best_psm="$PSM"

  mkdir -p "$work_dir"
  preprocess_png "$png" "$scaled"

  local psm psms
  read -r -a psms <<< "$PSM $PSM_FALLBACKS"
  for psm in "${psms[@]}"; do
    local try_txt="$work_dir/psm-${psm}.tesseract"
    local try_tsv="$work_dir/psm-${psm}.tsv"
    rm -f "${try_txt}.txt" "${try_tsv}.tsv"
    run_tesseract_psm "$psm" "$scaled" "$try_txt" "$try_tsv" "$GENERATE_TSV"
    local chars=0 conf=0
    if [[ -f "${try_txt}.txt" ]]; then
      chars="$(wc -c < "${try_txt}.txt" | tr -d ' ')"
    fi
    if [[ "$GENERATE_TSV" == "1" && -f "${try_tsv}.tsv" ]]; then
      conf="$(mean_confidence "${try_tsv}.tsv")"
    else
      conf=0
    fi
    local score="$chars"
    if awk "BEGIN { exit !($conf > 0) }"; then
      score="$(awk "BEGIN { print int($chars + $conf * 10) }")"
    fi
    local best_score="$best_chars"
    if awk "BEGIN { exit !($best_conf > 0) }"; then
      best_score="$(awk "BEGIN { print int($best_chars + $best_conf * 10) }")"
    fi
    if [[ "$score" -gt "$best_score" ]]; then
      best_chars="$chars"
      best_conf="$conf"
      best_psm="$psm"
      if [[ -f "${try_txt}.txt" ]]; then
        cp -f "${try_txt}.txt" "$best_txt"
      else
        : > "$best_txt"
      fi
      if [[ -f "${try_tsv}.tsv" ]]; then
        cp -f "${try_tsv}.tsv" "$best_tsv"
      else
        : > "$best_tsv"
      fi
    fi
    # Primary PSM with substantial text: skip slower fallback passes.
    if [[ "$psm" == "$PSM" && "$chars" -ge 300 ]]; then
      if [[ "$GENERATE_TSV" != "1" ]] || awk "BEGIN { exit !($conf >= 35) }"; then
        break
      fi
    fi
  done

  if [[ -s "$best_txt" ]]; then
    cp -f "$best_txt" "$page_txt"
  else
    : > "$page_txt"
  fi
  if [[ -s "$best_tsv" ]]; then
    cp -f "$best_tsv" "$page_tsv"
  else
    : > "$page_tsv"
  fi

  local chars="$best_chars"
  local conf="$best_conf"

  if [[ "$ALLOW_OCRAD_FALLBACK" == "1" && "$chars" -lt 80 ]]; then
    if command -v ocrad >/dev/null 2>&1; then
      ocrad "$scaled" > "${page_txt}.ocrad" 2>/dev/null || true
      if [[ -s "${page_txt}.ocrad" ]]; then
        {
          echo "=== tesseract (psm=${best_psm}, conf=${conf}, chars=${chars}) ==="
          cat "$page_txt" 2>/dev/null || true
          echo
          echo "=== ocrad fallback (last resort) ==="
          cat "${page_txt}.ocrad"
        } > "${page_txt}.combined"
        mv -f "${page_txt}.combined" "$page_txt"
        chars="$(wc -c < "$page_txt" | tr -d ' ')"
      fi
    elif command -v gocr >/dev/null 2>&1; then
      gocr "$scaled" > "${page_txt}.gocr" 2>/dev/null || true
      if [[ -s "${page_txt}.gocr" ]]; then
        {
          echo "=== tesseract (psm=${best_psm}, conf=${conf}, chars=${chars}) ==="
          cat "$page_txt" 2>/dev/null || true
          echo
          echo "=== gocr fallback ==="
          cat "${page_txt}.gocr"
        } > "${page_txt}.combined"
        mv -f "${page_txt}.combined" "$page_txt"
        chars="$(wc -c < "$page_txt" | tr -d ' ')"
      fi
    fi
  fi

  rm -rf "$work_dir"
}

ocr_pdf() {
  local pdf="$1"
  local base id pages i png page_txt page_tsv merged qa conf_sum conf_n avg profile_label

  base="$(basename "$pdf" .pdf)"
  id="$(echo "$base" | tr ' ' '_' | tr '[:upper:]' '[:lower:]')"
  pages="$(pdfinfo "$pdf" 2>/dev/null | awk -F': ' '/^Pages:/{print $2; exit}' | tr -d ' ')"

  merged="$TEXT_DIR/${id}.txt"
  qa="$TEXT_DIR/${id}.qa.tsv"
  if [[ "$FORCE" -eq 0 && -s "$merged" && -s "$qa" ]]; then
    echo "skip (already OCR'd): $pdf"
    return 0
  fi

  mkdir -p "$IMG_DIR/$id" "$SCALED_DIR/$id" "$PAGE_DIR/$id" "$TSV_DIR/$id"
  profile_label="$PROFILE"
  echo "OCR [$profile_label]: $pdf ($pages pages @ ${DPI}dpi, max-width ${MAX_WIDTH})"

  if [[ "$FORCE" -eq 1 ]]; then
    rm -f "$merged" "$qa"
  fi

  pdftoppm -png -r "$DPI" "$pdf" "$IMG_DIR/$id/page" >/dev/null

  mapfile -t pngs < <(ls "$IMG_DIR/$id"/page-*.png 2>/dev/null | sort -V)
  if [[ ${#pngs[@]} -eq 0 ]]; then
    echo "error: no rendered pages for $pdf" >&2
    return 1
  fi

  : > "$merged"
  printf "page\tmean_confidence\tchars\tpsm\n" > "$qa"

  conf_sum=0
  conf_n=0

  for ((i = 1; i <= ${#pngs[@]}; i++)); do
    png="${pngs[$((i - 1))]}"
    page_txt="$PAGE_DIR/$id/page-$(printf '%03d' "$i").txt"
    page_tsv="$TSV_DIR/$id/page-$(printf '%03d' "$i").tsv"
    scaled="$SCALED_DIR/$id/page-$(printf '%03d' "$i").png"
    ocr_page "$png" "$page_txt" "$page_tsv" "$scaled"

    conf="$(mean_confidence "$page_tsv")"
    chars="$(wc -c < "$page_txt" | tr -d ' ')"
    printf "%d\t%s\t%s\t\n" "$i" "$conf" "$chars" >> "$qa"

    if awk "BEGIN { exit !($conf > 0) }"; then
      conf_sum="$(awk "BEGIN { print $conf_sum + $conf }")"
      conf_n=$((conf_n + 1))
    elif [[ "$PROFILE" == "fast" && "$chars" -ge 500 ]]; then
      conf_sum="$(awk "BEGIN { print $conf_sum + 80 }")"
      conf_n=$((conf_n + 1))
    elif [[ "$PROFILE" == "fast" && "$chars" -ge 150 ]]; then
      conf_sum="$(awk "BEGIN { print $conf_sum + 55 }")"
      conf_n=$((conf_n + 1))
    elif [[ "$PROFILE" == "fast" && "$chars" -gt 0 ]]; then
      conf_sum="$(awk "BEGIN { print $conf_sum + 25 }")"
      conf_n=$((conf_n + 1))
    fi

    {
      echo "=== Page $i ==="
      cat "$page_txt"
      echo
    } >> "$merged"
  done

  if [[ "$conf_n" -gt 0 ]]; then
    avg="$(awk "BEGIN { printf \"%.1f\", $conf_sum / $conf_n }")"
  else
    avg="0"
  fi
  echo "done: $merged (avg conf=$avg)"
}

pilot_files=(
  "2020Notes.pdf"
  "20080314_ZCF_notes.pdf"
  "20080318_ZCF_notes.pdf"
  "20080321_ZCF_notes_notepage7doubled.pdf"
  "ZFnote.pdf"
  "Correspondence_Hajek.pdf"
)

phase_b_files=(
  "Exam_2014.pdf"
  "2024-10-08 20.13.17_05102014_notarized_notes_scan_higher_res.pdf"
  "Notes_Phd1.pdf"
  "05102014_notarized_notes_scan_low_res.pdf"
)

phase_c_files=(
  "Notes_Thetheoryofcomputationaldynamics.pdf"
  "NSF_1991_Report.pdf"
  "2025-12-22 09.55.35.pdf"
  "2025-12-22 09.58.14.pdf"
  "2025-12-24 14.07.32.pdf"
  "2025-12-24 14.44.42.pdf"
  "DOC102924-10292024193202.pdf"
)

all_files=(
  "${pilot_files[@]}"
  "${phase_b_files[@]}"
  "${phase_c_files[@]}"
)

mode=""
files=()
while [[ $# -gt 0 ]]; do
  case "$1" in
    --hifi) PROFILE=hifi; shift ;;
    --force) FORCE=1; shift ;;
    --all) mode=all; shift ;;
    --pilot) mode=pilot; shift ;;
    --phase-b) mode=phase-b; shift ;;
    --phase-c) mode=phase-c; shift ;;
    --dpi) DPI="$2"; USER_DPI=1; shift 2 ;;
    --max-width) MAX_WIDTH="$2"; USER_MAX_WIDTH=1; shift 2 ;;
    --conf) CONF_THRESHOLD="$2"; shift 2 ;;
    -h|--help) usage; exit 0 ;;
    *.pdf) files+=("$1"); shift ;;
    *) echo "unknown arg: $1" >&2; usage; exit 1 ;;
  esac
done

apply_profile
set_paths
mkdir -p "$TEXT_DIR" "$PAGE_DIR" "$TSV_DIR" "$IMG_DIR" "$SCALED_DIR"

if [[ ${#files[@]} -eq 0 ]]; then
  case "$mode" in
    all) files=("${all_files[@]}") ;;
    pilot) files=("${pilot_files[@]}") ;;
    phase-b) files=("${phase_b_files[@]}") ;;
    phase-c) files=("${phase_c_files[@]}") ;;
    *) usage; exit 1 ;;
  esac
fi

sort_pdfs_by_pages() {
  local sorted=()
  while IFS= read -r line; do
    sorted+=("$line")
  done < <(
    for pdf in "$@"; do
      pages="$(pdfinfo "$PDF_DIR/$pdf" 2>/dev/null | awk -F': ' '/^Pages:/{print $2; exit}' | tr -d ' ')"
      printf "%05d\t%s\n" "${pages:-99999}" "$pdf"
    done | sort -n | cut -f2-
  )
  files=("${sorted[@]}")
}

if [[ "$mode" == "all" ]]; then
  sort_pdfs_by_pages "${files[@]}"
fi

for pdf in "${files[@]}"; do
  path="$PDF_DIR/$pdf"
  if [[ ! -f "$path" ]]; then
    echo "missing: $path" >&2
    exit 1
  fi
  ocr_pdf "$path"
done
