#!/usr/bin/env bash
# Regenerate sources-text/ - plain-text extractions of the text-bearing
# in-corpus witnesses (grep/anchor aid; derived data).
# Charter: ADR-0001-codification-charter.md.
#
# Not included: Dimacs1996.pdf (image-only; gaps G10), the LNCS 11972 volume
# (the chapter extract covers the Willard text; volume text would pollute
# greps with other authors), nachlass scans (their OCR lives under
# ../collected_dew_materials/ocr/). Texts from OCR-layer witnesses
# (willard1993-tr, willard1993) are search aids, not authoritative.

set -u
cd "$(dirname "$0")"
ROOT="$(cd ../.. && pwd)"
OUT=sources-text
mkdir -p "$OUT"
fail=0
gen() { # key, repo-relative pdf path
  if pdftotext "$ROOT/$2" "$OUT/$1.txt" 2>/dev/null; then
    printf 'ok   %-22s %6s words\n' "$1" "$(wc -w < "$OUT/$1.txt")"
  else
    echo "FAIL $1  ($2)"; fail=1
  fi
}

gen willard1993-tr      nachlass/papers/1993technicalreport/willard1993_self_verifying_axiom_systems_tr93_10_searchable.pdf
gen willard1993         nachlass/papers/willard1993_self_verifying_axiom_systems_kgc_lncs713.pdf
gen willard2000-tab     nachlass/papers/willard2000_tableaux_robinson_q_author_tab2000.pdf
gen willard2001         nachlass/papers/willard2001_self_verifying_axiom_systems_author_jsl1.pdf
gen willard2002a        nachlass/papers/willard2002_semantic_tableaux_robinson_q_author_jsl2.pdf
gen willard2002c        nachlass/papers/willard2002_new_exceptions_tableaux_author_tab2.pdf
gen willard2004         nachlass/papers/willard2004_addition_not_multiplication_fol75.pdf
gen willard2005         nachlass/papers/willard2005_addition_total_consistency_author_jsl5.pdf
gen willard2005-tab     nachlass/papers/willard2005_real_valued_tableaux_author_tab5.pdf
gen willard2006a        nachlass/papers/willard2006_generalization_second_incompleteness_author_apal6.pdf
gen willard2006b        nachlass/papers/willard2006_real_valued_arithmetic_author_jsl6.pdf
gen willard2006-wollic  nachlass/papers/willard2006_i_sigma0_herbrand_author_wollic.pdf
gen willard2006-talk    nachlass/papers/willard2006_nature_godel_second_incompleteness_talk.pdf
gen willard2007-apal    nachlass/papers/willard2007_passive_induction_author_apal7.pdf
gen willard2007-kgs6    nachlass/papers/willard2007_fourteen_year_effort_author_kgs6.pdf
gen willard2009         nachlass/papers/willard2009_i_sigma0_herbrand_author_inf9.pdf
gen willard2011         nachlass/papers/willard2011_self_justifying_logics_arxiv_1108.6330.pdf
gen willard2013         nachlass/papers/willard2013_significance_self_justifying_axiom_systems_arxiv_1307.0150.pdf
gen willard2014         nachlass/papers/willard2014.pdf
gen willard2014-archive nachlass/papers/willard2014_broader_epistemological_author_archive.pdf
gen willard2016         nachlass/papers/willard2016_theta_function_symbol_arxiv_1612.08071.pdf
gen willard2018         nachlass/papers/willard2018.pdf
gen willard2020         nachlass/papers/willard2020.pdf
gen willard2020-lfcs    nachlass/papers/willard2020_tender_line_lfcs2020_lncs11972_chapter.pdf
gen dew-resume          lit/dewresume.pdf

exit $fail
