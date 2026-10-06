#!/usr/bin/env bash
# Regenerate the generic chain of the fundamental lemma (Theorem 4.6 design
# note §3): the clause fragments (gen_den.py / gen_sem.py --ri), then each
# ri_X.clj from X.clj (gen_ri.py, with the rename list and the constants each
# standard namespace declares, both from the warm server's analysis), then the
# hand edits at the R-specific steps (ri_edits.py).
set -e
cd "$(dirname "$0")/.."
python3 tools/gen_den.py --ri lcert/formal
python3 tools/gen_sem.py --ri lcert/formal
for x in ${RI_NAMESPACES:-den sem model unfold mono splitting subst substitution vweaken fundamental outer recsyn lemma36 conversion convcase}; do
  python3 tools/gen_ri.py tools/ri_renames.txt tools/ri_declared/$x.txt lcert/formal/$x.clj lcert/formal/ri_$x.clj
done
python3 tools/ri_edits.py lcert/formal
