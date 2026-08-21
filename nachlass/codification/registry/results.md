# Results Registry (numbered-item inventory)

One row per numbered item (definition / theorem / lemma / proposition /
remark / conjecture) extracted from a corpus item. This registry is the
machine-checked index; the human-readable statements live in the per-paper
records under `../extraction/`. Filled from C4 onward.

Columns — **Id**: `<Paper>#<Label>` (e.g. `Willard2005#Thm1`). **Paper**:
corpus key. **Label**: the paper's own numbering. **Type**: def | thm |
lemma | prop | remark | conj. **Page**: page anchor in the witness. **Topic**:
canonical topic slug (aligned at C13). **Proof**: full | sketch | cited |
stated-only | n/a(defs). **Depends**: comma-separated Ids or external
citations. **Notes**: variant deltas, drift pointers.

| Id | Paper | Label | Type | Page | Topic | Proof | Depends | Notes |
| --- | --- | --- | --- | --- | --- | --- | --- | --- |
