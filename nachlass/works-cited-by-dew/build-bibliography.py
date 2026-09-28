#!/usr/bin/env python3
"""Build works-cited-by-dew omnibus bibliography from Willard SJAS text layers.

Reads codification/sources-text/*.txt reference sections, excludes Willard
self-citations, clusters equivalent external works, and writes:
  bibliography.json / bibliography.tsv / BIBLIOGRAPHY.md / README.md
"""
from __future__ import annotations

import json
import re
from collections import defaultdict
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
SRC = ROOT / "codification" / "sources-text"
OUT = ROOT / "works-cited-by-dew"

STEM_TO_CORPUS = {
    "willard1993": "Willard1993",
    "willard1993-tr": "Willard1993-TR",
    "willard1998": "Willard1998",
    "willard2000-tab": "Willard2000-TAB",
    "willard2001": "Willard2001",
    "willard2002a": "Willard2002a",
    "willard2002c": "Willard2002c",
    "willard2004": "Willard2004",
    "willard2005": "Willard2005",
    "willard2005-tab": "Willard2005-TAB",
    "willard2006a": "Willard2006a",
    "willard2006b": "Willard2006b",
    "willard2006-wollic": "Willard2006-WoLLIC",
    "willard2006-talk": "Willard2006-Talk",
    "willard2007-apal": "Willard2007-APAL",
    "willard2007-kgs6": "Willard2007-KGS6",
    "willard2009": "Willard2009",
    "willard2011": "Willard2011",
    "willard2013": "Willard2013",
    "willard2014": "Willard2014",
    "willard2014-archive": "Willard2014",
    "willard2016": "Willard2016",
    "willard2018": "Willard2018",
    "willard2020": "Willard2020",
    "willard2020-lfcs": "Willard2020-LFCS",
}

STOP = {
    "and", "the", "a", "an", "of", "on", "in", "for", "to", "its", "with",
    "from", "by", "into", "about", "some", "how", "new", "note", "journal",
    "pp", "vol", "eds", "proceedings", "conference", "springer", "press",
    "university", "logic", "theorem", "this", "that", "which", "first",
    "order", "order", "arithmetic", "math", "mathematical",
}

LETTER = r"A-Za-zÁÉÍÓÚÄÖÜČŠŽáéíóúäöüčšžÀÂÃÅÇÈÊËÌÎÏÑÒÔÕÙÛÝàâãåçèêëìîïñòôõùûýß"
WORD = rf"[{LETTER}][{LETTER}\-]*"

VENUE = re.compile(
    r"\b(Journal|Annals|Archive|Springer|Technical|Ph\.?D|Jour\.|JSL|APAL|"
    r"Fundamenta|Notre Dame|Bulletin|Handbook|Proceedings|LNCS|LNAI|"
    r"Information and Computation|SIAM|Siam|Cambridge|Princeton|Oxford|"
    r"North Holland|Bibliopolic|arXiv|Cornell|Monatsh|Dissertation)\b",
    re.I,
)


def deocr(s: str) -> str:
    # Join spaced capitals common in OCR: "H AJEK", "W ILLARD", "G ODEL"
    s = re.sub(r"\b([A-Za-z]) ([A-ZÁÉÍÓÚÄÖÜČŠŽ]{2,})\b", r"\1\2", s)
    s = re.sub(r"\b([A-ZÁÉÍÓÚÄÖÜČŠŽ]) ([A-ZÁÉÍÓÚÄÖÜČŠŽ]{2,})\b", r"\1\2", s)
    return s.replace("\u00ad", "")


def clean_body(s: str) -> str:
    s = s.replace("\f", "\n")
    s = re.sub(r"\n+", " ", s)
    s = re.sub(r"\s+", " ", s).strip()
    s = re.sub(r"\s+\d{1,3}\s*$", "", s).strip()
    return s


def find_refs_region(text: str) -> tuple[str, str]:
    """Return (region, how_found). Prefer a late heading."""
    starts = []
    for pat in [
        r"(?im)^(?:\s*)(References|Bibliography|REFERENCES|REFERENCE LIST)\b.*$",
    ]:
        for m in re.finditer(pat, text):
            # Prefer starts in the last 40% of the file
            if m.start() >= len(text) * 0.45:
                starts.append(m.start())
            else:
                starts.append(m.start())  # keep early ones as fallback
    if starts:
        # Prefer the last heading in the file
        pos = max(starts)
        return text[pos:], "heading"
    return text[int(len(text) * 0.8) :], "tail-fallback"


def looks_bibliographic(body: str) -> bool:
    if len(body) < 35:
        return False
    if not re.search(r"[A-Za-z]{3,}", body):
        return False
    # Reject pure page-range debris / proof fragments / mid-entry tails
    if re.match(r"^[\d\s.,;:()\[\]{}\-–—]+$", body):
        return False
    if re.match(r"^(pp\.|,?\s*pp\.|,?\s*\d)", body, re.I):
        return False
    if re.search(r"\b(Closed Subtree|INVALID\.|Branch\)|FIGURE)\b", body):
        return False
    # Springer LNCS mid-entry tails that lost their author line
    if re.match(r"^,?\s*Springer-Verlag", body, re.I):
        return False
    has_year = bool(re.search(r"\b((?:19|20)\d{2})\b", body))
    has_venue = bool(VENUE.search(body))
    has_quote = bool(re.search(r"[“\"„].+[”\"]", body))
    # Author-ish start (letter or opening quote).
    authorish = bool(
        re.match(
            r"^[A-ZÁÉÍÓÚÄÖÜČŠŽ“\"].{0,50}?(,|:|\band\b|\bet\b)",
            deocr(body),
        )
    )
    # LNCS-style "Surname, I.: Title" needs the colon early in the author block
    has_colon_title = bool(re.match(r"^[A-Z][^:]{2,70}:\s+[A-Z“\"]", deocr(body)))
    return (has_year or has_venue or has_quote) and (authorish or has_colon_title)


def parse_bracket_entries(refs: str) -> list[tuple[str, str]]:
    pattern = re.compile(r"(?m)^(?:\s*)[\[\(\{]([0-9A-Za-z]{1,8})[\]\)\}]\s*")
    parts = pattern.split(refs)
    out = []
    if len(parts) < 3:
        return out
    i = 1
    while i + 1 < len(parts):
        lid, body = parts[i], clean_body(parts[i + 1])
        # Truncate body if it accidentally includes a later "References" junk
        if looks_bibliographic(body):
            out.append((lid, body))
        i += 2
    return out


def parse_plain_numbered(refs: str) -> list[tuple[str, str]]:
    """LNCS / Springer '1. Author, A.: Title' and '11. E. Nelson,' styles."""
    # Line-start number, then capital or curly-quote
    pattern = re.compile(r"(?m)^(?:\s*)(\d{1,2})\.\s+(?=[A-Z“\"„])")
    starts = [(m.start(1), m.group(1), m.end()) for m in pattern.finditer(refs)]
    if not starts:
        return []
    # Require the sequence to look like a bibliography: first id <= 3, mostly contiguous
    ids = [int(s[1]) for s in starts]
    if min(ids) > 3:
        return []
    out = []
    for idx, (_, lid, body_start) in enumerate(starts):
        end = starts[idx + 1][0] if idx + 1 < len(starts) else len(refs)
        body = clean_body(refs[body_start:end])
        if looks_bibliographic(body):
            out.append((lid, body))
    return out


def parse_entries(refs: str) -> list[tuple[str, str]]:
    # Detect dominant style from the start of the refs block (skip heading line)
    head = refs[:800]
    plain = parse_plain_numbered(refs)
    bracket = parse_bracket_entries(refs)

    if re.search(r"(?m)^(?:\s*)1\.\s+[A-Z“\"„]", head) and len(plain) >= 5:
        chosen = plain
    elif re.search(r"(?m)^(?:\s*)\[1\]\s", head) and len(bracket) >= 5:
        chosen = bracket
    elif re.search(r"(?m)^(?:\s*)\[[A-Za-z]", head) and len(bracket) >= 3:
        chosen = bracket
    elif len(plain) >= len(bracket):
        chosen = plain
    else:
        chosen = bracket

    best: dict[str, str] = {}
    for lid, body in chosen:
        if lid not in best or len(body) > len(best[lid]):
            best[lid] = body

    def sk(k: str):
        return (0, int(k)) if k.isdigit() else (1, k.lower())

    return [(k, best[k]) for k in sorted(best, key=sk)]


def is_self(entry: str) -> bool:
    e = deocr(entry)
    e_l = e.lower()
    head = e_l[:140]
    # Em-dash author continuation in Willard's own bibliographies is almost
    # always another Willard paper (or the previous Willard entry).
    if re.match(r"^[—–\-−]\s*,", e) or re.match(r"^—,", e):
        return True
    if re.search(r"\bd\.?\s*e?\.?\s*willard\b", head):
        return True
    if re.search(r"\bwillard,\s*d", head):
        return True
    # Spaced OCR / glyph corruption: W ILLARD, WiUard, Wlllard, WiUard
    if re.search(r"\bw\s*i\s*l\s*l\s*a\s*r\s*d\b", head):
        return True
    if re.search(r"\bwi[lu]l?ard\b", head) or re.search(r"\bwlllard\b", head):
        return True
    ym = re.search(r"\b((?:19|20)\d{2})\b", e_l)
    authorish = e_l[: ym.start()] if ym else head
    if "willard" in authorish and not re.search(
        r"\b(wilkie|williams|wilson|wilkinson)\b", authorish
    ):
        return True
    # Title cues that are unmistakably Willard's own SJAS line
    if re.search(
        r"\bself[- ]verifying axiom systems\b|\bself[- ]justifying axiom systems\b",
        e_l,
    ) and re.search(r"\b(wi[lu]l?ard|wlllard|willard)\b", head):
        return True
    return False


def year_of(entry: str) -> str:
    ms = re.findall(r"\b((?:19|20)\d{2})\b", entry)
    # Prefer a year that is not obviously a page folio after pp.
    for y in ms:
        # skip if immediately after 'pp.' within 10 chars before match
        pass
    return ms[0] if ms else ""


def fold(s: str) -> str:
    """Fold accents for clustering (hájek ~ hajek)."""
    repl = str.maketrans(
        {
            "á": "a",
            "à": "a",
            "ä": "a",
            "â": "a",
            "ã": "a",
            "å": "a",
            "é": "e",
            "è": "e",
            "ë": "e",
            "ê": "e",
            "í": "i",
            "ì": "i",
            "ï": "i",
            "î": "i",
            "ó": "o",
            "ò": "o",
            "ö": "o",
            "ô": "o",
            "õ": "o",
            "ú": "u",
            "ù": "u",
            "ü": "u",
            "û": "u",
            "ý": "y",
            "č": "c",
            "š": "s",
            "ž": "z",
            "ß": "ss",
            "ç": "c",
            "ñ": "n",
            "Á": "A",
            "À": "A",
            "Ä": "A",
            "Â": "A",
            "É": "E",
            "Í": "I",
            "Ó": "O",
            "Ö": "O",
            "Ú": "U",
            "Ü": "U",
            "Č": "C",
            "Š": "S",
            "Ž": "Z",
        }
    )
    return s.translate(repl)


def authors_of(entry: str) -> str:
    e = deocr(entry)
    # LNCS Author, A.B.:
    m = re.match(rf"^(.{{3,90}}?):\s+", e)
    if m and not m.group(1).lower().startswith("http"):
        return re.sub(r"\s+", " ", m.group(1)).strip()[:140]
    # Quoted title: authors are everything before the opening quote
    qm = re.search(r"[“\"„]", e)
    if qm and qm.start() > 3:
        head = e[: qm.start()].strip(" ,.")
        return re.sub(r"\s+", " ", head)[:140]
    # Initials + surname(s), allowing accented letters and &
    m = re.match(
        rf"^((?:[{LETTER}]\.\s*)*{WORD}(?:,\s*(?:[{LETTER}]\.\s*)+)?(?:\s+(?:and|&)\s+(?:[{LETTER}]\.\s*)*{WORD}(?:,\s*(?:[{LETTER}]\.\s*)+)?)*)",
        e,
    )
    if m:
        return re.sub(r"\s+", " ", m.group(1)).strip(" ,.")[:140]
    ym = re.search(r"\b((?:19|20)\d{2})\b", e)
    head = e[: ym.start()] if ym else e[:100]
    head = re.sub(r"\s+", " ", head).strip(" ,.")
    return head[:140]


def title_of(entry: str) -> str:
    e = deocr(entry)
    qm = re.search(r"[“\"„”](.+?)[”\"“]", e)
    if qm:
        t = re.sub(r"\s+", " ", qm.group(1)).strip()
        if len(t) >= 8:
            return t[:200]
    # Also handle ASCII quote that opens but never closes cleanly
    qm2 = re.search(r'["„]([^"]{8,160})(?:"|,|\s+Monats|\s+Journal|\s+Annals)', e)
    if qm2:
        t = re.sub(r"\s+", " ", qm2.group(1)).strip()
        if len(t) >= 8:
            return t[:200]
    # LNCS Author, A.: Title. Venue
    m = re.match(r"^[^:]{3,90}:\s+(.+)$", e)
    if m:
        rest = m.group(1)
        rest = re.split(
            r"\b(Jour\.|JSL|APAL|Inf\.|Annals|Journal|In:|pp\.|vol\.|Arch\.|"
            r"Bull\.|Russ\.|Cornell|arXiv|Springer|Studies|Mathematics Notes|"
            r"Oxford|Princeton|North Holland|Academic Press|MIT Press)\b",
            rest,
            maxsplit=1,
        )[0]
        t = re.sub(r"\s+", " ", rest).strip(" .")
        if len(t) >= 8 and not t.startswith(")"):
            return t[:200]
    # Author(s), Title, Venue — drop author prefix then cut at venue
    auth = authors_of(e)
    rest = e
    if auth and e.startswith(auth):
        rest = e[len(auth) :].lstrip(" ,.")
    elif auth:
        # OCR author may differ slightly; try strip through first comma-clause
        rest = re.sub(r"^[^,]{3,80},\s*", "", e, count=1)
    rest = re.split(
        r",\s*(?:Annals|Journal|Archive|Springer|Technical|Ph\.?D|"
        r"this JOURNAL|Jour\.|JSL|APAL|In:|Arch\.|Bull\.|Oxford|"
        r"Princeton|North Holland|Academic|MIT Press|Proceedings|"
        r"Studies|Mathematics Notes|Cambridge|Handbook|pp\.|Comm\.|"
        r"Transactions|Bulletin|Fundamenta|Monatsh)",
        rest,
        maxsplit=1,
        flags=re.I,
    )[0]
    rest = re.split(r",\s*vol\.", rest, maxsplit=1, flags=re.I)[0]
    rest = re.split(r"\.\s*Springer", rest, maxsplit=1, flags=re.I)[0]
    t = re.sub(r"\s+", " ", rest).strip(" ,.")
    if (
        len(t) >= 8
        and not re.match(r"^[\d\s.,)(\-–—]+$", t)
        and not t.startswith(")")
        and "did not think" not in t.lower()
    ):
        return t[:200]
    return ""


def cluster_key(entry: str):
    authors = fold(authors_of(entry).lower())
    ap = re.sub(rf"\b[{LETTER.lower()}]\.?\b", " ", authors, flags=re.I)
    ap = re.sub(r"[^\w\s\-]", " ", ap)
    toks = [t for t in ap.split() if len(t) > 2 and t not in STOP]
    surnames = tuple(toks[:3])
    year = year_of(entry)
    title = fold(title_of(entry).lower())
    tw = [
        w
        for w in re.sub(r"[^\w\s]", " ", title).split()
        if w not in STOP and len(w) > 2
    ][:8]
    return surnames, year, " ".join(tw)


def keys_match(a, b) -> bool:
    sa, ya, ta = a
    sb, yb, tb = b
    if ya and yb and ya != yb:
        return False
    if sa and sb and not (set(sa) & set(sb)):
        return False
    wa, wb = set(ta.split()), set(tb.split())
    if ta and tb:
        if len(wa & wb) >= 2 or ta in tb or tb in ta:
            return True
        # Same primary surname + year + one shared significant title word
        if ya and ya == yb and sa and sb and sa[0] == sb[0] and (wa & wb):
            return True
        # Classic monograph: same two surnames + year even with thin title overlap
        if (
            ya
            and ya == yb
            and sa
            and sb
            and len(set(sa) & set(sb)) >= 2
            and (wa & wb)
        ):
            return True
        return False
    # One or both titleless: merge on shared surnames (order-insensitive) + year
    if ya and ya == yb and sa and sb:
        shared = set(sa) & set(sb)
        if set(sa) == set(sb) or (sa[0] == sb[0] and len(shared) >= 1):
            return True
        if len(shared) >= 2:
            return True
        # Title on one side only: allow if surnames match and title has a cue
        titled = ta or tb
        if titled and shared and any(
            w in titled for w in ("metamathematics", "predicative", "bounded")
        ):
            return True
    return False


# Known external works mentioned in Willard1998 extraction notes (chapter OCR
# only lists selected refs). Self-citations omitted.
MANUAL: dict[str, list[tuple[str, str]]] = {
    "Willard1998": [
        ("28", "R. Solovay, Private communications (1994)."),
    ],
}


def main() -> None:
    OUT.mkdir(exist_ok=True)
    raw: dict[str, dict[str, str]] = defaultdict(dict)
    log: dict = {"problems": [], "per_doc": {}}

    for stem, corpus in STEM_TO_CORPUS.items():
        path = SRC / f"{stem}.txt"
        if not path.exists():
            log["problems"].append(f"missing {path}")
            continue
        text = path.read_text(errors="replace")
        region, how = find_refs_region(text)
        ents = parse_entries(region)
        if corpus == "Willard2006b" and len(ents) < 3:
            log["problems"].append(
                "Willard2006b: no bibliography in the JSL text layer; "
                "use Willard2005-TAB for the conference-form reference list"
            )
        if len(ents) < 3 and how == "tail-fallback":
            log["problems"].append(f"{stem}: weak refs parse ({len(ents)} entries)")
        for lid, body in ents:
            if lid not in raw[corpus] or len(body) > len(raw[corpus][lid]):
                raw[corpus][lid] = body
        log["per_doc"][f"{stem}->{corpus}"] = {
            "n": len(ents),
            "how": how,
        }

    for corpus, ents in MANUAL.items():
        for lid, body in ents:
            raw[corpus].setdefault(lid, body)

    clusters = []
    external_rows = self_rows = 0
    for corpus, idmap in raw.items():
        for lid, body in idmap.items():
            if is_self(body):
                self_rows += 1
                continue
            external_rows += 1
            key = cluster_key(body)
            found = None
            for c in clusters:
                if keys_match(key, c["key"]):
                    found = c
                    break
            if found is None:
                found = {"key": key, "best": body, "occs": []}
                clusters.append(found)
            found["occs"].append((corpus, lid, body))
            if len(body) > len(found["best"]):
                found["best"] = body
                found["key"] = key

    clusters.sort(
        key=lambda c: (
            c["key"][0][0] if c["key"][0] else "zzz",
            c["key"][1] or "9999",
            c["key"][2],
        )
    )

    payload = []
    for i, c in enumerate(clusters, 1):
        docs = sorted({d for d, _, _ in c["occs"]})
        best = deocr(c["best"])
        payload.append(
            {
                "id": f"WC{i:04d}",
                "authors": authors_of(best),
                "year": year_of(best),
                "title": title_of(best),
                "canonical_entry": best,
                "documents": docs,
                "n_documents": len(docs),
                "occurrences": [
                    {
                        "document": d,
                        "local_ref": lid,
                        "entry": deocr(body),
                    }
                    for d, lid, body in sorted(c["occs"], key=lambda x: (x[0], x[1]))
                ],
            }
        )

    (OUT / "bibliography.json").write_text(
        json.dumps(payload, indent=2, ensure_ascii=False) + "\n"
    )
    tsv = ["id\tyear\tauthors\ttitle\tn_documents\tdocuments"]
    for it in payload:
        tsv.append(
            "\t".join(
                [
                    it["id"],
                    it["year"],
                    it["authors"].replace("\t", " "),
                    it["title"].replace("\t", " "),
                    str(it["n_documents"]),
                    "; ".join(it["documents"]),
                ]
            )
        )
    (OUT / "bibliography.tsv").write_text("\n".join(tsv) + "\n")
    (OUT / "_extract-log.json").write_text(
        json.dumps(
            {
                **log,
                "external_rows": external_rows,
                "self_rows": self_rows,
                "clusters": len(payload),
                "documents_covered": sorted(raw.keys()),
            },
            indent=2,
        )
        + "\n"
    )

    # Human-readable omnibus
    lines = [
        "# Works Cited by DEW (SJAS)",
        "",
        "Omnibus bibliography of **non-self** citations appearing in Dan E.",
        "Willard's self-verifying / self-justifying / incompleteness-boundary",
        "(SJAS) witnesses. For each external work, every DEW corpus document",
        "in which the citation occurs is listed.",
        "",
        "Self-citations (Willard citing Willard) are excluded; those live in",
        "`../papers` and `../codification/registry/corpus.md`.",
        "",
        f"**{len(payload)}** distinct external works, drawn from "
        f"**{external_rows}** bibliography rows across "
        f"**{len(raw)}** DEW documents.",
        "",
        "Machine-readable companions: [`bibliography.json`](bibliography.json),",
        "[`bibliography.tsv`](bibliography.tsv). Regenerator:",
        "[`build-bibliography.py`](build-bibliography.py).",
        "",
        "## Coverage notes",
        "",
        "- Text layers under `../codification/sources-text/` are the parse source;",
        "  page images govern if a formula or glyph is at issue.",
        "- `Willard2006b` (JSL) has no bibliography in its text layer; the",
        "  conference form `Willard2005-TAB` carries the reference list for that",
        "  floating-point line.",
        "- `Willard1998` chapter OCR only notes selected end references; Solovay",
        "  1994 is recorded manually from the extraction record.",
        "- `Willard1993-TR` is an OCR witness; some entries are fragmentary and",
        "  may need image confirmation.",
        "- Local reference numbers differ by paper; identity is by clustered",
        "  author/year/title, not by shared `[n]`.",
        "",
        "---",
        "",
    ]

    for it in payload:
        heading = it["id"]
        bit = []
        if it["authors"]:
            bit.append(it["authors"])
        if it["year"]:
            bit.append(f"({it['year']})")
        if it["title"]:
            bit.append(f"*{it['title']}*")
        title_line = " ".join(bit) if bit else it["canonical_entry"][:120]
        lines.append(f"## {heading} — {title_line}")
        lines.append("")
        lines.append(f"- **Canonical entry:** {it['canonical_entry']}")
        lines.append(
            f"- **Cited in ({it['n_documents']}):** "
            + ", ".join(f"`{d}`" for d in it["documents"])
        )
        lines.append("- **Local reference keys:**")
        for occ in it["occurrences"]:
            lines.append(
                f"  - `{occ['document']}` [{occ['local_ref']}]"
            )
        lines.append("")

    # Index by DEW document
    lines.append("---")
    lines.append("")
    lines.append("## Index by DEW document")
    lines.append("")
    by_doc: dict[str, list[str]] = defaultdict(list)
    for it in payload:
        for d in it["documents"]:
            by_doc[d].append(it["id"])
    for d in sorted(by_doc):
        ids = by_doc[d]
        lines.append(f"- `{d}` — {len(ids)} external works: " + ", ".join(ids))
    lines.append("")

    (OUT / "BIBLIOGRAPHY.md").write_text("\n".join(lines))

    readme = f"""# Works Cited by DEW

The first-order reverse of [`../works-citing-dew`](../works-citing-dew): an
omnibus bibliography of works **Willard cites** in the course of the SJAS
literature (self-verifying / self-justifying / incompleteness-boundary papers),
with every citing DEW document recorded per entry.

| Artifact | Role |
| --- | --- |
| [`BIBLIOGRAPHY.md`](BIBLIOGRAPHY.md) | Human-readable omnibus (one section per external work) |
| [`bibliography.tsv`](bibliography.tsv) | Compact index: id, year, authors, title, documents |
| [`bibliography.json`](bibliography.json) | Full structured data including local ref keys and raw entries |
| [`build-bibliography.py`](build-bibliography.py) | Regenerator (reads `../codification/sources-text/`) |

## Scope

- **In:** bibliography entries from SJAS-related Willard witnesses under
  `../codification/sources-text/`, mapped to corpus keys in
  `../codification/registry/corpus.md`.
- **Out:** Willard self-citations; biology / data-structures lines; works that
  *cite* Willard (see `../works-citing-dew`).

## Current tally

- **{len(payload)}** clustered external works
- **{external_rows}** bibliography rows after dropping self-citations
- **{self_rows}** self-citation rows excluded
- **{len(raw)}** DEW documents contributing at least one parsed entry

Regenerate:

```sh
python3 works-cited-by-dew/build-bibliography.py
```
"""
    (OUT / "README.md").write_text(readme)

    print(f"clusters={len(payload)} external_rows={external_rows} self_rows={self_rows}")
    print("Top by n_documents:")
    for it in sorted(payload, key=lambda x: -x["n_documents"])[:12]:
        print(
            f"  {it['id']} n={it['n_documents']} {it['year']} "
            f"{it['authors'][:40]} | {it['title'][:50]}"
        )


if __name__ == "__main__":
    main()
