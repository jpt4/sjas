# Works Cited by DEW

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

- **157** clustered external works
- **712** bibliography rows after dropping self-citations
- **174** self-citation rows excluded
- **23** DEW documents contributing at least one parsed entry

Regenerate:

```sh
python3 works-cited-by-dew/build-bibliography.py
```
