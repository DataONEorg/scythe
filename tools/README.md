# Developer tools

The scripts here aren't part of the package and aren't run by the tests or by CI.

## Benchmarking the OpenAlex adapter

`benchmark_openalex.R` measures how well `citation_search_openalex()` finds a gold set of known dataset citations:

- **Recall.** Which gold citations it found. For each one it missed, the reason, found by looking the article up in OpenAlex:
  - the article isn't in OpenAlex;
  - the article has no full text, and its references don't resolve the dataset;
  - the article has full text, but the DOI search didn't match it;
  - the identifier can't be normalized.
- **Precision.** Each match that isn't in the gold set is checked against the citing work's Crossref reference list. Matches that Crossref doesn't corroborate go to `needs-review.csv`, for someone to check against the article's full text.
- **Coverage by method and source.** Recall broken down by repository DOI prefix, matches broken down by method (`text`, `graph`, `both`), and overlap with the sources in the gold set.

### Before you run it

A benchmark spends real OpenAlex credits. Full-text searches cost 10 credits each, and a gold set of a few thousand datasets can need tens of thousands of credits. So:

1. Run it only after the package tests pass (`devtools::test()`, including the live OpenAlex tests).
2. Set an OpenAlex API key with `scythe_set_key("openalex", ...)`. The script won't run without one.
3. Run it from a machine with a good, stable connection, such as a cluster node, in a session that can run for hours (`screen` or `tmux`).
4. Approve the cost explicitly. The script prints its estimate and requires either `--approve <credits>`, `--approve unlimited`, or a "y" at the prompt in an interactive R session. It never starts on its own.

If OpenAlex rate-limits the run, or the daily budget runs out, the script stops and keeps its checkpoints. Rerun the same command after the reset time it prints, and it continues where it stopped.

### Gold set

Use a CSV with `article_id` and `dataset_id` columns (DOIs, in any form), and optionally a `source` column. For example, export the confirmed citations from the DataONE metrics service with `get_metrics_citations()` in `helm/scripts/search.R`, renaming `source_id` to `article_id` and `target_id` to `dataset_id`.

```
Rscript tools/benchmark_openalex.R --gold gold.csv --out openalex-benchmark --approve 20000
```

The results go in the output directory: `summary.txt`, `gold-recall.csv`, `precision-by-method.csv`, `needs-review.csv`, `overlap-by-source.csv`, and the adapter's review logs in `logs/`.
