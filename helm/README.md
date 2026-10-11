# Scythe Helm Chart

This chart deploys a Kubernetes CronJob that searches the literature for citations of DataONE datasets, using the `scythe` R package, and prepares new citations for submission to DataONE's metrics service.

The job runs **hourly**. Each run reads the search's saved state from the persistent volume, does whatever work is due and possible, and exits. The work is organized as:

- **Priority cycles**, starting on the 1st and 15th of each month, which search a list of priority repositories (member nodes) as completely as possible:
  1. the OpenAlex citation-graph search;
  2. then, in parallel, the full-text searches: OpenAlex full text, xDD, Springer, Scopus and PLOS.
- **A monthly citation-graph pass over every other DataONE repository**, once a cycle's OpenAlex searches are done. PANGAEA goes last.
- **A daily comparison with DataONE's metrics service**, which lists the citations it doesn't have yet in `results/to-submit.csv`.

Each source's rate limits are tracked as the job runs. A source that hits a limit stops, and starts again on a later run once the limit resets, so a search that takes days continues where it left off.

The R function behind all this is `scythe::scheduled_search()`; see its help page for the details of each setting.

## How citations are identified

A citation is a pair: the citing work (`source_id`, usually an article's DOI) and the DataONE identifier it cites (`target_id`). This is how DataONE's metrics service records them.

**`target_id` is the identifier exactly as DataONE's system metadata records it.** That can be a version identifier (PID) or a series identifier (SID):
- a paper that cites a SID is recorded against the SID;
- a paper that cites a PID is recorded against that PID;
- citations are never moved between versions or grouped under one dataset; the metrics service aggregates versions when it reports.

Repositories write DOIs in many different ways: `doi:10.18739/A2X`, `https://doi.org/10.18739/A2X`, `http://dx.doi.org/10.18739/A2X`, a bare `10.18739/A2X`, and more. DataONE preserves each as submitted. Scythe records the exact literal: a citation found as `https://doi.org/10.18739/a2x` is recorded as `doi:10.18739/A2X` if that is how DataONE identifies the object. The `target_type` column says whether it's a PID or a SID.

**Every DOI identifier is collected,** whatever the repository's convention:
- ARCTIC uses a DOI as the PID of each metadata record;
- ESS-DIVE uses a DOI as the SID;
- EDI uses a DOI as the PID of each data package's ORE resource map.

Collection checks both PIDs and SIDs, of every kind of record, in any DOI serialization. Occasionally DataONE has two literals for the same DOI, probably by mistake, since objects can't be changed once submitted: for example `doi.org/10.18739/A29G5GG55` and `doi:10.18739/A29G5GG55`. Scythe then uses the `doi:` form and lists the other in the DOI list's `other_identifiers`.

**Searching uses the bare DOI name**, `10.xxxx/yyyy`, which every source matches regardless of case and however the citing text writes it (`doi:…`, a `https://doi.org/` link, and so on). OpenAlex's citation-graph search instead finds the dataset's own OpenAlex record by its DOI, then follows OpenAlex's citation links to it, so it doesn't depend on how the citing text is written. It does depend on OpenAlex having a record for the dataset.

For now, scythe searches only for identifiers that are DOIs, since those are what journals ask authors to cite. Other identifier types may be added later.

## A cycle, step by step

1. **Collect the DOIs.**
   - **Priority repositories:** their DOIs come from each repository's own search index, which is up to date, whereas DataONE's coordinating node (CN) can lag by weeks and has gaps.
   - **Authority:** only the records a repository is authoritative for are kept, so copies it holds of other repositories' records are left out.
   - **Fallback:** a repository whose index refuses queries (ESS-DIVE), or has fewer records than the CN (GOA, whose index is mislabelled), uses the CN instead, with a note in the job log.
   - **Skipped:** DataCite test DOIs (`10.5072/…`) and the prefixes in `priority.excludePrefixes`.
   - **Saving:** the list is saved, so the cycle searches the same DOIs to the end.
2. **OpenAlex citation-graph search** over the priority DOIs. Its results are saved on their own (`priority-graph.csv`) before any full-text results.
3. **Full-text searches.**
   - OpenAlex full text runs in the main process. xDD, Springer, Scopus and PLOS each run as a separate *track*, a parallel process, for up to `tracks.minutes` per run.
   - The tracks start on the run after the cycle's citation-graph search finishes.
   - Each track works through the priority repositories in order, saving its progress, and continues on the next run.
   - A track that hasn't finished when the next cycle starts finishes its pass first, then starts on the new cycle's DOIs.
   - In later cycles, OpenAlex's full-text search looks only at recently published works (see Lookback).
4. **The other repositories.** Once a cycle's OpenAlex searches are done, the monthly citation-graph pass runs over every other repository's DOIs, collected from the CN. A new priority cycle pauses it, and it resumes afterwards from where it stopped.
5. **The metrics service.** Once a day, every citation found so far is compared with those the service has:
   - matching is on the exact `{source_id, target_id}` pair, with DOIs compared regardless of serialization or case;
   - those the service doesn't have are written to `results/to-submit.csv`;
   - every pair is submitted, including the common case of one work citing several datasets in the same repository.

**Timing:** a typical two-week cycle takes about four days, set by PLOS's published limit of 7,200 requests a day. The first cycle's OpenAlex full-text search covers all works, not just recent ones, and can take up to three weeks of OpenAlex budget; later cycles wait for it.

## Files on the volume

Everything is under `persistence.mountPath` (`/var/data/scythe` by default):

| Path | Contents |
|---|---|
| `results/citations.csv` | Every citation found so far, one row per pair: `article_id`, `article_title`, `dataset_id` (the DOI searched for), `target_id` (the exact DataONE identifier), `target_type` (`pid` or `sid`), `node`, `sources` (each search that found it), `first_phase`, `first_cycle`, `first_found`, and `in_metrics` (`yes`, `no`, or `unmapped` for a citation without a DataONE identifier, which shouldn't happen) |
| `results/to-submit.csv` | The citations the metrics service doesn't have, with `source_id` (the citing DOI, lowercase, without a prefix) and `target_id` first |
| `results/<cycle>/` | Each search's citations for a cycle, once it completes: `priority-graph.csv`, `priority-text.csv` (OpenAlex full text), `priority-<source>.csv`, and `priority-text-new.csv` (full-text citations the graph search didn't find) |
| `results/others-<date>/others-graph.csv` | A completed pass over the other repositories |
| `state/status.md` | **A summary of the search, rewritten every run:** open alerts, the current cycle and each track's progress, the DOIs collected per repository, the other repositories' pass, the metrics comparison, and citation counts |
| `state/alerts.log` | Every alert raised and resolved, with times |
| `state/dois/` | The DOI list for each cycle and pass, with each DOI's exact identifier, PID or SID, repository, and where it was collected from |
| `state/cycle.json`, `others.json`, `tracks/`, `openalex/`, `metrics.json`, … | The search's progress and checkpoints; don't edit them while the job could be running |
| `openalex-logs/` | OpenAlex's review logs: matches, dataset-to-dataset links, citing works without a DOI, identifiers OpenAlex doesn't have, failed batches |

All results files are replaced whole, so they can be copied or read at any time.

## Watching the search

Start with `state/status.md`. The job log also has one summary line for OpenAlex and each track on every run, for example:

```
PLOS: on cycle 2026-10-15's DOIs; ARCTIC 4,200/11,875, overall 6,100/27,900; 12 citations; paused (rate limited) until 2026-10-16T02:14:00Z
```

Problems that need someone to act are logged once as `ACTION NEEDED:` lines, listed in `status.md` while they last, and logged as `RESOLVED:` when they clear:

| Alert | What to do |
|---|---|
| No API key, or a key rejected | Add or fix the key in the `api-keys` secret. That source's track waits until then. |
| A track's pass has spanned two cycles | That source can't keep up with the cycle: use fewer priority repositories or less frequent cycles, or leave it out of `tracks.sources`. Scopus's weekly quota is the likely cause. |
| A track paused for more than three days | Check the service's status and the key's quota. |
| The first full OpenAlex full-text pass projected to take over a month | Raise the OpenAlex budget or use fewer priority repositories. |
| Collecting DOIs failed, or found far fewer than the last cycle | Check the DataONE CN and the repositories' indexes; see `state/dois/<cycle>-sources.csv`. |
| A track failed | Check the job log; the track is retried on each run. |
| The metrics comparison hasn't worked for three days | Check the metrics service and `cronjob.metrics.url`. |

## Submitting citations

Automatic submission to the metrics service is planned but not yet built. Until then, prepare the citations in `results/to-submit.csv` with `scythe::write_citation_pairs()`, which looks up each citing work's metadata in Crossref and writes the JSON the metrics service ingests. Pass the exact DataONE identifier as the target:

```r
submit <- read.csv("results/to-submit.csv", colClasses = "character")
pairs <- data.frame(article_id = submit$source_id, dataset_id = submit$target_id)
scythe::write_citation_pairs(pairs, "citation_pairs.json")
```

## Configuration

Key values in `values.yaml`, all under `cronjob`:

| Value | Default | Meaning |
|---|---|---|
| `schedule` | `5 * * * *` | Keep this hourly. |
| `activeDeadlineHours` | 23 | Kubernetes stops a run that takes longer than this. |
| `lockStaleHours` | 24 | Only one run works at a time; a lock older than this was left by a run that died, and is taken over. |
| `priority.nodes` | ARCTIC, ESS_DIVE, KNB, GOA, and the DataONE-hosted repositories | Searched every cycle, in this order. |
| `priority.excludePrefixes` | `urn:node:KNB=10.6073/AA/,10.6085/AA/` | DOI prefixes not searched from a repository's own index, as `node=prefix,prefix;node=…`. KNB's index lists retired LTER records and PISCO's records as its own (DataONEorg/dataone#29); PISCO's are still searched through PISCO. |
| `priority.cycleDays` | `1,15` | Days of the month a cycle may start. |
| `others.nodes` | `all` | The other repositories' graph pass: `all` (every repository not in the priority list), `none`, or a list. |
| `others.exclude`, `others.last`, `others.everyDays` | `""`, PANGAEA, 30 | Repositories left out of that pass, searched last in it, and days between passes. |
| `openalex.approve` | `""` (500 credits a run) | The OpenAlex credits a run may spend, or `unlimited`. |
| `openalex.lookback` | 90 | See Lookback. |
| `openalex.fullTextEveryCycles` | 0 | Every this many cycles, the OpenAlex full-text search covers all works; 0 for never. |
| `tracks.sources` | `xdd,springer,scopus,plos` | The full-text tracks. |
| `tracks.minutes` | 50 | The longest each track runs per hourly run. |
| `tracks.options` | `""` | Overrides of scythe's rate-limit settings, as `name=value;…`, for example `springer.min_interval=2;max_wait=300`. The tracks adapt to each service's limits, so these are only starting points. |
| `metrics.checkHours` | 24 | Hours between metrics comparisons; 0 for never. |
| `metrics.url` | `""` (package default) | The metrics service's address. The default is `logproc-stage-ucsb-1.test.dataone.org`, the production server, because the certificate for `metrics.dataone.org` doesn't yet include that name (DataONEorg/metrics-service#103). |
| `alerts.passCycles`, `alerts.pausedDays`, `alerts.projectionDays` | 2, 3, 30 | When to raise those alerts. |

### Lookback

In later cycles, the OpenAlex full-text search only looks for citing works published since the start of the last cycle that completed its full-text search, less `openalex.lookback` days. This keeps each cycle to a day or two of OpenAlex budget, instead of searching every published work again.

The trade-off: OpenAlex adds works some time after they're published. A work indexed more than `lookback` days after its publication date is missed by these incremental searches. A longer lookback catches more of those late additions, but costs more credits each cycle. To catch anything missed, set `openalex.fullTextEveryCycles` to search all works every so many cycles.

The lookback only affects OpenAlex's full-text search:
- the citation-graph searches always cover all works;
- the other sources can't filter by date, so they search every DOI in every pass.

## API keys

For instructions on obtaining API keys, see the package's README.md. The job reads them from a Kubernetes secret named `api-keys`:

```
kubectl create secret generic -n scythe api-keys \
  --from-literal=springer={key} \
  --from-literal=scopus={key} \
  --from-literal=openalex={key} \
  --from-literal=openalex_mailto={email}
```

Every key is optional. Without a Springer or Scopus key, that source's track waits and an alert is raised. Without an OpenAlex key, OpenAlex allows only 1,000 credits a day, far too few for this job.

## Persistent storage

This chart uses a dynamic PVC on CephFS to keep the search's state and results between runs. An example is below. For more about CephFS on the cluster, see the [k8s-cluster docs](https://github.com/DataONEorg/k8s-cluster/blob/main/storage/Ceph/Ceph-CSI-CephFS.md#provisioning-dynamic-cephfs-volumes).

```
---
apiVersion: v1
kind: PersistentVolumeClaim
metadata:
  name: scythe-results
spec:
  accessModes:
    - ReadWriteMany
  resources:
    requests:
      storage: 10Gi
  storageClassName: csi-cephfs-sc
```

To create the PVC, run `kubectl apply -f pvc.yaml -n scythe`. Do this only once. Two releases must not share the same volume and directory; the run lock stops two runs working at the same time, but they would still share one search's state.

## Container image

`scripts/search.R` is copied into the image and run by the CronJob. It's a thin wrapper that passes the chart's values to `scythe::scheduled_search()`.

The image installs scythe from GitHub. The `SCYTHE_REF` build argument chooses the branch, tag or commit (default `develop`), so an image must be built from a ref that includes the code the chart expects. Build it from the `helm/` directory, and push it, with docker buildx:

```
docker buildx build \
  --platform linux/amd64 \
  --build-arg SCYTHE_REF=develop \
  -t ghcr.io/dataoneorg/scythe:{tag} \
  --push .
```

The chart uses the image tagged with the chart's `appVersion`, unless `image.tag` is set.

## Running a search by hand

To start a run now, instead of waiting for the next hour:

```
kubectl create job -n scythe --from=cronjob/scythe scythe-manual-$(date +%s)
```

If a scheduled run is still going, the manual run sees its lock and exits straight away.
