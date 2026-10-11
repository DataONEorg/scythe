[![R-CMD-check](https://github.com/DataONEorg/scythe/workflows/R-CMD-check/badge.svg)](https://github.com/DataONEorg/scythe/actions)
[![Codecov test coverage](https://codecov.io/gh/DataONEorg/scythe/branch/develop/graph/badge.svg)](https://codecov.io/gh/DataONEorg/scythe?branch=develop)
[![Project Status: WIP – Initial development is in progress, but there has not yet been a stable, usable release suitable for the public.](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)

- **Authors**: Jeanette Clark, Matthew B. Jones, Maya Samet, Althea Marks
- [doi:10.18739/A2VT1GR7M](http://doi.org/10.18739/A2VT1GR7M)
- **License**: [Apache 2](http://opensource.org/licenses/Apache-2.0)
- [Package source code on Github](https://github.com/DataONEorg/scythe)
- [**Submit Bugs and feature requests**](https://github.com/DataONEorg/scythe/issues)

Automates the full text harvesting of dataset citations from various full text article databases, 
including Scopus, PLOS, Springer, XDD, and OpenAlex.

## Installation

### Released version

```
remotes::install_github("DataONEorg/scythe@v1.1.0")
```

The *scythe* R package should be available for use at this point.

### Development version

Development versions can be installed from GitHub.

```
remotes::install_github("DataONEorg/scythe@develop")
```

## Quick Start

To set API keys for use in the package, see the section below on authentication.

```
library(scythe)
scythe_set_key(source = "scopus", secret = "YOUR KEY")

identifier <- "10.18739/A22274"
results <- citation_search(identifier)
```

### Rate limits and resuming a search

Each source limits how fast it can be searched. Scythe spaces out its requests, slows down when a source rate-limits it, and keeps to published limits such as PLOS's 10 requests a minute, 300 an hour and 7,200 a day. A short wait for a limit to reset happens automatically. If the wait would be longer (more than 5 minutes by default, set with `options(scythe.max_wait = ...)`), the search stops and returns the citations it has found so far, with a warning saying when it can continue. The search functions report problems as warnings of these classes:

- `scythe_rate_limited`: the source rate-limited the search, its quota ran out, or its requests kept failing.
- `scythe_auth_error`: the source rejected the API key.
- `scythe_request_failed`: some identifiers couldn't be searched and were skipped.

Each search function's result has a `search_status` attribute (`openalex_status` for OpenAlex) recording whether the search completed and, if not, why it stopped. To search a long list of identifiers over several runs, pass a `checkpoint_dir`. Each run then continues where the last one stopped, and doesn't send requests before a recorded rate limit has reset:

```
results <- citation_search_plos(identifiers, checkpoint_dir = "checkpoints")
attr(results, "search_status")
```

### Authorization Credentials & API Key Management

The function `scythe_set_key()` manages API keys using the [`keyring`](https://github.com/r-lib/keyring) package. `keyring` uses your operating system's credential store to securely keep track of key-value pairs. Running `scythe_set_key()` for the first time will prompt you to set a password for your keyring, should you need to lock or unlock it.

#### Scopus

To obtain a Scopus API key, make an account at the [Elseviers Developers Portal](https://dev.elsevier.com/) and create API key. Once you've obtained your key, you can use `scythe_set_key()` to securely set your key. This key is accessed in the `citation_search()` function, but you can also retrieve the value using `keyring::key_get()`.

```
scythe_set_key(source = "scopus", secret = "YOUR_KEY")
keyring::key_get("scopus", keyring = "scythe")
```

#### Springer

The Springer Nature API key is available by creating an application [here](https://dev.springernature.com/) after signing up for an account. The key can be set and retrieved using:

```
scythe_set_key(source = "springer", secret = "YOUR_KEY")
keyring::key_get("springer", keyring = "scythe")
```

#### OpenAlex

[OpenAlex](https://openalex.org) works without a key, but its daily budget without one is only 1,000 credits. A free API key, from an [OpenAlex account](https://openalex.org/settings/api), raises it to $1 of usage a day (10,000 credits). A full-text search costs 10 credits and any other query costs 1. Optionally, also set a contact email:

```
scythe_set_key(source = "openalex", secret = "YOUR_KEY")
scythe_set_key(source = "openalex_mailto", secret = "you@example.org")
```

`citation_search_openalex()` finds works that mention a dataset DOI anywhere in their full text, and works whose reference lists cite the dataset in OpenAlex's citation graph. It returns only citing works that have a DOI and are not datasets. Everything else is written as JSON Lines files to a review log directory, set with `log_dir` or `options(scythe.log_dir = ...)`:

- datasets that cite or mention other datasets;
- citing works without a DOI;
- identifiers that OpenAlex doesn't index;
- batches that failed.

Many dataset-to-dataset links are versions of a dataset or members of a data package, rather than reuse of data. After a search, `classify_data_relations(log_dir)` looks up each link in DataCite and labels it as a version, derivation, composition or citation link. It isn't needed to get citation counts, and it doesn't change the search results; it's there for reviewing these links, and for future versions that count data-to-data citations. It sends up to two DataCite requests for each dataset, but uses no OpenAlex credits.

Before sending any request, the function estimates the credits a search will use. It stops if the estimate exceeds `max_credits`, which defaults to 500. Pass `max_credits = Inf` to approve a larger search:

```
results <- citation_search_openalex(identifiers, max_credits = Inf,
                                    checkpoint_dir = "openalex-checkpoints")
```

If OpenAlex rate-limits a search, or the daily budget runs out, the search stops sending requests and returns what it has found so far. It records when the limit resets, so later runs that use the same `checkpoint_dir` wait until then. A rerun with the same `checkpoint_dir` skips the batches that already finished.

For the scheduled search, approve OpenAlex spending with the helm chart's `cronjob.openalex.approve` value: a number of credits, or `unlimited`. See `tools/README.md` for measuring OpenAlex's precision and recall.


## How citations are identified

Scythe records each citation as a pair: the citing work (usually an article's DOI) and the dataset identifier it cites. DataONE's metrics service records citations the same way, as `source_id` and `target_id`.

**The cited identifier is the one the citation used, PID or SID:**
- in DataONE, every version of a dataset has its own identifier (PID), and a series identifier (SID) always resolves to the newest version;
- a paper that cites a SID is recorded against that SID, and one that cites a PID against that PID;
- citations aren't moved between versions or grouped under one dataset; reporting can add up versions later.

**The recorded identifier is DataONE's exact string.** Repositories write DOIs in many ways, as `doi:10.18739/A2X`, `https://doi.org/10.18739/A2X`, a bare `10.18739/A2X` and more, and DataONE keeps each exactly as submitted. Scythe searches for the DOI, but records the citation against the identifier exactly as DataONE has it (`target_id`), whatever form the citing paper used.

**How the sources are searched.** Every source is searched for the bare DOI name, `10.xxxx/yyyy`. Each source matches it regardless of case, and however the citing text writes the DOI, for example as a `doi:` identifier or a `https://doi.org/` link. OpenAlex's citation-graph search works differently: it finds the dataset's own record in OpenAlex by its DOI, then follows OpenAlex's citation links to it, so it only finds datasets that OpenAlex has a record for.

Scythe currently searches only for identifiers that are DOIs. Other identifier types may be added later.

## Searching DataONE's repositories

`collect_dataone_dois()` gathers the DOI identifiers of DataONE repositories (member nodes), ready to search:

```
dois <- collect_dataone_dois(c("urn:node:ARCTIC", "urn:node:ESS_DIVE"))
attr(dois, "doi_sources")
```

It collects every DOI, whichever way the repository uses them: ARCTIC gives each metadata record a DOI PID, ESS-DIVE gives each dataset a DOI SID, and EDI gives each package's ORE resource map a DOI PID. Each row has the normalized `doi` used for searching and the exact `identifier` in DataONE.

**Where the DOIs come from:**
- **Up-to-date indexes:** each repository's own search index is used where possible, because the DataONE coordinating node (CN) can lag behind.
- **Fallback:** if a repository's index refuses queries, or has fewer records than the CN, the CN is used instead.
- **Authority:** only records the repository is authoritative for are kept, so copies of other repositories' records are left out.
- **Skipped:** DataCite test DOIs (`10.5072/…`) are skipped, as are any prefixes given in `exclude_prefixes`.

## Comparing with DataONE's metrics service

`get_metrics_citations()` fetches the citations DataONE's metrics service has recorded for one or more repositories. `citations_not_in_metrics()` keeps only the found citations it doesn't have yet. It compares (citing work, cited identifier) pairs, with DOIs compared regardless of how they're written or their case:

```
recorded <- get_metrics_citations(c("urn:node:ARCTIC", "urn:node:KNB"))
new <- citations_not_in_metrics(found, recorded)
```

The service's address is set with `options(scythe.metrics_url = ...)`.

## Scheduled search

`scheduled_search()` runs an ongoing search of DataONE. It's designed to be called hourly:
- **Priority cycles:** twice a month, it searches a list of priority repositories with OpenAlex and, in parallel, xDD, Springer, Scopus and PLOS;
- **Other repositories:** monthly, it runs OpenAlex's citation-graph search over all other DataONE repositories;
- **Metrics:** daily, it lists the citations the metrics service doesn't have yet.

It keeps to each source's rate limits, resuming where it stopped, and reports progress and problems in a status file. The helm chart in [`helm/`](helm/README.md) runs it as a Kubernetes CronJob; its README explains the cycle, the files it writes, and how to watch it.

## Testing

The tests never spend OpenAlex credits by default: two live OpenAlex tests are skipped unless you set `SCYTHE_TEST_OPENALEX=true`. See the Testing section of [CONTRIBUTING.md](CONTRIBUTING.md) for what runs where, and how to run all the live tests before a release.

## Acknowledgments
Work on this package was supported by:

- NSF-PLR grant #1546024 to M. B. Jones, S. Baker-Yeboah, J. Dozier, M. Schildhauer, and A. Budden
- NSF-PLR grant #2042102 to M. B. Jones, A. Budden, M. Schildhauer, and J. Dozier

[![nceas_footer](https://live-ncea-ucsb-edu-v01.pantheonsite.io/sites/default/files/2020-03/NCEAS-full%20logo-4C.png)](https://www.nceas.ucsb.edu)

[![dataone_footer](https://www.dataone.org/sites/all/images/DataONE_LOGO.jpg)](https://www.dataone.org)
