[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/scythe)](https://cran.r-project.org/package=scythe)
[![R-CMD-check](https://github.com/DataONEorg/scythe/workflows/R-CMD-check/badge.svg)](https://github.com/DataONEorg/scythe/actions)
[![Codecov test coverage](https://codecov.io/gh/DataONEorg/scythe/branch/develop/graph/badge.svg)](https://codecov.io/gh/DataONEorg/scythe?branch=develop)
[![Project Status: WIP – Initial development is in progress, but there has not yet been a stable, usable release suitable for the public.](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)

- **Authors**: Jeanette Clark, Matthew B. Jones, Maya Samet
- [doi:10.5063/______________](http://doi.org/10.5063/_______________)
- **License**: [Apache 2](http://opensource.org/licenses/Apache-2.0)
- [Package source code on Github](https://github.com/DataONEorg/scythe)
- [**Submit Bugs and feature requests**](https://github.com/DataONEorg/scythe/issues)

Automates the full text harvesting of dataset citations from various full text article databases, 
including Scopus, PLOS, and Springer.

## Installation

### Released version

```
remotes::install_github("DataONEorg/scythe@v0.9.0")
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

### Authorization Credentials & API Key Management

The function `scythe_set_key()` manages API keys using the [`keyring`](https://github.com/r-lib/keyring) package. `keyring` uses your operating system's credential store to securely keep track of key-value pairs. Running `scythe_set_key()` for the first time will prompt you to set a password for your keyring, should you need to lock or unlock it.

#### Scopus

To obtain a Scopus API key, make an account at the [Elseviers Developers Portal](https://dev.elsevier.com/) and create API key. Once you've obtained your key, you can use `scythe_set_key()` to securely set your key. This key is accessed in the `citation_search()` function, but you can also retrieve the value using `keyring::key_get()`.

```
scythe_set_key(source = "scopus", secret = "YOUR_KEY")
keyring::key_get("scopus", keyring = "scythe")
```

#### Springer

The Springer Nature API key is available by creating an application [here](https://dev.springernature.com/admin/applications) after signing up for an account. The key can be set and retrieved using:

```
scythe_set_key(source = "springer", secret = "YOUR_KEY")
keyring::key_get("springer", keyring = "scythe")
```

## Acknowledgments
Work on this package was supported by:

- NSF-PLR grant #1546024 to M. B. Jones, S. Baker-Yeboah, J. Dozier, M. Schildhauer, and A. Budden

[![nceas_footer](https://live-ncea-ucsb-edu-v01.pantheonsite.io/sites/default/files/2020-03/NCEAS-full%20logo-4C.png)](https://www.nceas.ucsb.edu)

[![dataone_footer](https://www.dataone.org/sites/all/images/DataONE_LOGO.jpg)](https://www.dataone.org)
