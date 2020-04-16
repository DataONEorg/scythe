[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/scythe)](https://cran.r-project.org/package=scythe)
[![Build Status](https://travis-ci.org/DataONEorg/scythe.png?branch=develop)](https://travis-ci.org/DataONEorg/scythe)
[![Codecov test coverage](https://codecov.io/gh/DataONEorg/scythe/branch/develop/graph/badge.svg)](https://codecov.io/gh/DataONEorg/scythe?branch=develop)
[![Project Status: WIP – Initial development is in progress, but there has not yet been a stable, usable release suitable for the public.](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)

- **Authors**: TBD
- [doi:10.5063/______________](http://doi.org/10.5063/_______________)
- **License**: [Apache 2](http://opensource.org/licenses/Apache-2.0)
- [Package source code on Github](https://github.com/DataONEorg/scythe)
- [**Submit Bugs and feature requests**](https://github.com/DataONEorg/scythe/issues)

Automates the full text harvesting of dataset citations from various full text article databases, 
including Scopus, PLOS, and Pubmed Central.

## Installation 

### Released version

```
install.packages("scythe")
```

The *scythe* R package should be available for use at this point.

### Development version

Development versions can be installed from GitHub.

```
remotes::install_github("DataONEorg/scythe@develop")
```

## Quick Start

TBD...

```
library(scythe)
# R example goes here
```

### Authorization Credentials & API Key Management

Navigate to your home directory in the terminal and create an .Renviron in which to store your API keys.

```
touch .Renviron
```

#### Scopus

In order to obtain a Scopus API key, make an account at the [Elseviers Developers Portal](https://dev.elsevier.com/) and create API key. Once you have your API key, store it in your .Renviron file as the variable ELSEVIER_SCOPUS_KEY.

```
ELSEVIER_SCOPUS_KEY = <your_api>
```

## Acknowledgments
Work on this package was supported by:

- NSF-PLR grant #1546024 to M. B. Jones, S. Baker-Yeboah, J. Dozier, M. Schildhauer, and A. Budden

[![nceas_footer](https://live-ncea-ucsb-edu-v01.pantheonsite.io/sites/default/files/2020-03/NCEAS-full%20logo-4C.png)](http://www.nceas.ucsb.edu)
