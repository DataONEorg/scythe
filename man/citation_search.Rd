% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/citation_search.R
\name{citation_search}
\alias{citation_search}
\title{Search for citations in text across all APIs}
\usage{
citation_search(identifiers, sources = c("plos", "scopus", "springer", "xdd"))
}
\arguments{
\item{identifiers}{a vector of identifiers to be searched for}

\item{sources}{a vector indicating which sources to query (one or
more of plos, scopus, springer)}
}
\value{
tibble of matching dataset and publication identifiers
}
\description{
Search for citations in text across all APIs
}
\examples{
\dontrun{
identifiers <- c("10.18739/A22274", "10.18739/A2D08X", "10.5063/F1T151VR")
result <- citation_search(identifiers, sources = c("plos"))
}
}
