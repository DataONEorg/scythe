% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/scythe_set_key.R
\name{scythe_set_key}
\alias{scythe_set_key}
\title{Set API keys for search services}
\usage{
scythe_set_key(source, secret)
}
\arguments{
\item{source}{(char) Key source, one of "scopus" or "springer"}

\item{secret}{(char) API key value}
}
\description{
This function sets API keys using the `keyring` package. `keyring`
uses your operating system's credential store to securely keep
track of key-value pairs. Running this function for the first time will
prompt you to set a password for your keyring, should you need to lock
or unlock it. See `keyring::keyring_unlock` for more details.
}
