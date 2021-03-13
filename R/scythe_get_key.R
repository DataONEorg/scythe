#' Get API secret for a key source.
#'
#' Look for API keys for services, which are represented as character strings.
#'
#' Secrets are typically stored in a keyring named "scythe" (see the keyring
#' package), or, alternatively, in an environment variable with a name
#' identical to "source".
#'
#' @param source the name of the source service to look up
#' @return character the secret value, or NA if not set
#'
#' @export
#' @examples
#' \dontrun{
#' scythe_get_key("scopus_key")
#' }
scythe_get_key <- function(source) {
    secret <- NA
    if (Sys.getenv(source) != "") {
        secret <- Sys.getenv(source)
    } else {
        secret <- tryCatch({
            keyring::key_get(source, keyring = "scythe")
        },
        error = function(cond) {
            return(NA)
        })
    }
    return(secret)
}
