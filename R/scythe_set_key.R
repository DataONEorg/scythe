#' Set API keys for search services
#'
#' This function sets API keys using the `keyring` package. `keyring`
#' uses your operating system's credential store to securely keep
#' track of key-value pairs. Running this function for the first time will
#' prompt you to set a password for your keyring, should you need to lock
#' or unlock it. See `keyring::keyring_unlock` for more details.
#'
#' @param source (char) Key source, one of "scopus" or "springer"
#' @param secret (char) API key value
#'
#' @export

scythe_set_key <- function(source, secret) {
  if (!(source %in% c("springer", "scopus"))) {
    stop("Please set the key source to be one of 'springer' or 'scopus.'",
         call. = FALSE)
  }
  
  kr_list <- keyring::keyring_list()$keyring
  
  if (!("scythe" %in% kr_list)) {
    message("No scythe keyring found. Creating keyring...")
    keyring::keyring_create("scythe")
  }
  if ("scythe" %in% kr_list &
      source %in% keyring::key_list(keyring = "scythe")$service) {
    message("Scythe key already exists. Previous key was overwritten.")
  }
  
  keyring::key_set_with_value(service = source,
                              password = secret,
                              keyring = "scythe")
  
}
