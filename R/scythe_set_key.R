#' Set API keys for search services
#' 
#' This function sets API keys using the `keyring` package. `keyring`
#' uses your operating system's credential store to securely keep
#' track of key-value pairs. Running this function for the first time will
#' prompt you to set a password for your keyring, should you need to lock
#' or unlock it. See `keyring::keyring_unlock` for more details.
#'
#' @param scopus_key (char) SCOPUS API key value
#' @param bmc_key (char) BMC API key value
#'
#' @export

scythe_set_key <- function(scopus_key = NULL, bmc_key = NULL){
    
    kr_list <- keyring::keyring_list()$keyring
    
    if (!("scythe" %in% kr_list)){
        message("No scythe keyring found. Creating keyring...")
        keyring::keyring_create("scythe")
    }
    if ("scythe" %in% kr_list){
        message("Scythe keyring already exists. Previous key was overwritten.")
    }

    if (!is.null(scopus_key)){
        keyring::key_set_with_value(service = "scopus_key",
                                    password = scopus_key,
                                    keyring = "scythe")
    }
    
    if (!is.null(bmc_key)){
        keyring::key_set_with_value(service = "bmc_key",
                                    password = bmc_key,
                                    keyring = "scythe")
    }
}

