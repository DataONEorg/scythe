#' Set API keys for search services
#'
#' @param identifiers a vector of identifiers to be searched for
#'
#' @export
#' @examples

scythe_set_key <- function(scopus_key = NULL, bmc_key = NULL){
    
    message("Setting keyring backend to 'file'")
    
    options(keyring_backend = "file")
    
    if (!("scythe" %in% keyring::keyring_list()$keyring)){
        kr <- keyring::backend_file$new()
        kr$keyring_create("scythe")
    }
    if ("scythe" %in% keyring::keyring_list()$keyring){
        message("Keyring already exists.")
    }



    if (!is.null(scopus_key)){
        keyring::key_set_with_value(service = "ELSEVIER_SCOPUS_KEY",
                                    password = scopus_key,
                                    keyring = "scythe")
    }
    
    # if (!is.null(bmc_key)){
    #     keyring::key_set_with_value(service = "SPRINGER_BMC_KEY",
    #                                 password = bmc_key,
    #                                 keyring = "scythe")
    # }
}

