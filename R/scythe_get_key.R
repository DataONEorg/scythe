#' Return which API keys are set
#' 
#' This function looks for API keys for Scopus and Springer, and returns
#' them if they are set.
#'
#' @return list A named list of key value pairs.
#'
#' @export

scythe_get_key <- function(){
    
    kr_list <- keyring::keyring_list()$keyring
    
    if ("scythe" %in% kr_list){
        r <- list(scopus_key = keyring::key_get("scopus_key", keyring = "scythe"),
             springer_key = keyring::key_get("springer_key", keyring = "scythe"))
    }
    if (!("scythe" %in% kr_list)){
        r <- list(scopus_key = Sys.getenv("scopus_key"),
                  springer_key = Sys.getenv("springer_key"))
    }
    
    return(r)
    
}

