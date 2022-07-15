
#' Encrypt a password
#' @description Symmetric encryption/decryption with a secret key.
#' Uses the `libsodium` encryption library (in the `sodium` package).
#' Unlike `safer::encrypt_string` on which this function is based, it takes a
#' vector argument.
#' @param x A character string or character vector.
#' @param secret The secret used for decrypting/encrypting
#' @export
#' @rdname encrypt
#' @importFrom safer encrypt_string decrypt_string
encrypt <- function(x, secret = Sys.getenv("SHINTO_PASS_SECRET")){
  
  validate_shinto_pass_secret(secret)
  
  vapply(x, safer::encrypt_string, key = secret,
         USE.NAMES = FALSE, FUN.VALUE = character(1)
  )
}

#' @export
#' @rdname encrypt
decrypt <- function(x, secret = Sys.getenv("SHINTO_PASS_SECRET")){
  
  validate_shinto_pass_secret(secret)
  
  decrypt_try <- function(x, key){
    tryCatch(
      safer::decrypt_string(x,key),
      error = function(e)NA_character_
    )
  }
  
  vapply(x,
         decrypt_try, 
         key = secret,
         USE.NAMES = FALSE, 
         FUN.VALUE = character(1)
  )
  
}



#' Check if a config file is encrypted
#' @description Checks whether a `config.yml` file is completely or partially encrypted.
#' Returns TRUE when all `dbpassword` (or other `password_names`) entries are already encrypted,
#' FALSE when one or more are not yet encrypted (with a message)
#' @seealso \code{\link{encrypt_config_file}}
#' @examples
#' \dontrun{
#' Sys.setenv(SHINTO_PASS_SECRET = "somesecret")
#' config_is_encrypted("conf/config.yml")
#' }
#' @export
config_is_encrypted <- function(file, 
                                password_names = "dbpassword",
                                secret = Sys.getenv("SHINTO_PASS_SECRET")){
  
  obj <- unlist(yaml::read_yaml(file))
  pass <- obj[grepl(password_names, names(obj))]
  
  i_enc <- string_is_encrypted(pass, secret)
  
  if(all(i_enc)){
    message("All passwords are encrypted in this file.")
    return(TRUE)
  } else if(sum(i_enc) > 0){
    message("Some passwords are encrypted in this file, run encrypt_config_file to again.")
    return(FALSE)
  } else {
    message("None of the passwords are encrypted in this file, run encrypt_config_file")
    return(FALSE)
  }
  
}

string_is_encrypted <- function(string, secret = Sys.getenv("SHINTO_PASS_SECRET")){
  !is.na(decrypt(string, secret = secret))
}

#' Encrypts a config file
#' @description Finds password fields in a YAML config file, and encrypts them.
#' The results are always written to a new file, with suffix ".encrypted" (or provided 
#' `suffix` argument).
#' @param file A YAML file with password entries
#' @param secret Secret used in symmetric encryption (see \code{\link{encrypt}})
#' @param suffix Suffix for new file name
#' @param password_names Entries to look for with passwords.
#' @export
#' @rdname encrypt_file
#' @examples
#' \dontrun{
#' Sys.setenv(SHINTO_PASS_SECRET = "mysecret")
#' encrypt_config_file("config.yml")
#' }
encrypt_config_file <- function(file, 
                                secret = Sys.getenv("SHINTO_PASS_SECRET"),
                                suffix = ".encrypted", 
                                password_names = "dbpassword"){
  
  out_file <- paste0(file, suffix)
  cfg <- read_config(file)
  
  n_dec <- 0
  n_notdec <- 0
  
  for(i in seq_along(cfg)){
    
    x <- cfg[[i]]
    
    for(j in seq_along(x)){
      
      ind <- which(names(x[[j]]) %in% password_names)
      if(length(ind) > 0){
        pass <- cfg[[i]][[j]][[ind]]
        
        # if not already encrypted, encrypt it now
        if(is.na(decrypt(pass))){
          cfg[[i]][[j]][[ind]] <- encrypt(pass, secret = secret)
          n_dec <- n_dec + 1
        } else{
          n_notdec <- n_notdec + 1
        }
      }
    }
  }
  
  message(glue::glue("{n_dec} passwords encrypted"))
  message(glue::glue("{n_notdec} passwords were already encrypted"))
  
  yaml::write_yaml(cfg, out_file)
}



#' Decrypt a config file
decrypt_config_file <- function(file, file_out,
                                db_config = NULL,  # keep only entries that appear in this list
                                secret = Sys.getenv("SHINTO_PASS_SECRET"),
                                password_names = "dbpassword"){
  
  # normally, the global config
  cfg <- read_config(file)
  
  # db_config list (app db config)
  dbc <- read_config(db_config)
  
  # loop over sections (development, production, etc.)
  for(i in seq_along(cfg)){
    
    x <- cfg[[i]]
    section <- names(cfg)[i]
    
    # db_config has no default section; defaults must appear in development
    if(section == "default")section <- "development"
    
    for(k in seq_along(x)){
      
      if(!names(x)[k] %in% names(dbc[[section]])){
        cfg[[i]][[k]] <- NULL
      } else {
        
        ind <- which(names(cfg[[i]][[k]]) %in% password_names)
        if(length(ind) > 0){
          pass <- cfg[[i]][[k]][[ind]]
          cfg[[i]][[k]][[ind]] <- decrypt(pass, secret = secret)
        }
      
      }
    }
  }
  
  yaml::write_yaml(cfg, file_out)
    

}


if(FALSE){
  decrypt_config_file(open_global_config(), "test/test.yml", db_config = "test/db_connections.yaml")  
}
  



get_shinto_pass_secret <- function(){
  Sys.getenv("SHINTO_PASS_SECRET")
}

# Utils
# - not exported
validate_shinto_pass_secret <- function(secret){
  
  if(is.null(secret) || secret == ""){
    stop("Set environment variable SHINTO_PASS_SECRET or provide a secret. See ?shintobag::encrypt.")
  }
  
}




