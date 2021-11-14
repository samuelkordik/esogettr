#' Stores Login Credentials
#'
#' Stores login credentials to ESO EHR securely on the system keyring.
#' See ?keyring for details on system credential stores.
#'
#' @param static TRUE only displays the results without prompting for updates unless none is found.
#'
#' @return
#' @export
update_authentication <- function(static = FALSE) {

  user_key <- keyring::key_list("ESO_user")
  cid_key <- keyring::key_list("ESO_cid")

  user_key <- paste(user_key$username, collapse = "")
  cid_key <- paste(cid_key$username, collapse = "")

  # First time here?
  if (stringr::str_length(cid_key) == 0 | stringr::str_length(user_key) == 0) {
    if (interactive()) {
      print("No ESO credentials found!")
      get_new_credentials()
    } else {
      print("No ESO credentials found! Run update_authentication() in console to store credentials.")
    }

  } else {

    if (interactive() & !static) {
      do_change <- readline(prompt=glue::glue("Current ESO credentials:
                                            ----------------------------
                                            Company ID: {cid_key}
                                            Username: {user_key}
                                            ----------------------------
                                            Change this? (Yes/no/cancel)"))
      switch(str_to_upper(do_change),
             YES = {
               get_new_credentials()
             },
             NO = {
               print("Keeping existing credentials.")
             },
             CANCEL = {
               stop("User cancelled")
             })
    } else {
      print(glue::glue("Current ESO credentials:
                        ----------------------------
                        Company ID: {cid_key}
                        Username: {user_key}
                        ----------------------------
                       Run update_authentication() in console to store credentials."))
    }
  }
}

get_new_credentials <- function() {
  cid <- readline(prompt="Enter ESO company ID: ")
  username <- readline(prompt="Enter ESO username: ")
  password <- askpass::askpass(prompt="Enter your ESO password: ")

  store_authentication(cid, username, password)
}

#' Stores authentication
#'
#' @param cid Company ID
#' @param username Username
#' @param password Password
#'
#' @return
store_authentication <- function(cid, username, password) {
  keyring::key_set_with_value("ESO_user",username, password)
  keyring::key_set_with_value("ESO_cid", cid, cid)
}

get_credential <- function(credential = c("cid","username","password")) {

  switch(credential,
         cid = {
           cred <- keyring::key_list("ESO_cid")
           cred <- paste(cred$username, collapse="")
         },
         username = {
           cred <- keyring::key_list("ESO_user")
           cred <- paste(cred$username, collapse="")
         },
         password = {
           username <- keyring::key_list("ESO_user")
           cred <- keyring::key_get("ESO_user", username=paste(username$username, collapse=""))
         }
  )
  if (str_length(cred) == 0) {
    if (interactive()) {
      update_authentication()
    } else {
      stop("No credentials found in keyring. Use update_authentication() to set credentials.")
    }
  }

  cred
}

get_session <- function() {
  username <- get_credential("username")
  password <- get_credential("password")
  agencyId <- get_credential("cid")

  s <- httr::POST(
    url = "https://www.esosuite.net/login/api/login",

    body = paste0('{"username":"',username,
                  '","password":"',password,
                  '","agencyLoginId":"',agencyId, '"}'),
    httr::add_headers(Accept = "application/json, text/plain, */*",
                `Content-Type` = "application/json"
    ))
  s
}


