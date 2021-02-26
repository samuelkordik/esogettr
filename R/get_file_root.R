#' Gets the file directory for ESO data.
#' Generates the file path for ESO data based on the
#' local system; if in interactive mode gives option
#' to adjust if needed.
#'
#' @param override_interactive whether or not to force default value
#'
#' @return file_root path (as string)
#' @export
#'
get_file_root <- function(override_interactive = FALSE) {
  # system switching inspired by clipr package.
  sys_type <- Sys.info()["sysname"]
  user_name <- Sys.info()["user"]

  file_root <- switch(sys_type,
                      "Darwin" = paste0("/Users/",user_name,"/OneDrive - Cypress Creek Emergency Medical Services/March Data/"),
                      "Windows" = windows_file_root(user_name, override_interactive))

  while(!dir_exists(file_root)) {
    if(interactive() & !override_interactive & sys_type == "Windows") {
      # Give option to handle wrong file root. Only applies on windows.
      file_root <- windows_file_root(user_name, override_interactive)
    } else {
      stop(glue("Invalid file root specified: {file_root}"))
    }
  }

  message(paste("Using",file_root,"for data source."))
  return(file_root)
}

#' Get file root on Windows
#' Gives interactive options for choosing differently.
#'
#' @param user_name system user name
#' @param override_interactive inherit from above
#'
#' @return file_root path (as string)
windows_file_root <- function(user_name, override_interactive = FALSE) {

  # default file root
  file_root <- paste0("C:/Users/",user_name,"/OneDrive - Cypress Creek Emergency Medical Services/March Data/")

  # Check to see if interactive


  if(interactive()) {
    if(override_interactive == FALSE) {

      menu <- "Where is your data located?:
    ======================
    (w) for W:/Monthly ESO Data/
    (a) to enter your own.
    (u) or [enter] to use default.
    ======================
    Type an option to continue:"
      inp <- readline(prompt=menu)
      switch(inp,
             w = {
               file_root <- "W:/Monthly ESO Data/"
             },
             a = {
               file_root <- readline(prompt="Enter your data location:")
             },
             ... = {
             }
      )
    }
  }


  return(file_root)
}
