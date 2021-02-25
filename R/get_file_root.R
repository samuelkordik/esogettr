#' Gets the file directory for ESO data.
#' Generates the file path for ESO data based on the
#' local system; if in interactive mode gives option
#' to adjust if needed.
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

  message(paste("Using",file_root,"for data source."))
  return(file_root)
}
