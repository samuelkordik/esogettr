#' Read ESO CSV File
#'
#' Standardized code to read_csv the file in based on
#' year, month, table_name
#'
#' @param year YYYY formatted string
#' @param month MM formatted string or FALSE to omit
#' @param table_name standardized name of table
#' @param ... Additional arguments to pass on to read_csv.
#'
#' @return tibble with file contents
#'
read_eso_csv <- function(year, month=FALSE, table_name,...) {

  # Check if file_root has been defined and if not, then set it
  if(!exists("file_root")) {
    file_root <<- get_file_root()
  }

  file_name <- generate_filename(year, month, table_name)
  path <- paste0(file_root, file_name)




  while (!is_file(path)) {
    # Handle missing file
    if(interactive()) {
      menu <- glue("File '{file_name}' not found. Select an option:
                   (y) Change year
                   (m) Change month
                   (t) Change table_name
                   (r) Change file_root
                   (x) Exit")

      switch(uio(menu, accept=c("y","m","t","r","x"), wait = TRUE),
             y = {
               year <- set_year()
              },
             m= {
               month <- set_month()
             },
             t = {
               table_name <- readline(prompt="Enter new table name:")
             },
             r = {
               file_root <<- get_file_root()
             },
             x = {
               on.exit("Goodbye!",add=TRUE)
               stop(paste("File not found:",path))
             })
      file_name <- generate_filename(year, month, table_name)
      path <- paste0(file_root, file_name)
    } else {
      stop(paste("File not found:",path))
    }
  }

  message(glue("Importing from {path}"))
  read_csv(path, ...)
}


#' Generates a file name to be used to get a path
#'
#' @param year YYYY formatted string
#' @param month MM formatted string or FALSE to omit
#' @param table_name standardized name of table
#'
#' @return filename
generate_filename <- function(year, month=FALSE,table_name) {
  if(month == FALSE) {
    file_name <- paste0(paste(year, table_name, sep="_"),".csv")
  } else {
    file_name <- paste0(paste(year, month, table_name, sep="_"),".csv")
  }
  file_name
}
