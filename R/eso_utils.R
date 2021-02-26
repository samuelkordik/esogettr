#' Merges two or more files together
#'
#' Useful when Ad Hoc limits return row length and you end up with A, B, etc. files
#'
#'
#' @param year YYYY year to look for
#' @param month MM month (or FALSE) to look for
#' @param table_name Table name to look for
#' @param ... Arguments to pass onto read_csv for import.
#'
#' @return dataset
#' @export
#'
#' @examples
merge_eso_data <- function(year, month=FALSE,table_name,...) {
  if(!exists("file_root")) {
    file_root <<- get_file_root()
  }

  if(month == FALSE) {
    glob <- paste(year, table_name, sep="_")
  } else {
    glob <- paste(year, month, table_name, sep="_")
  }

  matched_files <- dir_ls(file_root, glob=paste0("*",glob,"*"))
  file_list <- paste0(seq_along(matched_files), ": ", path_file(matched_files))
  file_list_c <- paste(file_list, collapse = "\n")

  switch(uio(glue("Choose file(s) to merge. Separate multiple numbers with a comma.
  a: All files
  0: No files
  {file_list_c}"),
      wait=TRUE),
      "0" = {
        message("No files selected")
        return(NULL)
      },
      a = {
        matched_files <- matched_files
        message("Merging all selected")
      },
      ...={
        selectors <- as.numeric(strsplit("1", split=",")[[1]])
        matched_files <- matched_files[selectors]
        message(paste(na.omit(c("Merging files:", file_list[selectors])),collapse="\n"))
      })

  df <- read_csv(matched_files[1],...)

  if(length(matched_files) > 1) {
    for (i in 2:length(matched_files)) {
      df <- bind_rows(df, read_csv(matched_files[i]),...)
    }
  }

  message(glue("Saving merged file as {glob}.csv"))
  write_csv(df, paste0(file_root, glob, ".csv"))
  return(df)

}
