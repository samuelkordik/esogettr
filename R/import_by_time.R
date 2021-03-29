#' Import named tables for date range
#'
#'
#'
#' @param start Y-M-D formatted start date
#' @param end Y-M-D formatted end date
#' @param tablename Tablename
#'
#' @return
#' @export
#'
#' @examples
get_eso_by_date <- function(start, end, tablename) {
  stopifnot(any(is.character(start), class(lubridate::ymd(start)) == "Date"))
  stopifnot(any(is.character(end), class(lubridate::ymd(end)) == "Date"))

  # IF start, end are character, turn into a date
  if (is.character(start)) start <- lubridate::ymd(start)
  if (is.character(end)) end <- lubridate::ymd(end)

  # Get years and loop through months
  years <- seq(from=lubridate::year(start), to=lubridate::year(end))

  # Empty list for paths to load
  load_paths <- tibble()

  for (y in years) {
    months <- seq(from = if_else(y == lubridate::year(start),
                                 lubridate::month(start),
                                 1),
                  to = if_else(y == lubridate::year(end),
                               lubridate::month(end),
                               12))
    # Look for full years
    if(!exists("file_root")) {
      file_root <<- get_file_root()
    }

    file_name <- generate_filename(y, FALSE, tablename)
    path <- paste0(file_root, file_name)

    if (fs::file_exists(path)) {
      if (exists("load_paths")) {
        load_paths <- add_row(load_paths, y = y, m = NA, tablename = tablename)
      } else {
        load_paths <- tibble_row(y = y, m = NA, tablename = tablename)
      }
    } else {
      # Load months
      for (m in months) {
        path <- paste0(file_root, generate_filename(y, sprintf("%02d",m), tablename))
        if (!fs::file_exists(path)) stop(paste("File missing:", path))
        if (exists("load_paths")) {
          load_paths <- add_row(load_paths, y = y, m = sprintf("%02d",m), tablename = tablename)
        } else {
          load_paths <- tibble_row(y = y, m = sprintf("%02d",m), tablename = tablename)
        }
      }
    }
  }

  # Load files


  pmap_dfr(list(load_paths$y, load_paths$m, load_paths$tablename),
           load_individual_data) -> out

}

load_individual_data <- function(year, month, tablename) {
  if (is.na(month)) month <- FALSE
  switch(tablename,
         Patient_Info = {
           import_patients(year, month)
         },
         Incidents = {
           import_incidents(year, month)
         },
         `Vitals+` = {
           import_vitals(year, month)
         },
         Treatments = {
           import_treatments(year, month)
         },
         Stroke = {
           import_stroke(year, month)
         },
         Narrative = {
           import_narrative(year, month)
         },
         CPR = {
           import_cpr(year, month)
         },
         {
           import_eso_data(year,  month, tablename)
         })
}

