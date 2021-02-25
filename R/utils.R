#' set_year
#'
#' Returns the year of the previous month OR allows user to enter year.
#' @return YYYY formatted string.
#' @export
#'
#' @examples
set_year <- function() {
  a <- year(today() - months(1))
  b <- uio(paste0("Year is set as '", a, "'. Press Enter to continue, or type in an alternate entry."), wait=TRUE)
  ifelse(b == "", a, b)
}

#' Sets month
#'
#' Returns the month of the previous month or allows user to enter month.
#' @return MM formatted string.
#' @export
#'
#' @examples
set_month <- function() {
  a <- sprintf("%02d",month(today() - months(1)))
  b <- uio(paste0("Month is set as '", a, "'. Press Enter to continue, or type in an alternate entry."), wait=TRUE)
  ifelse(b == "", a, b)
}
