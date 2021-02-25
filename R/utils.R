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

# uio
#
# uio is a wrapper function that provides the
#   ability to interact with the user. If not
#   in interactive mode, this function stops
#   execution as a protective measure.
#
# `output` is the text displayed (output) to
#   the user.
# `wait` specifies whether or not the script
#   should pause for input. Defaults to FALSE.
#
# Returns either TRUE (if `wait` is FALSE) or
#   the value entered by the user.

uio <- function(output, wait = FALSE) {
  if(interactive()) {
    if(wait) {
      cat(output)
      inp <- readline(prompt="Type an option to continue:"); inp
    } else {
      write(output, stdout())
    }
  } else {
    stop()
  }
}

# freq_table
#
# freq_table reports frequencies, percentages, and totals for a tibble
# based on grouping columns.
#
# Arguments:
# .data       tibble to use.
# group_var   vector of columns to group by

freq_table <- function(.data, group_var, summary=TRUE) {

  totals <- .data %>% tally() %>%
    summarise(n=sum(n)) %>% mutate(pct=100) %>%
    mutate(!!group_var[1] := "**Total**")

  if(summary==TRUE) {
    bind_rows(
      .data %>% group_by_at(group_var) %>% tally() %>%
        mutate(pct=round((n/totals[[1]])*100,2)),
      totals
    )
  } else {
    .data %>% group_by_at(group_var) %>% tally() %>%
      mutate(pct=round((n/totals[[1]])*100,2))
  }
}
