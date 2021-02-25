#' set_year
#'
#' Returns the year of the previous month OR allows user to enter year.
#' @return YYYY formatted string.
#' @export
#'
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
set_month <- function() {
  a <- sprintf("%02d",month(today() - months(1)))
  b <- uio(paste0("Month is set as '", a, "'. Press Enter to continue, or type in an alternate entry."), wait=TRUE)
  ifelse(b == "", a, b)
}


#' User Input/Output
#'
#' uio is a wrapper function that provides the
#' ability to interact with the user. If not
#' in interactive mode, this function stops
#' execution as a protective measure.
#'
#' @param output is the text displayed (output) to
#   the user.
#' @param wait specifies whether or not the script
#   should pause for input. Defaults to FALSE.
#'
#' @return TRUE (if wait is FALSE) or value entered by the user.
#' @export
#'
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


#' freq_table
#'
#' freq_table reports frequencies, percentages, and totals for a tibble
#' based on grouping columns.
#'
#' @param .data tibble to use
#' @param group_var vector of columns to group by
#' @param summary logi whether to summarize
#'
#' @return summarized (or not) list
#' @export
#'
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

#' Add Operational Periods
#'
#' Uses DispatchedTime field in responses dataset and matches
#' against operational period intervals. Adds a column named
#' "op_period" with the value of period_name, an indice, and the
#' interval in YYYY-MM-DD HH:MM:SS format.
#'
#' @param responses Source data set with the DispatchedTime POSIXct field
#' @param startDate Date time of interval starts
#' @param endDate Date time of interval ends
#' @param period_length Length of each operational period in hours
#' @param period_name Name to insert in column
#'
#' @return responses + op_period field
#' @export
#'
#' @examples
#' \dontrun{
#' startDate <- mdy_hms("02/14/2021 18:00:00", tz="US/Central")
#' endDate <- mdy_hms("02/19/2021 18:00:00", tz="US/Central")
#' add_op_periods(responses, startDate, endDate)
#' }
#' # adds op_period column with value of (e.g.) "Operational Period 1 (2021-02-14 18:00:00-2021-02-15 06:00:00)"
#'
add_op_periods <- function(responses, startDate, endDate, period_length=12,period_name = "Operational Period") {
  op_period_intervals <- get_operational_period_intervals(startDate, endDate, period_length)
  responses <- responses %>% mutate(op_period = "")

  for (i in 1:length(op_period_intervals)) {
    responses <- responses %>% mutate(op_period = if_else(DispatchedTime %within% op_period_intervals[i], paste(period_name, i, paste0("(", int_start(op_period_intervals[i]), "-", int_end(op_period_intervals[i]),")")), op_period))

  }
  responses
}

#' Get list of operational period intervals
#'
#' @param startDate start date POSIXct
#' @param endDate end date POSIXct
#' @param period_length length in hours of interval
#'
#' @return vector sequence of intervals
#'
get_operational_period_intervals <- function(startDate, endDate, period_length = 12) {
  # Set first period as interval
  period_start <- startDate
  period_end <- startDate + hours(period_length)
  sequence_index <- 1

  op_period_intervals <- c(interval(period_start, period_end))

  # Loop through remaining periods
  while(period_end < endDate) {
    sequence_index <- sequence_index + 1
    period_start <- period_end
    period_end <- period_start + hours(period_length)
    op_period_intervals <- c(op_period_intervals, interval(period_start, period_end))
  }
  op_period_intervals
}

