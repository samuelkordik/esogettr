% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/eso_transformations.R
\name{rsi}
\alias{rsi}
\title{Did the patient get RSI meds?}
\usage{
rsi(the_incidents, year, month = FALSE)
}
\arguments{
\item{the_incidents}{table of incidents}

\item{year}{Year of data set as "YYYY" string}

\item{month}{Month of data set as "MM" string, or FALSE to look for full year.}
}
\value{
tibble with RSI column added
}
\description{
Did the patient get RSI meds?
}
