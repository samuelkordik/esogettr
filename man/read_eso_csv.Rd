% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read_eso_files.R
\name{read_eso_csv}
\alias{read_eso_csv}
\title{Read ESO CSV File}
\usage{
read_eso_csv(year, month = FALSE, table_name, ...)
}
\arguments{
\item{year}{YYYY formatted string}

\item{month}{MM formatted string or FALSE to omit}

\item{table_name}{standardized name of table}

\item{...}{Additional arguments to pass on to read_csv.}
}
\value{
tibble with file contents
}
\description{
Standardized code to read_csv the file in based on
year, month, table_name
}
