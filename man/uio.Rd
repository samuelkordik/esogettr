% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{uio}
\alias{uio}
\title{User Input/Output}
\usage{
uio(output, accept = NULL, wait = FALSE)
}
\arguments{
\item{output}{is the text displayed (output) tothe user.}

\item{accept}{list of acceptable responses.}

\item{wait}{specifies whether or not the script should pause for input. Defaults to FALSE.}
}
\value{
TRUE (if wait is FALSE) or value entered by the user.
}
\description{
uio is a wrapper function that provides the
ability to interact with the user. If not
in interactive mode, this function stops
execution as a protective measure.
}
