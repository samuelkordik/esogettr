% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/trauma.R
\name{get_major_trauma_patients}
\alias{get_major_trauma_patients}
\title{Get Major Trauma Patients}
\usage{
get_major_trauma_patients(start, end)
}
\arguments{
\item{start}{date to start}

\item{end}{date to end}
}
\value{
Incidents table with additions.
}
\description{
This function loads a partially deidentified trauma patient data set for given timeframe.
}
\details{
INCLUSION CRITERIA:
All transported patients within designated date range with a mechanism of trauma and one or more of the following:
\itemize{
\item (A) patients with a mechanism of trauma, transported emergently or flown.
\item (B) patients with a mechanism of trauma and unstable vital signs/physiological parameters
\item (C) patients with an anatomical injury defined in the trauma criteria. Done using a selected filter list based on prior data:
\itemize{
\item Injury Detail matches a specified list chosen using prior data.
\item Support Signs & Symptoms
\item Primary or secondary impressions
\item CDC2011 Trauma Criteria Form information
}
}

Author: Samuel Kordik, Cypress Creek EMS
Revision Date: 2021-06-02
}
