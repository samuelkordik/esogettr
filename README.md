
<!-- README.md is generated from README.Rmd. Please edit that file -->

# esogettr

<!-- badges: start -->
<!-- badges: end -->

The goal of esogettr is to make it easy to import data from ESO EHR data
sources and manipulate it for use in data analytics, modeling, and
reporting purposes.

## Installation

How to import:

``` r
install_github("samuelkordik/esogettr")
```

## Data Sources

### ESO Data

esogettr expects ESO data to be downloaded from Ad Hoc to a single
directory with the following specifications:

-   Flat file CSV
-   Name: Either “YYYY-MM{table_name}” or “YYYY\_{table_name}” with the
    table names following a pattern that isn’t necessarily logical.
-   

## Getting Started

Importing ESO data:

``` r
library(esogettr)
## set file_root
file_root <- get_file_root()

## set year and month
year <- set_year()
month <- set_month()

## import incidents (general function)
incidents <- import_eso_data(year, month, "incidents")

## import incidents (specific function)
incidents <- import_incidents(year, month)
```

Note that you have to set file_root first, as the path to the ESO data
may vary based on machine, OS, or network. Years and months are passed
as strings, and serve as the filter to get into the right flat file. I
know, it’s clunky.

Custom import functions ensure consistent typing, and more:

-   import_patients
-   import_incidents: adds `dtDate` for incident and `month`, which is
    YYYY-MM format
-   import_vitals: adds `vsDate` as date time field
-   import_treatments: Combines General, Airway, Cardiac, Medications,
    and IV treatments (but strangely not labs). Also adds treatmentDate
    datetime field.
-   import_stroke: Handles some unknown stuff.
-   import_narrative
-   import_cpr: handles some things. TODO here.
-   import_all_data (imports all years/months/etc.) **Garbage function**
-   filtered_incidents: Uses an existing data set to filter incidents
    for import. TODO here with example.

## ESO Data Transformations

-   join_crew: Adds Lead, Driver fields.
-   cardiac_arrest: Adds OHCA field. **Deprecate in favor of
    is_cardiac_arrest**.
-   rsi: Adds field for RSI meds or not
-   rsi_onscene
-   blood

## Utilities

-   merge_eso_data: Useful to merge multiple files downloaded from Ad
    Hoc together. Key for year-long or larger datasets that run into row
    length limitations.

## Notes

FILE ROOTS:

``` r
#file_root <- "C:/Users/skordik/Data/"
#file_root <- "/Users/samuelkordik/Data/"
#file_root <- "W:/Monthly ESO Data/"
```
