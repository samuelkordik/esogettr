
<!-- README.md is generated from README.Rmd. Please edit that file -->

# groverr

<!-- badges: start -->

<!-- badges: end -->

The goal of groverr is to make it easy to import data from ESO sources,
CAD, and manipulate it.

## Installation

How to
import:

``` r
install_github("samuelkordik/groverr", auth_token="35476994d9ed123dc5722c5557c960b8bf7874c8")
```

## Data Sources

### ESO Data

Groverr expects ESO data to be downloaded from Ad Hoc to a single
directory with the following specifications:

  - Flat file CSV

  - Name: Either “YYYY-MM{table\_name}” or “YYYY\_{table\_name}” with
    the table names following a pattern that isn’t necessarily logical.

  - 
## Getting Started

Importing ESO data:

``` r
library(groverr)
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

Note that you have to set file\_root first, as the path to the ESO data
may vary based on machine, OS, or network. Years and months are passed
as strings, and serve as the filter to get into the right flat file. I
know, it’s clunky.

Custom import functions ensure consistent typing, and more:

  - import\_patients
  - import\_incidents: adds `dtDate` for incident and `month`, which is
    YYYY-MM format
  - import\_vitals: adds `vsDate` as date time field
  - import\_treatments: Combines General, Airway, Cardiac, Medications,
    and IV treatments (but strangely not labs). Also adds treatmentDate
    datetime field.
  - import\_stroke: Handles some unknown stuff.
  - import\_narrative
  - import\_cpr: handles some things. TODO here.
  - import\_all\_data (imports all years/months/etc.) **Garbage
    function**
  - filtered\_incidents: Uses an existing data set to filter incidents
    for import. TODO here with example.

## ESO Data Transformations

  - join\_crew: Adds Lead, Driver fields.
  - cardiac\_arrest: Adds OHCA field. **Deprecate in favor of
    is\_cardiac\_arrest**.
  - rsi: Adds field for RSI meds or not
  - rsi\_onscene
  - blood

## Importing CAD

  - get\_cad\_incidents(server, database, UID, PWD, agency,
    tzone=“US/Central”)

## Utilities

  - add\_op\_periods: Can sort CAD incidents or any thing with a
    DispatchedTime field into operational periods.
  - merge\_eso\_data: Useful to merge multiple files downloaded from Ad
    Hoc together. Key for year-long or larger datasets that run into row
    length limitations.

## Notes

FILE
ROOTS:

``` r
#file_root <- "C:/Users/skordik/OneDrive - Cypress Creek Emergency Medical Services/March Data/"
#file_root <- "/Users/samuelkordik/OneDrive - Cypress Creek Emergency Medical Services/March Data/"
#file_root <- "W:/Monthly ESO Data/"
```
