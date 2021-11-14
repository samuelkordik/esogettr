#' Import ESO Table
#'
#' Works with consistent naming conventions to
#' simplify importing the monthly data table export.
#' See `vignette("importing-pcrs")` for more information.
#'
#' Requires file_root to be present in the environment.
#'
#' @param year Year of data set as "YYYY" string
#' @param month Month of data set as "MM" string, or FALSE to look for full year.
#' @param table_name string of table name to find file.
#'
#' @return Tibble with imported data
#' @export
#'
#' @examples
#' import_eso_data("2020", "02", "Personnel") # Imports one month
#' import_eso_data("2020", FALSE, "Personnel") # Imports an entire year

import_eso_data <- function(year, month=FALSE, table_name) {
  read_eso_csv(year, month, table_name)
}

#' @describeIn import_eso_data Imports the crew information table.
#' @export

import_personnel <- function(year, month=FALSE) {
  read_eso_csv(year, month, "Personnel", na=character(),
                           col_types = cols(
                             PatientCareRecordId = col_character(),
                             `Crew Member First Name` = col_character(),
                             `Crew Member Last Name` = col_character(),
                             `Crew Member Role` = col_character(),
                             `Crew Member Id` = col_character(),
                             `Crew Member Level` = col_character()
                           ))
}


#' @describeIn import_eso_data Imports the patient information table and adds calculated age and age range.
#' @export

import_patients <- function(year, month=FALSE) {
  patients <- read_eso_csv(year, month, "Patient_Info", na=character(),
           col_types = cols(
             `Phone - Home` = col_character(),
             `Social Security Number` = col_character(),
             `Zip-Postal Code` = col_character(),
             `Phone - Cell` = col_character(),
             `Driving License Number` = col_character(),
             `Physician Middle Name` = col_character()
           ))
  patients %>% mutate(`Patient DOB` = mdy_hms(`Patient DOB`),
                      age_years = convert_estimated_age(`Patient Age`),
                      age_range = get_age_range(`Patient Age`)
                      )
}


#' Convert estimated age to decimal age in years
#'
#' @param patient_age Age in string
#'
#' @return patient age in years, as double
convert_estimated_age <- function(patient_age) {
  ages <- str_match(patient_age,
                    "(\\d+) Years, (\\d+) Months, (\\d*) Days")
  ages[,2] <- as.numeric(ages[,2]) # Years
  ages[,3] <- as.numeric(ages[,3]) # Months
  ages[,4] <- as.numeric(ages[,4]) # Days
  as.numeric(ages[,2]) + as.numeric(ages[,3])/12 + as.numeric(ages[,4])/365
}

#' Takes age in years and returns the age range
#'
#' Ages as follows:
#' Newborn: < 24 hours
#' Neonate: 1 day - 1 month
#' Infant: 1 month - 1 year
#' Toddler: 1-3 years
#' Preschooler: 3-6 years
#' School Ager: 6-12 years
#' Adolescent: 12-18 years
#' Adult: 18-64 years
#' Geriatric: 64 years and older
#'
#' @param patient_age Age in string
#'
#' @return age range definition as string
get_age_range <- function(patient_age) {
  ages <- str_match(patient_age,
                    "(\\d+) Years, (\\d+) Months, (\\d*) Days")
  age_years <- as.numeric(ages[,2]) # Years
  age_months <- as.numeric(ages[,3]) # Months
  age_days <- as.numeric(ages[,4]) # Days

  case_when(
    age_years == 0 & age_months == 0 & age_days <= 1 ~ "Newborn",
    age_years == 0 & age_months < 1 ~ "Neonate",
    age_years < 1 ~ "Infant",
    age_years < 3 ~ "Toddler",
    age_years < 6 ~ "Preschooler",
    age_years < 12 ~ "School Ager",
    age_years < 18 ~ "Adolescent",
    age_years < 64 ~ "Adult",
    age_years >= 64 ~ "Geriatric"
  )

}


#' Import Incidents with helpful additional fields added
#'
#' Uses two approaches to import: the year, month option brings
#' in incidents based on a given year and month. The start and end
#' option does so based on time.
#'
#' @param year Year of the data desired
#' @param month Month of the data desired
#' @param start Start date
#' @param end End date.
#' @param join_crew BOOL Adds columns for lead and driver crew names.
#'
#' @return
#' @export
#'
import_incidents <- function(year=NULL, month=NULL, start=NULL, end=NULL, join_crew=NULL) {
  if (missing(year)) {
    if (missing(start)) stop("Missing year and start dates. Must specify one or the other.")
    if (missing(end)) stop("Missing year and end dates. Must specify one or the other.")

    incidents <- get_eso_by_date(start, end, "Incidents", join_crew)

  } else {
    stopifnot(typeof(year) == "character")
    if (missing(month)) {
      month <- FALSE
    } else {
      #stopifnot(typeof(month) == "character")
    }

    incidents <- import_incidents_by_file(year, month, join_crew)

  }

incidents

}


import_incidents_by_file <- function(year, month=FALSE, join_crew = NULL) {
  if (missing(join_crew)) join_crew <- FALSE

  incidents <- read_eso_csv(year, month, "Incidents",
                        col_types = cols(
                          PatientCareRecordId = col_character(),
                          `Incident Date` = col_character(),
                          `Incident Number` = col_character(),
                          `Lock Date` = col_character(),
                          `Sync Date` = col_character(),
                          `First Lock Date` = col_character(),
                          `Run Type` = col_character(),
                          Priority = col_character(),
                          `Call Received Time` = col_character(),
                          `Call Received Time in second` = col_integer(),
                          `Dispatched Time` = col_character(),
                          `Dispatched Time in second` = col_integer(),
                          `En Route Time` = col_character(),
                          `En Route Time in second` = col_integer(),
                          `On Scene Time` = col_character(),
                          `On Scene Time in second` = col_integer(),
                          `At Patient Time` = col_character(),
                          `At Patient Time in second` = col_integer(),
                          `Depart Scene Time` = col_character(),
                          `Depart Scene Time in second` = col_integer(),
                          `At Destination Time` = col_character(),
                          `At Destination Time in second` = col_integer(),
                          `Incident Closed Time` = col_character(),
                          `Incident Closed Time in second` = col_integer(),
                          `PSAP Call Time` = col_character(),
                          `PSAP Call Time in second` = col_integer(),
                          `Dispatch Notified Time` = col_character(),
                          `Dispatch Notified Time in second` = col_integer(),
                          `Transfer of Patient Care Time` = col_character(),
                          `Transfer of Patient Care Time in second` = col_integer(),
                          `Scene Mileage` = col_double(),
                          `Destination Mileage` = col_double(),
                          `Loaded Mileage` = col_double(),
                          `Mileage Start` = col_double(),
                          `Mileage End` = col_double(),
                          `Total Mileage` = col_double(),
                          `Type of Response Delay` = col_character(),
                          `Type of Scene Delay` = col_character(),
                          `Type of Transport Delay` = col_character(),
                          `Type of Turn Around Delay` = col_character(),
                          `Type of Dispatch Delay` = col_character(),
                          `Location Type` = col_character(),
                          `Location Name` = col_character(),
                          Address1 = col_character(),
                          Address2 = col_character(),
                          City = col_character(),
                          State = col_character(),
                          County = col_character(),
                          `ZIP Postal` = col_character(),
                          Disposition = col_character(),
                          `Transported To - Destination Type` = col_character(),
                          `Transported To - Destination` = col_character(),
                          `Transported To - Destination Facility Code` = col_integer(),
                          `Destination Address1` = col_character(),
                          `Destination Address2` = col_character(),
                          `Destination City` = col_character(),
                          `Destination State` = col_character(),
                          `Destination County` = col_character(),
                          `Destination ZIP Postal` = col_character(),
                          Unit = col_character(),
                          `Predefined Location Type` = col_character(),
                          `Predefined Location` = col_character(),
                          Shift = col_character(),
                          Vehicle = col_character(),
                          `Level of Service` = col_character(),
                          `Transferred To` = col_character(),
                          `Transferred Unit` = col_character(),
                          `Complaint Reported by Dispatch` = col_character(),
                          `EMD Card Number` = col_character(),
                          `Scene GPS Location Latitude` = col_double(),
                          `Scene GPS Location Longitude` = col_double(),
                          `Triage Priority` = col_character(),
                          `EMD Complaint` = col_character(),
                          Department = col_character(),
                          `Hospital Designation` = col_character(),
                          `Is First Unit On Scene` = col_character(),
                          `Transport Mode` = col_character(),
                          `Transport Method` = col_character()
                        )
  )
  incidents <- incidents %>% mutate(dtDate = mdy_hms(`Incident Date`, truncated=1),
                                    month = paste(year(dtDate), sprintf("%02d", month(dtDate)), sep="-")
  )

  if (join_crew == TRUE) {
    incidents <- join_crew(incidents, year, month)
  }

  incidents

}

import_incidents_by_time <- function(start, end)


#' @describeIn import_eso_data Imports vital sign records
#' @export
#'
import_vitals <- function(year, month=FALSE) {
  vitals <- read_eso_csv(year, month, "Vitals+",
                     col_types = cols(
                       .default = col_character(),
                       `BP Systolic` = col_integer(),
                       `BP Diastolic` = col_integer(),
                       `Pulse Rate` = col_integer(),
                       `Respiration Rate` = col_integer(),
                       SpO2 = col_integer(),
                       EtCO2 = col_integer(),
                       `Glucose level` = col_integer(),
                       `Temperature F` = col_integer(),
                       `Pain Scale  0-10` = col_integer(),
                       `Glasgow Scale Total Score` = col_integer(),
                       `Revised Trauma GCS` = col_integer(),
                       `Revised Trauma BP` = col_integer(),
                       `Revised Trauma RR` = col_integer(),
                       `Revised Trauma Total Score` = col_integer(),
                       `Pediatric Trauma Total Score` = col_integer()
                     )
  )
  vitals <- vitals %>% mutate(vsDate = mdy_hms(`VitalSign Date Time`, truncated=1))
  vitals

}

#' @describeIn import_eso_data Imports complete treatment set
#' @export
#'

import_treatments <- function(year, month=FALSE) {

  treatments <- read_eso_csv(year, month, "General",
                             col_types =   cols(
                               .default = col_character(),
                               `Placed At` = col_character(),
                               Rate = col_integer(),
                               PEEP = col_integer(),
                               Vt = col_integer(),
                               FiO2 = col_integer(),
                               PIP = col_integer(),
                               FlowRate = col_double(),
                               Amount = col_integer(),
                               Pressure = col_integer()
                             )
  ) %>% mutate(category="General")

  for(t in c("Airway", "Cardiac", "Medications", "IV")) {
    treatments <- bind_rows(
      treatments,
      read_eso_csv(year, month, t,
               col_types =   cols(
                 .default = col_character(),
                 `Placed At` = col_character(),
                 Rate = col_integer(),
                 PEEP = col_integer(),
                 Vt = col_integer(),
                 FiO2 = col_integer(),
                 PIP = col_integer(),
                 FlowRate = col_double(),
                 Amount = col_integer(),
                 Pressure = col_integer()
               )
      ) %>% mutate(category = t)
    )
  }

  treatments %>% filter(!is.na(`Treatment Name`)) %>% mutate(treatmentDate = mdy_hms(`Treatment Date`))

}

#' @describeIn import_eso_data Imports Stroke Form Data
#' @export
#'

import_stroke <- function(year, month=FALSE) {

  if(month == FALSE) {
    file_name <- paste(year, "Stroke", sep="_")
  } else {
    file_name <- paste(year, month, "Stroke", sep="_")
  }
  strokes <- read_csv(paste0(file_root, file_name, ".csv"))
  strokes %>% mutate_all(funs(na_if(str_to_upper(.),"UNKNOWN"))) %>% mutate_all(funs(na_if(.,"UNK")))
  strokes
}


#' @describeIn import_eso_data Import Narrative
#'
#' @return tibble of imported data
#' @export
#'

import_narrative <- function(year, month=FALSE) {

  narrative <- read_eso_csv(year, month, "Narrative",
                        col_types = cols(
                          PatientCareRecordId = col_character(),
                          `Primary Impression` = col_character(),
                          `Secondary Impression` = col_character(),
                          `Chief Complaint` = col_character(),
                          `Chief Complaint Duration` = col_double(),
                          `Time Units of Chief Complaint` = col_character(),
                          `Chief Narrative` = col_character(),
                          `Secondary Complaint` = col_character(),
                          `Secondary Complain Duration` = col_logical(),
                          `Time Units Of Secondary Complaint` = col_logical(),
                          `Secondary Narrative` = col_logical(),
                          `Support Primary 1` = col_character(),
                          `Support Sign 1` = col_character(),
                          `Other Comments 1` = col_character(),
                          `Support Primary 2` = col_character(),
                          `Support Sign 2` = col_character(),
                          `Other Comments 2` = col_character(),
                          `Support Primary 3` = col_character(),
                          `Support Sign 3` = col_character(),
                          `Other Comments 3` = col_character(),
                          `Support Primary 4` = col_character(),
                          `Support Sign 4` = col_character(),
                          `Other Comments 4` = col_character(),
                          `Support Primary 5` = col_character(),
                          `Support Sign 5` = col_character(),
                          `Other Comments 5` = col_character(),
                          Injured = col_character(),
                          Pregnant = col_character(),
                          `Injury Primary` = col_character(),
                          `Injury Detail` = col_character(),
                          `Injury Place` = col_character(),
                          `Injury Date` = col_character(),
                          `Medical Trauma` = col_character(),
                          `Barriers To Care` = col_character(),
                          `Alcohol Drug Usage` = col_character(),
                          `Chief Complaint Anatomic Location` = col_logical(),
                          `Chief Complaint Organ System` = col_character(),
                          `Injury Intent` = col_character(),
                          `Height Of Fall` = col_double(),
                          `Neurological Outcome At Hospital Discharge` = col_logical(),
                          `Thrombolytic Screen` = col_logical(),
                          `Patient Moved From Scene To Ambulance Method` = col_character(),
                          `Patient Position During Transport` = col_character(),
                          `Patient Moved From Ambulance To Destination Method` = col_character(),
                          `Patient Condition At Destination` = col_character(),
                          `Protocol Used` = col_logical(),
                          `Patients Level of Distress` = col_character(),
                          `Final Patient Acuity` = col_logical()
                        )
  )
  narrative

}

#' @describeIn import_eso_data Imports CPR table
#' @export
#'
import_cpr <- function(year, month=FALSE) {

  cpr <- read_eso_csv(year, month, "CPR", col_types = cols(
    PatientCareRecordId = col_character(),
    `Cardiac Arrest` = col_character(),
    `Cardiac Arrest Etiology` = col_character(),
    `Arrest Witnessed By` = col_character(),
    `CPR Initiated By` = col_character(),
    `AED used prior to arrival` = col_character(),
    `AED used by` = col_character(),
    `Resuscitation Attempted` = col_character(),
    `Initial ECG Rhythm` = col_character(),
    ROSC = col_character(),
    `Rhythmat Destination` = col_character(),
    `Resuscitation Discontinued Time` = col_character(),
    `Field Pronouncement Expired` = col_character(),
    `Discontinuation Reason` = col_character(),
    `Field Pronouncement Time` = col_character(),
    `Field Pronouncement Physician` = col_character(),
    `Estimated Time Collapse To 911` = col_double(),
    `Estimated Time Collapse To CPR` = col_double(),
    `ROSC Time` = col_character(),
    `Pre-Arrival Instructions` = col_character(),
    `First Defibrillation By` = col_character(),
    `Time of First Defibrillation` = col_datetime(format = ""),
    `Hypothermia Provided` = col_character(),
    `End of Event` = col_character(),
    Defibrillated = col_character(),
    `Time of First CPR` = col_character(),
    `CPR Feedback` = col_character(),
    `ITD Used` = col_character(),
    `ROSC Occured` = col_character(),
    `Estimated Time of Arrest` = col_character(),
    `Resuscitation - Attempted Defib` = col_character(),
    `Resuscitation - Attempted Ventilation` = col_character(),
    `Resuscitation - Chest Compressions` = col_character(),
    `Resuscitation - Considered Futile` = col_character(),
    `Resuscitation - DNR` = col_character(),
    `Resuscitation - Signs of Circulation` = col_character()
  ))
  cpr %>% mutate_all(funs(na_if(str_to_upper(.),"UNKNOWN"))) %>% mutate_all(funs(na_if(.,"UNK"))) %>%
    mutate(`Resuscitation Discontinued Time` = mdy_hms(`Resuscitation Discontinued Time`),
           `ROSC Time` = mdy_hms(`ROSC Time`))
  cpr
}

#' import_all_data
#'
#' Imports all data in file root for a given table name.
#'
#' @param table_name Name of ESO table
#'
#' @return
#' @export
#'

import_all_data <- function(table_name) {
  files <- list.files(path = file_root, pattern = paste0("\\", table_name, ".csv$"))
  files %>% map_dfr(~ read_csv(paste0(file_root, .)))
}
