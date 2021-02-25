#' Import ESO Table
#'
#' Works with consistent naming conventions to
#' simplify importing the monthly data table export.
#'
#' Requires file_root to be present in the environment.
#'
#' @param year YYYY string
#' @param month MM string OR logi FALSE.
#' @param table_name string of table name to find file.
#'
#' @return Tibble with imported data
#' @export
#'

import_eso_data <- function(year, month=FALSE, table_name) {
  if(month == FALSE) {
    read_csv(paste0(file_root, paste(year, table_name, sep="_"), ".csv", col_ty), na=character())
  } else {
    read_csv(paste0(file_root, paste(year, month, table_name, sep="_"), ".csv"), na=character())
  }
}

#' Import Patients table
#'
#' @param year String with year to select data for.
#' @param month optional string with month to select data for. FALSE will load a full year's worth of data.
#'
#' @return Tibble with imported data
#' @export
#'

import_patients <- function(year, month=FALSE) {
  if(month == FALSE) {
    path <- paste0(file_root, paste(year, "Patient_Info", sep="_"), ".csv")
  } else {
    path <- paste0(file_root, paste(year, month, "Patient_Info", sep="_"), ".csv")
  }
  read_csv(path, na=character(),
           col_types = cols(
             `Phone - Home` = col_character(),
             `Social Security Number` = col_character(),
             `Zip-Postal Code` = col_character(),
             `Phone - Cell` = col_character(),
             `Driving License Number` = col_character()
           ))
}
#' Import Incidents
#'
#' @param year String with year to select data for.
#' @param month optional string with month to select data for. FALSE will load a full year's worth of data.
#'
#' @return Tibble with imported data, dtDate, month fields
#' @export
#'

import_incidents <- function(year, month=FALSE) {

  if(month == FALSE) {
    file_name <- paste(year, "Incidents", sep="_")
  } else {
    file_name <- paste(year, month, "Incidents", sep="_")
  }
  incidents <- read_csv(paste0(file_root, file_name, ".csv"),
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
  incidents

}

#' Import Vitals
#'
#' @param year String with year to select data for.
#' @param month optional string with month to select data for. FALSE will load a full year's worth of data.
#'
#' @return Tibble with imported data, vsDate
#' @export
#'

import_vitals <- function(year, month=FALSE) {

  if(month == FALSE) {
    file_name <- paste(year, "Vitals+", sep="_")
  } else {
    file_name <- paste(year, month, "Vitals+", sep="_")
  }
  vitals <- read_csv(paste0(file_root, file_name, ".csv"),
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

#' Import complete treatment set
#'
#' @param year String with year to select data for.
#' @param month optional string with month to select data for. FALSE will load a full year's worth of data.
#'
#' @return tibble with treatments, dtTreatment
#' @export
#'

import_treatments <- function(year, month=FALSE) {
  if(month == FALSE) {
    file_pre <- year
  } else {
    file_pre <- paste(year, month, sep="_")
  }

  treatments <-  read_csv(paste0(file_root, file_pre, "_General.csv"),
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
      read_csv(paste0(file_root, file_pre, "_", t, ".csv"),
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

#' Imports Stroke Form Data
#'
#' @param year String with year to select data for.
#' @param month optional string with month to select data for. FALSE will load a full year's worth of data.
#'
#' @return Tibble with stroke form data for selected year and month.
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


#' Import Narrative
#'
#' @param year String with year to select data for.
#' @param month optional string with month to select data for. FALSE will load a full year's worth of data.
#'
#' @return tibble of imported data
#' @export
#'

import_narrative <- function(year, month=FALSE) {
  if(month == FALSE) {
    file_name <- paste(year, "Narrative", sep="_")
  } else {
    file_name <- paste(year, month, "Narrative", sep="_")
  }
  narrative <- read_csv(paste0(file_root, file_name, ".csv"),
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

#' Imports CPR table
#'
#' @param year String with year to select data for.
#' @param month optional string with month to select data for. FALSE will load a full year's worth of data.
#'
#' @return tibble with CPR data
#' @export
#'
import_cpr <- function(year, month=FALSE) {

  if(month == FALSE) {
    file_name <- paste(year, "CPR", sep="_")
  } else {
    file_name <- paste(year, month, "CPR", sep="_")
  }
  cpr <- read_csv(paste0(file_root, file_name, ".csv"), col_types = cols(
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
