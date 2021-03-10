#' Imports a Filtered Incident Set
#'
#' Imports the Incidents table for a given year and month
#' but only includes patient care records that match
#' \code{`PatientCareRecordId`} in the \code{filter_set}.
#'
#' @inheritParams import_eso_data
#' @param filter_set Tibble of ESO data
#' @param .left_join optional argument that does a left join to add the filtered set to incidents.
#' @export
#'
filtered_incidents <- function(year, month, filter_set, .left_join=FALSE) {
  incidents <- import_eso_data(year, month, "Incidents") %>% filter(PatientCareRecordId %in% filter_set$PatientCareRecordId)
  if (.left_join) {
    incidents <- left_join(incidents, filter_set, by="PatientCareRecordId")
  }
  return(incidents)
}

#' Add Crew to ESO table
#'
#' Adds a column to any ESO data table for Lead and Driver, using Last, First.
#'
#' @param .data ESO data table (needs to have PatientCareRecordId index)
#' @inheritParams import_eso_data
#'
#' @return ESO data table + Lead, Driver column
#' @export
#'
join_crew <- function(.data, year, month=FALSE) {
  personnel <- import_eso_data(year, month, "Personnel") %>% unite(full_name, `Crew Member Last Name`, `Crew Member First Name`, sep=", ") %>% dplyr::select(PatientCareRecordId, full_name, `Crew Member Role`) %>% filter(`Crew Member Role` %in% c("Lead", "Driver"))
  personnel <- personnel %>% unique() %>% spread(key=`Crew Member Role`, value=full_name)
  .data %>% left_join(personnel, by="PatientCareRecordId")
}

#' Adds OHCA field to Incidents Table
#' Transforms incidents table to include a
#' field "OHCA" indicating if the patient
#' was a cardiac arrest or not.
#'
#' @param the_incidents Incidents table
#' @inheritParams import_eso_data
#' @return Incidents + OHCA field
#' @export
cardiac_arrest <- function(the_incidents, year, month=FALSE) {
  cpr <- import_eso_data(year, month, "CPR")

  #filter cpr by data
  cpr <- cpr %>% right_join(the_incidents, by="PatientCareRecordId")

  #mutate
  cpr <- cpr %>% mutate(OHCA = if_else(`Cardiac Arrest` %in% c("Yes, After EMS Arrival", "Yes, Prior to EMS Arrival"), "Yes", if_else(`Cardiac Arrest`=="No", "No", "No", missing="No"), missing="No")) %>% dplyr::select(PatientCareRecordId, OHCA)

  #Return join new column
  the_incidents %>% left_join(cpr, by="PatientCareRecordId")
}

#' Wrapper for Cardiac Arrest function
#'
#' @param data incident table
#' @inheritParams import_eso_data
#'
#' @return tibble with OHCA field
#' @export
#'
is_cardiac_arrest <- function(data, year, month=FALSE) {
  cardiac_arrest(data, year, month)
}

#' Did the patient get RSI meds?
#'
#' @param the_incidents table of incidents
#' @inheritParams import_eso_data
#' @return tibble with RSI column added
#' @export
#'
rsi <- function(the_incidents, year, month=FALSE) {
  rsi_meds <- import_eso_data(year, month, "Medications") %>% filter(`Treatment Name` %in% c("Rocuronium", "Succinylcholine", "Etomidate", "Pancuronium"))

  #mutate data directly
  the_incidents %>% mutate(RSI = if_else(PatientCareRecordId %in% rsi_meds$PatientCareRecordId, "RSI", "Not RSI"))
}


#' DId the patient get RSI meds before leaving scene?
#'
#' @param the_incidents tibble of incidents
#' @inheritParams import_eso_data
#'
#' @return tibble iith RSI_onscene
#' @export
#'
rsi_onscene <- function(the_incidents, year, month=FALSE) {
  rsi_meds <- import_eso_data(year, month, "Medications") %>% filter(`Treatment Name` %in% c("Rocuronium", "Succinylcholine", "Etomidate", "Pancuronium")) %>% dplyr::select(PatientCareRecordId, `Treatment Name`, `Treatment Date`)

  # add incidents in and calculate time fields
  rsi_meds <- rsi_meds %>% left_join(the_incidents, by="PatientCareRecordId")
  rsi_meds <- rsi_meds %>% mutate(med_time = mdy_hms(`Treatment Date`), depart_scene = if_else(`Transferred Unit` == "EMS Provider (Air)", mdy_hm(`Transfer of Patient Care Time`), mdy_hm(`Depart Scene Time`), missing=mdy_hm(`Depart Scene Time`)))

  # filter for only time fields that count
  rsi_meds <- rsi_meds %>% filter(med_time < depart_scene)

  #mutate data directly
  the_incidents %>% mutate(RSI_onscene = if_else(PatientCareRecordId %in% rsi_meds$PatientCareRecordId, "RSI on scene", "No RSI on scene"))
}


#' Blood
#'
#' Transforms incidents table to include a field "Blood" indicating if the patient received blood products or not.
#'
#' @param the_incidents tibble of incidents
#' @inheritParams import_eso_data
#' @return tibble with Blood field
#' @export
#'
blood <- function(the_incidents, year, month=FALSE) {
  blood_meds <- import_eso_data(year, month, "Medications") %>% filter(`Treatment Name` %in% c('Fresh Frozen Plasma (FFP)', 'Packed Red Blood Cells (PRBC)', 'Whole Blood'))
  #mutate data directly
  the_incidents %>% mutate(Blood = if_else(PatientCareRecordId %in% blood_meds$PatientCareRecordId, "Gave blood products", "Did not give blood products"))
}
