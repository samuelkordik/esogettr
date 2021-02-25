#' Imports a Filtered Incident Set
#' Imports incidents table filtered against the filter_set
#' passed to it. Filter Set has to have PatientCareRecordId index.
#'
#' @param year
#' @param month
#' @param filter_set Tibble of ESO data
#' @export
#'
#' @examples
filtered_incidents <- function(year, month, filter_set) {
  import_eso_data(year, month, "Incidents") %>% filter(PatientCareRecordId %in% filter_set$PatientCareRecordId)
}

#' Add Crew to ESO table
#'
#' Adds a column to any ESO data table for Lead and Driver, using Last, First.
#'
#' @param .data ESO data table (needs to have PatientCareRecordId index)
#' @param year
#' @param month
#'
#' @return ESO data table + Lead, Driver column
#' @export
#'
#' @examples
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
#' @param year
#' @param month
#'
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
#' @param data
#' @param year
#' @param month
#'
#' @return
#' @export
#'
#' @examples
is_cardiac_arrest <- function(data, year, month=FALSE) {
  cardiac_arrest(data, year, month)
}

# rsi
#
# Transforms incidents table to include a
# field "RSI" indicating if the patient
# received RSI meds or not.
rsi <- function(the_incidents, year, month=FALSE) {
  rsi_meds <- import_eso_data(year, month, "Medications") %>% filter(`Treatment Name` %in% c("Rocuronium", "Succinylcholine", "Etomidate", "Pancuronium"))

  #mutate data directly
  the_incidents %>% mutate(RSI = if_else(PatientCareRecordId %in% rsi_meds$PatientCareRecordId, "RSI", "Not RSI"))
}

# rsi_onscene
#
# Transforms incidents table to include a
# field "RSI" indicating if the patient
# received RSI meds or not prior to departing scene.

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

# blood
#
# Transforms incidents table to include a
# field "Blood" indicating if the patient
# received blood products or not.

blood <- function(the_incidents, year, month=FALSE) {
  blood_meds <- import_eso_data(year, month, "Medications") %>% filter(`Treatment Name` %in% c('Fresh Frozen Plasma (FFP)', 'Packed Red Blood Cells (PRBC)', 'Whole Blood'))
  #mutate data directly
  the_incidents %>% mutate(Blood = if_else(PatientCareRecordId %in% blood_meds$PatientCareRecordId, "Gave blood products", "Did not give blood products"))
}
