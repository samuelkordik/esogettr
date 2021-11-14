#' Get Major Trauma Patients
#'
#' This function loads a partially deidentified trauma patient data set for given timeframe.
#'
#' INCLUSION CRITERIA:
#' All transported patients within designated date range with a mechanism of trauma and one or more of the following:
#' - (A) patients with a mechanism of trauma, transported emergently or flown.
#' - (B) patients with a mechanism of trauma and unstable vital signs/physiological parameters
#' - (C) patients with an anatomical injury defined in the trauma criteria. Done using a selected filter list based on prior data:
#'     - Injury Detail matches a specified list chosen using prior data.
#'     - Support Signs & Symptoms
#'     - Primary or secondary impressions
#'     - CDC2011 Trauma Criteria Form information
#'
#' Author: Samuel Kordik, Cypress Creek EMS
#' Revision Date: 2021-06-02
#'
#' @param start date to start
#' @param end date to end
#'
#' @return Incidents table with additions.
#' @export
#'
#' @examples
get_major_trauma_patients <- function(start, end) {
  #Load libraries
  library(tidyverse)
  library(lubridate)
  library(groverr)

  # LOAD INCIDENTS & PATIENTS
  incidents <- get_eso_by_date(start, end, "Incidents") %>% filter(Disposition %in% c("Patient Treated, Transferred Care to Another EMS Professional",
                                                                                      "Transported Lights/Siren",
                                                                                      "Transported Lights/Siren, Downgraded",
                                                                                      "Transported No Lights/Siren",
                                                                                      "Transported No Lights/Siren, Upgraded"
  ))
  patients <- get_eso_by_date(start, end, "Patient_Info") %>% left_join(incidents %>% select(PatientCareRecordId, dtDate), by="PatientCareRecordId") %>%
    mutate(pt_age = case_when(age_years < 1 ~ age_years,
                              age_years > 90 ~ 90,
                              TRUE ~ round(age_years, 0))) %>% select(PatientCareRecordId, pt_age, Gender, Race, Ethnicity)
  incidents <- incidents %>% left_join(patients, by="PatientCareRecordId") %>% left_join(get_eso_by_date(start, end, "Narrative"), by="PatientCareRecordId")

  # FILTER FOR JUST TRAUMAS
  incidents <- incidents %>% mutate(trauma_mech = if_else(`Medical Trauma` != "Medical" | Injured == "true", "yes", "no"))

  # LOAD Trauma Form Content
  trauma_form <- get_eso_by_date(start, end, "Trauma")
  incidents <- incidents %>% full_join(trauma_form, by="PatientCareRecordId")


  # IDENTIFY (A) patients transported emergently.
  incidents <- incidents %>% mutate(emergent_transport = if_else((`Transport Method` == "Air Medical-Rotor Craft" | Disposition %in% c("Transported Lights/Siren", "Transported No Lights/Siren, Upgraded")),
                                                                 1,
                                                                 0))
  # IDENTIFY (B) patients with unstable vital signs
  incidents <- incidents %>% filter_unstable_vitals(patients, start, end)

  # IDENTIFY (C) patients with anatomical injuries
  incidents <- incidents %>% filter_anatomical_injury(start, end)

  trauma_patients <- incidents %>% filter(trauma_mech == "yes") %>% filter(!is.na(trauma_anatomical_injury)|emergent_transport==1|!is.na(unstable_vitals))
  trauma_patients
}


#' Filter Unstable Vitals
#'
#' Augments an incidents tibble (with PatientCareRecordId primary key) with unstable vitals field.
#'
#' @param .data Incidents tibble we are augmenting.
#' @param patients  Patients tibble
#' @param start  Start Date
#' @param end End Date
#'
#' @return .data tibble with additional field for unstable vitals.
#'
#' @examples
filter_unstable_vitals <- function(.data, patients, start, end) {
  vitals <- get_eso_by_date(start, end, "Vitals+") %>% left_join(patients %>% select(PatientCareRecordId, pt_age), by="PatientCareRecordId") %>%
    mutate(unstable_vitals = case_when(
      `Pulse Strength` %in% c("Absent", "Weak", "Thready") ~ "Absent-Weak Radial Pulse",
      pt_age < 0.25 & `BP Systolic` < 60 ~ "Hypotensive",
      pt_age < 3 & `BP Systolic` < 70 ~ "Hypotensive",
      pt_age < 8 & `BP Systolic` < 80 ~ "Hypotensive",
      `BP Systolic` < 90 ~ "Hypotensive",
      pt_age < 0.25 & `Respiration Rate` > 50 ~ "Respiratory Compromise",
      pt_age < 3 & `Respiration Rate` > 40 ~ "Respiratory Compromise",
      `Respiration Rate` > 30 | `Respiration Rate` < 10 ~ "Respiratory Compromise",
      `Glasgow Scale Total Score` < 13 ~ "Altered Mental Status"
    )) %>% drop_na(unstable_vitals) %>% group_by(PatientCareRecordId) %>% summarise(unstable_vitals = paste(unstable_vitals, collapse=", "))

  .data %>% left_join(vitals %>% select(PatientCareRecordId, unstable_vitals), by="PatientCareRecordId")
}

filter_anatomical_injury <- function(.data, start, end) {
  # INJURY FILTERS
  injury_detail_filter <- c(
    "Assault by pushing victim from high place",
    "Assault by pushing victim in front of moving object (car, train, etc)",
    "Assault by sharp object (stabbing)",
    #"Assault with firearm",
    "Hit or run over by motor vehicle",
    #"Fall from high place",
    "Fall from tree",
    "Fall from, out of or through building",
    "Fall from, out of or through roof",
    "Fall from, out of or through window",
    "Fall on and from ladder",
    "Other fall from one level to another",
    "Discharge of handgun (Undetermined Intent)",
    "Discharge of larger firearm (Undetermined Intent)",
    "Firearm Injury (Accidental)",
    #"Firearm Injury (Assault)",
    "Firearm Injury (Self Inflicted)",
    "Handgun discharge and malfunction (accidental)",
    "Attempted Suicide",
    "Intentional self-harm by firearm discharge",
    #"Intentional self-harm with sharp object",
    #"Caught, crushed, jammed or pinched in or between objects",
    "Pedal cyclist injured in accident with car/pick-up truck/van",
    "Pedal cyclist injured in non-traffic accident",
    "2-wheeled or 3-wheeled motor vehicle collision injures pedestrian",
    #"Auto accident with pedestrian or animal injures occupant",
    "Bicycle accident injures pedestrian",
    "Car/pick-up truck/van collision injures pedestrian",
    "Heavy transport vehicle / bus collision injures pedestrian",
    "Other vehicle  accident injures occupant",
    "Pedestrian injured in collision with other nonmotor vehicle",
    "Pickup / van accident with pedestrian or animal injures occupant",
    #"Knife /sword /dagger contact",
    #"Struck by falling object",
    "Hanging as cause of asphyxiation",
    "Mechanical threat to breathing as cause of asphyxiation"
  )
  support_sign_filter <- c(
    "Abdominal rigidity",
    "Abdominal tenderness - rebound",
    "Hypotension",
    "Ankle - second degree burn",
    "Foot - first degree burn",
    "Foot - second degree burn",
    "Hand - second degree burn",
    "Hand - third degree burn",
    "Head and Face - second degree burn",
    "Lower leg - third degree burn",
    "Neck - second degree burn",
    "Thigh - third degree burn",
    "Trunk - third degree burn",
    "Wrist - second degree burn",
    "Cardiac arrest",
    "Hypotension",
    "Slowness and poor responsiveness",
    "Priapism",
    "Hemiplegia",
    "Monoplegia",
    "Paralysis",
    "Paralysis of lower limb",
    "Paralysis of upper limb",
    "Quadriplegia",
    "Apnea",
    "Periodic breathing/Cheyne-Stokes breathing",
    "Respiratory arrest",
    "Tension pneumothorax",
    "Traumatic asphyxiation/strangulation",
    "Cardiogenic shock",
    "Hypovolemic shock",
    "Septic shock",
    "Shock - unspecified",
    "Pallor"
  )
  impressions_filter <- c(
    "Acute abdomen",
    "Amputation of limb",
    "Amputation of other parts of head (face, ears,etc.)",
    "Anaphylactic Shock",
    "Brain Injury",
    "Cardiac arrest",
    "Cardiogenic shock",
    #"Eye Injury",
    "Foreign Body in Larynx",
    "Foreign Body in Pharynx",
    "Foreign Body in Respiratory Tract",
    "Foreign Body in Trachea",
    "Hemorrhagic Shock",
    "Hemothorax (Traumatic)",
    #"Hypotension",
    "Hypovolemia",
    "Hypovolemia / Shock",
    "Neurogenic Shock",
    "Obvious Death",
    "Pneumothorax",
    "Pneumothorax (Traumatic)",
    "Respiratory Arrest",
    "Respiratory Failure",
    "Subarachnoid Hemorrhage (Traumatic)",
    "Subdural Hemorrhage (Traumatic)",
    "Suffocation or Asphyxia",
    "Traumatic Circulatory Arrest"
  )

  # Add filter field based on above source data:
  .data %>% mutate(trauma_anatomical_injury = case_when(
    `Injury Detail` %in% injury_detail_filter ~ "Injury Detail",
    `Support Sign 1` %in% support_sign_filter ~ "Support Sign",
    `Support Sign 2` %in% support_sign_filter ~ "Support Sign",
    `Support Sign 3` %in% support_sign_filter ~ "Support Sign",
    `Support Sign 4`  %in% support_sign_filter ~ "Support Sign",
    `Support Sign 5` %in% support_sign_filter ~ "Support Sign",
    `Primary Impression` %in% impressions_filter ~ "Impression",
    `Secondary Impression` %in% impressions_filter ~ "Impression",
    `Is Trauma Activation` == "Yes" ~ "Trauma Activation",
    `Anatomy of Injury - Penetrating Injury` == "Yes" ~ "Penetrating Injury",
    `Anatomy of Injury - Long Bone Fractures` == "Yes" ~ "Long Bone Fractures",
    `Anatomy of Injury - Flail Chest` == "Yes" ~ "Flail Chest",
    `Anatomy of Injury - Serious Extremity Injury` == "Yes" ~ "Serious Extremity Injury",
    `Anatomy of Injury - Hand or Foot Amputation` == "Yes" ~ "Hand or Foot Amputation",
    `Anatomy of Injury - Pelvic Fractures` == "Yes" ~ "Pelvic Fractures",
    `Anatomy of Injury - Skull Fracture` == "Yes" ~ "Skull Fracture",
    `Anatomy of Injury - Paralysis` == "Yes" ~ "Paralysis",
    `Mechanism of Injury - Falls` == "Yes" ~ "Falls",
    `Mechanism of Injury - Auto vs  Pedestrian` == "Yes" ~ "Auto vs  Pedestrian",
    `Mechanism of Injury - Serious Motorcycle Crash` == "Yes" ~ "Serious Motorcycle Crash",
    `Mechanism of Injury - High-Risk Auto Crash` == "Yes" ~ "High-Risk Auto Crash",
    `Special Patient or System Configurations - Burns` == "Yes" ~ "Burns"
  ))
}
