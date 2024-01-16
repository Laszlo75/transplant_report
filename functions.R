#' gfr calculator from creatinine

f_gfr <- function(creatinine, age, sex) {
  A <- if_else(sex == "FEMALE", 0.7, 0.9)
  B <- case_when(
    sex == "FEMALE" & creatinine <= 62 ~ -0.241,
    sex == "FEMALE" & creatinine > 62 ~ -1.2,
    sex == "MALE" & creatinine <= 80 ~ -0.302,
    sex == "MALE" & creatinine > 80 ~ -1.2
  )
  C <- if_else(sex == "FEMALE", 1.012, 1)
  gfr <- 142 * (creatinine / 88.5714285714286 / A)^B * (0.9938^age) * C
  return(gfr)
}


#' Add new dashboard data
#'
#' `read_new_dashboard_data` binds the new *dashboard* data, both kidney and
#' pancreas to the `offers` data frame
#'
#' @param year year (YYYY)
#' @param month month (MM)
#' @returns a data frame
#'

read_new_dashboard_data <- function(year, month) {
  path <- paste("../raw_data/NHSBT OTDT Hub Dashboard ", year,
    sprintf("%02s", month), ".xlsm",
    sep = ""
  )
  new_kidney_data <-
    read_excel(path, sheet = "Kidney", range = "B5:C114") %>%
    `colnames<-`(c("centre", "offers")) %>% #
    filter(
      !is.na(centre),
      centre != "London"
    ) %>%
    mutate(
      month = month,
      year = year,
      date = as.Date(paste0(year, "-", month, "-01")),
      organ = "kidney"
    )
  new_pancreas_data <- read_excel(path, sheet = "Pancreas", range = "B5:C98") %>%
    `colnames<-`(c("centre", "offers")) %>%
    filter(
      !is.na(centre),
      centre != "London"
    ) %>%
    mutate(
      month = month,
      year = year,
      date = as.Date(paste0(year, "-", month, "-01")),
      organ = "pancreas"
    )
  if (exists("offers")) {
    return(rbind(offers, new_kidney_data, new_pancreas_data))
  } else {
    return(rbind(new_kidney_data, new_pancreas_data))
  }
}


#' read_new_declined_kidney_data` adds the *declined offers* for kidney
#'
#' @param year year (YYYY)
#' @param month month (MM)
#' @returns a data frame
#'
#'

# old pancreas declined offers data is missing, so I separate the 2 functions

read_new_declined_kidney_data <- function(year, month) {
  path2 <- paste("../raw_data/Kidney Cardiff ", year,
    sprintf("-%02s", month), ".xlsx",
    sep = ""
  )

  new_kidney_declined_data <-
    read_excel(path2,
      sheet = "Kidney Cardiff",
      range = cell_limits(c(3, 1), c(NA, 25))
    ) %>%
    set_names(c(
      "Donor_ID",
      "Hospital_ID",
      "Donor_type",
      "DRI_group",
      "Donor_age",
      "Donor_centre_region",
      "Offer_date",
      "Offer_type",
      "Kidney_offered",
      "Ultimate_organ_accepted",
      "Offered_to_particular_patient",
      "Recipient_ID",
      "Patient_RRI_group",
      "Patient_age",
      "Sensitisation",
      "Matchability_score",
      "Waiting_time_(days)",
      "High-priority_recipient",
      "Previous_kidney_offer_declines",
      "Ultimate_outcome_of_left_kidney",
      "Ultimate_outcome_of_right_kidney",
      "Primary_reason_for_centre_decline",
      "Secondary_reason_for_centre_decline",
      "Left_kidney_transplant_centre",
      "Right_kidney_transplant_centre"
    )) %>%
    filter(!startsWith(Donor_ID, "Note:"))


  if (exists("declined_kidney")) {
    return(rbind(declined_kidney, new_kidney_declined_data))
  } else {
    return(new_kidney_declined_data)
  }
}

#' `read_new_declined_pancreas_data` adds the *declined offers* for pancreas
#' @param year year
#' @param month month
#' @return a data frame
#'
#'

read_new_declined_pancreas_data <- function(year, month) {
  path2 <- paste("../raw_data/Pancreas Cardiff ",
    year,
    sprintf("-%02s", month),
    ".xlsx",
    sep = ""
  )

  new_pancreas_declined_data <-
    read_excel(path2,
      skip = 2,
      trim_ws = TRUE
    ) %>%
    set_names(c(
      "Donor_ID",
      "Hospital_ID",
      # "Donor_type",
      "Donor_age",
      "Donor_BMI",
      "Donor_centre_region",
      "Offer_date",
      "Fast_tracked",
      "Offered_to_particular_patient",
      "Recipient_ID",
      "Patient_age",
      "Highly_sensitised",
      "Outcome_of_offer",
      "Primary_reason_for_centre_decline",
      "Secondary_reason_for_centre_decline",
      "Pancreas_transplant_centre",
      "Ultimate_organ_transplanted"
    )) %>%
    filter(!is.na(Hospital_ID))

  if (exists("declined_pancreas")) {
    return(rbind(declined_pancreas, new_pancreas_declined_data))
  } else {
    return(new_pancreas_declined_data)
  }
}
