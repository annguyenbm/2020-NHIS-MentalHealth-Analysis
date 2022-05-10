library(tidyverse)

adult_raw <- read.csv("../data-raw/adult20.csv")

# Variable selections
adult_full <- adult_raw %>% 
  select(
    # An's Vars
    ANXFREQ_A, ANXEV_A, ANXMED_A, 
    DEPFREQ_A, DEPEV_A, DEPMED_A,
    AGEP_A, EDUC_A, CITZNSTP_A,
    BMICAT_A, FAMINCTC_A,
    CVDDIAG_A, DLYCARE_A, HICOV_A,
    DRK12MN1_A, DRK12MTP1_A, DRKSTAT_A,
    # Ben's Vars
    INCGRP_A, INCSSISSDI_A, INTV_MON, INTV_QRT,
    MHRX_A, MHTHDLY_A, MHTHND_A,
    MODFREQW_A, ORIENT_A, PA18_02R_A, PAIFRQ3M_A,
    PAYBLL12M_A, PAYWORRY_A, PCNTTC,
    POVRATTC_A, RACEALLP_A, REGION,
    # Andrew's Vars 
    SEX_A, SLPHOURS_A, STRFREQW_A, STRNR_A, VIGFREQW_A, 
    NOWACTIVE_A, WLKLEIS_A, FDSRUNOUT_A, FDSLAST_A, FDSBALANCE_A
  ) %>%
  rename(
    #Rename An's vars to reflect code below: 
    ANXFREQ = ANXFREQ_A, 
    ANXEV = ANXEV_A, 
    ANXMED = ANXMED_A, 
    DEPFREQ = DEPFREQ_A, 
    DEPEV = DEPEV_A, 
    DEPMED = DEPMED_A,
    AGEP = AGEP_A, 
    EDUC = EDUC_A, 
    CITZNSTP = CITZNSTP_A,
    BMICAT = BMICAT_A, 
    FAMINCTC = FAMINCTC_A,
    CVDDIAG = CVDDIAG_A, 
    DLYCARE = DLYCARE_A, 
    HICOV = HICOV_A,
    DRK12MN1 = DRK12MN1_A, 
    DRK12MTP1 = DRK12MTP1_A, 
    DRKSTAT = DRKSTAT_A,
    #Rename Andrew's variables to reflect code below: 
    SEX = SEX_A, 
    SLPHOURS = SLPHOURS_A, 
    STRFREQW = STRFREQW_A, 
    STRNR = STRNR_A, 
    VIGFREQW = VIGFREQW_A, 
    NOWACTIVE = NOWACTIVE_A, 
    WLKLEIS = WLKLEIS_A, 
    FDSRUNOUT = FDSRUNOUT_A, 
    FDSLAST = FDSLAST_A, 
    FDSBALANCE = FDSBALANCE_A
  )

# Transform variables
adult_full <- adult_full %>% 
  mutate(
    #An's Variables:
    # ANXFREQ 
    ANXFREQ = as_factor(ANXFREQ),
    anxiety_freq = as_factor(
      case_when(ANXFREQ %in% c("1", "2") ~ "Severe",
                ANXFREQ %in% c("3", "4", "5") ~ "Non-Severe",
                TRUE ~ NA_character_)),
    anxiety_freq_meta = as_factor(
      case_when(!is.na(anxiety_freq) ~ "Answered",
                is.na(anxiety_freq) ~ "Missing",
                ANXFREQ == 7 ~ "Refused",
                ANXFREQ == 8 ~ "Not Ascertained",
                ANXFREQ == 9 ~ "Don't Know")),
    # ANXEV
    ANXEV = as_factor(ANXEV),
    anxiety_history = as_factor(
      case_when(ANXEV == "1" ~ "Yes",
                ANXEV == "2" ~ "No" ,
                TRUE ~ NA_character_)),
    anxiety_history_meta = as_factor(
      case_when(!is.na(anxiety_history) ~ "Answered",
                is.na(anxiety_history) ~ "Missing",
                ANXEV == 7 ~ "Refused",
                ANXEV == 8 ~ "Not Ascertained",
                ANXEV == 9 ~ "Don't Know")),
    # ANXMED
    ANXMED = as_factor(ANXMED),
    anxiety_med = as_factor(
      case_when(ANXMED == "1" ~ "Yes",
                ANXMED == "2" ~ "No" ,
                TRUE ~ NA_character_)),
    anxiety_med_meta = as_factor(
      case_when(!is.na(anxiety_med) ~ "Answered",
                ANXMED == 7 ~ "Refused",
                ANXMED == 8 ~ "Not Ascertained",
                ANXMED == 9 ~ "Don't Know")),
    # DEPFREQ
    DEPFREQ = as_factor(DEPFREQ),
    depression_freq = as_factor(
      case_when(DEPFREQ %in% c("1", "2") ~ "Severe",
                DEPFREQ %in% c("3", "4", "5") ~ "Non-Severe",
                TRUE ~ NA_character_)),
    depression_freq_meta = as_factor(
      case_when(!is.na(depression_freq) ~ "Answered",
                DEPFREQ == 7 ~ "Refused",
                DEPFREQ == 8 ~ "Not Ascertained",
                DEPFREQ == 9 ~ "Don't Know")),
    # DEPEV
    DEPEV = as_factor(DEPEV),
    depression_history = as_factor(
      case_when(DEPEV == "1" ~ "Yes",
                DEPEV == "2" ~ "No" ,
                TRUE ~ NA_character_)),
    depression_history_meta = as_factor(
      case_when(!is.na(depression_history) ~ "Answered",
                is.na(depression_history) ~ "Missing",
                DEPEV == 7 ~ "Refused",
                DEPEV == 8 ~ "Not Ascertained",
                DEPEV == 9 ~ "Don't Know")),
    # DEPMED
    DEPMED = as_factor(DEPMED),
    depression_med = as_factor(
      case_when(DEPMED == "1" ~ "Yes",
                DEPMED == "2" ~ "No" ,
                TRUE ~ NA_character_)),
    depression_med_meta = as_factor(
      case_when(!is.na(depression_med) ~ "Answered",
                DEPMED == 7 ~ "Refused",
                DEPMED == 8 ~ "Not Ascertained",
                DEPMED == 9 ~ "Don't Know")),
    # AGE
    age = ifelse(AGEP <= 85, AGEP, NA_real_),
    age_meta = as_factor(
      case_when(!is.na(age) ~ "Answered",
                AGEP == 97 ~ "Refused",
                AGEP == 98 ~ "Not Ascertained",
                AGEP == 99 ~ "Don't Know")),
    # EDUC
    EDUC  = as_factor(EDUC),
    edu_lvl = as_factor(
      case_when(EDUC %in% c("0", "1", "2", "3", "4") ~ "Highschool/Lower",
                EDUC %in% c("5", "6", "7", "8") ~ "Bachelor/Undergrad equivalent",
                EDUC %in% c("9", "10", "11") ~ "Masters or higher",
                TRUE ~ NA_character_)),
    edu_lvl_meta = as_factor(
      case_when(!is.na(edu_lvl) ~ "Answered",
                EDUC == "97" ~ "Refused",
                EDUC == "98" ~ "Not Ascertained",
                EDUC == "99" ~ "Don't Know")),
    # CITZNSTP
    CITZNSTP = as_factor(CITZNSTP),
    citizenship = as_factor(
      case_when(CITZNSTP == "1" ~ "US",
                CITZNSTP == "2" ~ "Foreign",
                TRUE ~ NA_character_)),
    citizenship_meta = as_factor(
      case_when(!is.na(citizenship) ~ "Answered",
                CITZNSTP == 7 ~ "Refused",
                CITZNSTP == 8 ~ "Not Ascertained",
                CITZNSTP == 9 ~ "Don't Know")),
    # BMICAT
    BMICAT = as_factor(BMICAT),
    bmi_category = as_factor(
      case_when(BMICAT == "1" ~ "Underweight",
                BMICAT == "2" ~ "Healthy",
                BMICAT == "3" ~ "Overweight",
                BMICAT == "4" ~ "Obese",
                TRUE ~ NA_character_)),
    bmi_category_meta = as_factor(
      case_when(!is.na(bmi_category) ~ "Answered",
                BMICAT == 9 ~ "Unknown")),
    # FAMINCTC 
    family_income = FAMINCTC,
    family_income_meta = case_when(!is.na(family_income) ~ "Answered"),
    # CVDDIAG
    CVDDIAG = as_factor(CVDDIAG),
    cv19_diag_positive = as_factor(
      case_when(CVDDIAG == "1" ~ "Yes",
                CVDDIAG == "2" ~ "No",
                TRUE ~ NA_character_)),
    cv19_diag_positive_meta = as_factor(
      case_when(!is.na(cv19_diag_positive) ~ "Answered",
                is.na(cv19_diag_positive) ~ "Missing",
                CVDDIAG == 7 ~ "Refused",
                CVDDIAG == 8 ~ "Not Ascertained",
                CVDDIAG == 9 ~ "Don't Know")),
    # DLYCARE
    DLYCARE = as_factor(DLYCARE),
    delay_med_care = as_factor(
      case_when(DLYCARE == "1" ~ "Yes",
                DLYCARE == "2" ~ "No",
                TRUE ~ NA_character_)),
    delay_med_care_meta = as_factor(
      case_when(!is.na(delay_med_care) ~ "Answered",
                is.na(delay_med_care) ~ "Missing",
                DLYCARE == 7 ~ "Refused",
                DLYCARE == 8 ~ "Not Ascertained",
                DLYCARE == 9 ~ "Don't Know")),
    # HICOV 
    HICOV = as_factor(HICOV),
    insurance = as_factor(
      case_when(HICOV == "1" ~ "Yes",
                HICOV == "2" ~ "No",
                TRUE ~ NA_character_)),
    insurance_meta = as_factor(
      case_when(!is.na(insurance) ~ "Answered",
                is.na(insurance) ~ "Missing",
                HICOV == 7 ~ "Refused",
                HICOV == 8 ~ "Not Ascertained",
                HICOV == 9 ~ "Don't Know")),
    # DRK12MN1
    days_drink_alcohol = ifelse(DRK12MN1 <= 365, DRK12MN1, NA_real_),
    days_drink_alcohol_meta = as_factor(
      case_when(!is.na(days_drink_alcohol) ~ "Answered",
                is.na(days_drink_alcohol) ~ "Missing",
                DRK12MN1 == 996 ~ "Inconsistent",
                DRK12MN1 == 997 ~ "Refused",
                DRK12MN1 == 998 ~ "Not Ascertained",
                DRK12MN1 == 999 ~ "Don't Know")),
    # DRK12MPT1
    DRK12MTP1 = as_factor(DRK12MTP1),
    alcohol_freq = as_factor(
      case_when(DRK12MTP1 == "0" ~ "Never/None",
                DRK12MTP1 == "1" ~ "Weekly",
                DRK12MTP1 == "2" ~ "Monthly",
                DRK12MTP1 == "3" ~ "Yearly",
                TRUE ~ NA_character_)),
    alcohol_freq_meta = as_factor(
      case_when(!is.na(alcohol_freq) ~ "Answered",
                is.na(alcohol_freq) ~ "Missing",
                DRK12MTP1 == 6 ~ "Inconsistent",
                DRK12MTP1 == 7 ~ "Refused",
                DRK12MTP1 == 8 ~ "Not Ascertained",
                DRK12MTP1 == 9 ~ "Don't Know")),
    # DRKSTAT
    DRKSTAT = as_factor(DRKSTAT),
    drinking_status = as_factor(
      case_when(DRKSTAT == "1" ~ "Lifetime abstainer",
                DRKSTAT == "2" ~ "Former Irregular",
                DRKSTAT == "3" ~ "Former Regular",
                DRKSTAT %in% c("5", "6") ~ "Current Irregular",
                DRKSTAT %in% c("7", "8") ~ "Current Regular",
                TRUE ~ NA_character_)),
    drinking_status_meta = as_factor(
      case_when(!is.na(drinking_status) ~ "Answered",
                is.na(drinking_status) ~ "Missing",
                TRUE ~ "Unknown")),
    # INCGRP_A
    income_group = as_factor(case_when(INCGRP_A == 1 ~ "$0 - $34,999",
                                       INCGRP_A == 2 ~ "$35,000 - $49,999",
                                       INCGRP_A == 3 ~ "$50,000 - $74,999",
                                       INCGRP_A == 4 ~ "$75,000 - $99,999",
                                       INCGRP_A == 5 ~ "$100,000 or more",
                                       INCGRP_A == 8 ~ NA_character_,
                                       is.na(INCGRP_A) ~ NA_character_,
                                       TRUE ~ NA_character_)),
    income_group_meta = as_factor(ifelse(INCGRP_A == 8, "Not Ascertained", "Answered")),
    # Ben's Vars
    # INCSSISSDI_A
    ss_disability_income = as_factor(
      case_when(INCSSISSDI_A == 1 ~ "Yes",
                INCSSISSDI_A == 2 ~ "No",
                INCSSISSDI_A == 7 ~ NA_character_,
                INCSSISSDI_A == 8 ~ NA_character_,
                INCSSISSDI_A == 9 ~ NA_character_,
                is.na(INCSSISSDI_A) ~ NA_character_,
                TRUE ~ NA_character_)),
    ss_disability_income_meta = as_factor(
      case_when(INCSSISSDI_A == 1 ~ "Answered",
                INCSSISSDI_A == 2 ~ "Answered",
                INCSSISSDI_A == 7 ~ "Refused",
                INCSSISSDI_A == 8 ~ "Not Ascertained",
                INCSSISSDI_A == 9 ~ "Don't Know",
                is.na(INCSSISSDI_A) ~ "Missing",
                TRUE ~ NA_character_)),
    # INTV_MON
    interview_month = as_factor(case_when(INTV_MON == 1 ~ "January",
                                          INTV_MON == 2 ~ "February",
                                          INTV_MON == 3 ~ "March",
                                          INTV_MON == 4 ~ "April",
                                          INTV_MON == 5 ~ "May",
                                          INTV_MON == 6 ~ "June",
                                          INTV_MON == 7 ~ "July",
                                          INTV_MON == 8 ~ "August",
                                          INTV_MON == 9 ~ "September",
                                          INTV_MON == 10 ~ "October",
                                          INTV_MON == 11 ~ "November",
                                          INTV_MON == 12 ~ "December",
                                          is.na(INTV_MON) ~ NA_character_,
                                          TRUE ~ NA_character_)),
    interview_month_meta = as_factor(ifelse(
      is.na(INTV_MON), "Missing", "Answered")),
    # INTV_QRT
    interview_quarter = as_factor(case_when(INTV_QRT == 1 ~ "Q1",
                                            INTV_QRT == 2 ~ "Q2",
                                            INTV_QRT == 3 ~ "Q3",
                                            INTV_QRT == 4 ~ "Q4",
                                            is.na(INTV_QRT) ~ NA_character_,
                                            TRUE ~ NA_character_)),
    interview_quarter_meta = as_factor(
      if_else(interview_quarter %in% c("Q1", "Q2", "Q3", "Q4"), 
              "Answered", "Missing")),
    # MHRX_A
    mental_health_medicated = as_factor(
      case_when(MHRX_A == 1 ~ "Yes",
                MHRX_A == 2 ~ "No",
                MHRX_A == 7 ~ NA_character_,
                MHRX_A == 8 ~ NA_character_,
                MHRX_A == 9 ~ NA_character_,
                is.na(MHRX_A) ~ NA_character_,
                TRUE ~ NA_character_)),
    mental_health_medicated_meta = as_factor(
      case_when(MHRX_A == 1 ~ "Answered",
                MHRX_A == 2 ~ "Answered",
                MHRX_A == 7 ~ "Refused",
                MHRX_A == 8 ~ "Not Ascertained",
                MHRX_A == 9 ~ "Don't Know",
                is.na(MHRX_A) ~ "Missing",
                TRUE ~ NA_character_)),
    # MHTHDLY_A
    mental_health_cost_delay = as_factor(
      case_when(MHTHDLY_A == 1 ~ "Yes",
                MHTHDLY_A == 2 ~ "No",
                MHTHDLY_A == 7 ~ NA_character_,
                MHTHDLY_A == 8 ~ NA_character_,
                MHTHDLY_A == 9 ~ NA_character_,
                is.na(MHTHDLY_A) ~ NA_character_,
                TRUE ~ NA_character_)),
    mental_health_cost_delay_meta = as_factor(
      case_when(MHTHDLY_A == 1 ~ "Answered",
                MHTHDLY_A == 2 ~ "Answered",
                MHTHDLY_A == 7 ~ "Refused",
                MHTHDLY_A == 8 ~ "Not Ascertained",
                MHTHDLY_A == 9 ~ "Don't Know",
                is.na(MHTHDLY_A) ~ "Missing",
                TRUE ~ NA_character_)),
    # MHTHND_A
    mental_health_cost_avoid = as_factor(
      case_when(MHTHND_A == 1 ~ "Yes",
                MHTHND_A == 2 ~ "No",
                MHTHND_A == 7 ~ NA_character_,
                MHTHND_A == 8 ~ NA_character_,
                MHTHND_A == 9 ~ NA_character_,
                is.na(MHTHND_A) ~ NA_character_,
                TRUE ~ NA_character_)),
    mental_health_cost_avoid_meta = as_factor(
      case_when(MHTHND_A == 1 ~ "Answered",
                MHTHND_A == 2 ~ "Answered",
                MHTHND_A == 7 ~ "Refused",
                MHTHND_A == 8 ~ "Not Ascertained",
                MHTHND_A == 9 ~ "Don't Know",
                is.na(MHTHND_A) ~ "Missing",
                TRUE ~ NA_character_)),
    # MODFREQW_A
    physical_activity_moderate_freq = case_when(
      MODFREQW_A <= 28 ~ as.integer(MODFREQW_A),
      MODFREQW_A == 94 ~ 0L,
      MODFREQW_A == 95 ~ 28L,
      MODFREQW_A == 96 ~ 0L,
      MODFREQW_A == 97 ~ NA_integer_,
      MODFREQW_A == 98 ~ NA_integer_,
      MODFREQW_A == 99 ~ NA_integer_,
      is.na(MODFREQW_A) ~ NA_integer_,
      TRUE ~ NA_integer_),
    physical_activity_moderate_freq_meta = as_factor(
      case_when(MODFREQW_A <= 96 ~ "Answered",
                MODFREQW_A == 97 ~ "Refused",
                MODFREQW_A == 98 ~ "Not Ascertained",
                MODFREQW_A == 99 ~ "Don't Know",
                is.na(MODFREQW_A) ~ "Missing",
                TRUE ~ NA_character_)),
    # ORIENT_A
    sexuality = as_factor(case_when(ORIENT_A == 1 ~ "Gay",
                                    ORIENT_A == 2 ~ "Not Gay",
                                    ORIENT_A == 3 ~ "Bisexual",
                                    ORIENT_A == 4 ~ "Other",
                                    ORIENT_A == 5 ~ "Questioning",
                                    ORIENT_A == 7 ~ NA_character_,
                                    ORIENT_A == 8 ~ NA_character_,
                                    ORIENT_A == 9 ~ NA_character_,
                                    is.na(ORIENT_A) ~ NA_character_,
                                    TRUE ~ NA_character_)),
    sexuality_meta = as_factor(case_when(ORIENT_A == 1 ~ "Answered",
                                         ORIENT_A == 2 ~ "Answered",
                                         ORIENT_A == 3 ~ "Answered",
                                         ORIENT_A == 4 ~ "Answered",
                                         ORIENT_A == 5 ~ "Answered",
                                         ORIENT_A == 7 ~ "Refused",
                                         ORIENT_A == 8 ~ "Not Ascertained",
                                         ORIENT_A == 9 ~ "Don't Know",
                                         is.na(ORIENT_A) ~ "Missing",
                                         TRUE ~ NA_character_)),
    # PA18_02R_A
    physical_activity_aerobic = as_factor(
      case_when(PA18_02R_A == 1 ~ "Inactive",
                PA18_02R_A == 2 ~ "Insufficiently active",
                PA18_02R_A == 3 ~ "Sufficiently active",
                PA18_02R_A == 8 ~ NA_character_,
                is.na(PA18_02R_A) ~ NA_character_,
                TRUE ~ NA_character_)),
    physical_activity_aerobic_meta = as_factor(
      case_when(PA18_02R_A == 1 ~ "Answered",
                PA18_02R_A == 2 ~ "Answered",
                PA18_02R_A == 3 ~ "Answered",
                PA18_02R_A == 8 ~ "Not Ascertained",
                is.na(PA18_02R_A) ~ "Missing",
                TRUE ~ NA_character_)),
    # PAIFRQ3M_A
    pain_freq = as_factor(case_when(PAIFRQ3M_A == 1 ~ "Never",
                                    PAIFRQ3M_A == 2 ~ "Some Days",
                                    PAIFRQ3M_A == 3 ~ "Most Days",
                                    PAIFRQ3M_A == 4 ~ "Every Day",
                                    PAIFRQ3M_A == 7 ~ NA_character_,
                                    PAIFRQ3M_A == 8 ~ NA_character_,
                                    PAIFRQ3M_A == 9 ~ NA_character_,
                                    is.na(PAIFRQ3M_A) ~ NA_character_,
                                    TRUE ~ NA_character_)),
    pain_freq_meta = as_factor(case_when(PAIFRQ3M_A == 1 ~ "Answered",
                                         PAIFRQ3M_A == 2 ~ "Answered",
                                         PAIFRQ3M_A == 3 ~ "Answered",
                                         PAIFRQ3M_A == 4 ~ "Answered",
                                         PAIFRQ3M_A == 7 ~ "Refused",
                                         PAIFRQ3M_A == 8 ~ "Not Ascertained",
                                         PAIFRQ3M_A == 9 ~ "Don't Know",
                                         is.na(PAIFRQ3M_A) ~ "Missing",
                                         TRUE ~ NA_character_)),
    # PAYBLL12M_A
    pay_bill_ability = as_factor(case_when(PAYBLL12M_A == 1 ~ "Yes",
                                           PAYBLL12M_A == 2 ~ "No",
                                           PAYBLL12M_A == 7 ~ NA_character_,
                                           PAYBLL12M_A == 8 ~ NA_character_,
                                           PAYBLL12M_A == 9 ~ NA_character_,
                                           is.na(PAYBLL12M_A) ~ NA_character_,
                                           TRUE ~ NA_character_)),
    pay_bill_ability_meta = as_factor(
      case_when(PAYBLL12M_A == 1 ~ "Answered",
                PAYBLL12M_A == 2 ~ "Answered",
                PAYBLL12M_A == 7 ~ "Refused",
                PAYBLL12M_A == 8 ~ "Not Ascertained",
                PAYBLL12M_A == 9 ~ "Don't Know",
                is.na(PAYBLL12M_A) ~ "Missing",
                TRUE ~ NA_character_)),
    # PAYWORRY_A
    pay_bill_worry = as_factor(
      case_when(PAYWORRY_A == 1 ~ "Very Worried",
                PAYWORRY_A == 2 ~ "Somewhat Worried",
                PAYWORRY_A == 3 ~ "Not at all Worried",
                PAYWORRY_A == 7 ~ NA_character_,
                PAYWORRY_A == 8 ~ NA_character_,
                PAYWORRY_A == 9 ~ NA_character_,
                is.na(PAYWORRY_A) ~ NA_character_,
                TRUE ~ NA_character_)),
    pay_bill_worry_meta = as_factor(
      case_when(PAYWORRY_A == 1 ~ "Answered",
                PAYWORRY_A == 2 ~ "Answered",
                PAYWORRY_A == 3 ~ "Answered",
                PAYWORRY_A == 7 ~ "Refused",
                PAYWORRY_A == 8 ~ "Not Ascertained",
                PAYWORRY_A == 9 ~ "Don't Know",
                is.na(PAYWORRY_A) ~ "Missing",
                TRUE ~ NA_character_)),
    # PCNTTC
    house_people_number = ifelse(PCNTTC == 8, NA_integer_, as.integer(PCNTTC)),
    house_people_number_meta = as_factor(
      ifelse(PCNTTC == 8, "Not Ascertained", "Answered")),
    # POVRATTC_A
    poverty_rate = case_when(is.double(POVRATTC_A) ~ as.double(POVRATTC_A),
                             is.na(POVRATTC_A) ~ NA_real_,
                             TRUE ~ NA_real_),
    poverty_rate_meta = as_factor(
      ifelse(POVRATTC_A == 11, "Rounded Down", "Exact")),
    # RACEALLP_A
    race = as_factor(case_when(RACEALLP_A == 1 ~ "White",
                               RACEALLP_A == 2 ~ "Black",
                               RACEALLP_A == 3 ~ "Asian",
                               RACEALLP_A == 4 ~ "AIAN",
                               RACEALLP_A == 5 ~ "AIAN mixed",
                               RACEALLP_A == 6 ~ "Other",
                               RACEALLP_A == 7 ~ NA_character_,
                               RACEALLP_A == 8 ~ NA_character_,
                               RACEALLP_A == 9 ~ NA_character_,
                               is.na(RACEALLP_A) ~ NA_character_,
                               TRUE ~ NA_character_)),
    race_meta = as_factor(case_when(RACEALLP_A == 1 ~ "Answered",
                                    RACEALLP_A == 2 ~ "Answered",
                                    RACEALLP_A == 3 ~ "Answered",
                                    RACEALLP_A == 4 ~ "Answered",
                                    RACEALLP_A == 5 ~ "Answered",
                                    RACEALLP_A == 6 ~ "Answered",
                                    RACEALLP_A == 7 ~ "Refused",
                                    RACEALLP_A == 8 ~ "Not Ascertained",
                                    RACEALLP_A == 9 ~ "Don't know",
                                    is.na(RACEALLP_A) ~ "Missing",
                                    TRUE ~ NA_character_)),
    # REGION
    region = as_factor(case_when(REGION == 1 ~ "Northeast",
                                 REGION == 2 ~ "Midwest",
                                 REGION == 3 ~ "South",
                                 REGION == 4 ~ "West",
                                 TRUE ~ NA_character_)),
    region_meta = as_factor(if_else(region %in% c("Northeast", "Midwest", 
                                        "South", "West"), 
                       "Answered", "Missing")),
    # Andrew's Vars
    # SEX 
    SEX = as_factor(SEX),
    sex_mf = as_factor(case_when(SEX == "1" ~ "Male",
                                 SEX == "2" ~ "Female",
                                 TRUE ~ NA_character_)),
    sex_mf_meta = as_factor(case_when(!is.na(sex_mf) ~ "Answered",
                                      SEX == "7" ~ "Refused",
                                      SEX == "9" ~ "Don't Know")),
    # SLPHOURS
    sleep_hours = case_when(SLPHOURS <= 24 ~ SLPHOURS,
                            TRUE ~ NA_integer_),
    sleep_hours_meta = as_factor(
      case_when(!is.na(sleep_hours) ~ "Answered",
                SLPHOURS == 97 ~ "Refused",
                SLPHOURS == 98 ~ "Not Ascertained",
                SLPHOURS == 99 ~ "Don't Know")),
    # STRFREQW 
    strength_act_freq_w = case_when(STRFREQW <= 30 ~ STRFREQW,
                                    STRFREQW == 94 ~ 0L,
                                    STRFREQW == 95 ~ 28L,
                                    STRFREQW == 96 ~ 0L,
                                    TRUE ~ NA_integer_),
    strength_act_freq_w_meta = as_factor(
      case_when(!is.na(strength_act_freq_w) ~ "Answered",
                STRFREQW == 95 ~ "Answered",
                STRFREQW == 97 ~ "Refused",
                STRFREQW == 98 ~ "Not Ascertained",
                STRFREQW == 99 ~ "Don't Know")),
    # STRNR (to create str_unable)
    str_unable = as_factor(case_when(STRNR == 9996 ~ "Yes",
                                     STRNR <= 9995 ~ "No",
                                     TRUE ~ NA_character_)),
    str_unable_meta = as_factor(
      case_when(STRNR <= 9996 ~ "Answered",
                STRNR == 9997 ~ "Refused",
                STRNR == 9998 ~ "Not Ascertained",
                STRNR == 9999 ~ "Don't Know")),
    # VIGFREQW 
    vig_act_freq_w = case_when(VIGFREQW <= 30 ~ VIGFREQW,
                               VIGFREQW == 94 ~ 0L,
                               VIGFREQW == 95 ~ 28L,
                               VIGFREQW == 96 ~ 0L,
                               TRUE ~ NA_integer_), 
    vig_act_freq_w_meta = as_factor(
      case_when(!is.na(vig_act_freq_w) ~ "Answered",
                VIGFREQW == 95 ~ "Answered",
                VIGFREQW == 97 ~ "Refused",
                VIGFREQW == 98 ~ "Not Ascertained",
                VIGFREQW == 99 ~ "Don't Know")),
    # VIGFREQW (to create vig_unable)
    vig_unable = as_factor(case_when(VIGFREQW == 96 ~ "Yes",
                                     VIGFREQW <= 95 ~ "No",
                                     TRUE ~ NA_character_)),
    vig_unable_meta = as_factor(
      case_when(VIGFREQW <= 96 ~ "Answered",
                VIGFREQW == 97 ~ "Refused",
                VIGFREQW == 98 ~ "Not Ascertained",
                VIGFREQW == 99 ~ "Don't Know")),
    # NOWACTIVE
    NOWACTIVE = as_factor(NOWACTIVE),
    now_active = as_factor(case_when(NOWACTIVE == "1" ~ "Yes",
                                     NOWACTIVE == "2" ~ "No",
                                     TRUE ~ NA_character_)),
    now_active_meta = as_factor(
      case_when(!is.na(now_active) ~ "Answered",
                NOWACTIVE == "7" ~ "Refused",
                NOWACTIVE == "8" ~ "Not Ascertained",
                NOWACTIVE == "9" ~ "Don't Know")),
    # WLKLEIS 
    WLKLEIS = as_factor(WLKLEIS),
    walk_leisure_yn = as_factor(case_when(WLKLEIS == "1" ~ "Yes",
                                          WLKLEIS == "2" ~ "No",
                                          TRUE ~ NA_character_)),
    walk_leisure_yn_meta = as_factor(
      case_when(!is.na(walk_leisure_yn) ~ "Answered",
                is.na(WLKLEIS) ~ "Missing",
                WLKLEIS == "7" ~ "Refused",
                WLKLEIS == "8" ~ "Not Ascertained",
                WLKLEIS == "9" ~ "Don't Know")),
    # FDSRUNOUT 
    FDSRUNOUT = as_factor(FDSRUNOUT),
    food_runout_worry = as_factor(
      case_when(FDSRUNOUT == "1" ~ "Often true",
                FDSRUNOUT == "2" ~ "Sometimes true",
                FDSRUNOUT == "3" ~ "Never true",
                TRUE ~ NA_character_)),
    food_runout_worry_meta = as_factor(
      case_when(!is.na(food_runout_worry) ~ "Answered",
                FDSRUNOUT == "7" ~ "Refused",
                FDSRUNOUT == "8" ~ "Not Ascertained",
                FDSRUNOUT == "9" ~ "Don't Know")),
    # FDSLAST 
    FDSLAST = as_factor(FDSLAST), 
    food_not_last_m = as_factor(
      case_when(FDSLAST == "1" ~ "Often true",
                FDSLAST == "2" ~ "Sometimes true",
                FDSLAST == "3" ~ "Never true",
                TRUE ~ NA_character_)),
    food_not_last_m_meta = as_factor(
      case_when(!is.na(food_not_last_m) ~ "Answered",
                FDSLAST == "7" ~ "Refused",
                FDSLAST == "8" ~ "Not Ascertained",
                FDSLAST == "9" ~ "Don't Know")),
    # FDSBALANCE 
    FDSBALANCE = as_factor(FDSBALANCE),
    unafford_bal_meals_m = as_factor(
      case_when(FDSBALANCE == "1" ~ "Often true",
                FDSBALANCE == "2" ~ "Sometimes true",
                FDSBALANCE == "3" ~ "Never true",
                TRUE ~ NA_character_)),
    unafford_bal_meals_m_meta = as_factor(
      case_when(!is.na(unafford_bal_meals_m) ~ "Answered",
                FDSBALANCE == "7" ~ "Refused",
                FDSBALANCE == "8" ~ "Not Ascertained",
                FDSBALANCE == "9" ~ "Don't Know"))
  ) %>% 
  # Remove columns containing raw data
            # An's Vars
  select(-c(ANXFREQ, ANXEV, ANXMED, 
            DEPFREQ, DEPEV, DEPMED,
            AGEP, EDUC, CITZNSTP,
            BMICAT, FAMINCTC,
            CVDDIAG, DLYCARE, HICOV,
            DRK12MN1, DRK12MTP1, DRKSTAT,
            # Ben's Vars
            INCGRP_A, INCSSISSDI_A, INTV_MON, INTV_QRT,
            MHRX_A, MHTHDLY_A, MHTHND_A,
            MODFREQW_A, ORIENT_A, PA18_02R_A, PAIFRQ3M_A,
            PAYBLL12M_A, PAYWORRY_A, PCNTTC,
            POVRATTC_A, RACEALLP_A, REGION,
            # Andrew's Vars
            SEX, SLPHOURS, STRFREQW, STRNR, VIGFREQW, 
            NOWACTIVE, WLKLEIS, FDSRUNOUT, FDSLAST, FDSBALANCE)
  )

# Create two data frames, one with main variables, one with meta:

# Select and order main data columns: 

adult_full %>%
  select(!ends_with("_meta")) %>%
  select(anxiety_freq, anxiety_history, anxiety_med, depression_freq,
         depression_history, depression_med, mental_health_medicated,
         age, interview_month, interview_quarter, sex_mf, house_people_number,
         region, race, sexuality, edu_lvl, citizenship, bmi_category,
         family_income, income_group, poverty_rate, cv19_diag_positive,
         sleep_hours, alcohol_freq, drinking_status, days_drink_alcohol,
         insurance, delay_med_care, ss_disability_income,
         mental_health_cost_delay, mental_health_cost_avoid,
         pay_bill_ability, pay_bill_worry, pain_freq,
         physical_activity_aerobic, now_active,
         physical_activity_moderate_freq, strength_act_freq_w,
         str_unable, vig_act_freq_w, vig_unable, walk_leisure_yn,
         food_runout_worry, food_not_last_m, unafford_bal_meals_m) ->
  adult

# Select and order meta columns 
adult_full %>%
  select(ends_with("_meta")) %>%
  select(anxiety_freq_meta, anxiety_history_meta, anxiety_med_meta,
         depression_freq_meta, depression_history_meta, depression_med_meta,
         mental_health_medicated_meta, age_meta, interview_month_meta,
         interview_quarter_meta, sex_mf_meta, house_people_number_meta,
         region_meta, race_meta, sexuality_meta, edu_lvl_meta, 
         citizenship_meta, bmi_category_meta, family_income_meta, 
         income_group_meta, poverty_rate_meta, cv19_diag_positive_meta,
         sleep_hours_meta, alcohol_freq_meta, drinking_status_meta,
         days_drink_alcohol_meta, insurance_meta, delay_med_care_meta,
         ss_disability_income_meta, mental_health_cost_delay_meta,
         mental_health_cost_avoid_meta, pay_bill_ability_meta, 
         pay_bill_worry_meta, pain_freq_meta,
         physical_activity_aerobic_meta, now_active_meta,
         physical_activity_moderate_freq_meta, strength_act_freq_w_meta,
         str_unable_meta, vig_act_freq_w_meta, vig_unable_meta, 
         walk_leisure_yn_meta, food_runout_worry_meta, 
         food_not_last_m_meta, unafford_bal_meals_m_meta) ->
  adult_meta

# Reshape the adult_full object by combining the two subsetted groups of columns: 

adult_full <- bind_cols(adult, adult_meta)


# Compress all main data objects for use in the shiny app: 

# Main (45 cols with actual data)
write_rds(adult, "../data/adult_main.rds", compress = "gz")

# Meta (45 cols with metadata)
write_rds(adult_meta, "../data/adult_meta.rds", compress = "gz")

# Full (90 cols combining the previous two)
write_rds(adult_full, "../data/adult_full.rds", compress = "gz")


         