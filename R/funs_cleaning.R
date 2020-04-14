# Functions

# Data processing functions------------------------------------------------
# Combining two TI data sets and removing schools that didn't contribute data all four years
# Also removing centers that contributed less than an average of 10 months over the 4 years
combine_TI_data <- function(raw_pre_data, raw_post_data) {
  data <- plyr::rbind.fill(raw_pre_data, raw_post_data) %>%
    group_by(CcmhID) %>%
    filter(n_distinct(Data_year) == 4) %>%
    ungroup() %>%
    mutate(month = lubridate::month(Date, label = T, abbr = T))

  centers <- group_by(data, CcmhID, Data_year) %>%
    mutate(n_months = n_distinct(month)) %>%
    group_by(CcmhID) %>%
    filter(mean(n_months) >= 10)

  filter(data, CcmhID %in% unique(centers$CcmhID))
}

# Combining CcmhIDs with the update dates provided by Karl
# The update date represents the day they downloaded the new instance of TI that allowed for the new profile report
# A couple centers were listed twice in the CCAPS-update data file from Karl. Not sure what's going on with those, but I'm removing them.
# Also removing centers that are missing the CCAPS update date.
get_update_dates <- function(combined_TI_data) {
  IDs <- openxlsx::read.xlsx("data/2018-09-20_membership.xlsx", sheet = 1) %>%
    select(CcmhID, ccmhmemberid = Activationkey) %>%
    mutate(ccmhmemberid = as.numeric(as.character(ccmhmemberid))) %>%
    filter(!is.na(ccmhmemberid) &  CcmhID != 0) %>%
    distinct()

  ccaps_update_dates <- openxlsx::read.xlsx("data/CCAPS-update-date.xlsx", sheet = 1, detectDates = T) %>%
    rename(update_date = Installed.Update.with.CCAPS.2015) %>%
    mutate(ccmhmemberid = as.numeric(as.character(ccmhmemberid))) %>%
    left_join(IDs) %>%
    filter(!is.na(CcmhID) & !is.na(update_date)) %>%
    group_by(ccmhmemberid) %>%
    filter(n() == 1) %>%
    ungroup()

  ccaps_start_dates <- group_by(combined_TI_data, CcmhID) %>%
    summarize(start_date = max(CcapsVer2015StartDate, na.rm = T)) %>%
    filter(!is.na(start_date)) %>%
    mutate(start_date =stringr::str_sub(start_date, 1, 10))

  left_join(ccaps_update_dates, ccaps_start_dates)

}

# Identifying centers that meet criteria for updating to new CCAPS version
# Centers had to have updated before 2016-07-01 and set their start date for the new version to before 2016-07-01
get_updated_centers <- function(ccaps_update_dates) {
  filter(ccaps_update_dates, update_date <= "2016-07-01" & start_date <= "2016-07-01")
}

# Creating data set of centers that contributed all 4 years and met CCAPS update criteria
create_precleaned_data <- function(combined_TI_data, included_centers) {
  filter(combined_TI_data, CcmhID %in% included_centers)
}


# Cleaning data for analyses ----------------------------------------------

# Clean rate of change
clean_roc <- function(precleaned_data, min_ccaps) {
  # First course of treatment
  data <- CCMHr::create_courses(precleaned_data, firstOnly = T) %>%
    mutate(feedback = ifelse(Date <= "2015-07-01", 0, 1))

  usethis::ui_info(glue::glue("Clients before cleaning: {n_distinct(data$UniqueClientID)}"))
  usethis::ui_info(glue::glue("Centers before cleaning: {n_distinct(data$CcmhID)}"))

  data <- mutate(data, Is_appointment = ifelse(!is.na(AppointID), 1, 0))
  appts <- filter(data, Is_appointment == 1)
  ccaps <- filter(data, Is_ValidCCAPS == 1)

  # Include only attended individual sessions
  usethis::ui_info(glue::glue("Clients with appt data before cleaning: {n_distinct(appts$UniqueClientID)}"))
  usethis::ui_info(glue::glue("Centers with appt data before cleaning: {n_distinct(appts$CcmhID)}"))

  appts <- filter(appts, ClientAttendance == "Attended") %>%
    filter(AppointmentCategory == 2|AppointmentCategory == 3|AppointmentCategory == 4)

  # Delete duplicate appointments
  appts$duplicate <- duplicated(appts[c("UniqueClientID", "Date")])
  appts <- filter(appts, duplicate == FALSE) %>%
    select(-duplicate)

  # Delete duplicate CCAPS
  usethis::ui_info(glue::glue("Clients with CCAPS data before cleaning: {n_distinct(ccaps$UniqueClientID)}"))
  usethis::ui_info(glue::glue("Centers with CCAPS data before cleaning: {n_distinct(ccaps$CcmhID)}"))

  ccaps$duplicate <- duplicated(ccaps[c("UniqueClientID", "Date")])
  ccaps <- filter(ccaps, duplicate == FALSE) %>%
    select(-duplicate)

  usethis::ui_info(glue::glue("Clients before minimum of 3 CCAPS and appts: {n_distinct(intersect(appts$UniqueClientID, ccaps$UniqueClientID))}"))

  # 3+ CCAPS
  ccaps <- group_by(ccaps, UniqueClientID) %>%
    filter(n() >= min_ccaps) %>%
    mutate(CCAPS_seq = row_number(), CCAPS_N = n()) %>%
    select(UniqueClientID, CcmhID, Date, CCAPS_seq, CCAPS_N, Depression34:DI)

  # 3+ appts
  appts <- group_by(appts, UniqueClientID) %>%
    filter(n() >= min_ccaps) %>%
    mutate(appt_seq = row_number(), appt_N = n()) %>%
    select(UniqueClientID, CcmhID, Date, appt_seq, appt_N)

  usethis::ui_info(glue::glue("Clients after minimum of 3 CCAPS and appts: {n_distinct(intersect(appts$UniqueClientID, ccaps$UniqueClientID))}"))
  usethis::ui_info(glue::glue("Centers before minimum of 3 CCAPS and appts: {n_distinct(intersect(appts$CcmhID, ccaps$CcmhID))}"))

  # Combine data, 3+ matched CCAPS
  combined <- merge(x = appts, y = ccaps, by = c("UniqueClientID", "Date", "CcmhID"), all.x = TRUE) %>%
    group_by(UniqueClientID) %>%
    mutate(CCAPS_N = max(CCAPS_N, na.rm = T), appt_N = max(appt_N, na.rm = T)) %>%
    mutate(matched_CCAPS_N = sum(!is.na(DI))) %>%
    filter(matched_CCAPS_N >= min_ccaps)

  usethis::ui_info(glue::glue("Clients after combining: {n_distinct(combined$UniqueClientID)}"))
  usethis::ui_info(glue::glue("Centers after combining: {n_distinct(combined$CcmhID)}"))

  # CCAPS at first appointment
  first_percent <- group_by(combined, UniqueClientID) %>%
    summarize(first_ccaps = ifelse(!is.na(first(Depression34)), 1, 0)) %>%
    janitor::tabyl(first_ccaps) %>%
    janitor::adorn_pct_formatting() %>%
    filter(first_ccaps == 0) %>%
    pull(percent)
  usethis::ui_info(paste0("Percentage of clients without a CCAPS at their first appt: ", first_percent))

  combined <- group_by(combined, UniqueClientID) %>%
    filter(!is.na(first(Depression34))) %>%
    mutate_at(vars(Depression34:DI), list(first = first)) %>%
    ungroup()

  above20 <- group_by(combined, UniqueClientID) %>%
    summarize(appt_N = ifelse(max(appt_N) > 20, 1, 0)) %>%
    janitor::tabyl(appt_N) %>%
    janitor::adorn_pct_formatting() %>%
    filter(appt_N == 1) %>%
    pull(percent)
  usethis::ui_info(paste0("Percentage of clients with appointments over 20: ", above20))

  combined20 <- filter(combined, appt_seq <= 20) %>%
    group_by(UniqueClientID) %>%
    mutate(matched_CCAPS_N = sum(!is.na(DI))) %>%
    filter(matched_CCAPS_N >= min_ccaps) %>%
    ungroup()

  # Bin first CCAPS
  combined20 <- create_CCAPS_bins(combined20)

  # Anxiety 23, 24, 25
  # Social Anxiety 19, 20, 21
  # Academics 16, 17
  # Eating 11, 12, 13

  cant_alert <- group_by(combined20, UniqueClientID) %>%
    slice(1) %>%
    ungroup() %>%
    summarize(Anxiety = mean(anxiety_bin >= 23),
              social_anxiety = mean(social_anxiety_bin >= 20),
              academics = mean(academics_bin >= 16),
              eating = mean(eating_bin >= 11)) %>%
    mutate_all(scales::percent_format(accuracy = .1))
  usethis::ui_info(glue::glue("Percentage of clients on {c('Anxiety', 'Social Anxiety', 'Academics', 'Eating')} that can't alert: {cant_alert}"))

  combined_long <- select(combined20, UniqueClientID:DI) %>%
    pivot_longer(cols = -c(UniqueClientID:CCAPS_N), names_to = "subscale", values_to = "score")
  combined_long <- select(combined20, contains("bin")) %>%
    pivot_longer(cols = contains("bin"), names_to = "bin_subscale", values_to = "bin") %>%
    cbind(combined_long, .) %>%
    filter(subscale != "Anxiety34" | bin != 23) %>%
    filter(subscale != "Anxiety34" | bin != 24) %>%
    filter(subscale != "Anxiety34" | bin != 25) %>%
    filter(subscale != "Social_Anxiety34" | bin != 19) %>%
    filter(subscale != "Social_Anxiety34" | bin != 20) %>%
    filter(subscale != "Social_Anxiety34" | bin != 21) %>%
    filter(subscale != "Academics34" | bin != 16) %>%
    filter(subscale != "Academics34" | bin != 17) %>%
    filter(subscale != "Eating34" | bin != 11) %>%
    filter(subscale != "Eating34" | bin != 12) %>%
    filter(subscale != "Eating34" | bin != 13)

  usethis::ui_info(glue::glue("Clients after removing those that can't alert: {n_distinct(combined_long$UniqueClientID)}"))

  # Code alerts, scores > boundary
  alert_boundaries <- CCMHr::alerts %>%
    rename(alert_score = score)

  combined_alerts <- mutate(combined_long, appt_seq = appt_seq - 1) %>%
    left_join(alert_boundaries, by = c("subscale" = "subscale", "appt_seq" = "session", "bin" = "bin")) %>%
    mutate(alert = ifelse(score > alert_score, 1, 0)) %>%
    group_by(UniqueClientID) %>%
    mutate(alert_any = max(alert, na.rm = T)) %>%
    select(-c(bin_subscale, bin, alert_score))

  any_alert <- group_by(combined_alerts, UniqueClientID) %>%
    slice(1) %>%
    ungroup() %>%
    summarize(alert_any = mean(alert_any)) %>%
    mutate(alert_any = scales::percent(alert_any, accuracy = .1))
  usethis::ui_info(paste0("Percentage of clients alerting on any subscale at any point in treatment: ", any_alert))

  # Combine data and create CCAPS frequency variable
  scores_wide <- select(combined_alerts, UniqueClientID, CcmhID, Date, appt_seq, subscale, score) %>%
    pivot_wider(names_from = subscale, values_from = score)
  alert_wide <- select(combined_alerts, UniqueClientID, CcmhID, Date, appt_seq, subscale, alert, alert_any) %>%
    mutate(subscale = paste0(subscale, "_alert")) %>%
    pivot_wider(names_from = subscale, values_from = alert) %>%
    group_by(UniqueClientID) %>%
    summarize(across(contains("alert"), max, na.rm = T)) %>%
    mutate_all(function(x) ifelse(!is.finite(x), NA, x))


  cleaned_data <- full_join(scores_wide, alert_wide) %>%
    mutate(inv_appt_seq = (-1/(appt_seq+1))+1,
           log_appt_seq = log(1+appt_seq)) %>%
    mutate(feedback = ifelse(Date <= "2015-07-01", 0, 1))

  oneline <- group_by(cleaned_data, UniqueClientID) %>%
    summarize(appt_N = n(),
              CCAPS_n = sum(!is.na(DI))) %>%
    mutate(ccaps_freq = CCAPS_n/appt_N,
           appt_n = scale(appt_N),
           ccaps_freq = scale(ccaps_freq))

  cleaned_data <- left_join(cleaned_data, oneline)

  # Alert rates by subscale
  # cleaned_data %>% group_by(UniqueClientID) %>%
  #   summarize_at(vars(contains("alert")), list(function(x) max(hablar::s(x)))) %>%
  #   summarize_at(vars(contains("alert")), list(mean), na.rm = T) %>%
  #   View()
  #
  # cleaned_data %>% group_by(UniqueClientID, feedback) %>%
  #   summarize_at(vars(contains("alert")), list(function(x) max(hablar::s(x)))) %>%
  #   group_by(feedback) %>%
  #   summarize_at(vars(contains("alert")), list(mean), na.rm = T) %>%
  #   View()

  first_ccaps <- group_by(cleaned_data, UniqueClientID, CcmhID) %>%
    summarize(across(Depression34:DI, first, .names = "{col}_first")) %>%
    ungroup() %>%
    CCMHr::ccaps34_cuts(data = ., version = "2018", first = T) %>%
    mutate(across(Depression34_first:DI_first, scale, .names = "{col}_scaled")) %>%
    # mutate(Depression34_first = ifelse(Depression34_low_cut == 0, NA, Depression34_first),
    #        Anxiety34_first = ifelse(Anxiety34_low_cut == 0, NA, Anxiety34_first),
    #        Social_Anxiety34_first = ifelse(Social_Anxiety34_low_cut == 0, NA, Social_Anxiety34_first),
    #        Academics34_first = ifelse(Academics34_low_cut == 0, NA, Academics34_first),
    #        Eating34_first = ifelse(Eating34_low_cut == 0, NA, Eating34_first),
    #        Hostility34_first = ifelse(Hostility34_low_cut == 0, NA, Hostility34_first),
    #        Alcohol34_first = ifelse(Alcohol34_low_cut == 0, NA, Alcohol34_first),
    #        DI_first = ifelse(DI_low_cut == 0, NA, DI_first)) %>%
    group_by(CcmhID) %>%
    mutate(across(Depression34_first_scaled:DI_first_scaled, function(x) mean(x, na.rm = T), .names = "{col}_center")) %>%
    ungroup() %>%
    mutate(Depression34_first_centered = Depression34_first_scaled - Depression34_first_scaled_center,
           Anxiety34_first_centered = Anxiety34_first_scaled - Anxiety34_first_scaled_center,
           Social_Anxiety34_first_centered = Social_Anxiety34_first_scaled - Social_Anxiety34_first_scaled_center,
           Academics34_first_centered = Academics34_first_scaled - Academics34_first_scaled_center,
           Eating34_first_centered = Eating34_first_scaled - Eating34_first_scaled_center,
           Hostility34_first_centered = Hostility34_first_scaled - Hostility34_first_scaled_center,
           Alcohol34_first_centered = Alcohol34_first_scaled - Alcohol34_first_scaled_center,
           DI_first_centered = DI_first_scaled - DI_first_scaled_center) %>%
    select(-c(Depression34_first_scaled:DI_first_scaled))

  cleaned_data <- left_join(cleaned_data, first_ccaps)
  names(cleaned_data) <- stringr::str_remove(names(cleaned_data), "_scaled")

  cleaned_data
}

# Anchor ROC scores
anchor_roc <- function(roc_data_intermediate) {
  roc_data_intermediate <- mutate(roc_data_intermediate, Depression34 = Depression34 - Depression34_first,
                     Anxiety34 = Anxiety34 - Anxiety34_first,
                     Social_Anxiety34 = Social_Anxiety34 - Social_Anxiety34_first,
                     Academics34 = Academics34 - Academics34_first,
                     Eating34 = Eating34 - Eating34_first,
                     Hostility34 = Hostility34 - Hostility34_first,
                     Alcohol34 = Alcohol34 - Alcohol34_first,
                     DI = DI - DI_first)

  roc_data_intermediate
}

# Clean change
clean_change <- function(roc_data_intermediate) {
  # First and last CCAPS
  change_data <- roc_data_intermediate %>%
    group_by(UniqueClientID, CcmhID) %>%
    summarize(across(Depression34:DI, first_present, .names = "{col}_first"), across(Depression34:DI, last_present, .names = "{col}_last"), across(Depression34_first_centered:DI_first_centered, first), across(Depression34_first_center:DI_first_center, first), across(Depression34_low_cut:DI_low_cut, first)) %>%
    # Positive change is improvement
    mutate(Depression34_change = Depression34_first - Depression34_last,
           Anxiety34_change = Anxiety34_first - Anxiety34_last,
           Social_Anxiety34_change = Social_Anxiety34_first - Social_Anxiety34_last,
           Academics34_change = Academics34_first - Academics34_last,
           Eating34_change = Eating34_first - Eating34_last,
           Hostility34_change = Hostility34_first - Hostility34_last,
           Alcohol34_change = Alcohol34_first - Alcohol34_last,
           DI_change = DI_first - DI_last) %>%
    select(-contains("_last"))

  change_data_other <- suppressWarnings(roc_data_intermediate %>%
    group_by(UniqueClientID) %>%
    summarize_at(vars(ccaps_freq, feedback, appt_n, contains("alert")), list(max), na.rm = T) %>%
    mutate_all(function(x) ifelse(!is.finite(x), NA, x)))

  change_data <- inner_join(change_data, change_data_other)
  change_data
}

# Clean deterioration
clean_deterioration <- function(change_data) {
  # Clients who can't deteriorate because they start too high
  deter_data <- mutate(change_data,
                        Depression_include = ifelse(Depression34_first <= 2.95, 1, 0),
                        Anxiety_include = ifelse(Anxiety34_first <= 2.94, 1, 0),
                        SocialAnxiety_include = ifelse(Social_Anxiety34_first <= 2.91, 1, 0),
                        Academics_include = ifelse(Academics34_first <= 2.6, 1, 0),
                        Eating_include = ifelse(Eating34_first <= 2.67, 1, 0),
                        Hostility_include = ifelse(Hostility34_first <= 3.01, 1, 0),
                        Alcohol_include = ifelse(Alcohol34_first <= 2.89, 1, 0),
                        DI_include = ifelse(DI_first <= 3.2, 1, 0)) %>%
    # Code deterioration
    mutate(Depression34_deter = ifelse(Depression34_change <= -1.05, 1, 0),
           Anxiety34_deter = ifelse(Anxiety34_change <= -1.06, 1, 0),
           Social_Anxiety34_deter = ifelse(Social_Anxiety34_change <= -1.09, 1, 0),
           Academics34_deter = ifelse(Academics34_change <= -1.40, 1, 0),
           Eating34_deter = ifelse(Eating34_change <= -1.33, 1, 0),
           Hostility34_deter = ifelse(Hostility34_change <= -.99, 1, 0),
           Alcohol34_deter = ifelse(Alcohol34_change <= -1.11, 1, 0),
           DI_deter = ifelse(DI_change <= -.8, 1, 0)) %>%
    mutate(Depression34_deter = ifelse(Depression_include == 1, Depression34_deter, NA),
           Anxiety34_deter = ifelse(Anxiety_include == 1, Anxiety34_deter, NA),
           Social_Anxiety34_deter = ifelse(SocialAnxiety_include == 1, Social_Anxiety34_deter, NA),
           Academics34_deter = ifelse(Academics_include == 1, Academics34_deter, NA),
           Eating34_deter = ifelse(Eating_include == 1, Eating34_deter, NA),
           Hostility34_deter = ifelse(Hostility_include == 1, Hostility34_deter, NA),
           Alcohol34_deter = ifelse(Alcohol_include == 1, Alcohol34_deter, NA),
           DI_deter = ifelse(DI_include == 1, DI_deter, NA))

  # Report how many clients were removed and couldn't deteriorate
  # deter_data %>%
  #   summarize_at(vars(Depression_include:DI_include), list(function(x) mean(x == 0, na.rm = T))) %>%
  #   View("excluded")
  #
  # deter_data %>%
  #   summarize_at(vars(Depression_deter:DI_deter), list(function(x) mean(x == 0, na.rm = T))) %>%
  #   View("deterioration")
  #
  # deter_data %>%
  #   group_by(feedback) %>%
  #   summarize_at(vars(Depression_deter:DI_deter), list(function(x) mean(x == 0, na.rm = T))) %>%
  #   View("deterioration_feedback")

  deter_data
}

# Clean moderators
create_moderators <- function(precleaned_data) {
  # Moderators:
    # Alert- done
    # Total sessions- done
    # Baseline CCAPS- done
    # CCAPS frequency- done
    # Prior hospitalization

  # First CCAPS, SDS, CLICC
  filter(precleaned_data, Has_SDS == 1) %>%
    group_by(UniqueClientID) %>%
    arrange(UniqueClientID, Date) %>%
    slice(1) %>%
    select(UniqueClientID, SDS_64) %>%
    ungroup() %>%
    mutate(hospitalization = ifelse(SDS_64 > 1, 1, 0))

}

# Session by session alert data
session_alerts <- function(data = roc_data_intermediate) {

  alert_boundaries <- CCMHr::alerts %>%
    rename(alert_score = score)

  cleaned_data <- select(data, UniqueClientID, appt_seq, Depression34:DI) %>%
    group_by(UniqueClientID) %>%
    mutate(across(c(Depression34:DI), first, .names = "{col}_first"))

  # Bin first CCAPS
  cleaned_data <- create_CCAPS_bins(cleaned_data)

  # Anxiety 23, 24, 25
  # Social Anxiety 19, 20, 21
  # Academics 16, 17
  # Eating 11, 12, 13

  scores_long <- select(cleaned_data, UniqueClientID:DI) %>%
    pivot_longer(cols = Depression34:DI, names_to = "subscale", values_to = "score") %>%
    ungroup()
  combined_long <- ungroup(cleaned_data) %>%
    select(contains("bin")) %>%
    pivot_longer(cols = contains("bin"), names_to = "bin_subscale", values_to = "bin") %>%
    cbind(scores_long, .) %>%
    filter(subscale != "Anxiety34" | bin != 23) %>%
    filter(subscale != "Anxiety34" | bin != 24) %>%
    filter(subscale != "Anxiety34" | bin != 25) %>%
    filter(subscale != "Social_Anxiety34" | bin != 19) %>%
    filter(subscale != "Social_Anxiety34" | bin != 20) %>%
    filter(subscale != "Social_Anxiety34" | bin != 21) %>%
    filter(subscale != "Academics34" | bin != 16) %>%
    filter(subscale != "Academics34" | bin != 17) %>%
    filter(subscale != "Eating34" | bin != 11) %>%
    filter(subscale != "Eating34" | bin != 12) %>%
    filter(subscale != "Eating34" | bin != 13)

  combined_alerts <- left_join(combined_long, alert_boundaries, by = c("subscale" = "subscale", "appt_seq" = "session", "bin" = "bin")) %>%
    mutate(alert = ifelse(score > alert_score, 1, 0)) %>%
    filter(!is.na(alert)) %>%
    mutate(appt_seq = appt_seq + 1)

  combined_alerts
}
