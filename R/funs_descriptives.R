# Descriptives functions

build_flow_chart <- function(precleaned_data, moderators) {
  # Starting N of clients and centers contributing data for all 4 years
  data_flow_chart <- data.frame(Step = "Starting N- Centers contributing all 4 years", Clients = n_distinct(precleaned_data$UniqueClientID), Centers = n_distinct(precleaned_data$CcmhID))

  # 1+ attended individual session
  data <- mutate(precleaned_data, Is_appointment = ifelse(!is.na(AppointID), 1, 0))
  appts <- filter(data, Is_appointment == 1)
  ccaps <- filter(data, Is_ValidCCAPS == 1)
  appts <- filter(appts, ClientAttendance == "Attended") %>%
    filter(AppointmentCategory == 2|AppointmentCategory == 3|AppointmentCategory == 4)
  data_flow_chart2 <- rbind(data_flow_chart, data.frame(Step = "1+ attended individual appt", Clients = n_distinct(appts$UniqueClientID), Centers = n_distinct(appts$CcmhID)))

  # 3+ attended individual appts
  appts$duplicate <- duplicated(appts[c("UniqueClientID", "Date")])
  appts <- filter(appts, duplicate == FALSE) %>%
    select(-duplicate)
  appts <- group_by(appts, UniqueClientID) %>%
    filter(n() >= 3) %>%
    mutate(appt_seq = row_number(), appt_N = n()) %>%
    select(UniqueClientID, CcmhID, Date, appt_seq, appt_N)
  data_flow_chart3 <- rbind(data_flow_chart2, data.frame(Step = "3+ attended individual appt", Clients = n_distinct(appts$UniqueClientID), Centers = n_distinct(appts$CcmhID)))

  # 3+ individual appts with CCAPS
  ccaps$duplicate <- duplicated(ccaps[c("UniqueClientID", "Date")])
  ccaps <- filter(ccaps, duplicate == FALSE) %>%
    select(-duplicate)
  ccaps <- group_by(ccaps, UniqueClientID) %>%
    filter(n() >= 3) %>%
    mutate(CCAPS_seq = row_number(), CCAPS_N = n()) %>%
    select(UniqueClientID, CcmhID, Date, CCAPS_seq, CCAPS_N, Depression34:DI)
  combined <- merge(x = appts, y = ccaps, by = c("UniqueClientID", "Date", "CcmhID"), all.x = TRUE) %>%
    group_by(UniqueClientID) %>%
    mutate(CCAPS_N = max(CCAPS_N, na.rm = T), appt_N = max(appt_N, na.rm = T)) %>%
    mutate(matched_CCAPS_N = sum(!is.na(DI))) %>%
    filter(matched_CCAPS_N >= 3)
  combined20 <- filter(combined, appt_seq <= 20) %>%
    group_by(UniqueClientID) %>%
    mutate(matched_CCAPS_N = sum(!is.na(DI))) %>%
    filter(matched_CCAPS_N >= 3) %>%
    ungroup()
  data_flow_chart4 <- rbind(data_flow_chart3, data.frame(Step = "3+ individual appts with CCAPS", Clients = n_distinct(combined20$UniqueClientID), Centers = n_distinct(combined20$CcmhID)))

  # CCAPS at first appt
  combined_first <- group_by(combined20, UniqueClientID) %>%
    filter(!is.na(first(Depression34))) %>%
    mutate_at(vars(Depression34:DI), list(first = first)) %>%
    ungroup()
  data_flow_chart5 <- rbind(data_flow_chart4, data.frame(Step = "CCAPS at first appointment", Clients = n_distinct(combined_first$UniqueClientID), Centers = n_distinct(combined_first$CcmhID)))

  # Missing hx moderator
  combined_hx <- left_join(combined_first, moderators) %>%
    filter(!is.na(hospitalization))
  data_flow_chart6 <- rbind(data_flow_chart5, data.frame(Step = "Complete data on moderators", Clients = n_distinct(combined_hx$UniqueClientID), Centers = n_distinct(combined_hx$CcmhID)))

  rename(data_flow_chart6, `Data cleaning step` = Step, `Client N` = Clients, `Center N` = Centers)
}

calculate_alphas <- function(data_courses, change_data) {
  alpha_data <- filter(data_courses, Is_ValidCCAPS == 1) %>%
    filter(UniqueClientID %in% change_data$UniqueClientID) %>%
    arrange(UniqueClientID, Date) %>%
    group_by(UniqueClientID) %>%
    slice(1) %>%
    ungroup() %>%
    select(CCAPS_01:CCAPS_70)

  alpha_data$CCAPS_18r <- abs(alpha_data$CCAPS_18-4)
  alpha_data$CCAPS_39r <- abs(alpha_data$CCAPS_39-4)

  alphas <- data.frame(Subscale = c("Depression", "Generalized_Anxiety", "Social_Anxiety", "Academics", "Eating", "Hostility",  "Alcohol", "DI"), Alpha = NA)
  alphas[1,2] <- psychometric::alpha(dplyr::select(alpha_data, c(CCAPS_11, CCAPS_13,CCAPS_24, CCAPS_27, CCAPS_45, CCAPS_51))) #Depression34
  alphas[2,2] <- psychometric::alpha(dplyr::select(alpha_data, c(CCAPS_05, CCAPS_17,CCAPS_21, CCAPS_22, CCAPS_31, CCAPS_34))) #Anxiety34
  alphas[3,2] <- psychometric::alpha(dplyr::select(alpha_data, c(CCAPS_03,CCAPS_39r, CCAPS_46, CCAPS_49, CCAPS_52))) #Social Anxiety
  alphas[4,2] <- psychometric::alpha(dplyr::select(alpha_data, c(CCAPS_18r, CCAPS_57, CCAPS_59, CCAPS_66))) #Academics
  alphas[5,2] <- psychometric::alpha(dplyr::select(alpha_data, c(CCAPS_06, CCAPS_16, CCAPS_29))) #Eating
  alphas[6,2] <- psychometric::alpha(dplyr::select(alpha_data, c(CCAPS_36, CCAPS_40, CCAPS_48, CCAPS_58, CCAPS_64, CCAPS_68))) #Hostility
  alphas[7,2] <- psychometric::alpha(dplyr::select(alpha_data, c(CCAPS_30, CCAPS_33,CCAPS_54, CCAPS_63))) #Alcohol
  alphas[8,2] <- psychometric::alpha(dplyr::select(alpha_data, c(CCAPS_05, CCAPS_11, CCAPS_13, CCAPS_17, CCAPS_21, CCAPS_22, CCAPS_24,CCAPS_27, CCAPS_31, CCAPS_34, CCAPS_40, CCAPS_45,CCAPS_46, CCAPS_48, CCAPS_51, CCAPS_52, CCAPS_57, CCAPS_58, CCAPS_59, CCAPS_66))) # DI
  alphas[,2] <- round(alphas[,2], 2)
  alphas
}

# Session by session alert data
session_alerts <- function(data = roc_data_intermediate) {

  alert_boundaries <- CCMHr::alerts %>%
    rename(alert_score = score)

  cleaned_data <- select(data, UniqueClientID, Date, appt_seq, Depression34:DI) %>%
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
