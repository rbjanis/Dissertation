# Analysis functions


# Pre post analysis -------------------------------------------------------
prepost_analyses <- function(subscale, data = change_data) {
  outcome_change <- paste0(rlang::as_name(enquo(subscale)), "_change")
  outcome_alert <- paste0(rlang::as_name(enquo(subscale)), "_alert")
  outcome_first <- paste0(rlang::as_name(enquo(subscale)), "_first")

  client_data <- ungroup(data) %>%
    filter(!is.na(!!as.name(outcome_change))) %>%
    mutate(outcome_change = !!as.name(outcome_change),
           outcome_alert = !!as.name(outcome_alert),
           outcome_first = !!as.name(outcome_first))

  rm(outcome_change, outcome_alert, outcome_first)

  client_data <- mutate(client_data, across(c(outcome_change, feedback, hospitalization, outcome_alert), scale)) %>%
    group_by(CcmhID) %>%
    mutate(across(c(ccaps_freq, appt_n, outcome_first), mean, .names = "{col}_center")) %>%
    ungroup() %>%
    mutate(ccaps_freq = ccaps_freq - ccaps_freq_center,
           appt_n = appt_n - appt_n_center,
           outcome_first = outcome_first - outcome_first_center,
           across(c(ccaps_freq, appt_n, outcome_first), scale)) %>%
    select(-contains("_center"))

  model_data <- group_by(client_data, CcmhID) %>%
    summarize(across(c(ccaps_freq, appt_n, outcome_first), mean, .names = "{col}_center")) %>%
    mutate(across(c(ccaps_freq_center, appt_n_center, outcome_first_center), scale)) %>%
    left_join(client_data, .)

  # model_data <- ungroup(data) %>%
  #   filter(!is.na(!!as.name(outcome_change))) %>%
  #   mutate(outcome_change = !!as.name(outcome_change),
  #          outcome_first_centered = !!as.name(outcome_first_centered),
  #          outcome_first_center = !!as.name(outcome_first_center),
  #          outcome_alert = !!as.name(outcome_alert)) %>%
  #   mutate(across(c(outcome_change, feedback, outcome_alert, hospitalization), scale))

  # alert_only <- ungroup(model_data) %>%
  #   filter(outcome_alert == 1)

  null <- lme(outcome_change ~ 1,
                     random =~ 1| CcmhID,
                     data = model_data,
                     na.action=na.omit,
                     control=list(opt="optim"),
                     method="REML")

  feedback <- lme(outcome_change ~ 1 + feedback,
              random =~ 1| CcmhID,
              data = model_data,
              na.action=na.omit,
              control=list(opt="optim"),
              method="REML")

  feedback_random <- lme(outcome_change ~ 1 + feedback,
                  random =~ 1 + feedback| CcmhID,
                  data = model_data,
                  na.action=na.omit,
                  control=list(opt="optim"),
                  method="REML")

  if (subscale %in% c("Depression34", "Anxiety34", "Alcohol34", "DI")) {
    main <- lme(outcome_change ~ 1 + feedback + outcome_alert + outcome_first + outcome_first_center + ccaps_freq + ccaps_freq_center + appt_n + appt_n_center + hospitalization,
                      random =~ 1 + feedback| CcmhID,
                      data = model_data,
                      na.action=na.omit,
                      control=list(opt="optim"),
                      method="REML")

    moderators <- lme(outcome_change ~ 1 + feedback + feedback*outcome_alert + feedback*outcome_first + feedback*outcome_first_center + feedback*ccaps_freq + feedback*ccaps_freq_center + feedback*appt_n + feedback*appt_n_center + feedback*hospitalization,
                      random =~ 1 + feedback| CcmhID,
                      data = model_data,
                      na.action=na.omit,
                      control=list(opt="optim"),
                      method="REML")
  } else {
    main <- lme(outcome_change ~ 1 + feedback + outcome_alert + outcome_first + outcome_first_center + ccaps_freq + ccaps_freq_center + appt_n + appt_n_center + hospitalization,
                      random =~ 1| CcmhID,
                      data = model_data,
                      na.action=na.omit,
                      control=list(opt="optim"),
                      method="REML")

    moderators <- lme(outcome_change ~ 1 + feedback + feedback*outcome_alert + feedback*outcome_first + feedback*outcome_first_center + feedback*ccaps_freq + feedback*ccaps_freq_center + feedback*appt_n + feedback*appt_n_center + feedback*hospitalization,
                      random =~ 1| CcmhID,
                      data = model_data,
                      na.action=na.omit,
                      control=list(opt="optim"),
                      method="REML")
  }

  # if (subscale %in% c("Depression34", "Anxiety34", "Social_Anxiety34", "Alcohol34", "DI")) {
  #   alert_model <- lme(outcome_change ~ 1 + feedback,
  #                     random =~ 1 + feedback| CcmhID,
  #                     data = alert_only,
  #                     na.action=na.omit,
  #                     control=list(opt="optim"),
  #                     method="ML")
  # } else {
  #   alert_model <- lme(outcome_change ~ 1 + feedback,
  #                     random =~ 1| CcmhID,
  #                     data = alert_only,
  #                     na.action=na.omit,
  #                     control=list(opt="optim"),
  #                     method="ML")
  # }

  return(list(null = null, feedback = feedback, feedback_random = feedback_random, main = main, moderators = moderators))
}


# Deterioration -----------------------------------------------------------
get_deter_rates <- function(subscale, data = deter_data) {
  outcome_deter <- paste0(rlang::as_name(enquo(subscale)), "_deter")

  model_data <- filter(data, !is.na(!!as.name(outcome_deter))) %>%
    mutate(outcome_deter = !!as.name(outcome_deter))


  model_data %>%
    group_by(feedback) %>%
    summarize({{subscale}} := mean(outcome_deter == 1, na.rm = T)) %>%
    ungroup() %>%
    select(-feedback)
}




deterioration_analyses <- function(subscale, data = deter_data) {
  outcome_deter <- paste0(rlang::as_name(enquo(subscale)), "_deter")
  outcome_alert <- paste0(rlang::as_name(enquo(subscale)), "_alert")
  outcome_first <- paste0(rlang::as_name(enquo(subscale)), "_first")

  client_data <- ungroup(data) %>%
    filter(!is.na(!!as.name(outcome_deter))) %>%
    mutate(outcome_deter = !!as.name(outcome_deter),
           outcome_alert = !!as.name(outcome_alert),
           outcome_first = !!as.name(outcome_first))

  rm(outcome_deter, outcome_alert, outcome_first)

  client_data <- mutate(client_data, across(c(feedback, hospitalization, outcome_alert), scale)) %>%
    group_by(CcmhID) %>%
    mutate(across(c(ccaps_freq, appt_n, outcome_first), mean, .names = "{col}_center")) %>%
    ungroup() %>%
    mutate(ccaps_freq = ccaps_freq - ccaps_freq_center,
           appt_n = appt_n - appt_n_center,
           outcome_first = outcome_first - outcome_first_center,
           across(c(ccaps_freq, appt_n, outcome_first), scale)) %>%
    select(-contains("_center"))

  model_data <- group_by(client_data, CcmhID) %>%
    summarize(across(c(ccaps_freq, appt_n, outcome_first), mean, .names = "{col}_center")) %>%
    mutate(across(c(ccaps_freq_center, appt_n_center, outcome_first_center), scale)) %>%
    left_join(client_data, .)

  null <- glmer(outcome_deter ~ 1 + (1 | CcmhID),
                data = model_data,
                family = binomial(link = "logit"),
                control=glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000)))

  feedback <- glmer(outcome_deter ~ 1 + feedback + (1 | CcmhID),
                    data = model_data,
                    family = binomial(link = "logit"),
                    control=glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000)))

  feedback_random <- glmer(outcome_deter ~ 1 + feedback + (1 + feedback| CcmhID),
                           data = model_data,
                           family = binomial(link = "logit"),
                           control=glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000)))

  moderators <- glmer(outcome_deter ~ 1 + feedback + feedback*outcome_first + feedback*outcome_first_center + feedback*ccaps_freq + feedback*ccaps_freq_center + feedback*appt_n + feedback*appt_n_center + feedback*hospitalization + (1 | CcmhID),
                    data = model_data,
                    family = binomial(link = "logit"),
                    control=glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000)))

  # alert_model <- glmer(outcome_deter ~ 1 + feedback + (1 + feedback| CcmhID),
  #                          data = alert_only,
  #                          family = binomial(link = "logit"),
  #                          control=glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000)))

  return(list(null = null, feedback = feedback, feedback_random = feedback_random, moderators = moderators))
}

deterioration_omnibus <- function(data) {
  model_data <- rowwise(data) %>%
    mutate(any_deter = max(c_across(Depression34_deter:DI_deter), na.rm = T),
           any_deter = ifelse(!is.finite(any_deter), NA, any_deter))

  null <- glmer(any_deter ~ 1 + (1 | CcmhID),
                data = model_data,
                family = binomial(link = "logit"),
                control=glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000)))

  feedback <- glmer(any_deter ~ 1 + feedback + (1 | CcmhID),
                    data = model_data,
                    family = binomial(link = "logit"),
                    control=glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000)))

  feedback_random <- glmer(any_deter ~ 1 + feedback + (1 + feedback| CcmhID),
                           data = model_data,
                           family = binomial(link = "logit"),
                           control=glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000)))

  moderators <- glmer(any_deter ~ 1 + feedback + feedback*ccaps_freq + feedback*appt_n + feedback*hospitalization + (1 | CcmhID),
                      data = model_data,
                      family = binomial(link = "logit"),
                      control=glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000)))

  return(list(null = null, feedback = feedback, feedback_random = feedback_random, moderators = moderators))
}

# Rate of change ----------------------------------------------------------
test_change_shape <- function(subscale, data = roc_data_intermediate) {
  outcome_temp <- paste0(rlang::as_name(enquo(subscale)))

  model_data <- ungroup(data) %>%
    filter(!is.na(!!as.name(outcome_temp))) %>%
    mutate(outcome = !!as.name(outcome_temp),
           log10_appt_seq = log10(1+appt_seq),
           log_appt_seq = log_appt_seq/sd(log_appt_seq),
           inv_appt_seq = inv_appt_seq/sd(inv_appt_seq),
           appt_seq = appt_seq/sd(appt_seq)) %>%
    mutate(outcome = scale(outcome))

  loglinear <- lmer(outcome ~ 1 + log_appt_seq +
                      (1 + log_appt_seq | CcmhID) +
                      (1 + log_appt_seq | CcmhID:UniqueClientID), data=model_data, REML=F)

  quad <- lmer(outcome ~ 1 + appt_seq + I(appt_seq^2) +
                 (1 + appt_seq | CcmhID) +
                 (1 + appt_seq | CcmhID:UniqueClientID), data=model_data, REML=F)

  inverse <- lmer(outcome ~ 1 + inv_appt_seq +
                    (1 + inv_appt_seq | CcmhID) +
                    (1 + inv_appt_seq | CcmhID:UniqueClientID), data=model_data, REML=F)

  return(list(loglinear = loglinear, quad = quad, inverse = inverse))
}


roc_analyses <- function(subscale, data = roc_data_intermediate) {
  outcome_temp <- paste0(rlang::as_name(enquo(subscale)))
  outcome_first_temp <- paste0(rlang::as_name(enquo(subscale)), "_first")
  outcome_alert_temp <- paste0(rlang::as_name(enquo(subscale)), "_alert")

  client_data <- ungroup(data) %>%
    mutate(outcome_first = !!as.name(outcome_first_temp),
           outcome_alert = !!as.name(outcome_alert_temp),
           outcome = !!as.name(outcome_temp)) %>%
    filter(!is.na(outcome)) %>%
    group_by(CcmhID, UniqueClientID) %>%
    summarize(across(c(outcome_first, feedback, hospitalization, outcome_alert, ccaps_freq, appt_n), first)) %>%
    group_by(CcmhID) %>%
    mutate(across(c(ccaps_freq, appt_n, outcome_first), mean, .names = "{col}_center")) %>%
    ungroup() %>%
    mutate(ccaps_freq = ccaps_freq - ccaps_freq_center,
           appt_n = appt_n - appt_n_center,
           outcome_first = outcome_first - outcome_first_center) %>%
    select(-contains("_center"))

  center_data <- group_by(client_data, CcmhID) %>%
    summarize(across(c(ccaps_freq, appt_n, outcome_first), function(x) mean(x, na.rm = T), .names = "{col}_center")) %>%
    mutate(across(c(ccaps_freq_center, appt_n_center, outcome_first_center), scale))

  client_data <- mutate(client_data, across(c(feedback, outcome_alert, outcome_first, ccaps_freq, appt_n, hospitalization), scale)) %>%
    select(UniqueClientID, feedback, outcome_alert, outcome_first, ccaps_freq, appt_n, hospitalization)

  model_data <- ungroup(data) %>%
    mutate(outcome = !!as.name(outcome_temp)) %>%
    filter(!is.na(outcome)) %>%
    select(UniqueClientID, CcmhID, outcome, log_appt_seq, inv_appt_seq, Depression34:DI) %>%
    left_join(client_data) %>%
    left_join(center_data) %>%
    mutate(log_appt_seq = log_appt_seq/sd(log_appt_seq),
           outcome = scale(outcome)) %>%
    mutate(across(is.matrix, as.numeric))


    null <- lmer(outcome ~ 1 + log_appt_seq + feedback +
                   (1 + log_appt_seq | CcmhID) +
                   (1 + log_appt_seq | CcmhID:UniqueClientID), data=model_data, REML=F)

    feedback <- lmer(outcome ~ 1 + log_appt_seq*feedback +
                       (1 + log_appt_seq | CcmhID) +
                       (1 + log_appt_seq | CcmhID:UniqueClientID), data=model_data, REML=F)

    feedback_random <- lmer(outcome ~ 1 + log_appt_seq*feedback +
                              (1 + log_appt_seq + log_appt_seq:feedback| CcmhID) +
                              (1 + log_appt_seq | CcmhID:UniqueClientID), data=model_data, REML=F)

    if (subscale %in% c("Academics34", "Eating34")) {
      moderators <- lmer(outcome ~ 1 + log_appt_seq*feedback*outcome_first + log_appt_seq*feedback*outcome_first_center + log_appt_seq*feedback*outcome_alert + log_appt_seq*feedback*ccaps_freq + log_appt_seq*feedback*ccaps_freq_center + log_appt_seq*feedback*appt_n + log_appt_seq*feedback*appt_n_center + log_appt_seq*feedback*hospitalization +
                           (1 + log_appt_seq | CcmhID) +
                           (0 + log_appt_seq | CcmhID:UniqueClientID), data=model_data, REML=T)
    } else {
      moderators <- lmer(outcome ~ 1 + log_appt_seq*feedback*outcome_first + log_appt_seq*feedback*outcome_first_center + log_appt_seq*feedback*outcome_alert + log_appt_seq*feedback*ccaps_freq + log_appt_seq*feedback*ccaps_freq_center + log_appt_seq*feedback*appt_n + log_appt_seq*feedback*appt_n_center + log_appt_seq*feedback*hospitalization +
                           (1 + log_appt_seq + log_appt_seq:feedback| CcmhID) +
                           (0 + log_appt_seq | CcmhID:UniqueClientID), data=model_data, REML=T)
    }


    # alert_model <- lme(outcome ~ 0 + log_appt_seq + log_appt_seq:feedback,
    #                 random =~ 0 + log_appt_seq| CcmhID/UniqueClientID,
    #                 data = alert_only,
    #                 na.action=na.omit,
    #                 control=list(opt="optim"),
    #                 method="ML")

  return(list(null = null, feedback = feedback, feedback_random = feedback_random, moderators = moderators))
}

