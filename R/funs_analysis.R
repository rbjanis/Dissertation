# Analysis functions


# Pre post analysis -------------------------------------------------------
prepost_analyses <- function(subscale, data = change_data) {
  outcome_change <- paste0(rlang::as_name(enquo(subscale)), "_change")
  outcome_cut <- paste0(rlang::as_name(enquo(subscale)), "_low_cut")
  outcome_first_centered <- paste0(rlang::as_name(enquo(subscale)), "_first_centered")
  outcome_first_center <- paste0(rlang::as_name(enquo(subscale)), "_first_center")
  outcome_alert <- paste0(rlang::as_name(enquo(subscale)), "_alert")

  model_data <- ungroup(data) %>%
    filter(!is.na(!!as.name(outcome_change))) %>%
    mutate(outcome_change = !!as.name(outcome_change),
           outcome_first_centered = !!as.name(outcome_first_centered),
           outcome_first_center = !!as.name(outcome_first_center),
           outcome_alert = !!as.name(outcome_alert))

  alert_only <- ungroup(data) %>%
    filter(!is.na(!!as.name(outcome_change)) & !!as.name(outcome_alert) == 1) %>%
    mutate(outcome_change = !!as.name(outcome_change))

  null <- lme(outcome_change ~ 1,
                     random =~ 1| CcmhID,
                     data = model_data,
                     na.action=na.omit,
                     control=list(opt="optim"),
                     method="ML")

  feedback <- lme(outcome_change ~ 1 + feedback,
              random =~ 1| CcmhID,
              data = model_data,
              na.action=na.omit,
              control=list(opt="optim"),
              method="ML")

  feedback_random <- lme(outcome_change ~ 1 + feedback,
                  random =~ 1 + feedback| CcmhID,
                  data = model_data,
                  na.action=na.omit,
                  control=list(opt="optim"),
                  method="ML")

  if (subscale %in% c("Depression34", "Anxiety34", "Alcohol34", "DI")) {
    main <- lme(outcome_change ~ 1 + feedback + outcome_alert + outcome_first_centered + outcome_first_center + ccaps_freq + appt_n + hospitalization,
                      random =~ 1 + feedback| CcmhID,
                      data = model_data,
                      na.action=na.omit,
                      control=list(opt="optim"),
                      method="ML")

    moderators <- lme(outcome_change ~ 1 + feedback + feedback*outcome_alert + feedback*outcome_first_centered + feedback*outcome_first_center + feedback*ccaps_freq + feedback*appt_n + feedback*hospitalization,
                      random =~ 1 + feedback| CcmhID,
                      data = model_data,
                      na.action=na.omit,
                      control=list(opt="optim"),
                      method="ML")
  } else {
    main <- lme(outcome_change ~ 1 + feedback + outcome_alert + outcome_first_centered + outcome_first_center + ccaps_freq + appt_n + hospitalization,
                      random =~ 1| CcmhID,
                      data = model_data,
                      na.action=na.omit,
                      control=list(opt="optim"),
                      method="ML")

    moderators <- lme(outcome_change ~ 1 + feedback + feedback*outcome_alert + feedback*outcome_first_centered + feedback*outcome_first_center + feedback*ccaps_freq + feedback*appt_n + feedback*hospitalization,
                      random =~ 1| CcmhID,
                      data = model_data,
                      na.action=na.omit,
                      control=list(opt="optim"),
                      method="ML")
  }

  if (subscale %in% c("Depression34", "Anxiety34", "Social_Anxiety34", "Alcohol34", "DI")) {
    alert_model <- lme(outcome_change ~ 1 + feedback,
                      random =~ 1 + feedback| CcmhID,
                      data = alert_only,
                      na.action=na.omit,
                      control=list(opt="optim"),
                      method="ML")
  } else {
    alert_model <- lme(outcome_change ~ 1 + feedback,
                      random =~ 1| CcmhID,
                      data = alert_only,
                      na.action=na.omit,
                      control=list(opt="optim"),
                      method="ML")
  }

  return(list(null = null, feedback = feedback, feedback_random = feedback_random, main = main, moderators = moderators, alert_model = alert_model))
}


# Deterioration -----------------------------------------------------------
get_deter_rates <- function(subscale, data = deter_data) {
  outcome_deter <- paste0(rlang::as_name(enquo(subscale)), "_deter")
  outcome_cut <- paste0(rlang::as_name(enquo(subscale)), "_low_cut")
  outcome_first_centered <- paste0(rlang::as_name(enquo(subscale)), "_first_centered")
  outcome_first_center <- paste0(rlang::as_name(enquo(subscale)), "_first_center")
  outcome_alert <- paste0(rlang::as_name(enquo(subscale)), "_alert")

  model_data <- filter(data, !is.na(!!as.name(outcome_deter))) %>%
    mutate(outcome_deter = !!as.name(outcome_deter),
           outcome_first_centered = !!as.name(outcome_first_centered),
           outcome_first_center = !!as.name(outcome_first_center),
           outcome_alert = !!as.name(outcome_alert))


  model_data %>%
    group_by(feedback) %>%
    summarize({{subscale}} := mean(outcome_deter == 1, na.rm = T)) %>%
    ungroup() %>%
    select(-feedback)
}

deterioration_analyses <- function(subscale, data = deter_data) {
  outcome_deter <- paste0(rlang::as_name(enquo(subscale)), "_deter")
  outcome_cut <- paste0(rlang::as_name(enquo(subscale)), "_low_cut")
  outcome_first_centered <- paste0(rlang::as_name(enquo(subscale)), "_first_centered")
  outcome_first_center <- paste0(rlang::as_name(enquo(subscale)), "_first_center")
  outcome_alert <- paste0(rlang::as_name(enquo(subscale)), "_alert")

  model_data <- ungroup(data) %>%
    filter(!is.na(!!as.name(outcome_deter))) %>%
    mutate(outcome_deter = !!as.name(outcome_deter),
           outcome_first_centered = !!as.name(outcome_first_centered),
           outcome_first_center = !!as.name(outcome_first_center),
           outcome_alert = !!as.name(outcome_alert))

  alert_only <- ungroup(data) %>%
    filter(!is.na(!!as.name(outcome_deter)) & !!as.name(outcome_alert) == 1) %>%
    mutate(outcome_deter = !!as.name(outcome_deter))

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

  moderators <- glmer(outcome_deter ~ 1 + feedback + feedback*outcome_first_centered + feedback*outcome_first_center + feedback*ccaps_freq + feedback*appt_n + feedback*hospitalization + (1 | CcmhID),
                    data = model_data,
                    family = binomial(link = "logit"),
                    control=glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000)))

  alert_model <- glmer(outcome_deter ~ 1 + feedback + (1 + feedback| CcmhID),
                           data = alert_only,
                           family = binomial(link = "logit"),
                           control=glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000)))

  return(list(null = null, feedback = feedback, feedback_random = feedback_random, moderators = moderators, alert_model = alert_model))
}



# Rate of change ----------------------------------------------------------
test_change_shape <- function(subscale, data = roc_data) {
  outcome <- paste0(rlang::as_name(enquo(subscale)))
  outcome_cut <- paste0(rlang::as_name(enquo(subscale)), "_low_cut")

  model_data <- filter(data, !is.na(!!as.name(outcome))) %>%
    mutate(outcome = !!as.name(outcome)) %>%
    mutate(log10_appt_seq = log10(1+appt_seq))

  linear <- lme(outcome ~ 0 + appt_seq,
              random =~ 0 + appt_seq| CcmhID/UniqueClientID,
              data = model_data,
              na.action=na.omit,
              control=list(opt="optim"),
              method="ML")

  loglinear <- lme(outcome ~ 0 + log_appt_seq,
                random =~ 0 + log_appt_seq| CcmhID/UniqueClientID,
                data = model_data,
                na.action=na.omit,
                control=list(opt="optim"),
                method="ML")

  log10linear <- lme(outcome ~ 0 + log10_appt_seq,
                   random =~ 0 + log10_appt_seq| CcmhID/UniqueClientID,
                   data = model_data,
                   na.action=na.omit,
                   control=list(opt="optim"),
                   method="ML")

  quad <- lme(outcome ~ 0 + appt_seq + I(appt_seq^2),
                   random =~ 0 + appt_seq| CcmhID/UniqueClientID,
                   data = model_data,
                   na.action=na.omit,
                   control=list(opt="optim"),
                   method="ML")

  inverse <- lme(outcome ~ 0 + inv_appt_seq,
              random =~ 0 + inv_appt_seq| CcmhID/UniqueClientID,
              data = model_data,
              na.action=na.omit,
              control=list(opt="optim"),
              method="ML")

  return(list(linear = linear, loglinear = loglinear, log10linear = log10linear, quad = quad, inverse = inverse))
}


roc_analyses <- function(subscale, data = roc_data) {
  outcome <- paste0(rlang::as_name(enquo(subscale)))
  outcome_cut <- paste0(rlang::as_name(enquo(subscale)), "_low_cut")
  outcome_first_centered <- paste0(rlang::as_name(enquo(subscale)), "_first_centered")
  outcome_first_center <- paste0(rlang::as_name(enquo(subscale)), "_first_center")
  outcome_alert <- paste0(rlang::as_name(enquo(subscale)), "_alert")

  model_data <- filter(data, !is.na(!!as.name(outcome))) %>%
    mutate(outcome = !!as.name(outcome),
           outcome_first_centered = !!as.name(outcome_first_centered),
           outcome_first_center = !!as.name(outcome_first_center),
           outcome_alert = !!as.name(outcome_alert))


  alert_only <- ungroup(data) %>%
    filter(!is.na(!!as.name(outcome)) & !!as.name(outcome_alert) == 1) %>%
    mutate(outcome = !!as.name(outcome))

  # loglinear model
  if (subscale %in% c("Depression34", "Anxiety34", "Social_Anxiety34", "DI")) {
    null <- lme(outcome ~ 0 + log_appt_seq,
                random =~ 0 + log_appt_seq| CcmhID/UniqueClientID,
                data = model_data,
                na.action=na.omit,
                control=list(opt="optim"),
                method="ML")

    feedback <- lme(outcome ~ 0 + log_appt_seq + log_appt_seq:feedback,
                    random =~ 0 + log_appt_seq| CcmhID/UniqueClientID,
                    data = model_data,
                    na.action=na.omit,
                    control=list(opt="optim"),
                    method="ML")

    feedback_random <- lme(outcome ~ 0 + log_appt_seq +log_appt_seq:feedback,
                           random = list(CcmhID =~ 0 + log_appt_seq + log_appt_seq:feedback,
                                         UniqueClientID =~ 0 + log_appt_seq),
                           data = model_data,
                           na.action=na.omit,
                           control=list(opt="optim"),
                           method="ML")

    main <- lme(outcome ~ 0 + log_appt_seq + log_appt_seq:feedback + log_appt_seq:outcome_first_centered + log_appt_seq:outcome_first_center + log_appt_seq:outcome_alert + log_appt_seq:ccaps_freq + log_appt_seq:appt_n + log_appt_seq:hospitalization,
                      random =~ 0 + log_appt_seq| CcmhID/UniqueClientID,
                      data = model_data,
                      na.action=na.omit,
                      control=list(opt="optim"),
                      method="ML")

    moderators <- lme(outcome ~ 0 + log_appt_seq + log_appt_seq:feedback + log_appt_seq:outcome_first_centered + log_appt_seq:outcome_first_center + log_appt_seq:outcome_alert + log_appt_seq:ccaps_freq + log_appt_seq:appt_n + log_appt_seq:hospitalization + log_appt_seq:feedback:outcome_first_centered + log_appt_seq:feedback:outcome_first_center + log_appt_seq:feedback:outcome_alert + log_appt_seq:feedback:ccaps_freq + log_appt_seq:feedback:appt_n + log_appt_seq:feedback:hospitalization,
                      random =~ 0 + log_appt_seq| CcmhID/UniqueClientID,
                      data = model_data,
                      na.action=na.omit,
                      control=list(opt="optim"),
                      method="ML")

    alert_model <- lme(outcome ~ 0 + log_appt_seq + log_appt_seq:feedback,
                    random =~ 0 + log_appt_seq| CcmhID/UniqueClientID,
                    data = alert_only,
                    na.action=na.omit,
                    control=list(opt="optim"),
                    method="ML")

  } else if (subscale %in% c("Academics34", "Eating34", "Hostility34", "Alcohol34")) {
    null <- lme(outcome ~ 0 + inv_appt_seq,
                random =~ 0 + inv_appt_seq| CcmhID/UniqueClientID,
                data = model_data,
                na.action=na.omit,
                control=list(opt="optim"),
                method="ML")

    feedback <- lme(outcome ~ 0 + inv_appt_seq + inv_appt_seq:feedback,
                    random =~ 0 + inv_appt_seq| CcmhID/UniqueClientID,
                    data = model_data,
                    na.action=na.omit,
                    control=list(opt="optim"),
                    method="ML")

    feedback_random <- lme(outcome ~ 0 + inv_appt_seq +inv_appt_seq:feedback,
                           random = list(CcmhID =~ 0 + inv_appt_seq + inv_appt_seq:feedback,
                                         UniqueClientID =~ 0 + inv_appt_seq),
                           data = model_data,
                           na.action=na.omit,
                           control=list(opt="optim"),
                           method="ML")

    main <- lme(outcome ~ 0 + inv_appt_seq + inv_appt_seq:feedback + inv_appt_seq:outcome_first_centered + inv_appt_seq:outcome_first_center + inv_appt_seq:outcome_alert + inv_appt_seq:ccaps_freq + inv_appt_seq:appt_n + inv_appt_seq:hospitalization,
                      random =~ 0 + inv_appt_seq| CcmhID/UniqueClientID,
                      data = model_data,
                      na.action=na.omit,
                      control=list(opt="optim"),
                      method="ML")

    moderators <- lme(outcome ~ 0 + inv_appt_seq + inv_appt_seq:feedback + inv_appt_seq:outcome_first_centered + inv_appt_seq:outcome_first_center + inv_appt_seq:outcome_alert + inv_appt_seq:ccaps_freq + inv_appt_seq:appt_n + inv_appt_seq:hospitalization + inv_appt_seq:feedback:outcome_first_centered + inv_appt_seq:feedback:outcome_first_center + inv_appt_seq:feedback:outcome_alert + inv_appt_seq:feedback:ccaps_freq + inv_appt_seq:feedback:appt_n + inv_appt_seq:feedback:hospitalization,
                      random =~ 0 + inv_appt_seq| CcmhID/UniqueClientID,
                      data = model_data,
                      na.action=na.omit,
                      control=list(opt="optim"),
                      method="ML")

    alert_model <- lme(outcome ~ 0 + inv_appt_seq + inv_appt_seq:feedback,
                    random =~ 0 + inv_appt_seq| CcmhID/UniqueClientID,
                    data = alert_only,
                    na.action=na.omit,
                    control=list(opt="optim"),
                    method="ML")
  }

  return(list(null = null, feedback = feedback, feedback_random = feedback_random, main = main, moderators = moderators, alert_model = alert_model))
}

