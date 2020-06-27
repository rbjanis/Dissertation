# Results functions

pull_deter_stats <- function(subscale_name, moderator) {
  beta <- round(pull(select(filter(deter_params_feedback_raw, subscale == subscale_name & term == moderator), estimate)),2)
  or <- round(exp(pull(select(filter(deter_params_feedback_raw, subscale == subscale_name & term == moderator), estimate))),2)
  se <- round(pull(select(filter(deter_params_feedback_raw, subscale == subscale_name & term == moderator), `std.error`)),2)
  tstat <- round(pull(select(filter(deter_params_feedback_raw, subscale == subscale_name & term == moderator), statistic)),2)
  pval <- round(pull(select(filter(deter_params_feedback_raw, subscale == subscale_name & term == moderator), `p.value`)),3)
  glue::glue("$\\beta$ = {beta}, SE = {se}, OR = {or}, *z* = {tstat}, *p* {ifelse(pval == 0, '< .001', glue::glue('= {pval}'))}")
}

pull_change_mods <- function(subscale_name, moderator) {
  beta <- round(pull(select(filter(change_params_fixed_raw, subscale == subscale_name & term == moderator), estimate)),2)
  se <- round(pull(select(filter(change_params_fixed_raw, subscale == subscale_name & term == moderator), `std.error`)),2)
  df <- pull(select(filter(change_params_fixed_raw, subscale == subscale_name & term == moderator), df))
  tstat <- round(pull(select(filter(change_params_fixed_raw, subscale == subscale_name & term == moderator), statistic)),2)
  pval <- round(pull(select(filter(change_params_fixed_raw, subscale == subscale_name & term == moderator), `p.value`)),3)
  glue::glue("$\\beta$ = {beta}, SE = {se}, *t*({df}) = {tstat}, *p* {ifelse(pval == 0, '< .001', glue::glue('= {pval}'))}")
}

format_number <- function(x) {
  format(x, big.mark = ",")
}

create_deter_r2_feedback <- function(model){
 suppressWarnings(map_df(model[1:8], function(x) {r2glmm::r2beta(x[["feedback"]], method = 'nsj')}, .id = "subscale")) %>%
    select(subscale, Effect, Rsq) %>%
    mutate(Rsq = round(Rsq, 4)) %>%
    pivot_wider(names_from = subscale, values_from = Rsq) %>%
    mutate_if(is.numeric, scales::percent, accuracy = .01)
}

create_deter_r2_mods <- function(model){
  suppressWarnings(map_df(model[1:8], function(x) {r2glmm::r2beta(x[["moderators"]], method = 'nsj')}, .id = "subscale")) %>%
    select(subscale, Effect, Rsq) %>%
    mutate(Rsq = round(Rsq, 4)) %>%
    pivot_wider(names_from = subscale, values_from = Rsq) %>%
    mutate_if(is.numeric, scales::percent, accuracy = .01) %>%
    filter(Effect != "Model") %>%
    mutate(Effect = fct_relevel(Effect, "feedback", "outcome_first_centered", "outcome_first_center", "ccaps_freq", "appt_n", "hospitalization", "feedback:outcome_first_centered", "feedback:outcome_first_center", "feedback:ccaps_freq", "feedback:appt_n", "feedback:hospitalization")) %>%
    arrange(Effect) %>%
    mutate(Effect = fct_recode(Effect, Feedback = "feedback",
                               `Client baseline` = "outcome_first_centered",
                               `Center baseline` = "outcome_first_center",
                               `CCAPS frequency` = "ccaps_freq",
                               `Appointment N` = "appt_n",
                               Hospitalization = "hospitalization",
                               `Feedback * Client baseline` = "feedback:outcome_first_centered",
                               `Feedback * Center baseline` = "feedback:outcome_first_center",
                               `Feedback * CCAPS frequency` = "feedback:ccaps_freq",
                               `Feedback * Appointment N` = "feedback:appt_n",
                               `Feedback * Hospitalization` = "feedback:hospitalization"))
}

create_change_r2_feedback <- function(model){
  suppressWarnings(map_df(model[1:8], function(x) {r2glmm::r2beta(x[["feedback"]], method = 'nsj')}, .id = "subscale")) %>%
    select(subscale, Effect, Rsq) %>%
    mutate(Rsq = round(Rsq, 4)) %>%
    pivot_wider(names_from = subscale, values_from = Rsq) %>%
    mutate_if(is.numeric, scales::percent, accuracy = .1)
}

create_change_R2_mods <- function(model){
  suppressWarnings(map_df(model[1:8], function(x) {r2glmm::r2beta(x[["moderators"]], method = 'nsj')}, .id = "subscale")) %>%
  select(subscale, Effect, Rsq) %>%
  mutate(Rsq = round(Rsq, 4)) %>%
  pivot_wider(names_from = subscale, values_from = Rsq) %>%
  mutate_if(is.numeric, scales::percent, accuracy = .01) %>%
  filter(Effect != "Model") %>%
  mutate(Effect = fct_relevel(Effect, "feedback", "outcome_alert", "outcome_first_centered", "outcome_first_center", "ccaps_freq", "appt_n", "hospitalization", "feedback:outcome_alert", "feedback:outcome_first_centered", "feedback:outcome_first_center", "feedback:ccaps_freq", "feedback:appt_n", "feedback:hospitalization")) %>%
  arrange(Effect) %>%
  mutate(Effect = fct_recode(Effect, Feedback = "feedback",
                             `Off track` = "outcome_alert",
                             `Client baseline` = "outcome_first_centered",
                             `Center baseline` = "outcome_first_center",
                             `CCAPS frequency` = "ccaps_freq",
                             `Appointment N` = "appt_n",
                             Hospitalization = "hospitalization",
                             `Feedback * Off track` = "feedback:outcome_alert",
                             `Feedback * Client baseline` = "feedback:outcome_first_centered",
                             `Feedback * Center baseline` = "feedback:outcome_first_center",
                             `Feedback * CCAPS frequency` = "feedback:ccaps_freq",
                             `Feedback * Appointment N` = "feedback:appt_n",
                             `Feedback * Hospitalization` = "feedback:hospitalization"))
}

change_lrts <- function(subscale) {
  lrt <- anova(change_models[[subscale]][["null"]], change_models[[subscale]][["feedback"]])
  pval <- round(lrt[2, "p-value"],3)

  glue::glue("$\\chi$^2^(1) = {round(lrt[2, 'L.Ratio'],2)}, *p* {ifelse(pval == 0, '< .001', glue::glue('= {pval}'))}")
}

change_lrts_rand <- function(subscale) {
  lrt <- anova(change_models[[subscale]][["feedback"]], change_models[[subscale]][["feedback_random"]])
  pval <- round(lrt[2, "p-value"],3)

  glue::glue("$\\chi$^2^(1) = {round(lrt[2, 'L.Ratio'],2)}, *p* {ifelse(pval == 0, '< .001', glue::glue('= {pval}'))}")
}

roc_lrts <- function(subscale) {
  lrt <- anova(roc_models[[subscale]][["null"]], roc_models[[subscale]][["feedback"]])
  pval <- round(lrt[2, "Pr(>Chisq)"],3)

  glue::glue("$\\chi$^2^(1) = {round(lrt[2, 'Chisq'],2)}, *p* {ifelse(pval == 0, '< .001', glue::glue('= {pval}'))}")
}

roc_lrts_rand <- function(subscale) {
  lrt <- anova(roc_models[[subscale]][["feedback"]], roc_models[[subscale]][["feedback_random"]])
  pval <- round(lrt[2, "Pr(>Chisq)"],3)

  glue::glue("$\\chi$^2^(1) = {round(lrt[2, 'Chisq'],2)}, *p* {ifelse(pval == 0, '< .001', glue::glue('= {pval}'))}")
}

predict_change <- function(x) {
  center <- attributes(x[["feedback"]][["data"]][["outcome_change"]])[[1]]
  scale <- attributes(x[["feedback"]][["data"]][["outcome_change"]])[[2]]
  newdata <- data.frame(feedback = c(unique(x[["feedback"]][["data"]][["feedback"]])[1], unique(x[["feedback"]][["data"]][["feedback"]])[2]))
  (predict(x[["feedback"]], newdata, level = 0)*scale)+center
}

predict_roc <- function(x, subscale, data = roc_data_intermediate) {
  subscale_mean <- mean(pull(data, subscale), na.rm = T)
  subscale_sd <- sd(pull(data, subscale), na.rm = T)

    newdata <- data.frame(log_appt_seq = rep(unique(x[["feedback"]]@frame[["log_appt_seq"]]), 2), feedback = c(rep(unique(x[["feedback"]]@frame[["feedback"]])[1], 20), rep(unique(x[["feedback"]]@frame[["feedback"]])[2], 20)))
  (predict(x[["feedback"]], newdata, re.form = NA)*subscale_sd)+subscale_mean
}

pull_roc_mods <- function(subscale_name, moderator) {
  beta <- round(pull(select(filter(roc_params_mods_fixed_raw, subscale == subscale_name & term == moderator), estimate)),2)
  se <- round(pull(select(filter(roc_params_mods_fixed_raw, subscale == subscale_name & term == moderator), `std.error`)),2)
  df <- round(pull(select(filter(roc_params_mods_fixed_raw, subscale == subscale_name & term == moderator), df)),0)
  tstat <- round(pull(select(filter(roc_params_mods_fixed_raw, subscale == subscale_name & term == moderator), statistic)),2)
  pval <- round(pull(select(filter(roc_params_mods_fixed_raw, subscale == subscale_name & term == moderator), `p.value`)),3)
  glue::glue("$\\beta$ = {beta}, SE = {se}, *t*({df}) = {tstat}, *p* {ifelse(pval == 0, '< .001', glue::glue('= {pval}'))}")
}

deter_alert_rates <- function(subscale, data = deter_data) {
  outcome_deter <- paste0(rlang::as_name(enquo(subscale)), "_deter")
  outcome_alert <- paste0(rlang::as_name(enquo(subscale)), "_alert")

  data %>%
    filter(!is.na(!!as.name(outcome_deter))) %>%
    mutate(outcome_deter = !!as.name(outcome_deter),
           outcome_alert = !!as.name(outcome_alert)) %>%
    group_by(feedback, outcome_alert) %>%
    summarize({{subscale}} := mean(outcome_deter == 1, na.rm = T)) %>%
    ungroup() %>%
    mutate(feedback = ifelse(feedback == 1, "Feedback", "No feedback"),
           outcome_alert = ifelse(outcome_alert == 1, "Off Track", "Not Off Track"),
           {{subscale}} := scales::percent(!!as.name(subscale), accuracy = .01)) %>%
    pivot_wider(names_from = outcome_alert, values_from = {{subscale}}) %>%
    mutate(subscale = subscale, .before = 1)
}
