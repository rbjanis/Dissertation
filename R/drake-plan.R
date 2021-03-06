source("R/packages.R")
source("R/funs_cleaning.R")
source("R/funs_analysis.R")
source("R/funs_descriptives.R")
source("R/funs_results.R")

plan <- drake::drake_plan(
  # Data cleaning-----------------------------------------------------------------------------
  raw_pre_data = target(
    CCMHr::loadRDa("data/2013-2015_TI_2019-02-05.RDa"),
    format = "fst"),
  raw_post_data = target(
    CCMHr::loadRDa("data/2016-2018_TI_2019-03-12.rda"),
    format = "fst"),
  combined_TI_data = target(
    combine_TI_data(raw_pre_data, raw_post_data),
    format = "fst"),
  ccaps_update_dates = get_update_dates(combined_TI_data),
  updated_centers = get_updated_centers(ccaps_update_dates),
  included_centers = intersect(unique(combined_TI_data$CcmhID), updated_centers$CcmhID),
  precleaned_data = create_precleaned_data(combined_TI_data, included_centers),
  data_courses = CCMHr::create_courses(precleaned_data, firstOnly = T),
  moderators = create_moderators(precleaned_data),
  roc_data_intermediate = clean_roc(data_courses, min_ccaps = 3) %>%
    left_join(select(moderators, UniqueClientID, hospitalization)) %>%
    filter(!is.na(hospitalization)),
  change_data = clean_change(roc_data_intermediate) %>%
    left_join(select(moderators, UniqueClientID, hospitalization)) %>%
    filter(!is.na(hospitalization)),
  deter_data = clean_deterioration(change_data),
  # roc_data = anchor_roc(roc_data_intermediate),
  flow_chart = build_flow_chart(data_courses, moderators),
  alphas = calculate_alphas(data_courses, change_data),
  alert_data = session_alerts(roc_data_intermediate),

  # Models----------------------------------------------------------------------------------
  subscales = c("Depression34", "Anxiety34", "Social_Anxiety34", "Academics34",
                "Eating34", "Hostility34", "Alcohol34", "DI"),
  change_models = purrr::map(subscales, prepost_analyses, data = change_data) %>%
    set_names(subscales),
  deter_models = purrr::map(subscales, deterioration_analyses, data = deter_data) %>%
    set_names(subscales),
  # deter_omnibus_model = deterioration_omnibus(data = deter_data),
  change_shape = purrr::map(subscales, test_change_shape, data = roc_data_intermediate) %>%
    set_names(subscales),
  shape_anovas = map_df(change_shape[1:8], function(x) anova(x[["loglinear"]], x[["inverse"]], x[["quad"]])),
  roc_models = purrr::map(subscales, roc_analyses, data = roc_data_intermediate) %>%
    set_names(subscales),

  # Results---------------------------------------------------------------------------------
  # deter_r2_feedback = create_deter_r2_feedback(deter_models),
  # deter_r2_mods = create_deter_r2_mods(deter_models),
  # change_r2_feedback = create_change_r2_feedback(change_models),
  # change_R2_mods = create_change_R2_mods(change_models),

  # # Reports
  # results = rmarkdown::render(
  #   knitr_in("results.rmd"),
  #   output_file = file_out("results.docx"),
  #   quiet = TRUE
  # )
)

make(plan)

drake_config(plan) %>%
  vis_drake_graph()

build_times(type = "build")
