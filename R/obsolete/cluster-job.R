library(purrr)

roc_R2_feedback <- suppressWarnings(map_df(roc_models[1:8], function(x) {r2glmm::r2beta(x[["feedback_random"]])}, .id = "subscale"))

roc_R2_mods <- suppressWarnings(map_df(roc_models[1:8], function(x) {r2glmm::r2beta(x[["moderators"]])}, .id = "subscale"))
