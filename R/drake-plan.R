source("R/packages.R")
source("R/funs.R")

plan <- drake::drake_plan(
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
  included_centers = intersect(unique(combined_TI_data$CcmhID), updated_centers$CcmhID)
)

drake_config(plan) %>%
  vis_drake_graph()

make(plan)
build_times(type = "build")

included_centers <- readd(included_centers)
