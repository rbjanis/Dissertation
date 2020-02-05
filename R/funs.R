# Functions

# Data processing functions------------------------------------------------
# Combining two TI data sets and removing schools that didn't contribute data all four years
combine_TI_data <- function(raw_pre_data, raw_post_data) {
  plyr::rbind.fill(raw_pre_data, raw_post_data) %>%
  group_by(CcmhID) %>%
    filter(n_distinct(Data_year) == 4)
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


# Preprocessing -----------------------------------------------------------



# Modeling functions ------------------------------------------------------



# Extract results from models ---------------------------------------------


