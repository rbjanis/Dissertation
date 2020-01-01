# Combining data from schools that provided data in 2013-2015 and 2016-2018
# Identifying schools that updated to the new CCAPS profile report by 2016-07-01

library(tidyverse)

load("data/2013-2015_TI_2019-02-05.RDa")
load("data/2016-2018_TI_2019-03-12.rda")

CCAPS_start_date <- group_by(TI1618, CcmhID) %>%
  summarize(CcapsVer2015StartDate = min(CcapsVer2015StartDate, na.rm = T))

group_by(TI1618, CcmhID) %>%
  filter(!is.na(CcapsVer2015StartDate)) %>%
  summarize(n_distinct(CcapsVer2015StartDate), min(CcapsVer2015StartDate), max(CcapsVer2015StartDate)) %>%
  View()

IDs <- xlsx::read.xlsx("data/2018-09-20_membership.xlsx", sheetIndex = 1) %>%
  select(CcmhID, ccmhmemberid = Activationkey) %>%
  mutate(ccmhmemberid = as.numeric(as.character(ccmhmemberid))) %>%
  filter(!is.na(ccmhmemberid)) %>%
  distinct()

ccaps_updates <- xlsx::read.xlsx("data/CCAPS-update-date.xlsx", sheetIndex = 1) %>%
  rename(update_date = Installed.Update.with.CCAPS.2015) %>%
  mutate(ccmhmemberid = as.numeric(as.character(ccmhmemberid))) %>%
  left_join(IDs)

range(ccaps_updates$update_date, na.rm = T)

# 107901854 & 795814607 (1107 & 1174) appear in Karl's file twice

# Comparing CcapsVer2015StartDate to Udate date

CCAPS_start_date_comp <- left_join(CCAPS_start_date, ccaps_updates) %>%
  mutate(CcapsVer2015StartDate = stringr::str_sub(CcapsVer2015StartDate, 1, 10), CcapsVer2015StartDate = lubridate::ymd(CcapsVer2015StartDate)) %>%
  mutate(CcapsVer2015StartDate = case_when(CcapsVer2015StartDate < "2015-07-27" ~ as.Date("2015-07-27"),
                                           TRUE ~ CcapsVer2015StartDate))

sum(CCAPS_start_date_comp$CcapsVer2015StartDate < "2016-06-30", na.rm = T)
sum(CCAPS_start_date_comp$update_date < "2016-06-30", na.rm = T)
sum(CCAPS_start_date_comp$update_date < "2016-06-30" & CCAPS_start_date_comp$CcapsVer2015StartDate < "2016-06-30", na.rm = T)

ccaps_schools <- filter(CCAPS_start_date_comp, update_date < "2016-06-30" & CcapsVer2015StartDate < "2016-06-30") %>%
  select(CcmhID)

# Schools that provided data in both data sets
data_schools <- intersect(unique(TI1315$CcmhID), unique(TI1618$CcmhID))

both_schools <- intersect(data_schools, ccaps_schools$CcmhID)


ccaps_updates$CcmhID[!data_schools %in% ccaps_updates$CcmhID]
ccaps_updates$ccmhmemberid[!data_schools %in% ccaps_updates$CcmhID]

TI1315_schools <- filter(TI1315, CcmhID %in% data_schools)
TI1618_schools <- filter(TI1618, CcmhID %in% data_schools)
