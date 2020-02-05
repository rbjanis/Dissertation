# Combining data from schools that provided data in 2013-2015 and 2016-2018
# Identifying schools that updated to the new CCAPS profile report by 2016-07-01

library(tidyverse)

load("data/2013-2015_TI_2019-02-05.RDa")
load("data/2016-2018_TI_2019-03-12.rda")

IDs <- openxlsx::read.xlsx("data/2018-09-20_membership.xlsx") %>%
  select(CcmhID, ccmhmemberid = Activationkey) %>%
  mutate(ccmhmemberid = as.numeric(as.character(ccmhmemberid))) %>%
  filter(!is.na(ccmhmemberid)) %>%
  distinct()

ccaps_updates <- openxlsx::read.xlsx("data/CCAPS-update-date.xlsx", detectDates = T) %>%
  rename(update_date = Installed.Update.with.CCAPS.2015) %>%
  mutate(ccmhmemberid = as.numeric(as.character(ccmhmemberid))) %>%
  left_join(IDs)

CCAPS_start_date <- group_by(TI1618, CcmhID) %>%
  summarize(CcapsVer2015StartDate = min(CcapsVer2015StartDate, na.rm = T))

group_by(TI1618, CcmhID) %>%
  filter(!is.na(CcapsVer2015StartDate)) %>%
  summarize(n_distinct(CcapsVer2015StartDate), min(CcapsVer2015StartDate), max(CcapsVer2015StartDate)) %>%
  View()

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

# Schools that provided data in all years
schools13 <- filter(TI1315, Data_year == 2013) %>% pull(CcmhID) %>% unique()
schools14 <- filter(TI1315, Data_year == 2014) %>% pull(CcmhID) %>% unique()
schools16 <- filter(TI1618, Data_year == 2016) %>% pull(CcmhID) %>% unique()
schools17 <- filter(TI1618, Data_year == 2017) %>% pull(CcmhID) %>% unique()

data_schools <- Reduce(intersect, list(schools13, schools14, schools16, schools17))
both_schools <- intersect(data_schools, ccaps_schools$CcmhID)


ccaps_updates$CcmhID[!data_schools %in% ccaps_updates$CcmhID]
ccaps_updates$ccmhmemberid[!data_schools %in% ccaps_updates$CcmhID]

TI1315_schools <- filter(TI1315, CcmhID %in% both_schools)
TI1618_schools <- filter(TI1618, CcmhID %in% both_schools)

n_distinct(TI1315_schools$UniqueClientID)
n_distinct(TI1315_schools$CcmhID)
n_distinct(TI1618_schools$UniqueClientID)
n_distinct(TI1618_schools$CcmhID)

save(TI1315_schools, TI1618_schools, data_schools, both_schools, file = "data/included-schools.rda")
