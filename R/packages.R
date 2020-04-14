# Setup

library(tidyverse)
library(drake)
library(CCMHr)
library(beepr)
library(usethis)
library(nlme)
library(lmerTest)
conflicted::conflict_prefer("filter", "dplyr")
conflicted::conflict_prefer("select", "dplyr")
conflicted::conflict_prefer("lmer", "lmerTest")
