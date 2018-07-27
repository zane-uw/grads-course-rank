rm(list = ls())
gc()

library(tidyverse)
library(edwHelpers)

load("data/raw/raw-data.RData")

kuali <- read_csv("data/raw/programs-kuali.csv")

grads <- grads.raw %>%
  filter(DegreeNum == 1) %>%
  select(SDBSrcSystemKey, DegreeGrantedQtrKeyId, DegreeCode, MajorCode, StudentCohortQtrKeyId,
         DegreeGrantedGPA, DegreeGrantedUWEarnedCredits, DegreeGrantedTransferCredits,
         DegreeGrantedTotalCredits, TimeToDegreeInTerms, TimeToDegreeInYears)

courses <- courses.raw %>%
