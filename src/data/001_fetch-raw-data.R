rm(list = ls())
gc()

library(tidyverse)
library(odbc)
library(dbplyr)
# source("src/config.R")

sdbcon <- dbConnect(odbc(), 'sqlserver01')

cur.yrq <- tbl(sdbcon, in_schema('sec', 'sdbdb01')) %>%
  mutate(x = current_yr*10 + current_qtr) %>%
  select(x) %>%
  collect() %>%
  as.numeric()

grads.raw <- tbl(sdbcon, in_schema("AnalyticInteg.sec", "IV_StudentDegreeMajorCompletions")) %>%
  filter(DegreeGrantedQtrKeyId >= (cur.yrq - 50),           # ED: changed from 2 years to 5 years
         StudentClassCode <= 4,
         DegreeAwardLevelGroup == "Undergraduate") %>%
  collect()

stus <- data.frame("system_key" = unique(grads.raw$SDBSrcSystemKey))

courses.raw <- tbl(sdbcon, in_schema("sec", "transcript_courses_taken")) %>%
  inner_join(stus, by = "system_key", copy = T) %>%
  collect()

programs.raw <- tbl(sdbcon, in_schema("sec", "CM_Programs")) %>%
  filter(program_level == "Undergraduate",
         program_type == "Major") %>%
  collect()

creds.raw <- tbl(sdbcon, in_schema("sec", "CM_Credentials")) %>%
  # filter(credential_status == "active") %>%
  collect() %>%
  filter(grepl("bachelor", credential_title, ignore.case = T) == T)

dbDisconnect(sdbcon)

# save(list = ls(pattern = ".raw"), file = "data/raw/raw-data.RData")
save(list = ls(pattern = ".raw"), file = "data/raw/raw-data-5yrs.RData")
