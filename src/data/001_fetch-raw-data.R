rm(list = ls())
gc()

library(tidyverse)
library(odbc)
library(dbplyr)
source("src/config.R")

aicon <- dbConnect(odbc::odbc(), dns, Database = dabs[1], UID = uid,
                   PWD = rstudioapi::askForPassword("pwd-"))
sdbcon <- dbConnect(odbc::odbc(), dns, Database = dabs[2], UID = uid,
                   PWD = rstudioapi::askForPassword("pwd-"))
d <- Sys.Date()
cur.yrq <- tbl(sdbcon, in_schema("sec", "sys_tbl_39_calendar")) %>%
  filter(first_day >= d) %>% select(table_key) %>% collect() %>% first(1) %>%     # 2x first to go from tibble -> vector
  first(1) %>% as.numeric()

grads.raw <- tbl(aicon, in_schema("sec", "IV_StudentDegreeMajorCompletions")) %>%
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
  filter(credential_status == "active") %>%
  collect() %>%
  filter(grepl("bachelor", credential_title, ignore.case = T) == T)

dbDisconnect(aicon)
dbDisconnect(sdbcon)

# save(list = ls(pattern = ".raw"), file = "data/raw/raw-data.RData")
save(list = ls(pattern = ".raw"), file = "data/raw/raw-data-5yrs.RData")