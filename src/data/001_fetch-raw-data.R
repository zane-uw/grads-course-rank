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

grads.raw <- tbl(aicon, in_schema("sec", "IV_StudentDegreeMajorCompletions"))
grads.raw <- grads.raw %>%
  filter(DegreeGrantedQtrKeyId >= 20162,
         StudentClassCode <= 4,
         DegreeAwardLevelGroup == "Undergraduate") %>%
  collect()

stus <- data.frame("system_key" = unique(grads.raw$SDBSrcSystemKey))

courses.raw <- tbl(sdbcon, in_schema("sec", "transcript_courses_taken"))
courses.raw <- courses.raw %>%
  inner_join(stus, by = "system_key", copy = T) %>%
  collect()

dbDisconnect(aicon)
dbDisconnect(sdbcon)

save(list = ls(pattern = ".raw"), file = "data/raw/raw-data.RData")
