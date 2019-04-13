# 04-2019 refactoring to include more years of graduation(s)

rm(list = ls())
gc()

library(tidyverse)
library(edwHelpers)

options(theme_set(theme_bw(base_size = 15)))
options(tibble.print_min = 100)

setwd(rstudioapi::getActiveProject())
load("data/raw/raw-data-5yrs.RData")

grads <- grads.raw %>%
  # filter(DegreeNum == 1) %>%
  select(system_key = SDBSrcSystemKey, degree.yrq = DegreeGrantedQtrKeyId, DegreeNum, DegreeCode, MajorCode, StudentCohortQtrKeyId,
         DegreeGrantedGPA, DegreeGrantedUWEarnedCredits, DegreeGrantedTransferCredits,
         DegreeGrantedTotalCredits, TimeToDegreeInTerms, TimeToDegreeInYears, AcademicCareerEntryType, DegreeMajorNum)
grads$ftfy <- if_else(grads$AcademicCareerEntryType == "FTFY", "FTFY", "EVERYONE ELSE")

courses <- courses.raw %>%
  filter(course_number >= 100) %>%
  mutate(yrq = (tran_yr * 10) + tran_qtr,
         ckey = paste(trimws(dept_abbrev), course_number, sep = " "),
         grade = as.numeric(recode(trimws(grade),
                                   "A"  = "40",
                                   "A-" = "38",
                                   "B+" = "34",
                                   "B"  = "31",
                                   "B-" = "28",
                                   "C+" = "24",
                                   "C"  = "21",
                                   "C-" = "18",
                                   "D+" = "14",
                                   "D"  = "11",
                                   "D-" = "08",
                                   "E"  = "00"))) %>%
  filter(!is.na(grade)) %>%
  select(system_key, yrq, ckey, course_credits, course_branch, grade_system, college, grade) %>%
  arrange(system_key, yrq)


    # # some prelim basic data explorations
    # qplot(grads$TimeToDegreeInTerms, geom = "histogram", binwidth = 1)
    # (m <- mean(grads$TimeToDegreeInTerms, na.rm = T))
    # (s <- sd(grads$TimeToDegreeInTerms, na.rm = T))
    # (cutoff <- m + 2*s)                               # that seems like a lot of terms
    # p1 <- qplot(grads$TimeToDegreeInTerms, geom = "histogram", binwidth = 1) +
    #   geom_vline(xintercept = cutoff, color = "red", linetype = 2) +
    #   geom_vline(xintercept = m, color = "green")
    # p1
    #
    # sub <- grads[grads$AcademicCareerEntryType == "FTFY",]
    # qplot(sub$TimeToDegreeInTerms, geom = "histogram", binwidth = 1)
    # (m2 <- mean(sub$TimeToDegreeInTerms, na.rm = T))
    # (s2 <- sd(sub$TimeToDegreeInTerms, na.rm = T))
    # (cutoff2 <- m2 + 2*s2)
    # p2 <- qplot(sub$TimeToDegreeInTerms, geom = "histogram", binwidth = 1) +
    #   geom_vline(xintercept = cutoff2, color = "red", linetype = 2) +
    #   geom_vline(xintercept = m2, color = "green")
    #
    # cbind(m, m2, s, s2, cutoff, cutoff2)
    # p1; p2
    #
    # ggplot(data = grads, aes(x = TimeToDegreeInTerms)) +
    #   geom_histogram(binwidth = 1) +
    #   facet_wrap(c("ftfy"))
    #
    # ggplot(data = subset(grads, grads$StudentCohortQtrKeyId >= 20104), aes(x = TimeToDegreeInTerms)) +
    #   geom_histogram(binwidth = 1) +
    #   facet_wrap(c("ftfy"))
    #
    # # ggplot(data = grads, aes(x = TimeToDegreeInYears, y = MajorCode)) +
    # #  geom_boxplot()
    #
    # ggplot(data = grads, aes(x = AcademicCareerEntryType, y = TimeToDegreeInTerms)) + geom_boxplot()
    # ggplot(data = grads, aes(x = AcademicCareerEntryType, y = TimeToDegreeInYears)) + geom_boxplot()
    #
    # # how about this?
    # cbind(prop.table(table(grads$StudentCohortQtrKeyId)))
    # # 20114 is the first q with > 1% of the total
    #
    # grads <- grads %>% filter(StudentCohortQtrKeyId >= 20114)
    # ggplot(data = grads, aes(x = AcademicCareerEntryType, y = TimeToDegreeInYears)) + geom_boxplot()
    # # much nicer


# filter current majors ---------------------------------------------------
#
# cur.maj <- kuali %>%
#   filter(status == "active", str_detect(title, "Minor") == F) %>%                                   # is.na(doNotPublish) | doNotPublish == F) %>%
#   select(program_code, program_title, program_admissionType, credentialAdmissionType, code, title)

creds <- creds.raw %>%
  filter(grepl("true", DoNotPublish, ignore.case = T) == F) %>%
  select(program_verind_id, credential_code, credential_title, credential_description)
progs <- programs.raw %>%
  select(program_verind_id, program_code, program_title, program_dept_code, campus_name, program_school_or_college,
         program_admissionType, program_description, program_level, program_type)

cur.maj <- inner_join(progs, creds) %>%
  mutate(campus_num = recode(str_trim(campus_name),
                             "Seattle" = "0",
                             "Bothell" = "1",
                             "Tacoma" = "2"))

# CODES:
# grads$DegreeCode = "0_NAME_00_0_0" = campus_program_pathway_level_degreetype
# cur.maj has credential code = program-pathway-level-degreetype; level is undergrad, type is arts/science/etc.
# There are some mismatches between the ways that pathways are coded though

i <- str_split(cur.maj$credential_code, pattern = "-", simplify = T)    # split credential code into a character matrix
i[,2] <- str_pad(i[,2], 2, side = "right", pad = "0")
cur.maj$major_key <- paste(cur.maj$campus_num, i[,1], i[,2], i[,3], i[,4], sep = "_")

# there are some majors that have reversed codes in the grads file (Phys, HCDE, a few others)
# this needs a quick fix to swap the 0's to the correct side of the pathway for merging with Kuali data
# I suppose I could do this the other way and un-pad the majors but I'll stick with this for continuity since
# other apps use the padded version
i <- str_split(grads$DegreeCode, "_", simplify = T)
i[,3] <- as.numeric(i[,3])
i[,3] <- str_pad(i[,3], 2, side = "right", pad = "0")
# grads$major_key <- paste(i[,1], i[,2], i[,3], sep = "_")
grads$major_key <- apply(i, 1, paste, collapse = "_")

# now filter grads against current majors
old.majors <- grads[!(grads$major_key %in% cur.maj$major_key),]     # may want to inspect
cbind(unique(old.majors$MajorCode))
old.majors %>% group_by(MajorCode) %>% summarize(n_distinct(system_key))

grads <- grads[grads$major_key %in% cur.maj$major_key,]

# maj <- cur.maj %>% distinct(major_key, .keep_all = T) # select(major_key, program_title, title, program_admissionType) %>% distinct(major_key, .keep_all = T)
grads <- grads %>% inner_join(cur.maj)


# merge grads with courses ------------------------------------------------

dat <- inner_join(courses, grads, by = "system_key") %>% distinct(system_key, yrq, major_key, ckey, .keep_all = T)
# dat[dat$yrq > dat$degree.yrq,]

dat %>% distinct(system_key, TimeToDegreeInTerms) %>% ggplot(., aes(x = TimeToDegreeInTerms)) + geom_bar() + xlim(c(0, 20))

dat <- dat %>%
  group_by(system_key, DegreeNum, major_key) %>%
  filter(yrq <= degree.yrq,
         StudentCohortQtrKeyId >= 20074,               #! 20193 - 50 = 20143 - 60, rounded down = 20074
         yrq >= StudentCohortQtrKeyId) %>%
  arrange(system_key, DegreeNum, major_key, yrq) %>%
  ungroup()

save(dat, file = "data/clean/all-degree-grads-and-courses-5yrs.Rdata")    #! 5 yrs here
