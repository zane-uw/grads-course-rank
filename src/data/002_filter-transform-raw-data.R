rm(list = ls())
gc()

library(tidyverse)
library(edwHelpers)

options(theme_set(theme_bw(base_size = 15)))

setwd(rstudioapi::getActiveProject())
load("data/raw/raw-data-5yrs.RData")              #! 5 years/2 here

kuali <- read_csv("data/raw/programs-kuali.csv")    # ****** FIX THIS, can't use DoNotPublish after all

grads <- grads.raw %>%
  filter(DegreeNum == 1) %>%
  select(syskey = SDBSrcSystemKey, degree.yrq = DegreeGrantedQtrKeyId, DegreeCode, MajorCode, StudentCohortQtrKeyId,
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
  select(syskey = system_key, yrq, ckey, course_credits, course_branch, grade_system, college, grade) %>%
  arrange(syskey, yrq)


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

cur.maj <- kuali %>%
  filter(status == "active", str_detect(title, "Minor") == F) %>%                                   # is.na(doNotPublish) | doNotPublish == F) %>%
  select(program_code, program_title, program_admissionType, credentialAdmissionType, code, title)

i <- str_split(cur.maj$code, pattern = "-", 3, simplify = T)
i[,2] <- str_pad(i[,2], 2, side = "right", pad = "0")
cur.maj$mkey <- paste(i[,1], i[,2], sep = "_")

# there are some majors that inexplicably have reversed codes in the grads file (Phys, HCDE, a few others)
# this needs a quick fix to swap the 0's to the correct side of the pathway for merging with Kuali data
# I suppose I could do this the other way and un-pad the majors but I'll stick with this for continuity since
# other apps use the padded version
m <- str_split(grads$MajorCode, "_", n = 3, simplify = T)
m[,3] <- as.numeric(m[,3])
m[,3] <- str_pad(m[,3], 2, side = "right", pad = "0")
grads$mkey <- paste(m[,2], m[,3], sep = "_")
# now safe to filter grads against current majors
old.majors <- grads[!(grads$mkey %in% cur.maj$mkey),]     # may want to inspect
grads <- grads[grads$mkey %in% cur.maj$mkey,]

maj <- cur.maj %>% select(mkey, program_title, title, credentialAdmissionType) %>% distinct(mkey, .keep_all = T)
grads <- grads %>% inner_join(maj)


# merge grads with courses ------------------------------------------------

dat <- inner_join(courses, grads, by = "syskey") %>% distinct(syskey, yrq, mkey, ckey, .keep_all = T)
# dat[dat$yrq > dat$degree.yrq,]

dat <- dat %>%
  group_by(syskey, DegreeCode) %>%
  filter(yrq <= degree.yrq,
         StudentCohortQtrKeyId >= 20084,               #! 20134 --> 20084
         yrq >= StudentCohortQtrKeyId) %>%
  arrange(syskey, mkey, yrq) %>% ungroup()

save(dat, file = "data/clean/first-degree-grads-and-courses-5yrs.Rdata")    #! 5 yrs here
