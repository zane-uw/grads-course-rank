rm(list = ls())
gc()

library(tidyverse)
library(edwHelpers)

options(theme_set(theme_bw(base_size = 15)))

load("data/clean/first-degree-grads-and-courses.Rdata")


# mutations ----------------------------------------------------------------

# r <- dat[dat$yrq == 20163,]
# x <- dat[dat$syskey == 305308,]
# using time to degree in terms, not years - summer appears to count as a term unless it was the first term
x <- dat[dat$syskey == 599234,]
length(unique(x$yrq)); x$TimeToDegreeInTerms[1]

rm(x, r)
mean(dat$TimeToDegreeInTerms)
sd(dat$TimeToDegreeInTerms)
2*sd(dat$TimeToDegreeInTerms)

low <- mean(dat$TimeToDegreeInTerms) - sd(dat$TimeToDegreeInTerms)
hi <- mean(dat$TimeToDegreeInTerms) + sd(dat$TimeToDegreeInTerms)

dat <- dat %>% filter(TimeToDegreeInTerms >= low, TimeToDegreeInTerms <= hi)
qplot(dat$TimeToDegreeInTerms, binwidth = 1)
range(dat$TimeToDegreeInTerms)

# remove summer starts and old, old records?
dat <- dat %>% filter(yrq >= StudentCohortQtrKeyId)

# remove summer quarter
dat <- dat %>% filter(get.q(yrq) != 3)

# calculate terms/student, merge back in
x <- dat %>% select(syskey, yrq) %>% distinct() %>% group_by(syskey) %>% arrange(yrq) %>% mutate(qnum = seq(n()))

dat <- dat %>% inner_join(x)

table(dat$qnum)
# dat <- dat %>% arrange(syskey, mkey, yrq) %>% group_by(syskey, mkey) %>% mutate(qnum = qtr.diff(yrq, min(yrq)) + 1)
# table(dat$qnum)
  #
  # wtf <- dat[dat$qnum >= 40,]
  # View(dat[dat$syskey == 892339,])

# prototype ranks ---------------------------------------------------------

r <- dat %>% group_by(mkey, qnum, ckey) %>% summarize(class.pop = n()) %>% arrange(desc(class.pop)) %>% top_n(7, wt = class.pop) %>%
  ungroup() %>% arrange(mkey, qnum, class.pop)
  # x <- r[r$mkey == "B BUS_10" & r$qnum == 3,]
  # cbind(xtabs(n ~ ckey, data = x))
  #
  #
  # # r <- r %>% group_by(mkey, qnum) %>% arrange(desc(n)) %>% top_n(10, n) %>% ungroup()
  #
  # # rk <- r %>% select(mkey, qnum, ckey, ranking = n) %>% distinct() %>% arrange(mkey, qnum)
