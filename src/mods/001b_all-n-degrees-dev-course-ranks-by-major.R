rm(list = ls())
gc()

library(tidyverse)
library(edwHelpers)
library(huxtable)

options(theme_set(theme_bw(base_size = 15)))

load("data/clean/all-degree-grads-and-courses-5yrs.Rdata")

# diagnostics -------------------------------------------------------------

qplot(data = dat, x = AcademicCareerEntryType, y = DegreeGrantedTransferCredits, geom = 'boxplot')
qplot(data = dat, x = AcademicCareerEntryType, y = TimeToDegreeInTerms, geom = 'boxplot')

# mutations ----------------------------------------------------------------

# low <- mean(dat$TimeToDegreeInTerms, na.rm = T) - sd(dat$TimeToDegreeInTerms, na.rm = T)
# hi <- mean(dat$TimeToDegreeInTerms, na.rm = T) + sd(dat$TimeToDegreeInTerms, na.rm = T)

# dat <- dat %>% filter(TimeToDegreeInTerms >= low, TimeToDegreeInTerms <= hi)
# qplot(dat$TimeToDegreeInTerms, binwidth = 1)
# range(dat$TimeToDegreeInTerms)
# rm(hi, low)

# remove summer quarter
dat <- dat %>% filter(get.q(yrq) != 3)

# terms per student-degree
dat <- dat %>% group_by(system_key, major_key) %>%
  arrange(system_key, major_key, yrq) %>%
  mutate(qnum = rep(1:length(unique(yrq)), times = rle(yrq)$lengths))


# generate quarter name + # -----------------------------------------------

# same kind of rep/rle - arranged by qtr and grouped by key+qtr.abbv+major
dat$qtr.abbv <- if_else(get.q(dat$yrq) == 1, "Win",
                        if_else(get.q(dat$yrq) == 2, "Spr", "Aut"))

dat <- dat %>%
  arrange(system_key, major_key, qtr.abbv, yrq) %>%
  group_by(system_key, qtr.abbv, major_key, add = F) %>%
  mutate(qtr.order = rep(1:length(unique(yrq)), times = rle(yrq)$lengths)) %>%
  arrange(system_key, major_key, yrq) %>%
  ungroup() %>%
  mutate(term = paste(qtr.abbv, qtr.order, sep = "-"))

dat$qtr.abbv <- factor(dat$qtr.abbv, levels = c("Aut", "Win", "Spr"), ordered = T)

# ranks ---------------------------------------------------------

rk <- dat %>%
  group_by(major_key) %>%
  mutate(n.maj = n_distinct(system_key)) %>%            # distinct students in major
  group_by(major_key, term) %>%
  mutate(n.maj.qtr = n_distinct(system_key)) %>%        # distinct students by major + term
  group_by(major_key, term, ckey) %>%
  mutate(n.maj.qtr.class = n_distinct(system_key)) %>%  # distinct students by major + term + course
  ungroup() %>%
  group_by(major_key, ckey) %>%
  mutate(n.maj.class = n_distinct(system_key)) %>%      # distinct students by major + course (not term as above)
  ungroup()

# save(rk, file = "data/clean/all-degrees-ranked-data.RData")


# Create shiny-compatible data --------------------------------------------

shinydat <- rk %>%
  select(major_key, ckey, term, qtr.abbv, qtr.order, starts_with("n."), program_title, credential_title, program_admissionType, starts_with("med."), starts_with("mean.")) %>%
  distinct() %>%
  group_by(major_key, term) %>%
  arrange(qtr.order, qtr.abbv, -n.maj.qtr.class) %>%
  filter(row_number() <= 7) %>%
  ungroup() %>%
  mutate(per.course.total = n.maj.class / n.maj,
         per.course.in.qtr = n.maj.qtr.class / n.maj) %>%
  arrange(qtr.order, qtr.abbv) %>%
  mutate(o = rep(seq(from = 1, to = length(rle(term)$lengths)), times = rle(term)$lengths))
# table(shinydat$o, shinydat$term)

save(shinydat, file = "data/clean/shiny-data.RData")

# draw/save function test
b <- shinydat %>% filter(major_key == "0_FINANC_00_1_2", n.maj.class >= 10)

# generate ordering for courses by major/not major based on the abbreviations
b$class.order <- order.courses(b$ckey)
b$o <- rep(1:length(unique(b$term)), time = rle(b$term)$lengths)
tick.labs <- unique(b$term)

# GEOM_TILE ---------------------------------------------------------------

ggplot(data = b, aes(x = o, class.order, label = sprintf("%0.2f", round(per.course.in.qtr, digits = 2)))) +
  geom_tile(aes(fill = 100*b$per.course.in.qtr)) +
  geom_text(fontface = 'bold') +
  scale_y_discrete(limits = rev(unique(sort(b$class.order)))) +
  scale_x_continuous(breaks = b$o, labels = b$term) +
  scale_fill_viridis_c(direction = -1, option = "C", begin = .5) +       # https://ggplot2.tidyverse.org/reference/scale_viridis.html
  ylab("Course") +
  xlab("Quarter") +
  labs(caption = paste("n students in major:", max(b$n.maj)),
       fill = "% in quarter",
       subtitle = paste(unique(b$credential_title))) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_line(size = .5, color = 'gray80'))

ggplot(data = b, aes(x = o, y = ckey)) +
  geom_tile(aes(fill = 100*b$per.course.in.qtr)) +
  ylab("Course") + xlab("Quarter") +
  labs(caption = paste("n students in major:", max(b$n.maj)),
       fill = "% in quarter",
       subtitle = paste(unique(b$program_title))) +
  scale_y_discrete(limits = rev(unique(sort(b$ckey)))) +
  scale_x_continuous(breaks = b$o, labels = b$term) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_line(size = .5, color = 'gray80'))

# try: total pop, top -> bottom as order
ggplot(data = b, aes(x = o, reorder(ckey, pop.class.total))) +
  geom_tile(aes(fill = 100*b$pop.in.qtr.total)) +
  scale_x_continuous(breaks = b$o, labels = b$term) +
  scale_fill_distiller(palette = "Purples")

# try: major/not major as order
a <- str_sub(b$ckey, end = 4)
ua <- unique(a)
b$class.order <- factor(a, levels = ua, ordered = T)
b$class.order <- reorder(b$ckey, as.numeric(b$class.order))

ggplot(data = b, aes(x = o, class.order, label = sprintf("%0.2f", round(pop.in.qtr.total, digits = 2)))) +
  geom_tile(aes(fill = 100*b$pop.in.qtr.total)) +
  geom_text(fontface = 'bold') +
  scale_y_discrete(limits = rev(unique(sort(b$class.order)))) +
  scale_x_continuous(breaks = b$o, labels = b$term) +
  scale_fill_viridis_c(direction = -1, option = "C", begin = .5) +       # https://ggplot2.tidyverse.org/reference/scale_viridis.html
  ylab("Course") +
  xlab("Quarter") +
  labs(caption = paste("n students in major:", max(b$n.maj)),
       fill = "% in quarter",
       subtitle = paste(unique(b$program_title))) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_line(size = .5, color = 'gray80'))



# misc summary data -------------------------------------------------------

dat %>% distinct(system_key, degree.yrq) %>% group_by(degree.yrq) %>% summarize(n())

dat %>% distinct(system_key, program_title) %>% group_by(program_title) %>% summarize(n())
dat %>% distinct(system_key, program_title, degree.yrq) %>% group_by(program_title, degree.yrq) %>% summarize(n())

deg.gpa <- dat %>% distinct(system_key, program_title, DegreeGrantedGPA)
qplot(data= deg.gpa, x = program_title, y = DegreeGrantedGPA, geom = "boxplot") + coord_flip() + scale_y_continuous(limits = c(1.9, NA))
rm(deg.gpa)