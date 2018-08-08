rm(list = ls())
gc()

library(tidyverse)
library(edwHelpers)

setwd(rstudioapi::getActiveProject())

# ggplot2 template --------------------------------------------------------

# ggplot(data = b, aes(x = o, class.order, label = sprintf("%0.2f", round(pop.in.qtr.total, digits = 2)))) +
#   geom_tile(aes(fill = 100*b$pop.in.qtr.total)) +
#   geom_text(fontface = 'bold') +
#   scale_y_discrete(limits = rev(unique(sort(b$class.order)))) +
#   scale_x_continuous(breaks = b$o, labels = b$term) +
#   scale_fill_viridis_c(direction = -1, option = "C", begin = .5) +       # https://ggplot2.tidyverse.org/reference/scale_viridis.html
#   ylab("Course") +
#   xlab("Quarter") +
#   labs(caption = paste("n students in major:", max(b$n.maj)),
#        fill = "% in quarter",
#        subtitle = paste(unique(b$program_title))) +
#   theme(panel.grid.major = element_blank(),
#         panel.grid.minor = element_line(size = .5, color = 'gray80'))


# helper functions --------------------------------------------------------

# can't do this using group_by()
order.courses <- function(x){
  a <- str_sub(x, end = -5)
  ua <- unique(a)
  co <- factor(a, levels = unique(ua), ordered = T)
  co <- reorder(x, as.numeric(co))
  return(co)
}

# setup -------------------------------------------------------------------

load("data/clean/first-degree-grads-and-courses.Rdata")

# filter; calculate terms/student sequence

dat <- dat %>% filter(dat$AcademicCareerEntryType == "FTFY", get.q(yrq) != 3)

(low <- mean(dat$TimeToDegreeInTerms) - sd(dat$TimeToDegreeInTerms))
(hi <- mean(dat$TimeToDegreeInTerms) + sd(dat$TimeToDegreeInTerms))

dat <- dat %>% filter(TimeToDegreeInTerms >= low, TimeToDegreeInTerms <= hi)
qplot(dat$TimeToDegreeInTerms, binwidth = 1)
range(dat$TimeToDegreeInTerms)
rm(hi, low)

# calculate terms/student and merge back in
x <- dat %>% select(syskey, yrq) %>% distinct() %>% group_by(syskey) %>% arrange(yrq) %>% mutate(qnum = seq(n()))

dat <- dat %>% inner_join(x)
rm(x)

# create quarter name + #
dat$qtr.abbv <- if_else(get.q(dat$yrq) == 1, "Win",
                        if_else(get.q(dat$yrq) == 2, "Spr", "Aut"))
# old fashioned way :(
x <- dat %>% select(syskey, yrq, qtr.abbv) %>% group_by(syskey, yrq, qtr.abbv) %>% distinct() %>% ungroup()
x <- x %>% arrange(syskey, qtr.abbv) %>% group_by(syskey, qtr.abbv) %>% mutate(qtr.order = seq_along(qtr.abbv),
                                                                               term = paste(qtr.abbv, qtr.order, sep = "-"))

dat <- dat %>% inner_join(x) %>% arrange(syskey, yrq)
rm(x)

dat$qtr.abbv <- factor(dat$qtr.abbv, levels = c("Aut", "Win", "Spr"), ordered = T)

# check numbers
table(dat$term)

# create ranks
rk <- dat %>%
  group_by(mkey) %>% mutate(n.maj = n_distinct(syskey), mean.gpa = mean(DegreeGrantedGPA), med.gpa = median(DegreeGrantedGPA)) %>%
  group_by(mkey, term) %>% mutate(n.maj.qtr = n_distinct(syskey)) %>%
  group_by(mkey, term, ckey) %>% mutate(n.maj.qtr.class = n_distinct(syskey)) %>%
  ungroup() %>%
  group_by(mkey, ckey) %>% mutate(n.maj.class = n_distinct(syskey), mean.grade = mean(grade) / 10, med.grade = median(grade) / 10) %>% ungroup()


too.small <- rk %>% filter(n.maj < 5)
# how many? which ones?
cbind(unique(too.small$mkey))
rk <- rk %>% filter(n.maj >= 5)

rk <- rk %>%
  select(mkey, ckey, term, qtr.abbv, qtr.order, starts_with("n."), program_title, title, credentialAdmissionType, starts_with("med."), starts_with("mean.")) %>%
  distinct() %>%
  group_by(mkey, term) %>%
  arrange(qtr.order, qtr.abbv, -n.maj.qtr.class) %>%
  filter(seq_along(n.maj.qtr.class) <= 7) %>%
  ungroup() %>%
  mutate(pop.class.total = n.maj.class / n.maj,                           # prop of [stu in major] who [took this class anytime]
         pop.in.qtr.total = n.maj.qtr.class / n.maj,                      # prop of [stu in major] who [took this class in this quarter num]
         pop.in.qtr.if.class.taken = n.maj.qtr.class / n.maj.class)       # prop of [stu in major who took this class] and [did so in this quarter num]


# term ordering w/ single var
rk <- rk %>% arrange(qtr.order, qtr.abbv) %>%
  mutate(o = rep(seq(from = 1, to = length(rle(rk$term)$lengths)), times = rle(rk$term)$lengths))

table(rk$o, rk$term)

# draw/save function test
b <- rk %>% filter(mkey == "PHYS_10")

# generate ordering for courses by major/not major based on the abbreviations
b$class.order <- order.courses(b$ckey)

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


# wrapper for output ------------------------------------------------------

# input map: major code (or vector of codes)
# gen course order -> print
# setwd("vizzes/take001/")
majors.in <- unique(rk$mkey)
for(i in 1:length(majors.in)){

  b <- rk %>% filter(mkey == majors.in[i])
  b$class.order <- order.courses(b$ckey)

  fname <- paste(unique(b$mkey), unique(b$title), sep = "-")

  p <- ggplot(data = b, aes(x = o, class.order, label = sprintf("%0.2f", round(pop.in.qtr.total, digits = 2)))) +
    geom_tile(aes(fill = 100*b$pop.in.qtr.total)) +
    geom_text(fontface = 'bold') +
    scale_y_discrete(limits = rev(unique(sort(b$class.order)))) +
    scale_x_continuous(breaks = b$o, labels = b$term) +
    scale_fill_viridis_c(direction = -1, option = "C", begin = .5) +
    ylab("Course") +
    xlab("Quarter") +
    labs(caption = paste("n students in major:", max(b$n.maj)),
         fill = "% in quarter",
         subtitle = paste(unique(b$title))) +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_line(size = .5, color = 'gray80'))

  ggsave(fname, path = "vizzes/take001/", plot = p, device = "png", dpi = "print")
}