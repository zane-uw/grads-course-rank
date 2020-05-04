rm(list = ls())
gc()

library(tidyverse)
library(edwHelpers)
library(RColorBrewer)

setwd(rstudioapi::getActiveProject())

custom.pal <- palette(brewer.pal(5, 'YlGnBu'))

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

# function to re-order courses w/in groups
order.courses <- function(x){
  a <- str_sub(x, end = -5)
  ua <- unique(a)
  co <- factor(a, levels = unique(ua), ordered = T)
  co <- reorder(x, as.numeric(co))
  return(co)
}

# setup -------------------------------------------------------------------

# load("data/clean/first-degree-grads-and-courses-5yrs.Rdata")
# load('data/clean/all-degrees-ranked-data.RData')
load('data/clean/all-degree-grads-and-courses-5yrs.Rdata')


# filter; calculate terms/student sequence
# dat <- dat %>% filter(dat$AcademicCareerEntryType == "FTFY", get.q(yrq) != 3)

# (low <- mean(dat$TimeToDegreeInTerms) - sd(dat$TimeToDegreeInTerms))
# (hi <- mean(dat$TimeToDegreeInTerms) + sd(dat$TimeToDegreeInTerms))

# I'm editing this to a 4-year path
range(dat$TimeToDegreeInTerms)
median(dat$TimeToDegreeInTerms)
dat <- dat %>% filter(TimeToDegreeInTerms == median(TimeToDegreeInTerms))

# dat <- dat %>% filter(TimeToDegreeInTerms >= low, TimeToDegreeInTerms <= hi)
# qplot(data = dat, x = TimeToDegreeInTerms, binwidth = 1)
# range(dat$TimeToDegreeInTerms)
# rm(hi, low)

# calculate terms/student and merge back in
x <- dat %>% select(system_key, yrq) %>% distinct() %>% group_by(system_key) %>% arrange(yrq) %>% mutate(qnum = seq(n()))

dat <- dat %>% inner_join(x)
rm(x)

# create quarter name + #
dat$qtr.abbv <- if_else(get.q(dat$yrq) == 1, "Win",
                        if_else(get.q(dat$yrq) == 2, "Spr", "Aut"))
# old fashioned way :(
x <- dat %>% select(system_key, yrq, qtr.abbv) %>% group_by(system_key, yrq, qtr.abbv) %>% distinct() %>% ungroup()
x <- x %>% arrange(system_key, qtr.abbv) %>% group_by(system_key, qtr.abbv) %>% mutate(qtr.order = seq_along(qtr.abbv),
                                                                               term = paste(qtr.abbv, qtr.order, sep = "-"))

dat <- dat %>% inner_join(x) %>% arrange(system_key, yrq)
rm(x)

dat$qtr.abbv <- factor(dat$qtr.abbv, levels = c("Aut", "Win", "Spr"), ordered = T)

# check numbers
table(dat$term)     # nicely 'even' buckets

# create ranks
rk <- dat %>%
  group_by(major_key) %>% mutate(n.maj = n_distinct(system_key), mean.gpa = mean(DegreeGrantedGPA), med.gpa = median(DegreeGrantedGPA)) %>%
  group_by(major_key, term) %>% mutate(n.maj.qtr = n_distinct(system_key)) %>%
  group_by(major_key, term, ckey) %>% mutate(n.maj.qtr.class = n_distinct(system_key)) %>%
  ungroup() %>%
  group_by(major_key, ckey) %>%
  mutate(n.maj.class = n_distinct(system_key), mean.grade = mean(grade) / 10, med.grade = median(grade) / 10) %>%
  ungroup()


too.small <- rk %>% filter(n.maj < 5)
# how many? which ones?
cbind(sort(unique(too.small$major_key)))
write(cbind(unique(too.small$major_key)), file = "data/paths_with_too_few_students.txt", ncolumns = 1, append = F)
rk <- rk %>% filter(n.maj >= 5)

rk <- rk %>% select(major_key,
                   starts_with(c('n.', 'med.', 'mean.')),
                   ckey,
                   term,
                   qtr.abbv,
                   qtr.order,
                   program_title,
                   program_admissionType) %>%
  distinct() %>%
  group_by(major_key, term) %>%
  arrange(qtr.order, qtr.abbv, -n.maj.qtr.class) %>%
  filter(seq_along(n.maj.qtr.class) <= 7) %>%
  ungroup() %>%
  mutate(pop.class.total = n.maj.class / n.maj,                           # prop of [stu in major] who [took this class anytime]
         pop.in.qtr.total = n.maj.qtr.class / n.maj,                      # prop of [stu in major] who [took this class in this quarter num]
         pop.in.qtr.if.class.taken = n.maj.qtr.class / n.maj.class)       # prop of [stu in major who took this class] and [did so in this quarter num]


# term ordering w/ single var
rk <- rk %>%
  arrange(qtr.order, qtr.abbv) %>%
  mutate(o = rep(seq(from = 1, to = length(rle(rk$term)$lengths)), times = rle(rk$term)$lengths))

table(rk$o, rk$term)

# draw/save function test
b <- rk[grepl("ISS O", rk$major_key),] # %>% filter(major_key == "PHYS_00")

# generate ordering for courses by major/not major based on the abbreviations
b$class.order <- order.courses(b$ckey)

# Tweaks suggested by @hlee

ggplot(data = b, aes(x = o, class.order, label = sprintf("%0.2i", round(100*pop.in.qtr.total)))) +
  geom_tile(aes(fill = 100*b$pop.in.qtr.total, alpha = .99)) +
  geom_text(fontface = 'bold') +
  scale_y_discrete(limits = rev(unique(sort(b$class.order)))) +
  scale_x_continuous(breaks = b$o, labels = b$term) +
  scale_fill_gradientn(colors = custom.pal, limits = c(1, 100), guide = "colorbar") +
  ylab("Course") +
  xlab("Quarter") +
  guides(alpha = F) +
  labs(caption = paste("n students in major:", max(b$n.maj)),
       fill = "% in quarter",
       subtitle = paste(unique(b$title))) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_line(size = .5, color = 'gray80'),
        panel.background = element_rect(fill = NA))

# wrapper for output ------------------------------------------------------

# input map: major code (or vector of codes)
# gen course order -> print
# setwd("vizzes/take001/")
majors.in <- sort(unique(rk$major_key))
for(i in 1:length(majors.in)){

  b <- rk %>% filter(major_key == majors.in[i])
  b$class.order <- order.courses(b$ckey)

  fname <- paste(unique(b$major_key), unique(b$title), sep = "-")

  p <- ggplot(data = b, aes(x = o, class.order, label = sprintf("%0.2i", round(100*pop.in.qtr.total)))) +
    geom_tile(aes(fill = 100*b$pop.in.qtr.total)) +   # alpha = ...
    geom_text(fontface = 'bold') +
    scale_y_discrete(limits = rev(unique(sort(b$class.order)))) +
    scale_x_continuous(breaks = b$o, labels = b$term) +
    scale_fill_gradient2(low = "#FFFF9F", mid = "#29AB87", high = "#54278f", midpoint = 50, limits = c(1, 100)) +
    ylab("Course") +
    xlab("Quarter") +
    guides(alpha = F) +
    labs(caption = paste("n students in major:", max(b$n.maj)),
         fill = "% in quarter",
         subtitle = paste(unique(b$title))) +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_line(size = .5, color = 'gray80'),
          panel.background = element_rect(fill = NA))

  #! ggsave(fname, path = "vizzes/two years/", plot = p, device = "png", dpi = "print")
  ggsave(fname, path = "vizzes/five years/", plot = p, device = "png", dpi = "screen")
}