rm(list = ls())
gc()

library(tidyverse)
library(edwHelpers)
library(huxtable)

options(theme_set(theme_bw(base_size = 15)))

load("data/clean/first-degree-grads-and-courses.Rdata")



# diagnostics -------------------------------------------------------------

qplot(data = dat, x = AcademicCareerEntryType, y = DegreeGrantedTransferCredits, geom = 'boxplot')
qplot(data = dat, x = AcademicCareerEntryType, y = TimeToDegreeInTerms, geom = 'boxplot')

# mutations ----------------------------------------------------------------

low <- mean(dat$TimeToDegreeInTerms) - sd(dat$TimeToDegreeInTerms)
hi <- mean(dat$TimeToDegreeInTerms) + sd(dat$TimeToDegreeInTerms)

dat <- dat %>% filter(TimeToDegreeInTerms >= low, TimeToDegreeInTerms <= hi)
qplot(dat$TimeToDegreeInTerms, binwidth = 1)
range(dat$TimeToDegreeInTerms)
rm(hi, low)

# remove summer quarter
dat <- dat %>% filter(get.q(yrq) != 3)

# calculate terms/student and merge back in
x <- dat %>% select(syskey, yrq) %>% distinct() %>% group_by(syskey) %>% arrange(yrq) %>% mutate(qnum = seq(n()))

dat <- dat %>% inner_join(x)
rm(x)
# table(dat$qnum)

# generate quarter name + # -----------------------------------------------

dat$qtr.abbv <- if_else(get.q(dat$yrq) == 1, "Win",
                        if_else(get.q(dat$yrq) == 2, "Spr", "Aut"))
# old fashioned way :(
x <- dat %>% select(syskey, yrq, qtr.abbv) %>% group_by(syskey, yrq, qtr.abbv) %>% distinct() %>% ungroup()
x <- x %>% arrange(syskey, qtr.abbv) %>% group_by(syskey, qtr.abbv) %>% mutate(qtr.order = seq_along(qtr.abbv),
                                                                               term = paste(qtr.abbv, qtr.order, sep = "-"))

dat <- dat %>% inner_join(x) %>% arrange(syskey, yrq)
rm(x)

dat$qtr.abbv <- factor(dat$qtr.abbv, levels = c("Aut", "Win", "Spr"), ordered = T)

# prototype ranks ---------------------------------------------------------

rk <- dat %>%
  group_by(mkey) %>% mutate(n.maj = n_distinct(syskey)) %>%
  group_by(mkey, term) %>% mutate(n.maj.qtr = n_distinct(syskey)) %>%
  group_by(mkey, term, ckey) %>% mutate(n.maj.qtr.class = n_distinct(syskey)) %>%
  ungroup() %>%
  group_by(mkey, ckey) %>% mutate(n.maj.class = n_distinct(syskey)) %>% ungroup()
  # summarize(tot = class.pop = n()) %>%
  # arrange(desc(class.pop)) %>%
  # top_n(7, wt = class.pop) %>%
  # ungroup() %>%
  # arrange(mkey, qnum, class.pop)


b <- rk %>% filter(mkey == "B BUS_10") %>%
  select(mkey, ckey, term, qtr.abbv, qtr.order, starts_with("n."), program_title, credentialAdmissionType) %>%
  distinct() %>%
  arrange(qtr.order, qtr.abbv, -n.maj.qtr.class)

b <- b %>% group_by(mkey, term) %>% arrange(qtr.order, qtr.abbv, -n.maj.qtr.class) %>%             # don't need mkey here but good practice to remember it for later
  filter(seq_along(n.maj.qtr.class) <= 7)                                           # top_n will keep ties, here I arbitrarily discard them

b <- b %>% mutate(pop.class.total = n.maj.class / n.maj,                            # prop of [stu in major] who [took this class anytime]
                  pop.in.qtr.total = n.maj.qtr.class / n.maj,                       # prop of [stu in major] who [took this class in this quarter num]
                  pop.in.qtr.if.class.taken = n.maj.qtr.class / n.maj.class)        # prop of [stu in major who took this class] and [did so in this quarter num]

ggplot(data = b, aes(x = pop.in.qtr.total, y = ckey)) + geom_point() + theme(panel.grid.major.x = element_blank(), panel.grid.minor = element_blank()) + facet_grid(.~ qtr.order + qtr.abbv, scales = "free")
ggplot(data = b, aes(pop.in.qtr.total)) + geom_bar(aes(fill = ckey), position = position_stack(reverse = TRUE)) + theme(panel.grid.major.x = element_blank(), panel.grid.minor = element_blank()) + facet_grid(.~ qtr.order + qtr.abbv, scales = "free")
ggplot(data = b, aes(x = ckey, y = pop.in.qtr.total)) + geom_col(width = .1) + geom_point(size = 1.5) + coord_flip() + theme(panel.grid.major.y = element_blank(), panel.grid.minor = element_blank()) + facet_grid(rows = vars(qtr.order), cols = vars(qtr.abbv), scales = "free")




# comp sci ----------------------------------------------------------------

b <- rk %>% filter(mkey == "C SCI_00") %>%
  select(mkey, ckey, term, qtr.abbv, qtr.order, starts_with("n."), program_title, credentialAdmissionType) %>%
  distinct() %>%
  arrange(qtr.order, qtr.abbv, -n.maj.qtr.class)

b <- b %>% group_by(mkey, term) %>% arrange(qtr.order, qtr.abbv, -n.maj.qtr.class) %>%             # don't need mkey here but good practice to remember it for later
  filter(seq_along(n.maj.qtr.class) <= 7)                                           # top_n will keep ties, here I arbitrarily discard them

b <- b %>% mutate(pop.class.total = n.maj.class / n.maj,                            # prop of [stu in major] who [took this class anytime]
                  pop.in.qtr.total = n.maj.qtr.class / n.maj,                          # prop of [stu in major] who [took this class in this quarter num]
                  pop.in.qtr.if.class.taken = n.maj.qtr.class / n.maj.class)     # prop of [stu in major who took this class] and [did so in this quarter num]

# now can, e.g., rank classes by quarter
#   - if someone took that class at all
b %>% group_by(ckey, add = F) %>% arrange(-pop.class.total, ckey, -pop.in.qtr.if.class.taken)
ggplot(data = b, aes(x = ckey, y = pop.in.qtr.total)) + geom_col(width = .1) + geom_point(size = 1.5) + coord_flip() + theme(panel.grid.major.y = element_blank(), panel.grid.minor = element_blank()) + facet_grid(rows = vars(qtr.order), cols = vars(qtr.abbv), scales = "free")

b$o <- rep(seq(from = 1, length.out = nrow(b)/7), each = 7)
tick.labs <- unique(b$term)
ggplot(data = b, aes(x = o, y = ckey)) + geom_point() + geom_line() + ylab("Course") + xlab("Quarter") + labs(caption = paste(unique(b$program_title))) + scale_x_continuous(breaks = b$o, labels = b$term)
ggplot(data = b, aes(x = o, y = ckey)) + geom_point(size = b$pop.in.qtr.total*10) + ylab("Course") + xlab("Quarter") + labs(caption = paste(unique(b$program_title))) + scale_x_continuous(breaks = b$o, labels = b$term)
ggplot(data = b, aes(x = o, ckey)) +
  geom_point(size = log(100*b$pop.in.qtr.total)) + # geom_line(color = "gray70") +
  ylab("Course") + xlab("Quarter") + labs(caption = paste(unique(b$program_title))) +
  scale_y_discrete(limits = rev(unique(sort(b$ckey)))) +
  scale_x_continuous(breaks = b$o, labels = b$term) +
  theme(panel.grid.major = element_blank(), panel.grid.minor.x = element_blank())


# geom_tile ---------------------------------------------------------------

ggplot(data = b, aes(x = o, ckey)) +
  geom_tile(aes(fill = 100*b$pop.in.qtr.total)) +
  ylab("Course") + xlab("Quarter") +
  labs(caption = paste("n students in major:", max(b$n.maj)),
       fill = "% in quarter",
       subtitle = paste(unique(b$program_title))) +
  scale_y_discrete(limits = rev(unique(sort(b$ckey)))) +
  scale_x_continuous(breaks = b$o, labels = b$term) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_line(size = .5, color = 'gray80'))

# TODO: wrap the select major and ggplot in a function
# TODO: intelligent re-ordering of ckey with major at top? Or total popularity top -> bottom?

# try: total pop, top -> bottom as order
ggplot(data = b, aes(x = o, reorder(ckey, pop.class.total))) +
  geom_tile(aes(fill = 100*b$pop.in.qtr.total)) +
  scale_x_continuous(breaks = b$o, labels = b$term) +
  scale_fill_distiller(palette = "Purples", direction = 1)

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

dat %>% distinct(syskey, degree.yrq) %>% group_by(degree.yrq) %>% summarize(n())

dat %>% distinct(syskey, program_title) %>% group_by(program_title) %>% summarize(n())
dat %>% distinct(syskey, program_title, degree.yrq) %>% group_by(program_title, degree.yrq) %>% summarize(n())

<<<<<<< HEAD
deg.gpa <- dat %>% distinct(syskey, program_title, DegreeGrantedGPA)
qplot(data= deg.gpa, x = program_title, y = DegreeGrantedGPA, geom = "boxplot") + coord_flip() + scale_y_continuous(limits = c(1.9, NA))
rm(deg.gpa)
=======
>>>>>>> d96ce22055ef347fa3febafa36f22379be7fba17
