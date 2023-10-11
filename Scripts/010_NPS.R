
# Course attendance and evaluation survey data

# to do:
# merge files based on shared columns
# get time (morning, arvo), too
# figure out two-day courses
# put in trainer (from downloaded course - need gender from initials)
# correlations!


# library
library(tidyverse)
library(lubridate)
library(marketr)
library(dunn.test)
library(stringr)

# data and processing ----------------------------------------------------------
numb_dat <- read.csv("data/course_numbers.csv")

course_dat <- read.csv("data/course_attendees.csv") %>% 
  mutate(date = dmy(Course.date)) %>% 
         select(-Course.date) %>% 
  rename(course_code = Course.code,
         year = Year)

# response data, rename questions
response_dat <- read.csv("data/17-07-2023_Survey Response Processed Data - Responses_Processed.csv") %>% 
  mutate(date = dmy(On.what.day.did.you.attend.the.course.)) %>% 
  rename(course_long = "Which.course.did.you.attend.",
         course_code = Course_code,
         uni = "Where.did.you.attend.the.course.",
         cq1 = "How.worthwhile.was.attending.the.course.for.you.",
         cq2 = "Did.you.feel.that.the.training.course.atmosphere.was.welcoming.",
         cq3 = "Did.you.feel.comfortable.interacting.with.the.instructors.",
         cq4 = "Was.the.course.content.new.for.you.",
         cq5 = "Do.you.think.that.the.instructors.were.knowledgable.about.the.material.being.taught.",
         cq6 = "Do.you.think.that.the.instructors.gave.clear.answers.to.questions.",
         cq7 = "Do.you.think.that.the.instructors.were.good.communicators.",
         cq8 = "How.likely.is.it.that.you.will.use.this.technology.more.in.your.research.",
         cq9 = "How.likely.is.it.that.you.would.recommend.Intersect.training.courses.to.colleagues.",
         cq10 = "Do.you.feel.confident.to.apply.what.you.have.learnt.at.the.training.",
         oq1 = "Which.parts.of.the.course.did.you.find.most.useful..Why.",
         oq2 = "Which.parts.of.the.course.did.you.find.least.useful..Why.",
         oq3 = "Do.you.have.any.other.suggestions.or.feedback.on.this.course.or.Intersect.Training.in.general.")
names(response_dat)

# institution column to join data frames
member_code <- unique(course_dat$Member)
write.csv(member_code, "Data/member unifier raw.csv", row.names = F)
response_code <- unique(response_dat$Uni_short)
#write.csv(response_code, "Data/uni unifier raw.csv", row.names = F)
# please note, "institution.csv" was created by manually aligning these two
# files
institution <- read.csv("Data/institution.csv")

response_dat <- response_dat %>% 
  select(-Uni_short) %>% 
  left_join(institution, by = "uni")
head(response_dat)

course_dat <- course_dat %>% 
  left_join(institution, by = "Member") # ignore error
head(course_dat)

# course attendance --------------------------------
# # How many deliveries in each year
# head(numb_dat)
# 
# # 2022's data - do later
# # numb_dat$Course.date <- parse_date_time(numb_dat$Course.date, orders = "dmy")
# # head(numb_dat$Course.date)
# # numb_dat2 <- numb_dat %>% filter(Course.date > "2022-05-10")
# # head(numb_dat2)
# 
# # remove duplicates (day 2 courses)
# numb_dat2 <- numb_dat %>% distinct(EventbriteID, .keep_all = T)
# 
# # course deliveries
# spss101_num <- nrow(numb_dat3 %>% filter(Course.code == "SPSS101"))
# qltrics101_num <- nrow(numb_dat3 %>% filter(Course.code == "QLTRICS101"))
# redcap101_num <- nrow(numb_dat3 %>% filter(Course.code == "REDCAPS101"))
# unix101_num <- nrow(numb_dat3 %>% filter(Course.code == "UNIX101"))
# python101 <- nrow(numb_dat3 %>% filter(Course.code == "PYTHON101"))
# r101 <- nrow(numb_dat3 %>% filter(Course.code == "R101"))

# NPS and average scores -------------------------------------------------------
# calculate NPS
response_dat2 <- response_dat %>% 
                rename(nps_question = cq9)

nps <- nps_calc(response_dat2, date, course_code, institution)                  

# calculate average scores for other nine metrics (cq1-8, 10)
cq_df <- response_dat2 %>% 
  group_by(date, course_code, institution) %>% 
  summarise(across(c(cq1:cq8, cq10, nps_question), ~ mean(.x, na.rm = T)))

# merge
response_sum <- left_join(nps, cq_df, 
                         by = c("date", "course_code", "institution"))
head(response_sum)
sum(!is.na(response_sum$cq2))
sum(is.na(response_sum$cq2))

# join data frames by uni/member, course_code/course.code, date/Course_date
dat <- left_join(course_dat, response_sum, 
                 by = c("date", "course_code", "institution"))
head(dat)
sum(!is.na(dat$cq2))
sum(is.na(dat$cq2))

names(dat)

########################### analyses ################################### 
# Time ------------------------------------------------------
# day of the week
dat <- dat %>% 
  mutate(day = wday(date, label = T))

dat %>% 
  group_by(day) %>% 
  summarise(nps = mean(nps, na.rm = T),
            sum_courses = n()) %>% 
  arrange()

nps_day <- kruskal.test(nps ~ day, data = dat)
m_nps <- dunn.test(x = dat$nps, g = dat$day, method = "bonferroni", kw = T)
m_nps$P.adjusted

# month
# time (arvo, morning)

# year

# Quarter
# day of the week
dat %>% 
  group_by(Quarter) %>% 
  summarise(nps = mean(nps, na.rm = T),
            sum_courses = n()) %>% 
  arrange()

nps_day <- kruskal.test(nps ~ day, data = dat)
m_nps <- dunn.test(x = dat$nps, g = dat$day, method = "bonferroni", kw = T)
m_nps$P.adjusted

# level (1, 2, 300)-------------------------------------------------------------
dat <- dat %>% 
  mutate(level = str_sub(course_code, -3))
table(dat$level)

dat %>% 
  group_by(level) %>% 
  summarise(nps = mean(nps, na.rm = T),
            sum_courses = n()) %>% 
  arrange()

# technology/tool --------------------------------------------------------------
dat %>% 
  group_by(Tool) %>% 
  summarise(nps = mean(nps, na.rm = T),
            sum_courses = n()) %>% 
  filter(sum_courses > 5) %>% 
  arrange(desc(nps)) %>% 
  print(n = 22)

# institution ------------------------------------------------------------------
dat %>% 
  group_by(institution) %>% 
  summarise(nps = mean(nps, na.rm = T),
            sum_courses = n()) %>% 
  filter(sum_courses > 5) %>% 
  arrange(desc(nps))

# role x technology
# year -------------------------------------------------------------------------
# no data for 2016 - 2017
dat %>% 
  group_by(year) %>% 
  summarise(nps = mean(nps, na.rm = T),
            sum_courses = n()) %>% 
  filter(year >= 2018) %>% 
  arrange(year)


# delivery ---------------------------------------------------------------------
# in person or online
dat %>% 
  # filter(year != 2022,
  #        year != 2021) %>% 
  group_by(Delivery) %>% 
  summarise(nps = mean(nps, na.rm = T),
            sum_courses = n()) %>% 
  drop_na()


table(del_dat$Delivery)

wilcox.test(del_dat$Delivery, del_dat$nps)

# 9 quantitative metrics -------------------------------------------------------
# make a table of tests here - e.g. by tool/tech, or institution, ... NPS



# Solo versus packaged (Course.type) -------------------------------------------
table(dat$Course.Type)

dat %>% 
  filter(Course.Type == "Packaged" |
         Course.Type == "Solo")  %>% 
  group_by(Course.Type) %>% 
  summarise(nps = mean(nps, na.rm = T),
            sum_courses = n()) %>% 
  filter(sum_courses > 5) %>% 
  arrange(desc(nps))

