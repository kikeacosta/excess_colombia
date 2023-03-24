library(tidyverse)
library(haven)
library(lubridate)
dt <- read_sav("data_input/Defun_2013.sav")

colnames(dt)

dt2 <- 
  dt %>% 
  select(dpto = 1,
         year = 9,
         month = 10,
         sex = 13,
         age = 15,
         edu = 17,
         edu2 = 18,
         cause = CAUSA_666)


dtxx <- read_csv("data_input/nofetal2021.CSV")

dtx2 <- 
  dtxx %>% 
  select(dpto = 1,
         year = 7,
         month = 8,
         sex = 11,
         age1 = 13,
         edu1 = 15,
         edu2 = 16)

unique(dtx2$age)

dts <- 
  dtx2 %>% 
  mutate(age = age1 %>% as.integer(),
         age = case_when(age <= 6 ~ 0,
                          age %in% 7:8 ~ 1,
                          age %in% 9:28 ~ (age - 8)*5,
                          age == 29 ~ -1),
         date = make_date(d = 15, m = month, y = year),
         edu = edu1 %>% as.double(),
         edu = case_when(edu1 <= 2 | edu1 == 13 ~ "pr",
                         edu1 == 3 ~ "sc",
                         edu1 %in% 4:12 ~ "ps",
                         edu1 == 99 ~ "un"))

unique(dts$edu)
table(dts$edu)


dts_s <- 
  dts %>% 
  group_by(date, age, sex) %>% 
  summarise(dts = n())





