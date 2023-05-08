library(tidyverse)
library(readxl)
rm(list=ls())
source("Code/00_functions.R")
options(scipen=999)

pp <- read_rds("data_inter/pop_weekly_sex_age.rds")
dt <- read_rds("data_inter/deaths_weekly_sex_age.rds")

unique(pp$geo)
unique(dt$geo)

pp2 <- 
  pp %>% 
  mutate(geo = case_when(geo == "Archipiélago de San Andrés" ~ "San Andrés y Providencia",
                         geo == "Quindio" ~ "Quindío",
                         geo == "Bogotá, D.C." ~ "Bogotá",
                         TRUE ~ geo))

dt2 <- 
  dt %>% 
  mutate(year = year %>% as.double()) %>% 
  left_join(pp2)

dt2 %>% filter(is.na(pop)) %>% pull(geo) %>% unique()

dt3 <- 
  dt2 %>% 
  group_by(geo, sex, age, cause) %>% 
  arrange(date) %>% 
  mutate(t = 1:n()) %>% 
  ungroup() %>% 
  mutate(exposure = pop / 52,
         w = ifelse(date < "2020-03-15", 1, 0)) %>% 
  select(year, week, date, geo, sex, age, cause, dts, pop, exposure, t, w) %>% 
  arrange(date, geo, sex, age, cause)  

write_rds(dt3, "data_inter/master_sex_age.rds")

test <- 
  dt3 %>% 
  filter(geo %in% c("Total", "Bogotá", "Atlántico"),
         cause != "estudio") %>% 
  group_by(geo, sex, age, cause) %>% 
  do(est_baseline(db = .data)) %>% 
  ungroup()

unique(test$cause)
unique(test$geo)

chunk <- 
  dt3 %>% 
  filter(geo %in% c("Bogotá"),
         cause == "natural",
         sex == "t",
         age == "80") %>% 
  group_by(geo, sex, age, cause) %>% 
  do(est_baseline(db = .data)) %>% 
  ungroup()

test %>% 
  filter(geo == "Total",
         age == "TOT",
         cause == "total",
         sex != "t",
         year <= 2022) %>% 
  ggplot()+
  geom_line(aes(date, dts))+
  geom_line(aes(date, bsn))+
  geom_ribbon(aes(date, ymin = ll, ymax = ul), alpha = 0.2)+
  geom_point(aes(date, dts))+
  geom_vline(xintercept = ymd("2020-03-15"), linetype = "dashed")+
  facet_wrap(~sex)+
  theme_bw()
