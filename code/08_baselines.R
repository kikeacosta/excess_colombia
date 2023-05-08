rm(list=ls())
source("Code/00_functions.R")
options(scipen=999)

dt <- read_rds("data_inter/master_sex_age.rds")

bsn <- 
  dt %>% 
  filter(cause != "estudio") %>% 
  mutate(dts = dts + 1) %>% 
  group_by(geo, sex, age, cause) %>% 
  do(est_baseline2(db = .data)) %>% 
  ungroup()

write_rds(bsn, "data_inter/first_try_all.rds")

bsn_tot <- 
  bsn %>% 
  filter(geo == "Total",
         age == "TOT",
         cause == "total",
         sex == "t",
         year <= 2022)

bsn_tot %>% 
  ggplot()+
  geom_line(aes(date, dts))+
  geom_line(aes(date, bsn))+
  geom_ribbon(aes(date, ymin = ll, ymax = ul), alpha = 0.2)+
  geom_point(aes(date, dts))+
  geom_vline(xintercept = ymd("2020-03-15"), linetype = "dashed")+
  facet_wrap(~sex)+
  theme_bw()

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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