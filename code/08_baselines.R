rm(list=ls())
source("Code/00_functions.R")

dt <- read_rds("data_inter/master_sex_age.rds")

bsn <- 
  dt %>% 
  mutate(dts = dts + 1) %>% 
  group_by(geo, sex, age, cause) %>% 
  do(est_baseline2(db = .data)) %>% 
  ungroup()

bsn2 <- 
  bsn %>% 
  mutate(dts = dts - 1,
         bsn = bsn - 1,
         ll = ll - 1,
         ul = ul - 1,
         ll = ifelse(ll < 0, 0, ll),
         ul = ifelse(ul < 0, 0, ul)) %>% 
  arrange(year, week, geo, cause, sex, age)

write_rds(bsn2, "data_output/baselines_monthly.rds")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

bsn2 <- read_rds("data_output/baselines_monthly.rds")

bsn2 %>% 
  filter(geo == "total",
         age == "total",
         cause == "total",
         sex == "t",
         year <= 2022) %>% 
  ggplot()+
  geom_line(aes(date, dts))+
  geom_line(aes(date, bsn))+
  geom_ribbon(aes(date, ymin = ll, ymax = ul), alpha = 0.2)+
  geom_point(aes(date, dts))+
  geom_vline(xintercept = ymd("2020-03-15"), linetype = "dashed")+
  facet_wrap(~sex)+
  theme_bw()

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

unique(bsn2$cause)
unique(bsn2$geo)

bsn2 %>% 
  filter(geo == "La Guajira",
         age == "total",
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


# annual estimates =====
# ~~~~~~~~~~~~~~~~~~~~~~
bsn3 <- 
  bsn2 %>% 
  filter(year %in% 2020:2022) %>% 
  mutate(bsn = ifelse(bsn < 0, 0, bsn),
         exc = dts - bsn,
         bsn2 = ifelse(dts < bsn, dts, bsn),
         exc_pos = dts - bsn2) %>% 
  group_by(year, geo, sex, age, cause) %>% 
  summarise(dts = sum(dts),
            bsn = sum(bsn),
            exc = sum(exc),
            exc_pos = sum(exc_pos)) %>% 
  ungroup() %>% 
  mutate(psc = exc / bsn,
         psc_pos = exc_pos / bsn)

write_rds(bsn3, "data_output/baselines_annual.rds")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

geo = case_when(geo == "Archipiélago de San Andrés" ~ "San Andrés y Providencia",
                geo == "Quindio" ~ "Quindío",
                geo == "Bogotá, D.C." ~ "Bogotá",
                geo == "Total" ~ "total",
                TRUE ~ geo)


bsn3 <- read_rds("data_output/baselines_annual.rds")
pp <- read_rds("data_inter/pop_weekly_sex_age.rds")

pp2 <- 
  pp %>% 
  filter(week == 26) %>% 
  select(geo, sex, age, year, pop) %>% 
  mutate(geo = case_when(geo == "Archipiélago de San Andrés" ~ "San Andrés y Providencia",
                         geo == "Quindio" ~ "Qu indío",
                         geo == "Bogotá, D.C." ~ "Bogotá",
                         geo == "Total" ~ "total",
                         TRUE ~ geo))

pp_st <- 
  pp2 %>% 
  filter(geo == "total") %>% 
  select(-geo, pop_st = pop)

bsn4 <- 
  bsn3 %>% 
  left_join(pp2) %>% 
  left_join(pp_st) %>% 
  mutate(mx = exc / pop,
         mx_pos = exc_pos / pop,
         mx_bsn = bsn / pop,
         exc_st = mx * pop_st,
         exc_st_pos = mx_pos * pop_st,
         bsn_st = mx_bsn * pop_st)

exc <- 
  bsn4 %>% 
  group_by(year, geo, sex, cause) %>% 
  summarise(dts = sum(dts),
            bsn = sum(bsn),
            bsn_st = sum(bsn_st),
            exc = sum(exc),
            exc_pos = sum(exc_pos),
            exc_st = sum(exc_st),
            exc_st_pos = sum(exc_st_pos)) %>% 
  ungroup()

psc <- 
  exc %>% 
  mutate(psc = exc/bsn,  
         psc_pos = exc_pos/bsn, 
         psc_st = exc_st/bsn_st, 
         psc_st_pos = exc_st_pos/bsn_st) %>% 
  filter(cause == "total")

write_rds(psc, "data_output/pscores_annual.rds")

