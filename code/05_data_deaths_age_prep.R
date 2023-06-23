rm(list=ls())
source("Code/00_functions.R")
options(scipen=999)

dt <-
  read_xlsx("data_input/anexos-defunciones-covid-dept-semana-05-2023.xlsx",
            sheet = 2,
            skip =  11,
            col_types = "text")

dt2 <- 
  dt %>% 
  rename(year = 1,
         geo = 2,
         age = 3,
         week = 4) %>% 
  drop_na(week) %>% 
  fill(year, geo, age) %>% 
  gather(-year, -geo,   -age,   -week, key = sex, value = dts) %>% 
  mutate(year = str_replace(year, "pr", ""),
         id = str_sub(sex, -2, -1),
         id = str_replace(id, "\\.", "") %>% as.double(),
         cause = case_when(id %in% c(5:8) ~ "natural",
                           id %in% c(9:12) ~ "violenta",
                           id %in% c(13:16) ~ "estudio",
                           TRUE ~ "aaaa"),
         sex = str_sub(sex, 1, 1) %>% str_to_lower(),
         dts = dts %>% as.double()) %>% 
  select(-id) %>% 
  filter(week != "Total")
  

unique(dt2$geo)

# adding total for the whole country
tot1 <- 
  dt2 %>% 
  filter(geo != "Total") %>% 
  group_by(year, week, sex, age, cause) %>% 
  summarise(dts = sum(dts)) %>% 
  ungroup() %>% 
  mutate(geo = "total") %>% 
  bind_rows(dt2 %>% 
              filter(geo != "Total"))
  
# # ignoring unknown geo, foreign so far
# dt5 <- 
#   dt4 %>% 
#   filter(!geo %in% c("Extranjero", "Sin información"))


# sex imputation
dt3 <- 
  tot1 %>% 
  spread(sex, dts) %>% 
  mutate(f = f %>% as.double(),
         i = i %>% as.double(),
         m = m %>% as.double(),
         t = t %>% as.double(),
         f2 = f+i*f/(f+m),
         m2 = m+i*m/(f+m)) %>% 
  select(-f, -m, -i) %>% 
  rename(f = f2, m = m2) %>% 
  gather(f, m, t, key = sex, value = dts) %>% 
  replace_na(list(dts = 0))

# cause imputation
dt4 <- 
  dt3 %>% 
  spread(cause, dts) %>% 
  mutate(total = natural+violenta+estudio,
         nat = ifelse(total == 0, 0,
                      natural+estudio*natural/total),
         vio = ifelse(total == 0, 0,
                      violenta+estudio*violenta/total)) %>% 
  select(-natural, -violenta, -estudio) %>% 
  gather(nat, vio, total, key = cause, value = dts) %>% 
  arrange(year, week, geo, cause, sex, age) %>% 
  mutate(cause = case_when(cause == "nat" ~ "natural",
                           cause == "vio" ~ "violenta",
                           TRUE ~ cause))

unique(dt4$age)

# age imputation
tots_age <- 
  dt4 %>% 
  filter(age == "Total") %>% 
  rename(dts_tot = dts) %>% 
  select(-age)

dt5 <-
  dt4 %>%
  filter(!age %in% c("Total", "Edad desconocida")) %>%
  group_by(year, week, geo, cause, sex) %>%
  mutate(knw_tot = sum(dts),
         ci = ifelse(knw_tot == 0, 0, dts/sum(dts))) %>%
  ungroup() %>% 
  left_join(tots_age) %>% 
  mutate(dts2 = dts_tot * ci) %>% 
  select(-ci, -dts, -dts_tot, - knw_tot) %>% 
  mutate(age = str_replace(age, "De ", ""),
         age = str_sub(age, 1, 2),
         age = ifelse(age == "Me",  0, age %>% as.double()),
         age = age %>% as.character()) %>% 
  rename(dts = dts2) %>% 
  bind_rows(tots_age %>% rename(dts = dts_tot) %>% mutate(age = "total")) %>% 
  arrange(year, week, geo, cause, sex, age)


unique(dt5$geo)

# geo imputation
# ~~~~~~~~~~~~~~
tots_geo <- 
  dt5 %>% 
  filter(geo == "total") %>% 
  rename(dts_tot = dts) %>% 
  select(-geo)

dt6 <-
  dt5 %>%
  filter(!geo %in% c("total", "Sin información", "Extranjero")) %>%
  group_by(year, week, cause, sex, age) %>%
  mutate(knw_tot = sum(dts),
         ci = ifelse(knw_tot == 0, 0, dts/sum(dts))) %>%
  ungroup() %>% 
  left_join(tots_geo) %>% 
  mutate(dts2 = dts_tot * ci) %>% 
  select(-ci, -dts, -dts_tot, - knw_tot)  %>% 
  rename(dts = dts2) %>% 
  bind_rows(tots_geo %>% rename(dts = dts_tot) %>% mutate(geo = "total")) %>% 
  arrange(year, week, geo, cause, sex, age)

unique(dt6$geo)
unique(dt6$cause)
unique(dt6$sex)
unique(dt6$age)


# adding dates ====
dt7 <- 
  dt6 %>% 
  # filter(week != "Total") %>% 
  mutate(week = str_replace(week, "semana |Semana ", ""),
         week = week %>% as.double(),
         isoweek = paste0(year, "-W", sprintf("%02d", week), "-7"),
         date = ISOweek2date(isoweek)) %>% 
  arrange(date, sex, geo)

dt7 %>% 
  filter(geo == "total",
         age == "total",
         sex == "t") %>% 
  ggplot()+
  geom_point(aes(date, dts))+
  facet_wrap(~cause, scales = "free_y")+
  theme_classic()

dt7 %>% 
  filter(geo == "Amazonas",
         age == "total",
         sex == "t") %>% 
  ggplot()+
  geom_point(aes(date, dts))+
  facet_wrap(~cause, scales = "free_y")+
  theme_classic()

write_rds(dt7, "data_inter/deaths_weekly_sex_age.rds")

