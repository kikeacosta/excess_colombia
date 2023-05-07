rm(list=ls())
source("Code/00_functions.R")
options(scipen=999)

# wks <- 
#   read_xlsx("data_input/anexos-defunciones-covid-dept-semana-05-2023.xlsx",
#             sheet = 1,
#             skip =  10,
#             col_types = "text")

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
  select(-id)

unique(dt2$geo)

# adding total for the whole country
tot1 <- 
  dt2 %>% 
  filter(geo != "Total") %>% 
  group_by(year, week, sex, age, cause) %>% 
  summarise(dts = sum(dts)) %>% 
  ungroup() %>% 
  mutate(geo = "Total") %>% 
  bind_rows(dt2 %>% 
              filter(geo != "Total"))
  
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
  gather(f,m,t, key = sex, value = dts) %>% 
  replace_na(list(dts = 0))

# age imputation
db <-
  dt3 %>%
  filter(year == 2015,
         geo == "Atlántico",
         sex == "t",
         week == "Total",
         cause == "violenta")

age_imp <-
  function(db){
    tot <-
      db %>%
      filter(age == "Total") %>%
      pull(dts)

    out <-
      db %>%
      filter(!age %in% c("Total", "Edad desconocida")) %>%
      mutate(dts2 = (dts/sum(dts))*tot)

    return(out)
  }

# pausing age inmputation (>30 mins)
# dt4 <- 
#   dt3 %>%
#   group_by(year, geo, sex, week, cause) %>% 
#   do(age_imp(db = .data)) %>% 
#   ungroup()
#   spread()

unique(dt3$age)
# ignoring unknown age so far
tots <- 
  dt3 %>% 
  filter(age == "Total")

dt4 <- 
  dt3 %>% 
  filter(!age %in% c("Total", "Edad desconocida")) %>% 
  mutate(age = str_replace(age, "De ", ""),
         age = str_sub(age, 1, 2),
         age = ifelse(age == "Me",  0, age %>% as.double()),
         age = age %>% as.character()) %>% 
  bind_rows(tots %>% 
              mutate(age = "TOT"))


# geo imputation
# ~~~~~~~~~~~~~~
geo_imp <-
  function(db){
    tot <-
      db %>%
      filter(geo == "Total") %>%
      pull(dts)
    
    out <-
      db %>%
      filter(!geo %in% c("Total", "Edad desconocida")) %>%
      mutate(dts2 = (dts/sum(dts))*tot)
    
    return(out)
  }

# ignoring unknown geo, foreign so far
dt5 <- 
  dt4 %>% 
  filter(!geo %in% c("Extranjero", "Sin información"))

unique(dt5$geo)
unique(dt5$age)
unique(dt5$sex)

# all causes
# ~~~~~~~~~~
dt6 <- 
  dt5 %>% 
  spread(cause, dts) %>% 
  mutate (total = estudio + natural + violenta) %>% 
  gather(total, estudio, natural, violenta, key = cause, value = dts)


# adding dates ====
dt7 <- 
  dt6 %>% 
  filter(week != "Total") %>% 
  mutate(week = str_replace(week, "semana |Semana ", ""),
         week = week %>% as.double(),
         isoweek = paste0(year, "-W", sprintf("%02d", week), "-7"),
         date = ISOweek2date(isoweek)) %>% 
  arrange(date, sex, geo)

dt7 %>% 
  filter(geo == "Total",
         age == "TOT",
         sex == "t") %>% 
  ggplot()+
  geom_point(aes(date, dts))+
  facet_wrap(~cause, scales = "free_y")+
  theme_classic()

dt7 %>% 
  filter(geo == "Amazonas",
         age == "TOT",
         sex == "t") %>% 
  ggplot()+
  geom_point(aes(date, dts))+
  facet_wrap(~cause, scales = "free_y")+
  theme_classic()

write_rds(dt7, "data_inter/deaths_weekly_sex_age.rds")

