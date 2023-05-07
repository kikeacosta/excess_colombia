library(tidyverse)
library(readxl)

pp1 <-
  read_xlsx("data_input/DCD-area-sexo-edad-proypoblacion-dep-2005-2019.xlsx",
            skip = 11) %>% 
  rename(code = 1,
         geo = 2,
         year = 3,
         area = 4) %>% 
  filter(area == "Total") 


pp2 <- 
  read_xlsx("data_input/DCD-area-sexo-edad-proyepoblacion-dep-2020-2050-ActPostCOVID-19.xlsx",
            skip = 11) %>% 
  rename(code = 1,
         geo = 2,
         year = 3,
         area = 4) %>% 
  filter(area == "Total") %>% 
  drop_na(year)

pp3 <- 
  bind_rows(pp1, pp2) %>% 
  gather(-code, -geo, -year, -area, key = sex_age, value = pop) %>% 
  separate(sex_age, c("sex", "age"), sep = "_") %>% 
  mutate(age = ifelse(sex %in% c("Total Mujeres", 
                                 "Total Hombres",
                                 "Total general"), "TOT", age),
         sex = case_when(sex %in% c("Total Mujeres", "Mujeres") ~ "f",
                         sex %in% c("Total Hombres", "Hombres") ~ "m",
                         sex %in% c("Total general", "Total") ~ "t",
                         TRUE ~ "na")) %>% 
  select(-area)

unique(pp3$sex)
unique(pp3$geo)

pp4 <- 
  pp3 %>% 
  bind_rows(pp3 %>% 
  group_by(year, sex, age) %>% 
  summarise(pop = sum(pop)) %>% 
  ungroup() %>% 
  mutate(geo = "Total", code = "00"))

# weekly population ====
# ~~~~~~~~~~~~~~~~~~~~~~

geos <- unique(pp4$geo)
ages <- unique(pp4$age)
sexs <- unique(pp4$sex)

pop_interpol <- 
  expand_grid(geo = geos, sex = sexs, age = ages, 
              year = 2014:2024, week = 1:52) %>% 
  bind_rows(expand_grid(geo = geos, sex = sexs, age = ages, 
                        year = c(2015, 2020), week = 53)) %>% 
  arrange(year, week) %>% 
  left_join(pp4 %>% 
              mutate(week = 26) %>% 
              select(-code)) %>% 
  group_by(geo, sex, age) %>% 
  mutate(t = 1:n()) %>% 
  do(interpop(db = .data)) %>% 
  ungroup()

pop_interpol2 <- 
  pop_interpol %>% 
  mutate(isoweek = paste0(year, "-W", sprintf("%02d", week), "-7"),
         date = ISOweek2date(isoweek))

unique(pop_interpol2$age)
unique(pop_interpol2$geo)
unique(pop_interpol2$sex)

pop_interpol2 %>% 
  filter(geo == "Total",
         sex == "t",
         age == "TOT") %>% 
  ggplot()+
  geom_line(aes(date, pop2))+
  geom_point(aes(date, pop), col = "red")

unique(pop2$age)

pop2 <- 
  pop_interpol2 %>% 
  select(-pop, -t) %>% 
  rename(pop = pop2)

pop3 <- 
  pop2 %>% 
  filter(age != "TOT") %>% 
  mutate(age = age %>% as.double(),
         age = case_when(age == 0 ~ age,
                         age %in% 1:4 ~ 1,
                         age %in% 5:84 ~ age - age%%5,
                         age >= 85 ~ 85)) %>% 
  group_by(geo, sex, age, year, week, isoweek, date) %>% 
  summarise(pop = sum(pop)) %>% 
  ungroup() %>% 
  mutate(age = age %>% as.character()) %>% 
  bind_rows(pop2 %>% 
              filter(age == "TOT"))

unique(pop3$age)

write_rds(pop3, "data_inter/pop_weekly_sex_age.rds")

