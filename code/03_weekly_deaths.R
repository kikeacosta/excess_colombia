rm(list=ls())
source("Code/00_functions.R")

wks <- 
  read_xlsx("data_input/week_dates.xlsx")


wks2 <- 
  wks %>% 
  rename(week = Semana) %>% 
  gather(-week, key = year, value = date) %>% 
  mutate(date = ymd(date),
         year = year %>% as.double()) %>% 
  drop_na(date)


dt <-
  read_xlsx("data_input/weekly_deaths.xlsx")

dt2 <- 
  dt %>% 
  filter(week != "Total") %>% 
  mutate(week = week %>% as.double()) %>% 
  left_join(wks2)

dt2 %>% 
  ggplot()+
  geom_point(aes(date, deaths))+
  geom_line(aes(date, deaths))



pop <- 
  read_rds("data_inter/pop.rds") %>% 
  mutate(week = 26)

pop_interpol <- 
  expand_grid(year = 2014:2022, week = 1:52) %>% 
  bind_rows(expand_grid(year = c(2015, 2020), week = 53)) %>% 
  arrange(year, week) %>% 
  left_join(pop) %>% 
  mutate(t = 1:n()) %>% 
  do(interpop(db = .data)) %>% 
  ungroup() %>% 
  left_join(wks2)

pop_interpol %>% 
  ggplot()+
  geom_line(aes(date, pop2))+
  geom_point(aes(date, pop), col = "red")

pop2 <- 
  pop_interpol %>% 
  select(-pop, -t) %>% 
  rename(pop = pop2)

dt3 <- 
  dt2 %>% 
  left_join(pop2)


write_rds(dt3, "data_inter/master_weekly_dts_pop.rds")
