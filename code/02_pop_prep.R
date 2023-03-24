library(tidyverse)
library(readxl)

dt1 <-
  read_xlsx("data_input/DCD-area-proypoblacion-Nac-1950-2019.xlsx",
            skip = 11) %>% 
  select(year = 3,
         area = 4,
         pop = 5) %>% 
  drop_na(pop) %>% 
  filter(area == "Total") %>% 
  select(-area)

dt2 <-
  read_xlsx("data_input/DCD-area-proypoblacion-Nac-2020-2070.xlsx",
            skip = 11) %>% 
  select(year = 3,
         area = 4,
         pop = 5) %>% 
  drop_na(pop) %>% 
  filter(area == "Total") %>% 
  select(-area)

pop <- 
  bind_rows(dt1, dt2)


write_rds(pop, "data_inter/pop.rds")
