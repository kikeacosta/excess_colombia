rm(list=ls())
source("Code/00_functions.R")

dts <- 
  read_rds("data_inter/master_weekly_dts_pop.rds")

dts2 <- 
  dts %>% 
  mutate(t = 1:n(),
         w = ifelse(date < "2020-03-15", 1, 0)) %>% 
  rename(dts = deaths, 
         exposure = pop)

bsn <- 
  est_baseline(dts2)

avs <- 
  dts2 %>% 
  filter(year < 2020) %>% 
  group_by(week) %>% 
  summarise(av_dts = mean(dts)) %>% 
  ungroup()

bsn2 <- 
  bsn %>% 
  left_join(avs)

bsn2 %>% 
  ggplot()+
  geom_ribbon(aes(date, ymin = ll, ymax = ul), alpha = 0.2, fill = "blue")+
  geom_point(aes(date, dts), size = 0.5)+
  geom_line(aes(date, dts))+
  geom_line(aes(date, bsn), col = "blue")+
  geom_line(aes(date, av_dts), col = "red")+
  theme_bw()

ggsave("figures/baseline_weekly.png",
       w = 12,
       h = 4)

out <- 
  bsn2 %>% 
  filter(date > "2020-03-01") %>% 
  mutate(exc_k = dts - bsn,
         exc_d = dts - av_dts) %>% 
  summarise(exc_k = sum(exc_k),
            exc_d = sum(exc_d))
