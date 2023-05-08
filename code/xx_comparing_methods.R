rm(list=ls())
source("Code/00_functions.R")
options(scipen=999)

dt <- read_rds("data_inter/master_sex_age.rds")

# different methods for excess mortality estimation ====
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# averages
# ~~~~~~~~

# weekly average
wk <- 
  dt %>% 
  filter(year <= 2019) %>% 
  group_by(week, geo, sex, age, cause) %>% 
  summarise(dts_av = mean(dts)) %>% 
  ungroup()

dt2 <- 
  dt %>% 
  left_join(wk)

dt2 %>% 
  filter(geo == "Total",
         age == "TOT",
         cause == "total",
         sex != "t",
         year <= 2022) %>% 
  ggplot()+
  geom_line(aes(date, dts))+
  geom_line(aes(date, dts_av))+
  geom_point(aes(date, dts), size = 1)+
  geom_vline(xintercept = ymd("2020-03-15"), linetype = "dashed")+
  facet_wrap(~sex)+
  theme_bw()

dt2 %>% 
  filter(geo == "Total",
         age == "TOT",
         cause == "total",
         sex == "t",
         year <= 2022) %>% 
  ggplot()+
  geom_line(aes(date, dts), alpha = 0.4)+
  geom_point(aes(date, dts), size = 0.5)+
  geom_line(aes(date, dts_av), col = "#ef476f")+
  geom_vline(xintercept = ymd("2020-03-15"), linetype = "dashed")+
  # facet_wrap(~sex)+
  theme_bw()

# ggsave("figures/baseline_averages_tot.png",
#        w = 7,
#        h = 3.5)


exc_av <- 
  dt2 %>% 
  filter(year %in% 2020:2022) %>% 
  mutate(exc = dts - dts_av) %>% 
  group_by(year, geo, sex, age, cause) %>% 
  summarise(dts = sum(dts),
            bsn = sum(dts_av),
            exc = sum(exc)) %>% 
  ungroup() %>% 
  mutate(psc = dts / bsn)
  
  
exc_av %>% 
  filter(geo == "Total",
         age == "TOT",
         cause == "total",
         sex == "t")


# annual average
# ~~~~~~~~~~~~~~
yr <- 
  dt %>% 
  filter(year <= 2022) %>% 
  group_by(year, geo, sex, age, cause) %>% 
  summarise(dts = sum(dts)) %>% 
  ungroup()

tots <- 
  yr %>% 
  filter(geo == "Total",
         age == "TOT",
         cause == "total",
         sex == "t")

av1519 <- 
  tots %>% 
  filter(year %in% 2015:2019) %>% 
  summarise(dts_av = mean(dts)) %>% 
  pull(dts_av)

exc_yr <- 
  tots %>% 
  mutate(av1519 = av1519,
         exc = dts - av1519,
         psc = dts / av1519)

exc_yr





av1519 <- 
  yr %>% 
  filter(year %in% 2015:2019) %>% 
  group_by(geo, sex, age, cause) %>% 
  summarise(dts_av = mean(dts)) %>% 
  ungroup()
  
exc_yr <- 
  yr %>% 
  filter(year %in% 2020:2022,
         cause == "total") %>% 
  left_join(av1519) %>% 
  mutate(exc = dts - dts_av,
         psc = dts / dts_av)



exc_yr %>% 
  filter(age == "TOT")



# weekly trend
# ~~~~~~~~~~~~





# Serfling model
# ~~~~~~~~~~~~~~
bsn <- 
  dt %>% 
  filter(geo == "Total", 
         cause != "estudio") %>% 
  group_by(geo, sex, age, cause) %>% 
  do(est_baseline2(db = .data)) %>% 
  ungroup()

bsn_tot <- 
  bsn %>% 
  filter(geo == "Total",
         age == "TOT",
         cause == "total",
         sex == "t",
         year <= 2022)

bsn_tot %>% 
  ggplot()+
  geom_ribbon(aes(date, ymin = ll, ymax = ul), alpha = 0.2, fill = "#1e6091")+
  geom_line(aes(date, dts), alpha = 0.4)+
  geom_point(aes(date, dts), size = 0.5)+
  geom_line(aes(date, bsn), col = "#1e6091")+
  geom_vline(xintercept = ymd("2020-03-15"), linetype = "dashed")+
  facet_wrap(~cause)+
  theme_bw()

# ggsave("figures/baseline_serfling_tot.png",
#        w = 7,
#        h = 3.5)

bsn <- read_rds("data_inter/first_try_all.rds")

bsn_yr <- 
  bsn %>% 
  filter(cause == "total",
         year <= 2022) %>% 
  group_by(year, geo, sex, age, cause) %>% 
  summarise(dts = sum(dts),
            bsn = sum(bsn)) %>% 
  ungroup() %>% 
  mutate(exc = dts - bsn,
         psc = dts / bsn)


bsn_yr_tot <- 
  bsn_yr %>% 
  filter(age == "TOT",
         year >= 2020)


# comparison
# ~~~~~~~~~~

comp <- 
  bsn_yr %>% 
  filter(year >= 2020) %>% 
  mutate(source = "gam") %>% 
  bind_rows(exc_yr %>% 
              mutate(source = "promedio") %>% 
              rename(bsn = dts_av))



comp %>% 
  filter(sex == "t",
         age == "TOT") %>% 
  ggplot()+
  geom_point(aes(psc, geo, col = source))+
  facet_grid(~year)+
  scale_x_log10(limits = c(0.5, 2.1))+
  geom_vline(xintercept = 1, linetype = "dashed")+
  scale_color_manual(values = c("#1e6091", "#ef476f"))+
  theme_bw()

ggsave("figures/pscores_compar.png",
       w = 7,
       h = 5)

comp %>% 
  filter(sex == "t",
         age == "TOT",
         geo != "Total") %>% 
  ggplot()+
  geom_point(aes(exc, geo, col = source))+
  facet_grid(~year)+
  scale_color_manual(values = c("#1e6091", "#ef476f"))+
  # scale_x_log10(limits = c(0.5, 2.1))+
  geom_vline(xintercept = 0, linetype = "dashed")+
  theme_bw()

comp_rel <- 
  comp %>% 
  select(year, geo, sex, age, source, psc) %>% 
  spread(source, psc) %>% 
  filter(age == "TOT") %>% 
  mutate(diff = (promedio-1) / (gam-1))

comp_rel %>% 
  filter(sex == "t") %>% 
  ggplot()+
  geom_point(aes(diff, geo))+
  facet_grid(~year)+
  scale_x_log10(limits = c(0.5, 2.1))+
  geom_vline(xintercept = 1, linetype = "dashed")+
  theme_bw()+
  labs(x = "sesgo", y = "")

ggsave("figures/pscores_compar2.png",
       w = 7,
       h = 5)




bsn_all <- 
  bsn_tot %>% 
  left_join(dt2)
  

bsn_all %>% 
  ggplot()+
  geom_ribbon(aes(date, ymin = ll, ymax = ul), alpha = 0.2, fill = "#1e6091")+
  geom_line(aes(date, dts), alpha = 0.4)+
  geom_point(aes(date, dts), size = 0.5)+
  geom_line(aes(date, bsn), col = "#1e6091")+
  geom_line(aes(date, dts_av), col = "#ef476f")+
  geom_vline(xintercept = ymd("2020-03-15"), linetype = "dashed")+
  facet_wrap(~cause)+
  theme_bw()

# ggsave("figures/baseline_compar_tot.png",
#        w = 7,
#        h = 3.5)

unique(bsn_all$sex)
unique(bsn_all$age)
unique(bsn_all$geo)

exc_yr_serf <- 
  bsn_all %>% 
  group_by(year) %>% 
  summarise(dts = sum(dts),
            bsn = sum(bsn)) %>% 
  ungroup() %>% 
  mutate(exc = dts - bsn,
         psc = dts / bsn)



# Comparing excess mortality
# ~~~~~~~~~~~~~~~~~~~~~~~~~~

bsn_yr_tot



