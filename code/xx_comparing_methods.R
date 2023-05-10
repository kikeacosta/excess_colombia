rm(list=ls())
source("Code/00_functions.R")
options(scipen=999)

dt <- read_rds("data_inter/master_sex_age.rds") %>% 
  filter(age == "TOT",
         sex == "t",
         cause == "total",
         year <= 2022) %>% 
  select(-age, -sex, -cause)

# different methods for excess mortality estimation ====
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# averages
# ~~~~~~~~

# weekly average
wk <- 
  dt %>% 
  filter(year <= 2019) %>% 
  group_by(week, geo) %>% 
  summarise(bsn = mean(dts)) %>% 
  ungroup()

dt_av <- 
  dt %>% 
  left_join(wk) %>% 
  mutate(method = "promedio_1")

dt_av %>% 
  filter(geo == "Total") %>% 
  ggplot()+
  geom_line(aes(date, dts))+
  geom_line(aes(date, bsn), col = "red")+
  geom_point(aes(date, dts), size = 1)+
  geom_vline(xintercept = ymd("2020-03-15"), linetype = "dashed")+
  theme_bw()

# ggsave("figures/baseline_averages_tot.png",
#        w = 7,
#        h = 3.5)

exc_av <- 
  dt_av %>% 
  filter(year %in% 2020:2022) %>% 
  mutate(exc = dts - bsn) %>% 
  group_by(year, geo) %>% 
  summarise(dts = sum(dts),
            bsn = sum(bsn),
            exc = sum(exc)) %>% 
  ungroup() %>% 
  mutate(psc = dts / bsn,
         method = "promedio_1")
  

# annual average
# ~~~~~~~~~~~~~~
yr <- 
  dt %>% 
  filter(year <= 2019) %>% 
  group_by(geo) %>% 
  summarise(bsn = mean(dts)) %>% 
  ungroup()

dt_av2 <- 
  dt %>% 
  left_join(yr) %>% 
  mutate(method = "promedio_2")

exc_av2 <- 
  dt_av2 %>% 
  filter(year %in% 2020:2022) %>% 
  mutate(exc = dts - bsn) %>% 
  group_by(year, geo) %>% 
  summarise(dts = sum(dts),
            bsn = sum(bsn),
            exc = sum(exc)) %>% 
  ungroup() %>% 
  mutate(psc = dts / bsn,
         method = "promedio_2")
  
  
# Serfling model
# ~~~~~~~~~~~~~~
bsn <- 
  dt %>% 
  group_by(geo) %>% 
  do(est_baseline2(db = .data)) %>% 
  ungroup() %>% 
  mutate(method = "gam")

bsn %>% 
  filter(geo == "Total") %>% 
  ggplot()+
  geom_ribbon(aes(date, ymin = ll, ymax = ul), alpha = 0.2, fill = "#1e6091")+
  geom_line(aes(date, dts), alpha = 0.4)+
  geom_point(aes(date, dts), size = 0.5)+
  geom_line(aes(date, bsn), col = "#1e6091")+
  geom_vline(xintercept = ymd("2020-03-15"), linetype = "dashed")+
  # facet_wrap(~cause)+
  theme_bw()

exc_gam <- 
  bsn %>% 
  filter(year %in% 2020:2022) %>% 
  group_by(year, geo) %>% 
  summarise(dts = sum(dts),
            bsn = sum(bsn)) %>% 
  ungroup() %>% 
  mutate(exc = dts - bsn,
         psc = dts / bsn) %>% 
  mutate(method = "gam")


# comparison
# ~~~~~~~~~~

bsn_all <- 
  bind_rows(bsn,
            dt_av,
            dt_av2)


bsn_all %>% 
  filter(geo == "Total") %>% 
  ggplot()+
  geom_ribbon(aes(date, ymin = ll, ymax = ul), alpha = 0.2)+
  geom_line(aes(date, dts), alpha = 0.4)+
  geom_point(aes(date, dts), size = 0.5)+
  geom_line(aes(date, bsn, col = method))+
  geom_vline(xintercept = ymd("2020-03-15"), linetype = "dashed")+
  theme_bw()

ggsave("figures/baselines_tot.png",
       w = 7,
       h = 3.5)


comp <- 
  bind_rows(exc_gam,
            exc_av2)
  
comp %>% 
  ggplot()+
  geom_point(aes(psc, geo, col = method))+
  facet_grid(~year)+
  scale_x_log10(limits = c(0.5, 2.1))+
  geom_vline(xintercept = 1, linetype = "dashed")+
  theme_bw()

ggsave("figures/pscores_compar.png",
       w = 7,
       h = 5)

comp %>% 
  filter(geo != "Total") %>% 
  ggplot()+
  geom_point(aes(exc, geo, col = method))+
  facet_grid(~year)+
  # scale_color_manual(values = c("#1e6091", "#ef476f"))+
  # scale_x_log10(limits = c(0.5, 2.1))+
  geom_vline(xintercept = 0, linetype = "dashed")+
  theme_bw()

comp_rel <- 
  comp %>% 
  select(year, geo, method, exc) %>% 
  spread(method, exc) %>% 
  mutate(diff = (weekly_sp_average) / (gam))

comp_rel %>% 
  ggplot()+
  geom_point(aes(diff, geo))+
  facet_grid(~year)+
  scale_x_log10(limits = c(0.03, 30))+
  geom_vline(xintercept = 1, linetype = "dashed")+
  theme_bw()+
  labs(x = "sesgo", y = "")

ggsave("figures/pscores_compar2.png",
       w = 7,
       h = 5)

comp_tot <- 
  comp %>% 
  group_by(geo, method) %>% 
  summarise(dts = sum(dts),
            bsn = sum(bsn)) %>% 
  ungroup() %>% 
  mutate(exc = dts - bsn,
         psc = dts / bsn)

comp_tot2 <- 
  comp_tot %>% 
  select(geo, method, exc) %>% 
  spread(method, exc) %>% 
  mutate(diff_rel = promedio_2/gam,
         diff_rel = ifelse(diff_rel < 0, Inf, diff_rel))


comp_tot2 %>% 
  ggplot()+
  geom_point(aes(diff_rel, geo))+
  # facet_grid(~year)+
  scale_x_log10(breaks = c(1, 2, 3, 4, 5))+
  geom_vline(xintercept = 1, linetype = "dashed")+
  theme_bw()+
  labs(x = "sesgo", y = "")

ggsave("figures/pscores_compar2.png",
       w = 7,
       h = 5)




bsn_all %>% 
  filter(geo %in% c("Nariño", "Valle del Cauca", "Bogotá"),
         year <= 2022) %>% 
  ggplot()+
  # geom_ribbon(aes(date, ymin = ll, ymax = ul), alpha = 0.2)+
  geom_line(aes(date, dts), alpha = 0.4)+
  geom_point(aes(date, dts), size = 0.5)+
  # geom_line(aes(date, bsn, col = method))+
  geom_vline(xintercept = ymd("2020-03-15"), linetype = "dashed")+
  geom_vline(xintercept = ymd("2017-01-01"), linetype = "dashed")+
  theme_bw()+
  facet_wrap(~geo, ncol = 1, scales = "free_y")

ggsave("figures/dts_casos.png",
       w = 7,
       h = 5)

bsn_all %>% 
  filter(geo == "Valle del Cauca",
         year <= 2022) %>% 
  ggplot()+
  # geom_ribbon(aes(date, ymin = ll, ymax = ul), alpha = 0.2)+
  geom_line(aes(date, dts), alpha = 0.4)+
  geom_point(aes(date, dts), size = 0.5)+
  # geom_line(aes(date, bsn, col = method))+
  geom_vline(xintercept = ymd("2020-03-15"), linetype = "dashed")+
  theme_bw()+
  labs(title = "Valle del Cauca")


# 




bsn_all %>% 
  ggplot()+
  # geom_ribbon(aes(date, ymin = ll, ymax = ul), alpha = 0.2)+
  geom_line(aes(date, dts), alpha = 0.4)+
  geom_point(aes(date, dts), size = 0.5)+
  # geom_line(aes(date, bsn, col = method))+
  geom_vline(xintercept = ymd("2020-03-15"), linetype = "dashed")+
  geom_vline(xintercept = ymd("2017-01-01"), linetype = "dashed")+
  theme_bw()+
  facet_wrap(~geo, ncol = 1, scales = "free_y")


ggsave("figures/baselines_tot.pdf",
       w = 7,
       h = 100,
       limitsize = FALSE)










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

comp2 <- 
  comp %>% 
  group_by(geo, source) %>% 
  summarise(exc = sum(exc)) %>% 
  spread(source, exc) %>% 
  mutate(rel_diff = promedio / gam)

# Comparing excess mortality
# ~~~~~~~~~~~~~~~~~~~~~~~~~~

bsn_yr_tot



