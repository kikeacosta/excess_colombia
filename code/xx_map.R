rm(list=ls())
source("Code/00_functions.R")
library(sf)
library(rgeos)
library(geodata)

geo_col <- 
  gadm("COL", level=1, path = "data_input/", version="latest", resolution=1) %>%
  st_as_sf() %>% 
  mutate(NAME_1 = ifelse(NAME_1 == "Bogotá D.C.", "Bogotá", NAME_1))

geo_col %>% 
  ggplot() +
  geom_sf()+
  coord_sf(xlim = c(-80, -66),
           ylim = c(-5, 13),
           expand = 0)+
  theme_bw()

unique(geo_col$NAME_1)

psc <- read_rds("data_output/pscores_annual.rds")

psc_20 <- 
  psc %>% 
  filter(year == 2020,
         sex == "t",
         geo != "total") %>% 
  mutate(psc1 = cut(psc_pos, breaks = seq(0, 2, 0.1)),
         psc2 = cut(psc_st_pos, breaks = seq(0, 2, 0.1))) %>% 
  select(NAME_1 = geo, psc_pos, psc_st_pos, psc1, psc2)

unique(psc_20$NAME_1)

cols_map <- 
  magma(length(unique(psc_20$psc1)), alpha = 1, begin = 0, end = 1, direction = 1) %>% 
  rev()

col_psc <- 
  geo_col %>% 
  left_join(psc_20)

col_psc %>% 
  ggplot() +
  geom_sf(aes(fill = psc1))+
  # scale_fill_viridis_b(breaks = seq(0, 1.5, 0.25))+
  scale_fill_manual(values = cols_map)+
  coord_sf(xlim = c(-80, -66),
           ylim = c(-5, 13),
           expand = 0)+
  labs(title = "p-scores")+
  theme_bw()

ggsave("figures/map_pscores.pdf",
       w = 10,
       h = 10)

ggsave("figures/map_pscores.png",
       w = 10,
       h = 10)

col_psc %>% 
  ggplot() +
  geom_sf(aes(fill = psc2))+
  # scale_fill_viridis_b(breaks = seq(0, 1.5, 0.25))+
  scale_fill_manual(values = cols_map)+
  coord_sf(xlim = c(-80, -66),
           ylim = c(-5, 13),
           expand = 0)+
  labs(title = "standardized p-scores")+
  theme_bw()

ggsave("figures/map_pscores_st.pdf",
       w = 10,
       h = 10)

ggsave("figures/map_pscores_st.png",
       w = 10,
       h = 10)
