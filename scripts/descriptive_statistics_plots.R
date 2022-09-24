library(tidyverse)
library(haven)
library(importinegi)
library(sf)
library(rgeos)

# We load the list of removed treatment units that had less than 7 preperiods
load("data/less_7_preperiods.RData")
`%notin%` <- Negate(`%in%`)

homicide <- read_stata(file = "data/panel_tasa_homicidios.dta")

# Map with the year the reform came into force ----

map_reform <- homicide %>% 
  filter(year == 2011) %>%
  select(MUN,year_entrada)

map_reform <- sig_marcogeo(year = 2013, mapa = "municipios") %>%
  st_as_sf() %>%
  mutate(MUN = paste0(CVE_ENT, CVE_MUN)) %>%
  left_join(map_reform, by = "MUN") %>%
  mutate(year_entrada = ifelse(is.na(year_entrada), "0", year_entrada))

map_ent <- sig_marcogeo(year = 2013, mapa = "entidades") %>%
  st_as_sf()

map_reform$year_entrada <- ordered(map_reform$year_entrada, levels = c(0,2007,2008,2009,2010,2011))
levels(map_reform$year_entrada)[levels(map_reform$year_entrada) == "0"] <- "No reform"

#Gráfico año de entrada----
m <- ggplot() +
  geom_sf(data = map_reform, aes(fill = year_entrada),color = NA) +
  scale_fill_manual(values = c("white", paste0("grey",16:4*5))) +
  geom_sf(data = map_ent, alpha = 0.25, fill = NA, size = 0.15, color = "black") +
  theme_void() +
  theme(plot.margin=grid::unit(c(0,0,0,0), "mm")) +
  labs(fill = "Year")

ggsave(plot =  m,
       filename = "plots/figure_1.pdf",
       width = 5,
       height = 3,
       units = "in")

system2(command = "pdfcrop", 
        args    = c("plots/figure_1.pdf", 
                    "plots/figure_1.pdf"))

# Homicide rate plot (National) ----
load(file = "data/tasa_homicidio_nacional.RData")

g0 <- ggplot(tasa_nacional) +
  geom_line(aes(year,tasa_nacional), color = "black") +
  theme_light() +
  ylab("Homicide rate") +
  xlab("Year")

ggsave(plot = g0,
       filename = "plots/figure_2_a.pdf",
       width = 5,
       height = 3,
       units = "in")

# Homicide rate plot (Treatment and control groups) -----
A0 <- homicide %>%
  filter(trat2 == 0) %>%
  group_by(year) %>%
  summarise(Control = mean(tasa_homicidios))

A1 <- homicide %>% filter(trat2 == 1 & MUN %notin% removed_homicide) %>%
  group_by(year) %>%
  summarise(Treatment = mean(tasa_homicidios))

homicide_rate_plot <- left_join(A0,A1, by = "year") %>% 
  pivot_longer(!year, names_to = "tcgroup", values_to = "mean_murder_rate")

rm(A0,A1,homicide)

g1 <- ggplot(homicide_rate_plot) +
  geom_line(aes(year,mean_murder_rate,col = tcgroup)) +
  theme_light() +
  scale_color_manual(values = c("black","darkgray")) +
  ylab("Mean homicide rate") +
  xlab("Year") +
  labs(col = "Group")

ggsave(plot = g1,
       filename = "plots/figure_2_b.pdf",
       width = 5,
       height = 3,
       units = "in")