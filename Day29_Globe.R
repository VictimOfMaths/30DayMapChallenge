rm(list=ls())

library(tidyverse)
library(sf)
library(rnaturalearth)
library(ggtext)

#Code heavily borrowed from
#https://stackoverflow.com/questions/43207947/whole-earth-polygon-for-world-map-in-ggplot2-and-sf

crs <- "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +datum=WGS84 +units=m +no_defs"

#List of countries with trade agreements taken from
#https://www.gov.uk/guidance/uk-trade-agreements-with-non-eu-countries

map <- ne_countries(scale = 50, type = "countries", returnclass = "sf") %>%
  select(iso_a3, iso_n3, admin) %>% 
  filter(iso_a3 %in% c("ATG", "COL", "ECU", "PER", "CAN", "BRB", "BLZ", "BHS",
                       "DMA", "DOM", "GRD", "GUY", "JAM", "KNA", "LCA", "VCT",
                       "TTO", "CRI", "SLV", "GTM", "HND", "NIC", "PAN", "CHL",
                       "CIV", "MDG", "MUS", "SYC", "ZWE", "FRO", "GEO", "ISL",
                       "NOR", "ISR", "JOR", "KEN", "XXK", "LBN", "LIE", "MAR",
                       "PAL", "FJI", "PNG", "KOR", "CHE", "TUN", "UKR", "BWA",
                       "SWZ", "LSO", "NAM", "ZAF", "MOZ", "JPN", "GBR")) %>% 
  mutate(flag=case_when(
    iso_a3=="GBR" ~ 1,
    iso_a3 %in% c("CAN", "KEN") ~ 2,
                  TRUE ~ 3))

sphere <- st_graticule(ndiscr = 10000, margin = 10e-6) %>%
  st_transform(crs = crs) %>%
  st_convex_hull() %>%
  summarise(geometry = st_union(geometry))

tiff("Outputs/GlobalBritain.tiff", units="in", width=6, height=7, res=500)
ggplot()+
  geom_sf(data = sphere, fill = "deepskyblue", alpha = 0.7) +
  geom_sf(data = map, colour="Grey20", aes(fill=as.factor(flag)), show.legend=FALSE, size=0.2) +
  scale_fill_manual(values=c("Tomato", "yellowgreen", "forestgreen"))+
  theme_bw()+
  theme(plot.subtitle=element_markdown(), plot.title=element_text(face="bold", size=rel(2)))+
  labs(title="Global Britain",
       subtitle="Countries which have signed <span style='color:forestgreen;'>trade agreements</span> or <span style='color:yellowgreen;'>agreements in principle </span> with the UK<br>for January 2021 onwards",
       caption="Data from www.gov.uk | Plot by @VictimOfMaths")
dev.off()
