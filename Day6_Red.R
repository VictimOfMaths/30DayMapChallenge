rm(list=ls())

library(tidyverse)
library(paletteer)
library(rnaturalearth)
library(sf)
library(countrycode)

options(scipen=999)

#Read in data sources from GBD results tool
#http://ghdx.healthdata.org/gbd-results-tool

#Precise search terms here:
#http://ghdx.healthdata.org/gbd-results-tool?params=gbd-api-2019-permalink/0b6d64029da442ab7e4feb5885a1ab84

data <- read.csv("data/IHME-GBD_2019_DATA-3ea2e067-1.csv") %>% 
  filter(measure=="Deaths" & metric=="Number") %>% 
  mutate(ISO_A3=countrycode(location, origin="country.name", destination="iso3c")) %>% 
  select(location, val, year, ISO_A3) %>% 
  spread(year, val) %>% 
  mutate(change=`2019`-`2009`) %>% 
  filter(!ISO_A3 %in% c("AFG", "IRN", "IRQ", "JOR", "KWT", "LBN", "TUR",
                        "OMN", "PSE", "QAT", "SAU", "SYR", "ARE", "YEM"))

#Extract Somalia to replace Somaliland and Morocco to replace Sahrawi
#(this is a pragmatic choice based on the GBD countries, I have no opinions
#that are relevant to the politics of these states!)
temp <- data %>% 
  filter(location %in% c("Morocco", "Somalia")) %>% 
  mutate(ISO_A3=case_when(
    location=="Morocco" ~ "SAH",
    location=="Somalia" ~ "SOL",
    TRUE ~ ISO_A3
  )) 

data <- bind_rows(data, temp)

#Get map  
map <-  ne_download(scale=110, type="countries", category="cultural", returnclass="sf") %>% 
  mutate(ISO_A3=case_when(
    SOV_A3=="SAH" ~ "SAH",
    SOV_A3=="SOL" ~ "SOL",
    TRUE ~ ISO_A3
  ))
  
map.data <- full_join(map, data, by="ISO_A3")

tiff("Outputs/HIVDeaths.tiff", units="in", width=7, height=8, res=500)
map.data %>% 
  filter(!is.na(change)) %>% 
ggplot()+
  geom_sf(aes(geometry=geometry, fill=change), colour=NA)+
  scale_fill_paletteer_c("pals::ocean.balance", direction=-1, 
                         limits=c(min(data$change), abs(min(data$change))),
                         labels=c("-100,000", "-50,000", "0", "+50,000",
                                  "+100,000"), name="")+
  theme_classic()+
  theme(axis.line=element_blank(), axis.ticks=element_blank(), 
        axis.text=element_blank(), axis.title=element_blank(),
        legend.position=c(0.23,0.35), 
        plot.title=element_text(face="bold", colour="#7f0000", size=rel(2)),
        plot.subtitle=element_text(colour="#7f0000"),
        plot.caption=element_text(colour="#7f0000"))+
  guides(fill = guide_colourbar(ticks = FALSE))+
  labs(title="The decline of another pandemic",
       subtitle="Changes in annual deaths from HIV/AIDS 2009-2019",
       caption="Data from Global Burden of Disease Study | Plot by @VictimOfMaths")
dev.off()
