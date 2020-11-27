rm(list=ls())

library(tidyverse)
library(curl)
library(sf)
library(rnaturalearth)
library(showtext)

#Download shapefile of species distribution from European Environment Agency 
temp <- tempfile()
temp2 <- tempfile()
source <- "https://www.eea.europa.eu/data-and-maps/data/article-17-database-habitats-directive-92-43-eec-2/article-17-2020-spatial-data/article-17-2020-spatial-data/at_download/file"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
unzip(zipfile=temp, exdir=temp2)
shapefile <- st_read(file.path(temp2, "art17_2013_2018_public.gpkg"),
                     layer="Art17_species_distribution_2013_2018_EU") %>% 
  #Filter only data for Bisonus Bonasus, code 2647
  filter(code==2647) 

#Bring in European map
EUmap <- ne_countries(scale=10, type="countries", 
                      country=c("Austria", "Belgium", "Bulgaria", "Croatia",
                                "Cyprus", "Czechia", "Denmark", "Estonia",
                                "Finland", "France", "Germany", "Greece", "Hungary",
                                "Ireland", "Italy", "Latvia", "Lithuania", 
                                "Luxembourg", "Malta", "Netherlands", "Poland",
                                "Portugal", "Romania", "Slovakia", "Slovenia",
                                "Spain", "Sweden"),
                      returnclass="sf") %>% 
  st_transform(3035)

outlines <- ne_countries(scale=10, type="countries", continent="Europe",
                         returnclass="sf")%>% 
  st_transform(3035)

font_add_google("Amatic SC")
showtext_auto()

tiff("Outputs/EuropeanBison.tiff", units="in", width=6.3, height=8, res=500)
ggplot()+
  geom_sf(data=EUmap, aes(geometry=geometry), fill="#80C18F", colour=NA)+
  geom_sf(data=outlines, aes(geometry=geometry), fill=NA, colour="#00400E", size=0.3)+
  geom_sf(data=shapefile, aes(geometry=geom), fill="#CA0018", colour=NA)+
  theme_classic()+
  xlim(2500000,6000000)+
  ylim(1300000,5300000)+
  theme(axis.line=element_blank(), axis.ticks=element_blank(),
        axis.text=element_blank(), 
        plot.title=element_text(face="bold", size=180, family="Amatic SC"),
        plot.subtitle=element_text(family="Amatic SC", size=80),
        plot.caption=element_text(family="Amatic SC", size=70),
        plot.background=element_rect(fill="#FCECC0"),
        panel.background=element_rect(fill="#FCECC0"))+
  labs(title="Hic sunt Bisones",
       subtitle="The (very small) distribution of the European Bison, Europe's biggest mammal, across the EU",
       caption="Data from the European Envinromnent Agency | Created by @VictimOfMaths")

dev.off()

