rm(list=ls())

library(tidyverse)
library(curl)
library(sf)
library(showtext)

temp <- tempfile()
temp2 <- tempfile()
temp3 <- tempfile()
source <- "https://www.nationalgrid.com/uk/electricity-transmission/document/81201/download"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
unzip(zipfile=temp, exdir=temp2)
unzip(zipfile=paste0(temp2, "\\OHL.zip"), exdir=temp3)
OHL <- st_read(file.path(temp3, "OHL.shp"))

font_add_google("Metal Mania")
showtext_auto()

tiff("Outputs/NationalGridMap.tiff", units="in", width=8, height=10, res=500)
ggplot()+
  geom_sf(data=OHL, aes(geometry=geometry), colour="yellow", alpha=0.2, size=2)+
  geom_sf(data=OHL, aes(geometry=geometry), colour="yellow", alpha=0.1, size=3.5)+
  geom_sf(data=OHL, aes(geometry=geometry), colour="yellow")+
  theme_void()+
  theme(plot.background=element_rect(fill="black"), 
        panel.background=element_rect(fill="black"),
        plot.title=element_text(face="bold", size=280, family="Metal Mania",
                                colour="Yellow", hjust=0.1, vjust=-0.3),
        plot.subtitle=element_text(family="Metal Mania", colour="Yellow",
                                   size=80, hjust=0.033, vjust=-0.5),
        plot.caption=element_text(family="Metal Mania", colour="Yellow",
                                  size=60, hjust=0.93))+
  labs(title="Transfer of Power",
       subtitle="Every overhead power line in England & Wales",
       caption="Data © National Grid UK Plot by @VictimOfMaths")
dev.off()
