rm(list=ls())

library(tidyverse)
library(stringr)
library(sf)
library(maptools)
library(curl)
library(readxl)
library(spatstat)
library(paletteer)
library(forcats)
library(showtext)
library(arrow)

#Read in pubs data. Unfortunately not open data, so I can't share :(
raw.on <- read_csv_arrow("")

#Read in postcode data
PClookup <- read_csv_arrow("")

#Remove whitespace from postcodes for matching
PClookup$pc <- str_replace_all(PClookup$postcode, " ", "")
raw.on$pc <- str_replace_all(raw.on$OT_Postcode, " ", "")

raw.on <- merge(raw.on, PClookup, by="pc", all.x=TRUE)

#Separate pubs data for name analysis
pubs <- subset(raw.on, OT_SubLicenceDescription=="Pubs")[,c(1,6,20,27,32,33,42,43)]

#Download shapefile of country boundaries
temp <- tempfile()
temp2 <- tempfile()
source <- "http://geoportal1-ons.opendata.arcgis.com/datasets/f2c2211ff185418484566b2b7a5e1300_0.zip?outSR={%22latestWkid%22:27700,%22wkid%22:27700}"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
unzip(zipfile=temp, exdir=temp2)

#The actual shapefile has a different name each time you download it, so need to fish the name out of the unzipped file
name <- list.files(temp2, pattern=".shp")
shapefile <- st_read(file.path(temp2, name))

font_add_google("Assistant")
showtext_auto()

tiff("Outputs/CGAPubs2016.tiff", units="in", width=6, height=10.8, res=500)
ggplot(data=shapefile)+
  geom_sf(aes(geometry=geometry), fill=NA, colour=NA)+
  geom_point(data=subset(raw.on, OT_SubLicenceDescription=="Pubs"), 
             aes(x=oseast1m, y=osnrth1m), shape=".", colour="#FFFA84")+
  theme_classic()+
  theme(panel.background=element_rect(fill="Black"), 
        plot.background=element_rect(fill="Black"),
        axis.line=element_blank(), axis.title=element_blank(), 
        axis.text=element_blank(),
        axis.ticks=element_blank(), 
        plot.title=element_text(margin=margin(t=0, b=-70),face="bold", size=rel(11),
                                family="Assistant", colour="white", vjust=-1.8),
        plot.title.position="plot", 
        plot.subtitle=element_text(vjust=-32.5, size=rel(6), colour="white"),
        plot.caption=element_text(size=rel(4), colour="white",vjust=20))+
  labs(title="Pubs, pubs everywhere",
       subtitle="Every pub in Great Britain in 2016",
       caption="Data from CGA Strategy | Plot by @VictimOfMaths")
dev.off()
