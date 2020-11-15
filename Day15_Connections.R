rm(list=ls())

library(tidyverse)
library(curl)
library(sf)
library(ggmap)
library(osmdata)
library(raster)

#Download Rights of Way data from rowmaps.com
temp <- tempfile()
temp2 <- tempfile()
source <- "http://www.rowmaps.com/datasets/SP/PROW.zip"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
unzip(zipfile=temp, exdir=temp2)

#The actual shapefile has a different name each time you download it, so need to fish the name out of the unzipped file
name <- list.files(temp2, pattern=".shp")[1]
shapefile <- st_read(file.path(temp2, name))

#Get Local Authority boundaries
temp <- tempfile()
temp2 <- tempfile()
source <- "https://opendata.arcgis.com/datasets/1d78d47c87df4212b79fe2323aae8e08_0.zip?outSR=%7B%22latestWkid%22%3A27700%2C%22wkid%22%3A27700%7D"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
unzip(zipfile=temp, exdir=temp2)

#The actual shapefile has a different name each time you download it, so need to fish the name out of the unzipped file
name <- list.files(temp2, pattern=".shp")
boundaries <- st_read(file.path(temp2, name)) %>% 
  filter(lad19nm=="Sheffield")

#Convert projection to align with stamen map
boundaries.3857 <- st_transform(boundaries, 3857)
shapefile.3857 <- st_transform(shapefile, 3857)

#get stamen map
stamen.map <- get_stamenmap(getbb("Sheffield"), maptype="terrain", zoom=12)

tiff("Outputs/SheffieldROW.tiff", units="in", width=12, height=8, res=300)
ggmap(stamen.map)+
  geom_sf(data=boundaries.3857, aes(geometry=geometry), colour="Black", 
          fill="transparent", inherit.aes=FALSE)+
  geom_sf(data=shapefile.3857, aes(geometry=geometry), colour="Red",
          inherit.aes=FALSE)+
  theme_classic()+
  theme(axis.line=element_blank(), axis.ticks=element_blank(), axis.text=element_blank(),
        axis.title=element_blank(),  plot.title=element_text(face="bold", size=rel(2)))+
  labs(title="The paths less trodden",
       subtitle="Every public right of way in Sheffield",
       caption="ROW data from Sheffield Council via rowmaps.com\nMap tiles by Stamen Design, under CC BY 3.0. Data by OpenStreetMap, under ODbL\nCreated by @VictimOfMaths")
dev.off()
