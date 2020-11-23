rm(list=ls())

library(tidyverse)
library(sf)
library(curl)

#LSOA E01033266
temp <- tempfile()
temp2 <- tempfile()
source <- "https://opendata.arcgis.com/datasets/fd7e9e6e82584a54b06aae40b8ca6988_0.zip?outSR=%7B%22latestWkid%22%3A27700%2C%22wkid%22%3A27700%7D"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
unzip(zipfile=temp, exdir=temp2)

#The actual shapefile has a different name each time you download it, so need to fish the name out of the unzipped file
name <- list.files(temp2, pattern=".shp")
LSOA <- st_read(file.path(temp2, name)) %>% 
  filter(LSOA11CD=="E01033266")

#MSOA E02006844
temp <- tempfile()
temp2 <- tempfile()
source <- "https://opendata.arcgis.com/datasets/efeadef72f3745df86edc1c146006fc0_0.zip?outSR=%7B%22latestWkid%22%3A27700%2C%22wkid%22%3A27700%7D"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
unzip(zipfile=temp, exdir=temp2)

#The actual shapefile has a different name each time you download it, so need to fish the name out of the unzipped file
name <- list.files(temp2, pattern=".shp")
MSOA <- st_read(file.path(temp2, name)) %>% 
  filter(MSOA11CD=="E02006844")

#LA E08000019
temp <- tempfile()
temp2 <- tempfile()
source <- "https://opendata.arcgis.com/datasets/7f83b82ef6ce46d3a5635d371e8a3e7c_0.zip?outSR=%7B%22latestWkid%22%3A27700%2C%22wkid%22%3A27700%7D"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
unzip(zipfile=temp, exdir=temp2)

#The actual shapefile has a different name each time you download it, so need to fish the name out of the unzipped file
name <- list.files(temp2, pattern=".shp")
LAD <- st_read(file.path(temp2, name)) %>% 
  filter(lad20cd=="E08000019")

#Constituency E14000919
temp <- tempfile()
temp2 <- tempfile()
source <- "https://opendata.arcgis.com/datasets/b64677a2afc3466f80d3d683b71c3468_0.zip?outSR=%7B%22latestWkid%22%3A27700%2C%22wkid%22%3A27700%7D"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
unzip(zipfile=temp, exdir=temp2)

#The actual shapefile has a different name each time you download it, so need to fish the name out of the unzipped file
name <- list.files(temp2, pattern=".shp")
constituency <- st_read(file.path(temp2, name)) %>% 
  filter(pcon18cd=="E14000919")

#OA E00172499
temp <- tempfile()
temp2 <- tempfile()
source <- "https://opendata.arcgis.com/datasets/09b58d063d4e421a9cad16ba5419a6bd_0.zip?outSR=%7B%22latestWkid%22%3A27700%2C%22wkid%22%3A27700%7D"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
unzip(zipfile=temp, exdir=temp2)

#The actual shapefile has a different name each time you download it, so need to fish the name out of the unzipped file
name <- list.files(temp2, pattern=".shp")
OA <- st_read(file.path(temp2, name)) %>% 
  filter(OA11CD=="E00172499")

#Built-up areas E35000930
temp <- tempfile()
temp2 <- tempfile()
source <- "https://opendata.arcgis.com/datasets/30858d02474b4f5a85916acac0f45168_0.zip?outSR=%7B%22latestWkid%22%3A27700%2C%22wkid%22%3A27700%7D"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
unzip(zipfile=temp, exdir=temp2)

#The actual shapefile has a different name each time you download it, so need to fish the name out of the unzipped file
name <- list.files(temp2, pattern=".shp")
BUA <- st_read(file.path(temp2, name)) %>% 
  filter(BUASD11CD=="E35000930")

#Parishes E43000173
temp <- tempfile()
temp2 <- tempfile()
source <- "https://opendata.arcgis.com/datasets/71f843bdb5144752be922b7a4c82ab5e_0.zip?outSR=%7B%22latestWkid%22%3A27700%2C%22wkid%22%3A27700%7D"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
unzip(zipfile=temp, exdir=temp2)

#The actual shapefile has a different name each time you download it, so need to fish the name out of the unzipped file
name <- list.files(temp2, pattern=".shp")
parish <- st_read(file.path(temp2, name)) %>% 
  filter(parncp18cd=="E43000173")
 
#Travel to work areas E30000261
temp <- tempfile()
temp2 <- tempfile()
source <- "https://opendata.arcgis.com/datasets/60637d04599242c988898dc154048e52_0.zip?outSR=%7B%22latestWkid%22%3A27700%2C%22wkid%22%3A27700%7D"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
unzip(zipfile=temp, exdir=temp2)

#The actual shapefile has a different name each time you download it, so need to fish the name out of the unzipped file
name <- list.files(temp2, pattern=".shp")
TTWA <- st_read(file.path(temp2, name)) %>% 
  filter(TTWA11CD=="E30000261")

#Plot them all
tiff("Outputs/Boundaries.tiff", units="in", width=12, height=8, res=500)
ggplot()+
  geom_point(aes(x=434580, y=387300), colour="red")+
  geom_sf(data=TTWA, aes(geometry=geometry), fill=NA, colour="#CC79A7")+
  geom_sf(data=LAD, aes(geometry=geometry), fill=NA, colour="#D55E00")+
  geom_sf(data=parish, aes(geometry=geometry), fill=NA, colour="#0072B2")+
  geom_sf(data=BUA, aes(geometry=geometry), fill=NA, colour="#F0E442")+
  geom_sf(data=constituency, aes(geometry=geometry), fill=NA, colour="#009E73")+
  geom_sf(data=MSOA, aes(geometry=geometry), fill=NA, colour="#56B4E9")+
  geom_sf(data=LSOA, aes(geometry=geometry), fill=NA, colour="#E69F00")+
  geom_sf(data=OA, aes(geometry=geometry), fill=NA, colour="#000000")+
  annotate("text", x=457000, y=394800, colour="#CC79A7", label="Travel to work area")+
  annotate("text", x=420000, y=398000, colour="#D55E00", label="Local Authority")+
  annotate("text", x=427000, y=388300, colour="#0072B2", label="Parish")+
  annotate("text", x=444000, y=388200, colour="#F0E442", label="Built-up Area")+
  annotate("text", x=437000, y=389000, colour="#009E73", label="Constituency")+
  annotate("text", x=434600, y=386000, colour="#56B4E9", label="MSOA")+
  annotate("text", x=434500, y=387900, colour="#E69F00", label="LSOA")+
  theme_void()+
  theme(plot.title=element_text(face="bold", size=rel(2), colour="white", hjust=0.01),
        plot.subtitle=element_text(colour="white", hjust=0.01),
        plot.caption=element_text(colour="white", hjust=0.99),
        plot.background=element_rect(fill="Grey20"))+
  labs(title="The boundaries of Sheffield",
       subtitle="Geographical units around my (normal) office",
       caption="Data from ONS | Map by @VictimOfMaths")
dev.off()

