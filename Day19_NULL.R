rm(list=ls())

library(tidyverse)
library(stringr)
library(sf)
library(curl)
library(readxl)
library(paletteer)
library(showtext)
library(arrow)

#Read in off-trade data  Unfortunately not open data, so I can't share :(
raw.off <- read_csv_arrow("X:/ScHARR/SARG_IARP/General/Data/Market research data (Nielson and CGA)/CGA 2016/OUTPUT FINAL 2015 OffData.txt")
#Read in pubs data  Unfortunately not open data, so I can't share :(
raw.on <- read_csv_arrow("X:/ScHARR/SARG_IARP/General/Data/Market research data (Nielson and CGA)/CGA 2016/OUTPUT FINAL 2015 On Data.txt")

#Read in postcode data
PClookup <- read_csv_arrow("X:/ScHARR/SARG_IARP/General/Data/Market research data (Nielson and CGA)/DENSITY/out_lookup_residential_pc.txt")

#Remove whitespace from postcodes for matching
PClookup$pc <- str_replace_all(PClookup$postcode, " ", "")
raw.off$pc <- str_replace_all(raw.off$OT_Postcode, " ", "")
raw.on$pc <- str_replace_all(raw.on$OT_Postcode, " ", "")

raw.off <- merge(raw.off, PClookup, by="pc", all.x=TRUE) %>% 
  select(lsoa11)
raw.on <- merge(raw.on, PClookup, by="pc", all.x=TRUE) %>% 
  select(lsoa11)

#Collapse to LSOA level data
data <- bind_rows(raw.off, raw.on) %>% 
  group_by(lsoa11) %>% 
  summarise(outlets=n())

#Download LSOA shapefile
temp <- tempfile()
temp2 <- tempfile()
source <- "https://opendata.arcgis.com/datasets/fd7e9e6e82584a54b06aae40b8ca6988_0.zip?outSR=%7B%22latestWkid%22%3A27700%2C%22wkid%22%3A27700%7D"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
unzip(zipfile=temp, exdir=temp2)

#The actual shapefile has a different name each time you download it, so need to fish the name out of the unzipped file
name <- list.files(temp2, pattern=".shp")
shapefile <- st_read(file.path(temp2, name)) %>% 
  rename(lsoa11=LSOA11CD)

map.data <- full_join(shapefile, data)

font_add_google("Josefin Sans")
showtext_auto()

tiff("Outputs/AlcoholVoids.tiff", units="in", width=10, height=8, res=500)
map.data %>% 
  filter(is.na(outlets)) %>% 
  ggplot()+
  geom_sf(aes(geometry=geometry), colour=NA, fill="#2c7fb8")+
  theme_classic()+
  theme(axis.line=element_blank(), axis.ticks=element_blank(), axis.text=element_blank(),
        axis.title=element_blank(), plot.title=element_text(face="bold", size=rel(14), 
                                                            family="Josefin Sans", vjust=-0.4),
        plot.subtitle=element_text(family="Josefin Sans", size=rel(6), lineheight=0.2, vjust=-7), 
        plot.caption=element_text(family="Josefin Sans", size=rel(3.5), vjust=15))+
  labs(title="The alcohol voids",
       subtitle="Lower Super Output Areas in England & Wales\nwith nowhere you can buy alcohol",
       caption="Data from CGA Strategy | Plot by @VictimOfMaths")
  dev.off()

