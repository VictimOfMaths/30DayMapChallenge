rm(list=ls())

library(tidyverse)
library(curl)
library(lubridate)
library(sf)
library(rmapshaper)
library(paletteer)
library(showtext)

#Read in admissions data created by
#https://github.com/VictimOfMaths/COVID_LA_Plots/blob/master/UnderlyingCode.R
data <- read_csv("COVID_LA_Plots/LACases.csv", col_types="icccDidddddiidddddd") %>% 
  select(date, admroll_avg, code) %>% 
  filter(!is.na(admroll_avg)) %>%
  #Set up non-date scale for number of days since first date in data
  group_by(code) %>% 
  mutate(days=as.numeric(date-min(date))) %>% 
  ungroup()

#Bring in shapefile of Local Authorities
temp <- tempfile()
temp2 <- tempfile()
source <- "https://opendata.arcgis.com/datasets/1d78d47c87df4212b79fe2323aae8e08_0.zip?outSR=%7B%22latestWkid%22%3A27700%2C%22wkid%22%3A27700%7D"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
unzip(zipfile=temp, exdir=temp2)

#The actual shapefile has a different name each time you download it, so need to fish the name out of the unzipped file
name <- list.files(temp2, pattern=".shp")
shapefile <- st_read(file.path(temp2, name)) %>%
  rename(code=lad19cd) %>% 
  #Keep only England
  filter(substr(code, 1, 1)=="E") %>% 
#Sort out Bucks
  mutate(code=case_when(code %in% c("E07000004", "E07000005", "E07000006", "E07000007") ~ "E06000060",
         TRUE ~ as.character(code))) %>% 
  group_by(code) %>% 
  summarise(lat=mean(lat), long=mean(long)) %>% 
    ungroup()

sparkdata <- shapefile %>% 
  as.data.frame() %>% 
  select(code, lat, long) %>% 
  merge(data) %>% 
  filter(!is.na(code))

#Extract parameters for sizing sparklines
xrange <- max(shapefile$lat, na.rm=TRUE)-min(shapefile$lat, na.rm=TRUE)
yrange <- max(shapefile$long, na.rm=TRUE)-min(shapefile$long, na.rm=TRUE)
daymax <- max(sparkdata$days, na.rm=TRUE)
admmax <- max(sparkdata$admroll_avg, na.rm=TRUE)

#Convert COVID data to lat/long values
sparkdata <- sparkdata %>% 
  mutate(plot.x=days*(xrange/(40*daymax))+long, 
         plot.y=admroll_avg*(yrange/(10*admmax))+lat) %>% 
  group_by(code) %>% 
  mutate(max.days=max(days), curr.adm=if_else(days==max.days, admroll_avg, 0),
         curr.adm=max(curr.adm)) %>% 
  ungroup()

#Convert shapefile to outline of England only in lat/long
outline <- shapefile %>% 
  summarise() %>% 
  st_transform(crs=4326) %>% 
  ms_simplify(keep=0.1)

font_add_google("Assistant")
showtext_auto()

#Plot
tiff("Outputs/COVIDSparklinesAdm.tiff", units="in", width=7, height=10, res=500)
ggplot()+
  geom_sf(data=outline, aes(geometry=geometry), fill="grey90", colour="grey80")+
  geom_line(data=sparkdata, aes(x=plot.x, y=plot.y, colour=curr.adm, group=code), show.legend=FALSE)+
  scale_colour_paletteer_c("pals::kovesi.linear_ternary_red_0_50_c52")+
  theme_void()+
  theme(plot.title=element_text(margin=margin(t=0, b=-70),face="bold", size=rel(11),
                          family="Assistant", hjust=0.1, vjust=10),
        plot.subtitle=element_text(size=rel(6), vjust=-8, hjust=0.25),
        plot.caption=element_text(size=rel(4), hjust=0.85))+
  labs(title="COVID-19 admission rates across England",
       subtitle="Rolling 7-day average of confirmed new admissions to hospital with a positive test",
       caption="Data from NHS England | Plot by @VictimOfMaths")
dev.off()
