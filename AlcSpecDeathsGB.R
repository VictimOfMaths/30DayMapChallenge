rm(list=ls())

library(tidyverse)
library(curl)
library(readxl)
library(sf)
library(paletteer)
library(showtext)

temp <- tempfile()
source <- "https://fingertipsws.phe.org.uk/api/all_data/csv/by_profile_id?parent_area_code=E92000001&parent_area_type_id=6&child_area_type_id=102&profile_id=87&category_area_code="
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")

engdata <- read.csv(temp) %>% 
  filter(Indicator.ID=="91380" & Sex=="Persons" & Area.Type=="County & UA (pre 4/19)") %>% 
  select(`Time.period`, Area.Code, Area.Name, Value, Count, Denominator)

#pull out 2015-17 data for Poole, Dorset & Bournemouth as they are missing from 2016-18 data
#due to new LA boundaries
temp <- subset(engdata, Area.Code %in% c("E06000028", "E06000029", "E10000009") & 
                 Time.period=="2015 - 17") %>% 
  mutate(Area.Code=case_when(
    Area.Code %in% c("E06000028", "E06000029") ~ "E06000058",
    TRUE ~ "E06000059"), 
         Area.Name=case_when(
           Area.Code %in% c("E06000028", "E06000029") ~ "Bournemouth, Christchurch and Poole",
           TRUE ~ "Dorset"))

engdata <- engdata %>% 
  filter(Time.period=="2016 - 18") %>% 
  bind_rows(temp) %>% 
  rename(code=Area.Code, LA=Area.Name) %>% 
  group_by(code, LA) %>% 
  summarise(Count=sum(Count), Denominator=sum(Denominator)) %>% 
  ungroup() %>% 
  mutate(alcrate=Count*100000/Denominator) %>% 
  select(LA, alcrate, code)

#Wales alcohol data
temp <- tempfile()
source <- "https://www.healthmapswales.wales.nhs.uk/IAS/data/csv?viewId=155&geoId=108&subsetId=&viewer=CSV"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
waldata <- read.csv(temp)[,c(2,162)]
colnames(waldata) <- c("LA", "alcrate")

#Read in LA codes to match into Welsh data
temp <- tempfile()
source <- "http://geoportal1-ons.opendata.arcgis.com/datasets/a267b55f601a4319a9955b0197e3cb81_0.csv"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
LAcodes <- read.csv(temp)[,c(1,2)]
colnames(LAcodes) <- c("code", "LA")

#rename Vale of Glamorgan to ensure matching
LAcodes$LA <- if_else(LAcodes$LA=="Vale of Glamorgan", "The Vale of Glamorgan", 
                      as.character(LAcodes$LA))
waldata <- merge(waldata, LAcodes, by="LA")

#Merge into English data
data <- bind_rows(engdata, waldata)

#Get Scottish data
temp <- tempfile()
source <- "https://www.nrscotland.gov.uk/files//statistics/alcohol-deaths/2018/alcohol-specific-deaths-18-all-tabs.xlsx"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")

scodata <- as.data.frame(t(read_excel(temp, sheet="5 - Local Authority", 
                                      range="C85:AH85", col_names=FALSE)))

#Get LA codes
scodata <- LAcodes %>% 
  filter(substr(code,1,1)=="S") %>% 
  arrange(LA) %>% 
  mutate(Count=scodata$V1,
         #Fix some weird misalignment with shapefile
         code=case_when(
           code=="S12000015" ~ "S12000047",
           code=="S12000024" ~ "S12000048",
           code=="S12000046" ~ "S12000049",
           code=="S12000044" ~ "S12000050",
           TRUE ~ as.character(code)))

#Bring in populations
temp <- tempfile()
source <- "https://www.nrscotland.gov.uk/files//statistics/population-estimates/mid-18/mid-year-pop-est-18-tabs.xlsx"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")

scotpop <- read_excel(temp, sheet="Table 3", range="A7:C38", col_names=FALSE)
colnames(scotpop) <- c("code", "LA", "Population")

scodata <- merge(scodata, scotpop, by="LA", all=TRUE) %>% 
  select(LA, code.x, Population, Count) %>% 
  rename(code=code.x) %>% 
  mutate(alcrate=Count*100000/Population) %>% 
  select(LA, code, alcrate)

data <- bind_rows(data, scodata)

#Read in shapefile
temp <- tempfile()
temp2 <- tempfile()
source <- "https://opendata.arcgis.com/datasets/43b324dc1da74f418261378a9a73227f_0.zip?outSR=%7B%22latestWkid%22%3A27700%2C%22wkid%22%3A27700%7D"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
unzip(zipfile=temp, exdir=temp2)

#The actual shapefile has a different name each time you download it, so need to fish the name out of the unzipped file
name <- list.files(temp2, pattern=".shp")
shapefile <- st_read(file.path(temp2, name)) %>% 
  rename(code=ctyua19cd) %>% 
  #Remove Northern Ireland
  filter(substr(code, 1, 1)!="N")

map.data <- full_join(shapefile, data, by="code")

font_add_google("Open Sans")
showtext_auto()

tiff("Outputs/AlcSpecDeathsGB.tiff", units="in", width=9, height=11, res=500)
ggplot(map.data)+
  geom_sf(aes(geometry=geometry, fill=alcrate), colour=NA)+
  scale_fill_paletteer_c("pals::kovesi.linear_blue_5_95_c73", #direction=-1,
                         name="Alcohol-specific\ndeaths per 100,000",
                         na.value="transparent")+
  theme_classic()+
  theme(axis.line=element_blank(), axis.ticks=element_blank(), 
        axis.text=element_blank(), axis.title=element_blank(),
        plot.background=element_rect(fill="#252525"),
        panel.background=element_rect(fill="#252525"),
        legend.background=element_rect(fill="#252525"),
        legend.text=element_text(colour="#c6dbef", family="Roboto", size=45),
        legend.title=element_text(colour="#c6dbef", family="Roboto", size=60, 
                                  lineheight=0.2,vjust=-6),
        legend.position=c(0.17,0.33),
        plot.title=element_text(colour="#c6dbef", face="bold", size=120, 
                                family="Roboto", vjust=-20),
        plot.subtitle=element_text(colour="#c6dbef",family="Roboto", size=50,
                                   lineheight=0.2, vjust=-53),
        plot.caption=element_text(colour="#c6dbef",family="Roboto", size=50,
        lineheight=0.2, vjust=10))+
  guides(fill = guide_colourbar(ticks = FALSE))+
  labs(title="Dying for a drink",
       subtitle="Rates of deaths from causes attributable\nentirely to alcohol across Great Britain",
       caption="Data from Public Health England, NHS Wales & National Records of Scotland\nPlot by @VictimOfMaths")
  dev.off()
  