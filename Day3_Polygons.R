rm(list=ls())

library(tidyverse)
library(paletteer)
library(sf)
library(showtext)

#Read in ground coordinates pulled from wikidata through this query:
#https://query.wikidata.org/#SELECT%20%3Fclub%20%3FclubLabel%20%3Fvenue%20%3FvenueLabel%20%3Fcoordinates%0AWHERE%0A%7B%0A%09%3Fclub%20wdt%3AP31%20wd%3AQ476028%20.%0A%09%3Fclub%20wdt%3AP115%20%3Fvenue%20.%0A%09%3Fvenue%20wdt%3AP625%20%3Fcoordinates%20.%0A%09SERVICE%20wikibase%3Alabel%20%7B%20bd%3AserviceParam%20wikibase%3Alanguage%20%22en%22%20%7D%0A%7D

coords <- read.csv("data/GroundCoordinates.csv") %>% 
  mutate(coordinates=gsub("Point\\(", "", coordinates),
         coordinates=gsub("\\)", "", coordinates)) %>% 
  separate(coordinates, into=c("lat", "long"), sep=" ") %>% 
  mutate(lat=as.numeric(lat), long=as.numeric(long))

#Filter out Permier League teams (filter on ground name as club names are duplicated)
data <- coords %>% 
  select(clubLabel, venueLabel, lat, long) %>% 
  filter(venueLabel %in% c("Anfield", "King Power Stadium", "Tottenham Hotspur Stadium",
                           "Goodison Park", "St Mary's Stadium", "Molineux Stadium",
                           "Stamford Bridge stadium", "Villa Park", "Emirates Stadium",
                           "Etihad Stadium", "St Jamesâ€™ Park", "Elland Road",
                           "Selhurst Park", "London Stadium", "Old Trafford",
                           "American Express Community Stadium", "Craven Cottage", 
                           "The Hawthorns", "Bramall Lane", "Turf Moor")) %>% 
  filter(clubLabel %in% c("Liverpool F.C.", "Leicester City F.C.", "Tottenham Hotspur F.C.",
                          "Everton F.C.", "Southampton F.C.", "Wolverhampton Wanderers F.C.",
                          "Chelsea F.C.", "Aston Villa F.C.", "Arsenal F.C.",
                          "Manchester City F.C.", "Newcastle United F.C.", "Leeds United F.C.",
                          "Crystal Palace F.C.", "West Ham United F.C.", "Manchester United F.C.",
                          "Brighton & Hove Albion F.C.", "Fulham F.C.", 
                          "West Bromwich Albion F.C.", "Sheffield United F.C.",
                          "Burnley F.C."
                          )) %>% 
  st_as_sf(coords=c("lat", "long"), crs=4326) %>% 
  st_transform(crs=27700)

#Read in shapefile
#Bring in shapefile of Local Authorities
temp <- tempfile()
temp2 <- tempfile()
source <- "https://opendata.arcgis.com/datasets/4fcca2a47fed4bfaa1793015a18537ac_4.zip?outSR=%7B%22latestWkid%22%3A27700%2C%22wkid%22%3A27700%7D"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
unzip(zipfile=temp, exdir=temp2)

#The actual shapefile has a different name each time you download it, so need to fish the name out of the unzipped file
name <- list.files(temp2, pattern=".shp")
outline <- st_read(file.path(temp2, name)) %>% 
  summarise()

#Generate voronoi polygons
voronoi <- data %>% 
  st_union() %>%
  st_voronoi() %>%
  st_collection_extract()

# Put them back in their original order
voronoi <- voronoi[unlist(st_intersects(data,voronoi))]

voronoi <- st_intersection(st_cast(voronoi), outline, col=0)

voronoi <- data %>% 
  st_combine() %>% 
  st_voronoi() %>% 
  st_cast() %>% 
  st_intersection(outline) %>%
  st_cast() %>% 
  st_sf()

font_add_google("Roboto")
showtext_auto()

tiff("Outputs/PremierLeagueVoronoi.tiff", units="in", width=8, height=10, res=500)
ggplot()+
  geom_sf(data=outline, aes(geometry=geometry), fill="White", colour=NA)+
  geom_sf(data=voronoi, aes(geometry=geometry, fill=rownames(voronoi)), colour="Black",
          size=0.2, show.legend=FALSE)+
  geom_sf(data=data, aes(geometry=geometry), shape=21, colour="#38003c", fill="#00ff85")+
  scale_fill_manual(values=c("#003399", "white", "#122F67", "#670E36", "#003090",
                             "white", "#EF0107", "white", "#0057B8", "#1B458F",
                             "#034694", "#DA291C", "#7A263A", "#FDB913", "#C8102E",
                             "#D71920", "#6C1D45", "#EE2737", "#241F20", "#6CABDD"))+
  theme_classic()+
  theme(axis.line=element_blank(), axis.ticks=element_blank(), axis.text=element_blank(),
        axis.title=element_blank(), 
        plot.title=element_text(margin=margin(t=0, b=-70), face="bold", size=rel(11), 
                                family="Roboto", 
                                hjust=0,  vjust=9),
        plot.subtitle=element_text(size=rel(6), vjust=-12, hjust=0),
        plot.caption=element_text(size=rel(4), hjust=0.85))+
  labs(title="Support your local (Premier League) team",
       subtitle="The nearest current Premier League club to every point in England",
       caption="Ground data from Wikidata | Plot by @VictimOfMaths")
dev.off()
