rm(list=ls())

library(tidyverse)
library(sf)
library(ggtext)
library(showtext)

data <- st_read("Data/lccm-2019_3762222/lccm-2019_3762222.gpkg")

font_add_google("Smythe")
showtext_auto()

tiff("Outputs/NorfolkFields.tiff", units="in", width=10, height=8, res=500)
ggplot(data)+
  geom_sf(aes(fill=crop_name), colour=NA, show.legend=FALSE)+
  scale_fill_manual(values=c("#f74740", "#5dbcd2", "#76bc6b", "#5dbcd2", "#3a617e", "#5dbcd2", "#5dbcd2", "#5dbcd2", 
                             "#fded39", "#fded39", "#fded39", "#fded39", "#fded39"))+
  theme_classic()+
  theme(axis.line=element_blank(), axis.ticks=element_blank(), axis.text=element_blank(),
        axis.title=element_blank(), plot.title=element_text(face="bold", size=rel(14), family="Smythe"),
        plot.subtitle=element_markdown(family="Smythe", size=rel(8)), 
        plot.caption=element_text(family="Smythe", size=rel(5)))+
  labs(title="The fields of Norfolk",
       subtitle="Crop land use data for <span style='color:#f74740;'>Beet</span>, <span style='color:#76bc6b;'>grass</span>, <span style='color:#3a617e;'>rape</span>, <span style='color:#fded39;'>cereals</span> and <span style='color:#5dbcd2;'>other crops</span> in 2019",
       caption="Data from Centre for Ecology & Hydrology | Plot by @VictimOfMaths")
dev.off()
