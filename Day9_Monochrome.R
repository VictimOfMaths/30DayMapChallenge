rm(list=ls())

library(tidyverse)
library(sf)
library(paletteer)
library(showtext)

#OS contour data downloaded from Digimap

sk28ne <- st_read("C:\\Users\\cm1cra\\Desktop\\Maps\\Sheffield Contours\\terrain-5_3737301\\sk\\SK28NE_line.shp")
sk28se <- st_read("C:\\Users\\cm1cra\\Desktop\\Maps\\Sheffield Contours\\terrain-5_3737301\\sk\\SK28SE_line.shp")
sk29se <- st_read("C:\\Users\\cm1cra\\Desktop\\Maps\\Sheffield Contours\\terrain-5_3737301\\sk\\SK29SE_line.shp")
sk38ne <- st_read("C:\\Users\\cm1cra\\Desktop\\Maps\\Sheffield Contours\\terrain-5_3737301\\sk\\SK38NE_line.shp")
sk38nw <- st_read("C:\\Users\\cm1cra\\Desktop\\Maps\\Sheffield Contours\\terrain-5_3737301\\sk\\SK38NW_line.shp")
sk38se <- st_read("C:\\Users\\cm1cra\\Desktop\\Maps\\Sheffield Contours\\terrain-5_3737301\\sk\\SK38SE_line.shp")
sk38sw <- st_read("C:\\Users\\cm1cra\\Desktop\\Maps\\Sheffield Contours\\terrain-5_3737301\\sk\\SK38Sw_line.shp")
sk39se <- st_read("C:\\Users\\cm1cra\\Desktop\\Maps\\Sheffield Contours\\terrain-5_3737301\\sk\\SK39SE_line.shp")
sk39sw <- st_read("C:\\Users\\cm1cra\\Desktop\\Maps\\Sheffield Contours\\terrain-5_3737301\\sk\\SK39SW_line.shp")
sk48nw <- st_read("C:\\Users\\cm1cra\\Desktop\\Maps\\Sheffield Contours\\terrain-5_3737301\\sk\\SK48NW_line.shp")
sk48sw <- st_read("C:\\Users\\cm1cra\\Desktop\\Maps\\Sheffield Contours\\terrain-5_3737301\\sk\\SK48SW_line.shp")
sk49sw <- st_read("C:\\Users\\cm1cra\\Desktop\\Maps\\Sheffield Contours\\terrain-5_3737301\\sk\\SK49SW_line.shp")

map <- bind_rows(sk28ne, sk28se, sk29se, sk38ne, sk38nw, sk38se, sk38sw, sk39se, sk39sw, sk48nw, sk48sw, sk49sw)

font_add_google("Shadows Into Light")
showtext_auto()

tiff("Outputs/SheffieldContours.tiff", units="in", width=14, height=10, res=500)
ggplot(map)+
  geom_sf(aes(geometry=geometry, colour=PROP_VALUE), show.legend=FALSE)+
  scale_colour_paletteer_c("pals::ocean.gray", direction=-1)+
  theme_void()+
  theme(plot.title=element_text(face="bold", size=rel(20), vjust=-1, 
                                family="Shadows Into Light"),
        plot.caption=element_text(vjust=7, 
        family="Shadows Into Light", size=rel(5)))+
  labs(title="The seven hills of Sheffield",
       caption="Plot by @VictimOfMaths | Contour data: © Crown copyright and database rights 2020 Ordnance Survey (100025252)")
dev.off()
