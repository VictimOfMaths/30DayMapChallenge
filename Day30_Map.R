rm(list=ls())

library(rnaturalearth)
library(sf)
library(ggpattern)
library(nngeo)
library(showtext)

font_add_google("Nanum Pen Script")
showtext_auto()

map <- ne_states(country="United States of America", returnclass="sf") %>% 
  mutate(i=1:51) %>% 
  filter(!name %in% c("Alaska", "Hawaii")) %>% 
  st_remove_holes()

tiff("Outputs/UnitedStatesOfKitteh.tiff", units="in", width=9, height=6, res=500)
ggplot(map)+
  geom_sf_pattern(aes(pattern_fill=as.factor(i)), pattern="placeholder", 
                  pattern_type="kitten", show.legend=FALSE)+
  theme_void()+
  theme(plot.title=element_text(face="bold", size=180, family="Nanum Pen Script", hjust=0.1),
        plot.caption=element_text(size=70, family="Nanum Pen Script", hjust=0.8))+
  labs(title="The United States of Kitteh",
       caption="Stupidity inspired by @coolbutuseless | Created by @VictimOfMaths")
dev.off()