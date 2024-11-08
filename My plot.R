
library(sf)
library(ggplot2)
library(tmap)
library(tidyverse)
library(ggrepel)
library(cowplot)
shape_data <- st_read("C:/Users/PROXIMITY REPORT/Downloads/Compressed/Geo FIles/District_272.shp")
plot(st_geometry(shape_data))
print(shape_data)


district<-Dissert_New |> 
  group_by(subdist,poverty) |> 
  count() |> 
  ungroup() |> 
  pivot_wider(names_from = poverty, values_from= n) |> 
  mutate(prop_not = round(`Not poor`/(`Not poor` + `Extremely poor` + Poor),3),
         prop_poor = round(Poor/(`Not poor` + `Extremely poor` + Poor),3),
         Prop_ext = round(`Extremely poor`/(`Not poor` + `Extremely poor` + Poor),3),
         prop_not = prop_not*100,
         prop_poor = prop_poor*100,
         Prop_ext =Prop_ext*100) |> 
  inner_join(shape_data, by=c('subdist'='District')) |> 
  mutate(Label=str_remove(Label, " Municipal"))


plot(st_geometry(shape_data))
print(shape_data)


ggplot(data = district) +
  geom_sf(aes( geometry=geometry, fill=prop_not, color=Label)) +
  scale_fill_gradient(low = "lightcoral", high = "darkred") +
  geom_text(aes(geometry = geometry, label = prop_not),
                  stat = "sf_coordinates", size = 2, color = "black")+
  theme_minimal()+
theme(legend.position="none")




Extreme<-ggplot(data = district) +
  geom_sf(aes(geometry = geometry, fill = Prop_ext)) +
  scale_fill_gradient(low = "green", high = "darkred") +  # Red shaded gradient
  geom_text_repel(aes(geometry = geometry, label = Label),
                  stat = "sf_coordinates", size = 1.5, color = "black") +
  geom_text_repel(aes(geometry = geometry, label = Prop_ext),
                  stat = "sf_coordinates", size = 1.5, color = "blue", vjust = -0.1)+
  theme(legend.position = "none",  
        axis.text = element_blank(),  
        axis.title = element_blank(),  
        axis.ticks = element_blank(),  
        panel.grid = element_blank())
  


poor<-ggplot(data = district) +
  geom_sf(aes(geometry = geometry, fill = prop_poor)) +
  scale_fill_gradient(low = "green", high = "darkred") +  # Red shaded gradient
  geom_text_repel(aes(geometry = geometry, label = Label),
                  stat = "sf_coordinates", size = 1.5, color = "black") +
  geom_text_repel(aes(geometry = geometry, label = prop_poor),
                  stat = "sf_coordinates", size = 1.5, color = "blue", vjust = -0.1)+
  labs(fill='Scale')+
  theme(  
        axis.text = element_blank(),  
        axis.title = element_blank(),  
        axis.ticks = element_blank(),  
        panel.grid = element_blank())




not_poor<-ggplot(data = district) +
  geom_sf(aes(geometry = geometry, fill = prop_not)) +
  scale_fill_continuous(limits=c(0,100))+
  scale_fill_gradient(low = "red", high = "green") +  # Red shaded gradient
  geom_text_repel(aes(geometry = geometry, label = Label),
                  stat = "sf_coordinates", size = 1, color = "black") +
  geom_text_repel(aes(geometry = geometry, label = prop_not),
                  stat = "sf_coordinates", size = 1, color = "blue", vjust = -0.1)+
  labs(fill='Scale')+
  theme(  
    axis.text = element_blank(),  
    axis.title = element_blank(),  
    axis.ticks = element_blank(),  
    panel.grid = element_blank())
  




ggsave(filename="poor map.png" ,plot=poor, dpi=600)
ggsave(filename="Extreme poverty map.png" ,plot=Extreme, dpi=600)
ggsave(filename="not poor map.png" ,plot=not_poor, dpi=600)





