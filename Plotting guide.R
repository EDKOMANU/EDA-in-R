library(tidyverse)
library(gtsummary)
library(flextable)
library(gmodels)
library(readxl)
library(officer)
library(ggplot2)
library(ggtext)

  

#--------plotting--------
a=data.frame(table(malaria$Sex, malaria$TM01))

a<-a %>% 
  rename(Sex=Var1,
         Treated_Mal=Var2) %>% 
  mutate(prop)

a1<-ggplot(a,mapping = aes(x = Treated_Mal, y = Freq, fill=Sex ))+
  geom_col(width = 0.7, position = "dodge") + 
  scale_fill_manual(values=c("#CC0000","#66c2a5")) + 
  geom_text(mapping = aes(label =scales::comma(Freq)), 
            size=6, hjust=0.3, vjust=1,col= "white", position = position_dodge(width =0.7))+
  labs(x = 'Received Treatement for Malaria',
       y = "Population",
       fill = "") +
  # Set the coordinate system for the plot, allowing data points to be partially displayed outside of the plot area.
  
  theme(panel.grid.major.x=element_line(color = "gray", size=0.25),
        panel.grid.major.y=element_blank(),
        legend.position = ("top"),
        axis.text.x = element_text(color = "black",size = 9, face = "bold"),
        axis.text.y = element_text(color = "black", size = 9, face = "bold"),
        panel.background = element_rect(fill = NA, color = NA)) 
  ggsave(path.expand("C:/Users/PROXIMITY REPORT/Desktop/MA POP Learning materials/lecture notes/622/plot.png"), a1, width = 6, height = 4)

data1<-malaria %>% 
  select(Sex, Age)
ggplot(data,aes(x=Sex,y=Age, fill= Sex))+
  geom_boxplot()+ stat_summary(fun =median,geom = "text", col='white', aes(label = round(..y.., 2)), vjust=1)+
  scale_fill_manual(values=c("#CC0000","#66c2a5"))+ 
  labs(title='BoxPlot of Age by  sex',
       x= 'Sex',
       y= 'Age',
       fill="")+
  theme(legend.position = 'top',
        axis.text.x = element_text(size=9, face='bold'),
        axis.text.y=element_text(size=9, face ='bold'))
ggsave(path.expand("C:/Users/PROXIMITY REPORT/Desktop/MA POP Learning materials/lecture notes/622/box.png"), box, width = 6, height = 4)
doc<-doc %>% 
  body_add_break(pos='after') %>% 
  body_add_img("C:/Users/PROXIMITY REPORT/Desktop/MA POP Learning materials/lecture notes/622/box.png", width=6, height=5,  pos= 'after') %>% 
  print(target= "C:/Users/PROXIMITY REPORT/Desktop/MA POP Learning materials/lecture notes/622/group2.docx")
