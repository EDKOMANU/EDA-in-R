Sex<- Dissert_New |> 
  group_by(Sex) |> 
  mutate(pop = n()) |>
  ungroup() |> 
  mutate(propSex=pop/sum(pop)) |> 
  group_by(Sex, poverty) |> 
  mutate(propPov= round((n()/pop)*100,1)) |> 
  select(sex=Sex, poverty, pop, propPov, propSex) |> 
  mutate(poverty=factor(poverty, levels=c("Extremely poor", "Poor", "Not poor")))

SexChart<-distinct(Sex, sex, poverty, .keep_all = T) |> 
  ggplot(aes(x=poverty, y=propPov, fill=sex))+
  geom_col()+
  #theme(legend.position = "none")+
  labs(x="Poverty Status", 
       y="percent", 
       fill="")+
  geom_text(mapping = aes(label =scales::comma(propPov)), 
            size=6, hjust=1.5, vjust=1,col= "white", position = position_dodge(width =0.7))+
  coord_flip()+
  facet_wrap(~sex)+
  theme(legend.position = 'top',
        axis.text.x = element_text(size=12, face='bold'),
        axis.text.y=element_text(size=12, face ='bold'),
        legend.text = element_text(size = 12, face='bold'),
        legend.key.spacing.x = unit(6, 'cm'),strip.text = element_blank())



Urb<- Dissert_New |> 
  group_by(urbrur) |> 
  mutate(pop = n()) |>
  ungroup() |> 
  #mutate(propSex=pop/sum(pop)) |> 
  group_by(urbrur, poverty) |> 
  mutate(propPov= round((n()/pop)*100,1)) |> 
  select(urbrur, poverty, pop, propPov) |> 
  mutate(poverty=factor(poverty, levels=c("Extremely poor", "Poor", "Not poor")))

UrbChart<-distinct(Urb, urbrur, poverty, .keep_all = T) |> 
  ggplot(aes(x=poverty, y=propPov, fill=urbrur))+
  geom_col()+scale_fill_manual(values=c("brown","#66c2a5"))+
  #theme(legend.position = "none")+
  labs(x="Poverty Status", 
       y="percent", 
       fill="")+#, title= "Poverty Status by Type of Place of Residence (Urban/Rural)")+
  geom_text(mapping = aes(label =scales::comma(propPov)), 
            size=8, hjust=-0.5, vjust=1,col= "black", position = position_dodge(width =0.7))+
  coord_flip()+
  facet_wrap(~urbrur)+
  scale_y_continuous(limits=c(0, 80))+
  theme(legend.position = 'top',
                            axis.text.x = element_text(size=20, face='bold'),
                            axis.text.y=element_text(size=20, face ='bold'),
                            legend.text = element_text(size = 20, face='bold'),
        legend.key.spacing.x = unit(6, 'cm'),strip.text = element_blank())
levels(factor(Sex$poverty))








Age<- Dissert_New |> 
  group_by(Age) |> 
  mutate(pop = n()) |>
  ungroup() |> 
  mutate(propAge=pop/sum(pop)) |> 
  group_by(Age, poverty) |> 
  mutate(propPov= round((n()/pop)*100,1)) |> 
  select(age=Age, poverty, pop, propPov, propAge) |> 
  mutate(poverty=factor(poverty, levels=c("Extremely poor", "Poor", "Not poor")), 
         age= factor(age, levels=c('Youth', 'Adult', 'Aged')))

AgeChart<-distinct(Age, age, poverty, .keep_all = T) |> 
  ggplot(aes(x=poverty, y=propPov, fill=age))+
  geom_col()+
  #theme(legend.position = "none")+
  labs(x="Poverty Status", 
       y="percent", 
       fill="")+ scale_y_continuous(limits=c(0,70))+
  geom_text(mapping = aes(label =scales::comma(propPov)), 
            size=6, hjust=0, vjust=1,col= "black", position = position_dodge(width =0.7))+
  coord_flip()+
  facet_wrap(~age)+
  theme(legend.position = 'top',
        axis.text.x = element_text(size=12, face='bold'),
        axis.text.y=element_text(size=12, face ='bold'),
        legend.text = element_text(size = 12, face='bold'),
        legend.key.spacing.x = unit(6, 'cm'),strip.text = element_blank())



region<- Dissert_New |> 
  group_by(New_Region) |> 
  mutate(pop = n()) |>
  ungroup() |> 
  mutate(propAge=pop/sum(pop)) |> 
  group_by(New_Region, poverty) |> 
  mutate(propPov= round((n()/pop)*100,1)) |> 
  select(New_Region, poverty, pop, propPov, propAge) |> 
  mutate(poverty=factor(poverty, levels=c("Extremely poor", "Poor", "Not poor")))

regionChart<-distinct(region, New_Region, poverty, .keep_all = T) |> 
  ggplot(aes(x=poverty, y=propPov, fill=New_Region))+
  geom_col()+ scale_fill_manual(values=c("#66c2a5", "darkgreen", 'brown'))+
  #theme(legend.position = "none")+
  labs(x="Poverty Status", 
       y="percent", 
       fill="")+ scale_y_continuous(limits=c(0,70))+
  geom_text(mapping = aes(label =scales::comma(propPov)), 
            size=6, hjust=0, vjust=1,col= "black", position = position_dodge(width =0.7))+
  coord_flip()+
  facet_wrap(~New_Region)+
  theme(legend.position = 'top',
        axis.text.x = element_text(size=12, face='bold'),
        axis.text.y=element_text(size=12, face ='bold'),
        legend.text = element_text(size = 12, face='bold'),
        legend.key.spacing.x = unit(5, 'cm'),strip.text = element_blank())



poverty<- Dissert_New |> 
  group_by(poverty) |> 
  summarise(`number of households`=n()*9.998) |> 
  mutate(percent=round((`number of households`/sum(`number of households`))*100,1), 
         poverty=factor(poverty, levels=c('Not poor' ,'Poor','Extremely poor'))) #|> 
  pivot_longer(names_to = 'type', values_to = 'popu', cols=c(`number of households`, percent) )
  

povchart<-poverty |> 
  ggplot(aes(x=fct_rev(poverty), y=percent, fill=poverty))+
  geom_col()+ scale_y_continuous(limits=c(0, 60))+
  scale_fill_manual(values=c('darkgreen', 'lightcoral','brown' ))+
  #theme(legend.position = "none")+
  labs(x="Poverty Status", 
        
       fill="")+
  geom_text(mapping = aes(label =scales::comma(percent)), 
            size=8, hjust=0, vjust=1,col= "black", position = position_dodge(width =0.7))+
  coord_flip()+
  
  theme(legend.position = 'none',
        axis.text.x = element_text(size=15, face='bold'),
        axis.text.y=element_text(size=15, face ='bold'),
        legend.text = element_text(size = 15, face='bold'),
        )



industry<- Dissert_New |> 
  group_by(Industry) |> 
  mutate(pop = n()) |>
  ungroup() |> 
  mutate(propAge=pop/sum(pop)) |> 
  group_by(Industry, poverty) |> 
  mutate(propPov= round((n()/pop)*100,1)) |> 
  select(Industry, poverty, pop, propPov, propAge) |> 
  mutate(poverty=factor(poverty, levels=c("Extremely poor", "Poor", "Not poor")))

regionChart<-distinct(industry, Industry, poverty, .keep_all = T) |> 
  na.omit() |> 
  ggplot(aes(x=poverty, y=propPov, fill=Industry))+
  geom_col()+ scale_fill_manual(values=c("#66c2a5", "darkgreen", 'brown'))+
  #theme(legend.position = "none")+
  labs(x="Poverty Status", 
       y="percent", 
       fill="")+ scale_y_continuous(limits=c(0,80))+
  geom_text(mapping = aes(label =scales::comma(propPov)), 
            size=8, hjust=0, vjust=1,col= "black", position = position_dodge(width =0.7))+
  coord_flip()+
  facet_wrap(~Industry)+
  theme(legend.position = 'top',
        axis.text.x = element_text(size=20, face='bold'),
        axis.text.y=element_text(size=20, face ='bold'),
        legend.text = element_text(size = 20, face='bold'),
        legend.key.spacing.x = unit(3, 'cm'),strip.text = element_blank())

