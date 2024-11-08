library(tidyverse)
library(gtsummary)
library(haven)
library(officer)
library(survey)
library(svyVGAM)
library(cardx)
library(flextable)
library(parameters)
library(broom.helpers)
library(nnet)

setwd("C:/Users/PROXIMITY REPORT/Desktop/rips 607/New/New analysis")
 Census<- read_dta("defactopopn_10%_20221011d.dta")
 Dissert<-read_dta("Census_dissertation_16072024.dta")
 
 
 new <- Census |> 
   select(nqid, a11d, p02, ) |> 
   mutate(across(where(is.labelled), as_factor)) |> 
   
   mutate(p02 = as.numeric(p02),
          new_age = case_when(p02 <= 14 ~ 'Child', 
                              p02 >=15 & p02 <= 64 ~ 'Working_age',
                              p02>=65 ~ 'Aged')) |> 
   group_by(nqid, new_age) |> 
   summarize(count=n()) |> 
   ungroup() |> 
   pivot_wider(names_from = new_age, values_from = count)
   

 Dissert1<-Dissert |> 
   mutate(across(where(is.labelled), as_factor)) |>
   left_join(new, by='nqid') |> 
  mutate(Child = replace_na(Child, 0),
         Working_age = replace_na(Working_age, 0),
         Aged = replace_na(Aged, 0),
         dependency= ((Child+Aged)/Working_age))
 
 occupat <- Census |> 
   select(nqid, a11d, p14b1_new) |> 
   mutate(across(where(is.labelled), as_factor)) |>
   mutate(p14b1_new = if_else(p14b1_new %in% c("Managers","Professionals","Clerical support workers", "Technicians and associate professionals"),
                              "Business Admin/Profeesionals", if_else(p14b1_new %in%
                                                                        c("Craft and related trades workers", "Plant and machine operators, and assemblers",
                                                                          "Elementary occupations", "Other occupations"), "Industrial work", p14b1_new) )) |>  na.omit()
 
 occupat_count<-occupat |> 
   group_by(nqid, p14b1_new) |> 
   count() |> 
   ungroup() |> 
   group_by(nqid) |> 
   slice_max(n, n=1) |> 
   ungroup() |> 
   rename(hh_occu = p14b1_new)
 
 occupat_count<-distinct(occupat_count, nqid, .keep_all = T)
 
 Dissert1<-Dissert1 |> 
   left_join(occupat_count, by="nqid") 
#Dissert<-Dissert |> 
#  mutate(dependency = if_else(dependency==Inf, 0, dependency))  



Dissert_New<-Dissert1 |> 
  mutate(
         dependency= Child+Aged,
        # dependency = if_else(dependency==Inf, (Child + Aged)*10, dependency),
         p17=if_else(is.na(p17), "Outside labour force", p17), 
        
         h06 = if_else(is.na(h06), 'Own dwelling', h06),
         Sector = if_else(p17 %in% c("Public (Government)", "Semi-Public/Parastatal"), "Public", 
                          if_else(p17 == "Private Informal", "Private Informal", 
                                if_else(p17=="Outside labour force", "Outside labour force",
                                "Private Formal"))),
         Sector= factor(Sector, levels=c("Public","Private Formal","Private Informal","Outside labour force")),
         Ownership_dwell = fct_collapse(h06, "Private" = c("Estate developer","Other private individual","Other"),
                                                          "Family/Relatives" = c("Family property","Relative not household member"),
                                                          "Corporate/public house" = c("Private employer", "Other private agency", "Public/Government")), 
         Ownership_dwell= factor(Ownership_dwell, levels=c('Own dwelling', "Private","Family/Relatives","Corporate/public house")) ,
        New_Region = fct_collapse(Region, "Northern Region" = c( "Northern" ,"Savannah","North East","Upper East","Upper West", "Oti"), 
                                  "Middle Region" = c("Ashanti", "Western North", "Ahafo","Bono", "Bono East","Eastern"), 
                                  "Coastal/Southern Region" = c("Western",  "Central", "Greater Accra","Volta" ))) |> 
  select(-c( paidwork, ownpw, a11c,econ_active_pop, rweight, restype)) |> 
  rename(condition = living_sconditions) |> 
  mutate(across(where(is.labelled), as_factor)) |>  
  #mutate(across(everything(), ~ if (is.factor(.)) as.character(.) else .)) |>   Convert factors to characters
  #mutate(across(everything(), ~ replace_na(., "Ukn"))) |>   
  mutate(across(where(is.character), as.factor)) |>  
  mutate(twater = factor(twater, levels = c(1, 0), labels = c('unimproved', 'improved')), 
         agric_mach = factor(agric_mach, levels = c(1, 0), labels = c('do not own', 'own')), 
         farm_mach = factor(farm_mach, levels = c(1, 0), labels = c('do not own', 'own')),
         fishery_equ = factor(fishery_equ, levels = c(1, 0), labels = c('do not own', 'own')), 
         #Region= str_replace(Region, pattern=' ', replacement='')
         #no_children= as.factor(no_children), 
         p02=as.numeric(p02)-1, 
         poverty= case_when(score<=0.3 ~ 'Not poor', 
                            score > 0.3 & score < 0.5 ~'Poor', 
                            score >= 0.5 ~'Extremely poor'), 
         poverty= factor(poverty, levels=c('Not poor' ,'Extremely poor','Poor')), 
         Age= case_when(p02<=35 ~ 'Youth', 
                        p02 > 35 & p02 <= 64 ~'Adult', 
                        p02 >=65 ~'Aged'), 
         Region = factor(Region,levels=c('Greater Accra', 'Western' ,'Central', 'Volta', 'Eastern', 'Ashanti', 'Western North', 'Ahafo', 'Bono', 'Bono East', 'Oti', 'Northern', 'Savannah', 
                                         'North East', 'Upper East', 'Upper West')), 
         Marital_status = factor(p10, levels=c( 'Currently in union','Formerly in union' ,'never married')), 
         edu = factor(edu, levels=c('Tertiary/Higher', 'None', 'Primary','JHS/JSS','SHS/SSS/VOC')), 
         Age = factor(Age, levels=c('Adult','Youth','Aged')),
         Sex = factor(a11d, levels=c("Male", "Female")), 
         urbrur= factor(urbrur, levels=c("Urban", "Rural")),
         #Sector= factor(Sector, levels=c("Public","Private Formal","Private Informal")),
         Ownership_dwell= factor(Ownership_dwell, levels=c( "Private",'Own dwelling',"Family/Relatives","Corporate/public house"))) |> 
  mutate(Industry = if_else(p14b1_new %in% c("Managers","Professionals","Clerical support workers", "Technicians and associate professionals", "Service and sales workers"),
                             "Service and sales", if_else(p14b1_new %in%
                                                                       c("Craft and related trades workers", "Plant and machine operators, and assemblers",
                                                                         "Elementary occupations","Elementary occupation workers", "Other occupations"), "Industrial work", p14b1_new) ), 
         
         hh_occu= if_else(hh_occu %in% c("Business Admin/Profeesionals","Service and sales workers"),
                          "Service and sales", if_else(hh_occu %in%
                                                         c("Elementary occupation workers","Industrial work"), "Industrial work", hh_occu) ),
         Industry = if_else(is.na(Industry), hh_occu, Industry), 
         Industry=factor(Industry, levels=c("Service and sales", "Industrial work", "Skilled agricultural, forestry and fishery workers"))) 
 






  
levels(factor(Dissert_New$Industry))
table(Dissert_New$pov)

#----Svy Design
svy<-svydesign(id=~nqid+pid, weights=~weight, data=Dissert_New)

aaaa<-Dissert_New|> 
  filter(poverty %in% c('Extremely poor','Poor'))
  sum(aaaa$score)

#Univariate-----------

univar<- tbl_svysummary(svy, include = c(poverty, Age, Sex, edu,Sector, Marital_status, Industry, New_Region,urbrur,Ownership_dwell, size, dependency),
                        statistic = list(
                          all_continuous() ~ "{mean} ({sd})",  
                          #p02 ~ "{median} ({min} {max})",                    
                          dependency ~ "{mean} ({sd})",  
                          size ~ "{mean} ({sd})" ), missing = "no") |>  
  as_flex_table( separate_with = "variable") |> 
  height_all(height = 0.02) |> 
  hline(part = 'footer') |> 
  font(fontname="times new roman", part='all') |> 
  fontsize(size = 9, part = "all")|> 
  autofit()


#Bivariate------------
bivar<- tbl_svysummary(svy, by=poverty, percent = 'row', 
                       include = c(poverty, Age, Sex, edu,Sector, Marital_status,Industry, New_Region,urbrur,Ownership_dwell, size, dependency),statistic = list(
                                     all_continuous() ~ "{mean} ({sd})",  
                                     #p02 ~ "{median} ({min} {max})",                    
                                     dependency ~ "{mean} ({sd})",  
                                     size ~ "{mean} ({sd})" ), 
                       label= list(poverty = 'Level of Poverty'), missing = "no") |> 
  add_p() |> 
  as_flex_table( separate_with = "variable")  |>  
  
  height_all(height = 0.01) |>  
  hline(part = 'footer') |> 
  font(fontname="times new roman", part='all') |>  
  fontsize(size = 9, part = "all") |> 
  autofit()





###____________Multinom--------
mult1<-multinom(poverty ~ Age + Sex + edu + Sector + Marital_status + Industry + New_Region + urbrur + Ownership_dwell + size + dependency, weights = weight,data=Dissert_New)

null<-multinom(poverty ~ 1, weights = weight,data=Dissert_New)
R2<- 1 - (logLik(mult1) / logLik(null))

mult_sum1 <-tbl_regression(mult1, exponentiate = T) |> 
  
  as_flex_table( separate_with = "variable") |> 
  height_all(height = 0.01) |>  
  hline(part = 'footer') |> 
  font(fontname="times new roman", part='all') |> 
  fontsize(size = 9, part = "all") |> 
  autofit()
#null_model <- svy_vglm(poverty ~ 1, design = svy, family = multinomial())

summary(mult1)
# Extract deviance (pseudo-likelihood based measure)----
deviance_full <- deviance(mult1)
deviance_null <- deviance(null)

#tidy_parameters(mult1)

# Calculate McFadden's Pseudo R-squared
pseudo_r2 <- 1 - (deviance_full  / deviance_null)
pseudo_r2



#Model- Multinomial Logistic Regression------


mult <- svy_vglm(poverty ~ Age + Sex + edu + econs + Marital_status + Region + urbrur + size + no_children,
                 design = svy,
                 family = multinomial())

mult_sum <-tbl_regression(mult, exponentiate = T) |> 
  
  as_flex_table( separate_with = "variable") |> 
  height_all(height = 0.01) |>  
  hline(part = 'footer') |> 
  font(fontname="times new roman", part='all') |> 
  fontsize(size = 9, part = "all") |> 
  autofit()



#--Exporting--------
doc<-read_docx("new_ana.docx") %>% 
  body_add_flextable(value=univar)|> 
  body_add_break(pos='after')  |>  
  body_add_flextable( value=bivar)|> 
  body_add_break(pos='after') |>  
  body_add_flextable( value=mult_sum1 ) |>
  body_add_break(pos='after') |> 
  body_add_gg(value=Score) |> 
  print(target= "new_ana.docx") 
  


         
levels(factor(Dissert_New$Sector))
 

table(Dissert_New$p14b1_new)

 
 
 
save(Dissert_New, file="Dissert.rdata")
 
Dissert_New |> 
 # group_by((pov)) |> 
  summarise(dependency = mean(score))
  


Score<-Dissert_New |> 
  select(Age, score) |> 
  ggplot(aes(x=Age, y=score, fill = Age))+
  geom_boxplot()+stat_summary(fun =mean,geom = "text", col='white', aes(label = round(..y.., 1)), vjust=1)+
  labs(title='BoxPlot of Multidimensional Poverty Score by Age Catergories',
       x= 'Sex',
       y= 'Multidimensional Poverty Score',
       fill="")+
  theme(legend.position = 'none',
        axis.text.x = element_text(size=9, face='bold'),
        axis.text.y=element_text(size=9, face ='bold'))

Dissert_New |> 
  select(p02, score) |> 
  group_by(p02) |> 
  summarise(score=mean(score)) |> 
  ggplot(aes(x=p02, y=score, colour='green',))+
  geom_line()



Dissert_New |>
  ggplot(aes(y = score)) + 
  geom_boxplot() #+ 
  #stat_summary(fun = mean, geom = "text"), aes(label = round(..y.., 1)), col = 'white', vjust = 1)

Dissert_New<-Dissert_New |> 
  left_join(occupat_count, by="nqid")  
  #---------Not needed------
  
 household <- Census |> 
   mutate(across(where(is.labelled), as_factor)) |>
   filter(a11c=='Head') |> 
   filter(!restype=='Homeless household') |> 
   select(nqid, pid,p14b1_new) |> 
   mutate(p14b1_new = if_else(p14b1_new %in% c("Managers","Professionals","Clerical support workers", "Technicians and associate professionals"),
                              "Business Admin/Profeesionals", if_else(p14b1_new %in%
                                                                        c("Craft and related trades workers", "Plant and machine operators, and assemblers",
                                                                          "Elementary occupations", "Other occupations"), "Industrial work", p14b1_new) ))
Dissert_New<-Dissert_New |> 
  left_join(household, by=c("nqid", "pid"))
 
 
 