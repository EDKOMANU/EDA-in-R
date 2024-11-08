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
library(tmap)
library(sf)
library(psych)
setwd("C:/Users/PROXIMITY REPORT/Desktop/rips 607/New/New analysis")




Dissert_New |> 
  select(score, size, dependency) |> 
  corr.test(alpha = T)

Census<- read_dta("defactopopn_10%_20221011d.dta")
Dissert<-read_dta("Census_dissertation_16072024.dta")

new <- Census |> 
  select(nqid, a11d, p02, ) |> 
  mutate(across(where(is.labelled), as_factor)) |> 
  
  mutate(p02 = as.numeric(p02),
         new_age = case_when(p02 <= 14 ~ 'Child', 
                             p02 >=15 & p02 <= 59 ~ 'Working_age',
                             p02>=60 ~ 'Aged')) |> 
  group_by(nqid, new_age) |> 
  summarize(count=n()) |> 
  ungroup() |> 
  pivot_wider(names_from = new_age, values_from = count)


Dissert<-Dissert |> 
  mutate(across(where(is.labelled), as_factor)) |>
  left_join(new, by='nqid') |> 
  mutate(Child = replace_na(Child, 0),
         Working_age = replace_na(Working_age, 0),
         Aged = replace_na(Aged, 0),
         dependency= ((Child+Aged)/Working_age))


#Dissert<-Dissert |> 
#  mutate(dependency = if_else(dependency==Inf, 0, dependency))  



Dissert_New<-Dissert |> 
  mutate(
    dependency= round(((Child+Aged)/Working_age)*10,2),
    dependency = if_else(dependency==Inf, (Child + Aged)*10, dependency),
    p17=if_else(is.na(p17), "Outside labour force", p17), 
    h06 = if_else(is.na(h06), 'Own dwelling', h06),
    Sector = if_else(p17 %in% c("Public (Government)", "Semi-Public/Parastatal"), "Public", 
                     if_else(p17 == "Private Informal", "Private Informal", 
                             if_else(p17=="Outside labour force", "Outside labour force",
                                     "Private Formal"))),
    Sector= factor(Sector, levels=c("Public","Private Formal","Private Informal","Outside labour force")),
    Ownership_dwell = fct_collapse(h06, "Private" = c("Estate developer","Other private individual"),
                                   "Family/Relatives" = c("Family property","Relative not household member"),
                                   "Corporate/public house" = c("Private employer", "Other private agency", "Public/Government","Other")), 
    Ownership_dwell= factor(Ownership_dwell, levels=c('Own dwelling', "Private","Family/Relatives","Corporate/public house")) )



Dissert_New <- Dissert_New |> 
  select(-c( paidwork, ownpw, a11c,econ_active_pop, rweight, restype)) |> 
  rename(condition = living_sconditions) |> 
  mutate(across(where(is.labelled), as_factor)) |>  
 # mutate(across(everything(), ~ if (is.factor(.)) as.character(.) else .)) |>  
  #mutate(across(everything(), ~ replace_na(., "Ukn"))) |> 
  mutate(across(where(is.character), as.factor)) |>  # Optionally, convert back to factors
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
                        p02 > 35 & p02 <= 59 ~'Adult', 
                        p02 >=60 ~'Aged'), 
         Region = factor(Region,levels=c('Greater Accra', 'Western' ,'Central', 'Volta', 'Eastern', 'Ashanti', 'Western North', 'Ahafo', 'Bono', 'Bono East', 'Oti', 'Northern', 'Savannah', 
                                         'North East', 'Upper East', 'Upper West')), 
         Marital_status = factor(p10, levels=c( 'Currently in union','Formerly in union' ,'never married')), 
         edu = factor(edu, levels=c('Tertiary/Higher', 'Primary','JHS/JSS','SHS/SSS/VOC', 'None')), 
         Age = factor(Age, levels=c('Adult','Youth','Aged')),
         Sex = factor(a11d, levels=c("Male", "Female")), 
         urbrur= factor(urbrur, levels=c("Urban", "Rural")),
         Sector= factor(Sector, levels=c("Public","Private Formal","Private Informal","Outside labour force")),
         Ownership_dwell= factor(Ownership_dwell, levels=c('Own dwelling', "Private","Family/Relatives","Corporate/public house"))) |> 
  mutate(p14b1_new = if_else(p14b1_new %in% c("Managers","Professionals","Clerical support workers", "Technicians and associate professionals"),
                             "Business Admin/Profeesionals", if_else(p14b1_new %in%
                                                                       c("Craft and related trades workers", "Plant and machine operators, and assemblers",
                                                                         "Elementary occupations", "Other occupations"), "Industrial work", p14b1_new) ))



svy<-svydesign(id=~nqid+pid, weights=~weight, data=Dissert_New)




#Univariate-----------


univar<- Dissert_New |> 
  select(poverty, Age, Sex, edu, Sector,p14b1_new,Marital_status,New_Region,urbrur,Ownership_dwell,  
         size, dependency) |> 
  tbl_summary(
                        statistic = list(
                          all_continuous() ~ "{mean} ({sd})",  
                          #p02 ~ "{median} ({min} {max})",                    
                          dependency ~ "{mean} ({sd})",  
                          size ~ "{mean} ({sd})" )) |>  
  as_flex_table( separate_with = "variable") |> 
  height_all(height = 0.02) |> 
  hline(part = 'footer') |> 
  font(fontname="times new roman", part='all') |> 
  fontsize(size = 9, part = "all")|> 
  autofit()


#Bivariate------------
bivar<- tbl_svysummary(svy, by=poverty, percent = 'row', 
                       include = c(poverty, Age, Sex, edu, Sector,p14b1_new, Marital_status,Region,urbrur,Ownership_dwell,  
                                   size, dependency),statistic = list(
                                     all_continuous() ~ "{mean} ({sd})",  
                                     #p02 ~ "{median} ({min} {max})",                    
                                     dependency ~ "{mean} ({sd})",  
                                     size ~ "{mean} ({sd})" ), 
                       label= list(poverty = 'Level of Poverty')) |> 
  add_p() |> 
  as_flex_table( separate_with = "variable")  |>  
  
  height_all(height = 0.01) |>  
  hline(part = 'footer') |> 
  font(fontname="times new roman", part='all') |>  
  fontsize(size = 9, part = "all") |> 
  autofit()




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



###____________Multinom--------
mult1<-multinom(poverty ~ Age + Sex + edu + Sector + p14b1_new + Marital_status + Region + urbrur + Ownership_dwell + size + dependency, weights = weight,data=Dissert_New)

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

tidy_parameters(mult1)

# Calculate McFadden's Pseudo R-squared
pseudo_r2 <- 1 - (deviance_full  / deviance_null)
pseudo_r2

#--Exporting--------
doc<-read_docx("analysis.docx") %>% 
  body_add_flextable(value=univar)|> 
  body_add_break(pos='after')  |>  
  body_add_flextable( value=bivar)|> 
  body_add_break(pos='after') %>% 
  body_add_flextable( value=mult_sum1 ) |> 
  print(target= "analysis.docx")



levels(factor(Dissert_New$Sector))


table(Dissert_New$p14b1_new)




save(Dissert_New, file="Dissert.rdata")

Dissert |> 
  group_by((pov)) |> 
  summarise(dependency = mean(dependency))






min(Dissert$p02)
table(Dissert$p02)

region=Dissert |> select(Region ) |> 
  mutate(Region= str_replace(Region, pattern=' ', replacement=''))