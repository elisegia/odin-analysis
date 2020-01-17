#Set Options
options(warn=-1)
options(scipen=999)

#Load Libraries
library(reshape)
library(plyr)
library(dplyr)
library(forcats)
library(lubridate)
library(ggplot2)
library(xlsx)
library(sp)
library(rgdal)
library(viridis)
library(sjPlot)
library(sjmisc)
library(pscl)
library(margins)
library(effects)

#Link to source for cross-tabs
source("http://pcwww.liv.ac.uk/~william/R/crosstab.r")

# Read in cleaned ODIN data from Pennsylvania [put website]
odin_pa <- read.csv("/Users/gbarboza/Desktop/ODIN_PA/Overdose_Information_Network_Data_CY_January_2018_-_Current_Monthly_County_State_Police.csv", na.strings=c("","NA"))

#Do some data cleaning
#Race and Ethnicity -------------------------------------
odin_pa$hisp<-ifelse(odin_pa$Ethnicity.Desc=='Hispanic',1,0)
odin_pa$race_eth[odin_pa$hisp == 0 & odin_pa$Race=="White"]<-"NHWhite"
odin_pa$race_eth[odin_pa$hisp == 0 & odin_pa$Race=="Black"]<-"NHBlack"
odin_pa$race_eth[odin_pa$hisp == 1 ]<-"Hisp"
odin_pa$black<-ifelse(odin_pa$Race=='Black',1,0)
odin_pa$white<-ifelse(odin_pa$Race=='White',1,0)

#Response time-------------------------------------------
odin_pa$Response.Time.Desc[odin_pa$Response.Time.Desc == "DID NOT WORK"]<-NA
odin_pa$Response.Time.Desc <- factor(odin_pa$Response.Time.Desc)

#Date & Time__________________________________
odin_pa$DT <-  mdy_hms(odin_pa$datetime)

#Insert hour, month and year as separate variables
odin_pa$hour <- hour(odin_pa$DT) 
odin_pa$month <- month(odin_pa$DT)  
odin_pa$year <- year(odin_pa$DT)  

#Ages into numeric var______________________________
odin_pa <- odin_pa %>%
  mutate(age = fct_recode(Age.Range,
                          "1" = "0 - 9"  ,
                          "1" = "10-14" ,
                          "1"= "15 - 19",
                          "2" = "20 - 24",
                          "2" = "25 - 29",
                          "3"=  "30 - 39" ,
                          "4" = "40 - 49" ,
                          "5" = "50 - 59"  ,
                          "6" = "60 - 69" ,
                          "6" = "70 - 79" ,
                          "6" = "80 - *" 
                          )
         )

#Revival Action ___________________________________         
odin_pa <- odin_pa %>%
  mutate(revived = fct_recode(Revive.Action.Desc,
                          "Arrest" = "ARREST"  ,
                          "Hospital" = "HOSPITAL CONSCIOUS" ,
                          "Hospital"="HOSPITAL UNCONSCIOUS",
                          "Refused" = "REFUSED TRANSPORT",
                          "Other" = "RELEASED",
                          "Referred to treatment"=  "TRANSPORTED TO TREATMENT" ,
                          "Referred to treatment" = "VERBALLY REFERRED TO TREATMENT" ,
                          "Other" = "OTHER"  ,
                          "NA" = "DON'T KNOW" 
                          )
  )

odin_pa <- odin_pa %>%
  mutate(drugs = fct_recode(Susp.OD.Drug.Desc,
                              "Alcohol" = "ALCOHOL"  ,
                              "Heroin" = "HEROIN" ,
                              "Fentanyl"="FENTANYL",
                              "Fentanyl" = "FENTANYL ANALOG/OTHER SYNTHETIC OPIOID",
                              "Pharma" = "PHARMACEUTICAL STIMULANT",
                              "Pharma"=  "PHARMACEUTICAL OTHER" ,
                              "Pharma" = "PHARMACEUTICAL OPIOID" ,
                              "Benzos" = "BENZODIAZEPINES (I.E.VALIUM, XANAX, ATIVAN, ETC)"  ,
                              "Methadone" = "METHADONE", 
                              "Suboxone" = "SUBOXONE",
                              "MJ" = "MARIJUANA",
                              "MJ" = "SYNTHETIC MARIJUANA",
                              "Meth" = "METHAMPHETAMINE",
                              "Carf" = "CARFENTANIL",
                              "Salts" = "BATH SALTS",
                              "unknown" = "unknown"
                        
                        )
  )

odin_pa$heroin<-ifelse(odin_pa$drugs=='Heroin',1,0)
odin_pa$fentanyl<-ifelse(odin_pa$drugs=='Fentanyl',1,0)
odin_pa$pharma<-ifelse(odin_pa$drugs=='Pharma',1,0)

#Before any analysis let's make sure we have the right selections
#In order to know how many unique incidents there were, we need to consider that each incident could have multiple victims. 
#The dataset does not take that into consideration, so we need to apply some filters
#The following dataset provides the count of all drugs involved with the incident using the
#key that identifies a unique record containing the details of a single incident (identified by date, time and location)
unique_incidents <- odin_pa[!duplicated(odin_pa[,c('Incident.ID')]),] # of the 14,400 total cases, 10,290 were unique incidents
duplicated_incidents <- odin_pa[duplicated(odin_pa[,c('Incident.ID')]),] # 4,110 were not unique, there are duplicate victims and/or multiple drugs suspected of causing the OD

#Below we get all incidents where each line represents a unique victim regardless of how many drugs were suspected 
#in causing the OD
#The following dataset contains incidents with unique victim IDs (i.e. each incident may have had more than 1 victim) using the
#key that identifies a unique record for a victim.
#According to the result, there were 10,465 unique incidents with unique victim IDs

unique_incidents_victims <- odin_pa[!duplicated(odin_pa[,c('Incident.ID','Victim.ID')]),]
duplicated_incidents_victims <- odin_pa[duplicated(odin_pa[,c('Incident.ID','Victim.ID')]),]

##Finally, this dataset selects incidents with multiple distinct victims (note, 
#some cases have multiple victimizations of the same person, this code excludes individuals with repeat victimizations 
#as it is not necessary to include them (i.e. their characteristics do not change))
multiple_unique_victims <-unique_incidents_victims %>% 
dplyr::group_by(Incident.ID) %>%
dplyr::add_count(Incident.ID) %>%
dplyr::distinct(Incident.ID, .keep_all = TRUE)

mean(multiple_unique_victims$n) #average number of persons per incident
table(multiple_unique_victims$n) #distribution of number of unique victims of drug OD per incident

aggregate(n ~ race_eth, data=multiple_unique_victims, mean)
aggregate(Dose.Count ~ race_eth, data=multiple_unique_victims, mean)
aggregate(Dose.Unit ~ race_eth, data=multiple_unique_victims, mean)

#incidents and victims with each line representing a different drug suspected of causing the OD
#this dataset is the number of incidents with victims that had multiple drugs suspected of causing the OD using the
#unique identifier for each suspected drug causing the overdose. 
unique_incidents_victims_drugs <- odin_pa[!duplicated(odin_pa[,c('Incident.ID','Victim.ID', 'Victim.OD.Drug.ID')]),]
duplicated_incidents_victims_drugs <- odin_pa[duplicated(odin_pa[,c('Incident.ID','Victim.ID', 'Victim.OD.Drug.ID')]),]

multiple_unique_victims_polydrugs <-unique_incidents_victims_drugs %>% #this dataset selects incidents with multiple distinct victims and counts
  #how many drugs were suspected in causing their overdose
  dplyr::group_by(Incident.ID) %>%
  dplyr::add_count(Victim.ID) %>%
  dplyr::distinct(Victim.ID, .keep_all = TRUE)

multiple_unique_victims_polydrugs$polydrug<-ifelse(multiple_unique_victims_polydrugs$n >1,1,0)
newdata <- multiple_unique_victims_polydrugs[ which(multiple_unique_victims_polydrugs$race_eth=='NHWhite'), ]
crosstab(newdata, row.vars = "Naloxone_Admin", col.vars = "polydrug", type = "c")
chisq.test(newdata$Naloxone_Admin, newdata$polydrug, correct = FALSE)

multiple_unique_victims$missing<-ifelse(is.na(multiple_unique_victims$Survived),1,0)
#same as unique incidents
mean(multiple_unique_victims_polydrugs$n)
table(multiple_unique_victims_polydrugs$n)
aggregate(n ~ race_eth, data=multiple_unique_victims_polydrugs, mean)

#same result using the following code
polydruguse <- aggregate(Victim.OD.Drug.ID ~ Incident.ID+Victim.ID, data = unique_incidents_victims_drugs, FUN = length)
mean(polydruguse$Victim.OD.Drug.ID)
table(polydruguse$Victim.OD.Drug.ID)

##########Descriptive statistics
table(multiple_unique_victims_polydrugs$race_eth)

#Chi-Square of indep vars by race
crosstab(multiple_unique_victims_polydrugs, row.vars = "age", col.vars = "race_eth", type = "c")
chisq.test(multiple_unique_victims_polydrugs$age, multiple_unique_victims_polydrugs$race_eth, correct = FALSE)

#Descriptives on dates
odin_pa %>%
  ggplot(aes(wday(DT, label = TRUE))) +
  geom_bar(fill = "midnightblue", alpha = 0.8) +
  labs(x = NULL,
       y = "Number of events")

race_hour_OD <- unique_incidents_victims %>% dplyr::group_by(race = race_eth, hour = hour(DT)) %>% 
  dplyr::count()  %>% tidyr::spread(race, n) 

race_month_OD <- unique_incidents_victims %>% dplyr::group_by(race = race_eth, month = month(DT)) %>% 
  dplyr::count()  %>% tidyr::spread(race, n) 

#save to excel for better plotting, I did not like any options here
#write.xlsx(data.frame(race_hour_OD), "race_hour_OD_excel.xlsx")
#write.xlsx(data.frame(race_month_OD), "race_month_OD_excel.xlsx")

DRUG_OD <- unique_incidents_victims_drugs %>% dplyr::group_by(ID = Incident.ID, drug = Susp.OD.Drug.Desc) %>% 
  dplyr::count()  %>% tidyr::spread(drug, n) 

d<- unique_incidents_victims %>% #max od in a single day
  select(DT, race_eth) %>%
  dplyr::group_by(race = race_eth, year = year(DT), month = month(DT), day = day(DT)) %>% 
  dplyr::count()  %>% tidyr::spread(race, n) 

d<- as.data.frame(d)
write.xlsx(d, "d.xlsx")

d<- unique_incidents_victims %>%
  select(DT, race_eth) %>%
  dplyr::group_by(race = race_eth, hour = hour(DT)) %>% 
  dplyr::count()  %>% 
  filter(hour>0) %>% tidyr::spread(race, n)

colors <- c("Hisp" = "blue", "NHBlack" = "red", "NHWhite" = "green")

ggplot(d, aes(x=hour)) +
  geom_line(aes(hour, Hisp*50/3, color = "Hisp"), size=.8, linetype="dashed", size=.5) +
  geom_point(aes(hour, Hisp*50/3, color = "Hisp"),size=1) +
  geom_line(aes(hour, NHBlack*50/3, color = "NHBlack"),  linetype="dashed", size=.5) +
  geom_point(aes(hour, NHBlack*50/3,  color = "NHBlack"),size=1) +
  geom_line(aes(hour, NHWhite, color = "NHWhite"), linetype="dashed", size=.5 ) +
  geom_point(aes(hour, NHWhite, color = "NHWhite"),size=1) +
  scale_y_continuous(
    name = "White", 
    sec.axis = sec_axis(~ . *3/50  , name = "Non-White")) + 
  theme(panel.background = element_rect(fill= "white", color = "black", 
     linetype = "solid"), legend.position="top")  + 
  labs(y = "Number of Overdose Responses",
       x = "Hour of Day")  + scale_colour_manual(values = colors)  + scale_color_discrete(name = "Race/Ethnicity", 
      labels=c("Latino", "Non-Hispanic Black", "Non-Hispanic White")) +
  ggtitle("Hourly Distribution of Drug Overdose Responses by Race/Ethnicity") 
  
d<- unique_incidents_victims %>%
  select(DT, race_eth) %>%
  dplyr::group_by(race = race_eth, month = month(DT)) %>% 
  dplyr::count()  %>% 
  tidyr::spread(race, n)

ggplot(d, aes(x=month)) +
  geom_line(aes(month, Hisp*50/3, color = "Hisp"), size=.8, linetype="dashed", size=.5) +
  geom_point(aes(month, Hisp*50/3, color = "Hisp"),size=1) +
  geom_line(aes(month, NHBlack*50/3, color = "NHBlack"),  linetype="dashed", size=.5) +
  geom_point(aes(month, NHBlack*50/3,  color = "NHBlack"),size=1) +
  geom_line(aes(month, NHWhite, color = "NHWhite"), linetype="dashed", size=.5 ) +
  geom_point(aes(month, NHWhite, color = "NHWhite"),size=1) +
  scale_y_continuous(
    name = "White", 
    sec.axis = sec_axis(~ . *3/50  , name = "Non-White")) + 
  theme(panel.background = element_rect(fill= "white", color = "black", 
                                        linetype = "solid"), legend.position="top")  + 
  labs(y = "Number of Overdose Responses",
       x = "Month of Year")  + scale_colour_manual(values = colors)  + scale_color_discrete(name = "Race/Ethnicity", 
  labels=c("Latino", "Non-Hispanic Black", "Non-Hispanic White")) + scale_x_continuous(breaks=c(1:12)) +
  ggtitle("Monthly Distribution of Drug Overdose Responses by Race/Ethnicity") 

round(prop.table(table(unique_incidents_victims$race_eth)),3)
round(prop.table(table(unique_incidents_victims$age)),3)

unique_incidents_victims %>% 
  mutate(wday = wday(DT, label = TRUE)) %>% 
  ggplot(aes(x = wday)) +
  geom_bar()

unique_incidents_victims %>% #This is interesting but useless
  mutate(minute = minute(DT)) %>% 
  group_by(minute) %>% 
  summarise(
    avg_dose = mean(Dose.Unit, na.rm = TRUE),
    n = n()) %>% 
  ggplot(aes(minute, avg_dose)) +
  geom_line()

unique_incidents_victims %>% 
  filter(race_eth=="NHWhite") %>% 
  mutate(month = month(DT, label = TRUE)) %>% 
  ggplot(aes(x = month)) +
  geom_bar()

unique_incidents_victims %>% 
  filter(race_eth=="NHBlack") %>% 
  mutate(month = month(DT, label = TRUE)) %>% 
  ggplot(aes(x = month)) +
  geom_bar()

unique_incidents_victims %>% 
  filter(race_eth=="Hisp") %>% 
  mutate(month = month(DT, label = TRUE)) %>% 
  ggplot(aes(x = month)) +
  geom_bar()

unique_incidents_victims %>% 
  filter(race_eth=="NHWhite") %>% 
  count(week = floor_date(DT, "week")) %>% 
  ggplot(aes(week, n)) +
  geom_line() 

unique_incidents_victims %>% 
  filter(race_eth=="NHBlack") %>% 
  count(week = floor_date(DT, "week")) %>% 
  ggplot(aes(week, n)) +
  geom_line() 

unique_incidents_victims %>% 
  filter(race_eth=="Hisp") %>% 
  count(week = floor_date(DT, "week")) %>% 
  ggplot(aes(week, n)) +
  geom_line() 

unique_incidents_victims %>% 
  filter(race_eth=="NHWhite") %>% 
  mutate(hour = hour(DT)) %>% 
  ggplot(aes(x = hour)) +
  geom_bar()

unique_incidents_victims %>% 
  filter(race_eth=="NHBlack") %>% 
  mutate(hour = hour(DT)) %>% 
  ggplot(aes(x = hour)) +
  geom_bar()

unique_incidents_victims %>% 
  filter(race_eth=="Hisp") %>% 
  mutate(hour = hour(DT)) %>% 
  ggplot(aes(x = hour)) +
  geom_bar()


unique_incidents_victims$age_num <- as.numeric(as.character(unique_incidents_victims$age))

mod1 <- glm(Survived ~ Naloxone_Admin + age_num + hour +  month + year +
               heroin + pharma + fentanyl + Gender.Desc + white + black + black*Naloxone_Admin + white*Naloxone_Admin +
               Incident.County.Latitude+Victim.County.Longitude , family='binomial', data=unique_incidents_victims)
summary(mod1)
confint(mod1)
round(exp(coef(mod1)-1)*100,4)

pR2(mod1)
length(residuals(mod1)) #get number of observations
mod1_margins <- margins(mod1)

cplot(mod1, "age_num", what="predict", draw=TRUE)

allEffects(mod1)

plot(effect("white", mod1))
plot(effect("age_num", mod1))
mod_hisp <- glm(Survived ~ Naloxone_Admin * Gender.Desc * hisp, family='binomial', data=unique_incidents_victims)
no_naloxone_male = data.frame("hisp"=1, "Naloxone_Admin"=0, "Gender.Desc" = "Male")
naloxone_male = data.frame("hisp"=1, "Naloxone_Admin"=1, "Gender.Desc" = "Male")

no_naloxone_female = data.frame("hisp"=1, "Naloxone_Admin"=0, "Gender.Desc" = "Female")
naloxone_female = data.frame("hisp"=1, "Naloxone_Admin"=1, "Gender.Desc" = "Female")

predict(mod_hisp, no_naloxone_male, type = "response")
predict(mod_hisp, naloxone_male, type = "response")
predict(mod_hisp, no_naloxone_female, type = "response")
predict(mod_hisp, naloxone_female, type = "response")

mod_white <- glm(Survived ~  Naloxone_Admin * Gender.Desc * white , family='binomial', data=unique_incidents_victims)
no_naloxone_male = data.frame("white"=1, "Naloxone_Admin"=0, "Gender.Desc" = "Male")
naloxone_male = data.frame("white"=1, "Naloxone_Admin"=1, "Gender.Desc" = "Male")

no_naloxone_female = data.frame("white"=1, "Naloxone_Admin"=0, "Gender.Desc" = "Female")
naloxone_female = data.frame("white"=1, "Naloxone_Admin"=1, "Gender.Desc" = "Female")

predict(mod_white, no_naloxone_male, type = "response")
predict(mod_white, naloxone_male, type = "response")
predict(mod_white, no_naloxone_female, type = "response")
predict(mod_white, naloxone_female, type = "response")

#Note, for plots the order in which variables enter matters
mod_black <- glm(Survived ~ Naloxone_Admin *  Gender.Desc * black , family='binomial', data=unique_incidents_victims)
no_naloxone_male = data.frame("black"=1, "Naloxone_Admin"=0, "Gender.Desc" = "Male")
naloxone_male = data.frame("black"=1, "Naloxone_Admin"=1, "Gender.Desc" = "Male")

no_naloxone_female = data.frame("black"=1, "Naloxone_Admin"=0, "Gender.Desc" = "Female")
naloxone_female = data.frame("black"=1, "Naloxone_Admin"=1, "Gender.Desc" = "Female")

predict(mod_black, no_naloxone_male, type = "response")
predict(mod_black, naloxone_male, type = "response")
predict(mod_black, no_naloxone_female, type = "response")
predict(mod_black, naloxone_female, type = "response")


theme_set(theme_sjplot())
plot_model(mod_white, type = "pred", terms = c("Naloxone_Admin", "white"))
plot_model(mod_black, type = "pred", terms = c("Naloxone_Admin", "black"))
plot_model(mod_hisp, type = "pred", terms = c("Naloxone_Admin", "hisp"))

plot_model(mod_hisp, type = "int")
plot_model(mod_white, type = "int")
plot_model(mod_black, type = "int")
#######################
x<-aggregate(Incident.ID ~ Incident.County.Name, FUN = length, data = unique_incidents_victims)
x_black<-aggregate(Incident.ID ~ Incident.County.Name, FUN = length, data = subset(unique_incidents_victims, black==1))
x_white<-aggregate(Incident.ID ~ Incident.County.Name, FUN = length, data = subset(unique_incidents_victims, white==1))
x_hisp<-aggregate(Incident.ID ~ Incident.County.Name, FUN = length, data = subset(unique_incidents_victims, hisp==1))


pop_dat <- read.csv("/Users/gbarboza/Desktop/ODIN_PA/pop.csv")
x <- merge(x_black, x, by = "Incident.County.Name", all.y = TRUE)
x <- merge(x_white, x, by = "Incident.County.Name", all.y = TRUE)
x <- merge(x_hisp, x, by = "Incident.County.Name", all.y = TRUE)

names(x)[1] <- "NAME"
names(x)[2] <- "nOD_hisp"
names(x)[3] <- "nOD_white"
names(x)[4] <- "nOD_black"
names(x)[5] <- "nOD_all"

x[is.na(x)] = 0


overdose_rates <- merge(x, pop_dat, by = "NAME")

overdose_rates$nOD_all_rate <- ((overdose_rates$nOD_all/overdose_rates$ACS_17_5YR)*100000)/2
overdose_rates$nOD_hisp_rate <- ((overdose_rates$nOD_hisp/overdose_rates$pophisp)*100000)/2
overdose_rates$nOD_white_rate <- ((overdose_rates$nOD_white/overdose_rates$popwhite)*100000)/2
overdose_rates$nOD_black_rate <- ((overdose_rates$nOD_black/overdose_rates$popblack)*100000)/2

penn <- readOGR("/Users/gbarboza/Desktop/OD in PA/", "county demographics")
plot(penn)

overdose_rates <- overdose_rates %>% mutate(urban = factor(urban, levels = c(0, 1), labels = c("Rural", "Urban")))

gd <- overdose_rates %>% 
  group_by(urban) %>% 
  summarise(nOD_all_rate = mean(nOD_all_rate))

ggplot(overdose_rates, aes(x = urban, y = nOD_all_rate, color = urban, fill = urban)) +
  geom_bar(data = gd, stat = "identity", alpha = .3) +
  ggrepel::geom_text_repel(aes(label = overdose_rates$NAME), color = "black", size = 2.5, segment.color = "grey") +
  geom_point() +
  guides(color = "none", fill = "none") +
  theme_bw() +
  labs(
    title = "Heroin Overdose Response Rate by Urbanicity",
    subtitle = "Source: Pennsylvania Overdose Information Network (2018 - 2019)",
    x = "Region",
    y = "OD RATE"
  )

# We create the data foundation from the shapefile
penn@data$id = rownames(penn@data)
penn.points = fortify(penn, region="id")
penn.df = join(penn.points, penn@data, by="id")
# We merge with churn regional data
merged <- merge(penn.df, overdose_rates, by = "NAME")
merged <- merged[order(merged$order), ]

no_classes <- 5
labels <- c()

#all

quantiles <- quantile(merged$nOD_all_rate, probs = seq(0, 1, length.out = no_classes + 1))

for(idx in 1:length(quantiles)){
  labels <- c(labels, paste0(round(quantiles[idx], 2), 
                             " – ", 
                             round(quantiles[idx + 1], 2)))
}
labels <- labels[1:length(labels)-1]

merged$OD_all_quantiles <- cut(merged$nOD_all_rate, 
breaks = quantiles, 
labels = labels, 
include.lowest = T)


p <- ggplot() +
  geom_polygon(data = merged, aes(fill = OD_all_quantiles, 
                                    x = long, 
                                    y = lat, 
                                    group = group)) +
  coord_equal() +
  labs(x = NULL, 
       y = NULL, 
       title = "Spatial Distribution of Overdose Response Rates", 
       subtitle = "Pennsylvania Counties", 
       caption = "Source: Pennsylvania Overdose Information Network (2018-2019)") +
  # now the discrete-option is used, 
  # and we use guide_legend instead of guide_colourbar
  scale_fill_viridis(
    option = "cividis",
    name = "Overdose Response Rate",
    discrete = T,
    direction = -1,
    guide = guide_legend(
      keyheight = unit(5, units = "mm"),
      title.position = 'top',
      reverse = T
    ))
p


#white
# We create the data foundation from the shapefile
penn@data$id = rownames(penn@data)
penn.points = fortify(penn, region="id")
penn.df = join(penn.points, penn@data, by="id")
# We merge with churn regional data
merged <- merge(penn.df, overdose_rates, by = "NAME")
merged <- merged[order(merged$order), ]

no_classes <- 8
labels <- c()

quantiles <- quantile(merged$nOD_white_rate, probs = seq(0, 1, length.out = no_classes + 1))

for(idx in 1:length(quantiles)){
  labels <- c(labels, paste0(round(quantiles[idx], 2), 
                             " – ", 
                             round(quantiles[idx + 1], 2)))
}
labels <- labels[1:length(labels)-1]

merged$OD_white_quantiles <- cut(merged$nOD_white_rate, 
                               breaks = quantiles, 
                               labels = labels, 
                               include.lowest = T)


p <- ggplot() +
  geom_polygon(data = merged, aes(fill = OD_white_quantiles, 
                                  x = long, 
                                  y = lat, 
                                  group = group)) +
  coord_equal() +
  labs(x = NULL, 
       y = NULL, 
       title = "Spatial Distribution of Overdose Response Rates [Whites]", 
       subtitle = "Pennsylvania Counties", 
       caption = "Source: Pennsylvania Overdose Information Network (2018-2019)") +
  # now the discrete-option is used, 
  # and we use guide_legend instead of guide_colourbar
  scale_fill_viridis(
    option = "cividis",
    name = "Overdose Response Rate [Whites]",
    discrete = T,
    direction = -1,
    guide = guide_legend(
      keyheight = unit(5, units = "mm"),
      title.position = 'top',
      reverse = T
    ))
p

#black

# We create the data foundation from the shapefile
penn@data$id = rownames(penn@data)
penn.points = fortify(penn, region="id")
penn.df = join(penn.points, penn@data, by="id")
# We merge with churn regional data
merged <- merge(penn.df, overdose_rates, by = "NAME")
merged <- merged[order(merged$order), ]

no_classes <- 10
labels <- c()

quantiles <- unique(quantile(merged$nOD_black_rate, probs = seq(0, 1, length.out = no_classes + 1)))

for(idx in 1:length(quantiles)){
  labels <- c(labels, paste0(round(quantiles[idx], 2), 
                             " – ", 
                             round(quantiles[idx + 1], 2)))
}
labels <- labels[1:length(labels)-1]

merged$OD_black_quantiles <- cut(merged$nOD_black_rate, 
                                 breaks = quantiles, 
                                 labels = labels, 
                                 include.lowest = T)


p <- ggplot() +
  geom_polygon(data = merged, aes(fill = OD_black_quantiles, 
                                  x = long, 
                                  y = lat, 
                                  group = group)) +
  coord_equal() +
  labs(x = NULL, 
       y = NULL, 
       title = "Spatial Distribution of Overdose Response Rates [Black]", 
       subtitle = "Pennsylvania Counties", 
       caption = "Source: Pennsylvania Overdose Information Network (2018-2019)") +
  # now the discrete-option is used, 
  # and we use guide_legend instead of guide_colourbar
  scale_fill_viridis(
    option = "cividis",
    name = "Overdose Response Rate [Black]",
    discrete = T,
    direction = -1,
    guide = guide_legend(
      keyheight = unit(5, units = "mm"),
      title.position = 'top',
      reverse = T
    ))
p

#hisp

# We create the data foundation from the shapefile
penn@data$id = rownames(penn@data)
penn.points = fortify(penn, region="id")
penn.df = join(penn.points, penn@data, by="id")
# We merge with churn regional data
merged <- merge(penn.df, overdose_rates, by = "NAME")
merged <- merged[order(merged$order), ]

no_classes <- 14
labels <- c()

quantiles <- unique(quantile(merged$nOD_hisp_rate, probs = seq(0, 1, length.out = no_classes + 1)))

for(idx in 1:length(quantiles)){
  labels <- c(labels, paste0(round(quantiles[idx], 2), 
                             " – ", 
                             round(quantiles[idx + 1], 2)))
}
labels <- labels[1:length(labels)-1]

merged$OD_hisp_quantiles <- cut(merged$nOD_hisp_rate, 
                                 breaks = quantiles, 
                                 labels = labels, 
                                 include.lowest = T)


p <- ggplot() +
  geom_polygon(data = merged, aes(fill = OD_hisp_quantiles, 
                                  x = long, 
                                  y = lat, 
                                  group = group)) +
  coord_equal() +
  labs(x = NULL, 
       y = NULL, 
       title = "Spatial Distribution of Overdose Response Rates [Latinos]", 
       subtitle = "Pennsylvania Counties", 
       caption = "Source: Pennsylvania Overdose Information Network (2018-2019)") +
  # now the discrete-option is used, 
  # and we use guide_legend instead of guide_colourbar
  scale_fill_viridis(
    option = "cividis",
    name = "Overdose Response Rate [Latinos]",
    discrete = T,
    direction = -1,
    guide = guide_legend(
      keyheight = unit(5, units = "mm"),
      title.position = 'top',
      reverse = T
    ))
p


