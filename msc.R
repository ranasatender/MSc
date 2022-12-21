###### BRIEF DESCRIPTION OF THE PROJECT

###### MSc Dissertation

###### TITLE: Impact of Social Capital on Social Distancing and Spread 
######        of COVID-19 in India

###### Key Variables: 
###### Social Capital (proxy Voter Turnout; No. of SHGs; Blood Donation)
###### Social Distancing: Social Mobility to different places during COVID-19
###### COVID-19 cases and deaths

###### set directory

setwd("C:/Users/ranas/OneDrive/28 Aug 2019/Desktop/IFPRI")

###### Recall package

library(tidyverse)

###### Upload Google Community Mobility Report data of year 2020

data_mob<-read.csv("2020_IN_Region_Mobility_Report.csv")

###### Check data structure

glimpse(data_mob)

###### Change date format from character to date format

library(lubridate)

data_mob<-data_mob%>%
  mutate(date=dmy(date))

###### Check data structure

glimpse(data_mob)

###### Shorten the variables' name by renaming

data_mob<-data_mob%>%
  rename(retail=retail_and_recreation_percent_change_from_baseline,
         grocery=grocery_and_pharmacy_percent_change_from_baseline,
         parks=parks_percent_change_from_baseline,
         transit=transit_stations_percent_change_from_baseline,
         work=workplaces_percent_change_from_baseline,
         reside=residential_percent_change_from_baseline)

###### Upload voter turnout data (social capital proxy)

data_vot<-read.csv("voters_turnout.csv")

###### Check data structure

glimpse(data_vot)

###### import data on other social capital and control variables

data_cont<-read.csv("controls.csv")

###### import data on control variable (gdp per capita)

gdp<-read.csv("gdp.csv")

###### Check data structure

glimpse(data_cont)

###### Change variable (unit of blood collected) from character to numeric 

data_cont$blood_collect<-as.numeric(data_cont$blood_collect)

###### Check data structure

glimpse(data_cont)

###### rename variables' name to bring uniformity

###### ï..state to state

data_cont<-data_cont%>%
  rename(state=ï..state)

###### shorten area.km2 to area

data_cont<-data_cont%>%
  rename(area=area.km2)

###### ï..state to state for gdp data

gdp<-gdp%>%
  rename(state=ï..state)

###### Check data structure

glimpse(data_cont)
glimpse(gdp)

###### merge data

data_mv<-merge(data_mob,data_vot,by="state")

data_mv<-merge(data_mv,data_cont,by="state")

data_mv<-merge(data_mv,gdp,by="state")

glimpse(data_mv)

###### create new variables

###### population density persons per km2

data_mv<-data_mv%>%
  mutate(pop_den=population/area)

###### number of hospitals per 100,000 people

data_mv<-data_mv%>%
  mutate(hosp_lac=(no_hospitals/population)*100000)

###### number of hospital beds per 100,000 people

data_mv<-data_mv%>%
  mutate(bed=(hosp_beds/population)*100000)

###### number of ventilator beds per 100,000 people

data_mv<-data_mv%>%
  mutate(vent=(vent_beds/population)*100000)

###### number of ICU beds per 100,000 people

data_mv<-data_mv%>%
  mutate(icu=(ICU_beds/population)*100000)

###### natural log of gdp per capita (USD)

data_mv<-data_mv%>%
  mutate(log_gdp=log(gdp_capita_usd))

###### units of blood collected per 100,000 people

data_mv<-data_mv%>%
  mutate(bld=(blood_collect/population)*100000)

###### number of SHGs per 100,000 people

data_mv<-data_mv%>%
  mutate(shgm=(shg/population)*100000)

###### summary statistics of data

library(skimr)
library(lfe)
library(huxtable)

###### create a new variable to get specific statistics

sum_stats<-skim_with(numeric = sfl(Mean = mean,
                                   Median = median,
                                   SD = sd,
                                   Min = min,
                                   Max = max),append = FALSE)

###### generate summary statistics

summ_stat<-data_mv%>%
  filter(date<"2020-03-25")%>%
  sum_stats()%>%
  select(-n_missing, -complete_rate)

summ_stat

quick_xlsx(summ_stat,file = "summ_stat.xlsx")


###### CREATE CHOROPLETH MAP TO VISUALIZE GEOGRAPHICAL DISTRIBUTION OF 
###### COVID-19 cases

library(VIM)
library(maps)
library(sf)
library(rgdal)
library(rgeos)

###### IMPORT COVID-19 Data

data_cov20<-read.csv("covid_19_regional.csv")

###### Change variables' name

data_cov20<-data_cov20%>%
  rename(date=ï..date1)

data_cov20<-data_cov20%>%
  mutate(date=dmy(date))

data_20_map<-data_cov20%>%
  filter(date=="2020-03-24")

###### shapefile can be obtained from https://www.diva-gis.org/gdata 

###### Import polygons/shape

sf <- st_read(dsn="shape", layer="IND_adm1")
shape <- readOGR(dsn="shape", layer="IND_adm1")

###### Change variable name NAME_1 to state

sf<-sf%>%
  mutate(state=NAME_1)

###### Merge Shapefile with COVID-19 Data on 24 Mar. 2020

sf<-merge(sf,data_20_map,by="state",all=TRUE,sort=FALSE)

###### Map geographical spread of COVID-19 cases as on 24 Mar. 2020

p1<-sf%>%
  ggplot() + 
  geom_sf(aes(fill = Confirmed)) +
  scale_fill_gradientn(colours = terrain.colors(10))+
  theme_bw()+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        legend.title = element_blank(),
        plot.background = element_rect(fill = "transparent",colour = NA),
        plot.title = element_text(hjust = 0.5))+
  labs(x="",y="",
       title = "Total COVID-19 cases (24 Mar. 20)", size=11)+
  guides(fill=guide_legend(title=""))

###### Print plot p1

p1

###### Create social capital variable

data_mv<-data_mv%>%
  mutate(grp=ifelse(voter_turn_percent>median(voter_turn_percent),
                    "High Social Capital",
                    "Low Social Capital"))

###### plot mobility trend to retail between high and low social capital states

library(ggplot2)

###### Create plot

p2<-data_mv%>%
  filter(date<"2020-06-01")%>%
  group_by(date,grp)%>%
  summarise(retail=mean(retail,na.rm=TRUE))%>%
  ggplot(aes(date,retail,color=grp))+
  geom_line(size = 0.5)+
  scale_x_date(date_breaks = "1 weeks")

###### Drop vertical lines to show lockdown enforcement/uplifting

p2<-p2+geom_vline(xintercept = as.Date(ymd("2020-03-25")),
             linetype="dashed", color = "blue", size=0.5)+
  geom_vline(xintercept = as.Date(ymd("2020-06-01")), linetype="dashed", 
                  color = "darkgreen", size=0.5)

###### Tag vertical lines with title

p2<-p2+geom_text(x=as.Date("2020-03-26"),y=3,label="lockdown",colour="blue",
                 hjust=-0.01,size=4,family = "Garamond")+
  geom_text(x=as.Date("2020-05-24"),y=3,label="Uplifting",colour="darkgreen",
            hjust=-0.01,size=4,family = "Garamond")

###### Change theme

p2<-p2+theme_bw()

###### Edit legend and axis

p2<-p2+theme(legend.position="bottom",
             legend.background = element_rect(),
             legend.title = element_blank(),
             legend.box.background = element_rect(colour = "black",size = 1.2),
             axis.text.x=element_text(angle=45, hjust=1,color = "black"))

###### Add labels

p2<-p2+labs(x="Date",y="Percent Change in Mobility from Baseline",
            title = "Retail and Recreation", size=11)

###### Print plot p2

p2

###### run regression (effect of social capital on mobility to retail+recreation)

reg<-lm(retail~voter_turn_percent:factor(date) +
          pop_den:factor(date) +
          log_gdp:factor(date) +
          hosp_lac:factor(date) +
          bed:factor(date) +
          vent:factor(date) +
          icu:factor(date) +
          factor(date) +
          factor(state):factor(date),
        data = data_mv%>%filter(date<"2020-03-25"))

###### export regression table

library(stargazer)

stargazer(reg,type = "text",out = "reg.txt")

###### Visualize regression estimates

library(jtools)

p3<-effect_plot(reg,pred = voter_turn_percent,interval = TRUE,
            int.type = "confidence",
            y.label = "Percent change in mobility",
            x.label = "Social Capital (Percentage of voter turnout)",
            main.title = "Retail and Recreation")+
  ylim(-15,45)+
  theme_bw()

###### print plot p3

p3

###### create grid of all plots

library(cowplot)

plot_grid(p1,p2,p3,
          ncol=2,labels=LETTERS[1:3],
          label_fontfamily = "serif",
          label_fontface = "plain",
          label_colour = "black")

################ END OF EXCERCISE ################

###### Gathering pg. 152 O'Reilley- A common problem is a dataset where some of the 
###### column names are not names of variables, but values of a variable.

data %>%
  gather(`1999`, `2000`, key = "year", value = "cases")


###### Spreading is the opposite of gathering. 
###### You use it when an observation is scattered across multiple rows.

spread(data, key = type, value = count)