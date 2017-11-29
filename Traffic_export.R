setwd("D:/Rcodes/Shiny")

#install devtools package for downloading packages from github
# install.packages("devtools")
library(devtools)
#install curl for easier use
# install.packages("curl")
library(curl)
#installing rga package from github
# install_github("skardhamar/rga")
library(rga)
library(lubridate)
library(zoo)
library(dplyr)


#### Takes token file for GA ###
rga.open(instance = "ga", where="D:/Rcodes/Shiny/ga.rga")
A<-as.yearmon(Sys.Date())


Br_web<-ga$getData(118205422,start.date = "2016-09-01",end.date = Sys.Date(),dimensions="ga:yearMonth",metrics = "ga:users")
Br_web$country<-"Brazil"
Id_web<-ga$getData(102733486,start.date = "2016-09-01",end.date = Sys.Date(),dimensions="ga:yearMonth",metrics = "ga:users")
Id_web$country<-"Indonesia"
Ph_web<-ga$getData(96537958,start.date = "2016-09-01",end.date = Sys.Date(),dimensions="ga:yearMonth",metrics = "ga:users")
Ph_web$country<-"Philippines"
Sg_web<-ga$getData(83567831,start.date = "2016-09-01",end.date = Sys.Date(),dimensions="ga:yearMonth",metrics = "ga:users")
Sg_web$country<-"Singapore"

Web_traffic<-rbind(Br_web,Id_web,Ph_web,Sg_web)

Web_traffic$month_year<-as.Date(paste0(substr(Web_traffic$yearMonth,1,4),"-",substr(Web_traffic$yearMonth,5,7),"-01"))

Web_traffic$yearMonth<-NULL

##AMP
amp<-ga$getData(152853517,
               start.date = as.Date(A)%m-%months(12),
               end.date = Sys.Date(),
               filter = "ga:country == Singapore,ga:country == Brazil,ga:country == Philippines,ga:country == Indonesia",
               dimensions = "ga:country,ga:yearMonth",
               metrics="ga:users", 
               max = 10000
)
amp$month_year<-as.Date(paste0(substr(amp$yearMonth,1,4),"-",substr(amp$yearMonth,5,7),"-01"))

amp$yearMonth<-NULL



## android

an<-ga$getData(81232466,
               start.date = as.Date(A)%m-%months(12),
               end.date = Sys.Date(),
               filter = "ga:country == Singapore,ga:country == Brazil,ga:country == Philippines,ga:country == Indonesia",
               dimensions = "ga:country,ga:yearMonth",
               metrics="ga:users", 
               max = 10000
)
an$month_year<-as.Date(paste0(substr(an$yearMonth,1,4),"-",substr(an$yearMonth,5,7),"-01"))

##IOS

ios<-ga$getData(95398628,
                start.date = as.Date(A)%m-%months(12),
                end.date = Sys.Date(),
                filter = "ga:country == Singapore,ga:country == Brazil,ga:country == Philippines,ga:country == Indonesia",
                dimensions = "ga:country,ga:yearMonth",
                metrics="ga:users", 
                max = 10000
)
ios$month_year<-as.Date(paste0(substr(ios$yearMonth,1,4),"-",substr(ios$yearMonth,5,7),"-01"))

an$yearMonth<-NULL
an$Source<-"Android-app"
ios$yearMonth<-NULL
ios$Source<-"ios-app"
amp$Source<-"AMP"
# web_agg<-Web_trafic %>%
#   mutate(month_year= as.yearmon(date)) %>%
#   group_by(month_year,country) %>%
#   summarise(users1 =sum(users)) %>%
#   select(month_year,users1,country) %>%
#   ungroup() 
Web_traffic$Source<-"Web"

traffic<-rbind(Web_traffic,ios,an,amp)

write.csv(traffic,"D:/Rcodes/Shiny/traffic.csv")
