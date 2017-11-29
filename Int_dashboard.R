# setwd("C:/Users/srinija/Dropbox/Orders")

library(lubridate)
library(zoo)

# library(reshape)

net_orders <- read.csv("./Order(Log).csv", stringsAsFactors = F)
net_orders_i <- subset(net_orders, net_orders$Country != "India" & net_orders$Country != "-")

########################################################## ONLY FOR Present Month -----

p <- as.Date('2017-09-01') ##First day of current month
q<-3 ## start day of the current month for considering cancellations
Y<-'2017' ## cancellation year
r<-as.Date('2017-10-02') ##(next month end day for considering cancellations)


## Gross----
gross_orders_march <- subset(net_orders_i, as.yearmon(net_orders_i$placed_at) == as.yearmon(p) & net_orders_i$Removal_Date == "-")

report <- data.frame(unique(net_orders_i$Country), stringsAsFactors = F)
colnames(report) <- c("Country")

gross_orders_march <- subset(gross_orders_march, gross_orders_march$original_duration_months%%3 == 0)
gross_orders_march <- subset(gross_orders_march, grepl('Ray',gross_orders_march$Master_Plan_2) | grepl('Fabric', gross_orders_march$Master_Plan_2))

##Ray New Acquistions(Gross)

temp<-subset(gross_orders_march,gross_orders_march$HUR_Flag=="Hunt" & grepl('Ray',gross_orders_march$Master_Plan_2))

tryCatch({temp<-aggregate(practice_id~Country, data = temp, FUN = function(x) length(unique(x)))},error=function(e){print("empty dataframe orders_country")})
colnames(temp) <- c("x", "freq")

report$Ray_New_Acq<-temp$freq[match(report$Country,temp$x)]


##Reach Slots

temp<-subset(gross_orders_march, grepl('Fabric',gross_orders_march$Master_Plan_2))
tryCatch({temp<-aggregate(practice_id~Country, data = temp, FUN = length)},error=function(e){print("empty dataframe reachorders_country")})
colnames(temp) <- c("x", "freq")

report$Reach_slots<-temp$freq[match(report$Country,temp$x)]

##temporary MasterPlan column

gross_orders_march$MasterPlan<-"Fabric"

gross_orders_march$MasterPlan[gross_orders_march$Master_Plan_2 != "Fabric"] <- "Ray"



##Gross billings

tryCatch({temp <- aggregate(Revenue~MasterPlan+Country, data = gross_orders_march, FUN = sum)},error=function(e){print("empty dataframe gross revenue country")})
temp_ray <- subset(temp, temp$MasterPlan == "Ray")
report$ray_revenue <- temp_ray$Revenue[match(report$Country, temp_ray$Country)]
temp_fabric <- subset(temp, temp$MasterPlan == "Fabric")
report$reach_revenue <- temp_fabric$Revenue[match(report$Country, temp_fabric$Country)]
report[is.na(report)] <- 0
report$total_revenue <- report$ray_revenue + report$reach_revenue



##Cancellations----

net_orders_i<-subset(net_orders_i,net_orders_i$Removal_Date != "-")
cn_orders_march <- subset(net_orders_i,
                           ((substr(net_orders_i$Removal_Date, 6, 7) == paste0("0",month(p)) &  day(as.Date(net_orders_i$Removal_Date))>q)
                            | (substr(net_orders_i$Removal_Date, 6, 7) == paste0("0",month(r)) & day(as.Date(net_orders_i$Removal_Date))< day(r)))
                          # & substr(net_orders_i$Removal_Date, 1, 4) == year(r) 
                          & month(net_orders_i$placed_at) != month(p)
                          & year(as.Date(net_orders_i$Removal_Date))==Y)
cn_orders_march <- subset(cn_orders_march, grepl('Ray',cn_orders_march$Master_Plan_2) | grepl('Fabric', cn_orders_march$Master_Plan_2)) 
cn_orders_march$MasterPlan<-"Fabric"
cn_orders_march$MasterPlan[grepl('Ray',cn_orders_march$Master_Plan_2)]<-"Ray"

## cancelled ray accounts hunts
temp<-subset(cn_orders_march,cn_orders_march$MasterPlan=="Ray" & cn_orders_march$HUR_Flag=="Hunt")
tryCatch({temp<-aggregate(practice_id~Country, data = temp, FUN = function(x) length(unique(x)))},error=function(e){print("no Ray hunts cancelled orders")})
         
report$ray_accounts_cn <- temp$practice_id[match(report$Country, temp$Country)]

## cancelled reach slots
temp<-subset(cn_orders_march,cn_orders_march$MasterPlan=="Fabric")
tryCatch({temp<-aggregate(practice_id~Country, data = temp, FUN = length)},error=function(e){print("no Reach slots cancelled")})

report$reach_slots_cn <- temp$practice_id[match(report$Country, temp$Country)]


##billings cancelled

temp<-subset(cn_orders_march,cn_orders_march$MasterPlan=="Ray" | cn_orders_march$MasterPlan=="Fabric")
tryCatch({temp<-aggregate(Revenue~Country+MasterPlan, data = temp, FUN = sum)},error=function(e){print("no cancellations")})

temp1<-subset(temp,temp$MasterPlan=="Ray")
report$ray_cn<-temp1$Revenue[match(report$Country,temp1$Country)]

temp1<-subset(temp,temp$MasterPlan== "Fabric")
report$reach_cn<-temp1$Revenue[match(report$Country,temp1$Country)]

report[is.na(report)]<-0

report$total_cn<-report$ray_cn+report$reach_cn

##Net----

report$Ray_Net<-report$ray_revenue-report$ray_cn
report$Reach_Net<-report$reach_revenue-report$reach_cn
report$Total_Net<-report$Ray_Net+report$Reach_Net


##Trend past##----

##International----

Int_orders <- read.csv("./Order(Log).csv", stringsAsFactors = F)
# Int_orders<- subset(Int_orders, Int_orders$Country != "India" & Int_orders$Country != "-")
Int_orders<-subset(Int_orders,Int_orders$Removal_Date == "-")


# Int_orders<-subset(Int_orders,Int_orders$original_duration_months%%3==0 & Int_orders$original_duration_months!=0) 
Int_orders$MasterPlan<-"Reach"
Int_orders$MasterPlan[!grepl('fabric',tolower(Int_orders$Master_Plan_2))]<-"Ray"
Int_orders$month_name<-(as.yearmon(Int_orders$placed_at))
Int_orders$per_month<-Int_orders$Revenue*30.5/Int_orders$duration_days
Int_orders$dur_actual<- Int_orders$duration_days/30.5
Int_orders[is.na(Int_orders)]<-0


Int_Ray<-subset(Int_orders,Int_orders$MasterPlan=="Ray")
Int_Reach<-subset(Int_orders,Int_orders$MasterPlan=="Reach")

## Reach_Master_Orders

Master_Reach<-read.csv("./Reach_Orders.csv",stringsAsFactors = F)
 
Int_Reach$tag<-Master_Reach$tag[match(Int_Reach$order_id,Master_Reach$order_id)]
Int_Reach$Renewal_subid<-Master_Reach$Renewal_subid[match(Int_Reach$order_id,Master_Reach$order_id)]
Int_Reach$Renewal_date<-Master_Reach$Renewal_date[match(Int_Reach$order_id,Master_Reach$order_id)]



##Cleared orders

# sum(Int_orders$Revenue)
# 
# C1<-Int_orders %>%
#   group_by(month_name,MasterPlan) %>%
#   # mutate(HURFlag,Int_orders$HUR_Flag !=  )
#   filter(MasterPlan=="Ray")%>%
#   summarise(Cleared_Revenue = sum(Revenue)) %>%
#   select(month_name, MasterPlan, Cleared_Revenue) %>%
#   ungroup()


# filter(country %in% input$country) %>%


