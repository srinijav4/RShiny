Orders1<-read.csv("C:/Users/srinija/Dropbox/Orders/Order(Log).csv", stringsAsFactors = F)
Orders1<-subset(Orders1,Orders1$Country != "India" & Orders1$Country != "-")
write.csv(Orders1,"D:/Rcodes/Shiny/Order(Log).csv")

Orders2<-read.csv("C:/Users/srinija/Dropbox/Master Orders/Reach_Orders.csv",stringsAsFactors = F)
Orders2<-subset(Orders2,Orders2$Country != "India" & Orders2$Country != "-")
write.csv(Orders2,"D:/Rcodes/Shiny/Reach_Orders.csv")