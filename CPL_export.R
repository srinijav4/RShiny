library(googlesheets)
library(lucr)
library(dplyr)

gs_ls()
cpl1<-gs_title("Mid-Market Ad Launch")
gs_ws_ls(cpl1)
S_cpl<- gs_read(ss=cpl1,ws="Singapore CPL",range = "A1:U100")
ID_PH_cpl<- gs_read(ss=cpl1,ws="ID & PH- CPL", range = "A1:V500")



s = 47.36
p = 1.25
i=0.0048

Scpl1<- S_cpl %>%
  mutate(monthyear = substr(`Date of entry`,4,10)) %>%
  mutate(Amount = as.numeric(`Invoice Amount (Including Tax) (To be filled by Naval)`)) %>%
  mutate(Amount_inr = Amount*s) %>%
            group_by(City,monthyear) %>%
           summarise(Revenue= sum(Amount_inr),Orders=length(`Name of establishment`)) %>%
  select(City,monthyear,Revenue,Orders) %>%
           ungroup()

cpl2<- ID_PH_cpl %>%
  mutate(monthyear = substr(`Date of entry`,4,10)) %>%
  mutate(Amount = as.numeric(`Initial Wallet Amount`)) %>%
  mutate(Amount_inr = ifelse(City=="Jakarta", Amount*i, Amount*p)) %>%
  group_by(City,monthyear) %>%
  summarise(Revenue= sum(Amount_inr),Orders=length(`Name of establishment`)) %>%
  select(City,monthyear,Revenue,Orders) %>%
  ungroup()

CPL<-rbind(Scpl1,cpl2)
CPL$monthyear<-paste0(substr(CPL$monthyear,4,7),"-",substr(CPL$monthyear,1,2),"-01")
CPL$Revenue<-round(CPL$Revenue/100000,2)

##export CSV

write.csv(CPL,"D:/Rcodes/Shiny/cpl.csv")
