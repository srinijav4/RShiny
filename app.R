# library(rmarkdown)
# 

## Country1 Country2 Country3 Country4 Country5 Country6 Country7
## Reach_HUR1,Reach_HUR2,Reach_HUR3,Reach_HUR4
## Acctype,Acctype2,Acctype3
## MasterPlan1

#####Current month tab----- charts : 1,2a,2b,10
### Trend tab ----- charts: 3,4,5,6,7,8,9,11,12
## traffic and trans charts : 13

## 1. Load Libraries ----
library(htmltools)
library(lubridate)
library(dplyr)
library(reshape)
library(shiny)
library(shinydashboard)
library(ggplot2)
library(zoo)
library(reshape2)
library(Hmisc)
library(DT)
library(extrafont)
# font_import()
# loadfonts(device = "win")
library(ggthemes)
library(htmlwidgets)
library(gtable)
library(grid)
library(plotly)
library(scales)
library(shinyWidgets)
library(sunburstR)
# devtools::install_github('hadley/ggplot2')


##report
##PSW

##sourcing previous Int code----
source('./Int_dashboard.R')




## Values----

total_Net<-round(sum(report$Total_Net)/100000,0)
total_Gross<-round(sum(report$total_revenue)/100000,0)
Total_Acc<-sum(report$Ray_New_Acq)
Total_Slots<-sum(report$Reach_slots)


update_time<-Sys.Date()


##Shiny----

ui <- dashboardPage(
  skin = "red",
  
  dashboardHeader(
    title = "International Dashboard - Current Month Till Date",
    titleWidth = 400,
    disable = F),
  
  dashboardSidebar(
    sidebarMenu(
    menuItem("1. Current Month Billings", tabName = "current",icon = icon("inr"),badgeColor = "red"),
    br(),
    menuItem("2. Metrics Trend", tabName = "nw", icon = icon("user-o"),badgeColor = "red"),
    br(),
    menuItem("3. Marketplace metrics", tabName = "tt", icon = icon("user-o"),badgeColor = "red"),
    br()),
    # tags$b(paste0("Last Updated Time : ",update_time)),
    width = 240,
    disable = F),
  
  dashboardBody(
 ##tab item1----   
    tabItems(
      tabItem(
        tabName = "current",
        fluidRow(
          valueBoxOutput("total_Net", width = 3),
          valueBoxOutput("total_Gross", width = 3),
          valueBoxOutput("Total_Acc", width = 3),
          valueBoxOutput("Total_Slots", width = 3)
        ),
        ##fluid row 1
        fluidRow(
          
          box(width = 12, title = "Billings Breakup", status = "primary", solidHeader = T, collapsible = T,collapsed = T,
              tabBox(width = 14, id = "tabset1",
                     tabPanel("Gross Billings",
              
              column(9, plotlyOutput("Chart1b",width = "100%",height="100%"))),
                     # checkboxGroupInput("type", "Please select ", 
                     #                        choices = unique(data_plot_B$city), 
                     #                        selected = 'Delhi', 
                     #                        inline = T))
                     # tabPanel("Ray and Reach", plotlyOutput("Chart1a")),
                     # tabPanel("Ray",plotlyOutput("Chart1b")),
                     # tabPanel("Reach",plotlyOutput("Chart1c"))
                     
              tabPanel("Net Billings",
                       column(9, plotlyOutput("Chart1a",width = "100%",height="100%"))),
              tabPanel("Cancellations Break-up",
                       checkboxGroupButtons(
                         inputId = "Country6", label = "Please select the Country ",
                         choices = c("Singapore","Philippines","Indonesia","Brazil"),
                         selected = c("Singapore","Philippines","Indonesia","Brazil"),
                         justified = T,size = "sm",
                         status = "danger",
                         checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon("remove", lib ="glyphicon" ))
                       ),
                       column(12,  sunburstOutput("Chart10")))
          )
        )),
        fluidRow(
          box(width = 6, title = "New Acquisitions Breakup", status = "primary", solidHeader = T, collapsible = T,collapsed = T,
          
              column(10, sunburstOutput("Chart2b"))
              
          ),
          box(width = 6,title = "Reach Slots Breakup", status = "primary", solidHeader = T, collapsible =T, collapsed = T,column(10,sunburstOutput("Chart2a"))
          )
        )
        ),
 ##tab item2----
          tabItem(
        tabName = "nw",
         ## Cleared Billings Trend
        fluidRow(
          box(width=12,title="Reveneue Achievement (Cleared Revenue)",status="primary",solidHeader=T,collapsible=T,collapsed=T,
              column(12,checkboxGroupButtons(
                inputId = "Country3", label = "Please select the Country ",
                choices = c("Brazil","Indonesia","Philippines","Singapore"),
                selected = c("Indonesia"),
                justified = T,size = "sm",
                status = "danger",
                checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon("remove", lib ="glyphicon" ))
              )),
              column(12, plotlyOutput("Chart7",width = "100%" , height = "100%"))
              
          )),
        ## Ray Acquistions Trend
        fluidRow(
          box(width=12,title="Ray Acquisitions",status="primary",solidHeader=T,collapsible=T,collapsed=T,
              column(12,checkboxGroupButtons(
                inputId = "Country4", label = "Please select the Country ",
                choices = c("Brazil","Indonesia","Philippines","Singapore"),
                selected = c("Indonesia"),
                justified = T,size = "sm",
                status = "danger",
                checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon("remove", lib ="glyphicon" ))
              )),
              column(12, plotlyOutput("Chart8",width = "100%" , height = "100%"))
              
          )),
        ## Reach Slots Trend
       fluidRow(
          box(width=12,title="Reach Slots Sales",status="primary",solidHeader=T,collapsible=T,collapsed=T,
                column(12,checkboxGroupButtons(
                inputId = "Reach_HUR3", label = "Select the HUR Type", 
                
                choices = c("Hunt","Renew","Up-sell"),
                selected = c("Hunt","Up-sell"),
                justified = T,size = "sm",
                status = "danger",
                checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon("remove", lib ="glyphicon" ))
              )
              ),
              
              column(12, plotlyOutput("Chart9",width = "100%" , height = "100%"))
              
          )),
        
        ## Reach slot duration trend 
        
        fluidRow(
         box(width=12,title="Reach Slots duration Break up",status="primary",solidHeader=T,collapsible=T,collapsed=T,
             column(12,checkboxGroupButtons(
               inputId = "Country1", label = "Please select the Country ",
               choices = c("Brazil","Indonesia","Philippines","Singapore"),
               selected = c("Brazil"),
               justified = T,size = "sm",
               status = "danger",
               checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon("remove", lib ="glyphicon" ))
                    )),
               column(12,checkboxGroupButtons(
                 inputId = "Reach_HUR1", label = "Select the HUR Type", 
                 
                 choices = c("Hunt","Renew","Up-sell"),
                 selected = c("Hunt","Up-sell"),
                 justified = T,size = "sm",
                 status = "danger",
                 checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon("remove", lib ="glyphicon" ))
               )
             ),
             column(12, plotlyOutput("Chart3",width = "100%" , height = "100%"))
          
          )),
        
        ##Ray  Duration Trend
        
        fluidRow(
          box(width=12,title="Ray duration Break up",status="primary",solidHeader=T,collapsible=T,collapsed=T,
              column(12,checkboxGroupButtons(
                                inputId = "Country2", label = "Please select the Country ", 
                                choices = c("Brazil","Indonesia","Philippines","Singapore"),
                                selected = c("Brazil"),
                                justified = T,size = "sm",
                                status = "danger",
                                checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon("remove", lib ="glyphicon" ))
                              )),
              column(12,checkboxGroupButtons(
                inputId = "Acctype", label = "Please select the HUR Flag ", 
                choices = c("New Acquistions","Field and Early Renewals"),
                selected = c("New Acquistions"),
                justified = T,size = "sm",
                status = "danger",
                checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon("remove", lib ="glyphicon" ))
              )),
                              column(12, plotlyOutput("Chart4")))
                    
          ),
 
 ## Reach ARPS Trend
 fluidRow(
   box(width=12,title="Reach ARPS",status="primary",solidHeader=T,collapsible=T,collapsed=T,
     
       column(12,checkboxGroupButtons(
         inputId = "Reach_HUR2", label = "Select the HUR Type ", 
         choices = c("Hunt","Renew","up-sell"),
         selected = c("Hunt"),
         justified = T,size = "sm",
         status = "danger",
         checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon("remove", lib ="glyphicon" ))
       )
       ),
       column(12, plotlyOutput("Chart5",width = "100%" , height = "100%"))
       
   )
   ),
 
 ##RAY ARPU
 fluidRow(
   box(width=12,title="Ray ARPU",status="primary",solidHeader=T,collapsible=T,collapsed=T,
         column(12,checkboxGroupButtons(
         inputId = "Acctype2", label = "Select the HUR Flag ", 
         
         choices = c("New Acquistions","Field and Early Renewals"),
         selected = c("New Acquistions"),
         justified = T,size = "sm",
         status = "danger",
         checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon("remove", lib ="glyphicon" ))
       )
       ),
       column(12, plotlyOutput("Chart6",width = "100%" , height = "100%"))
       
   )),
 ## Average Duration-Ray
 fluidRow(
   box(width=12,title="Average Duration",status="primary",solidHeader=T,collapsible=T,collapsed=T,
       tabBox(width = 14, id = "tabset2",
              tabPanel("Ray",
         column(12,checkboxGroupButtons(
           inputId = "Acctype3", label = "Please select the HUR Flag ", 
           choices = c("New Acquistions","Field and Early Renewals"),
         selected = c("New Acquistions"),
         justified = T,size = "sm",
         status = "danger",
         checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon("remove", lib ="glyphicon" ))
       )),
       column(12, plotlyOutput("Chart11",width = "100%" , height = "100%"))
       
   ),
   tabPanel("Reach",
            column(12,checkboxGroupButtons(
              inputId = "Reach_HUR4", label = "Select the HUR Type ", 
              choices = c("Hunt","Renew","up-sell"),
              selected = c("Hunt","Renew","up-sell"),
              justified = T,size = "sm",
              status = "danger",
              checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon("remove", lib ="glyphicon" ))
            )),
            column(12, plotlyOutput("Chart12",width = "100%" , height = "100%"))
            
   )
   )
    )
 )
          ),
 
 ##tab item3----
 tabItem(
   tabName = "tt",
   ## Cleared Billings Trend
   fluidRow(
     box(width=12,title="Traffic Trend",status="primary",solidHeader=T,collapsible=T,collapsed=T,
         column(12,checkboxGroupButtons(
           inputId = "Country7", label = "Please select the Country ",
           choices = c("Brazil","Indonesia","Philippines","Singapore"),
           selected = c("Brazil"),
           justified = T,size = "sm",
           status = "danger",
           checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon("remove", lib ="glyphicon" ))
         )),
         column(12, plotlyOutput("Chart13",width = "100%" , height = "100%"))
         
     )))
 ## End of tabs-----
 
 )
 )
)






#server-----

server<-function(input,output){
  output$total_Net<-renderValueBox({
    title
    valueBox(paste0(total_Net," Lakhs"),
            subtitle = "Total Net Billing",
             color = "olive",
            icon = icon("rupee")
            )
  })
  output$total_Gross<-renderValueBox({
    valueBox(paste0(total_Gross," Lakhs"),
             subtitle = "Total Gross Billing",
             color="olive",
             icon= icon("rupee"))
  })
  output$Total_Acc<-renderValueBox({
    valueBox(Total_Acc,
             subtitle = "New Ray Acquisitions",
             color="olive",
             icon=icon("hashtag"))
  })
  output$Total_Slots<-renderValueBox({
    valueBox(Total_Slots,
             subtitle = "Gross Reach Slots",
             color="olive",
             icon=icon("hashtag")
             )
  })
  output$Chart1a<-renderPlotly({
   q<- ggplotly(( ggplot()+theme(axis.text.x = element_text(size = 10),
                   axis.text.y = element_text(size = 10),
                   legend.text = element_text(size = 10),
                   legend.position = "bottom",
                   legend.justification = "center",
                   legend.direction = "horizontal",
                   legend.title = element_blank(),
                   text = element_text(family = "Palatino Linotype"))+ theme_gdocs()+scale_fill_wsj()+
      geom_bar(data=rbind(data.frame('Country'=report$Country,'Revenue'=round(report$Ray_Net/100000,0),'Type'=as.character("Ray")),data.frame('Country'=report$Country,'Revenue'=round(report$Reach_Net/100000,0),'Type'=as.character("Reach"))),
               aes(x=Country,y=Revenue,fill=`Type`),
               stat = "identity")+
      labs(x="Country",y="Revenue in Lakhs")))
   ggplotly(q, width = 800 , height= 400 ,tooltip = c("x", "y"))
  })
  output$Chart1b<-renderPlotly({
    p <- ggplot()+
      theme(axis.text.x = element_text(size = 10),
            axis.text.y = element_text(size = 10),
            legend.text = element_text(size = 10),
            legend.position = "bottom",
            legend.justification = "center",
            legend.direction = "horizontal",
            legend.title = element_blank(),
            text = element_text(family = "Palatino Linotype"))+theme_gdocs()+scale_fill_wsj()+
      geom_bar(data=rbind(data.frame('Country'=report$Country,'Revenue'=round(report$ray_revenue/100000,0),'Type'=as.character("Ray")),data.frame('Country'=report$Country,'Revenue'=round(report$reach_revenue/100000,0),'Type'=as.character("Reach"))),
               aes(x=Country,y=Revenue,colour=`Type`,fill=`Type`),
               stat = "identity")+
      labs(x="Country",y="Revenue in Lakhs")
   
    ggplotly(p, width = 800 , height= 400 ,tooltip = c("x", "y"))
    
  })
  output$Chart2a<-renderSunburst({
  
     sunburst(data.frame('Country'=report$Country,'Reach Slots'=report$Reach_slots), count = T, legend = list(w = 200, h = 20, s = 20, t = 200))
    
    # P <-plot_ly(data= data.frame('Country'=report$Country,'Reach Slots'=report$Ray_New_Acq), labels = report$Country, values = report$Reach_slots, type = "pie")
  })
  output$Chart2b<-renderSunburst({
   
    sunburst(data.frame('Country'=report$Country,'New Acquisitions'=report$Ray_New_Acq), count = T, legend = list(w = 200, h = 20, s = 20, t = 200))
    
    # P<- plot_ly(data= data.frame('Country'=report$Country,'New Acquisitions'=report$Ray_New_Acq), labels = report$Country, values = report$Ray_New_Acq, type = "pie") 
    # p<- ggplot(data= data.frame('Country'=report$Country,'New Acquisitions'=report$Ray_New_Acq),aes(x="", y=New.Acquisitions, fill=Country))+
    #   geom_bar(stat = 'identity')+
    #   coord_polar("y",start = 0 )+
    #   scale_fill_brewer(palette="Dark2")+
    #   geom_text(aes(y = New.Acquisitions/3 + c(0, cumsum(New.Acquisitions)[-length(New.Acquisitions)]),
    #                 label = percent(round(as.numeric(New.Acquisitions/sum(New.Acquisitions)),2)), size=2))+
    #   labs(x=NULL,y="New Acquistions")
                
  })
  output$Chart3<-renderPlotly({
    Int_Reach$month_name<-as.Date(Int_Reach$month_name, format = "%Y-%m-%d")
        Slot_brkup<-Int_Reach %>%
          filter(original_duration_months%%3==0 & original_duration_months !=0) %>%
        mutate(original_duration_months=as.character(original_duration_months)) %>%
      filter(Country %in% input$Country1) %>%
      filter(tag %in% tolower(input$Reach_HUR1)) %>%
          filter(year(month_name)>=2016 & MasterPlan == "Reach") %>%
      group_by(month_name, original_duration_months) %>%
      summarise(Reach_slots = length(order_id)) %>%
      select(month_name,original_duration_months,Reach_slots)%>%
          ungroup()
        
      
    q <-( ggplot() + 
      theme_gdocs() + scale_fill_economist() + theme_economist_white()+
          geom_line(data = Slot_brkup,
               aes(x=month_name,y= Reach_slots, color = original_duration_months),stat = "identity"
               )+
        geom_point(data = Slot_brkup,
                   aes(x=month_name,y= Reach_slots, color = original_duration_months),stat = "identity"
        )+
      labs(x="Month",y="Slots Sold") +
      scale_y_continuous(breaks = seq(0,200,by=10))+
      scale_x_date(date_breaks = "2 months",date_labels = "%b-%y")+
      guides(color= guide_legend(title = "Duration",title.position = "top")))
      
      
 
  ggplotly(q,
  tooltip = c("x", "y"))
  
  })
  output$Chart4<-renderPlotly({
    
  Acc<-Int_orders %>%
    filter(MasterPlan=="Ray") %>%
    mutate(HUR_Ray=ifelse(HUR_Flag=="Hunt","New Acquistions","Field and Early Renewals")) %>%
    filter(original_duration_months%%3 ==0 & original_duration_months !=0) %>%
    filter(Country %in% input$Country2) %>%
    filter(HUR_Ray %in% input$Acctype) %>%
    filter(year(placed_at)>=2016) %>%
    group_by(month_name,original_duration_months) %>%
    summarise(Accounts = length(unique(practice_id ))) %>%
    select(month_name,original_duration_months,Accounts)
  Acc$month_name<-as.Date(Acc$month_name)
  Acc$original_duration_months<-as.character(Acc$original_duration_months)
  
    p <- ggplot()+
      
      geom_line(data=Acc,aes(x=month_name,y=Accounts, group=original_duration_months,color=original_duration_months),stat="identity",  size = 0.5) +
      
      geom_point(data=Acc,aes(x=month_name,y=Accounts, group=original_duration_months,color=original_duration_months),stat="identity",  size = 1.5) +
      
      scale_x_date(date_breaks = "1 month",date_labels = "%b-%y") + 
      
      theme_gdocs() + scale_fill_economist() + scale_colour_hue() +
      
      theme(axis.text.x = element_text(size = 9),
            axis.text.y = element_text(size = 9),
            axis.title = element_text(size = 12),
            legend.text = element_text(size = 10),
            legend.position = "bottom",
            legend.justification = "center",
            legend.direction = "horizontal",
            legend.title = element_blank(),
            text = element_text(family = "Palatino Linotype"))
      
    ggplotly(p,tooltip= c("x","y","color"))
  })
  output$Chart5<-renderPlotly({
    Int_Reach$month_name<-as.Date(Int_Reach$month_name, format = "%Y-%m-%d")
    ARPS<-Int_Reach %>%
      # filter(tag == "hunt" | tag == "up-sell") %>%
      filter(tag %in% tolower(input$Reach_HUR2)) %>%
      filter(original_duration_months%%3==0) %>%
      filter(year(month_name)>=2016, MasterPlan == "Reach") %>%
      group_by(month_name,Country) %>%
      summarise(Reach_ARPS = round(sum(per_month)/length(practice_id ),0)) %>%
      select(month_name,Country,Reach_ARPS)%>%
      ungroup()
    # cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
    
    q <-( ggplot() + 
            theme_economist() + scale_fill_economist() + theme_gdocs()+
            geom_line(data = ARPS,
                     aes(x=month_name,y= Reach_ARPS,color=Country),stat = "identity",size=0.5,show.legend = TRUE)+
            geom_point(data = ARPS,
                      aes(x=month_name,y= Reach_ARPS,color=Country),stat = "identity",size=1.5,show.legend = TRUE)+
            # geom_text(data = ARPS,
            #            aes(x=month_name,y= Reach_ARPS+250,color=Country,label=Reach_ARPS,fontface ="bold"),stat = "identity",show.legend = FALSE)+
            labs(x="Month",y="ARPS") +
            scale_x_date(date_breaks = "2 months",date_labels = "%b-%y"))
    
    # ggsave("ARPs-New Reach Slots.png", plot = q, path = "D:/July Monthly reports",
    # scale = 1,width = 9, height = 5, #units = c("in", "cm", "mm"),
    # dpi = 300, limitsize = TRUE)
    
  ggplotly(q,tooltip= c("y","color"))
  })
  output$Chart6<-renderPlotly({
    Int_Ray$month_name<-as.Date(Int_Ray$month_name, format = "%Y-%m-%d")
    ARPU<-Int_Ray %>%
      filter(grepl('ray',tolower(Master_Plan_2))) %>%
      mutate(HUR_Ray=ifelse(HUR_Flag=="Hunt","New Acquistions","Field and Early Renewals")) %>%
      filter(HUR_Ray %in% input$Acctype2) %>%
      filter(year(placed_at)>2015) %>%
      group_by(month_name,Country) %>%
      summarise(Ray_ARPU = round(sum(per_month)/length(unique(practice_id )),0)) %>%
      select(month_name,Country,Ray_ARPU)%>%
      ungroup()
    
    q <-( ggplot() + 
            theme_gdocs() + scale_fill_economist() + scale_colour_hue() +
            geom_line(data = ARPU,
                     aes(x=month_name,y= Ray_ARPU,color= Country),size=0.5,stat = "identity")+
            geom_point(data = ARPU,
                      aes(x=month_name,y= Ray_ARPU,color= Country),size=1.5,stat = "identity")+
            labs(x="Month",y="ARPU") +
            scale_x_date(date_breaks = "2 months",date_labels = "%b-%y")) 
    
     ggplotly(q,tooltip= c("y","color"))
     })
  output$Chart8<-renderPlotly({
    Int_Ray$month_name<-as.Date(Int_Ray$month_name)
  a<- Int_Ray %>%
     filter(Country %in% input$Country4) %>%
    filter(original_duration_months%%3 ==0) %>%
     filter(HUR_Flag=="Hunt") %>%
     filter(year(placed_at)>2015) %>%
     group_by(month_name,Country) %>%
     summarise(Acquisitions = length(unique(practice_id ))) %>%
     select(month_name,Country,Acquisitions) %>%
     ungroup()
  
  b<- Int_Ray %>%
    # filter(Country %in% input$Country4) %>%
    mutate(Total = "Total") %>%
    filter(HUR_Flag=="Hunt") %>%
    filter(original_duration_months%%3 ==0) %>%
    filter(year(placed_at)>2015) %>%
    group_by(month_name,Total) %>%
    summarise(`Total Acquisitions` = length(unique(practice_id ))) %>%
    select(month_name,Total,`Total Acquisitions`) %>%
    ungroup()
    
   A<- ggplot()+
     geom_line(data=a,aes(x= month_name,y=Acquisitions , color=Country),stat='identity',size=0.5,position = 'identity')+
     geom_point(data=a,aes(x= month_name,y=Acquisitions , color=Country),stat='identity',size=1.5,position = 'identity')+
     theme_economist() + scale_fill_economist()+
     scale_x_date(date_breaks = ("2 months"),date_labels = ("%b-%y"))+
     geom_line(data=b,aes(x= month_name,y=`Total Acquisitions`,color=Total ),stat='identity',size=0.5,position = 'identity',show.legend = T)+
     geom_point(data=b,aes(x= month_name,y=`Total Acquisitions`,color=Total),stat='identity',size=1.5,position = 'identity')+
     theme_gdocs() + scale_fill_economist() + scale_colour_hue() 
     
   ggplotly(A,tooltip= c("y","color")) 
    
  })
  output$Chart9<-renderPlotly({
    Int_Reach$month_name<-as.Date(Int_Reach$month_name)
    a<- Int_Reach %>%
      filter(original_duration_months%%3==0) %>%
      filter(tag %in% tolower(input$Reach_HUR3)) %>%
      filter(year(placed_at)>2015) %>%
      group_by(month_name,Country) %>%
      summarise(`Reach Slots` = length(practice_id )) %>%
      select(month_name,Country,`Reach Slots`) %>%
      ungroup()
tag
    b<- Int_Reach %>%
      filter(original_duration_months%%3==0) %>%
      filter(tag %in% tolower(input$Reach_HUR3)) %>%
      mutate(Total = "Total") %>%
      filter(year(placed_at)>2015) %>%
      group_by(month_name,Total) %>%
      summarise(`Reach Slots` = length(practice_id )) %>%
      select(month_name,Total,`Reach Slots`) %>%
      ungroup()
    
    A<- ggplot()+
      geom_line(data=a,aes(x= month_name,y=`Reach Slots` , color=Country),stat='identity',size=0.5,position = 'identity')+
      geom_point(data=a,aes(x= month_name,y=`Reach Slots` , color=Country),stat='identity',size=1.5,position = 'identity')+
      theme_gdocs() + scale_fill_economist() + scale_colour_hue() +
      scale_x_date(date_breaks = ("2 months"),date_labels = ("%b-%y"))+
      geom_line(data=b,aes(x= month_name,y=`Reach Slots`,color=Total ),stat='identity',size=0.5,position = 'identity',show.legend = T)+
      geom_point(data=b,aes(x= month_name,y=`Reach Slots`,color=Total),stat='identity',size=1.5,position = 'identity')
    
    
    ggplotly(A,tooltip= c("y","color"))
  })
  output$Chart7<-renderPlotly({
    Int_orders$month_name<-as.Date(Int_orders$month_name)
    Revenue<-Int_orders %>%
      filter(year(placed_at)>2015) %>%
      # filter(Country == "Indonesia") %>%
      filter(Country %in% input$Country3) %>%
      group_by(month_name,MasterPlan) %>%
      summarise(Revenue_in_lakhs=round(sum(Revenue)/100000,0)) %>%
      select(month_name,MasterPlan,Revenue_in_lakhs) %>%
      ungroup()
    Revenue_total<-Int_orders %>%
      filter(year(placed_at)>2015) %>%
      # filter(Country == "Indonesia") %>%
      filter(Country %in% input$Country3) %>%
      mutate(Total="Total Revenue") %>%
      group_by(month_name,Total) %>%
      summarise(Revenue_in_lakhs=round(sum(Revenue)/100000,0)) %>%
      select(month_name, Total,Revenue_in_lakhs) %>%
      ungroup()
  
    A<- ggplot()+
      theme_gdocs() + scale_fill_economist() + scale_colour_hue() +
      geom_line(data=Revenue,aes(x= month_name,y=Revenue_in_lakhs , color=MasterPlan,group=MasterPlan),stat='identity',size=0.5,position = 'identity')+
      geom_point(data=Revenue,aes(x= month_name,y=Revenue_in_lakhs, color=MasterPlan,group=MasterPlan),stat='identity',size=1.5,position = 'identity')+
      scale_x_date(date_breaks = ("2 months"),date_labels = ("%b-%y"))+
    labs(x="Month",y="Cleared Revenue")+
      geom_line(data=Revenue_total,aes(x= month_name,y=Revenue_in_lakhs,color=Total ),stat='identity',size=0.5,position = 'identity',show.legend = T)+
      geom_point(data=Revenue_total,aes(x= month_name,y=Revenue_in_lakhs,color=Total),stat='identity',size=1.5,position = 'identity')
     # geom_text(data=Revenue,aes(x= month_name, y=Revenue_in_lakhs+5, color=MasterPlan,fontface="bold",group=MasterPlan,label=Revenue_in_lakhs),stat='identity',check_overlap = T,show.legend = F)+
     # geom_text(data=Revenue_total,aes(x= month_name,y=Revenue_in_lakhs+5,color=Total,fontface="bold",label=Revenue_in_lakhs),stat='identity',check_overlap = T,show.legend = F)

    # ggsave("Int Cleared Billings Breakup.png", plot = A, path = "D:/July Monthly reports",
    #        scale = 1,width = 8, height = 5, #units = c("in", "cm", "mm"),
    #        dpi = 300, limitsize = TRUE)
    
    ggplotly(A,tooltip= c("y","color")) 
    
  })
  output$Chart10<-renderSunburst({
    u<-rbind(data.frame(Country=report$Country,`Cancelled Revenue`=report$ray_cn,Product="Ray"),data.frame(Country=report$Country,`Cancelled Revenue`=report$reach_cn,Product="Reach"))
    
    v<-u %>%
      filter(Country %in% input$Country6)%>%
      group_by(Product) %>%
      summarise(`Cancelled Revenue`=sum(Cancelled.Revenue)) %>%
      select(Product,`Cancelled Revenue`) %>%
      ungroup()
    
    sunburst(v,count = T,legend = list(w = 200, h = 20, s = 20, t = 200))
    
  })
  output$Chart11<-renderPlotly({
    Int_Ray$month_name<-as.Date(Int_Ray$month_name)
    
    a<- Int_Ray %>%
      mutate(HUR_Ray=ifelse(HUR_Flag=="Hunt","New Acquistions","Field and Early Renewals")) %>%
      filter(original_duration_months%%3 ==0 & original_duration_months !=0) %>%
      filter(HUR_Ray %in% input$Acctype3) %>%
      filter(year(placed_at)>2015) %>%
      group_by(month_name,Country) %>%
      summarise(Av_dur = round(mean(dur_actual),0)) %>%
      select(month_name,Country,Av_dur) %>%
      ungroup()
    
    q<-ggplot()+
      geom_line(data = a,aes(x=month_name,y=Av_dur,color=Country),size=0.5,stat = 'identity')+
      geom_point(data = a,aes(x=month_name,y=Av_dur,color=Country),size=1.5,stat = 'identity')+
      scale_x_date(date_breaks = ("2 months"),date_labels = ("%b-%y"))+
      labs(x="Month",y="Average Duration in months" )+
      theme_gdocs()+scale_fill_canva()
    
    ggplotly(q,tooltip=c("x","y","color"))
      
      
  })
  output$Chart12<-renderPlotly({
    Int_Reach$month_name<-as.Date(Int_Reach$month_name)
    
    
    b<- Int_Reach %>%
      filter(original_duration_months%%3==0 & original_duration_months !=0) %>%
      filter(tag %in% tolower(input$Reach_HUR4)) %>%
      filter(year(placed_at)>2015) %>%
      group_by(month_name,Country) %>%
      summarise(Av_dur = round(mean(dur_actual),0)) %>%
      select(month_name,Country,Av_dur) %>%
      ungroup()
    
    q<-ggplot()+
      geom_line(data = b,aes(x=month_name,y=Av_dur,color=Country),size=0.5,stat = 'identity')+
      geom_point(data = b,aes(x=month_name,y=Av_dur,color=Country),size=1.5,stat = 'identity')+
      scale_x_date(date_breaks = ("2 months"),date_labels = ("%b-%y"))+
      labs(x="Month",y="Average Duration in months" )+
      theme_gdocs()+scale_fill_canva()
    
    ggplotly(q,tooltip=c("x","y","color"))
    
  })
  output$Chart13<-renderPlotly({
    traffic<-read.csv("./traffic.csv",stringsAsFactors = F)
    traffic1<-traffic %>%
      filter(country %in% input$Country7) %>%
      mutate(users=round(users/1000,0)) %>%
      select(country,month_year,Source,users) %>%
      ungroup()
    
    traffic2<-traffic %>%
      filter(country %in% input$Country7) %>%
      mutate(users=round(users/1000,0)) %>%
      group_by(month_year,country) %>%
      summarise(total_users=sum(users))%>%
      select(country,month_year,total_users) %>%
      ungroup()
    
    traffic1$month_year<-as.Date(traffic1$month_year)
    traffic2$month_year<-as.Date(traffic2$month_year)
    
    
    
    q<-ggplot()+
      geom_bar(data=traffic2,aes(x=month_year,y=total_users),fill="grey",stat='identity')+
      geom_line(data = traffic1,aes(x=month_year,y=users,color=Source),size=0.5,stat = 'identity')+
      geom_point(data = traffic1,aes(x=month_year,y=users,color=Source),size=1.5,stat = 'identity')+
      scale_x_date(date_breaks = ("2 months"),date_labels = ("%b-%y"))+
      labs(x="Month",y="Users in thousands" )+
      theme_gdocs()+scale_fill_canva()
      
    
    ggplotly(q,tooltip=c("x","y","color"))
    
    })
}

shinyApp(ui,server)


# devtools::install_github('hadley/ggplot2')
        








