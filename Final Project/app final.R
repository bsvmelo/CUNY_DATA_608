
library(tidyverse)
library(markdown)
library(shiny)
library(shinydashboard)
library(quantmod)
library(PerformanceAnalytics)
library(collapsibleTree)
#install.packages("collapsibleTree")

#dygraph


#https://shiny.rstudio.com/gallery/tabsets.html

#data input
mf.raw<-read.csv("C:/Users/atm20/OneDrive/Documents/CUNY_DATA_608/Final Project/MutualFunds.csv", sep = ",", header = TRUE, na.strings = c("N/A", ""))
#etf.raw<-read.csv("C:/Users/atm20/OneDrive/Documents/CUNY_DATA_608/Final Project/ETFs.csv", sep = ",", header = TRUE, na.strings = c("N/A", ""))
mf.strat<-read.csv("C:/Users/atm20/OneDrive/Documents/CUNY_DATA_608/Final Project/fund_strategy.csv", sep = ",", header = TRUE, na.strings = c("N/A", ""))
df<-mf.raw
df<-left_join(mf.raw,mf.strat,by="fund_category")
#Removing duplicates
df<-distinct(df,fund_long_name, fund_category, .keep_all= TRUE)
df<-distinct(df,investment_strategy,asset_stocks,top10_holdings, .keep_all= TRUE)
df<-df %>% select(fund_strategy, everything())
#Vectors:
#Fund Category == All
fund.cat<-unique(df$fund_category)
#Fund Category == Target Date Funds
fund.cat.target<- subset(df, grepl("Target-Date 2", fund_category))
fund.cat.target<-unique(fund.cat.target$fund_category)
#Fund Category == Allocation
fund.cat.alloc<- subset(df, grepl("Allocation--", fund_category))
fund.cat.alloc<-unique(fund.cat.alloc$fund_category)
#Sectors, Rating, Asset allocation
nms <- names(df)
sec.eq<-colnames(df[grepl("sector_", nms)])
sec.eq<-str_sub(sec.eq,13,)
sec.eq<-str_to_title(str_replace(sec.eq,"_"," "))

sec.alloc<-colnames(df[grepl("asset_", nms)])
sec.alloc<-str_sub(sec.alloc,7,)
sec.alloc<-str_to_title(sec.alloc)

sec.bonds<-colnames(df[grepl("fund_bonds_", nms)])
#sec.bonds<-sec.bonds[2:9]
sec.bonds<-str_sub(sec.bonds,12,)
sec.bonds<-str_to_title(str_replace(sec.bonds,"_"," "))

#Returns
qtr.ret.q1<-colnames(df[grepl("_q1", nms)])
qtr.ret.q2<- colnames(df[grepl("_q2", nms)])
qtr.ret.q3<- colnames(df[grepl("_q3", nms)])
qtr.ret.q4<- colnames(df[grepl("_q4", nms)])
qtr.ret<- c(qtr.ret.q1,qtr.ret.q2,qtr.ret.q3,qtr.ret.q4)
#Portfolio set up
#Fund categories definition
cat.pf.eq <-c("International Equity" , "U.S. Equity" , "Sector Equity")
cat.pf.fi <-c("Taxable Bond" , "Municipal Bond")
cat.pf.alt<-c("Alternative")

cat.pf<-c(cat.pf.eq, cat.pf.fi,cat.pf.alt)


#Pf creation



#aggressive -> Standard Deviation > 75 percentile
a<-df %>% group_by(fund_category) %>% filter(fund_stdev_3years >= quantile(fund_stdev_3years,0.75, na.rm=TRUE)) %>% ungroup() %>% 
    filter(fund_strategy %in% cat.pf) %>% group_by(fund_strategy) %>% top_n(3,fund_return_3years) %>% mutate(Pf_category = 'Aggressive', Pf_alloc = 1/18) %>% ungroup()
#conservative -> Standard Deviation < 25 percentile
b<-df %>% group_by(fund_category) %>% filter(fund_stdev_3years <= quantile(fund_stdev_3years,0.25, na.rm=TRUE)) %>% ungroup() %>% 
    filter(fund_strategy %in% cat.pf) %>% group_by(fund_strategy) %>% top_n(3,fund_return_3years) %>% mutate(Pf_category = 'Conservative', Pf_alloc = 1/18) %>% ungroup()
#moderate -> Standard Deviation btw 25 & 75 percentile
c<-df %>% group_by(fund_category) %>% filter(fund_stdev_3years > quantile(fund_stdev_3years,0.25, na.rm=TRUE) & fund_stdev_3years < quantile(fund_stdev_3years,0.75, na.rm=TRUE)) %>% ungroup() %>% 
    filter(fund_strategy %in% cat.pf) %>% group_by(fund_strategy) %>% top_n(3,fund_return_3years) %>% mutate(Pf_category = 'Moderate', Pf_alloc = 1/18) %>% ungroup()
#final pfs
dt<-rbind(a,b,c)


#Summary statistics
#replace NA by 0
dt <- dt %>%mutate_if(is.numeric, ~replace_na(., 0))
# Fund weighting
alloc<-1/18
# basic return and risk
dt %>% group_by(Pf_category)%>%summarise(Fund_Return_3_yrs=sum(Pf_alloc*fund_return_3years), Risk_3_yrs=sum(Pf_alloc*fund_stdev_3years), Sharpe_3_yrs=sum(Pf_alloc*fund_sharpe_ratio_3years), Fund_Return_5_yrs=sum(Pf_alloc*fund_return_5years), Risk_5_yrs=sum(Pf_alloc*fund_stdev_5years), Sharpe_5_yrs=sum(Pf_alloc*fund_sharpe_ratio_5years))%>%
    arrange(desc(Fund_Return_3_yrs))
#Return streams

#Pf_Category
#dt%>%group_by(Pf_category)%>%select(ends_with("_q1"))%>%summarize(count=n())
dt[paste0(qtr.ret.q1, "_cont")] <- map2(dt %>% select(ends_with("_q1")), alloc,  ~ .x *.y)
dt[paste0(qtr.ret.q2, "_cont")] <- map2(dt %>% select(ends_with("_q2")), alloc,  ~ .x *.y)
dt[paste0(qtr.ret.q3, "_cont")] <- map2(dt %>% select(ends_with("_q3")), alloc,  ~ .x *.y)
dt[paste0(qtr.ret.q4, "_cont")] <- map2(dt %>% select(ends_with("_q4")), alloc,  ~ .x *.y)

ret.stream<-dt%>%group_by(Pf_category) %>% select(contains('_cont')) %>% summarize(across(everything(), list(sum))) #%>% ungroup() %>% data.frame()
ret.stream<-as.data.frame(ret.stream)
ret.stream.pivot<-pivot_longer(ret.stream,2:88,names_to = "temp",values_to="Return")
#Date conversion
tt<-unlist(ret.stream.pivot[2])
dd<-if_else(str_sub(tt,18,19) %in% 'q1',"3/31/",if_else(str_sub(tt,18,19) %in% 'q2',"6/30/",if_else(str_sub(tt,18,19) %in% 'q3',"9/30/",if_else(str_sub(tt,18,19) %in% 'q4',"12/31/",""))))
yy<-str_sub(tt,13,16)
datt<-paste(dd,yy,sep="")
ret.stream.pivot[,4]=datt
colnames(ret.stream.pivot)[4]<-"Date"
ret.stream.pivot$Date<-as.Date(ret.stream.pivot$Date, "%m/%d/%Y")
ret.stream.pivot <- subset(ret.stream.pivot, select=c(4,3,1))

# Drawdowns
# ret.stream.pivot.1 <- ret.stream.pivot %>% filter(Pf_category == "Conservative") %>% select(Date,Return)
# # ret.stream.pivot.2 <- ret.stream.pivot %>% filter(Pf_category == "Aggressive") %>% select(Date,Return)
# # ret.stream.pivot.3 <- ret.stream.pivot %>% filter(Pf_category == "Moderate") %>% select(Date,Return)
# 
# ret.stream.pivot.11<-xts(x = ret.stream.pivot.1[, -1],order.by = as.Date(data$Date))
# # ret.stream.pivot.22<-xts(x = ret.stream.pivot.2[, -1],order.by = as.Date(data$Date))
# # ret.stream.pivot.33<-xts(x = ret.stream.pivot.3[, -1],order.by = as.Date(data$Date))
# 
# #fdd.1<-table.Drawdowns(ret.stream.pivot.11)
# fdd.2<-table.Drawdowns(ret.stream.pivot.22)
# fdd.3<-table.Drawdowns(ret.stream.pivot.33)

#dd.1 <- (length(ret.stream.pivot.1$Return))
#dd.2 <- (length(fdd.1$From))

#for(i in 1:dd.1){ for(j in 1:dd.2){ret.stream.pivot.1[i,3]<-if_else(ret.stream.pivot.1[[i,1]]>=fdd.1[j,1] & ret.stream.pivot.1[[i,1]]<=fdd.1[j,3],1,if_else(ret.stream.pivot.1[[i,1]]==1,1,0))  }}



#calculating cumsum
temp1<-ret.stream.pivot %>% filter(Pf_category == "Conservative") %>% arrange(Date) %>% mutate(cum.ret=1*(cumprod(1+Return)))
temp2<-ret.stream.pivot %>% filter(Pf_category == "Aggressive") %>% arrange(Date) %>% mutate(cum.ret=1*(cumprod(1+Return)))
temp3<-ret.stream.pivot %>% filter(Pf_category == "Moderate") %>% arrange(Date) %>% mutate(cum.ret=1*(cumprod(1+Return)))
ret.stream.pivot.ggp<-rbind(temp1,temp2,temp3)


#Fund_Strategy
#dt%>%group_by(Pf_category)%>%select(ends_with("_q1"))%>%summarize(count=n())

ret.stream.strat<-dt%>%group_by(Pf_category,fund_strategy) %>% select(contains('_cont')) %>% summarize(across(everything(), list(sum)))
ret.stream.strat<-as.data.frame(ret.stream.strat)
ret.stream.strat.pivot<-pivot_longer(ret.stream.strat,3:89,names_to = "temp",values_to="Return")
#Date conversion
ttt<-unlist(ret.stream.strat.pivot[3])
ddd<-if_else(str_sub(ttt,18,19) %in% 'q1',"3/31/",if_else(str_sub(ttt,18,19) %in% 'q2',"6/30/",if_else(str_sub(ttt,18,19) %in% 'q3',"9/30/",if_else(str_sub(ttt,18,19) %in% 'q4',"12/31/",""))))
yyy<-str_sub(ttt,13,16)
dattt<-paste(ddd,yyy,sep="")
ret.stream.strat.pivot[,5]=dattt
colnames(ret.stream.strat.pivot)[5]<-"Date"
ret.stream.strat.pivot$Date<-as.Date(ret.stream.strat.pivot$Date, "%m/%d/%Y")
ret.stream.strat.pivot <- subset(ret.stream.strat.pivot, select=c(5,4,1,2))

#asset allocation
#dt.test<-dt%>%summarise(across(where(starts_with("asset_")))*1/18)
#dt.test1<-dt
dt.test<-as.data.frame(map2(dt %>% select(starts_with("asset_")), 1/18,  ~ .x *.y))
colnames(dt.test)<-paste0(colnames(dt.test),"_alloca")
dt<-cbind(dt,dt.test)
asset_alloc<-dt%>%group_by(Pf_category)%>% select(Pf_category,contains("_alloca")) %>% summarize(across(where(is.numeric),sum))
asset_alloc.pivot<-pivot_longer(asset_alloc,-1,names_to = "Asset_Type",values_to = "Allocation")

asset_alloc_strat<-dt%>%group_by(Pf_category,fund_strategy)%>% select(Pf_category,fund_strategy,contains("_alloca")) %>% summarize(across(where(is.numeric),sum))
asset_alloc_strat.pivot<-pivot_longer(asset_alloc_strat,-c(1,2),names_to = "Asset_Type",values_to = "Allocation")


asset_alloc_strat_fund<-dt%>%group_by(Pf_category,fund_strategy,fund_short_name)%>% select(Pf_category,fund_strategy,fund_short_name,contains("_alloca")) %>% summarize(across(where(is.numeric),sum))
asset_alloc_strat_fund.pivot<-pivot_longer(asset_alloc_strat_fund,-c(1,2,3),names_to = "Asset_Type",values_to = "Allocation")

#tree
asset_alloc_tree<-asset_alloc_strat_fund.pivot%>%group_by(Pf_category,fund_strategy,fund_short_name)%>%summarise(sum=sum(Allocation))

#Bond Rating
dt.ratings<-as.data.frame(map2(dt %>% select(starts_with("fund_bonds_")), 1/18,  ~ .x *.y))
colnames(dt.ratings)<-paste0(sec.bonds,"_rtg")
dt<-cbind(dt,dt.ratings[2:9])
rating_alloc<-dt%>%group_by(Pf_category)%>% select(Pf_category,contains("_rtg")) %>% summarize(across(where(is.numeric),sum))
colnames(rating_alloc)[2:9]<-str_to_upper(sec.bonds[2:9])
rating_alloc.pivot<-pivot_longer(rating_alloc,-1,names_to = "Ratings",values_to = "Allocation")
rating_alloc.pivot[4]<-rep(8:1,3)
colnames(rating_alloc.pivot)[4]<-"rtg"


#equity sector
dt.sector<-as.data.frame(map2(dt %>% select(starts_with("fund_sector_")), 1/18,  ~ .x *.y))
colnames(dt.sector)<-paste0(sec.eq,"_seceq")
dt<-cbind(dt,dt.sector)
sec_alloc<-dt%>%group_by(Pf_category)%>% select(Pf_category,contains("_seceq")) %>% summarize(across(where(is.numeric),sum))
colnames(sec_alloc)[2:12]<-sec.eq
sec_alloc.pivot<-pivot_longer(sec_alloc,-1,names_to = "Sector",values_to = "Allocation")

#Asset name conversion
ttt<-unlist(asset_alloc.pivot[2])
ddd<-list(str_to_title(gsub(".*_(.+)_.*", "\\1", ttt)))
asset_alloc.pivot[,2]=ddd

tttt<-unlist(asset_alloc_strat.pivot[3])
dddd<-list(str_to_title(gsub(".*_(.+)_.*", "\\1", tttt)))
asset_alloc_strat.pivot[,3]=dddd

ttttt<-unlist(asset_alloc_strat_fund.pivot[4])
ddddd<-list(str_to_title(gsub(".*_(.+)_.*", "\\1", ttttt)))
asset_alloc_strat_fund.pivot[,4]=ddddd





#charts approved

# Facet grid by Fund strategy with VaRs
ret.stream.strat.pivot %>% 
    ggplot(aes(x = Return, fill = Pf_category)) + 
    geom_density(aes(fill = Pf_category, colour = Pf_category)) +
    #geom_histogram(alpha = 0.25, binwidth = .01) + #geom_vline(aes(xintercept = quantile(Return,0.05)), data=ret.stream.strat.pivot, colour = "darkgrey", alpha = 0.5) + 
    #annotate(geom = "curve", x = quantile(ret.stream.strat.pivot$Return,0.05)+0.01, y = 70, xend = quantile(ret.stream.strat.pivot$Return,0.05), yend = 60,curvature = .3, arrow = arrow(length = unit(2, "mm"))) +
    #annotate(geom = "text", x = quantile(ret.stream.strat.pivot$Return,0.05)+0.03, y = 73, label = quantile(ret.stream.strat.pivot$Return,0.05), hjust = "left") +
    stat_summary( geom = "vline", orientation = "y", aes(y = 1, xintercept = after_stat(x)), fun = function(x) {quantile(x, probs = c(0.025, 0.975))}) +
    facet_grid(fct_relevel(Pf_category,'Aggressive','Moderate','Conservative') ~ fct_relevel(fund_strategy,'U.S. Equity','Sector Equity','International Equity','Taxable Bond', 'Municipal Bond', 'Alternative')) +
    theme(legend.position = "none")

ret.stream.strat.pivot %>% 
    ggplot(aes(x = Return, fill = Pf_category)) + 
    geom_density(aes(fill = Pf_category, colour = Pf_category)) +
    #geom_histogram(alpha = 0.25, binwidth = .01) + #geom_vline(aes(xintercept = quantile(Return,0.05)), data=ret.stream.strat.pivot, colour = "darkgrey", alpha = 0.5) + 
    #annotate(geom = "curve", x = quantile(ret.stream.strat.pivot$Return,0.05)+0.01, y = 70, xend = quantile(ret.stream.strat.pivot$Return,0.05), yend = 60,curvature = .3, arrow = arrow(length = unit(2, "mm"))) +
    #annotate(geom = "text", x = quantile(ret.stream.strat.pivot$Return,0.05)+0.03, y = 73, label = quantile(ret.stream.strat.pivot$Return,0.05), hjust = "left") +
    stat_summary( geom = "vline", orientation = "y", aes(y = 1, xintercept = after_stat(x)), fun = function(x) {quantile(x, probs = c(0.025, 0.975))}) +
    facet_grid(fct_relevel(Pf_category,'Aggressive','Moderate','Conservative') ~ .) + theme(legend.position = "none")

# 
# ui <- dashboardPage(
#     dashboardHeader(),
#     dashboardSidebar(),
#     dashboardBody(
#         box(
#             title = "Status summary", 
#             solidHeader = TRUE, 
#             status = "primary", 
#             width = 4, 
#             textOutput("selected_var")
#         )
#     )
# )
# 
# server <- function(input, output, session) {
#     output$selected_var <- renderText("Your input is X")
# }
# 

#https://github.com/dreamRs/shinyWidgets
#https://www.rdocumentation.org/packages/shinyWidgets/versions/0.4.4/topics/useShinydashboard


# Risk Tolerance

risk_levels<-c("Conservative","Moderate","Aggressive")

# Define UI for application that draws a histogram
# ui <- dashboardPage(
#     dashboardHeader(title = "Portfolio Comparison Dashboard"),
#     dashboardSidebar(
#             checkboxGroupInput("dist","Select risk tolerance level(s):",choices=(risk_levels), selected = "Moderate")
#     ),
    
    # dashboardBody(
    #   tabItems(
    #     selected = 1,
    #     tabItem(
    #       tabName = "panel1",
    #       textOutput(outputId = "text1")
    #     ),
    #     tabItem(
    #       tabName = "panel2",
    #       textOutput(outputId = "text2")
    #     )
    #   )
    # )
    # dashboardBody(
    #   #tabItems(
    #     # infoBoxes with fill=FALSE
    #    #fluidRow(width=12,box(p("3-year Return"))),
    # 
    #     # fluidRow(
    #     #          column(2,box(p(""),background = 'black')),
    #     #          column(10,valueBoxOutput("info_box",width=4))
    #     #          ),
    #     #tabItem(tabName = "Summary",
    #     fluidRow(
    #       column(6,plotOutput('table1')),
    #       column(6,plotOutput('plot1'))
    #     ),
    #   
    #     # fluidRow(
    #     #     # A static infoBox
    #     #     #infoBox("Return", , icon = icon("credit-card")),
    #     #     # Dynamic infoBoxes
    #     #     column(2,box(p("3-year Return"))),
    #     #     column(10,valueBoxOutput("returnBox1",width = 4))
    #     #     ),
    #     # fluidRow(
    #     #     column(2,box(p("Volatility"))),
    #     #     column(10,valueBoxOutput("returnBox2",width = 4))
    #     #     ),
    #     # fluidRow(
    #     #   column(2,box(p("Sharpe-Ratio"))),
    #     #   column(10,valueBoxOutput("returnBox3",width = 4))
    #     #     ),   
    #         
    #     fluidRow(
    #       column(6,plotOutput('plot2')),
    #       column(6,plotOutput('plot3'))
    #     )   
    #     )
        #tabItem(tabName = "Breakdowns")
    
# server <- function(input, output) {
#     # output$returnBox <- renderValueBox({
#     #     
#     #     tablea<-dt %>% group_by(Pf_category)%>% filter(Pf_category %in% input$dist)  %>% summarise(Fund_Return_3_yrs=sum(Pf_alloc*fund_return_3years), Risk_3_yrs=sum(Pf_alloc*fund_stdev_3years), Sharpe_3_yrs=sum(Pf_alloc*fund_sharpe_ratio_3years), Fund_Return_5_yrs=sum(Pf_alloc*fund_return_5years), Risk_5_yrs=sum(Pf_alloc*fund_stdev_5years), Sharpe_5_yrs=sum(Pf_alloc*fund_sharpe_ratio_5years))%>%
#     #         select(Fund_Return_3_yrs) %>% arrange(desc(Fund_Return_3_yrs)) 
#     #     valueBox("Return",tablea, icon = icon("list"),color = "purple")
#     #     # infoBox(
#     #     #     "Return",tablea, icon = icon("list"),
#     #     #     color = "purple"
#     #     #)
#     # })
#     # output$TitleBox <-renderInfoBox({
#     # 
#     #   infoBox("Return", icon=NULL, color="aqua", width=4)  
#     #         
#     # })
#     
#     #Return 3Years
# #   output$info_box <- renderUI({
# #     
# #     tablea<-dt %>% group_by(Pf_category)%>% filter(Pf_category %in% input$dist)  %>% summarise(Fund_Return_3_yrs=sum(Pf_alloc*fund_return_3years), Risk_3_yrs=sum(Pf_alloc*fund_stdev_3years), Sharpe_3_yrs=sum(Pf_alloc*fund_sharpe_ratio_3years), Fund_Return_5_yrs=sum(Pf_alloc*fund_return_5years), Risk_5_yrs=sum(Pf_alloc*fund_stdev_5years), Sharpe_5_yrs=sum(Pf_alloc*fund_sharpe_ratio_5years))%>%
# #       select(Pf_category,Fund_Return_3_yrs) %>% arrange(desc(Fund_Return_3_yrs))
# #     ss <- (length(tablea$Fund_Return_3_yrs))
# #     lapply(1:ss, function(a) {
# #       
# #       valueBox(subtitle = NULL, value=tags$p(tags$span(tablea[[a,1]], style = "float:center"), style = "font-size: 50%"),icon=NULL,color = "aqua",width = 4)})
# #   })  
# #     output$returnBox1 <- renderUI({
# #              
# #             tablea<-dt %>% group_by(Pf_category)%>% filter(Pf_category %in% input$dist)  %>% summarise(Fund_Return_3_yrs=sum(Pf_alloc*fund_return_3years), Risk_3_yrs=sum(Pf_alloc*fund_stdev_3years), Sharpe_3_yrs=sum(Pf_alloc*fund_sharpe_ratio_3years), Fund_Return_5_yrs=sum(Pf_alloc*fund_return_5years), Risk_5_yrs=sum(Pf_alloc*fund_stdev_5years), Sharpe_5_yrs=sum(Pf_alloc*fund_sharpe_ratio_5years))%>%
# #                  select(Pf_category,Fund_Return_3_yrs) %>% arrange(desc(Fund_Return_3_yrs))
# #              ss <- (length(tablea$Fund_Return_3_yrs))
# # 
# #              #category<-reactiveValues(input$dist)
# #              #ss<-dim(tablea[1])
# #              #ss<-2
# #             
# #              lapply(1:ss, function(a) {
# #                 
# #                 valueBox(subtitle = NULL ,value=tags$p(paste0(round(tablea[[a,2]]*100,1),"%"),style = "font-size: 50%;"),icon=NULL,color = "purple")})
# #             })
# # #
# #     output$returnBox2 <- renderUI({
# #       
# #       tablea<-dt %>% group_by(Pf_category)%>% filter(Pf_category %in% input$dist)  %>% summarise(Fund_Return_3_yrs=sum(Pf_alloc*fund_return_3years), Risk_3_yrs=sum(Pf_alloc*fund_stdev_3years), Sharpe_3_yrs=sum(Pf_alloc*fund_sharpe_ratio_3years), Fund_Return_5_yrs=sum(Pf_alloc*fund_return_5years), Risk_5_yrs=sum(Pf_alloc*fund_stdev_5years), Sharpe_5_yrs=sum(Pf_alloc*fund_sharpe_ratio_5years))%>%
# #         select(Pf_category,Risk_3_yrs,Fund_Return_3_yrs) %>% arrange(desc(Fund_Return_3_yrs))
# #       ss <- (length(tablea$Risk_3_yrs))
# #       
# #       #category<-reactiveValues(input$dist)
# #       #ss<-dim(tablea[1])
# #       #ss<-2
# #       
# #       lapply(1:ss, function(a) {
# #         
# #         valueBox(subtitle = NULL,value=tags$p(paste0(round(tablea[[a,2]]*1,1),"%"),style = "font-size: 50%;"),icon=NULL,color = "purple")})
# #     })        
# #     output$returnBox3 <- renderUI({
# #       
# #       tablea<-dt %>% group_by(Pf_category)%>% filter(Pf_category %in% input$dist)  %>% summarise(Fund_Return_3_yrs=sum(Pf_alloc*fund_return_3years), Risk_3_yrs=sum(Pf_alloc*fund_stdev_3years), Sharpe_3_yrs=sum(Pf_alloc*fund_sharpe_ratio_3years), Fund_Return_5_yrs=sum(Pf_alloc*fund_return_5years), Risk_5_yrs=sum(Pf_alloc*fund_stdev_5years), Sharpe_5_yrs=sum(Pf_alloc*fund_sharpe_ratio_5years))%>%
# #         select(Pf_category,Sharpe_3_yrs,Fund_Return_3_yrs) %>% arrange(desc(Fund_Return_3_yrs))
# #       ss <- (length(tablea$Sharpe_3_yrs))
# #       
# #       #category<-reactiveValues(input$dist)
# #       #ss<-dim(tablea[1])
# #       #ss<-2
# #       
# #       lapply(1:ss, function(a) {
# #         
# #         valueBox(subtitle = NULL,value=tags$p(paste0(round(tablea[[a,2]]*1,1),""),style = "font-size: 50%;"),icon=NULL,color = "purple")})
# #     })  
#         
#         ret.stream.pivot.1 <- ret.stream.pivot %>% filter(Pf_category == "Conservative") %>% select(Date,Return)
#         ret.stream.pivot.11<-xts(x = ret.stream.pivot.1[, -1],order.by = as.Date(data$Date))
#         fdd.1<-table.Drawdowns(ret.stream.pivot.11)
#     
#         place_plot2<- reactive({ret.stream.pivot.ggp %>% filter(.data$Pf_category %in% .env$input$dist) %>% arrange(Date) %>%
#                 ggplot(aes(x=Date,y=cum.ret,group=Pf_category,color=Pf_category))+geom_line(show.legend = FALSE, size=1) + geom_vline(aes(xintercept = as.numeric(From)),data = fdd.1,colour = "grey50", alpha = 0.5)+ geom_vline(aes(xintercept = as.numeric(To)),data = fdd.1,colour = "grey50", alpha = 0.5) +
#             theme_gray()+ annotate("rect", xmin = fdd.1$From, xmax = fdd.1$To, ymin= -Inf, ymax=Inf, alpha = .1) +labs(title = "Growth of $1",x = NULL,  y = NULL) +theme(plot.title = element_text(face = "bold",size = 20))})
#             #  geom_vline(aes(xintercept = as.numeric(From)),data = fdd.1,colour = "grey50", alpha = 0.5)
#             # + geom_vline(aes(xintercept = as.numeric(To)),data = fdd.1,colour = "grey50", alpha = 0.5)
#             # + annotate("rect", xmin = fdd.1$From, xmax = fdd.1$To, ymin= -Inf, ymax=Inf, alpha = .2)})
#         # chart.CumReturns(return_xts,wealth.index=TRUE, main="Growth of $1")
#         # return_xts <- xts(x = data[, -1],order.by = as.Date(data$Date))
# 
#         output$plot2 <- renderPlot({ place_plot2() })
#     
#         output$plot3 <-renderPlot({
#           
#           dt_plot <- ret.stream.pivot %>% filter(Pf_category %in% input$dist)  %>% pivot_wider(names_from=Pf_category,values_from = Return)
#           ret.stream.pivot_xts<-xts(x = dt_plot[, -1],order.by = as.Date(dt_plot$Date))
#           #table.AnnualizedReturns(return_xts, scale = NA, Rf = 0, geometric = TRUE, digits = 4)
#           chart.Drawdown(ret.stream.pivot_xts,wealth.index=TRUE, main="Drawdown", plot.engine = "ggplot2")
#           
#         })
#         
#         
#         output$table1 <-renderPlot({
#           
#           
#         tablea <- dt %>% group_by(Pf_category)%>% filter(Pf_category %in% input$dist)  %>% 
#           summarise(Fund_Return_3_yrs=sum(round(Pf_alloc*fund_return_3years*100,0)), Risk_3_yrs=sum(round(Pf_alloc*fund_stdev_3years,0)), Sharpe_3_yrs=sum(round(Pf_alloc*fund_sharpe_ratio_3years,1)), Fund_Return_5_yrs=sum(Pf_alloc*fund_return_5years), Risk_5_yrs=sum(Pf_alloc*fund_stdev_5years), Sharpe_5_yrs=sum(Pf_alloc*fund_sharpe_ratio_5years))%>%
#                       select(Pf_category, Fund_Return_3_yrs, Risk_3_yrs, Sharpe_3_yrs) %>% arrange(desc(Fund_Return_3_yrs))
#           
#           
#         tableb<-pivot_longer(tablea,cols = ends_with("yrs"),names_to="stats",values_to="values")
#         
#         level_order <- factor(tableb$stats, level = c('Sharpe_3_yrs', 'Risk_3_yrs', 'Fund_Return_3_yrs'))
#         
#         ggplot(tableb, aes(x = Pf_category, y = level_order, fill = factor(Pf_category))) + geom_tile(color = "white") + geom_text(aes(label = values), color = "white", fontface="bold", size=9) +
#           labs(x = NULL, y = NULL,title = "3-year Metrics")+  scale_y_discrete(labels=c('Sharpe Ratio','Volatility','Return')) + scale_x_discrete(position = "top") + theme(plot.title = element_text(face = "bold", size = 25),text = element_text(size=20),legend.position = "none", axis.ticks =element_blank(), panel.grid.major =element_blank(), panel.background =element_blank() )
#           
#         })
#         
#         level_order_asset <- unique(factor(asset_alloc.pivot$Asset_Type, level = c('Stocks','Bonds','Convertible','Cash','Preferred','Others')))
#         
#         place_plot1<- reactive({asset_alloc.pivot %>% filter(.data$Pf_category %in% .env$input$dist) %>%
#             ggplot(aes(x=Pf_category,y=reorder(Asset_Type,Allocation),group=Pf_category,fill=Allocation))+  scale_fill_distiller(palette = "Spectral") + geom_tile(color = "white")+geom_text(aes(label = round(Allocation*100,0)), color = "white", fontface="bold", size=8)+labs(x = NULL, y = NULL,title = "Asset Allocation") + scale_x_discrete(position = "top") + theme(plot.title = element_text(face = "bold", size = 25),text = element_text(size=20),legend.position = "none", axis.ticks =element_blank(), panel.grid.major =element_blank(), panel.background =element_blank() ) })
#         #  geom_vline(aes(xintercept = as.numeric(From)),data = fdd.1,colour = "grey50", alpha = 0.5)  #scale_fill_distiller(palette = "Spectral") scale_fill_fermenter(n.breaks = 9, palette = "PuOr")
#         # + geom_vline(aes(xintercept = as.numeric(To)),data = fdd.1,colour = "grey50", alpha = 0.5)
#         # + annotate("rect", xmin = fdd.1$From, xmax = fdd.1$To, ymin= -Inf, ymax=Inf, alpha = .2)})
#         # chart.CumReturns(return_xts,wealth.index=TRUE, main="Growth of $1")
#         # return_xts <- xts(x = data[, -1],order.by = as.Date(data$Date))
#         
#         output$plot1 <- renderPlot({ place_plot1() })
#         
#         
#         
#         
#         
#     }

#icon = tags$i(class="fas fa-arrows-up-to-line", style="font-size: 24px; color: white")
#<i class="fa-duotone fa-meteor"></i>
#shinyApp(ui, server)    
    
ui <-fluidPage(
#     
    # App title ----
    titlePanel(h1("Portfolio Comparison Visualization Tool", style='background-color:#FFDEAD;padding-left: 15px')),
    #BD9060
      
    # Sidebar layout with input and output definitions ----
    sidebarLayout(

        # Sidebar panel for inputs ----
        sidebarPanel(width=2,
          tags$style(".well {background: #FFE4C4;border: 0px;}"),
          #7b899e
            # Input: Select the tolerance type ----
            #radioButtons("dist","Select risk tolerance level(s):",choices=(risk_levels), selected = "Moderate"),
            checkboxGroupInput("dist","Select risk tolerance level(s):",choices=(risk_levels), selected = "Moderate")),
        # br() element to introduce extra vertical spacing ----



        # Main panel for displaying outputs ----
        mainPanel(

            # Output: Tabset w/ plot, summary, and table ----
            tabsetPanel(type = "tabs",
                        tabPanel("Summary", 
                        hr(style = "border-top: 0px solid #000000;"),hr(style = "border-top: 0px solid #000000;"),
                        fluidRow(
                            column(6,plotOutput('table1')),
                            column(6,plotOutput('plot1'))
                            #column(4,plotOutput('plot2')),
                        ),
                        hr(style = "border-top: 0px solid #000000;"),hr(style = "border-top: 0px solid #000000;"),
                        fluidRow(
                            column(6,plotOutput('plot2')),
                            br(),
                            column(6,plotOutput('plot3'))
                        )
                        ),

                        tabPanel("Allocation Overview", 
                        hr(style = "border-top: 0px solid #000000;"),hr(style = "border-top: 0px solid #000000;"),
                        fluidRow(
                          column(6,plotOutput('plot4')),
                          column(6,plotOutput('plot5'))
                          
                        ),         
                                 
                      hr(style = "border-top: 0px solid #000000;"),hr(style = "border-top: 0px solid #000000;"),
                       fluidRow(
                         column(12,plotOutput('plot6'))
                               ),
                       hr(style = "border-top: 0px solid #000000;"),hr(style = "border-top: 0px solid #000000;"),
                       # fluidRow(
                       #   column(12,plotOutput('plot5'))
                       # )
                       fluidRow(
                         column(12,collapsibleTreeOutput('plot7'))
                       ),
                                
                       fluidRow(
                         column(12,plotOutput('plot8'))
                       )
                        ),
                        tabPanel("Performance Analysis", tableOutput("table"))
            )

        )
    )


)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  #Growth plot
  ret.stream.pivot.1 <- ret.stream.pivot %>% filter(Pf_category == "Conservative") %>% select(Date,Return)
  ret.stream.pivot.11<-xts(x = ret.stream.pivot.1[, -1],order.by = as.Date(data$Date))
  fdd.1<-table.Drawdowns(ret.stream.pivot.11)
  
  
  place_plot2<- reactive({ret.stream.pivot.ggp %>% filter(.data$Pf_category %in% .env$input$dist) %>% arrange(Date) %>%
      ggplot(aes(x=Date,y=cum.ret,group=Pf_category,color=Pf_category))+geom_line(show.legend = FALSE, size=1) + geom_vline(aes(xintercept = as.numeric(From)),data = fdd.1,colour = "grey50", alpha = 0.5)+ geom_vline(aes(xintercept = as.numeric(To)),data = fdd.1,colour = "grey50", alpha = 0.5) +
      theme_gray()+ annotate("rect", xmin = fdd.1$From, xmax = fdd.1$To, ymin= -Inf, ymax=Inf, alpha = .1) +labs(title = "Growth of $1", subtitle = "and drawdowns",x = NULL,  y = NULL) +theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),plot.title = element_text(face = "bold",size = 20))})
  #  geom_vline(aes(xintercept = as.numeric(From)),data = fdd.1,colour = "grey50", alpha = 0.5)
  # + geom_vline(aes(xintercept = as.numeric(To)),data = fdd.1,colour = "grey50", alpha = 0.5)
  # + annotate("rect", xmin = fdd.1$From, xmax = fdd.1$To, ymin= -Inf, ymax=Inf, alpha = .2)})
  # chart.CumReturns(return_xts,wealth.index=TRUE, main="Growth of $1")
  # return_xts <- xts(x = data[, -1],order.by = as.Date(data$Date))
  
  output$plot2 <- renderPlot({ place_plot2() })
  
  #Drawdown Plot
  output$plot3 <-renderPlot({
    
    dt_plot <- ret.stream.pivot %>% filter(Pf_category %in% input$dist)  %>% pivot_wider(names_from=Pf_category,values_from = Return)
    ret.stream.pivot_xts<-xts(x = dt_plot[, -1],order.by = as.Date(dt_plot$Date))
    #table.AnnualizedReturns(return_xts, scale = NA, Rf = 0, geometric = TRUE, digits = 4)
    chart.Drawdown(ret.stream.pivot_xts,wealth.index=TRUE, main="Drawdown", plot.engine = "ggplot2")
    
  })
  
  #metrics table
  output$table1 <-renderPlot({
    
    
    tablea <- dt %>% group_by(Pf_category)%>% filter(Pf_category %in% input$dist)  %>% 
      summarise(Fund_Return_3_yrs=sum(round(Pf_alloc*fund_return_3years*100,0)), Risk_3_yrs=sum(round(Pf_alloc*fund_stdev_3years,0)), Sharpe_3_yrs=sum(round(Pf_alloc*fund_sharpe_ratio_3years,1)), Fund_Return_5_yrs=sum(Pf_alloc*fund_return_5years), Risk_5_yrs=sum(Pf_alloc*fund_stdev_5years), Sharpe_5_yrs=sum(Pf_alloc*fund_sharpe_ratio_5years))%>%
      select(Pf_category, Fund_Return_3_yrs, Risk_3_yrs, Sharpe_3_yrs) %>% arrange(desc(Fund_Return_3_yrs))
    
    
    tableb<-pivot_longer(tablea,cols = ends_with("yrs"),names_to="stats",values_to="values")
    
    level_order <- factor(tableb$stats, level = c('Sharpe_3_yrs', 'Risk_3_yrs', 'Fund_Return_3_yrs'))
    
    ggplot(tableb, aes(x = Pf_category, y = level_order, fill = factor(Pf_category))) + geom_tile(color = "white") + geom_text(aes(label = values), color = "white", fontface="bold", size=9) +
      labs(x = NULL, y = NULL,title = "3-year Metrics")+  scale_y_discrete(labels=c('Sharpe Ratio','Volatility (%)','Return (%)')) + scale_x_discrete(position = "top") + theme(plot.title = element_text(face = "bold", size = 25),text = element_text(size=20),legend.position = "none", axis.ticks =element_blank(), panel.grid.major =element_blank(), panel.background =element_blank() )
    
  })
  
  
  #Asset allocation table
  
  #by Asset Allocation
  level_order_asset <- unique(factor(asset_alloc.pivot$Asset_Type, level = c('Stocks','Bonds','Convertible','Cash','Preferred','Others')))
  
  place_plot1<- reactive({asset_alloc.pivot %>% filter(.data$Pf_category %in% .env$input$dist) %>%
      ggplot(aes(x=Pf_category,y=reorder(Asset_Type,Allocation),group=Pf_category,fill=Allocation))+  scale_fill_distiller(palette = "Spectral") + geom_tile(color = "white")+geom_text(aes(label = round(Allocation*100,0)), color = "white", fontface="bold", size=8)+labs(x = NULL, y = NULL,title = "Current Asset Allocation (%)") + scale_x_discrete(position = "top") + theme(plot.title = element_text(face = "bold", size = 25),text = element_text(size=20),legend.position = "none", axis.ticks =element_blank(), panel.grid.major =element_blank(), panel.background =element_blank() ) })
  #  geom_vline(aes(xintercept = as.numeric(From)),data = fdd.1,colour = "grey50", alpha = 0.5)  #scale_fill_distiller(palette = "Spectral") scale_fill_fermenter(n.breaks = 9, palette = "PuOr")
  # + geom_vline(aes(xintercept = as.numeric(To)),data = fdd.1,colour = "grey50", alpha = 0.5)
  # + annotate("rect", xmin = fdd.1$From, xmax = fdd.1$To, ymin= -Inf, ymax=Inf, alpha = .2)})
  # chart.CumReturns(return_xts,wealth.index=TRUE, main="Growth of $1")
  # return_xts <- xts(x = data[, -1],order.by = as.Date(data$Date))
  
  output$plot1 <- renderPlot({ place_plot1() })

  #By rating
  #level_order_ratings <- factor(rating_alloc.pivot$Ratings, level = c('AAA','AA','A','BBB','BB','B','BELOW B','OTHERS'))
  
  place_plot4<- reactive({rating_alloc.pivot %>% filter(.data$Pf_category %in% .env$input$dist) %>% 
      ggplot(aes(x=Pf_category,y=reorder(Ratings,rtg),group=Pf_category,fill=Allocation))+  scale_fill_distiller(palette = "Spectral") + geom_tile(color = "white")+geom_text(aes(label = round(Allocation*100,0)), color = "white", fontface="bold", size=8)+labs(x = NULL, y = NULL,title = "Bond Ratings Breakdown (%)") + scale_x_discrete(position = "top") + theme(plot.title = element_text(face = "bold", size = 20),text = element_text(size=16),legend.position = "none", axis.ticks =element_blank(), panel.grid.major =element_blank(), panel.background =element_blank() ) })
  #  geom_vline(aes(xintercept = as.numeric(From)),data = fdd.1,colour = "grey50", alpha = 0.5)  #scale_fill_distiller(palette = "Spectral") scale_fill_fermenter(n.breaks = 9, palette = "PuOr")
  # + geom_vline(aes(xintercept = as.numeric(To)),data = fdd.1,colour = "grey50", alpha = 0.5)
  # + annotate("rect", xmin = fdd.1$From, xmax = fdd.1$To, ymin= -Inf, ymax=Inf, alpha = .2)})
  # chart.CumReturns(return_xts,wealth.index=TRUE, main="Growth of $1")
  # return_xts <- xts(x = data[, -1],order.by = as.Date(data$Date))
  
  output$plot4 <- renderPlot({ place_plot4() })
  
  #by equity sector
  place_plot5<- reactive({sec_alloc.pivot %>% filter(.data$Pf_category %in% .env$input$dist) %>% 
      ggplot(aes(x=Pf_category,y=reorder(Sector,Allocation),group=Pf_category,fill=Allocation))+  scale_fill_distiller(palette = "Spectral") + geom_tile(color = "white")+geom_text(aes(label = round(Allocation*100,0)), color = "white", fontface="bold", size=8)+labs(x = NULL, y = NULL,title = "Equity Sector Breakdown (%)") + scale_x_discrete(position = "top") + theme(plot.title = element_text(face = "bold", size = 20),text = element_text(size=16),legend.position = "none", axis.ticks =element_blank(), panel.grid.major =element_blank(), panel.background =element_blank() ) })
  #  geom_vline(aes(xintercept = as.numeric(From)),data = fdd.1,colour = "grey50", alpha = 0.5)  #scale_fill_distiller(palette = "Spectral") scale_fill_fermenter(n.breaks = 9, palette = "PuOr")
  # + geom_vline(aes(xintercept = as.numeric(To)),data = fdd.1,colour = "grey50", alpha = 0.5)
  # + annotate("rect", xmin = fdd.1$From, xmax = fdd.1$To, ymin= -Inf, ymax=Inf, alpha = .2)})
  # chart.CumReturns(return_xts,wealth.index=TRUE, main="Growth of $1")
  # return_xts <- xts(x = data[, -1],order.by = as.Date(data$Date))
  
  output$plot5 <- renderPlot({ place_plot5() }) 
  
  #by Strategy
  place_plot6<- reactive({asset_alloc_strat.pivot %>% filter(.data$Pf_category %in% .env$input$dist) %>%
      #ggplot(aes(x=reorder(Asset_Type,Allocation),y=round(Allocation*100,0),fill=fund_strategy))+  geom_bar(stat = 'identity', position = 'stack') + guides(x = guide_axis(angle = 45)) + facet_grid(~ Pf_category)+labs(x = NULL, y = NULL,title = NULL) + scale_x_discrete(position = "bottom") + theme(text = element_text(size=12),legend.position = "right", axis.ticks =element_blank(), panel.grid.major =element_blank(), panel.background =element_blank() ) })
      ggplot(aes(x=Pf_category,y=fund_strategy,fill=Allocation))+ geom_tile(color = "white") +scale_fill_gradient2(low = "#FFFFFF", mid= "#F5F5F5", high = "#DAA520", midpoint=.02) + guides(x = guide_axis(angle = 90)) + facet_grid(~ fct_relevel(Asset_Type,'Stocks','Bonds','Convertible','Cash','Preferred','Others'))+labs(x = NULL, y = NULL,title = "Strategy Asset Allocation Breakdown") + scale_x_discrete(position = "bottom") + theme(plot.title = element_text(face = "bold",size=20), text = element_text(size=16),legend.position = "right", axis.ticks =element_blank(), panel.grid.major =element_blank(),panel.grid.minor =element_blank(), panel.background =element_blank() ) })
  
  output$plot6 <- renderPlot({ place_plot6() })  
  
  
  
  #by Fund
  
  #Tree
  #place_plot5 <- reactive({asset_alloc_tree %>% filter(.data$Pf_category %in% .env$input$dist)})
  asset_alloc_tree<-asset_alloc_strat_fund.pivot%>%group_by(Pf_category,fund_strategy,fund_short_name,Asset_Type)%>%summarise(sum=sum(round(Allocation*100,0)))
  
  hierarchy<-c("Pf_category","fund_strategy","fund_short_name","Asset_Type","sum")
  
  Allocation_Tree <- asset_alloc_tree #%>% filter(Pf_category %in% input$dist)
  
  output$plot7 <- renderCollapsibleTree({collapsibleTree(Allocation_Tree, hierarchy, width = "100%",height = "100%")})
  
  #collapsibleTree( warpbreaks, c("wool", "tension", "breaks"))
  
  #heatmap
  gg_facet_nrow <- function(p) {
    n <- length(unique(ggplot_build(p)$data[[1]]$PANEL))
    par <- ggplot_build(p)$layout$facet$params
    wrap_dims(n, par$nrow, par$ncol)
  }
  he <- reactive(gg_facet_nrow(place_plot8()))
  
  place_plot8<- reactive({asset_alloc_strat_fund.pivot %>% filter(.data$Pf_category %in% .env$input$dist) %>%
      #ggplot(aes(x=reorder(Asset_Type,Allocation),y=round(Allocation*100,0),fill=fund_strategy))+  geom_bar(stat = 'identity', position = 'stack') + guides(x = guide_axis(angle = 45)) + facet_grid(~ Pf_category)+labs(x = NULL, y = NULL,title = NULL) + scale_x_discrete(position = "bottom") + theme(text = element_text(size=12),legend.position = "right", axis.ticks =element_blank(), panel.grid.major =element_blank(), panel.background =element_blank() ) })
      ggplot(aes(x=reorder(fund_strategy,Allocation),y=fund_short_name,fill=Allocation))+ geom_tile(color = "white") +scale_fill_gradient2(low = "#F8F8FF", mid= "#F5F5F5", high = "#DAA520", midpoint=.02) + guides(x = guide_axis(angle = 90)) + facet_grid(Pf_category ~ fct_relevel(Asset_Type,'Stocks','Bonds','Convertible','Cash','Preferred','Others'))+labs(x = NULL, y = NULL,title = "Fund Asset Allocation & Strategy Breakdown") + scale_x_discrete(position = "bottom") + theme(plot.title = element_text(face = "bold",size=20),text = element_text(size=12),legend.position = "right", axis.ticks =element_blank(), panel.grid.major =element_blank(),panel.grid.minor =element_blank(), panel.background =element_blank() ) })
  
  output$plot8 <- renderPlot({ place_plot8 () },height = function(){he()*300})   
  
  #asset_alloc_strat_fund.pivot
  
  
  #scale_fill_gradient2(low = "yellow",mid = "green",high = "orange",midpoint = .02)
  
  
    #observeEvent(input$dist, {data <- ret.stream.pivot() %>% filter(Pf_category %in% input$dist) %>% select(Date, Return) %>% mutate(cum.ret=1*(cumprod(1+Return)))})


    #output$check1 <- reactiveValues(checkbox = NULL)
    #observeEvent(input$checkbox, {data <- pivot_wider(ret.stream.pivot,names_from=Pf_category,values_from = Return)})
    #output$check1<-renderText({ input$checkbox })
    #if(input$checkbox == TRUE){data <- ret.stream.pivot %>% filter(Pf_category == input$dist) %>% select(Date, Return)}
    #else {data <- pivot_wider(ret.stream.pivot,names_from=Pf_category,values_from = Return)}

    #output$value <- renderPrint({ input$checkbox })
    #data<-xts(x = data[, -1],order.by = as.Date(data$Date))
    #data <- reactive(ret.stream.pivot %>% filter(Pf_category %in% input$dist) %>% pivot_wider(names_from=Pf_category,values_from = Return))
    #ret.stream.pivot_xts<-xts(x = data[, -1],order.by = as.Date(data$Date))

    #data <- reactive(pivot_wider(data,names_from=Pf_category,values_from = Return))
    #return_xts <- reactive(xts(x = data[, -1],order.by = as.Date(data$Date)))

    # place_plot1<- reactive({ret.stream.pivot.ggp %>% filter(.data$Pf_category %in% .env$input$dist) %>% arrange(Date) %>%
    #         ggplot(aes(x=Date,y=cum.ret,group=Pf_category,color=Pf_category))+geom_line(show.legend = TRUE) +theme_gray() })
    # # chart.CumReturns(return_xts,wealth.index=TRUE, main="Growth of $1")
    # # return_xts <- xts(x = data[, -1],order.by = as.Date(data$Date))
    # 
    # output$plot1 <- renderPlot({ place_plot1() })
    # 
    # output$table1 <- renderTable({
    #     #dt_plot <- ret.stream.pivot %>% filter(Pf_category %in% input$dist)  %>% pivot_wider(names_from=Pf_category,values_from = Return)
    #     #ret.stream.pivot_xts<-xts(x = dt_plot[, -1],order.by = as.Date(dt_plot$Date))
    #     #tablea=table.Stats(ret.stream.pivot_xts, ci = 0.95, digits = 4)
    #     #tbl1<-tablea
    #     #vec<-c("Minimum","Maximum","Geometric Mean","Stdev")
    #     #vec<-c("Stdev")
    #     #data_new1 <- tablea[match(vec, row.names(tablea)), ]
    #     #data_new1
    # 
    #     tablea<-dt %>% group_by(Pf_category)%>% filter(Pf_category %in% input$dist)  %>% summarise(Fund_Return_3_yrs=sum(Pf_alloc*fund_return_3years), Risk_3_yrs=sum(Pf_alloc*fund_stdev_3years), Sharpe_3_yrs=sum(Pf_alloc*fund_sharpe_ratio_3years), Fund_Return_5_yrs=sum(Pf_alloc*fund_return_5years), Risk_5_yrs=sum(Pf_alloc*fund_stdev_5years), Sharpe_5_yrs=sum(Pf_alloc*fund_sharpe_ratio_5years))%>%
    #         select(Fund_Return_3_yrs) %>% arrange(desc(Fund_Return_3_yrs))
    # 
    #     #data_new1<-as.data.frame(data_new1)
    #     #data_new1 <-data_new1[row.names(data_new1) %in% c("Stdev"), ]
    #     #tablea<-tablea[row.names(tablea) %in% c("Minimum","Maximum","Geometric Mean","Stdev"), ]
    #     #tablea<-as.tibble(tablea)
    #     #tablea
    #     #
    #     #tablea[order(factor(row.names(tablea)%in% c("Minimum","Maximum","Geometric Mean","Stdev"), levels=c("Minimum","Maximum","Geometric Mean","Stdev"))),]
    #     #data_new1
    # }) #, include.rownames = TRUE
    # # output$plot2 <- renderPlot({
    # #     dt_plot <- ret.stream.pivot %>% filter(Pf_category %in% input$dist)  %>% pivot_wider(names_from=Pf_category,values_from = Return)
    # #     ret.stream.pivot_xts<-xts(x = dt_plot[, -1],order.by = as.Date(dt_plot$Date))
    # #     #table.AnnualizedReturns(return_xts, scale = NA, Rf = 0, geometric = TRUE, digits = 4)
    # #     chart.CumReturns(ret.stream.pivot_xts,wealth.index=TRUE, main="Historical Performance - Growth of $1", legend.loc = 'top' ,colorset=c("firebrick", "darkgreen", "navy"))
    # #
    # #
    # # })
    # 
    # output$plot2 <-renderPlot({
    # 
    #     dt_plot <- ret.stream.pivot %>% filter(Pf_category %in% input$dist)  %>% pivot_wider(names_from=Pf_category,values_from = Return)
    #     ret.stream.pivot_xts<-xts(x = dt_plot[, -1],order.by = as.Date(dt_plot$Date))
    #     #table.AnnualizedReturns(return_xts, scale = NA, Rf = 0, geometric = TRUE, digits = 4)
    #     chart.Drawdown(ret.stream.pivot_xts,wealth.index=TRUE, main="Drawdown", plot.engine = "ggplot2", legend.loc = NULL)
    # 
    # })
    # 
    #output$txt1 <-renderTable({data()})

}

# Run the application
shinyApp(ui = ui, server = server)
