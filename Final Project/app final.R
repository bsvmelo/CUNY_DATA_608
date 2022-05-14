
library(tidyverse)
library(markdown)
library(shiny)
library(shinydashboard)
library(quantmod)
library(PerformanceAnalytics)

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
ui <- dashboardPage(
    dashboardHeader(title = "Portfolio Comparison Dashboard"),
    dashboardSidebar(
            checkboxGroupInput("dist","Select risk tolerance level(s):",choices=(risk_levels), selected = "Moderate")
    ),
    
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
    dashboardBody(
        # infoBoxes with fill=FALSE
       #fluidRow(width=12,box(p("3-year Return"))),

        # fluidRow(
        #          column(2,box(p(""),background = 'black')),
        #          column(10,valueBoxOutput("info_box",width=4))
        #          ),
      
        fluidRow(
          column(6,plotOutput('table1')),
          column(6,tableOutput('table2'))
        ),
      
        # fluidRow(
        #     # A static infoBox
        #     #infoBox("Return", , icon = icon("credit-card")),
        #     # Dynamic infoBoxes
        #     column(2,box(p("3-year Return"))),
        #     column(10,valueBoxOutput("returnBox1",width = 4))
        #     ),
        # fluidRow(
        #     column(2,box(p("Volatility"))),
        #     column(10,valueBoxOutput("returnBox2",width = 4))
        #     ),
        # fluidRow(
        #   column(2,box(p("Sharpe-Ratio"))),
        #   column(10,valueBoxOutput("returnBox3",width = 4))
        #     ),   
            
        fluidRow(
          column(6,plotOutput('plot1')),
          column(6,plotOutput('plot2'))
        )   
        
    )
)
server <- function(input, output) {
    # output$returnBox <- renderValueBox({
    #     
    #     tablea<-dt %>% group_by(Pf_category)%>% filter(Pf_category %in% input$dist)  %>% summarise(Fund_Return_3_yrs=sum(Pf_alloc*fund_return_3years), Risk_3_yrs=sum(Pf_alloc*fund_stdev_3years), Sharpe_3_yrs=sum(Pf_alloc*fund_sharpe_ratio_3years), Fund_Return_5_yrs=sum(Pf_alloc*fund_return_5years), Risk_5_yrs=sum(Pf_alloc*fund_stdev_5years), Sharpe_5_yrs=sum(Pf_alloc*fund_sharpe_ratio_5years))%>%
    #         select(Fund_Return_3_yrs) %>% arrange(desc(Fund_Return_3_yrs)) 
    #     valueBox("Return",tablea, icon = icon("list"),color = "purple")
    #     # infoBox(
    #     #     "Return",tablea, icon = icon("list"),
    #     #     color = "purple"
    #     #)
    # })
    # output$TitleBox <-renderInfoBox({
    # 
    #   infoBox("Return", icon=NULL, color="aqua", width=4)  
    #         
    # })
    
    #Return 3Years
#   output$info_box <- renderUI({
#     
#     tablea<-dt %>% group_by(Pf_category)%>% filter(Pf_category %in% input$dist)  %>% summarise(Fund_Return_3_yrs=sum(Pf_alloc*fund_return_3years), Risk_3_yrs=sum(Pf_alloc*fund_stdev_3years), Sharpe_3_yrs=sum(Pf_alloc*fund_sharpe_ratio_3years), Fund_Return_5_yrs=sum(Pf_alloc*fund_return_5years), Risk_5_yrs=sum(Pf_alloc*fund_stdev_5years), Sharpe_5_yrs=sum(Pf_alloc*fund_sharpe_ratio_5years))%>%
#       select(Pf_category,Fund_Return_3_yrs) %>% arrange(desc(Fund_Return_3_yrs))
#     ss <- (length(tablea$Fund_Return_3_yrs))
#     lapply(1:ss, function(a) {
#       
#       valueBox(subtitle = NULL, value=tags$p(tags$span(tablea[[a,1]], style = "float:center"), style = "font-size: 50%"),icon=NULL,color = "aqua",width = 4)})
#   })  
#     output$returnBox1 <- renderUI({
#              
#             tablea<-dt %>% group_by(Pf_category)%>% filter(Pf_category %in% input$dist)  %>% summarise(Fund_Return_3_yrs=sum(Pf_alloc*fund_return_3years), Risk_3_yrs=sum(Pf_alloc*fund_stdev_3years), Sharpe_3_yrs=sum(Pf_alloc*fund_sharpe_ratio_3years), Fund_Return_5_yrs=sum(Pf_alloc*fund_return_5years), Risk_5_yrs=sum(Pf_alloc*fund_stdev_5years), Sharpe_5_yrs=sum(Pf_alloc*fund_sharpe_ratio_5years))%>%
#                  select(Pf_category,Fund_Return_3_yrs) %>% arrange(desc(Fund_Return_3_yrs))
#              ss <- (length(tablea$Fund_Return_3_yrs))
# 
#              #category<-reactiveValues(input$dist)
#              #ss<-dim(tablea[1])
#              #ss<-2
#             
#              lapply(1:ss, function(a) {
#                 
#                 valueBox(subtitle = NULL ,value=tags$p(paste0(round(tablea[[a,2]]*100,1),"%"),style = "font-size: 50%;"),icon=NULL,color = "purple")})
#             })
# #
#     output$returnBox2 <- renderUI({
#       
#       tablea<-dt %>% group_by(Pf_category)%>% filter(Pf_category %in% input$dist)  %>% summarise(Fund_Return_3_yrs=sum(Pf_alloc*fund_return_3years), Risk_3_yrs=sum(Pf_alloc*fund_stdev_3years), Sharpe_3_yrs=sum(Pf_alloc*fund_sharpe_ratio_3years), Fund_Return_5_yrs=sum(Pf_alloc*fund_return_5years), Risk_5_yrs=sum(Pf_alloc*fund_stdev_5years), Sharpe_5_yrs=sum(Pf_alloc*fund_sharpe_ratio_5years))%>%
#         select(Pf_category,Risk_3_yrs,Fund_Return_3_yrs) %>% arrange(desc(Fund_Return_3_yrs))
#       ss <- (length(tablea$Risk_3_yrs))
#       
#       #category<-reactiveValues(input$dist)
#       #ss<-dim(tablea[1])
#       #ss<-2
#       
#       lapply(1:ss, function(a) {
#         
#         valueBox(subtitle = NULL,value=tags$p(paste0(round(tablea[[a,2]]*1,1),"%"),style = "font-size: 50%;"),icon=NULL,color = "purple")})
#     })        
#     output$returnBox3 <- renderUI({
#       
#       tablea<-dt %>% group_by(Pf_category)%>% filter(Pf_category %in% input$dist)  %>% summarise(Fund_Return_3_yrs=sum(Pf_alloc*fund_return_3years), Risk_3_yrs=sum(Pf_alloc*fund_stdev_3years), Sharpe_3_yrs=sum(Pf_alloc*fund_sharpe_ratio_3years), Fund_Return_5_yrs=sum(Pf_alloc*fund_return_5years), Risk_5_yrs=sum(Pf_alloc*fund_stdev_5years), Sharpe_5_yrs=sum(Pf_alloc*fund_sharpe_ratio_5years))%>%
#         select(Pf_category,Sharpe_3_yrs,Fund_Return_3_yrs) %>% arrange(desc(Fund_Return_3_yrs))
#       ss <- (length(tablea$Sharpe_3_yrs))
#       
#       #category<-reactiveValues(input$dist)
#       #ss<-dim(tablea[1])
#       #ss<-2
#       
#       lapply(1:ss, function(a) {
#         
#         valueBox(subtitle = NULL,value=tags$p(paste0(round(tablea[[a,2]]*1,1),""),style = "font-size: 50%;"),icon=NULL,color = "purple")})
#     })  
        
        ret.stream.pivot.1 <- ret.stream.pivot %>% filter(Pf_category == "Conservative") %>% select(Date,Return)
        ret.stream.pivot.11<-xts(x = ret.stream.pivot.1[, -1],order.by = as.Date(data$Date))
        fdd.1<-table.Drawdowns(ret.stream.pivot.11)
    
        place_plot1<- reactive({ret.stream.pivot.ggp %>% filter(.data$Pf_category %in% .env$input$dist) %>% arrange(Date) %>%
                ggplot(aes(x=Date,y=cum.ret,group=Pf_category,color=Pf_category))+geom_line(show.legend = FALSE, size=1) + geom_vline(aes(xintercept = as.numeric(From)),data = fdd.1,colour = "grey50", alpha = 0.5)+ geom_vline(aes(xintercept = as.numeric(To)),data = fdd.1,colour = "grey50", alpha = 0.5) +
            theme_gray()+ annotate("rect", xmin = fdd.1$From, xmax = fdd.1$To, ymin= -Inf, ymax=Inf, alpha = .1) +labs(title = "Growth of $1",x = NULL,  y = NULL) +theme(plot.title = element_text(face = "bold",size = 20))})
            #  geom_vline(aes(xintercept = as.numeric(From)),data = fdd.1,colour = "grey50", alpha = 0.5)
            # + geom_vline(aes(xintercept = as.numeric(To)),data = fdd.1,colour = "grey50", alpha = 0.5)
            # + annotate("rect", xmin = fdd.1$From, xmax = fdd.1$To, ymin= -Inf, ymax=Inf, alpha = .2)})
        # chart.CumReturns(return_xts,wealth.index=TRUE, main="Growth of $1")
        # return_xts <- xts(x = data[, -1],order.by = as.Date(data$Date))

        output$plot1 <- renderPlot({ place_plot1() })
    
        output$plot2 <-renderPlot({
          
          dt_plot <- ret.stream.pivot %>% filter(Pf_category %in% input$dist)  %>% pivot_wider(names_from=Pf_category,values_from = Return)
          ret.stream.pivot_xts<-xts(x = dt_plot[, -1],order.by = as.Date(dt_plot$Date))
          #table.AnnualizedReturns(return_xts, scale = NA, Rf = 0, geometric = TRUE, digits = 4)
          chart.Drawdown(ret.stream.pivot_xts,wealth.index=TRUE, main="Drawdown", plot.engine = "ggplot", legend.loc = NULL)
          
        })
        
        
        output$table1 <-renderPlot({
          
          
        tablea <- dt %>% group_by(Pf_category)%>% filter(Pf_category %in% input$dist)  %>% 
          summarise(Fund_Return_3_yrs=sum(round(Pf_alloc*fund_return_3years*100,0)), Risk_3_yrs=sum(round(Pf_alloc*fund_stdev_3years,0)), Sharpe_3_yrs=sum(round(Pf_alloc*fund_sharpe_ratio_3years,1)), Fund_Return_5_yrs=sum(Pf_alloc*fund_return_5years), Risk_5_yrs=sum(Pf_alloc*fund_stdev_5years), Sharpe_5_yrs=sum(Pf_alloc*fund_sharpe_ratio_5years))%>%
                      select(Pf_category, Fund_Return_3_yrs, Risk_3_yrs, Sharpe_3_yrs) %>% arrange(desc(Fund_Return_3_yrs))
          
          
        tableb<-pivot_longer(tablea,cols = ends_with("yrs"),names_to="stats",values_to="values")
        
        level_order <- factor(tableb$stats, level = c('Sharpe_3_yrs', 'Risk_3_yrs', 'Fund_Return_3_yrs'))
        
        ggplot(tableb, aes(x = Pf_category, y = level_order, fill = factor(Pf_category))) + geom_tile(color = "white") + geom_text(aes(label = values), color = "white", fontface="bold", size=9) +
          labs(x = NULL, y = NULL,title = "3-year Metrics")+  scale_y_discrete(labels=c('Sharpe Ratio','Volatility','Return')) + scale_x_discrete(position = "top") + theme(plot.title = element_text(face = "bold", size = 25),text = element_text(size=20),legend.position = "none", axis.ticks =element_blank(), panel.grid.major =element_blank(), panel.background =element_blank() )
          
        })
        
        
        
        
        
        
        
    }

#icon = tags$i(class="fas fa-arrows-up-to-line", style="font-size: 24px; color: white")
#<i class="fa-duotone fa-meteor"></i>
shinyApp(ui, server)    
    
#     fluidPage(
#     
#     # App title ----
#     titlePanel("Portfolio Comparison Visualization"),
#     
#     # Sidebar layout with input and output definitions ----
#     sidebarLayout(
#         
#         # Sidebar panel for inputs ----
#         sidebarPanel(
#             
#             # Input: Select the tolerance type ----
#             #radioButtons("dist","Select risk tolerance level(s):",choices=(risk_levels), selected = "Moderate"),
#             checkboxGroupInput("dist","Select risk tolerance level(s):",choices=(risk_levels), selected = "Moderate")),
#         # br() element to introduce extra vertical spacing ----
#         
#         
#         
#         # Main panel for displaying outputs ----
#         mainPanel(
#             
#             # Output: Tabset w/ plot, summary, and table ----
#             tabsetPanel(type = "tabs",
#                         tabPanel("Summary", fluidRow(column(12,tableOutput('table10'))
#                                                      
#                         ),
#                         fluidRow(
#                             column(6,plotOutput('plot1')),
#                             column(6,plotOutput('plot2'))
#                         ),
#                         fluidRow(
#                             column(3,plotOutput('plot3')),
#                             column(3,plotOutput('plot4')),
#                             column(3,plotOutput('plot5')),
#                             column(3,plotOutput('plot6'))
#                             
#                         )),
#                         
#                         tabPanel("Performance Overview", tableOutput('table1')),
#                         
#                         tabPanel("Risk Analysis", tableOutput("table"))
#             )
#             
#         )
#     )
#     
#     
# )

# Define server logic required to draw a histogram
# server <- function(input, output) {
#     
#     #observeEvent(input$dist, {data <- ret.stream.pivot() %>% filter(Pf_category %in% input$dist) %>% select(Date, Return) %>% mutate(cum.ret=1*(cumprod(1+Return)))})    
#     
#     
#     #output$check1 <- reactiveValues(checkbox = NULL)
#     #observeEvent(input$checkbox, {data <- pivot_wider(ret.stream.pivot,names_from=Pf_category,values_from = Return)})
#     #output$check1<-renderText({ input$checkbox })
#     #if(input$checkbox == TRUE){data <- ret.stream.pivot %>% filter(Pf_category == input$dist) %>% select(Date, Return)} 
#     #else {data <- pivot_wider(ret.stream.pivot,names_from=Pf_category,values_from = Return)}
#     
#     #output$value <- renderPrint({ input$checkbox })
#     #data<-xts(x = data[, -1],order.by = as.Date(data$Date))
#     #data <- reactive(ret.stream.pivot %>% filter(Pf_category %in% input$dist) %>% pivot_wider(names_from=Pf_category,values_from = Return)) 
#     #ret.stream.pivot_xts<-xts(x = data[, -1],order.by = as.Date(data$Date))
#     
#     #data <- reactive(pivot_wider(data,names_from=Pf_category,values_from = Return))
#     #return_xts <- reactive(xts(x = data[, -1],order.by = as.Date(data$Date)))
#     
#     place_plot1<- reactive({ret.stream.pivot.ggp %>% filter(.data$Pf_category %in% .env$input$dist) %>% arrange(Date) %>%
#             ggplot(aes(x=Date,y=cum.ret,group=Pf_category,color=Pf_category))+geom_line(show.legend = TRUE) +theme_gray() })
#     # chart.CumReturns(return_xts,wealth.index=TRUE, main="Growth of $1")
#     # return_xts <- xts(x = data[, -1],order.by = as.Date(data$Date))
#     
#     output$plot1 <- renderPlot({ place_plot1() })
#     
#     output$table1 <- renderTable({
#         #dt_plot <- ret.stream.pivot %>% filter(Pf_category %in% input$dist)  %>% pivot_wider(names_from=Pf_category,values_from = Return)
#         #ret.stream.pivot_xts<-xts(x = dt_plot[, -1],order.by = as.Date(dt_plot$Date))
#         #tablea=table.Stats(ret.stream.pivot_xts, ci = 0.95, digits = 4)
#         #tbl1<-tablea
#         #vec<-c("Minimum","Maximum","Geometric Mean","Stdev")
#         #vec<-c("Stdev")
#         #data_new1 <- tablea[match(vec, row.names(tablea)), ]
#         #data_new1
#         
#         tablea<-dt %>% group_by(Pf_category)%>% filter(Pf_category %in% input$dist)  %>% summarise(Fund_Return_3_yrs=sum(Pf_alloc*fund_return_3years), Risk_3_yrs=sum(Pf_alloc*fund_stdev_3years), Sharpe_3_yrs=sum(Pf_alloc*fund_sharpe_ratio_3years), Fund_Return_5_yrs=sum(Pf_alloc*fund_return_5years), Risk_5_yrs=sum(Pf_alloc*fund_stdev_5years), Sharpe_5_yrs=sum(Pf_alloc*fund_sharpe_ratio_5years))%>%
#             select(Fund_Return_3_yrs) %>% arrange(desc(Fund_Return_3_yrs)) 
#         
#         #data_new1<-as.data.frame(data_new1)
#         #data_new1 <-data_new1[row.names(data_new1) %in% c("Stdev"), ]
#         #tablea<-tablea[row.names(tablea) %in% c("Minimum","Maximum","Geometric Mean","Stdev"), ]
#         #tablea<-as.tibble(tablea)
#         #tablea
#         #
#         #tablea[order(factor(row.names(tablea)%in% c("Minimum","Maximum","Geometric Mean","Stdev"), levels=c("Minimum","Maximum","Geometric Mean","Stdev"))),]
#         #data_new1
#     }) #, include.rownames = TRUE
#     # output$plot2 <- renderPlot({
#     #     dt_plot <- ret.stream.pivot %>% filter(Pf_category %in% input$dist)  %>% pivot_wider(names_from=Pf_category,values_from = Return)
#     #     ret.stream.pivot_xts<-xts(x = dt_plot[, -1],order.by = as.Date(dt_plot$Date))
#     #     #table.AnnualizedReturns(return_xts, scale = NA, Rf = 0, geometric = TRUE, digits = 4)
#     #     chart.CumReturns(ret.stream.pivot_xts,wealth.index=TRUE, main="Historical Performance - Growth of $1", legend.loc = 'top' ,colorset=c("firebrick", "darkgreen", "navy"))
#     # 
#     # 
#     # })
#     
#     output$plot2 <-renderPlot({
#         
#         dt_plot <- ret.stream.pivot %>% filter(Pf_category %in% input$dist)  %>% pivot_wider(names_from=Pf_category,values_from = Return)
#         ret.stream.pivot_xts<-xts(x = dt_plot[, -1],order.by = as.Date(dt_plot$Date))
#         #table.AnnualizedReturns(return_xts, scale = NA, Rf = 0, geometric = TRUE, digits = 4)
#         chart.Drawdown(ret.stream.pivot_xts,wealth.index=TRUE, main="Drawdown", plot.engine = "ggplot2", legend.loc = NULL)
#         
#     })
#     
#     #output$txt1 <-renderTable({data()})
#     
# }
# 
# # Run the application 
# shinyApp(ui = ui, server = server)
