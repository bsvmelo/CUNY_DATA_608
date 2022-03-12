#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)

dt <- read.csv("https://raw.githubusercontent.com/charleyferrari/CUNY_DATA608/master/lecture3/data/cleaned-cdc-mortality-1999-2010-2.csv", header= TRUE)


dt<-dt%>%group_by(Year)%>%mutate(nat_pop=sum((Population)/n_distinct(ICD.Chapter)))
dt<-dt%>%group_by(ICD.Chapter,Year)%>%mutate(d=sum(Deaths),nat_rate=d/(nat_pop/100000))
dt<-ungroup(dt)

dt.2010 <- filter(dt,dt$Year == '2010') %>% select(ICD.Chapter) %>% distinct()
dt.states <- dt %>% select(State) %>% distinct()

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {

    
    #dtyear<-as.data.frame(dt$Year)
    #dtyear<-unique(dtyear$`dt$Year`)
    #dtdis<-as.data.frame(dt$ICD.Chapter)
    #diseases_2010<-unique(dt%>%group_by(ICD.Chapter)%>%filter(Year==2010)%>%select(ICD.Chapter))
    #dt <- filter(dt, dt$Year == 2010)
    updateSelectizeInput(session, 'state', choices = dt.states$State, server = TRUE)
    
    updateSelectizeInput(session, 'diseases', choices = dt.2010$ICD.Chapter, server = TRUE)
    
    # updateSelectInput(session, "diseases", label= NULL,
    #                   choices = diseases %>% 
    #                       select(ICD.Chapter) %>%
    #                       distinct())
    # updateSelectInput(session, "state", label= NULL,
    #                   choices = dt.states %>% 
    #                       select(dt$State) %>%
    #                       distinct())   
    
    # deaths by state in 2010
    result <- eventReactive(input$Submit,{
        req(input$Submit)
        result <- filter(dt, dt$Year == 2010 & dt$ICD.Chapter == input$diseases)
        })
    
    m.title <- eventReactive(input$Submit,{
        req(input$Submit)
        m.title <- paste0("Number of Deaths per State in 2010 by ",input$diseases)
    })    
    
    
    result_1 <- eventReactive(input$Submit,{
        req(input$Submit)
        result_1 <- filter(dt, dt$State == input$state  & dt$ICD.Chapter == input$diseases) 
    })
    m.title_1 <- eventReactive(input$Submit,{
        req(input$Submit)
        m.title_1 <- paste0("Mortality Rate in ",input$state," vs. National Average")
    })    
    m.title_2 <- eventReactive(input$Submit,{
        req(input$Submit)
        m.title_1 <- input$diseases
    })    
    
    # 
     output$Plot.1 <- renderPlot({

        # generate bins based on input$bins from ui.R
        #x    <- dt[, 4]
        #bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
         
         ggplot(data=result(), aes(x=reorder(State,Deaths),y=Deaths, fill=State))+geom_bar(stat="identity", show.legend = FALSE)+
             labs(title="Number of Deaths per State in 2010", x="State", y ="Number of Deaths") +
             theme_classic() + coord_flip()
         
    },height = 600, width=800)
     output$Plot.2 <- renderPlot({
         
         # generate bins based on input$bins from ui.R
         #x    <- dt[, 4]
         #bins <- seq(min(x), max(x), length.out = input$bins + 1)
         
         # draw the histogram with the specified number of bins
         colors <- c("State Rate" = "darkred", "National Rate" = "steelblue")
         
         ggplot(data=result_1(), aes(x=Year))+geom_line(aes(y=Crude.Rate, color="State Rate"), size=1)+
              geom_line(aes(y=nat_rate, color="National Rate"),size=1)+       
             labs(title=m.title_1(), x="Year", y ="Deaths per 100k people", color = "") +
             theme_minimal() +  scale_color_manual(values = colors) 
         
     }, width=800)

     output$text <- renderText({ 
         paste0("Mortality Rates & Deaths by ", m.title_2())
     })
     output$text2 <- renderText({ 
         ""
     })
    
})
