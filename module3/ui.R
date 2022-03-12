#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
    # dt <- read.csv("https://raw.githubusercontent.com/charleyferrari/CUNY_DATA608/master/lecture3/data/cleaned-cdc-mortality-1999-2010-2.csv", header= TRUE),
    # dt.states <- dt %>% select(State) %>% distinct(),
    # dt.states <-  unique(dt$State),
    # #dt.states <-  as.data.frame(dt.states),
    # dt.diseases <- dt %>% select(ICD.Chapter) %>% distinct(),
    # dt.diseases <-  as.data.frame(dt.diseases),
    #diseases_2010<-unique(dt%>%group_by(ICD.Chapter)%>%filter(Year==2010)%>%select(ICD.Chapter)),
    #diseases_2010<-(as.data.frame(diseases_2010)),
    #num<-1:length(unlist(diseases_2010)),
    #mylist<-as.list(num),
    #names(mylist)<-unlist(diseases_2010),
    #mylist<-as.list(mylist),
    
    # Application title
    titlePanel(""),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
           selectInput("state", "Select State (changes first plot):", choices=NULL, selected = NULL),
           
           #selectizeInput("m_year", "Select Year:", choices=NULL),
           # selectizeInput("diseases", "Select a Disease:", choices=NULL),
           
            selectInput("diseases","Specify a Disease (changes both plots):", choices = NULL, selected = NULL),
            
            actionButton("Submit", "Submit")
            
        ),

        # Show a plot of the generated distribution
        mainPanel(
            textOutput("text"),
            textOutput("text2"),
            plotOutput("Plot.2"),
            plotOutput("Plot.1"),
            #dataTableOutput("data.output")
        )
    )
))
