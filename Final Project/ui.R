library(shiny)

# ui ----
risk_levels<-c("Conservative","Moderate","Aggressive")
shinyUI(fluidPage(
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
                 checkboxGroupInput("dist","Select risk tolerance level(s):",choices=(risk_levels), selected = "Moderate"),
                 # br() element to introduce extra vertical spacing ----
                 selectInput("fund_name", "Select a fund:", choices=NULL, selected = NULL),
                checkboxInput("perf_data", "Display Performance Analysis Charts", value=FALSE)),
    
    
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
                  
                  tabPanel("Risk Analysis",
                           hr(style = "border-top: 0px solid #000000;"),hr(style = "border-top: 0px solid #000000;"),
                           fluidRow(
                             column(12,plotOutput('plot9'))
                           ),
                           hr(style = "border-top: 0px solid #000000;"),hr(style = "border-top: 0px solid #000000;"),
                           fluidRow(
                             column(12,plotOutput('plot10'))
                           )
                           
                  ),
                  tabPanel("Performance Analysis",
                           hr(style = "border-top: 0px solid #000000;"),hr(style = "border-top: 0px solid #000000;"),
                           fluidRow(
                             column(12,plotOutput('plot11'))
                           ),
                           # hr(style = "border-top: 0px solid #000000;"),hr(style = "border-top: 0px solid #000000;"),
                           # fluidRow(
                           #   column(12,plotOutput('plot12'))
                           # )
                           
                  ),
                  tabPanel("Fund Scorecard",
                           hr(style = "border-top: 0px solid #000000;"),hr(style = "border-top: 0px solid #000000;"),
                           fluidRow(
                             column(4,tableOutput('table2')),
                             column(4,plotOutput('plot12')),
                             column(4,plotOutput('plot13'))
                           ),
                           fluidRow(
                             column(4,plotOutput('plot14')),
                             column(4,plotOutput('plot15'))
                             #column(4,plotOutput('plot16'))
                           )
                  )
      )
      
    )
  )
  
  
)
)