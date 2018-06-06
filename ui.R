
library(shiny)

# Define UI for random distribution application 
shinyUI(
  navbarPage(id = "inTabset",
             windowTitle="WATERS Status Assessment Tool",
             title=div(img(src="waters_2.gif")), 
             tabPanel("Selection",
                      navlistPanel(
                        widths=c(2,10),well=F,
                        tabPanel(
                          "New",
                          
                          h3("New Assessment"),
                          uiOutput("selectWaterDistrict"),
                          uiOutput("selectWaterBodies"),
                          uiOutput("selectPeriod"),
                          uiOutput("dataButton")
                          
                          
                        ),
                        tabPanel("Open",
                                 fileInput('file1', h3('Load existing assessment'),
                                           accept=c('text/csv', 
                                                    'text/comma-separated-values,text/plain', 
                                                    '.csv'))  
                        )
                        
                      )
                      
             ),
             tabPanel(
               "Assessment",
               navlistPanel(
                 widths=c(2,10),well=F,
                 tabPanel("Indicators",
                          #p("Indicators"),
                          #p("Data loaded:"),
                          uiOutput("nrows"),
                          uiOutput("chkIndicators"),
                          uiOutput("goButton")
                 ),
                 tabPanel("Monte Carlo",
                          p("Options for Monte Carlo simulations."),
                          numericInput("n",
                                       label = "Number of simulations", min=1,
                                       value = 200)
                 )
               )),
             
             tabPanel(
               "Results",
               navlistPanel(
                 widths=c(2,10),well=F,
                 tabPanel("Class",
                          fluidRow(column(width=12,htmlOutput("titleTable1"),DT::dataTableOutput("resTable1")) 
                          ),
                          fluidRow(column(width=4,htmlOutput("titleTable2"),DT::dataTableOutput("resTable2")),
                                   column(width=4,htmlOutput("titleTable3"),DT::dataTableOutput("resTable3")),
                                   column(width=4,htmlOutput("titleTable4"),DT::dataTableOutput("resTable4"))
                          ),
                          fluidRow(column(width=12,htmlOutput("titleTableInd"),
                                          DT::dataTableOutput("resTableInd"))
                          ),
                          fluidRow(column(width=12,htmlOutput("titleTableObs"))        
                          ),
                          fluidRow(column(width=6,DT::dataTableOutput("resTableObs")),
                                   
                                   column(width=6,plotOutput("plotObs"))        
                          )
                          
                          
                 ),
                 tabPanel("Indicators",
                          h3("Indicator List"),
                          checkboxInput("chkClassBnds","Show Class Boundaries", value=FALSE, width=NULL),
                          DT::dataTableOutput("resTableMC")
                 ),
                 tabPanel("Errors",
                          fluidRow(column(width=3,
                                          h3("Indicator Errors"),
                                          checkboxInput("IgnoreErr", "Ignore indicator minimum year count", value = FALSE, width = NULL),
                                          p("Check this option to include indicators which otherwise would be ignored because they don't meet the requirements for number of years with data.")
                          ),
                          column(width=3,
                                 DT::dataTableOutput("resTableErr")
                          )
                          )
                          
                 )
               )
             )
  ))