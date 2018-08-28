
library(shiny)
library(shinydashboard)
library(DT)
library(shinyjs)


ui <- 
shinyUI(
  
  dashboardPage(skin = "black",title="WATERS Status Assessment Tool",
    dashboardHeader(title = tags$a(tags$img(src='waters_2.gif',height='20',width='204'))

                                  ),
    dashboardSidebar(
      sidebarMenuOutput(outputId = "dy_menu")),
    dashboardBody(
      tags$head(tags$style(HTML("td.small{width:10px;}"))),
      tags$script("$(document).on('click', '#dtind button', function () {
                    Shiny.onInputChange('lastClickId',this.id);
                    Shiny.onInputChange('lastClick', Math.random())
  });"),
      
      
      tabItems(
  # tab content
        tabItem(tabName = "waterbody",
                h3("Select Waterbody"),
                fluidRow(
                  column(2,
                         uiOutput("selectWaterType")),
                  column(2,
                         uiOutput("selectLan")),
                  column(2,
                         uiOutput("selectType")),
                  column(3,
                         uiOutput("selectPeriod")),
                  column(3,
                         "")
                ),
                fluidRow(column(10, offset = 1,
                                #box(
                                #  title = "Title 1", width=10,solidHeader = TRUE, status = "primary",
                                  DT::dataTableOutput("dtwb")
                                #),
            )),
            fluidRow(
              column(10, offset = 1,
                     h4(textOutput("wb_info")))
              ),
            fluidRow(
              column(3,offset = 1,
                     uiOutput("buttonWB"))
              )
            
          ),
  
        # tab content
        tabItem(tabName = "indicators",
                h3(textOutput("SelectedWB")),
                fluidRow(column(9,textOutput("SelectedType"))),
                fluidRow(column(4,uiOutput("selectPressure")),
                         column(3,h2(" "),
                                uiOutput("goButton"))),
                fluidRow(column(6,
                DT::dataTableOutput("dtind")))),
  
        # # tab content
        # tabItem(tabName = "data",
        #         fluidRow(column(9,""),
        #                  column(3,"")),
        #         fluidRow(column(5, offset = 1,h4("Data available")),
        #                  column(4,h4("WBs of same type with data"))),
        #         
        #   fluidRow(column(4, offset = 1,
        #     ""#DT::dataTableOutput("dtind")
        #   ))),
   
        # tab content
        tabItem(tabName = "status",
                fluidRow(column(width=8,
                h3(textOutput("SelectedWBStatus")),
                textOutput("SelectedTypeStatus")
                ),
                column(width=4,
                         #h3("Indicator Errors"),
                    checkboxInput("IgnoreErr", "Ignore indicator minimum year count", value = FALSE, width = NULL),
                    p("Check this option to include indicators which otherwise would be ignored because they don't meet the requirements for number of years with data.")
                  )
                ),
                #uiOutput("chkIndicators")),
                
                fluidRow(column(width=3,htmlOutput("titleTable1"),DT::dataTableOutput("resTable1")),
                         column(width=4,htmlOutput("titleTable2"),DT::dataTableOutput("resTable2")),
                         column(width=5,htmlOutput("titleTable3"),DT::dataTableOutput("resTable3"))
                                         ),
                fluidRow(column(width=5,offset=7,htmlOutput("titleTable4"),DT::dataTableOutput("resTable4"))
                ),
                fluidRow(column(width=12,htmlOutput("titleTableInd"),
                                DT::dataTableOutput("resTableInd"))
                ),
                fluidRow(column(width=6,h3(textOutput("titleTableObs")))),
                fluidRow(column(width=6,plotOutput("plotObs")),
                         column(width=6,DT::dataTableOutput("resTableObs"))
                )
                         

        ),

        # tab content
        tabItem(tabName = "options",
                h3("Options"),
                h3("Monte Carlo"),
                numericInput("n",
                             label = "Number of simulations", min=1,
                             value = 500),
                 p("Options for Monte Carlo simulations."),
                h3("Indicator List"),
                checkboxInput("chkClassBnds","Show Class Boundaries", value=FALSE, width=NULL)
               #uiOutput("chkIndicators")
                
        )
        
      )
  
  )
  )
)



