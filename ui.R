
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
      
      #shinyjs::useShinyjs(),
      #js function to reset a button, variableName is the button name whose value we want to reset
      #tags$script("Shiny.addCustomMessageHandler('resetInputValue', function(variableName){
      #            Shiny.onInputChange(variableName, null);
      #            });
      #            "),  
      
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
                        column(2,h2(" "),uiOutput("goButton")),
                        column(1,h2(" "),
                               checkboxInput("IgnoreErr", 
                                             "Use all data",
                                              value = FALSE, width = '100%')),
                        column(3,h2(" "),
                               p("Use all data for the selected waterbody, including indicators which have data for fewer than 3 out of 6 years.")#,
                        #column(1,h2(" "),uiOutput("btnExtrap"))
                        )),
               fluidRow(column(7,DT::dataTableOutput("dtind")),
                        column(3,DT::dataTableOutput("dtextrap"))
               )
               ),
   
        # tab content
        tabItem(tabName = "status",
                fluidRow(column(width=8,
                h3(textOutput("SelectedWBStatus")),
                textOutput("SelectedTypeStatus")
                ),
                column(width=4,

                        checkboxInput("ShowExtrap", "Show extrapolated results. Check this option to include indicators calculated by extrapolating from results from other waterbodies within same type",
                                      value = TRUE, width = '100%')
 
                )
                ),

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



