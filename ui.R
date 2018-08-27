
library(shiny)
library(shinydashboard)
library(DT)
library(shinyjs)


ui <- 
shinyUI(
  
  dashboardPage(skin = "black",title="WATERS Status Assessment Tool",
    dashboardHeader(title = tags$a(tags$img(src='waters_2.gif',height='20',width='204'))),
    dashboardSidebar(
      sidebarMenuOutput(outputId = "dy_menu")),
    dashboardBody(
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
                  column(2,
                         uiOutput("selectPeriod"))
                ),
                fluidRow(column(10, offset = 1,
                  DT::dataTableOutput("dtwb")
            )),
            h3(textOutput("wb_info"),
               uiOutput("indicatorButton")
            )
          ),
  
        # tab content
        tabItem(tabName = "indicators",
                h3("Select Indicators"),
                uiOutput("chkIndicators"),
                uiOutput("dataButton")),
  
        # tab content
        tabItem(tabName = "data",
                h3(textOutput("DataTitle")),
                h3(textOutput("SelectedWB")),
                textOutput("SelectedType"),
          fluidRow(column(4, offset = 1,
            DT::dataTableOutput("dtind")
          ),
          column(4,
                DT::dataTableOutput("dtindtype")
          ),
          column(1,uiOutput("goButton"))
                    )),
        
        # tab content
        tabItem(tabName = "status",
                h3("Status")#,
                #uiOutput("chkIndicators")
                
        )
        
      )
  
  )
  )
)



