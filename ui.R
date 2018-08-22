
library(shiny)
library(shinydashboard)
library(DT)
#library(shinyjs)


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
                  DT::dataTableOutput("dtwb")
            ),
            h3(textOutput("wb_info"),
               uiOutput("dataButton")
            )
          ),
  
        # tab content
        tabItem(tabName = "indicators",
          h3("Select Indicators")
          ),
        
        # tab content
        tabItem(tabName = "status",
                h3("Status")
        )
        
      )
  
  )
  )
)



