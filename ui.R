
library(shiny)
library(shinydashboard)
#library(shinyjs)


ui <- 
shinyUI(
  dashboardPage(skin = "black",title="WATERS Status Assessment Tool",
    dashboardHeader(title = tags$a(tags$img(src='waters_2.gif',height='20',width='204'))),
    dashboardSidebar(
      sidebarMenuOutput(outputId = "dy_menu")),
    dashboardBody()
    )
  )


