library(shiny)
library(shinydashboard)

ui = dashboardPage(
    dashboardHeader(title="Estancias Infantiles"),
    
    dashboardSidebar(
        sidebarMenu(
            menuItem("Histograma", tabName = "Dashboard", icon = icon("dashboard")),
            menuItem("puntos", tabName = "graph", icon = icon("area-chart")),
            menuItem("Data Table", tabName = "data_table", icon = icon("table")),
            menuItem("Imagen", tabName = "img", icon = icon("file-picture-o"))
        )
    ),
    
    dashboardBody(
        tags$head(
            tags$img(src='logo.png',height='80',width='230'))
    )
)


server = function(input, output) {}
shinyApp(ui, server)
