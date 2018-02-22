#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(plotly)
library(reshape2)
library(ggplot2)
library(shinydashboard)
   
shinyUI(
  dashboardPage(
    dashboardHeader(title = "Zakat Dashboard",titleWidth = 200),
    
    dashboardSidebar(
      width = 200,
      sidebarMenu(
        menuItem("Raw Data", tabName = "rawdata"),
        menuItem("Dashboard", tabName = "dashboard")
      )
    ),
    dashboardBody(
      tabItems(
        tabItem(tabName = "rawdata", 
                fluidPage(
                  sidebarLayout(
                    sidebarPanel(
                      fileInput("file1", "Choose CSV File",
                                accept = c(
                                  "text/csv",
                                  "text/comma-separated-values,text/plain",
                                  ".csv")
                      ),
                      tags$hr(),
                      checkboxInput("header", "Header", TRUE)
                    ),
                    mainPanel(
                      tableOutput("contents")
                    )
                  )
                )
        ),
        # this section I change/wrote
      
        
        tabItem(tabName = "dashboard",
               h5("Summary Distribution of Pusat Zakat UiTM Shah Alam"),
        
        
  
        fluidRow(
          valueBoxOutput ("student")
        ),
  
        fluidPage(
          box (plotlyOutput("plot", height = 230),height = 270),
          box (plotlyOutput("plot0", height = 230), height = 270)
        ),
        
        fluidPage(
          box (plotlyOutput("plot1", height = 330),height = 400),
          box (plotlyOutput("plot2", height = 330),height = 400, 
                 radioButtons("dist", "Campus:",
                              list("UiTM Shah Alam" = "p2",
                                   "UiTM Kampus Puncak Alam" = "p3",
                                   "UiTM Kampus Puncak Perdana" = "p4",
                                   "UiTM Kampus Hospital Sg. Buloh" = "p5"), selected = "p2"))),
           
        tabItem("file",
                downloadButton("report", "Generate report", "\n"))
        )
              )
            )
          )
        )
    

 



