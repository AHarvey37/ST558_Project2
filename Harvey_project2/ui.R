#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#
library(shinydashboard)
library(shiny)

dashboardPage(
  dashboardHeader(
    title = "National Park API"
    ),
  dashboardSidebar(
    sidebarMenu(
      menuItem("About",
               tabName = "about"),
      menuItem("Data Download",
               tabName = "dataDownload"),
      menuItem("Data Exploration",
               tabName = "dataExploration")
    )),
  dashboardBody(
    tabItems(
      tabItem("about",
              fluidRow(
                # box(
                #   width = 10, status = "test", solidHeader = TRUE,
                #   title = "This is a test",
                  textOutput("text"),
                  htmlOutput("NPS")
                  )
                ),
      tabItem("dataDownload",
              fluidRow(
                textOutput("text2")
                )
              ),
      tabItem("dataExploration",
              fluidRow(
                textOutput("text3")))
      )
    )
  )

# 
# # Define UI for application that draws a histogram
# fluidPage(
# 
#     # Application title
#     titlePanel("Old Faithful Geyser Data"),
# 
#     # Sidebar with a slider input for number of bins
#     sidebarLayout(
#         sidebarPanel(
#             sliderInput("bins",
#                         "Number of bins:",
#                         min = 1,
#                         max = 50,
#                         value = 30)
#         ),
# 
#         # Show a plot of the generated distribution
#         mainPanel(
#             plotOutput("distPlot")
#         )
#     )
# )
