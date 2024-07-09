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
    tags$head(
      tags$style(HTML("
    .multicol{
      -webkit-column-count:5;
      -moz-column-count:5;
      column-count:5;}"
    ))),
    tabItems(
      tabItem("about",
              fluidRow(
                # box(
                #   width = 10, status = "test", solidHeader = TRUE,
                #   title = "This is a test",
                  textOutput("text"),
                  uiOutput("NPS")
                  )
                ),
      tabItem("dataDownload",
              fluidRow(
                textOutput("text2"),
                radioButtons("choices",
                             label = "API Keyword:",
                             choices = c("Articles"="articles",
                                         "Events"="events",
                                         "All Parks"="allParks")
                             ),
                tags$div(class="multicol",
                checkboxGroupInput("state",
                                   "Select State(s) to search:",
                                   choices = c(
                                     "Alabama"="AL","Alaska"="AK","Arizona"="AZ",
                                     "Arkansas"="AR","California"="CA","Colorado"="CO",
                                     "Connecticut"="CT","Delaware"="DE","Florida"="FL",
                                     "Georgia"="GA","Hawaii"="HI","Idaho"="ID",
                                     "Illinois"="IL","Indiana"="IN","Iowa"="IA",
                                     "Kansas"="KS","Kentucky"="KY","Louisiana"="LA",
                                     "Maine"="ME","Maryland"="MD","Massachusetts"="MA",
                                     "Michigan"="MI","Minnesota"="MN","Mississippi"="MS",
                                     "Missouri"="MO","Montana"="MT","Nebraska"="NE",
                                     "Nevada"="NV","New Hampshire"="NH","New Jersey"="NJ",
                                     "New Mexico"="NM","New York"="NY","North Carolina"="NC",
                                     "North Dakota"="ND","Ohio"="OH","Oklahoma"="OK",
                                     "Oregon"="OR","Pennsylvania"="PA","Rhode Island"="RI",
                                     "South Carolina"="SC","South Dakota"="SD","Tennessee"="TN",
                                     "Texas"="TX","Utah"="UT","Vermont"="VT",
                                     "Virginia"="VA","Washington"="WA","West Virginia"="WV",
                                     "Wisconsin"="WI","Wyoming"="WY","Distict of Columbia"="DC",
                                     "Guam"="GU","Northern Mariana Islands"="MP","Puerto Rico"="PR",
                                     "U.S. Virgin Islands"="VI"),
                                   width = 450
                )),
                actionButton("search",
                             "Search"),
                DT::dataTableOutput("table")
                )
              ),
      tabItem("dataExploration",
              fluidRow(
                textOutput("text3"),
                # radioButtons("tabChoice",
                #              "Tables to choose:",
                #              choices = c("Table 1"="tab1"#,
                #                          #"Table 2"="tab2",
                #                          #"Table 3"="tab3",
                #                          #"Table 4"="tab4"
                #                          )),
                radioButtons("plotChoice",
                             "Choose which variables to plot:",
                             choices = c(
                               "Park & Total Articles(or Events)"="totbyPark",
                               "Event Type & Quantity" ="eventQuant",
                               "Density of Parks by Designation" = "dens",
                               "Density of Tags by Events/Article"="tagDens")
                             ),
                checkboxInput("facet",
                              "Facet",
                              FALSE
                              ),
                actionButton("build",
                             "Build"),
                plotOutput("outTable"),
                DT::dataTableOutput("table2")))
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
