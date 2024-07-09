# This is the user-interface definition of a Shiny web application.
#
# Load Libraries need to build UI
library(shinydashboard)
library(shiny)

# Create dashboard page
dashboardPage(
  # Create Header for UI
  dashboardHeader(
    # Specify Title string
    title = "National Park API Query"
    ),
  # Create Sidebar
  dashboardSidebar(
    # Build Sidebar menu
    sidebarMenu(
      # Sidebar tab 1 - "About"
      menuItem("About",
               # Set outputID
               tabName = "about"),
      # Sidebar tab 2 - "Data Download"
      menuItem("Data Download",
               # Set outputID
               tabName = "dataDownload"),
      # Sidebar tab 3 - "Data Exploration"
      menuItem("Data Exploration",
               # Set outputID
               tabName = "dataExploration")
    )),
  # Create Dashboard Body (Body of page when clicked on each tab)
  dashboardBody(
    # -----HTML code to set column number, used later on page-----
    tags$head(
      tags$style(HTML("
    .multicol{
      -webkit-column-count:5;
      -moz-column-count:5;
      column-count:5;}"
    ))),
    #-------------------------------------------------------------
    # Build UI for each individual tab
    tabItems(
      # Build Page for "About" tab
      tabItem("about",
              fluidRow(
                # Create a text output to display text from server, variable - "text" 
                textOutput("text"),
                # Creates a ui output that is used to display an image from a url
                uiOutput("NPS")
                )
              ),
      # Build Page for "Data Download" tab
      tabItem("dataDownload",
              fluidRow(
                # Create a text output to display text from server, variable - "text2"
                textOutput("text2"),
                # Create radio buttons on page.
                # This will limit the user input to search API on server side
                # Set outputID to "choices"
                radioButtons("choices",
                             # Set the display label
                             label = "API Keyword:",
                             # Define the radioButton choices in the format Label=outputID
                             choices = c("Articles"="articles",
                                         "Events"="events",
                                         "All Parks"="allParks")
                             ),
                # Insert blank lines for legibility
                br(),
                br(),
                # Use HTML block to set column width
                tags$div(class="multicol",
                         # Create checkboxes that can be multiple selection (group checkboxes)
                         # Set outputID to "state"
                         checkboxGroupInput("state",
                                            # Set display label 
                                            "Select State(s) to search:",
                                            # Create selections for checkboxes in form
                                            # "Label"="choice"
                                            # Choices are every US State and territory that has data in the NPS API
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
                                            # Set the column width
                                            width = 450
                                            )),
                # Insert line break
                br(),
                # Create button which will enable search of API with previous selection choices when clicked
                # Set outputID to "search"
                actionButton("search",
                             # Set label
                             "Search"),
                # Insert line break
                br(),
                br(),
                # Display resulting table from API query
                DT::dataTableOutput("table")
                )
              ),
      # Build Page for "Data Exploration" tab
      tabItem("dataExploration",
              fluidRow(
                # Set text output from server
                textOutput("text3"),
                # Create radio buttons to allow the user to pick which plot to display
                # Set outputID "plotChoice"
                radioButtons("plotChoice",
                             # Set Label
                             "Choose which variables to plot:",
                             # Create selections for checkboxes in form
                             # "Label"="choice"
                             # Choices are different types of plots that will be displayed using different
                             # queries and variables
                             choices = c(
                               "Park & Total Articles(or Events)"="totbyPark",
                               "Event Type & Quantity" ="eventQuant",
                               "Density of Tags by Events/Article"="tagDens",
                               "Density of Parks by Designation (All Parks)" = "dens"
                               )
                             ),
                # Create binary checkbox to turn on and off facetting
                checkboxInput("facet",
                              # Set Label
                              "Facet",
                              # Set initial checkbox value to FALSE
                              FALSE
                              ),
                # Create button to display selected plot type
                actionButton("build",
                             # Set Label
                             "Build"),
                # Insert linebreaks
                br(),
                br(),
                # Display plots
                plotOutput("outTable"),
                # Display contingency table that is used for the above plot
                DT::dataTableOutput("table2")))
      )
    )
  )

