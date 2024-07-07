#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)

# Define server logic required to draw a histogram
function(input, output, session) {
  
  
  output$text <- renderText({
    paste0("The purpose of this app is to explore three unique endpoints of the Naqtional Park API.","\n",
        "These three endpoints are 'alerts', 'articles', and 'events'.",
        "The user will be able to select between these three endpoints and type in any two letter state abbrivation. The user will then get a plot and table containing the information which they have just queried.",
        "For more information about the data please visit the National Park Service API at ",
        "'https://www.nps.gov/subjects/developer/index.html'")
    })
  
  output$NPS<- renderUI({
    tags$img(src="https://nps.gov/wrst/learn/historyculture/images/NPS_16.jpg?maxwidth=300&autorotate=false&quality=78&format=webp")
  })
  
  output$text2<- renderText({
    print("Query National Park Service API")
  })
  observeEvent(input$search,
               {keyword <- input$choices
               stateCode<- input$state
               source("utils.r")}
                      )
  
  output$text3<-renderText({
    print("this should be the final test.")
  })
  
    # output$distPlot <- renderPlot({
    # 
    #     # generate bins based on input$bins from ui.R
    #     x    <- faithful[, 2]
    #     bins <- seq(min(x), max(x), length.out = input$bins + 1)
    # 
    #     # draw the histogram with the specified number of bins
    #     hist(x, breaks = bins, col = 'darkgray', border = 'white',
    #          xlab = 'Waiting time to next eruption (in mins)',
    #          main = 'Histogram of waiting times')
# 
#     })

}
