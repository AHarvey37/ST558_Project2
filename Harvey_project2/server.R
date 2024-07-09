#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(ggplot2)
library(tidyverse)

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
  
  setwd("..")
  source("utils.r")
  
  globalDF<-reactiveValues()
  
  observeEvent(input$search,
               {keyword <- switch(input$choices,
                                  articles=as.character("articles"),
                                  events=as.character("events"))
               globalDF$a<-NULL
               for (i in 1:length(input$state)) {
                 stateCode<-paste(as.character(input$state[i]))
                 globalDF$a<-bind_rows(globalDF$a,my_wrapper(keyword,stateCode))
               }
               if(input$choices=="events"){
                 #build contingency table for events
                 globalDF$Events<-globalDF$a|>
                   mutate(isfree=as.factor(isfree))|>
                   count(fullName,types,isfree,name = "count")}
               else{
               #builds contingency table for number of parks
               globalDF$TotParks<-globalDF$a|>
                 count(fullName,states,name="count")}
               
               
               
               output$table <- 
                 DT::renderDT(
                   globalDF$a,
                   options=list(scrollX=TRUE))
                  }
               )
  
  output$text3<-renderText({
    print("Plots")})
  
  
  
  observeEvent(input$build,
               {
                 switch(input$plotChoice,
                        totbyPark={
                          if(input$facet==FALSE){
                   output$outTable<- renderPlot({
                     ggplot(globalDF$TotParks,
                            aes(x=fullName,
                                y=count,
                                fill = fullName))+
                       geom_bar(stat = "identity")+
                       theme(legend.position = "none",
                             axis.text.x = element_text(angle=45,
                                                        vjust = 1,
                                                        hjust = 1))+
                       labs(title = paste("Total", input$choices,"by national park"),
                            x = "National Park",
                            y = paste("Number of", input$choices))+
                       scale_x_discrete(labels= str_wrap(c(globalDF$TotParks$fullName),width =
                                                           25))+
                       scale_y_continuous(expand = expansion())
                   })
                   }
                          else {
                   output$outTable<- renderPlot({
                     ggplot(globalDF$TotParks,
                            aes(x=fullName,
                                y=count,
                                fill = fullName))+
                       geom_bar(stat = "identity")+
                       facet_wrap(vars(states))+
                       theme(legend.position = "none",
                             axis.text.x = element_text(angle=45,
                                                        vjust = 1,
                                                        hjust = 1))+
                       labs(title = paste("Total", input$choices,"by national park"),
                            x = "National Park",
                            y = paste("Number of", input$choices))+
                       scale_x_discrete(labels= str_wrap(c(globalDF$TotParks$fullName),width =
                                                           25))+
                       scale_y_continuous(expand = expansion())
                       
                     })
                 }
                          },eventQuant={
                            if(input$facet==FALSE){output$outTable<- renderPlot({
                              ggplot(globalDF$Events,
                                     aes(x=types,y=count))+
                                geom_point(aes(color=isfree))+
                                theme(axis.text.x = element_text(angle=45,
                                                                 vjust = 1,
                                                                 hjust = 1))+
                                labs(title = paste("Total", input$choices,"by national park"),
                                     x = "Dates",
                                     y = paste("Number of", input$choices))
                            })}
                            else{output$outTable<- renderPlot({
                              ggplot(globalDF$Events,
                                     aes(x=types,
                                         y=count))+
                                geom_point(aes(color=isfree))+
                                facet_wrap(vars(fullName))+
                                theme(axis.text.x = element_text(angle=45,
                                                                 vjust = 1,
                                                                 hjust = 1))+
                                labs(title = paste("Total", input$choices,"by national park"),
                                     x = "Dates",
                                     y = paste("Number of", input$choices))
                            })}
                   }
                 )
                 }
                )
  # {
  #                
  #                }
  #                else if(input$plotChoice=="eventQuant"&&input$choices=="events"){
  #                    if(input$facet==FALSE){
  #                      output$outTable<- renderPlot({
  #                      ggplot(globalDF$Events,
  #                                             aes(x=types,y=count))+
  #                        geom_point(aes(color=isfree))+
  #                        theme(axis.text.x = element_text(angle=45,
  #                                                         vjust = 1,
  #                                                         hjust = 1))+
  #                        labs(title = paste("Total", input$choices,"by national park"),
  #                             x = "Dates",
  #                             y = paste("Number of", input$choices))
  #                      })
  #                      }
  #                    else {
  #                      output$outTable<- renderPlot({
  #                        ggplot(globalDF$Events,
  #                               aes(x=types,
  #                                   y=count))+
  #                          geom_point(aes(color=isfree))+
  #                          facet_wrap(vars(fullName))+
  #                          theme(axis.text.x = element_text(angle=45,
  #                                                           vjust = 1,
  #                                                           hjust = 1))+
  #                          labs(title = paste("Total", input$choices,"by national park"),
  #                               x = "Dates",
  #                               y = paste("Number of", input$choices))
  #                      })
  #                    }
  #                  } 
  #                else{output$text3<-renderText({
  #                  print("Must select 'events' in data exploration tab")
  #                })
  #                }
  #                })
  
  observeEvent(input$build,{
    if(input$facet==TRUE){
      output$text3<-renderText({print("this works somehow")})
    }
  })
  
  observe(output$table2<-switch(input$plotChoice,
                                totbyPark={
                                  DT::renderDT(globalDF$TotParks,options=list(scrollX=TRUE))
                                  },
                                eventQuant={
                                  DT::renderDT(globalDF$Events,options=list(scrollX=TRUE))
                                  }
                                )
          )
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
