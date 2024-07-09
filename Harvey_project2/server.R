#
# This is the server logic of a Shiny web application. 

# load the utils file to enable access to functions
source("../utils.R")
  

# Load libraries necessary to use server file
library(shiny)
library(ggplot2)
library(tidyverse)
library(ggwordcloud)

# Function to load and use server file
function(input, output, session) {
  
  # Render text for output variable "text
  output$text <- renderText({
    # Used paste option for code legibility
    paste0("The purpose of this app is to explore three unique endpoints of the Naqtional Park API.","\n",
        "These three endpoints are 'alerts', 'articles', and 'events'.",
        "The user will be able to select between these three endpoints and type in any two letter state abbrivation.",
        " The user will then get a plot and table containing the information which they have just queried.",
        "For more information about the data please visit the National Park Service API at ",
        "'https://www.nps.gov/subjects/developer/index.html'")
    })
  
  # Render ui for output variable "NPS"
  output$NPS<- renderUI({
    # Uses HTML to pull an image of the National Park Service logo from a url
    tags$img(src="https://nps.gov/wrst/learn/historyculture/images/NPS_16.jpg?maxwidth=300&autorotate=false&quality=78&format=webp")
  })
  
  # Render text for output variable "text2", used to show loading API
  output$text2<- renderText({
    print("Loading API Query")
  })
  
  
  # Create a global variable for reactive values, this will allow data to be passed between output objects
  globalDF<-reactiveValues()
  
  # Create an observe event for search button on tab 2, observe event needed to observe user input on input variable "search"
  observeEvent(input$search,
               # Sets keyword to articles, events, or allParks using a switch statement block instead of if/else
               {keyword <- switch(input$choices,
                                  articles=as.character("articles"),
                                  events=as.character("events"),
                                  # When allParks chosen create object b in globalDF 
                                  # and calls getAllParks function from util file
                                  allParks={globalDF$b<-getAllParks()})
               # Sets globalDF$b to getAllParks even when allParks radio buttion not selected
               globalDF$b<-getAllParks()
               # For loop to compile multiple queries on the API into a single data frame
               for (i in 1:length(input$state)) {
                 # Sets the statecode based on the input from input variable "state" (large checkbox list)
                 stateCode<-paste(as.character(input$state[i]))
                 # Uses bind row to compile into single data frame
                 globalDF$a<-bind_rows(globalDF$a,my_wrapper(keyword,stateCode))
               }
               # All contingency tables are built here. While it increases loading time it also allows all radio buttons to be used in the next tab
               # Builds contingency table for word cloud
              if(input$choices=="articles"){
                globalDF$Cloud<-globalDF$a|>
                 count(fullName,tags,name = "count")
                }
               # builds contingency table for state parks
               globalDF$stateParks<-globalDF$b|>
                 count(states,designation,name = "count")|>
                 mutate(designation=na_if(designation,""))|>
                 filter(count>5)
               # builds contingency table for number of parks
               globalDF$TotParks<-globalDF$a|>
                 count(fullName,states,name="count")
               # build contingency table for events if events is chosen
               if(input$choices=="events"){
                 globalDF$Events<-globalDF$a|>
                   mutate(isfree=as.factor(isfree))|>
                   count(fullName,types,isfree,name = "count")
               }
               
               # Displays a full queried data frame based on which choice is selected.
               # Displays it on the bottom of tab 2
               output$table <- switch(input$choices,
                                      articles={DT::renderDT(
                                        globalDF$a,
                                        # Enables horizontal scrolling
                                        options=list(scrollX=TRUE))},
                                      events={DT::renderDT(
                                        globalDF$a,
                                        # Enables horizontal scrolling
                                        options=list(scrollX=TRUE))},
                                      allParks={DT::renderDT(
                                        globalDF$b,
                                        # Enables horizontal scrolling
                                        options=list(scrollX=TRUE))})
               # Closes oberverEvent
                  }
               )
  
  # Renders textg for input variable text3
  output$text3<-renderText({print("Plots")})
  
  # Observer Event for action button on tab 3 when clicked
  observeEvent(input$build,
               {
                 # Switch function to determine which plot is displayed based on radio buttons (plotChoice)
                 # Executes AFTER build action button clicked
                 switch(input$plotChoice,
                        # First radio button
                        totbyPark={
                          # If statement based on value of facet checkbox
                          if(input$facet==FALSE){
                            # Renders plot to be displayed
                            output$outTable<- renderPlot({
                              # Uses ggplot and a contingency table to build object
                              ggplot(globalDF$TotParks,
                                     aes(x=fullName,
                                         y=count,
                                         fill = fullName))+
                                # Creates a BAR plot
                                geom_bar(stat = "identity")+
                                # Layers added for legibility
                                theme(legend.position = "none",
                                      axis.text.x = element_text(angle=45,
                                                                 vjust = 1,
                                                                 hjust = 1))+
                                # Create Title, and labels for x axis and y axis
                                labs(title = paste("Total",
                                                   input$choices,
                                                   "by national park"),
                                     x = "National Park",
                                     y = paste("Number of", input$choices))+
                                # Sets with of x axis labels and wraps the text
                                scale_x_discrete(labels= str_wrap(c(globalDF$TotParks$fullName),width =
                                                                    25))+
                                # Removes white space on y axis
                                scale_y_continuous(expand = expansion())
                            # Closes renderPlot
                            })
                            # Closes if statement
                          }
                          # Else statement based on value of facet checkbox
                          else {
                            # Renders plot to be displayed if facet checkbox is clicked
                            output$outTable<- renderPlot({
                              # Uses ggplot and a contingency table to build object
                              ggplot(globalDF$TotParks,
                                     aes(x=fullName,
                                         y=count,
                                         fill = fullName))+
                                # Creates a BAR plot
                                geom_bar(stat = "identity")+
                                # Applies Faceting based on variable states
                                facet_wrap(vars(states))+
                                # Layers added for legibility
                                theme(legend.position = "none",
                                      axis.text.x = element_text(angle=45,
                                                                 vjust = 1,
                                                                 hjust = 1))+
                                # Create Title, and labels for x axis and y axis
                                labs(title = paste("Total", input$choices,"by national park"),
                                     x = "National Park",
                                     y = paste("Number of", input$choices))+
                                # Sets with of x axis labels and wraps the text
                                scale_x_discrete(labels= str_wrap(c(globalDF$TotParks$fullName),width =
                                                                    25))+
                                # Removes white space on y axis
                                scale_y_continuous(expand = expansion())
                            # Closes renderPlot
                            })
                          # Closes else statement
                          }
                          # Second Radio button
                          },eventQuant={
                            # If statement based on value of facet checkbox
                            if(input$facet==FALSE){
                              # Renders plot to be displayed
                              output$outTable<- renderPlot({
                                # Uses ggplot and a contingency table to build object
                                ggplot(globalDF$Events,
                                       aes(x=types,y=count))+
                                  # Creates a DOT plot colors them based on binary isfree variable
                                  geom_point(aes(color=isfree))+
                                  # Layers added for legibility
                                  theme(axis.text.x = element_text(angle=45,
                                                                 vjust = 1,
                                                                 hjust = 1))+
                                  # Create Title, and labels for x axis and y axis
                                  labs(title = paste("Total type of", input$choices),
                                       x = "Types of Events",
                                       y = paste("Number of", input$choices))
                                # Closes renderPlot
                                })
                            # Closes if statement
                            }
                            # Else statement based on value of facet checkbox
                            else{
                              # Renders plot to be displayed
                              output$outTable<- renderPlot({
                                # Uses ggplot and a contingency table to build object
                                ggplot(globalDF$Events,
                                       aes(x=types,
                                           y=count))+
                                  # Creates a DOT plot colors them based on binary isfree variable
                                  geom_point(aes(color=isfree))+
                                  # Applies Faceting based on name of park
                                  facet_wrap(vars(fullName))+
                                  # Layers added for legibility
                                  theme(axis.text.x = element_text(angle=45,
                                                                 vjust = 1,
                                                                 hjust = 1))+
                                  # Create Title, and labels for x axis and y axis
                                  labs(title = paste("Total type of", input$choices),
                                       x = "Types of Events",
                                       y = paste("Number of", input$choices))
                                # Closes renderPlot
                                })
                              # Closes else statement
                            }
                            # Fourth Radio button
                          },dens={
                            # If statement based on value of facet checkbox
                            if(input$facet==FALSE){
                              # Renders plot to be displayed
                              output$outTable<- renderPlot({
                                # Uses ggplot and a contingency table to build object
                                ggplot(globalDF$stateParks,
                                       aes(x=states,
                                           y=designation,
                                           size=count))+
                                  # Creates a Bubble plot based on size
                                  geom_point()+
                                  # Layers added for legibility
                                  theme(axis.text.x = element_text(angle=45,
                                                                   vjust = 1,
                                                                   hjust = 1))+
                                  # Create Title, and labels for x axis and y axis
                                  labs(title = paste("States by Desigination Density Greater than 5"),
                                       x = "States",
                                       y = "Designations")
                                # Closes renderPlot
                                })
                            # Closes if statement
                            }
                            else{
                              # Renders plot to be displayed
                              output$outTable<- renderPlot({
                                # Uses ggplot and a contingency table to build object
                                ggplot(globalDF$stateParks,
                                     aes(x=states,
                                         y=designation,
                                         size=count))+
                                  # Creates a Bubble plot based on size
                                  geom_point()+
                                  # Applies Faceting
                                  facet_wrap(vars(states))+
                                  # Layers added for legibility
                                  theme(axis.text.x = element_text(angle=45,
                                                                   vjust = 1,
                                                                   hjust = 1))+
                                  # Create Title, and labels for x axis and y axis
                                  labs(title = paste("States by Desigination Density Greater than 5"),
                                       x = "States",
                                       y = "Designations")
                                # Closes renderPlot
                                })
                              # Closes else statement
                              }
                            # Third Radio Button
                          },tagDens={
                            # If statement based on value of facet checkbox
                            if(input$facet==FALSE){
                              # Renders plot to be displayed
                              output$outTable<- renderPlot({
                                # Uses ggplot and a contingency table to build object
                                ggplot(globalDF$Cloud,
                                     aes(label=tags,size = count,color=count))+
                                  # Creates a WORD CLOUD plot based on number of tags
                                  ggwordcloud::geom_text_wordcloud()+
                                  # Sets the colors for low and high values of "count"
                                  scale_color_gradient(low = 'blue', high = 'red')+
                                  # Sets the max scale of the cloud
                                  scale_size_area(max_size = 100)+
                                  # Create Title, and labels for x axis and y axis
                                  labs(title = paste("Most frequent tag in",input$choices,"in Park"))
                                # Closes renderPlot
                                })
                              # Closes if statement
                              }
                            else{
                              # Renders plot to be displayed
                              output$outTable<-renderPlot({
                                # Uses ggplot and a contingency table to build object
                                ggplot(globalDF$Cloud,
                                       aes(label=tags,size = count,color=count))+
                                  # Creates a WORD CLOUD plot based on nubmer of tags
                                  ggwordcloud::geom_text_wordcloud()+
                                  # Sets the colors for low and high values of "count"
                                  scale_color_gradient(low = 'blue', high = 'red')+
                                  # Sets the max scale of the cloud
                                  scale_size_area(max_size = 10)+
                                  # Applies Faceting by park name
                                  facet_wrap(vars(fullName))
                                # Closes renderPlot
                                })
                              # Closes else statement
                            }
                            # Close case in switch statment
                          }
                        #Closes switch statement
                 )
                 # Close observeEvent
                 })

  # Renders text for input variable "text3"
  output$text3<-renderText({
    # Prints Build Plots
    print("Build Plots")
    # Close renderText
    })
    
  # Observe when radio buttons on tab 3 are changed using a switch statement,
  # Changes what contingency table is displayed
  observe(output$table2<-switch(input$plotChoice,
                                # When radio button 1 is clicked, display contingency table
                                totbyPark={
                                  DT::renderDT(globalDF$TotParks,options=list(scrollX=TRUE))
                                  },
                                # When radio button 2 is clicked, display contingency table
                                eventQuant={
                                  DT::renderDT(globalDF$Events,options=list(scrollX=TRUE))
                                  },
                                # When radio button 4 is clicked, display contingency table
                                dens={
                                  DT::renderDT(globalDF$stateParks,options=list(scrollX=TRUE))
                                },
                                # When radio button 3 is clicked, display contingency table
                                tagDens={
                                  DT::renderDT(globalDF$Cloud,options=list(scrollX=TRUE))
                                }
                                )
          )
}
