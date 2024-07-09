# ST558_Project2
Project 2 for Data Science for Statisticians 

• Brief description of the app and its purpose.
  The purpose of this app is to explore the National Park Service API. This app will allow the user to explore articles, and events within the API by state. The app will also provide summary exploration for tags used within articles and events as well as allow the used to view the density of locations by state and designation within the API. 

• A list of packages needed to run the app.
 Packages needed:
  shiny
  tidyverse
  shinydashboard
  httr
  jsonlite
  ggplot2
  ggwordcloud
  

• A line of code that would install all the packages used (so we can easily grab that and run it prior to running your app).
  Code to install necessary packages:
    install.packages(c("shiny","tidyverse","ggplot2","shinydashboard","jsonlite","httr","ggwordcloud"))

• The shiny::runGitHub() code that we can copy and paste into RStudio to run your app.
  Code to run the app:
    shiny::runGitHub("AHarvey37/ST558_Project2/Harvey_project2")