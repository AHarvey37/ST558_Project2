# Load necessary libraries to run all below code
library(tidyverse)
library(httr)
library(jsonlite)


#---------------------------------------------------------------------

# Store the API key as a string variable to be used in the urls later
key<-source("../ghost.R")[1]

#---------------------------------------------------------------------

# Create a function that pull the entire list of Parks in the NPS API
# Passes a default key, if program modified in the future, allow for a user to
#   a unique key to access API 
getAllParks<-function(key=source("../ghost.R")[1]){
  # Create a URL and uses the "GET" function from httr package to pull the JSON file
  apiParks<-GET(paste("https://developer.nps.gov/api/v1/parks?limit=1000",
                      "&api_key=",
                      key,
                      sep = ""))
  # Convert JSON file into usable data using rawToChar and fromJSON functions
  raw_ParkIDs<- fromJSON(rawToChar(apiParks$content))
  # Pull data and parse it into usable data frame
  data_Parks<- raw_ParkIDs$data|>
    # Use separate_longer_delim since some variables where character classes not lists
    separate_longer_delim(states, delim = ",")|>
    # Select Variables that will be used later
    select(c(fullName,parkCode,states,description,designation,id,latitude,longitude))
  # store in new data frame (superfluous but helps with legibility)
  total<-data_Parks
  # Return final data frame
  return(total)
}

#---------------------------------------------------------------------

# Create a function to retrieve articles based on a url which will be created later
getArticles<-function(Url){
  # Uses the "GET" function from httr package to pull the JSON file
  apiART<-GET(Url)
  # Convert JSON file into usable data using rawToChar and fromJSON functions
  art_parsed <- fromJSON(rawToChar(apiART$content),flatten = TRUE)|>
    # Convert to tibble
    as_tibble()
  # Extract usable data and parse
  art_parsed<-art_parsed$data|>
    # Map_if used to search tibble for variables that are data frames and turns them in lists
    map_if(is.data.frame,list)|>
    # Map_if used to search tibble for variables that are NULL and turns them in lists
    map_if(is.null,list)|>
    # Convert to tibble again
    as_tibble()|>
    # Unnest used to take tibbles from within the "relatedParks" variable and make
    #   them into their own variable named "relatedParks_*variable name*"
    unnest(relatedParks,names_sep = "_")|>
    # Use unnest on tags variable, this unnest results coerses to unnest_longer 
    #   to extrapolate tags observations into own row
    unnest(tags)|>
    # Rename "relatedParks_parkCode" to "parkCode" for consistency and to be used in join later
    rename("parkCode"="relatedParks_parkCode")
  # Return parsed and cleaned data frame
  return(art_parsed)
}

#---------------------------------------------------------------------

# Create a function to retrieve aEvents based on a url which will be created later
getEvents<-function(Url){
  # Uses the "GET" function from httr package to pull the JSON file
    getEventsAPI<-GET(Url)
    # Convert JSON file into usable data using rawToChar and fromJSON functions
    parsedEvents<- fromJSON(
      rawToChar(
        getEventsAPI$content),
      flatten = TRUE)
    # Extract usable data 
    parsedEvents<-parsedEvents$data|>
      as_tibble()
    # Parse data
    parsedEvents<-parsedEvents|>
      # Unnest coersed to wider to create separate variables from data frame within types 
      unnest(types, names_sep = "_")|>
      # Rename for future use
      rename("fullname"="parkfullname",
             "parkCode"="sitecode")|>
      # Delete used variables
      select(-c(updateuser:recurrencedateend,
                isrecurring:portalname,
                createuser,
                contactemailaddress:regresurl,
                images,
                imageidlist:organizationname,
                infourl:eventid,
                feeinfo:recurrencerule,
                datetimecreated,
                subjectname,longitude,latitude,tags))|>
      # Reorder variables
      select(fullname,parkCode,everything())|>
      # Unnest lists in dates to their own rows
      unnest_longer(dates,keep_empty = TRUE)
    # Return parsed and cleaned data frame
    return(parsedEvents)
}

#---------------------------------------------------------------------

# Create a wrapper function to create url, call specific functions to query API,
#  return cleaned data for use
my_wrapper<- function (keyword, stateCode = "NC",
                       key=as.character(source("../ghost.R")[1])){
  # Create Null value for future use
  y<- NULL
  # Build url based on inputs
  newUrl <- paste("https://developer.nps.gov/api/v1/",
                  keyword,
                  "?stateCode=",
                  stateCode,
                  "&api_key=",
                  key,
                  sep = "")
  # Nested ifelse statement to determine what y will be set to 
  ifelse(keyword=="articles",
         y<-getArticles(newUrl),
         ifelse(keyword=="events",
                y<-getEvents(newUrl),
                # If inputs do not work return all park table
                y<-getAllParks(key)
         )
  )
  
  # Set x to all park table 
  x<-getAllParks(key)
  
  # if else block to join appropriate table (for legibility and manipulation later),
  #   and further cleaning
  if(keyword=="articles" || keyword=="events"){
    # Join data via right join
    cleaned<-right_join(x, y , by = "parkCode",keep = FALSE, relationship = "many-to-many")|>
      # Filter to only applicable states
      filter(states == stateCode)|>
      # keep unique titles to minimize duplicates
      distinct(title,.keep_all = TRUE)
    # Return data frame 
    return(cleaned)
  }
  else{
    # Return all parks
    return(x)
  }
}