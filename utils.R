library(tidyverse)
library(httr)
library(jsonlite)


#---------------------------------------------------------------------
key<-source("ghost.r")[1]
#---------------------------------------------------------------------
# allStates<-c("AL","AK","AZ","AR","CA","CO","CT","DE","FL","GA","HI","ID","IL",
#              "IN","IA","KS","KY","LA","ME","MD","MA","MI","MN","MS","MO","MT",
#              "NE","NV","NH","NJ","NM","NY","NC","ND","OH","OK","OR","PA","RI",
#              "SC","SD","TN","TX","UT","VT","VA","WA","WV","WI","WY","DC","GU",
#              "MP","PR","VI")
total<-as_tibble(NULL)
  apiParks<-GET(paste("https://developer.nps.gov/api/v1/parks?limit=1000",
                      "&api_key=",
                      key,
                      sep = ""))
  raw_ParkIDs<- fromJSON(rawToChar(apiParks$content))
  data_Parks<- raw_ParkIDs$data|>
    separate_longer_delim(states, delim = ",")|>
    select(c(fullName,parkCode,description,states,id,latitude,longitude))
  total<-data_Parks



#---------------------------------------------------------------------
# Retrieve articles
getArticles<-function(Url){
  apiART<-GET(Url)
  art_parsed <- fromJSON(rawToChar(apiART$content),flatten = TRUE)|>
    as_tibble()
  art_parsed<-art_parsed$data|>
    map_if(is.data.frame,list)|>
    map_if(is.null,list)|>
    as_tibble()|>
    unnest(relatedParks,names_sep = "_")|>
    unnest(tags)|>
    rename("parkCode"="relatedParks_parkCode")
  return(art_parsed)
}

#---------------------------------------------------------------------
#Events Function
getEvents<-function(Url){
  #TotalEvents<-NULL
  #for (i in 1:length(allStates)){
    getEventsAPI<-GET(Url)
    parsedEvents<- fromJSON(
      rawToChar(
        getEventsAPI$content),
      flatten = TRUE)
    parsedEvents<-parsedEvents$data|>
      as_tibble()
    #TotalEvents<-bind_rows(TotalEvents,parsedEvents)
    
    parsedEvents<-parsedEvents|>
    unnest(types, names_sep = "_")|>
    rename("fullname"="parkfullname",
           "parkCode"="sitecode")|>
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
    select(fullname,parkCode,everything())|>
    unnest_longer(dates,keep_empty = TRUE)
  return(parsedEvents)
}
#---------------------------------------------------------------------

#Wrapper function to return cleaned data
my_wrapper<- function (keyword, stateCode = "NC",
                       key=as.character(source("ghost.r")[1]),
                       parkCodes=total,
                       Articles=totalARTs,
                       Events = TotalEvents){
  y<- NULL
  newUrl <- paste("https://developer.nps.gov/api/v1/",
                  keyword,
                  "?stateCode=",
                  stateCode,
                  "&api_key=",
                  key,
                  sep = "")
  ifelse(keyword=="articles",
         y<-getArticles(newUrl),
         ifelse(keyword=="events",
                y<-getEvents(newUrl),
                stop("Error in keyword, Please choose 'alerts','articles',or 'events'")
                )
         )
  x<-parkCodes
  cleaned<-right_join(x, y , by = "parkCode",keep = FALSE, relationship = "many-to-many")|>
    filter(states == stateCode)|>
    distinct(title,.keep_all = TRUE)
  return(cleaned)
  }