# set working directory -----------------
rm(list = ls()) # clear console
options(scipen = 999) # forces R to avoid exponential notation
system.info <- Sys.info()
setwd("")

# load libraries -------------------------
library(ggmap)
library(revgeo)
library(stringr)
library(geosphere)
library(httr)
library(jsonlite)

# national parks service API key
NPS_api_key <- "your key here"


# Pull all national parks data from the national parks service API --------------

  # pull all data for Alabama and clean to initialize a dataframe
  query <- paste0("https://developer.nps.gov/api/v1/parks?stateCode=al&api_key=",NPS_api_key)
  getdata<- GET(url=query) # query the API
  data <- fromJSON(content(getdata,type="text"))  # convert from JSON format
  nat.parks <-  data$data # create a national parks data frame
  nat.parks$state <- "AL"
  nat.parks$activities2 <- NA
  nat.parks$entrance_fee <- NA
  nat.parks$topics2 <- NA
  
  for(i in 1:nrow(nat.parks)){
    temp <- data.frame(nat.parks$activities[[i]])
    nat.parks$activities2[i] <- paste(temp$name, collapse = " / ")
    entrance.fee <- data.frame(nat.parks$entranceFees[[i]]) 
    topics <- data.frame(nat.parks$topics[[i]])
    if(nrow(entrance.fee) > 0){
      nat.parks$entrance_fee[i] <- as.numeric(entrance.fee$cost)
    }
    if(nrow(topics) > 0){
      nat.parks$topics2[i] <- paste(topics$name, collapse = " / ")
    }
  }
  nat.parks$activities <- nat.parks$activities2
  nat.parks$topics <- nat.parks$topics2
  nat.parks <- nat.parks[,c("state","name","fullName","parkCode","designation","longitude","latitude","latLong","topics","activities","entrance_fee")]
  
  # loop through the rest of the states and merge with existing nat.parks data.frame
  states <- tolower(read.csv("./Data/state_abbreviations.csv",stringsAsFactors = F)[1:50,1])
  for(k in 2:length(states)){
    print(k)
    query <- paste0("https://developer.nps.gov/api/v1/parks?stateCode=",states[k],"&api_key=",NPS_api_key)
    getdata<-GET(url=query)
    data <- fromJSON(content(getdata,type="text"))
    nat.parks.temp <-  data$data
    nat.parks.temp$state <- toupper(states[k])
    nat.parks.temp$activities2 <- NA
    nat.parks.temp$entrance_fee <- NA
    nat.parks.temp$topics2 <- NA
    for(i in 1:nrow(nat.parks.temp)){
      temp <- data.frame(nat.parks.temp$activities[[i]])
      nat.parks.temp$activities2[i] <- paste(temp$name, collapse = " / ")
      entrance.fee <- data.frame(nat.parks.temp$entranceFees[[i]]) 
      topics <- data.frame(nat.parks.temp$topics[[i]])
      if(nrow(entrance.fee) > 0){
        nat.parks.temp$entrance_fee[i] <- as.numeric(entrance.fee$cost)
      }
      if(nrow(topics) > 0){
        nat.parks.temp$topics2[i] <- paste(topics$name, collapse = " / ")
      }
    }
    nat.parks.temp$activities <- nat.parks.temp$activities2
    nat.parks.temp$topics <- nat.parks.temp$topics2
    nat.parks.temp <- nat.parks.temp[,c("state","name","fullName","parkCode","designation",
                                        "longitude","latitude","latLong","topics","activities",
                                        "entrance_fee")]
    
    nat.parks <- rbind.data.frame(nat.parks, nat.parks.temp)
  }
  
  # output the national parks data to a csv file 
  write_csv(nat.parks,"./Data/nps_sites.csv")