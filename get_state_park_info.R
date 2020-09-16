rm(list = ls()) # clear console
options(scipen = 999) # forces R to avoid exponential notation
system.info <- Sys.info()
setwd("")

## Load Libraries ----------------------------
library(RSelenium)
library(stringr)
library(rvest)
library(httr)
library(googleway)
register_google(key = "") 

library(ggmap)
library(revgeo)
library(stringr)
library(geosphere)
library(gmapsdistance)
library(httr)
library(jsonlite)

## user written functions -----------
define.element <- function(xpath){
  element <- remDr$findElement(using = "xpath", value = xpath)
  element
}

# start up docker: run these commands in the terminal if docker isn't already running :
# installing docker:
#sudo service docker start
#sudo docker run hello-world
#sudo docker run -d -p 4445:4444 selenium/standalone-firefox:2.53.0
#sudo docker run -d -p 4445:4444 selenium/standalone-firefox:2.53.0
#i-061a028ab2f282c5d

# start up selenium
  remDr <- remoteDriver(port=4445L)
  remDr$open()
  remDr$getStatus()
  remDr$navigate("https://www.reserveamerica.com/")
  remDr$getCurrentUrl()
  remDr$screenshot(display = T)

# create a data frame of state abbreviations
{
  state.vector <-  c('AL',
                     
                     'AK',
                     
                     'AZ',
                     
                     'AR',
                     
                     'CA',
                     
                     'CO',
                     
                     'CT',
                     
                     'DE',
                     
                     'FL',
                     
                     'GA',
                     
                     'HI',
                     
                     'ID',
                     
                     'IL',
                     
                     'IN',
                     
                     'IA',
                     
                     'KS',
                     
                     'KY',
                     
                     'LA',
                     
                     'ME',
                     
                     'MD',
                     
                     'MA',
                     
                     'MI',
                     
                     'MN',
                     
                     'MS',
                     
                     'MO',
                     
                     'MT',
                     
                     'NE',
                     
                     'NV',
                     
                     'NH',
                     
                     'NJ',
                     
                     'NM',
                     
                     'NY',
                     
                     'NC',
                     
                     'ND',
                     
                     'OH',
                     
                     'OK',
                     
                     'OR',
                     
                     'PA',
                     
                     'RI',
                     
                     'SC',
                     
                     'SD',
                     
                     'TN',
                     
                     'TX',
                     
                     'UT',
                     
                     'VT',
                     
                     'VA',
                     
                     'WA',
                     
                     'WV',
                     
                     'WI',
                     
                     'WY')
}
{
  states <- c(
    "Alabama",
    "Alaska",
    "Arizona",
    "Arkansas",
    "California",
    "Colorado",
    "Connecticut",
    "Delaware",
    "Florida",
    "Georgia",
    "Hawaii",
    "Idaho",
    "Illinois",
    "Indiana",
    "Iowa",
    "Kansas",
    "Kentucky",
    "Louisiana",
    "Maine",
    "Maryland",
    "Massachusetts",
    "Michigan",
    "Minnesota",
    "Mississippi",
    "Missouri",
    "Montana",
    "Nebraska",
    "Nevada",
    "New Hampshire",
    "New Jersey",
    "New Mexico",
    "New York",
    "North Carolina",
    "North Dakota",
    "Ohio",
    "Oklahoma",
    "Oregon",
    "Pennsylvania",
    "Rhode Island",
    "South Carolina",
    "South Dakota",
    "Tennessee",
    "Texas",
    "Utah",
    "Vermont",
    "Virginia",
    "Washington",
    "West Virginia",
    "Wisconsin",
    "Wyoming")
}
states <- data.frame(states,state.vector); colnames(states) <- c("state","abb")

# initialize a parks data frame to store park attributes
parks <- data.frame(matrix(ncol = 4, nrow = 0))
colnames(parks) <- c("site_name","state","site_type")

## get a master list of all parks ----------------------
# webscrape www.stateparks.com to obtain a master list 
# of all state parks in the US

# the script is set up in modules so that each module can be turned on or off 
# to avoid rescraping the same site over and over again if another part of the 
# script needs to be moditied
get.parks.list <- 0 # if 1, script will scrape the site
                    # if 0, script will load previously sraped results
                    
if(get.parks.list == 1){
  for(i in 1:length(state.vector)){
    print(i)
    url <- paste0("https://www.stateparks.com/",tolower(state.vector[i]),".html")
    remDr$navigate(url)
    
    
    all.text <- define.element("/html/body/div[5]/div/div[3]/div")
    raw.text <- all.text$getElementText()[[1]]
    raw.text.split <- strsplit(raw.text,"\n")[[1]]
    
    headers <- raw.text.split[startsWith(raw.text.split, "State" ) | startsWith(raw.text.split, "National") ]
    for(k in 1:length(raw.text.split)){
      if(raw.text.split[k] %in% headers){
        type <- raw.text.split[k]
      }
      if(!(raw.text.split[k] %in% headers)){
        parks[nrow(parks)+1,"site_name"] <- raw.text.split[k]
        parks[nrow(parks),"site_type"] <- type
        parks[nrow(parks),"state"] <- state.vector[i]
      }
    }
  }  
  write_csv(parks,"./state_parks_master_list")  
}
if(get.parks.list == 0){
  parks <- read.csv("./state_parks_master_list.csv")
}


## scrape detail information on each park ---------------
# loop through each park in the previously optained master
# park list, navigate to the park's webpage on stateparks.com
# and scrape the raw text from the page to obtain park attributes
get.park.detail <- 0
if(get.park.detail == 1){
  parks$info <- NA
  parks$amenities <- NA
  parks$overview <- NA
  # get park attributes
  for(k in 1:nrow(parks)){
    if(is.na(parks$overview[k])){
      remDr$setImplicitWaitTimeout(milliseconds = 2000)
      print(k)
      park.temp <- gsub("mgmt.","management",tolower(gsub(" ","_",parks$site_name[k])))
      park.temp <- gsub("wildlife_management_area","state_wildlife_management_area",park.temp)
      state.temp <- tolower(states$state[which(states$abb == parks$state[k])])
      url <- paste0("https://www.stateparks.com/",park.temp,"_in_",state.temp,".html")
      remDr$navigate(url)
      
      info <- NA
      amenities <- NA
      overview <- NA
      
      tryCatch({
        info <- remDr$findElement(using = 'class', value = "parkinfo")
        parks$info[k] <- info$getElementText()[[1]]
      }, error = function(e){"error"})
      
      tryCatch({
        amenities <- remDr$findElement(using = 'id', value = "amenities_chart")
        parks$amenities[k] <- amenities$getElementText()[[1]]
      }, error = function(e){"error"})
      
      tryCatch({
        overview <- remDr$findElement(using = 'id', value = "overview")
        parks$overview[k] <- overview$getElementText()[[1]]
      }, error = function(e){"error"})
      
      
      if(k %% 50 == 0 | k == nrow(parks)){
        write_csv(parks, "./parks_data_attributes.csv")
      }
    }
  }
}
if(get.park.detail == 0){
  parks <- read_csv("./Data/parks_data_attributes.csv") 
}

## scrape google reviews info on each park --------------
# this section obtains the google/facebook average user rating (1-5) 
# of the park along with the number of total review. This can be used
# as an index for popularity of "noteworthiness". For example Yosemete 
# national park will have far more total reviews than a regional state park
get.reviews <- 0
if(get.reviews == 1){
  parks$google.rating <- NA
  parks$google.rating_n <- NA
  parks$facebook.rating <- NA
  parks$facebook.rating_n <- NA
  parks$rel.search1 <- NA
  parks$rel.search2 <- NA
  parks$rel.search3 <- NA
  parks$rel.search4 <- NA
  parks$rel.search5 <- NA
  for(k in 1:nrow(parks)){
    tryCatch({
      # get text/google rating
      tryCatch({
        print(k)
        remDr$setImplicitWaitTimeout(milliseconds = 5000)
        Sys.sleep(runif(1,1,2))
        remDr$navigate("http://www.google.com/ncr")
        webElem <- remDr$findElement(using = "css", "[name = 'q']")
        webElem$sendKeysToElement(list(paste(parks$site_name[k],parks$state[k]), "\uE007"))
        txt<-remDr$findElement(using='css selector',"body")$getElementText()
        txt.split <- strsplit(txt[[1]],"\n")[[1]]
        gr.loc <- which(grepl("Google reviews",txt.split))
        gr <- txt.split[c(gr.loc[1]-1,gr.loc[1])]
        parks$google.rating[k] <- as.numeric(as.character(gr[1]))
        parks$google.rating_n[k] <- as.numeric(trimws(gsub(",","",gsub("Google reviews","",gr[2]))))
      }, error = function(e){""})
      
      #facebook rating
      tryCatch({
        fb.loc <- which(grepl("Facebook",txt.split))
        if(length(fb.loc) > 1){
          fr <- txt.split[c(fb.loc[2])]
        } else{
          fr <- txt.split[c(fb.loc[1])]
        }
        fr <- str_split(fr,"/5")[[1]]
        parks$facebook.rating[k] <- as.numeric(fr[1])
        parks$facebook.rating_n[k] <- as.numeric(trimws(gsub("votes","",str_split(fr[2],"·")[[1]][2])))
      }, error = function(e){""})
      
      #related searches
      tryCatch({
        as.loc <- which(grepl("People also search for",txt.split))
        parks$rel.search1[k] <- txt.split[as.loc+2]
        parks$rel.search2[k] <- txt.split[as.loc+3]
        parks$rel.search3[k] <- txt.split[as.loc+4]
        parks$rel.search4[k] <- txt.split[as.loc+5]
        parks$rel.search5[k] <- txt.split[as.loc+6]
      }, error = function(e){""})
      
    }, error = function(e){""})
    if(k %% 25 == 0 | k == nrow(parks)){
      write.csv(parks, "./Data/parks_data_reviews.csv", row.names = F)
    }
  }
}
if(get.reviews == 0){
  parks <- read_csv("./Data/parks_data_reviews.csv")
}


## get lat/lon for each park ----------------------------
get.park.coords <- 0
if(get.park.coords == 1){
    
    # extract lat and long from overview string
     parks$lat <- NA
     parks$lon <- NA
  
    # get missing lat/lon from google API
    for(k in 1:nrow(parks)){
      print( paste0(round((k/nrow(parks))*100,3),"% Complete"))
      if(is.na(parks$lat[k])){
        coords <- geocode(paste(parks$site_name[k], parks$state[k], sep = " "))
        parks$lon[k] <- coords$lon
        parks$lat[k] <- coords$lat
      }
      if(k %% 50 == 0){
        write_csv(parks,".Data/parks_data_geocoded.csv")
      }
      if(k == nrow(parks)){
        write_csv(parks,".Data/parks_data_geocoded.csv")
      }
      
    }
  }
if(get.park.coords == 0){
    parks <- read_csv(".Data/parks_data_geocoded.csv")
}

## get distance to major metro area --------------------
get.city.dist <- 0
if(get.city.dist == 1){
cities <- read.csv("./Data/uscities.csv")
parks$dist_to_city_pop10k <- NA
parks$dist_to_city_pop50k <- NA
parks$dist_to_city_pop100k <- NA
parks$dist_to_city_pop250k <- NA
parks$dist_to_city_pop500k <- NA
parks$dist_to_city_pop1mil <- NA
for(k in 1:nrow(parks)){
  if(is.na(parks$lat[k]) == F){
  print(k)
  temp <- cities
  cmat <- matrix(nrow = nrow(cities), ncol = 2)
  cmat[,1] <- cities$lng
  cmat[,2] <- cities$lat
  temp$dist <-  t(distm(c(parks$lon[k], parks$lat[k]), cmat, fun = distHaversine)*0.000621371)
  temp10k <- temp[which(temp$population > 10000),]
  temp50k <- temp[which(temp$population > 50000),]
  temp100k <- temp[which(temp$population > 100000),]
  temp250k <- temp[which(temp$population > 250000),]
  temp500k <- temp[which(temp$population > 500000),]
  temp1mil <- temp[which(temp$population > 1000000),]
  parks$dist_to_city_pop10k[k] <- temp10k$dist[which(temp10k$dist == min(temp10k$dist))]
  parks$dist_to_city_pop50k[k] <- temp50k$dist[which(temp50k$dist == min(temp50k$dist))]
  parks$dist_to_city_pop100k[k] <- temp100k$dist[which(temp100k$dist == min(temp100k$dist))]
  parks$dist_to_city_pop250k[k] <- temp250k$dist[which(temp250k$dist == min(temp250k$dist))]
  parks$dist_to_city_pop500k[k] <- temp500k$dist[which(temp500k$dist == min(temp500k$dist))]
  parks$dist_to_city_pop1mil[k] <- temp1mil$dist[which(temp1mil$dist == min(temp1mil$dist))]
  }
}
write_csv(parks,"./Data/parks_data_cityDist.csv")
}
if(get.city.dist == 0){
  parks <- read_csv("./Data/parks_data_cityDist.csv")
}


## get trails data -----------------------------------
# getting data on nearby trails by querying the hikingproject API
get.trail.data <- 0
if(get.trail.data == 1){
parks$trails_within_10miles <- NA
parks$average_trail_rating <- NA
parks$average_trail_rating_n <- NA
parks$average_trail_ascent <- NA
parks$average_trail_ascent_per_mile <- NA
parks$average_trail_highpoint <- NA
for(k in 1:nrow(parks)){
  print(k)
  lat <- parks$lat[k]
  lon <- parks$lon[k]
  radius <- 10
  query <- paste0("https://www.hikingproject.com/data/get-trails?lat=",lat,"&lon=",lon,"&maxDistance=10&key=YOUR API KEY")
  res <- GET(query)
  data <- fromJSON(content(res,type="text"))
  trails <- data$trails
  if(length(trails) > 0){
  parks$trails_within_10miles[k] <- nrow(trails)
  parks$average_trail_rating[k] <- mean(trails$stars)
  parks$average_trail_rating_n[k] <- mean(trails$starVotes)
  parks$average_trail_ascent[k] <- mean(trails$ascent)
  parks$average_trail_ascent_per_mile[k] <- mean(trails$ascent/trails$length)
  parks$average_trail_highpoint[k] <- mean(trails$high)
  parks$average_trail_lowpoint[k] <- mean(trails$low)
  parks$average_trail_length[k] <- mean(trails$length)
  } else {
    parks$trails_within_10miles[k] <- 0
    parks$average_trail_rating[k] <- NA
    parks$average_trail_rating_n[k] <- NA
    parks$average_trail_ascent[k] <- NA
    parks$average_trail_ascent_per_mile[k] <- NA
    parks$average_trail_highpoint[k] <- NA
    parks$average_trail_lowpoint[k] <- NA
    parks$average_trail_length[k] <- NA
  }
}
write_csv(parks,"./Data/parks_data_trailData.csv")
}
if(get.trail.data == 0){
parks <- read_csv("./Data/parks_data_trailData.csv")  
}

## get data from recreation.gov ---------------------
# query api to get facility IDs for all facilities within 10 mile radius
get.facilityIDs <- 0
if(get.facilityIDs == 1){
parks$rec.gov_facID <- NA
for(k in 1:nrow(parks)){
  tryCatch({
  print(k)
  # query api to get facility ID
  lat <- parks$lat[k]
  lon <- parks$lon[k]
  search.radius <- 10
  query <- paste0("https://ridb.recreation.gov/api/v1/facilities?limit=50&offset=0&latitude=",lat,"&longitude=",lon,"&radius=",search.radius)
  r <- GET(query, add_headers(apikey = "YOUR API KEY"))
  data <- fromJSON(content(r,type="text"))
  rec <- data$RECDATA
  parks$rec.gov_facID[k] <- list(rec$FacilityID)
  }, error = function(e){cat("ERROR :",conditionMessage(e), "\n")})
}
parks$rec.gov_facID <-  as.character(parks$rec.gov_facID)
write_csv(parks,"./Data/parks_data_recGovFacID.csv")
}
if(get.facilityIDs == 0){
  parks <- read_csv("./Data/parks_data_recGovFacID.csv")
}

# query api to get recreation area IDs for all rec areas within 10 mile radius
get.rec_areaIDs <- 0
if(get.rec_areaIDs == 1){
  parks$rec.gov_recID <- NA
  for(k in 1:nrow(parks)){
    tryCatch({
      print(k)
      # query api to get facility ID
      lat <- parks$lat[k]
      lon <- parks$lon[k]
      search.radius <- 10
      query <- paste0("https://ridb.recreation.gov/api/v1/facilities?limit=50&offset=0&latitude=",lat,"&longitude=",lon,"&radius=",search.radius)
      r <- GET(query, add_headers(apikey = "YOUR API KEY"))
      data <- fromJSON(content(r,type="text"))
      rec <- data$RECDATA
      parks$rec.gov_recID[k] <- list(rec$ParentRecAreaID)
    }, error = function(e){cat("ERROR :",conditionMessage(e), "\n")})
  }
  parks$rec.gov_recID <-  as.character(parks$rec.gov_recID)
  write_csv(parks,"./Data/parks_data_recGovRecAreaID.csv")
}
if(get.rec_areaIDs == 0){
  parks <- read_csv("./Data/parks_data_recGovRecAreaID.csv")
}


# query api to get list of activities avaliable at rec areas in proximity to destination location
get.rec.area.activities <- 0
if(get.rec.area.activities == 1){
  parks$rec.gov_rec_area_activities <- NA
  parks$rec.gov_rec_area_activities_no <- NA
  
  # loop start
  for(k in 1:nrow(parks)){
    initialized <- 0
    activities <- NA
    rec.IDs <- NA
    if(parks$rec.gov_recID[k] != "NULL" & is.na(parks$rec.gov_recID[k]) == F){
    print(k)
  
    # get all activies in 1 data frame
    rec.IDs <- unique(as.numeric(eval(parse(text = noquote(parks$rec.gov_recID[k])))))
    for(j in 1:length(rec.IDs)){
      query <- paste0("https://ridb.recreation.gov/api/v1/recareas/",rec.IDs[j],"/activities?limit=100&offset=0")
      r <- GET(query, add_headers(apikey = "YOUR API KEY"))
      data <- fromJSON(content(r,type="text"))
      rec <- data$RECDATA
      if(initialized == 0 & length(data[[1]]) > 0){
        activities <- data.frame(data$RECDATA)
        initialized <- 1
      }
      if(initialized == 1 & length(data[[1]]) > 0){
        activities <- rbind.data.frame(activities, data.frame(data$RECDATA))
      } 
    }
    
    # clean the data frame
    activities <- unique(activities)
    
    # extract variables from the data frame and add to parks data
    if(is.na(activities) == F){   
      parks$rec.gov_rec_area_activities[k] <- as.character(list(activities$ActivityName))
      parks$rec.gov_rec_area_activities_no[k] <- as.character(list(activities$ActivityID))
    }
    } 
  } 
  
  write_csv(parks,"./Data/parks_data_recGovActivities.csv")
}
if(get.rec.area.activities == 0){
  read_csv("./Data/parks_data_recGovActivities.csv")
}

# query api to get list of activities avaliable at facilities in proximity to destination location
get.fac.activities <- 0
if(get.fac.activities == 1){
  parks$rec.gov_fac_activities <- NA
  
  # loop start
  for(k in 1:nrow(parks)){
    initialized <- 0
    activities <- NA
    rec.IDs <- NA
    #print(k)
    
    if(parks$rec.gov_facID[k] != "NULL" & is.na(parks$rec.gov_facID[k]) == F){

      # get all activies in 1 data frame
      fac.IDs <- unique(as.numeric(eval(parse(text = noquote(parks$rec.gov_facID[k])))))
      for(j in 1:length(fac.IDs)){
        query <- paste0("https://ridb.recreation.gov/api/v1/facilities/",fac.IDs[j],"/activities?limit=50&offset=0")
        r <- GET(query, add_headers(apikey = "YOUR API KEY"))
        data <- fromJSON(content(r,type="text"))
        rec <- data$RECDATA
        print(rec)
        if(initialized == 0 & length(data[[1]]) > 0){
          activities <- data.frame(data$RECDATA)
          initialized <- 1
        }
        if(initialized == 1 & length(data[[1]]) > 0){
          activities <- rbind.data.frame(activities, data.frame(data$RECDATA))
        } 
      }
      
      # clean the data frame
      activities <- unique(activities)
      # extract variables from the data frame and add to parks data
      if(is.na(activities) == F){   
        parks$rec.gov_fac_activities[k] <- as.character(list(activities$ActivityName))
      }
    } 
  } 
  
  write_csv(parks,"./Data/parks_data_FacActivities.csv")
}
if(get.rec.area.activities == 0){
  read_csv("./Data/parks_data_FacActivities.csv", stringsAsFactors = F)
} 


# get activties avaliable at facilities and rec areas ----------
parks$rec.gov_activities <- NA
parks$rec.gov_activities_no <- NA
act <- read.csv("./COVID Park Survey/Data/recGovData/EntityActivities_API_v1.csv")

# get activities for each observation
for(k in 1:nrow(parks)){ # start for loop
  print(k)
  fac.IDs <- unique(as.numeric(eval(parse(text = noquote(parks$rec.gov_facID[k])))))
  rec.IDs <- unique(as.numeric(eval(parse(text = noquote(parks$rec.gov_recID[k])))))
  IDs <- c(fac.IDs, rec.IDs)
  activities <- act[which(act$EntityID %in% IDs),]
  if(nrow(activities) > 0){
    parks$rec.gov_activities[k] <- as.character(list(as.character(activities$ActivityDescription)))
    parks$rec.gov_activities_no[k] <- as.character(list(unique(activities$ActivityID)))
  }
}

act.list <- read.csv("./Data/recGovData/Activities_API_v1.csv", stringsAsFactors = F)
for(k in 1:nrow(act.list)){
  print(k)
  parks[,ncol(parks)+1] <- as.numeric(grepl(tolower(act.list$ActivityName[k]),tolower(parks$rec.gov_activities)) )
  name <- gsub(" ","_", paste0("rec.gov_",tolower(act.list$ActivityName[k])))
  colnames(parks)[ncol(parks)] <- name
}


## output final data --------------------------
parks <- parks[,-which(substr(colnames(parks),1,1) == "X")]
write_csv(parks,"./Data/rec_sites_raw.csv")

