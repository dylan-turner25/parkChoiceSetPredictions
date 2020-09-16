rm(list = ls()) # clear console
options(scipen = 999) # forces R to avoid exponential notation
system.info <- Sys.info()
setwd("")

## Load Libraries ----------------------------
library(rvest)
library(tidyverse)

## Load file of all us counties --------------------------------
us_counties <- read_csv("./Data/us_counties.csv")

# convert 4 digit fips to 5 digits
us_counties$fips[which(nchar(us_counties$fips) == 4)] <- paste0("0",us_counties$fips[which(nchar(us_counties$fips) == 4)])

# loop through each county in the us and get the living wage for a single adult
# according to https://livingwage.mit.edu
us_counties$living_wage_single_adult <- NA
for(k in 1:nrow(us_counties)){
  tryCatch({
    print(k)
    if(is.na(us_counties$living_wage_single_adult[k])){
      wages <- NA
      url <- paste0("https://livingwage.mit.edu/counties/",us_counties$fips[k])
      wages <- url %>%
        html() %>%
        html_nodes(xpath='/html/body/div[2]/div/div[1]/table') %>%
        html_table()
      wages <- wages[[1]]
      us_counties$living_wage_single_adult[k] <- wages[2,2]
    }
  }, error = function(e){cat("ERROR :",conditionMessage(e), "\n")})
}

# save as csv
write_csv(us_counties,"./Data/cost_of_living.csv")
