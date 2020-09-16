# set working directory -----------------
rm(list = ls()) # clear console
options(scipen = 999) # forces R to avoid exponential notation
system.info <- Sys.info()
setwd("")

# load libraries -------------------------
library(tidyverse)

# load raw recreation site data ---------
rec_sites <- read_csv("./Data/rec_sites_raw.csv")

# remove periods from column names
colnames(rec_sites) <- gsub("\\.","_",colnames(rec_sites))
colnames(rec_sites)

# deal with missing values------------

# replace NA with 0
vars <- c("google_rating","google_rating_n","average_trail_rating",
  "average_trail_rating_n","average_trail_ascent","average_trail_highpoint",
  "average_trail_lowpoint","average_trail_ascent_per_mile","average_trail_length")
for(k in 1:length(vars)){
  rec_sites[,vars[k]] <- replace(rec_sites[,vars[k]],is.na(rec_sites[,vars[k]]),0)
}



# save cleaned data
write_csv(rec_sites,"./Data/rec_sites_cleaned.csv")
