## A faster function for scraping min/max latitude and longitude for all reptile species
## Sima Bouzid 27 Oct 2015
########################################################################################

setwd("~/Dropbox/UW copy/Resources/lizlatlong_Tinkering")

# Read in .csv of data
reptile <- read.csv("Reptiles.csv", header=T, na.strings = c("", " "), stringsAsFactors = FALSE)

# Slow and clunky lizlatlong function
#~#~#~#~##~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#
lizlatlong <- function(x){
  
  # Isolate data ranges of interest
   species <- as.character(x$scientificname)
  order <- as.character(x$order)
  family <- as.character(x$family)
  latitude <-as.numeric(x$decimallatitude, stringsAsFactors = FALSE)
  longitude <- as.numeric(x$decimallongitude, stringsAsFactors = FALSE)
  pruned_df <- data.frame(species, order, family, latitude, longitude)
  # get rid of 0's that might have been accidentally inputted
  pruned_df <- pruned_df[complete.cases(pruned_df[,c("latitude", "longitude")]),]
  #return(pruned_df) #testing
  # Use a loop to extract min/max lat/long for each species 
  maxLat <- length(unique(pruned_df$species))
  minLat <- length(unique(pruned_df$species))
  maxLong <- length(unique(pruned_df$species))
  minLong <- length(unique(pruned_df$species)) 
  
  for (i in 1:length(unique(pruned_df$species))){
    y <- pruned_df[pruned_df$species==unique(pruned_df$species)[i],] 
    maxLat[i] <- max(y$latitude, na.rm=T)
    minLat[i] <- min(y$latitude, na.rm=T)
    maxLong[i] <- max(y$longitude, na.rm=T)
    minLong[i] <- max(y$longitude, na.rm=T)
  }
  # Recombine into a data frame
  new_df <- data.frame(species=(unique(pruned_df$species)), maxLat, minLat, maxLong, minLong)
  write.csv(new_df, file = "lizlatlong.csv")
}

# Testing the function; this takes 6-8 hours to run
test <- lizlatlong(rep_test)
#~#~#~#~##~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#



#~#~#~#~##~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#
### The NEW AND IMPROVED lizlatlong function

# load function contingencies
#install.packages("plyr")
library(plyr)

# run new function
########################################################################################
lizlatlong <- function(x){
  start <- Sys.time() # timing how long the function takes to run
  
  # Isolate data ranges of interest
  species <- as.character(x$scientificname)
  order <- as.character(x$order)
  family <- as.character(x$family)
  latitude <- as.numeric(x$decimallatitude, stringsAsFactors = FALSE)
  longitude <- as.numeric(x$decimallongitude, stringsAsFactors = FALSE)
  pruned_df <- data.frame(species, order, family, latitude, longitude)
  
  # get rid of rows with NAs in lat and long
  pruned_df <- pruned_df[complete.cases(pruned_df[,c("latitude", "longitude")]),]
  
  # for each combination of factors (species, order, family) find min/max lat/long
  pruned_df <- ddply(pruned_df, .(species, order, family), 
                     summarize, 
                     max_latitude = max(latitude),
                     min_latitude = min(latitude),
                     max_longitude = max(longitude),
                     min_longitude = min(longitude))
  
  write.csv(pruned_df, file = "lizlatlong.csv")
  end <- Sys.time() #end timing
  return(end-start)
}
########################################################################################


# testing the function; takes only about 15 seconds to run!
lizlatlong(reptile)

#~#~#~#~##~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#