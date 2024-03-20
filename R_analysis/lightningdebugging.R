install.packages("sp")
install.packages("maps")
install.packages("maptools", repos = "https://packagemanager.posit.co/cran/2023-10-13")
install.packages("zoo")
install.packages("dplyr")
library(sp)
library(maps)
library(maptools)
library(dplyr)
library(zoo)


## pointsDF: A data.frame whose first column contains longitudes and
##           whose second column contains latitudes.
##
## states:   An sf MULTIPOLYGON object with 50 states plus DC.
##
## name_col: Name of a column in `states` that supplies the states'
##           names.

#read in data
lightning = read.csv("C:/Users/clbpt/OneDrive/Documents/GitHub/STAT-172-Project-1/data/web_data/ncei_data/nldn-tiles-2020.csv")

lightning <- lightning %>%
  rename(date = )

lonlat_to_county_sp <- function(pointsDF) {
  # Prepare SpatialPolygons object with one SpatialPolygon
  # per state (plus DC, minus HI & AK)
  states <- map('county', fill=TRUE, col="transparent", plot=FALSE)
  IDs <- sapply(strsplit(states$names, ":"), function(x) x[1])
  states_sp <- map2SpatialPolygons(states, IDs=IDs,
                                   proj4string=CRS("+proj=longlat +datum=WGS84"))
  
  # Convert pointsDF to a SpatialPoints object 
  pointsSP <- SpatialPoints(pointsDF, 
                            proj4string=CRS("+proj=longlat +datum=WGS84"))
  
  # Use 'over' to get _indices_ of the Polygons object containing each point 
  indices <- over(pointsSP, states_sp)
  
  # Return the state names of the Polygons object containing each point
  stateNames <- sapply(states_sp@polygons, function(x) x@ID)
  stateNames[indices]
}


# Test the function using points in Wisconsin and Oregon.
testPoints <- data.frame(x = c(-90, -120), y = c(44, 44))

#lonlat_to_county_sp(testPoints)
#[1] "wisconsin" "oregon" # IT WORKS

testPoints <- data.frame(x = lightning$CENTERLON, y = lightning$CENTERLAT)

lightning$county <- lonlat_to_county_sp(testPoints)


lightning_monthly_scale_count <- lightning %>%
  mutate(date = zoo::as.yearmon(date)) %>%
  group_by(date, Region) %>%
  summarize(mean_interchange = mean(interchange))

