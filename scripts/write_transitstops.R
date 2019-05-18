library(tidyverse)
library(rgdal)

setwd("E:/Dropbox (Traffic Engineers)/kelsey/projects/hackathon19") # idk how to make this github

source("code/functions/unfactor.df.R") 
source("code/functions/read.gtfs.R") # edit function to pull direction from database

# set coordinate system to project gtfs lat/long coord into 
# for houston, texas south-central state plane
# look up projection codes at: spatialprojection.org 
# check if the above website is real
gtfs_projection <- "+init=epsg:2278"

# read gtfs data
gtfs <- "data/raw/gtfs_2019_0210_0831" %>% 
  read.gtfs(projection = gtfs_projection)

# identify which service_id's in the gtfs calendar file correspond with typical weekdays -- will vary by gtfs dataset
weekday_service_ids <- 6

# create table of stops with ids and coordinates, ...
# and potential features for statistical modeling: number of weekday trips, number of weekday routes, names of weekday routes
stops <- gtfs$trips %>% 
  filter(service_id %in% weekday_service_ids) %>% # filter for weekday trips
  left_join(gtfs$stop_times, by = "trip_id") %>% # join to all the stops these trips make
  group_by(stop_id) %>% 
  summarise(n_trips = n_distinct(trip_id), # number of unique trips servicing each stop (on typical weekday)
            n_routes = n_distinct(route_id), # number of unique routes servicing each stop (on typical weekday)
            route_short_names = paste(sort(unique(route_id)), collapse = ", ")) %>% # names of unique routes servicing each stop (on typical weekday) -- concatenated
  left_join(gtfs$stops %>% 
              select(stop_id, stop_name, stop_lat, stop_lon, stop_x, stop_y), # could remove stop_name, stop_lat, stop_lon
            by = "stop_id")

# write directly to database for future use
write.csv(stops, "data/processed/transit_stops.csv", row.names = F) 
