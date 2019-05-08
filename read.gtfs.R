read.gtfs <- function(folder, projection="+init=epsg:2278"){ # default projection: state plane texas south central
  tables <- folder %>% list.files() %>% paste(folder,.,sep="/") %>% as.list() %>% 
    lapply(function(file){
      table <- file %>% read.csv()
      table[] <- table[] %>% lapply(unfactor)
      return(table)
    })
  names(tables) <- folder %>% list.files() %>% str_replace("\\.txt","")
  
  # transform lat long coordinates to x y for stops and shapes tables
  stops_xy <- tables$stops[c("stop_lon","stop_lat")] %>% 
    as.matrix() %>% 
    SpatialPoints(proj4string = CRS("+init=epsg:4326")) %>% # wgs84
    spTransform(CRSobj = CRS(projection)) %>% 
    as.data.frame()
  tables$stops[c("stop_x","stop_y")] <- stops_xy[]
  shapes_xy <- tables$shapes[c("shape_pt_lon","shape_pt_lat")] %>% 
    as.matrix() %>% 
    SpatialPoints(proj4string = CRS("+init=epsg:4326")) %>% # wgs84
    spTransform(CRSobj = CRS(projection)) %>% 
    as.data.frame()
  tables$shapes[c("shape_pt_x","shape_pt_y")] <- shapes_xy[]
  
  # convert arrival_time and departure_time in stop_times to seconds
  tables$stop_times[c("arrival_time_sec","departure_time_sec")] <- tables$stop_times[c("arrival_time","departure_time")] %>% 
    lapply(function(time_string){
      time_string_test <- time_string %>% 
        str_replace("^ ","0") %>% 
        str_detect("^[0-9]{2}:[0-9]{2}:[0-9]{2}$") %>% 
        sum()
      if(time_string_test == length(time_string)){
        time_integer <- time_string %>% 
          str_replace("^ ","0") %>% 
          str_split(":") %>% 
          lapply(function(h_m_s){
            sum(as.integer(h_m_s) * c(3600, 60, 1))
          }) %>% 
          unlist()
      } else {
        time_integer <- NA
      }
      return(time_integer)
    })
  
  trips_patterns_xw <- tables$stop_times %>% 
    arrange(trip_id, stop_sequence) %>% 
    group_by(trip_id) %>% 
    summarise(pattern_id = paste(stop_id, collapse=", "), n_stops = n()) %>% 
    ungroup() %>% 
    mutate(pattern_id = as.integer(factor(pattern_id)))
  tables$trips <- tables$trips %>% 
    left_join(trips_patterns_xw, by="trip_id")
  tables$patterns <- tables$trips %>% 
    left_join(tables$stop_times, by="trip_id") %>% 
    select(pattern_id, stop_sequence, stop_id) %>% 
    unique()
  
  tables$trips <- tables$trips %>% 
    left_join(tables$routes %>% 
                select(route_id, route_short_name), 
              by="route_id")
  
  return(tables)
}