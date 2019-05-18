read.shapefile.lines <- function(dsn, layer){
  shape <- readOGR(dsn=dsn, layer=layer)
  shape.data <- shape@data %>% unfactor.df()
  shape.lines <- shape@lines %>% 
    lapply(function(lines){
      1:length(lines@Lines) %>% 
        lapply(function(i){
          line <- lines@Lines[[i]]@coords %>%
            as.data.frame() %>% 
            transmute(x = V1,
                      y = V2, 
                      sequence = 1:n(), 
                      segment_id = i)
        }) %>% 
        rbindlist() %>% 
        as.data.frame() %>% 
        mutate(line_id = as.integer(lines@ID))
    }) %>% 
    rbindlist() %>% 
    as.data.frame(stringsAsFactors=F)
  if(sum(shape.lines$segment_id == 1) == nrow(shape.lines)){
    shape.lines <- shape.lines %>% select(-segment_id)
  }
  list(lines = shape.lines, data = shape.data, proj4string = shape@proj4string, bbox = shape@bbox)
}

