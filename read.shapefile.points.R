read.shapefile.points <- function(dsn, layer){
  shape <- readOGR(dsn=dsn, layer=layer)
  shape.data <- shape@data %>% unfactor.df()
  shape.data[c("x","y")] <- as.data.frame(shape@coords)[]
  list(points = shape.data, proj4string = shape@proj4string, bbox = shape@bbox)
}
