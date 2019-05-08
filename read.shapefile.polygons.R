read.shapefile.polygons <- function(dsn, layer){
  spdf <- readOGR(dsn=dsn, layer=layer)
  spdf@data <- spdf@data %>% unfactor.df()
  spdf@data$polys_id <- row.names(spdf@data)
  spdf@polygons <- spdf@polygons %>% 
    lapply(function(polys){
      poly_nrows <- polys@Polygons %>% 
        lapply(function(Poly){
          nrow(Poly@coords)
        }) %>% 
        unlist()
      
      polys@Polygons %>% 
        lapply(function(Poly){
          Poly@coords %>%
            as.data.frame() %>% 
            transmute(x = V1,
                      y = V2, 
                      seq = 1:n(), 
                      poly_labpt_x = Poly@labpt[1], 
                      poly_labpt_y = Poly@labpt[2],
                      area = Poly@area,
                      poly_hole = Poly@hole, 
                      ringDir = Poly@ringDir)
        }) %>%
        rbindlist() %>% 
        as.data.frame(stringsAsFactors=F) %>%
        mutate(poly_id = rep(1:length(polys@Polygons), poly_nrows),
               poly_plotOrder = rep(polys@plotOrder, poly_nrows), 
               polys_labpt_x = polys@labpt[1],
               polys_labpt_y = polys@labpt[2], 
               polys_id = as.integer(polys@ID),
               polys_area = polys@area)
      
    }) %>% 
    rbindlist() %>% 
    as.data.frame(stringsAsFactors=F)
  
  polys_nrows <- spdf@polygons %>% 
    group_by(polys_id) %>% 
    tally() %>% 
    ungroup()
  spdf@polygons$polys_plotOrder <- rep(spdf@plotOrder, polys_nrows$n)
  
  return(spdf)
  
}

# polygons@polygons[[1]]@Polygons %>% class() # list (1+)
## polygons@polygons[[1]]@Polygons[[1]]@labpt # numeric (2)
## polygons@polygons[[1]]@Polygons[[1]]@area # numeric (1)
## polygons@polygons[[1]]@Polygons[[1]]@hole # logical (1)
## polygons@polygons[[1]]@Polygons[[1]]@ringDir # integer (1)
# polygons@polygons[[1]]@plotOrder %>% class() # integer (1+)
# polygons@polygons[[1]]@labpt %>% class() # numeric (2)
# polygons@polygons[[1]]@ID %>% class() # character (1)
# polygons@polygons[[1]]@area %>% class() # numeric (1)