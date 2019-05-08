unfactor <- function(field){
  if(class(field) == "factor"){
    as.character(field)
  } else {
    field
  }
}

unfactor.df <- function(df){
  df[] <- df[] %>% lapply(unfactor)
  df
}