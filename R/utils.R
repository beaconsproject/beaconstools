sep_network_name <- function(network_name){
  strsplit(network_name, "__")[[1]]
}

check_catchnum <- function(catchments_sf){
  
  # check CATCHNUM exists
  if(!"CATCHNUM" %in% names(catchments_sf)){
    stop("Catchments must contain column CATCHNUM")
  }
  
  # make sure it's a character
  if(!is.character(catchments_sf$CATCHNUM)){
    catchments_sf$CATCHNUM <- as.character(catchments_sf$CATCHNUM)
  }
  
  return(catchments_sf)
}

get_catch_list <- function(nets, benchmark_table){
# get unique catchments vector from benchmark_table using vector of colnames
  
  net_catchments <- unique(unlist(lapply(nets, function(x){
    benchmark_table[[x]]
  })))
  
  net_catchments[!is.na(net_catchments)]
}