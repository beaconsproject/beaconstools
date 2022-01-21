check_catchnum <- function(catchments_sf){
  
  # check CATCHNUM exists
  if(!"CATCHNUM" %in% names(catchments_sf)){
    stop("Catchments must contain column 'CATCHNUM'")
  }
  
  # make sure it's a character
  if(!is.character(catchments_sf$CATCHNUM)){
    catchments_sf$CATCHNUM <- as.character(catchments_sf$CATCHNUM)
  }
  
  return(catchments_sf)
}


get_catch_list <- function(nets, benchmark_table){
  
  # check nets are in benchmark_table
  if(!all(nets %in% colnames(benchmark_table))){
    stop(paste0("names do not appear as colnames in the benchmark table: ", nets[!nets %in% colnames(benchmark_table)]))
  }
  
  # get unique catchments vector from benchmark_table using vector of colnames
  net_catchments <- unique(unlist(lapply(nets, function(x){
    benchmark_table[[x]]
  })))
  
  net_catchments[!is.na(net_catchments)]
}


check_network <- function(reserves_sf){
  
  if(!"network" %in% names(reserves_sf)){
    stop("Benchmark/network object must contain column: network")
  }
  
  # make sure it's a character
  if(!is.character(reserves_sf$network)){
    reserves_sf$network <- as.character(reserves_sf$network)
  }
  
  return(reserves_sf)
}


check_for_geometry <- function(in_sf){
  
  if(!"geometry" %in% names(in_sf)){
    stop("Benchmark/network object must contain column: geometry")
  }
}


# check benchmark names are in benchmarks_sf$network
check_benchmark_names <- function(nets, benchmarks_sf){
  if(!all(nets %in% benchmarks_sf$network)){
    stop(paste0("reserve names are not in sf objects 'network' column: ", paste0(nets[!nets %in% benchmarks_sf$network], collapse=", ")))
  }
}


check_evaluation_table <- function(evaluation_table){
  if(!all(names(evaluation_table) %in% c("class_value", "area_km2", "network", "class_proportion", "target_km2", "prop_target_met"))){
    stop("network_evaluation_table must contain colnames: 'class_value', 'area_km2', 'network', 'class_proportion', 'target_km2', 'prop_target_met' \n Are you using the output from evaluate_targets_using_catchments(), evaluate_targets_using_clip() or evaluate_targets_using_benchmarks()?")
  }
}