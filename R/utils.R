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


# check all required criteria classes have been added to catchments for a given input target table
check_classes_in_catchments <- function(expected_classes, observed_classes, warning_, stop_){
  
  missing_classes <- expected_classes[!expected_classes %in% observed_classes]
  
  if(warning_ & length(missing_classes) > 0){
    warning(paste0("Classes are not in catchments: ", paste0(missing_classes, collapse=", "), ". Add them to catchments using criteria_to_catchments(), otherwise an area an area of zero will be assumed."))
  }
  if(stop_  & length(missing_classes) > 0){
    stop(paste0("Classes are not in catchments: ", paste0(missing_classes, collapse=", "), ". Add them to catchments using criteria_to_catchments()."))
  }
}

# convert long table into wide table with NAs (i.e. BUILDER style table)
long_to_wide <- function(df, col_names, values_col){
  
  # get out table nrow (i.e. longest list of values)
  tbl_rows <- df %>%
    dplyr::group_by(.data[[col_names]]) %>%
    dplyr::summarise(n = dplyr::n()) %>%
    dplyr::summarise(m = max(.data$n)) %>%
    dplyr::pull(.data$m)
  
  values_list <- lapply(unique(df[[col_names]]), function(x){
    vals <- df[[values_col]][df[[col_names]]==x]
    c(vals, rep(NA, tbl_rows - length(vals)))
    })
  names(values_list) <- unique(df[[col_names]])
  out_tab <- as.data.frame(do.call(cbind, values_list))
  
  return(out_tab)
}

# convert list of vectors into df with list element names as colnames and missing values as NAs (i.e. BUILDER style table)
list_to_wide <- function(values_list){
  
  # get out table nrow (i.e. longest list of values)
  tbl_rows <- max(unlist(lapply(values_list, function(x){
    length(x)
  })))
  
  values_list_nas <- lapply(values_list, function(x){
    c(x, rep(NA, tbl_rows - length(x)))
  })
  
  out_tab <- as.data.frame(do.call(cbind, values_list_nas))
  
  return(out_tab)
}

# remove OID from tables comgin out of BUILDER
remove_oid <- function(in_table){
  if("OID" %in% colnames(in_table)){
    out_table <- in_table %>%
      dplyr::select(-.data$OID)
  } else{
    out_table <- in_table
  }
  return(out_table)
}