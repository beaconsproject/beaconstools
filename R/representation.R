### gen_targets ###
#
#' Generate target tables for representation analysis.
#'
#'Takes a reference area polygon and a classified raster for which representation will be evaluated. Calculates the proportion of each class in the reference area 
#'and multiples by reserve size to get target in km2.
#'
#' @param reference_sf sf object of the reference area we are aiming to represent
#' @param representation_raster Raster object of the representation layer classified into categorical classes
#' @param class_values A vector of classes in representation_raster to generate targets for. Defaults to all classes in the representation_raster.
#' @param reserve_size The area in km2 that targets will sum to. Generally the approximate reserve size of reserves being evaluated.
#'
#' @return A tibble with columns: 
#'\itemize{
#'  \item{class_value: the list of class_values}
#'  \item{area_km2: the area of each class_value in the reference_sf}
#'  \item{reserve_size: the provided reserve_size}
#'  \item{class_proportion: area_km2/sum(area_km2)}
#'  \item{target_km2: class_proportion * reserve_area}
#'  }
#' @importFrom magrittr %>%
#' @export
#'
#' @examples
#' gen_targets(ref_poly, led_sample, 5000)
gen_targets <- function(reference_sf, representation_raster, reserve_size, class_values = c()){

  cellsize <- raster::res(representation_raster)[1] / 1000
  cellarea <- cellsize * cellsize

  # if more than one feature in the reference object, dissolve into single geometry
  if(nrow(reference_sf) > 1){
    reference_sf <- reference_sf %>%
      dplyr::summarise(geometry = sf::st_union(geometry))
  }

  x <- exactextractr::exact_extract(representation_raster, reference_sf, progress = FALSE)[[1]]

  # if no class_values provided, use all values in extracted raster
  if(length(class_values) == 0){
    class_values <- unique(x$value)
    class_values <- class_values[!is.na(class_values)]
  }

  # calc sum of each class_value
  df <- x %>%
    dplyr::filter(value %in% class_values) %>%
    dplyr::mutate(area = coverage_fraction * cellarea) %>%
    dplyr::group_by(value) %>%
    dplyr::summarize(area_km2 = sum(area))

  names(df) <- c("class_value", "area_km2")

  # if any class_values not in df, add them with area of zero
  for(i in class_values){
    if(!i %in% df$class_value){
      df <- rbind(df, data.frame(class_value = i, area_km2 = 0))
    }
  }

  # order by class_value
  df <- df[order(df$class_value),]

  # add reserve size and calc targets
  df$reserve_size <- reserve_size
  df$class_proportion <- df$area_km2 / sum(df$area_km2)
  df$target_km2 <- round(df$class_proportion * df$reserve_size, 2)

  return(df)
}


### evaluate_targets_using_catchments ###
#
#' Evaluate targets using benchmarks defined by lists of catchments.
#'
#' For a given benchmark or list of benchmarks, sums the areas of target values in each benchmark and evaluates against the targets from gen_targets().
#' Benchmarks are defined by a list of catchment CATCHNUMs, usually produced by BUILDER.
#' Usually used for evaluating sets of individual benchmarks, or overlapping benchmarks that should be combined into a single area for target evaluation.
#' Could also be used to evaluate a network of multiple benchmarks for a set of targets.
#'
#' @param catchments_sf sf object of the catchments dataset with target values summed using \code{criteria_to_catchments()}
#' @param criteria_name String representing the criteria name that will identify the target area columns (should match criteria name in \code{criteria_to_catchments()})
#' @param benchmark_table Data frame where columns are benchmark names and rows are catchments making up the benchmark. e.g. the "COLUMN_All_Unique_BAs" table 
#'   output by BUILDER.
#' @param target_table Data frame containing columns "class_value" and "target_km2". i.e. the output from \code{gen_targets()}. All classes in the target table are 
#'   evaluated. All target classes must have a matching column in the catchments dataset (e.g. class_value 1 in the led target table must have column led_1 in
#'   catchments_sf).
#' @param network_list Vector of networks to evaluate - usually just the list of benchmarks in the benchmark_table (e.g. PB_0001), but can also be combinations of 
#'   benchmarks (e.g. PB_0001__PB_0002) made using \code{gen_network_names()}. Usually combinations of benchmarks are multiple overlapping benchmarks that should have 
#'   targets evaluated for their combined area. Combinations of benchmark names must be separated by '__' and the individual respective names must appear in 
#'   benchmark_table. Defaults to all colnames in catchments_sf.
#'
#' @return A tibble with columns: 
#'\itemize{
#'  \item{class_value: the list of class_values from the target_table}
#'  \item{area_km2: area of the class value in the network}
#'  \item{network: name of the network copied from network_list}
#'  \item{class_proportion: copied from target_table}
#'  \item{target_km2: copied from target_table}
#'  \item{prop_target_met: area_km2/target_km2}
#'  }
#' @importFrom magrittr %>%
#' @export
#'
#' @examples
#' target_table <- gen_targets(ref_poly, led_sample, 1600)
#' evaluate_targets_using_catchments(
#'   catchments_sample, 
#'   "led", 
#'   benchmark_table_sample, 
#'   target_table, 
#'   c("PB_0001", "PB_0002", "PB_0003"))
evaluate_targets_using_catchments <- function(catchments_sf, criteria_name, benchmark_table, target_table, network_list=c()){
  
  catchments_sf <- check_catchnum(catchments_sf) # check for CATCHNUM and make character
  
  # set network_list if not provided
  if(length(network_list) == 0){
    network_list <- colnames(benchmark_table)
  }
  
  # STEP 1 - sum area of each class in each network
  counter <- 1
  for(net in network_list){
    
    # get catchment list for benchmark
    nets <- sep_network_name(net)
    
    net_catchments <- get_catch_list(nets, benchmark_table)
    
    # subset catchments and sum areas
    rows <- catchments_sf %>%
      dplyr::as_tibble() %>%
      dplyr::filter(CATCHNUM %in% net_catchments) %>%
      dplyr::select(CATCHNUM, tidyr::starts_with(criteria_name)) %>%
      tidyr::pivot_longer(cols = tidyr::starts_with(criteria_name), 
                   names_to = "class_value", 
                   names_prefix = paste0(criteria_name, "_"), 
                   values_to = "area", 
                   names_transform = list(class_value = as.integer)) %>%
      dplyr::filter(class_value %in% target_table$class_value) %>% # only sum classes we are evaluating targets for
      dplyr::group_by(class_value) %>%
      dplyr::summarize(area_km2 = sum(area)) %>%
      dplyr::add_row(class_value = target_table$class_value[!target_table$class_value %in% .$class_value], area_km2 = 0) %>%
      dplyr::mutate(network = net) %>%
      dplyr::arrange(class_value)
    
    if(counter == 1){
      df <- rows
      counter <- counter + 1
    } else{
      df <- rbind(df, rows)
    }
  }
  
  # STEP 2 - join targets and calculate proportion of target met
  df <- df %>%
    dplyr::left_join(target_table[c("class_value","class_proportion","target_km2")], by = "class_value")
  
  # add proportion of target met
  df$prop_target_met <- round(df$area_km2 / df$target_km2, 2)
  
  return(df)
}

### evaluate_targets_using_clip ###
#
#' Evaluate targets by clipping.
#'
#' Similar to \code{evaluate_targets_using_catchments()} but instead of pre-calculating the areas of the criteria raster in the catchments,
#' this method clips the criteria raster directly to each provided polygon.
#' Generally used for evaluating reserves that did not come from BUILDER and do not conform to catchment boundaries.
#' Slower than \code{evaluate_targets_using_catchments()} for large numbers of reserves.

# Using 'reserve language here since this function will mostly be used with existing PAs not from BUILDER.

#' @param reserves_sf sf object containing reserves to evaluate
#' @param reserves_id Unique id column in reserves_sf containing reserve names as strings.
#' @param representation_raster Raster object of the criteria layer that will be evaluated, with crs matching reserves_sf
#' @param target_table Data frame containing columns "class_value" and "target_km2". i.e. the output from \code{gen_targets()}. All classes in the target table are 
#'   evaluated. All class_values must match the values in the representation_raster
#' @param reserve_list The reserve_id values to process. Defaults to all reserve_id's in reserves_sf
#'
#' @return A tibble with columns: 
#'\itemize{
#'  \item{class_value: the list of class_values from the target_table}
#'  \item{area_km2: area of the class value in the network}
#'  \item{network: name of the network copied from network_list}
#'  \item{class_proportion: copied from target_table}
#'  \item{target_km2: copied from target_table}
#'  \item{prop_target_met: area_km2/target_km2}
#'  }
#' @importFrom magrittr %>%
#' @export
#'
#' @examples
#' target_table <- gen_targets(ref_poly, led_sample, 1600)
#' reserves <- catchments_to_benchmarks(
#'   benchmark_table_sample, 
#'   catchments_sample, 
#'   c("PB_0001", "PB_0002"))
#' evaluate_targets_using_clip(reserves, "network", led_sample, target_table)
evaluate_targets_using_clip <- function(reserves_sf, reserves_id, representation_raster, target_table, reserve_list=c()){
  
  # set reserve_list if not provided
  if(length(reserve_list) == 0){
    reserve_list <- reserves_sf[[reserves_id]]
  }
  
  cell_area <- prod(raster::res(representation_raster)) / 1000000 # convert to area in km2, assumes raster res is in metres
  
  counter <- 1
  for(reserve in reserve_list){
    
    # STEP 1 - sum area of each class in each network
    # subset
    reserve_sf <- reserves_sf[reserves_sf[[reserves_id]] == reserve,]
    
    # clip raster
    x <- exactextractr::exact_extract(representation_raster, reserve_sf, progress = FALSE)[[1]]
    names(x) <- c("class_value", "coverage_fraction")
    rows <- x %>%
      dplyr::filter(class_value %in% target_table$class_value) %>%
      dplyr::mutate(area = coverage_fraction * cell_area) %>%
      dplyr::group_by(class_value) %>%
      dplyr::summarize(area_km2 = sum(area)) %>%
      dplyr::add_row(class_value = target_table$class_value[!target_table$class_value %in% .$class_value], area_km2 = 0) %>%
      dplyr::mutate(network = reserve) %>%
      dplyr::arrange(class_value)
    
    if(counter == 1){
      df <- rows
      counter <- counter + 1
    } else{
      df <- rbind(df, rows)
    }
  }
  
  # STEP 2 - join targets and calculate proportion of target met
  df <- df %>%
    dplyr::left_join(target_table[c("class_value","class_proportion","target_km2")], by = "class_value")
  
  # add proportion of target met
  df$prop_target_met <- round(df$area_km2 / df$target_km2, 2)
  
  return(df)
}