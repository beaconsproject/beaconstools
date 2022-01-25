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
#' @importFrom rlang .data
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
      dplyr::summarise(geometry = sf::st_union(.data$geometry))
  }

  x <- exactextractr::exact_extract(representation_raster, reference_sf, progress = FALSE)[[1]]

  # if no class_values provided, use all values in extracted raster
  if(length(class_values) == 0){
    class_values <- unique(x$value)
    class_values <- class_values[!is.na(class_values)]
  }

  # calc sum of each class_value
  df <- x %>%
    dplyr::filter(.data$value %in% class_values) %>%
    dplyr::mutate(area = .data$coverage_fraction * cellarea) %>%
    dplyr::group_by(.data$value) %>%
    dplyr::summarize(area_km2 = sum(.data$area))

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
#' @param catchments_sf sf object of the catchments dataset with target values summed using [criteria_to_catchments()]
#' @param criteria_name String representing the criteria name that will identify the target area columns (should match criteria name in [criteria_to_catchments()])
#' @param benchmark_table Data frame where columns are benchmark names and rows are catchments making up the benchmark. e.g. the "COLUMN_All_Unique_BAs" table 
#'   output by BUILDER.
#' @param target_table Data frame containing columns "class_value" and "target_km2". i.e. the output from [gen_targets()]. All classes in the target table are 
#'   evaluated. All target classes must have a matching column in the catchments dataset (e.g. class_value 1 in the led target table must have column led_1 in
#'   catchments_sf).
#' @param network_list Vector of networks to evaluate - usually just the list of benchmarks in the benchmark_table (e.g. PB_0001), but can also be combinations of 
#'   benchmarks (e.g. PB_0001__PB_0002) made using [gen_network_names()]. Usually combinations of benchmarks are multiple overlapping benchmarks that should have 
#'   targets evaluated for their combined area. Combinations of benchmark names must be separated by '__' and the individual respective names must appear in 
#'   benchmark_table. Defaults to all colnames in catchments_sf.
#'
#' @return A tibble with columns: 
#'\itemize{
#'  \item{\code{class_value}: the list of class_values from the target_table}
#'  \item{\code{area_km2}: area of the class value in the network}
#'  \item{\code{network}: name of the network copied from network_list}
#'  \item{\code{class_proportion}: copied from target_table}
#'  \item{\code{target_km2}: copied from target_table}
#'  \item{\code{prop_target_met}: area_km2/target_km2}
#'  }
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @export
#'
#' @examples
#' target_table <- gen_targets(ref_poly, led_sample, 1600)
#" # evaluate targets in three individual benchmarks
#' evaluate_targets_using_catchments(
#'   catchments_sample, 
#'   "led", 
#'   benchmark_table_sample, 
#'   target_table, 
#'   c("PB_0001", "PB_0002", "PB_0003"))
#' 
#' # evaluate targets across a network of multiple benchmarks
#' evaluate_targets_using_catchments(
#'   catchments_sample, 
#'   "led", 
#'   benchmark_table_sample, 
#'   target_table, 
#'   c("PB_0002__PB_0001"))
evaluate_targets_using_catchments <- function(catchments_sf, criteria_name, benchmark_table, target_table, network_list=c()){
  
  catchments_sf <- check_catchnum(catchments_sf) # check for CATCHNUM and make character
  
  # Check all required classes are in the catchments. If not, provide warning. Missing classes will be assumed to have area of zero.
  expected_classes <- paste0(criteria_name, "_", target_table$class_value)
  observed_classes <- as.data.frame(catchments_sf) %>%
    dplyr::select(tidyr::starts_with(criteria_name)) %>%
    names()
  check_classes_in_catchments(expected_classes, observed_classes, warning_ = TRUE, stop_ = FALSE)
  
  # set network_list if not provided
  if(length(network_list) == 0){
    network_list <- colnames(benchmark_table)
  }
  
  # STEP 1 - sum area of each class in each network
  counter <- 1
  for(net in network_list){
    
    # get catchment list for benchmark
    nets <- sep_network_names(net)
    
    net_catchments <- get_catch_list(nets, benchmark_table)
    
    # subset catchments and sum areas
    rows <- catchments_sf %>%
      dplyr::as_tibble() %>%
      dplyr::filter(.data$CATCHNUM %in% net_catchments) %>%
      dplyr::select(.data$CATCHNUM, tidyr::starts_with(criteria_name)) %>%
      tidyr::pivot_longer(cols = tidyr::starts_with(criteria_name), 
                   names_to = "class_value", 
                   names_prefix = paste0(criteria_name, "_"), 
                   values_to = "area", 
                   names_transform = list(class_value = as.integer)) %>%
      dplyr::filter(.data$class_value %in% target_table$class_value) %>% # only sum classes we are evaluating targets for
      dplyr::group_by(.data$class_value) %>%
      dplyr::summarize(area_km2 = sum(.data$area))
    
    missing_classes <- target_table$class_value[!target_table$class_value %in% rows$class_value]
    
    rows <- rows %>%
      dplyr::add_row(class_value = missing_classes, area_km2 = 0) %>%
      dplyr::mutate(network = net) %>%
      dplyr::arrange(.data$class_value)
    
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
#' Similar to [evaluate_targets_using_catchments()] but instead of pre-calculating the areas of the criteria raster in the catchments,
#' this method clips the criteria raster directly to each provided polygon.
#' Generally used for evaluating reserves that did not come from BUILDER and do not conform to catchment boundaries.
#' Slower than [evaluate_targets_using_catchments()] for large numbers of reserves.

# Using 'reserve language here since this function will mostly be used with existing PAs not from BUILDER.

#' @param reserves_sf sf object containing reserves to evaluate
#' @param reserves_id Unique id column in reserves_sf containing reserve names as strings.
#' @param representation_raster Raster object of the criteria layer that will be evaluated, with crs matching reserves_sf
#' @param target_table Data frame containing columns "class_value" and "target_km2". i.e. the output from [gen_targets()]. All classes in the target table are 
#'   evaluated. All class_values must match the values in the representation_raster
#' @param reserve_list The reserve_id values to process. Defaults to all reserve_id's in reserves_sf
#'
#' @return A tibble with columns: 
#'\itemize{
#'  \item{\code{class_value}: the list of class_values from the target_table}
#'  \item{\code{area_km2}: area of the class value in the network}
#'  \item{\code{network}: name of the network copied from network_list}
#'  \item{\code{class_proportion}: copied from target_table}
#'  \item{\code{target_km2}: copied from target_table}
#'  \item{\code{prop_target_met}: area_km2/target_km2}
#'  }
#' @importFrom magrittr %>%
#' @importFrom rlang .data
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
      dplyr::filter(.data$class_value %in% target_table$class_value) %>%
      dplyr::mutate(area = .data$coverage_fraction * cell_area) %>%
      dplyr::group_by(.data$class_value) %>%
      dplyr::summarize(area_km2 = sum(.data$area))
    
    missing_class_values <- target_table$class_value[!target_table$class_value %in% rows$class_value]
    
    rows <- rows %>% 
      dplyr::add_row(class_value = missing_class_values, area_km2 = 0) %>%
      dplyr::mutate(network = reserve) %>%
      dplyr::arrange(.data$class_value)
    
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


### evaluate_targets_using_benchmarks ###
#
#' Combines benchmark results to evaluate networks.
#'
#' For a set of benchmarks in a network, takes the largest area value for each target class and evaluates the target. If area is 
#' tied, selects one at random.
#' 
#' Each target therefore has to be met in full by at least one benchmark in the network. To assess target classes summed 
#' across the entire network, use [evaluate_targets_using_catchments()] or [evaluate_targets_using_clip()].
#' 
#' @param benchmark_results Tibble or data frame output from [evaluate_targets_using_catchments()] or [evaluate_targets_using_clip()]
#' @param network_list Vector of networks to evaluate, such as those made with [gen_network_names()].
#'
#' @return A tibble with columns: 
#'\itemize{
#'  \item{\code{class_value}: the list of class_values from the target_table}
#'  \item{\code{area_km2}: area of the class value in the network}
#'  \item{\code{network}: name of the network copied from network_list}
#'  \item{\code{class_proportion}: copied from target_table}
#'  \item{\code{target_km2}: copied from target_table}
#'  \item{\code{prop_target_met}: area_km2/target_km2}
#'  }
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @export
#'
#' @examples
#' target_table <- gen_targets(ref_poly, led_sample, 1600)
#' benchmarks_results <- evaluate_targets_using_catchments(
#'   catchments_sample, 
#'   "led", 
#'   benchmark_table_sample, 
#'   target_table, 
#'   colnames(benchmark_table_sample))
#'  
#' # evaluate networks of 2 benchmarks 
#' network_list <- gen_network_names(unique(benchmarks_results$network), 2)
#' evaluate_targets_using_benchmarks(benchmarks_results, network_list)
#' 
#' # evaluate the option of using all three benchmarks in a single network
#' evaluate_targets_using_benchmarks(benchmarks_results, "PB_0001__PB_0002__PB_0003")
evaluate_targets_using_benchmarks <- function(benchmark_results, network_list){
  
  benchmark_results <- check_network(benchmark_results)
  check_evaluation_table(benchmark_results)
  
  counter <- 1
  for(net in network_list){
    
    nets <- sep_network_names(net)
    
    check_benchmark_names(nets, benchmark_results)
    
    # rebuild results table with whichever benchmark has the largest area for each target
    nets_rslts <- benchmark_results[benchmark_results$network %in% nets & benchmark_results$target_km2 > 0,] # drop targets that are zero
    net_rslts <- nets_rslts %>% 
      dplyr::group_by(.data$class_value) %>% 
      dplyr::arrange(dplyr::desc(.data$area_km2)) %>% 
      dplyr::slice_head(n = 1)
    net_rslts$network <- net
    
    if(counter == 1){
      df <- net_rslts
      counter <- counter + 1
    } else{
      df <- rbind(df, net_rslts)
    }
  }
  return(df)
}


### summarize_representation_results ###
#
#' Summarize representation results by network.
#'
#' Takes a table of network evaluation results from [evaluate_targets_using_catchments()] or [evaluate_targets_using_clip()]
#' and summarizes either the number of classes passed, or the number of classes not passed (i.e. target gaps) by each network.
#' 
#' Targets of zero are not counted.
#' 
#' Rare targets are often difficult to meet, so \code{target_inclusion_proportion} can be used to drop targets that make up a small 
#' proportion of the total target area. For example \code{target_inclusion_proportion = 0.05} would only consider targets covering 
#' at least 5% of the total target area.
#' 
#' @param network_evaluation_table Data frame output from [evaluate_targets_using_catchments()], [evaluate_targets_using_clip()] or 
#'   [evaluate_targets_using_benchmarks()].
#' @param criteria_name String representing the representation raster. Used to label the summary columns. Usually matches \code{criteria_name} 
#'  when using [evaluate_targets_using_catchments()].
#' @param target_pass_proportion Numeric between 0 and 1 that sets the proportion of the target that needs to be met in order for the target 
#'   to be considered 'passed'. Defaults to 1 (i.e. 100% of the target needs to be met in the network).
#' @param target_inclusion_proportion Numeric between 0 and 1. Target classes with a \code{class_proportion} value less than \code{target_inclusion_proportion} 
#'   are dropped. Defaults to 0 which includes all targets.
#' @param suffix Optional suffix string to add to the end of the summary column name.
#' @param gaps If TRUE, returns a summary column named e.g. \code{led_gaps} with the number of missed targets. Otherwise returns 
#'   a summary column named e.g. \code{led_passed} with the number of passed targets. Defaults to TRUE.
#'
#' @return A tibble with columns \code{network} and a summary column (e.g. \code{led_gaps}).
#' @export
#'
#' @examples
#' target_table <- gen_targets(ref_poly, led_sample, 1600)
#' reserves <- catchments_to_benchmarks(
#'   benchmark_table_sample, 
#'   catchments_sample, 
#'   c("PB_0001", "PB_0002"))
#' network_evaluation_table <- evaluate_targets_using_clip(
#'   reserves, "network", led_sample, target_table)
#'
#' summarize_representation_results(network_evaluation_table, "led")
#' summarize_representation_results(
#'   network_evaluation_table, "led", 
#'   0.9, 0.05, "_90pcnt_commontargets")
#' summarize_representation_results(network_evaluation_table, "led", 
#'   0.9, suffix = "_90pcnt", gaps = FALSE)
#' 
#' # join multiple summary columns using a list
#' library(purrr)
#' library(dplyr)
#' my_list <- list()
#' my_list <- append(my_list, 
#'   list(summarize_representation_results(network_evaluation_table, "led")))
#' my_list <- append(my_list, 
#'   list(summarize_representation_results(network_evaluation_table, "led", gaps = FALSE)))
#' my_list %>% reduce(left_join, by = "network")
summarize_representation_results <- function(network_evaluation_table, criteria_name, target_pass_proportion = 1, target_inclusion_proportion = 0, suffix = "", gaps = TRUE){
  
  # run columns checks
  network_evaluation_table <- check_network(network_evaluation_table)
  check_evaluation_table(network_evaluation_table)
  
  # make output table template
  df <- dplyr::tibble(network = unique(network_evaluation_table$network))
  
  for(net in df$network){
    
    # drop rows where target is 0
    rslts <- network_evaluation_table[network_evaluation_table$network == net & network_evaluation_table$target_km2 > 0,]
    
    # subset and summarize
    if(gaps){
      # gaps
      df[[paste0(criteria_name,"_gaps", suffix)]][df$network == net] <- nrow(rslts[rslts$prop_target_met < target_pass_proportion & rslts$class_proportion >= target_inclusion_proportion,]) 
    } else{
      #pass count
      df[[paste0(criteria_name,"_passed", suffix)]][df$network == net] <- nrow(rslts[rslts$prop_target_met >= target_pass_proportion & rslts$class_proportion >= target_inclusion_proportion,]) 
    }
  }
  return(df) 
}