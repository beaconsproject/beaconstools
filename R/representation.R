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
#' gen_targets(catchments_sample, led_sample, 5000)
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
#' Usually used for evaluating sets of individual benchmarks, or overlapping benchmarks that should be combined for target evaluation.
#'
#' @param catchments_sf sf object of the catchments dataset with target values summed using criteria_to_catchments()
#' @param criteria_name String representing the criteria name that will identify the target area columns (should match criteria name in criteria_to_catchments())
#' @param benchmark_table data frame where columns are benchmark names and rows are catchments making up the benchmark. e.g. the "COLUMN_All_Unique_BAs" table 
#'   output by BUILDER.
#' @param target_table data frame containing columns "class_value" and "target_km2". i.e. the output from gen_targets(). All classes in the target table are 
#'   evaluated. All target classes must have a matching column in the catchments dataset (e.g. class_value 1 in the led target table must have column led_1 in
#'   catchments_sf).
#' @param network_list list of networks to evaluate - usually just the list of benchmarks in the benchmark_table (e.g. PB_001), but can also be combinations of 
#'   benchmarks (e.g. PB_0001__PB_0002) made using gen_network_names(). Usually combinations of benchmarks are multiple overlapping benchmarks that should have 
#'   targets evaluated for their combined area.
#'
#' @return A tibble with columns: 
#'\itemize{
#'  \item{}
#'  \item{}
#'  \item{}
#'  \item{}
#'  \item{}
#'  }
#' @importFrom magrittr %>%
#' @export
#'
#' @examples
#' evaluate_targets_using_catchments()
evaluate_targets_using_catchments <- function(){}
  