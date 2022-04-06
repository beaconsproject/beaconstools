### sum_raster_values ###
#
#' Sum area of each raster value in a set of polygons.
#'
#' Intersects a raster with a set of reserve polygons and sums the area of each raster value.
#' 
#' Sums are returned in a tibble which can be joined to \code{reserves_sf} using a \code{dplyr::left_join()}
#'  and \code{dplyr::pivot_wider()}.
#'  
#'  Projections of \code{reserves_sf} and \code{sum_raster} must match and are assumed to have units in meters.
#'
#' @param reserves_sf sf object of reserves in which to sum raster values.
#' @param reserves_id String representing unique id column in \code{reserves_sf}.
#' @param sum_raster Raster object to sum.
#' @param class_vals Vector of class values in \code{sum_raster} to include in the output. Defaults to all non-NA values in \code{sum_raster}.
#' @param fill_zeros Should missing values be included with an area of zero? Default is TRUE.
#'
#' @return A tibble listing area in km2 for each raster value in each reserve.
#' 
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @export
#'
#' @examples
#' reserves <- dissolve_catchments_from_table(
#'   catchments_sample, 
#'   benchmark_table_sample,
#'   "network")
#'   
#' sum_raster_values(reserves, "network", habitat_sample)
#' sum_raster_values(reserves, "network", habitat_sample, fill_zeros = FALSE)
#' sum_raster_values(reserves, "network", habitat_sample, c(2,3))
#' 
#' # To join results back to reserves
#' library(tidyr)
#' library(dplyr)
#' reserves %>%
#'   left_join(
#'     pivot_wider(
#'       sum_raster_values(reserves, "network", habitat_sample), 
#'       names_from = value, values_from = area_km2)
#'    )
sum_raster_values <- function(reserves_sf, reserves_id, sum_raster, class_vals = c(), fill_zeros = TRUE){
  
  stopifnot(sf::st_crs(reserves_sf) == sf::st_crs(sum_raster))
  
  cellsize <- raster::res(sum_raster)[1] / 1000
  cellarea <- cellsize * cellsize
  
  x <- exactextractr::exact_extract(sum_raster, reserves_sf, progress = FALSE)
  names(x) <- reserves_sf[[reserves_id]]
  
  if(fill_zeros){
    # generate vector of all values in raster, or class_vals if provided
    if(length(class_vals > 0)){
      all_values <- class_vals
    } else{
      all_values <- unique(unlist(lapply(x, function(y){unique(y$value)})))
      all_values <- all_values[!is.na(all_values)]
    }
  }
  
  counter <- 1
  for(i in names(x)){
    i_sums <- x[[i]] 
    
    if(fill_zeros){
      # add any missing values with area 0 so they get included in output
      missing_vals <- all_values[!all_values %in% unique(i_sums$value)]
      for(val in missing_vals){
        i_sums <- rbind(i_sums, dplyr::tibble(value=val, coverage_fraction=0))
      }
    }
    
    i_sums <- i_sums %>%
      dplyr::mutate(area = .data$coverage_fraction * cellarea) %>%
      dplyr::group_by(.data$value) %>%
      dplyr::summarise(area_km2 = sum(.data$area)) %>%
      dplyr::mutate(reserve = i)
    
    # append into long tibble
    if(counter == 1){
      df_long <- i_sums
      counter <- counter + 1
    } else{
      df_long <- rbind(df_long, i_sums)
    }
  }
  
  # remove na
  df_long <- df_long[!is.na(df_long$value),]
  
  # rename reserve
  df_long <- df_long %>%
    dplyr::relocate("reserve")
  names(df_long)[names(df_long) == "reserve"] <- reserves_id
  
  # filter by class_vals if provided
  if(length(class_vals > 0)){
    df_long <- df_long %>%
      dplyr::filter(.data$value %in% class_vals)
  }
  
  return(df_long)
}