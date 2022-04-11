### sum_raster_values ###
#
#' Sum area of each raster value in a set of reserve polygons.
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


### sum_polygon_values ###
#
#' Sum area of a polygon layer inside a set of reserve polygons.
#'
#' Sums the area of a polygon dataset, or of each group with the polygon dataset, within each reserve polygon.
#' 
#' Sums are returned in a tibble which can be joined to \code{reserves_sf} using a \code{dplyr::left_join()}
#'  and \code{dplyr::pivot_wider()}.
#'  
#'  Projections of \code{reserves_sf} and \code{sum_polygon} must match and are assumed to have units in meters.
#'
#' @param reserves_sf sf object of reserves in which to sum raster values.
#' @param reserves_id String representing unique id column in \code{reserves_sf}.
#' @param sum_polygon sf object of one or more polygons to sum.
#' @param sum_polygon_groups Column name in \code{sum_polygon} holding grouping variables. If provided, area of each group is summed, otherwise
#' full area of \code{sum_polygon} is summed. 
#' @param class_vals Vector of class values in \code{sum_polygon_groups} to include in the output. Defaults to all non-NA values in \code{sum_polygon_groups}.
#' Ignored if \code{sum_polygon_groups} is not provided.
#' @param fill_zeros Should missing values be included with an area of zero? Default is TRUE.
#'
#' @return A tibble listing area in km2 for \code{sum_polygon} or \code{sum_polygon_groups} in each reserve.
#' 
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @export
#'
#' @examples
#' library(dplyr)
#' reserves <- dissolve_catchments_from_table(
#'   catchments_sample, 
#'   benchmark_table_sample,
#'   "network")
#' 
#' # Make a habitat polygon with 2 groups using catchments  
#' habitat <- catchments_sample %>%
#'              filter(CATCHNUM %in% c(191396, 191649)) %>%
#'              mutate(group = c(1,2)) %>%
#'              select(group)
#'              
#' sum_polygon_values(reserves, "network", habitat)
#' sum_polygon_values(reserves, "network", habitat, "group")
#' sum_polygon_values(reserves, "network", habitat, "group", c(1))
#' sum_polygon_values(reserves, "network", habitat, "group", c(2))
#' sum_polygon_values(reserves, "network", habitat, "group", c(2), fill_zeros = FALSE)
#' sum_polygon_values(reserves, "network", habitat, "group", fill_zeros = FALSE)
#' 
#' # To join results back to reserves
#' library(tidyr)
#' library(dplyr)
#' reserves %>%
#'   left_join(
#'     pivot_wider(
#'       sum_polygon_values(reserves, "network", habitat, "group"), 
#'       names_from = group, values_from = area_km2)
#'    )
sum_polygon_values <- function(reserves_sf, reserves_id, sum_polygon, sum_polygon_groups = "", class_vals = c(), fill_zeros = TRUE){
  
  stopifnot(sf::st_crs(reserves_sf) == sf::st_crs(sum_polygon))
  
  sf::st_agr(reserves_sf) = "constant"
  sf::st_agr(sum_polygon) = "constant"
  
  if(nchar(sum_polygon_groups) > 0){
    # Grouped code
    out_df <- dplyr::tibble(a = as.character(), b = as.character(), area_km2 = as.numeric()) # make table template
    names(out_df)[names(out_df) %in% c("a","b")] <- c(reserves_id, sum_polygon_groups)
    
    sum_polygon_dslv <- sum_polygon %>%
      dplyr::group_by(.data[[sum_polygon_groups]]) %>%
      sf::st_union() # make sure sum_polygon is a single feature per group
    
    all_groups <- unique(sum_polygon[[sum_polygon_groups]]) # get list of all groups
    
    for(reserve in reserves_sf[[reserves_id]]){ # loop reserves and intersect grouped sum_polygon
    
      # need to split this operation into two so the grouped sum_polygons can be made "constant". Otherwise we get the warning msg.
      df <- sum_polygon %>%
        dplyr::group_by(.data[[sum_polygon_groups]])
      sf::st_agr(df) = "constant"
      
      df <- df %>%
        sf::st_intersection(reserves_sf[reserves_sf[[reserves_id]] == reserve,]) %>%
        dplyr::mutate(area_km2 = round(as.numeric(sf::st_area(.data$geometry)) / 1000000, 3)) %>% # assumes units of m
        sf::st_drop_geometry() %>%
        dplyr::select(.data[[reserves_id]], .data[[sum_polygon_groups]], .data$area_km2) %>%
        dplyr::as_tibble()
      
      if(fill_zeros){
        # fill in missing values with 0
        for(group_id in all_groups[!all_groups %in% df[[sum_polygon_groups]]]){
          add_row <- dplyr::tibble(a = reserve, b = group_id, area_km2 = 0)
          names(add_row)[names(add_row) %in% c("a","b")] <- c(reserves_id, sum_polygon_groups)
          df <- rbind(df, add_row)
        }
      }
      out_df <- rbind(out_df, df)
      
      # filter by class_vals if requested
      if(length(class_vals > 0)){
        out_df <- out_df %>%
          dplyr::filter(.data[[sum_polygon_groups]] %in% class_vals)
      }
    }
    
  } else{
    # Non grouped code
    out_df <- dplyr::tibble(a = as.character(), area_km2 = as.numeric())
    names(out_df)[names(out_df) == "a"] <- c(reserves_id)
    
    sum_polygon_dslv <- sum_polygon %>%
      sf::st_union() # make sure sum_polygon is a single feature
    
    for(reserve in reserves_sf[[reserves_id]]){ # loop reserves and intersect sum_polygon
      
      df <- reserves_sf[reserves_sf[[reserves_id]] == reserve,] %>%
        sf::st_intersection(sum_polygon_dslv) %>%
        dplyr::mutate(area_km2 = round(as.numeric(sf::st_area(.data$geometry)) / 1000000, 3)) %>% # assumes units of m
        sf::st_drop_geometry() %>%
        dplyr::select(.data[[reserves_id]], .data$area_km2) %>%
        dplyr::as_tibble()
      
      # Fill in zeros if missing (i.e. if the polygons don't intersect)
      if(fill_zeros & nrow(df) == 0){
        df <- dplyr::tibble(a = reserve, area_km2 = 0)
        names(df)[names(df) == "a"] <- c(reserves_id)
      }
      out_df <- rbind(out_df, df)
    }
  }
  return(out_df)
}