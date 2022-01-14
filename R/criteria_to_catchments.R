#' Sum areas of criteria raster values inside catchment polygons.
#'
#' For a given raster layer, sums the area of all unique values in the raster and adds summed area's as columns in the catchments dataset.
#'
#'
#' @param catchments_sf sf object of catchments
#' @param criteria_raster Raster object of the criteria layer that will be summed, with crs matching catchments
#' @param criteria_name String representing the criteria name that will provide the suffix in the new column names
#' @param class_vals Vector of the class values to sum. Defaults to including all unique values of the raster intersecting the catchments
#'
#' @return sf object matching catchments_sf, with the additional columns added
#' @importFrom magrittr %>%
#' @export
#'
#' @examples
#' criteria_to_catchments(catchments_sample, led_sample, "led")

criteria_to_catchments <- function(catchments_sf, criteria_raster, criteria_name, class_vals = c()){
  
  catchments_sf <- check_catchnum(catchments_sf) # check for CATCHNUM and make character
  
  # check raster and catchments crs matches
  if(sf::st_crs(raster::crs(criteria_raster)) != sf::st_crs(catchments_sf)){
    stop("Raster projection does not match catchments, reproject raster to match catchments")
  }
  
  cell_area <- prod(raster::res(criteria_raster)) / 1000000 # convert to area in km2, assumes raster res is in metres
  
  # split catchments into blocks of 50 for processing
  catch_list <- unique(as.character(catchments_sf$CATCHNUM))
  catch_list_grouped <- split(catch_list, ceiling(seq_along(catch_list)/50))
  
  block_counter <- 1
  catch_counter <- 1
  for(catch_list_i in catch_list_grouped){
    
    print(paste0("block ", block_counter, " of ", length(catch_list_grouped)))
    block_counter <- block_counter + 1
    
    catchments_i <- catchments_sf[catchments_sf$CATCHNUM %in% catch_list_i,] # subset catchments
    x <- exactextractr::exact_extract(criteria_raster, catchments_i, progress = FALSE) # extract
    
    names(x) <- catch_list_i # name the list elements by their associated CATCHNUM
    
    # sum all values areas. Filter by class_vals later when we can calculate all unique values in all catchments
    for(catch_i in catch_list_i){
      i_sums <- x[[catch_i]] %>% 
        dplyr::mutate(area = coverage_fraction * cell_area) %>%
        dplyr::group_by(value) %>%
        dplyr::summarise(area_km2 = sum(area)) %>%
        dplyr::mutate(CATCHNUM = catch_i)
      
      # append into long tibble
      if(catch_counter == 1){
        df_long <- i_sums
        catch_counter <- catch_counter + 1
      } else{
        df_long <- rbind(df_long, i_sums)
      }
    }
  }
  
  # set up class_vals
  df_long_vals <- unique(df_long$value)
  if(length(class_vals) == 0){
    class_vals <- df_long_vals
  }
  
  missing_class_vals <- class_vals[!class_vals %in% df_long_vals]
  
  # pivot to wide table
  df_wide <- df_long %>%
    dplyr::add_row(value = missing_class_vals, area_km2 = 0.0, CATCHNUM=df_long$CATCHNUM[1]) %>% # force in all class_vals. Targets could be evaluated from a broader reference area that does include all class_vals
    tidyr::pivot_wider(id_col = CATCHNUM,
                       names_from = value, 
                       values_from = area_km2, 
                       values_fill = 0,
                       names_prefix = paste0(criteria_name, "_"))
  catchments_sf <- catchments_sf %>%
    dplyr::select(-dplyr::matches(setdiff(names(df_wide), "CATCHNUM"))) %>% # remove any df_wide columns already in catchments_sf. Effectively overwrites the old with the new columns
    dplyr::left_join(df_wide, by = "CATCHNUM")
  
  return(catchments_sf)
}