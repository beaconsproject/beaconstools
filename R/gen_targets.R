#' Generate target tables for representation analysis.
#'
#'Takes a reference area polygon and a classified raster for which representation will be avaluated. Calculates the proportion of each class in the reference area and multiples by reserve size to get target in km2.
#'
#' @param reference_sf sf object of the reference area we are aiming to represent
#' @param representation_raster Raster object of the representation layer classified into categorical classes
#' @param class_values A vector of classes in representation_raster to generate targets for. Defaults to all classes in the representation_raster.
#' @param reserve_size The area that targets will sum to. Generally the approximate reserve size of reserves being evaluated.
#'
#' @return A tibble with columns class (the list of class_values), area_km2 (the area of each class in the reference_sf), reserve_size (the provided reserve_size), class_proportion (area_km2/sum(area_km2)), target_km2 (class_proportion * reserve_area).
#' @export
#'
#' @examples
gen_targets <- function(reference_sf, representation_raster, class_values = c(), reserve_size){

  # requires packages sf, raster, exactextract

  cellsize <- raster::res(representation_raster)[1] / 1000
  cellarea <- cellsize * cellsize

  # if more than one feature in the reference object, dissolve into single geometry
  if(nrow(reference_sf) > 1){
    reference_sf <- reference_sf %>%
      dplyr::summarise(geometry = st_union(geometry))
  }

  x <- exactextractr::exact_extract(representation_raster, reference_sf)[[1]]

  # if no class values provided, use all values in extracted raster
  if(length(class_values) == 0){
    class_values <- unique(x$value)
  }

  # calc sum of each class value
  df <- x %>%
    dplyr::filter(value %in% class_values) %>%
    dplyr::mutate(area = coverage_fraction * cellarea) %>%
    dplyr::group_by(value) %>%
    dplyr::summarize(area_km2 = sum(area))

  names(df) <- c("class", "area_km2")

  # if any class_values not in df, add them with area of zero
  for(i in class_values){
    if(!i %in% df$class){
      df <- rbind(df, data.frame(class = i, area_km2 = 0))
    }
  }

  # order by class
  df <- df[order(df$class),]

  # add reserve size and calc targets
  df$reserve_size <- reserve_size
  df$class_proportion <- df$area_km2 / sum(df$area_km2)
  df$target_km2 <- round(df$class_proportion * df$reserve_size, 2)

  return(df)
}
