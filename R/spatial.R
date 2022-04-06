catchnums_in_polygon <- function(pa_sf, pa_id, catchments_sf){
  
  # Convert catchments to centroids and intersects with PAs
  # Output is a df of catchments in each polygon, where column names are unique ids from pa_sf. 
  # Long table would be better but this matches the output from BUILDER.
  
  # Using centroids can result in the inclusion of catchments that do not actually intersect pa_sf.
  # Instead use st_PointOnSurface, the calculation for which is explained here: https://gis.stackexchange.com/questions/76498/how-is-st-pointonsurface-calculated?newreg=839f6b14ce6b40c9b63956954a3ab969
  # This is essential when PAs were constructed using catchments, should be an acceptable method when non-catchment PAs are used too.
  
  # pa_id cannot be "CATCHNUM"
  if(pa_id == "CATCHNUM"){
    stop("pa_id cannot be 'CATCHNUM'")
  }
  
  sf::st_agr(catchments_sf) = "constant"
  
  catch_within <- catchments_sf %>% 
    sf::st_point_on_surface() %>% # get catchment centroids. 
    sf::st_within(pa_sf) %>% # test within for all centroids in all PAs, returns a row for each match. row.id is a catchnum index, col.id is a PA index.
    as.data.frame()
  
  catchments_sf$key <- 1:nrow(catchments_sf) # add a key column to sf table. Must be an index to match st_within output
  sf_catch_key <- sf::st_drop_geometry(catchments_sf[c("key","CATCHNUM")])
  pa_sf$key <- 1:nrow(pa_sf) # add key to pa_sf
  pa_key <- sf::st_drop_geometry(pa_sf[c("key",pa_id)])
  
  # convert indexes from st_within to catchnums using the keys to join
  tbl_long <- catch_within %>%
    dplyr::left_join(sf_catch_key, by = c("row.id" = "key")) %>%
    dplyr::left_join(pa_key, by = c("col.id" = "key")) %>%
    dplyr::select(.data$CATCHNUM, .data[[pa_id]]) %>%
    dplyr::arrange(.data[[pa_id]])
  
  # convert long table to wide table with missing values as NA
  out_tab <- long_to_wide(tbl_long, pa_id, "CATCHNUM")
  return(out_tab)
}

### dissolve_catchments_from_table ###
#
#' Get lists of catchments from a table and dissolve.
#' 
#' Takes lists of catchments from a table where each column represents a polygon to be created, and dissolves the 
#' catchments into a single polygon feature. Optionally adds area in km2, and area-weighted intactness.
#' 
#' \code{input_table} is in the form exported from BUILDER, where each column lists catchment ids, and each column
#' name represents the unique identifier of the output polygon.
#' 
#' This function is used to create benchmark polygons using tables created by BUILDER, and upstream polygons using
#' tables created by [get_upstream_catchments()].
#' 
#' The optional \code{dissolve_list} parameter can be used to filter the column names in \code{input_table} that will
#' be dissolved. The \code{dissolve_list} can also contain combinations of column name separated by \code{__}, in this
#' case the output polygon will dissolve the combined area of catchments from all columns in the string. For example, 
#' the string "PA1__PA2" in the \code{dissolve_list} will output a polygon representing the dissolved area of all
#' catchments listed in the PA1 and PA2 columns of \code{input_table}.
#' 
#' @param catchments_sf sf object of the catchments dataset with unique identifier column: CATCHNUM.
#' @param input_table Data frame where column names are polygon names and rows are catchments making up each polygon.
#' e.g. output from [get_upstream_catchments()].
#' @param out_feature_id String representing the output column name holding the polygon unique identifiers.
#' @param calc_area If TRUE, an area_km2 column is added to the output containing the polygon area.
#' @param intactness_id Optional string identifying an intactness column (values between 0 and 1) in catchments_sf. If provided, 
#' used to calculate the area weighted intactness (AWI) of the dissolved polygons.
#' @param dissolve_list Vector of columns in \code{input_table} to include in the output. Defaults to colnames(input_table).
#'   Can also dissolve multiple columns from \code{input_table} together by combining column names with \code{__} (e.g. PB_0001__PB_0002).
#'   
#' @return A sf object of polygons with unique identifier column \code{out_feature_id}.
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @export
#'
#' @examples
#' library(sf)
#' 
#' # Individual benchmarks
#' benchmarks <- dissolve_catchments_from_table(
#'   catchments_sf = catchments_sample, 
#'   input_table = benchmark_table_sample, 
#'   out_feature_id = "network",
#'   dissolve_list = c("PB_0001", "PB_0002"))
#' plot(benchmarks)
#' 
#' # Benchmarks combined into a network
#' network <- dissolve_catchments_from_table(
#'   catchments_sf = catchments_sample, 
#'   input_table = benchmark_table_sample, 
#'   out_feature_id = "network",
#'   dissolve_list = c("PB_0001__PB_0002"))
#' plot(network)
#' 
#' # Upstream polygons
#' upstream_table <- get_upstream_catchments(benchmarks, "network", catchments_sample)
#' dissolve_catchments_from_table(
#'   catchments_sample, 
#'   upstream_table, 
#'   "network",
#'   TRUE)
#'   
dissolve_catchments_from_table <- function(catchments_sf, input_table, out_feature_id, calc_area = FALSE, intactness_id = "", dissolve_list = c()){
  
  catchments_sf <- check_catchnum(catchments_sf) # check for CATCHNUM and make character
  check_for_geometry(catchments_sf)
  input_table <- remove_oid(input_table) #drop oid column is it exists
  
  # get colnames to process
  if(length(dissolve_list > 0)){
    feature_list <- dissolve_list
  } else{
    feature_list <- colnames(input_table)
  }
  
  saveCount <- 1
  for(col_id in feature_list){
    
    # separate names if combination
    col_ids <- sep_network_names(col_id)
    
    # get list of catchments
    catchments_list <- get_catch_list(col_ids, input_table)
    
    # dissolve based on parameters
    if(calc_area){
      dslv <- catchments_sf %>%
        dplyr::filter(.data$CATCHNUM %in% catchments_list) %>%
        dplyr::summarise(geometry = sf::st_union(.data$geometry)) %>%
        dplyr::mutate(id = col_id,
                      area_km2 = round(as.numeric(sf::st_area(.data$geometry) / 1000000), 2))
    } else{
      dslv <- catchments_sf %>%
        dplyr::filter(.data$CATCHNUM %in% catchments_list) %>%
        dplyr::summarise(geometry = sf::st_union(.data$geometry)) %>%
        dplyr::mutate(id = col_id)
    }
    
    # join AWI if requested
    if(nchar(intactness_id) > 0){
      if(intactness_id %in% colnames(catchments_sf)){
        awi <- catchments_sf %>%
          dplyr::filter(.data$CATCHNUM %in% catchments_list) %>%
          dplyr::mutate(area = as.numeric(sf::st_area(.data$geometry))) %>%
          sf::st_drop_geometry() %>%
          dplyr::summarise(AWI = sum(.data[[intactness_id]] * .data$area) / sum(.data$area))
        
        dslv$AWI <- round(awi$AWI, 4)
      } else{
        warning(paste0("Area-weighted intactness cannot be calculated, ", '"', intactness_id, '"', " not in catchments_sf"))
      }
    }
    
    # set out name
    names(dslv)[names(dslv) == "id"] <- out_feature_id
    
    # reorder columns - move geometry to last
    dslv <- dslv  %>%
      dplyr::relocate(.data$geometry, .after = dplyr::last_col())
    
    # append to df
    if(saveCount == 1){
      out_sf <- dslv
      saveCount <- saveCount + 1
    } else{
      out_sf <- rbind(out_sf, dslv)
    }
  }
  return(out_sf)
}


### extract_catchments_from_table ###
#
#' Get list of catchments from a table and extract to a new feature.
#' 
#' Takes a list of catchments from a table column using the column name, and extracts the 
#' catchments from the catchments dataset. If multiple column names are provided, the unique combination of 
#' catchments from all columns is returned.
#' 
#' @param catchments_sf sf object of the catchments dataset with unique identifier column: CATCHNUM.
#' @param input_table Data frame where column names are polygon names and rows are catchments making up each polygon.
#' e.g. output from [get_upstream_catchments()].
#' @param extract_feature_id String representing the column name to extrat from \code{input_table}. Can also take
#' a vector of names in which case multiple columns will be extracted.
#' @param out_feature_id String representing the output column name holding the polygon unique identifier.
#'   
#' @return A sf object of catchments with unique identifier column \code{out_feature_id}.
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @export
#'
#' @examples
#' 
#' extract_catchments_from_table(catchments_sample, benchmark_table_sample, "PB_0001", "network")
#' extract_catchments_from_table(
#'   catchments_sample, benchmark_table_sample, c("PB_0001", "PB_0002"), "network")
extract_catchments_from_table <- function(catchments_sf, input_table, extract_feature_id, out_feature_id){
  
  catchments_sf <- check_catchnum(catchments_sf) # check for CATCHNUM and make character
  check_for_geometry(catchments_sf)
  
  # get list of catchments
  catchments_list <- get_catch_list(extract_feature_id, input_table)
    
  # extract
  ex <- catchments_sf %>%
    dplyr::filter(.data$CATCHNUM %in% catchments_list) %>%
    dplyr::mutate(id = paste0(extract_feature_id,collapse="__")) %>%
    dplyr::select(.data$CATCHNUM,.data$id)
    
  # set out name
  names(ex)[names(ex) == "id"] <- out_feature_id
    
  return(ex)
}


### append_reserve ###
#
#' Adds a polygon to a sf object.
#'
#' Used to add reserves that are not output from BUILDER to a sf object of benchmarks.
#' For example, adding an existing protected area (PA) to an sf object of benchmarks. This allows
#' networks of combined benchmarks and existing PAs to be built using [benchmarks_to_networks()].
#'
#' @param benchmarks_sf sf object with unique id column named \code{network}, typically the output from
#'  [dissolve_catchments_from_table()].
#' @param add_reserve sf object to add as a single additional reserve to benchmarks_sf. All features will
#' be dissolved into a single POLYGON or MULTIPOLYGON feature to append to benchmarks_sf. If multiple add_reserve
#' object are required, add them using multiple calls to [append_reserve()].
#' @param reserve_name String that will become the reserve name in the benchmarks_sf \code{network} column.
#'
#' @return sf object matching benchmarks_sf.
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @export
#'
#' @examples
#' library(sf)
#' library(dplyr)
#' benchmarks <- dissolve_catchments_from_table(
#'   catchments_sample, 
#'   benchmark_table_sample, 
#'   "network",
#'   dissolve_list = c("PB_0001", "PB_0002"))
#' pa_1 <- data.frame(lon = c(-85, -82.5, -83), lat = c(51, 51.5, 50.5)) %>%
#'           sf::st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
#'           dplyr::summarise(geometry = st_combine(geometry)) %>%
#'           sf::st_cast("POLYGON") %>%
#'           sf::st_transform(st_crs(benchmarks)) %>%
#'           dplyr::mutate(pa_name="a protected area", ha=9999, class="provincial")
#' append_reserve(benchmarks, pa_1, "PA_1")
append_reserve <- function(benchmarks_sf, add_reserve, reserve_name){
  
  benchmarks_sf <- check_network(benchmarks_sf)
  check_for_geometry(benchmarks_sf)
  stopifnot(sf::st_crs(benchmarks_sf) == sf::st_crs(add_reserve))
  
  # dissolve add_reserve into one feature
  add_reserve <- add_reserve %>%
    dplyr::summarise(geometry = sf::st_union(.data$geometry)) %>%
    dplyr::mutate(network = reserve_name)
  
  # append to benchmarks_sf
  benchmarks_sf <- rbind(benchmarks_sf, add_reserve)
  
  return(benchmarks_sf)
}


### benchmarks_to_networks ###
#
#' Combine a set of benchmark polygons into a network.
#'
#' Takes a set of individual benchmark polygons and/or generic reserve polygons and combines them into network 
#' polygons based on a list of network names. Generally used for combining benchmarks into networks, but can also 
#' be used to build networks using any input reserve polygons.
#' 
#' For large lists of networks (>10,000), we recommend subsetting the network_list and making multiple calls
#' to \code{\link{benchmarks_to_networks}}.
#'
#' @param benchmarks_sf sf object with unique id column named \code{network}, typically the output from
#'  [dissolve_catchments_from_table()].
#' @param network_list Vector of strings detailing network names to be built. Typically network names built using [gen_network_names()] 
#' (e.g. PB_0001__PB_0002). All network names must include names from the benchmarks_sf \code{network} column, separated by \code{"__"} (double underscore).
#'
#' @return A sf object of networks.
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @export
#'
#' @examples
#' benchmarks <- dissolve_catchments_from_table(
#'   catchments_sample, 
#'   benchmark_table_sample, 
#'   "network",
#'   dissolve_list = colnames(benchmark_table_sample))
#' network_names <- gen_network_names(benchmarks$network, 2)
#' benchmarks_to_networks(benchmarks, network_names)
#' 
#' benchmarks_to_networks(benchmarks, c("PB_0001__PB_0002"))

benchmarks_to_networks <- function(benchmarks_sf, network_list){
  
  benchmarks_sf <- check_network(benchmarks_sf)
  check_for_geometry(benchmarks_sf)
  
  my_list <- list()
  for(net in network_list){
    
    # get benchmark names and check they are in benchmarks_sf$network
    nets <- sep_network_names(net)
    check_benchmark_names(nets, benchmarks_sf)
    
    # dissolve the network
    row_sfc <- benchmarks_sf %>%
      dplyr::filter(.data$network %in% nets) %>%
      sf::st_union()
    row <- sf::st_sf(network = net, geometry = row_sfc)
    
    my_list <- append(my_list, list(row))
  }
  
  if(utils::packageVersion("dplyr") > "1.0.0"){
    out_sf <- dplyr::bind_rows(my_list) # this is much faster than do.call but requires dplyr >0.9 for bind_rows to work with sf objects
  } else{
    out_sf <- do.call(rbind, my_list)
  }
  return(out_sf)
}


### list_overlapping_benchmarks ###
#
#' List all pairs of touching/overlapping benchmarks.
#'
#' For all touching/overlapping polygons in the input file, returns the network_name of the overlapping
#' pairs.
#' 
#' This can be converted to a list of pairs of individual names using \code{sep_network_names(list_overlapping_benchmarks())}. 
#' 
#'
#' @param benchmarks_sf sf object with unique id column named \code{network}, typically the output from
#'  [dissolve_catchments_from_table()], possibly with additional reserves added using [append_reserve()].
#'
#' @return Vector of network names constructed using the \code{"__"} separator.
#' @export
#'
#' @examples
#' benchmarks <- dissolve_catchments_from_table(
#'   catchments_sample, 
#'   benchmark_table_sample, 
#'   "network",
#'   dissolve_list = colnames(benchmark_table_sample))
#' list_overlapping_benchmarks(benchmarks)
#' 
#' sep_network_names(list_overlapping_benchmarks(benchmarks))

list_overlapping_benchmarks <- function(benchmarks_sf){
  
  benchmarks_sf <- check_network(benchmarks_sf)
  
  df <- as.data.frame(sf::st_intersects(benchmarks_sf, benchmarks_sf)) # get pairwise intersects
  
  # convert to list, drop duplicates and self intersects, sort names
  l <- unique(
    lapply(1:nrow(df), function(x){ 
      if(df[x,1] != df[x,2]){
        sort(c(df[x,1], df[x,2]))
        }
      })
    )
  l <- l[!sapply(l, is.null)] # remove null
  
  return_names <- sapply(l, function(x){ # convert indexes to names
    benchmarks <- paste0(sort(c(benchmarks_sf$network[x[1]], benchmarks_sf$network[x[2]])), collapse = "__")
  })
  return(return_names)
}