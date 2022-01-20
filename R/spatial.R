### catchments_to_benchmarks ###
#
#' Dissolve a set of catchments to create benchmark polygons.
#'
#' Takes a list of catchments defining a benchmark or a network of multiple benchmarks, and dissolves the 
#' catchments into a single polygon feature.
#'
#' @param benchmark_table Data frame where columns are benchmark names and rows are catchments making up the benchmark. e.g. the "COLUMN_All_Unique_BAs" table 
#'   output by BUILDER.
#' @param catchments_sf sf object of the catchments dataset with unique identifier column: CATCHNUM .
#' @param network_list Vector of networks to build - usually just the list of benchmarks in the benchmark_table (e.g. PB_0001), but can also be combinations of 
#'   benchmarks (e.g. PB_0001__PB_0002) made using \code{\link{gen_network_names()}} (usually it is more efficient to build individual benchmarks then merge them into
#'   networks using \code{\link{benchmarks_to_networks()}}).
#'
#' @return A sf object of networks.
#' @importFrom magrittr %>%
#' @export
#'
#' @examples
# need to load sf in the example otherwise filtering an sf object causes an error. Could also solve this by putting example inside requireNamespace()
#' library(sf)
#' 
#' # Individual benchmarks
#' benchmarks <- catchments_to_benchmarks(
#'   benchmark_table_sample, 
#'   catchments_sample, 
#'   c("PB_0001", "PB_0002"))
#' plot(benchmarks)
#' 
#' # Benchmarks combined into a network
#' network <- catchments_to_benchmarks(
#'   benchmark_table_sample, 
#'   catchments_sample, 
#'   c("PB_0001__PB_0002"))
#' plot(network)
catchments_to_benchmarks <- function(benchmark_table, catchments_sf, network_list){
  
  catchments_sf <- check_catchnum(catchments_sf) # check for CATCHNUM and make character
  check_for_geometry(catchments_sf)
  
  saveCount <- 1
  for(net in network_list){
    
    # separate names if network
    nets <- sep_network_name(net)
    
    # get list of catchments
    net_catchments_list <- get_catch_list(nets, benchmark_table)
    
    # subset
    net_catchments <- catchments_sf %>%
      dplyr::filter(CATCHNUM %in% net_catchments_list)
    
    # dissolve
    net_catchments_sfc <- net_catchments %>%
      #sf::st_snap(x = ., y = ., tolerance = 0.0001) %>% # st_snap is very slow. Run st_snap on provided catchments instead.
      sf::st_union()
    net_catchments_diss <- sf::st_sf(network = net, geometry = net_catchments_sfc)
    
    # append to df
    if(saveCount == 1){
      out_sf <- net_catchments_diss
      saveCount <- saveCount + 1
    } else{
      out_sf <- rbind(out_sf, net_catchments_diss)
    }
  }
  return(out_sf)
}

### append_reserve ###
#
#' Adds a polygon to a sf object.
#'
#' Used to add reserves that are not output from BUILDER to a sf object of benchmarks.
#' For example, adding an existing protected area (PA) to an sf object of benchmarks. This allows
#' networks of combined benchmarks and existing PAs to be built using \code{\link{benchmarks_to_networks()}}.
#'
#' @param benchmarks_sf sf object with unique id column named \emph{network}, typically the output from
#'  \code{\link{catchments_to_benchmarks()}}.
#' @param add_reserve sf object to add as a single additional reserve to benchmarks_sf. All features will
#' be dissolved into a single POLYGON or MULTIPOLYGON feature to append to benchmarks_sf. If multiple add_reserve
#' object are required, add them using multiple calls to \code{\link{append_reserve}}.
#' @param reserve_name String that will become the reserve name in the benchmarks_sf \emph{network} column.
#'
#' @return sf object matching benchmarks_sf.
#' @importFrom magrittr %>%
#' @export
#'
#' @examples
#' library(sf)
#' library(dplyr)
#' benchmarks <- catchments_to_benchmarks(
#'   benchmark_table_sample, 
#'   catchments_sample, 
#'   c("PB_0001", "PB_0002"))
#' pa_1 <- data.frame(lon = c(-85, -82.5, -83), lat = c(51, 51.5, 50.5)) %>%
#'           sf::st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
#'           dplyr::summarise(geometry = st_combine(geometry)) %>%
#'           sf::st_cast("POLYGON") %>%
#'           sf::st_transform(st_crs(benchmarks)) %>%
#'           dplyr::mutate(pa_name="a protected area", ha=9999, class="provincial")
#' append_reserve(benchmarks, pa_1, "PA_1")
append_reserve <- function(benchmarks_sf, add_reserve, reserve_name){
  
  check_for_network(benchmarks_sf)
  check_for_geometry(benchmarks_sf)
  
  # dissolve add_reserve into one feature
  add_reserve <- add_reserve %>%
    dplyr::summarise(geometry = sf::st_union(geometry)) %>%
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
#' polygons based on a list of network names.
#' 
#' Generally used for combining benchmarks into networks, but can also be used to build networks using any 
#' input reserve polygons.
#' 
#' For large lists of networks (>10,000), we recommend subsetting the network_list and making multiple calls
#' to \code{\link{benchmarks_to_networks}}.
#'
#' @param benchmarks_sf sf object with unique id column named \emph{network}, typically the output from
#'  \code{\link{catchments_to_benchmarks()}}.
#' @param network_list Vector of strings detailing network names to be built. Typically network names built using \code{\link{gen_network_names()}} 
#' (e.g. PB_0001__PB_0002). All network names must include names from the benchmarks_sf \emph{network} column, separated by "__" (double underscore).
#'
#' @return A sf object of networks.
#' @importFrom magrittr %>%
#' @export
#'
#' @examples
#' benchmarks <- catchments_to_benchmarks(
#'   benchmark_table_sample, 
#'   catchments_sample, 
#'   colnames(benchmark_table_sample))
#' network_names <- gen_network_names(benchmarks$network, 2)
#' benchmarks_to_networks(benchmarks, network_names)
#' 
#' benchmarks_to_networks(benchmarks, c("PB_0001__PB_0002"))

benchmarks_to_networks <- function(benchmarks_sf, network_list){
  
  check_for_network(benchmarks_sf)
  check_for_geometry(benchmarks_sf)
  
  my_list <- list()
  for(net in network_list){
    
    # get benchmark names and check they are in benchmarks_sf$network
    nets <- sep_network_name(net)
    check_benchmark_sf_names(nets, benchmarks_sf)
    
    # dissolve the network
    row_sfc <- benchmarks_sf %>%
      dplyr::filter(network %in% nets) %>%
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