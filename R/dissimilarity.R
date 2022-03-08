# This should be written in RCPP
ks_stat <- function(refVal, netVal) {
  # calculate KS statistic (representation index)
  ri <- suppressWarnings(round(ks.test(refVal, netVal)[[1]][[1]], 3))
  return(ri)
}

# This should be written in RCPP
bc_stat <- function(refVal, netVal) {
  
  x1 <- dplyr::as_tibble(refVal) %>%
    dplyr::count(.data$value)
  names(x1) <- c("cat","strata")
  
  x2 <- dplyr::as_tibble(netVal) %>%
    dplyr::count(.data$value)
  names(x2) <- c("cat","reserve")
  
  x <- merge(x1,x2,by="cat",all=T)
  #x$cat <- as.character(x$cat)
  x$strata <- as.numeric(x$strata)
  x$reserve <- as.numeric(x$reserve)
  x$reserve[is.na(x$reserve)] <- 0
  x$reserve[is.na(x$strata)] <- 0 # this is needed in case there is one reserve pixel and no strata pixel
  x$strata[is.na(x$strata)] <- 0
  x$strata <- x$strata/sum(x$strata)
  x$reserve <- x$reserve/sum(x$reserve)
  
  # calculate Bray-Curtis dissimilariy
  ri <- round(sum(abs(x$strata-x$reserve))/(sum(x$strata)+sum(x$reserve)), 3)
  return(ri)
}

ks_plot <- function(refVal, netVal, plotTitle="") {
  
  regLab <- "Reference area"
  netLab <- "Network"
  
  z1 <- c(refVal, netVal)
  z2 <- c(rep(regLab,length(refVal)), rep(netLab,length(netVal)))
  zz <- data.frame(cbind(z1,z2),stringsAsFactors=FALSE)
  names(zz) <- c("values","criteria")
  zz$values <- round(as.numeric(zz$values),3)
  
  # create and save density plot
  p <- ggplot2::ggplot(zz, ggplot2::aes(x=.data$values)) + ggplot2::geom_density(ggplot2::aes(group=.data$criteria, color=.data$criteria)) +
    ggplot2::ggtitle(plotTitle) +
    ggplot2::labs(x="Indicator value", y="Density")

  return(p)
}

bc_plot <- function(refVal, netVal, plotTitle="", labels=data.frame()) {
  
  x1 <- dplyr::as_tibble(refVal) %>%
    dplyr::count(.data$value)
  names(x1) <- c("cat","strata")
  
  x2 <- dplyr::as_tibble(netVal) %>%
    dplyr::count(.data$value)
  names(x2) <- c("cat","reserve")
  
  x <- merge(x1,x2,by="cat",all=T)
  x <- x[order(as.integer(as.character(x$cat))),]
  x$strata <- as.numeric(x$strata)
  x$reserve <- as.numeric(x$reserve)
  x$reserve[is.na(x$reserve)] <- 0
  x$reserve[is.na(x$strata)] <- 0 # this is needed in case there is one reserve pixel and no strata pixel
  x$strata[is.na(x$strata)] <- 0
  x$strata <- x$strata/sum(x$strata) #as.integer(x$strata)
  x$reserve <- x$reserve/sum(x$reserve) #as.integer(x$reserve)
  
  # prep labels if present
  if(nrow(labels) > 0 & "values" %in% names(labels)){
    for(i in labels$values){
      if(i %in% x$cat){
        x$cat[x$cat == i] <- labels$label[labels$values == i]
      }
    }
  }
  x$cat <- factor(x$cat, levels = x$cat)
  
  p <- ggplot2::ggplot(x, ggplot2::aes(x=.data$cat, y=.data$reserve)) + ggplot2::geom_bar(stat="identity", fill="white", colour="black") + ggplot2::coord_flip()
  p <- p + ggplot2::geom_point(data=x, ggplot2::aes(x=.data$cat, y=.data$strata), colour="black", size=3) + ggplot2::theme(legend.position = "none")
  p <- p + ggplot2::labs(x="", y="Proportional area (dots indicate regional proportions)")
  p <- p + ggplot2::ggtitle(plotTitle)
  
  return(p)
}


### calc_dissimilarity ###
#
#' Calculate dissimilarity values for a set of polygons.
#'
#' For a list of features (e.g. benchmarks or networks), calculate the dissimilarity value between each feature and a reference area for the 
#' provided raster layer. Continuous rasters use the KS-statistic to compare distributions, categorical rasters use the Bray-Curtis
#' statistic.
#' 
#' Graphs comparing distributions can optionally be created and saved to a user provided file path.
#' 
#' NA values are always removed. For categorical rasters, values can optionally be subset for the calculation and graphs using 
#' \code{categorical_class_values}.
#'
#' @param reserves_sf sf object with unique id column named \code{network}
#' @param reference_sf sf object of the reference area to compare against.
#' @param raster_layer Raster object that will be clipped to the reference and reserve areas, with crs matching reserves_sf
#' @param raster_type 'categorical' will use Bray-Curtis, 'continuous' will use KS-statistic.
#' @param categorical_class_values Vector of raster values in \code{raster_layer} of type 'categorical' to include in the calculation. 
#' Allows unwanted values to be dropped. Defaults to include all non-NA values.
#' @param plot_out_dir Path to folder in which to save plots. Default is not to create plots. 
#' Only creates plots if valid file path is provided. Dir will be created if it doesn't exist.
#' @param categorical_class_labels Optional data.frame object with columns \code{values} and \code{labels} indicating the label to use in Bray-Curtis graphs
#' for each raster value. Defaults to using the raster values. Labels can be provided for all or a subset of values. See examples.
#'
#' @return A vector of dissimilarity values matching the order of the input \code{reserves_sf}. Optionally a dissimilarity plot saved in the 
#' \code{plot_out_dir} for each computed value.
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @importFrom stats ks.test
#' @export
#'
#' @examples
#' reserves <- catchments_to_benchmarks(
#'   benchmark_table_sample, 
#'   catchments_sample, 
#'   c("PB_0001", "PB_0002", "PB_0003"))
#' calc_dissimilarity(reserves, ref_poly, led_sample, 'categorical')
#' calc_dissimilarity(reserves, ref_poly, led_sample, 'categorical', c(1,2,3,4,5))
#' calc_dissimilarity(reserves, ref_poly, led_sample, 'categorical', c(1,2,3,4,5), 
#'   "C:/temp/plots", data.frame(values=c(1,2,3,4,5), labels=c("one","two","three","four","five")))
#' calc_dissimilarity(reserves, ref_poly, led_sample, 'continuous', plot_out_dir="C:/temp/plots")
calc_dissimilarity <- function(reserves_sf, reference_sf, raster_layer, raster_type, categorical_class_values=c(), plot_out_dir="", categorical_class_labels=data.frame()){
  
  # geometries should match
  stopifnot(sf::st_crs(reserves_sf) == sf::st_crs(reference_sf))
  stopifnot(sf::st_crs(reserves_sf) == sf::st_crs(raster_layer))
  
  # set up output vector
  result_vector <- c()
  
  # check raster_layer is valid
  if(!raster_type %in% c('categorical', 'continuous')){
    stop("raster_layer must be on of: 'categorical', 'continuous'")
  }
  
  # check for network column
  check_network(reserves_sf)
  
  # check geometry column is present in sf objects
  check_for_geometry(reference_sf)
  check_for_geometry(reserves_sf)
  
  # should plots be made? Attempt to create directory if it doesn't already exist
  if(nchar(plot_out_dir) > 0){
    make_plots <- TRUE
    dir.create(plot_out_dir, recursive = TRUE, showWarnings = FALSE)
  } else{
    make_plots <- FALSE
  }
  
  # get the reference area and make sure it is a single geometry
  ref_sf <- reference_sf %>%
    dplyr::summarise(geometry = sf::st_union(.data$geometry))
  
  # extract values in ref area
  ref_ext <- exactextractr::exact_extract(raster_layer, reference_sf, progress = FALSE)[[1]]
  
  # filter by class_vals if provided, remove NAs, only keep cells with majority in reference_sf
  if(raster_type == 'categorical' & length(categorical_class_values) > 0){
    reference_vals <- ref_ext %>%
      dplyr::filter(!is.na(.data$value)) %>%
      dplyr::filter(.data$value %in% categorical_class_values) %>%
      dplyr::filter(.data$coverage_fraction > 0.5) %>% # only keep values from cells with at least half their area in the polygon
      dplyr::pull(.data$value)
  } else{
    reference_vals <- ref_ext %>%
      dplyr::filter(!is.na(.data$value)) %>%
      dplyr::filter(.data$coverage_fraction > 0.5) %>%
      dplyr::pull(.data$value)
  }
  
  # split networks into blocks of 10 for processing
  net_list <- as.character(reserves_sf$network)
  net_list_grouped <- split(net_list, ceiling(seq_along(net_list)/10))
  
  # run in blocks of 10 - seems optimal for maintaining a fast extract
  counter <- 1
  for(net_list_g in net_list_grouped){
    
    message(paste0("block ", counter, " of ", length(net_list_grouped)))
    counter <- counter + 1
    
    reserves_sf_g <- reserves_sf[reserves_sf$network %in% net_list_g,] # subset dissolved networks by the block of networks
    x <- exactextractr::exact_extract(raster_layer, reserves_sf_g, progress = FALSE) # extract
    
    names(x) <- net_list_g # name the list elements by their associated netname
    
    for(net in net_list_g){
      
      # for each network in the block, extract the values...
      if(raster_type == 'categorical' & length(categorical_class_values) > 0){
        target_vals <- x[[net]] %>%
          dplyr::filter(!is.na(.data$value)) %>%
          dplyr::filter(.data$value %in% categorical_class_values) %>%
          dplyr::filter(.data$coverage_fraction > 0.5) %>% # only keep values from cells with at least half their area in the polygon
          dplyr::pull(.data$value)
      } else{
        target_vals <- x[[net]] %>%
          dplyr::filter(!is.na(.data$value)) %>%
          dplyr::filter(.data$coverage_fraction > 0.5) %>%
          dplyr::pull(.data$value)
      }
      
      # run dissimilarity
      if(raster_type == "categorical"){
        result <- bc_stat(reference_vals, target_vals)
      } else{
        result <- ks_stat(reference_vals, target_vals)
      }
      
      # add result t return vector
      result_vector <- c(result_vector, result)
      
      # generate plot if requested
      if(make_plots){
        plot_out_path <- file.path(plot_out_dir, paste0(net, ".png"))
        
        if(raster_type == 'categorical'){
          plt <- bc_plot(reference_vals, target_vals, plotTitle = paste0(net, " (BC=", result, ")"), labels=categorical_class_labels)
        } else{
          plt <- ks_plot(reference_vals, target_vals, plotTitle = paste0(net, " (KS=", result, ")"))
        }
        ggplot2::ggsave(plt, file=plot_out_path)
      }
    }
  }
  return(result_vector)
}