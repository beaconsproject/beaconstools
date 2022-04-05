getOrder1 <- function(catchment_df, current_catchment){
  # get ORDER1 value from the provided CATCHNUM in a data.table
  as.character(catchment_df$ORDER1[catchment_df$CATCHNUM==current_catchment])
}

getOrder2 <- function(catchment_df, current_catchment){
  # get ORDER2 value from the provided CATCHNUM in a data.table
  as.numeric(as.character(catchment_df$ORDER2[catchment_df$CATCHNUM==current_catchment]))
}

getOrder3 <- function(catchment_df, current_catchment){
  # get ORDER3 value from the provided CATCHNUM in a data.table
  as.character(catchment_df$ORDER3[catchment_df$CATCHNUM==current_catchment])
}

getBasin <- function(catchment_df, current_catchment){
  # get BASIN value from the provided CATCHNUM in a data.table
  as.numeric(as.character(catchment_df$BASIN[catchment_df$CATCHNUM==current_catchment]))
}

order3ToOrder2 <- function(order3){
  # replicating builder function to get order 2 from 3
  base64Codes <- c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "=", "A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N", "O", "P", "Q", "R", "S", "T", "U", "V", "W", "X", "Y", "Z", "_", "a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o", "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z")
  idx0 <- match(substr(order3, 1, 1), base64Codes) - 1 # C# indexes start at zero so minus 1
  idx1 <- match(substr(order3, 2, 2), base64Codes) - 1
  return(idx0 * length(base64Codes) + idx1)
}

utf32_greaterthan <- function(x, y){
  # is x greater than y?
  # used to identify upstream catchments based on comparison of ORDER1 and ORDER1_3 strings.
  
  # get vectors of code points to compare
  xx <- utf8ToInt(x)
  yy <- utf8ToInt(y)
  
  # what is the length of the shortest vector?
  ii <- min(c(length(xx), length(yy)))
  
  # for each index...
  for(i in 1:ii){
    
    # if the value is not the same...
    if(xx[i] != yy[i]){
      
      # test if x > y
      if(xx[i] > yy[i]){
        return(TRUE)
      } else{
        return(FALSE)
      }
    }
    # otherwise move on to next index
  }
  
  # If they are the same up to length ii, the upstream catchment will have more characters, so if x is longer than y, return true.
  if(length(xx) > length(yy)){
    return(TRUE)
  } else{
    return(FALSE)
  }
}

utf32_greaterthan_vectorized <- function(x, y){
  sapply(x, function(xx){
    utf32_greaterthan(xx, y)
  })
}

isUpstream <- function(catchment_df, current_catchment, other_catchment){
  
  # Test if other_catchment is upstream of current_catchment
  
  # First test: basin = BASIN and order1 = ORDER1 and order2 >= ORDER2
  if(getBasin(catchment_df, other_catchment) == getBasin(catchment_df, current_catchment)){
    if(getOrder1(catchment_df, other_catchment) == getOrder1(catchment_df, current_catchment)){
      if(getOrder2(catchment_df, other_catchment) >= getOrder2(catchment_df, current_catchment)){
        return(TRUE)
      }
    }
  }
  # Second test: basin = BASIN and order1 contains ORDER1 and order1 > ORDER1.ORDER3
  if(getBasin(catchment_df, other_catchment) == getBasin(catchment_df, current_catchment)){
    if(grepl(getOrder1(catchment_df, current_catchment), getOrder1(catchment_df, other_catchment))){
      if(
        utf32_greaterthan(
          getOrder1(catchment_df, other_catchment),
          paste0(getOrder1(catchment_df, current_catchment), getOrder3(catchment_df, current_catchment))
        )
      ){
        return(TRUE)
      }
    }
  }
  return(FALSE)
}

getAggregationUpstreamCatchments_BUILDER_method <- function(catchment_df, agg_catchments, neighbours_tib){
  # Get all upstream catchments of an aggregation of catchments (e.g. a list of catchnums in a PA).
  
  # This version copied from BUILDER code.
  # It moves along a stream segment testing only the neighbours of each upstream catchment.
  # If a catchment is upstream it's neighbours are also tested. This has the problem that if the catchments
  # have errors, the upstream progress can be blocked. For example the dissolveing of catchments during construction
  # to eliminate very small catchments can lead to catchments on a stream that should be adjacent, actually not being
  # neighbours.
  # This method also very slow in R.
  # Both problems fixed using the more R-friendly version of this function.
  
  # create inspected list to track upstream catchments that have been inspected
  lst_inspected <- c()
  
  # create list to hold upstream catchments
  lst_upstream <- c()
  
  for(aggCatchmntIdx in agg_catchments){ # for each catchment in the aggregation
    
    # create a pending queue
    pendingQueue <- c()
    
    # add aggCatchmntIdx to the queue
    pendingQueue <- c(pendingQueue, aggCatchmntIdx)
    
    while(length(pendingQueue) > 0){
      
      currentCatchment <- pendingQueue[1]
      
      # remove current catchment from pending queue
      pendingQueue <- pendingQueue[-1]
      
      # get current catchments neighbours
      neighbourCatchments <- neighbours_tib %>%
        dplyr::filter(.data$CATCHNUM == currentCatchment) %>%
        dplyr::pull(.data$neighbours)
      
      for(nbr in neighbourCatchments){
        # if not already inspected
        if(!nbr %in% lst_inspected){
          
          # if not in the aggregation
          if(!nbr %in% agg_catchments){
            
            # test if neighbour is upstream of current agg catchment
            if(isUpstream(catchment_df = catchment_df, current_catchment = aggCatchmntIdx, other_catchment = nbr)){
              
              # if yes, add to upstream list
              lst_upstream <- c(lst_upstream, nbr)
              
              # put this catchment in the queue so its neighbours can be tested
              pendingQueue <- c(pendingQueue, nbr)
              
              # add this catchment to the inspected list
              lst_inspected <- c(lst_inspected, nbr)
            } else{
              # not upstream, do nothing
            }
          } else{
            # if it's in the aggregation, put it in the pending queue so its neighbours can be tested
            #pendingQueue <- c(pendingQueue, nbr)
            #lst_inspected <- c(lst_inspected, nbr)
            
            ### Dropping these lines because if its in the aggregation it'll get tested as an aggCatchmntIdx
            ### The aggCatchmntIdx will still run even if it's already been tested here, so these lines are
            ### unnecessarily repeating the search for neighbors.
          }
        }
      }
    }
  }
  return(lst_upstream)
}

getAggregationUpstreamCatchments_R <- function(catchment_tab, agg_catchments){
  # attempting a more R friendly version with less looping
  
  # This version tests the entire catchment dataset for upstream catchments, repeated
  # for each catchment in the aggregation.
  
  # This is quite slow and many aggregation catchments are repeating the same query because
  # they are on the same stream. A few tricks to speed this up and minimise queries:
  # Remove agg_catchments from the dataframe being queried
  # Remove upstream catchments from the search dataframe so they can't be added multiple times
  # Remove any agg_catchments from the aggregation if they are upstream of a catchment that has already been queried.
  # Start the queries using the most downstream catchments in the aggregation, this will remove as many agg_catchments
  # as possible and minimize the number of queries run.
  
  # make ORDER2 numeric
  catchment_tab$ORDER2 <- as.numeric(as.character(catchment_tab$ORDER2))
  
  # get list of BASINs for the agg_catchments
  basins <- catchment_tab %>%
    dplyr::filter(.data$CATCHNUM %in% agg_catchments) %>%
    dplyr::pull(.data$BASIN) %>%
    unique()
  
  # filter catchments to only include matching BASINs, and to remove agg_catchments
  search_tab <- catchment_tab %>%
    dplyr::filter(!.data$CATCHNUM %in% agg_catchments,
                  .data$BASIN %in% basins)
  
  # make second table to hold just the agg catchments (hopefully faster to query 2 smaller tables than one big)
  agg_tab <- catchment_tab %>%
    dplyr::filter(.data$CATCHNUM %in% agg_catchments) %>%
    dplyr::arrange(nchar(.data$ORDER1), .data$ORDER2) # run the most downstream catchments first to remove as many agg_catchments as possible. This minimizes queries. Rough approximation of downstream is shorter ORDER1 and lower ORDER2 values.
    
  catchList <- c()
  agg_up_list <- c()
  for(current_catchment in agg_tab$CATCHNUM){
    
    #print(paste0(length(catchList), "  -  ", length(agg_up_list), "  -  ", current_catchment))
    
    if(!current_catchment %in% agg_up_list){ # skip if the current agg_catchment is upstream of one that has already been tested. They'll have the same result.
      
      current_BASIN <- getBasin(agg_tab, current_catchment)
      current_ORDER1 <- getOrder1(agg_tab, current_catchment)
      current_ORDER2 <- getOrder2(agg_tab, current_catchment)
      current_ORDER3 <- getOrder3(agg_tab, current_catchment)
      
      # Get all CATCHNUMS matching first test: basin = BASIN and order1 = ORDER1 and order2 >= ORDER2
      test1 <- search_tab %>%
        dplyr::filter(.data$BASIN == current_BASIN & 
                        .data$ORDER1 == current_ORDER1 &
                        .data$ORDER2 >= current_ORDER2) %>%
        dplyr::pull(.data$CATCHNUM)
        
      # Get all CATCHNUMS matching second test: basin = BASIN and order1 contains ORDER1 and order1 > ORDER1.ORDER3
      test2 <- search_tab %>%
        dplyr::filter(.data$BASIN == current_BASIN & 
                        grepl(current_ORDER1, .data$ORDER1) &
                        utf32_greaterthan_vectorized(.data$ORDER1, paste0(current_ORDER1, current_ORDER3))) %>%
        dplyr::pull(.data$CATCHNUM)
      
      # add new upstream catchments to out list
      catchList <- c(catchList, test1, test2)
      
      # remove test1 and test2 catchments from the search table so they can't be added again
      search_tab <- search_tab %>%
        dplyr::filter(!.data$CATCHNUM %in% c(test1, test2))
      
      ###################
      
      # add agg_catchments upstream of current_catchment to a list.
      # these do not need to be tested because they'll have the same result as the current catchment
      test1_agg <- agg_tab %>%
        dplyr::filter(.data$BASIN == current_BASIN &
                        .data$ORDER1 == current_ORDER1 &
                        .data$ORDER2 >= current_ORDER2) %>%
        dplyr::pull(.data$CATCHNUM)

      test2_agg <- agg_tab %>%
        dplyr::filter(.data$BASIN == current_BASIN &
                        grepl(current_ORDER1, .data$ORDER1) &
                        utf32_greaterthan_vectorized(.data$ORDER1, paste0(current_ORDER1, current_ORDER3))) %>%
        dplyr::pull(.data$CATCHNUM)

      agg_up_list <- c(agg_up_list, test1_agg, test2_agg)
      
    }
  }
  
  # Should already be unique
  outVals <- unique(catchList)
  return(outVals)
}

### get_upstream_catchments ###
#
#' Calculate upstream catchments for a set of input polygons.
#' 
#' Calculates all upstream catchments for each provided protected area polygon and returns as a table, with column names as the unique id 
#' of the protected areas.
#'
#' @param pa_sf sf object of protected area polygons.
#' @param pa_id String matching the unique identifier column in \code{pa_sf}.
#' @param catchments_sf sf object of the catchments dataset with unique identifier column: CATCHNUM .
#'
#' @return Tibble where each column name is a unique protected area id, and each row is a catchment making up the 
#' upstream area for that protected area. Blank rows are filled with NA.
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @export
#'
#' @examples
#' reserves <- dissolve_catchments_from_table(
#'   catchments_sample, 
#'   benchmark_table_sample,
#'   "network", 
#'   dissolve_list = c("PB_0001", "PB_0002", "PB_0003"))
#' get_upstream_catchments(reserves, "network", catchments_sample)
get_upstream_catchments <- function(pa_sf, pa_id, catchments_sf){
  
  if(!all(c("ORDER1", "ORDER2", "ORDER3", "BASIN", "CATCHNUM") %in% colnames(catchments_sf))){
    stop("catchments_sf must have attributes: ORDER1, ORDER2, ORDER3, BASIN, CATCHNUM")
  }
  
  # get list of catchnums in each PA
  pa_catchnums_tab <- catchnums_in_polygon(pa_sf, pa_id, catchments_sf)
  
  up_agg_list <- list()
  for(col_id in colnames(pa_catchnums_tab)){
    
    # get list of catchments
    agg_catchments <- get_catch_list(col_id, pa_catchnums_tab)
    up_agg <- getAggregationUpstreamCatchments_R(catchments_sf, agg_catchments)
    
    # add to up_agg_list
    up_agg_list[[col_id]] <- up_agg
  }
  
  out_df <- dplyr::as_tibble(list_to_wide(up_agg_list))
  
  return(out_df)
}