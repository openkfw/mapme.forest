#' Pre-Processing of tree cover layers
#'
#' Calculates a binary tree cover mask for a baseline year.
#		Two routines are available, the first applying a threshold detection
#		for percentage of tree cover and the other one applying a threshold
#		for clump detection removing groups of raster cells smaller than the threshold.
#'
#' @param inputForestMap A RasterLayer representing forest cover.
#'   For the primary use-case of this function tree cover is expected
#'   as values in the range [0,100] representing percentage and is used
#'   when the user wants to obtain a binary tree cover map.
#
#' @param thresholdCover A numeric vector of length one in the range of
#'   [0,100]. It is used as the definition criterium for forest areas in
#'   datasets like Hansen (2013), where a pixel's values indicates the
#'   percentage of coverage with trees. Any pixel below this threshold
#'   will get the value 0, any pixel above will get the value 1.
#'
#' @param thresholdClump A numeric of length one indicating the number of
#'   pixels which are classified as a clump. Any number of pixels smaller than
#'   this threshold will be classified to NA.
#'
#' @return A RasterLayer object with binary values when \code{thresholdCover} was
#'   specified and with raster cell clumps removed in case \code{thresholdClump} was
#'   specified. Either one of the parameters can be specified on its own.
#'
#' @export prepTC
#' @name prepTC
#' @importFrom igraph graph.edgelist clusters V graph
#' @importFrom raster clump freq
#' @author Darius Görgen, Dr. Fabian Löw (MapTailor Geospatial Consulting GbR) \email{info@maptailor.net}
#' \cr
#' \emph{Maintainer:} KfW FZ Evaluierung \email{FZ-Evaluierung@kfw.de}
#' \cr
#' \emph{Contact Person:} Dr. Johannes Schielein
#' \cr
#' \emph{Copyright:} KfW Bankengruppe
#' \cr
#' \emph{License:} GPL-3
#'
#' @examples
#' library(raster)
#' library(mapme.forest)
#'
#' treeCover = raster(system.file("extdata", "pkgTest_treecover2000.tif",
#'                                package = "mapme.forest"))
#' binaryCover = prepTC(inputForestMap = treeCover,
#'                              thresholdCover = 75,
#'                              thresholdClump = 25)
#' binaryCover
#'
prepTC <- function(inputForestMap,
                   thresholdCover=NULL,
                   thresholdClump=NULL){

  layer_names = names(inputForestMap)
  # apply threshold cover
  if (!is.null(thresholdCover)){
    # create classification matrix to apply thresholdCover value
    f = function(x) ifelse(x>=thresholdCover, 1, 0)
    inputForestMap = calc(inputForestMap, f)
    # filename = paste0(filename, "binary.tif"), datatype = "INT1U"
  }

  # apply threshold clump
  if (!is.null(thresholdClump)){
    # check  if only 0s and 1s are present
    unique_vals = unique(inputForestMap)
    if(unique_vals[1] != 0 | unique_vals[2] != 1 | length(unique_vals) != 2){
      stop("Cannot apply clump removal to raster: Other values than 0 and 1 are supplied!")
    }
    clummy = clump(inputForestMap, directions = 8)
    clumpTmp = data.frame(freq(clummy))
    clumpTmp = clumpTmp[which(clumpTmp$count < thresholdClump), ]
    clumpTmp = as.vector(clumpTmp$value)
    clummy[clummy %in% clumpTmp] = 0
    clummy[clummy != 0] = 1
    clummy[is.na(clummy)] = 0
    inputForestMap = clummy
  }
  names(inputForestMap) = layer_names
  return(inputForestMap)
}


#' Get yearly binary tree cover maps
#'
#' This function calculates yearly tree cover maps based on a starting year layer
#' in a binary forest mask form and an loss year layer which indicates which pixels
#' were subject to forest loss in a given year,
#'
#' @param inputForestMap A \code{RasterLayer} with a binary mask of forest cover
#'   (with 0 representing no forest; 1 representing forest) for the starting year.
#' @param inputLossMap A \code{RasterLayer} in which the cell values
#'  represent the year in which forest loss occured during the time series
#'  represented in the \code{years} object.
#' @param years A \code{numeric} vector indicating the years represented in the
#'   values of the \code{inputLossMap} object. 0 is expected to indicate no loss
#'   at all, a value of 1 corresponds to the first value in \code{years}, a value
#'   of 2 to the second, and so on.
#'
#' @return A binary \code{RasterStack} with a number of layers equal to \code{length(years)}.
#'  A value of 0 represents no forest cover, a value of 1 represents forest cover.
#'  All cell values indicating forest loss in the \code{inputLossMap} object at a given year
#'  will be consectuivley set to 0. The result is a yearly binary classification of forest cover.
#' @author Darius Görgen, Dr. Fabian Löw (MapTailor Geospatial Consulting GbR) \email{info@maptailor.net}
#' \cr
#' \emph{Maintainer:} KfW FZ Evaluierung \email{FZ-Evaluierung@kfw.de}
#' \cr
#' \emph{Contact Person:} Dr. Johannes Schielein
#' \cr
#' \emph{Copyright:} KfW Bankengruppe
#' \cr
#' \emph{License:} GPL-3
#'
#' @importFrom raster values stack reclassify raster addLayer nlayers
#' @export getTM
#' @name getTM
#' @examples
#' library(raster)
#' library(mapme.forest)
#'
#' binaryCover = raster(system.file("extdata", "pkgTest_binaryCover.tif",
#'                                  package = "mapme.forest"))
#' lossYear = raster(system.file("extdata", "pkgTest_lossyear.tif",
#'                               package = "mapme.forest"))
#' yearlyMaps = getTM(inputForestMap = binaryCover,
#'                    inputLossMap = lossYear,
#'                    years = 2001:2018)
#' yearlyMaps
#'
getTM <- function(inputForestMap,
                  inputLossMap,
                  years = 2000:2018){


  # check if first supplied year is not smaller than 2000
  if(sort(years)[1] - 2000 < 0){
    stop("Starting year specified is smaller 2000. That is not supported with GFW data.")
  }

  unis = sort(unique(values(inputLossMap)))

  if (length(unis) == 1 & unis[1] == 0){ # if no forest change occurred, replicate base layer
    message("No forest loss in inputLossMap. Replicating inputForestMap.")
    binaryList = stack(lapply(1:length(years), function(i) inputForestMap))
    names(binaryList) = paste0("y",years)

  } else { # when forest loss occurred, calculate binary layers

    tmp = lapply(years,
                 function(i){
                   i = i - 2000
                   # create classification matrix for current year value i
                   # 1 represents loss so far, 0 represents not loss
                   tmpLS = inputLossMap < i
                   tmpTC = inputForestMap
                   # set all losses to 0
                   tmpTC[tmpLS == 1] = 0
                   return(tmpTC)
                 })

    # stack the results and get present and missing years
    binaryList = stack(tmp)
    names(binaryList) = paste0("y",years)
    rm(tmp)
  }
  # gc
  rm(inputLossMap, inputForestMap)
  gc()
  return(binaryList)
}

