#' Area of Forest Loss
#'
#' Calculation of the area where forest loss occured in a tree cover time series.
#' It uses information on the occurence of forest loss and a forest mask to focus
#' the analysis to pixles where lost occured. The area is then summed up for every
#' polygon specified in the studysite object.
#'
#'
#' @param inputLossMap A \code{RasterLayer} object with values indicating the
#'   year tree cover was lost. The annual area of tree cover lost is calculated
#'   based on the values found in this data set.
#' @return The studysite object with it's attribute table being ammended by the
#'   results of the calculation. Additionally, a csv file containing the \code{polyName}
#'   attribute as well as the results of the calculation can be saved to disk.
#'
#' @note This function relies heavily on parallization, indicating the
#'   importance of both, a high number of CPUs and large enough RAM.
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
#'
#' @export LossCalc
#' @name LossCalc
#' @inheritParams AreaCalc
#' @import raster
#' @importFrom exactextractr exact_extract
#' @import sf
#' @importFrom tibble rownames_to_column
#' @importFrom dplyr left_join
#' @importFrom parallel mclapply
#' @importFrom utils write.csv
#'
#' @examples
#' library(sf)
#' library(raster)
#' library(mapme.forest)
#'
#' aoi = st_read(system.file("extdata", "aoi_polys.gpkg", package = "mapme.forest"))
#' yearlyRaster = stack(system.file("extdata", "pkgTest_yearlyCover.tif",
#'                                   package = "mapme.forest"))
#' lossYear = raster(system.file("extdata", "pkgTest_lossyear.tif",
#'                               package = "mapme.forest"))
#'
#' result = LossCalc(inputForestMap = yearlyRaster,
#'                  inputLossMap = lossYear,
#'                  studysite = aoi[1,],
#'                  latlon = TRUE,
#'                  polyName = "id",
#'                  ncores = 1,
#'                  saveCSV = FALSE,
#'                  years = 2000:2018)
#' str(result)
#'
LossCalc <- function (inputForestMap=NULL,
                      inputLossMap=NULL,
                      studysite=NULL,
                      latlon=NULL,
                      polyName=NULL,
                      ncores=1,
                      saveCSV=FALSE,
                      years=2001:2018) {


  #--------------------------- CHECK FOR ERROS IN INPUT -----------------------#


  if (!class(inputForestMap) %in% c("RasterLayer", "RasterStack", "RasterBrick")){
    stop(paste0("No valid raster object specified in 'inputForestMap'.\n", "Must be of class 'RasterLayer','RasterStack'or 'RasterBrick').\n", "See ?LossCalc for details."))
  }

  if (!class(inputLossMap) == "RasterLayer"){
    stop(paste0("inputLossMap is not a single RasterLayer.\n", "It must be of class 'RasterLayer').\n", "See ?LossCalc for details."))
  }

  if(nlayers(inputForestMap) != length(years)){
    stop(paste0("Number of layers in inputForestMap and length of years differ. \n", "Make sure that for each year you specify a layer is present in inputForestMap.", "See ?LossCalc for details."))
  }

  if (class(studysite)[1] != "sf"){
    stop(paste0("No valid spatial object specified in 'studysite'.\n", "Must be of class 'sf'.", "See ?LossCalc for details."))
  }

  if (nrow(studysite) == 0){
    stop(paste0("A spatial object with 0 features was specified.\n", "At least one feature needs to be present.\n", "See ?LossCalc for details."))
  }

  if (!polyName %in% names(studysite)){
    stop(paste0("There is no column named ", polyName," in the aoi object. Please check your \n", "input for polyName."))
  }

  if (length(studysite[[polyName]]) != length(unique(studysite[[polyName]]))){
    stop(paste0("Names for the spatial features in specified column are not unique.\n", "Specify a column name in the 'polyName' object which has unique values for each feature.\n", "See ?LossCalc for details."))
  }

  if (!latlon %in% c(TRUE, FALSE)){
    stop(paste0("No valid input for 'latlon' found.\n","Choose 'TRUE' when 'inputForestMap' are in geographic coordinates, 'FALSE' otherwise.\n", "See ?LossCalc for details."))
  }

  if (st_crs(studysite) != st_crs(inputForestMap)){
    stop(paste0("The CRS of the 'studysite' and 'inputForestMap' objects are not identical.\n", "Reproject either of these to the CRS of the other (Preferebly reproject 'studysite')."))
  }

  if (st_crs(studysite) != st_crs(inputLossMap)){
    stop(paste0("The CRS of the 'studysite' and 'inputLossMap' objects are not identical.\n", "Reproject either of these to the CRS of the other (Preferebly reproject 'studysite')."))
  }

  if(saveCSV != FALSE){
    if(!file.exists(saveCSV)){
      stop(paste0("The directory you specified does not exist.\n", "Specify a valid directory for .csv output.\n", "See ?LossCalc for details."))
    }
  }

  #----------------------------- DATA PREPARATION -----------------------------#
  # splitting studysite object to list for parallel processing
  studysiteList = lapply(1:nrow(studysite), function(i){
    return(studysite[i, ])})

  unis = sort(unique(values(inputLossMap)))

  if (length(unis) > 0){
    if (ncores == 1){ # sequential processing
      LossStats <- lapply(studysiteList, function(feature){
        loss_calc_seq(inputForestMap = inputForestMap,
                      inputLossMap = inputLossMap,
                      studysite = feature,
                      years = years,
                      unis = unis,
                      latlon = latlon)
      })
    } else { # parallel processing
      # calculate CO2 emissions for each single observation year
      LossStats <- mclapply(studysiteList, function(feature){
        loss_calc_seq(inputForestMap = inputForestMap,
                      inputLossMap = inputLossMap,
                      studysite = feature,
                      years = years,
                      unis = unis,
                      latlon = latlon)
      }, mc.cores = ncores)
    }
    #-------------------------------- PREPARING OUTPUT --------------------------#
    # prepare result data
    LossStats =  as.data.frame(do.call("rbind", LossStats))
    colnames(LossStats) = paste0("loss_",unis+2000)
    # add missing years with 0
    for (i in years){
      if (!i %in% (unis + 2000) )
        LossStats[paste0("loss_", i)] = 0
    }
    # reorder columns to mirror years
    index = unlist(lapply(years, function(i) pmatch(paste0("loss_",i), names(LossStats))))
    LossStats = LossStats[ ,index]
    # add unique identifier to loss stats
    LossStats[ ,polyName] = unlist(st_drop_geometry(studysite)[polyName])
  }
  # when no change occurred return 0 for all years
  if (length(unis) == 0){ # check if loss occured in unis
    LossStats = as.data.frame(matrix(nrow=nrow(studysite), ncol=length(years)+1, data = 0))
    names(LossStats) = c(polyName, paste0("loss_", years, sep=""))
    LossStats[polyName] = st_drop_geometry(studysite)[polyName]

  }
  # Save the results if specified by user
  if (saveCSV != FALSE){
    write.csv(LossStats, file=paste0(saveCSV,"/LossStatistics.csv"), row.names = FALSE)
  }
  studysite = suppressMessages(left_join(studysite, LossStats))
  return(studysite)
}# end of function


#' Loss area calculation single mode (Helper Function)
#'
#' @param inputForestMap A raster stack
#' @param inputLossMap A raster layer
#' @param studysite An sf object with a single polygon
#' @param years Numeric vector indicating years
#' @param unis Numeric vector indicating yearly cell values
#' @param latlon Logical indicating if raster is projected or unprojected
#'
#' @return A data.frame with yearly values of forest cover loss.
#' @export loss_calc_seq
#' @keywords internal
#' @importFrom sf st_transform
#' @importFrom raster projection crop Which area xres yres
#' @importFrom exactextractr exact_extract
#' @author Darius Görgen, Dr. Fabian Löw (MapTailor Geospatial Consulting GbR) \email{info@maptailor.net}
#' \cr
#' \emph{Maintainer:} KfW FZ Evaluierung \email{FZ-Evaluierung@kfw.de}
#' \cr
#' \emph{Contact Person:} Dr. Johannes Schielein
#' \cr
#' \emph{Copyright:} KfW Bankengruppe
#' \cr
#' \emph{License:} GPL-3
loss_calc_seq <- function(inputForestMap, inputLossMap, studysite, years, unis, latlon){
  # project shape to raster CRS
  studysite2 <- st_transform(studysite, projection(inputLossMap))
  # calculate bounding boxes for raster and shapefile
  ratio = coverratio(inputForestMap, studysite2)
  if(ratio > 10){
    lossyear = crop(inputLossMap, studysite2)
    treecover = crop(inputForestMap, studysite2)
  } else {
    lossyear = inputLossMap
    treecover = inputForestMap
    rm(inputForestMap); gc()
    rm(inputLossMap); gc()
  }
  # set all pixels to 0 which are no forest in treecover
  index = sum(treecover)
  lossyear[index == 0] = 0
  rm(treecover, index); gc()

  # calculate loss for each single observation year
  DummyAnLoss = raster()
  for (i in unis){
    dummy = Which(lossyear == i )
    if(latlon == TRUE){
      dummy = dummy*area(dummy) # raster::area approximates surface area of cells in km2
    } else {
      dummy = dummy*(xres(dummy)*yres(dummy)) # convert to km2, assuming it is square meters
    }
    DummyAnLoss = addLayer(DummyAnLoss, dummy)
  }
  #---------------------------- ZONAL STATISTICS --------------------------#
  results <- exact_extract(DummyAnLoss, studysite2, "sum")
  rm(DummyAnLoss, studysite2,lossyear, dummy); gc()
  return(results)
}
