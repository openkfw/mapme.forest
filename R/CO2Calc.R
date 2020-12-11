#' CO2 Emissions by Forest Loss
#'
#' Calculation of the CO2 emissions resulting from forest loss in a time series.
#' It uses information on the occurence of forest loss and the amount of CO2
#' present in the biomass to calculate the sum of CO2 emissions on a yearly basis.
#'
#' @param inputCO2Map A \code{RasterLayer} object with values indicating the
#'   the amount of CO2 equivalent present in the biomass of a given pixel.
#'   The annual sum of CO2 emissions per feature in the \code{studysite} object
#'   is calculated based on this data set.
#' @param inputLossMap A \code{RasterLayer} object with values indicating the
#'   year tree cover was lost. The annual area of tree cover lost is calculated
#'   based on the values found in this data set.
#'
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
#' @examples
#' library(sf)
#' library(raster)
#' library(mapme.forest)
#'
#' aoi = st_read(system.file("extdata", "aoi_polys.gpkg", package = "mapme.forest"))
#' yearlyRaster = stack(system.file("extdata", "pkgTest_yearlyCover.tif",
#'                                  package = "mapme.forest"))
#' lossRaster = raster(system.file("extdata", "pkgTest_lossyear.tif",
#'                                 package = "mapme.forest"))
#' co2Raster = raster(system.file("extdata", "pkgTest_co2_emission.tif",
#'                                package = "mapme.forest"))
#'
#' result = CO2Calc(inputForestMap = yearlyRaster,
#'                  inputLossMap = lossRaster,
#'                  inputCO2Map = co2Raster,
#'                  studysite = aoi[1,],
#'                  polyName = "id",
#'                  ncores = 2,
#'                  saveCSV = FALSE,
#'                  years = 2000:2018)
#' str(result)
#'
#' @export CO2Calc
#' @name CO2Calc
#' @inheritParams AreaCalc
#' @import raster
#' @importFrom exactextractr exact_extract
#' @import sf
#' @importFrom tibble rownames_to_column
#' @importFrom dplyr left_join
#' @importFrom parallel mclapply
#' @importFrom utils write.csv
CO2Calc <- function (inputForestMap=NULL,
                     inputLossMap=NULL,
                     inputCO2Map=NULL,
                     studysite=NULL,
                     ncores=1,
                     polyName=NULL,
                     saveCSV=FALSE,
                     years=2001:2018) {


  #--------------------------- CHECK FOR ERROS IN INPUT -----------------------#


  if (!class(inputForestMap)[1] %in% c("RasterLayer", "RasterStack", "RasterBrick")){
    stop(paste0("No valid raster object specified in 'inputForestMap'.\n",
                "Must be of class 'RasterLayer','RasterStack'or 'RasterBrick').\n",
                "See ?CO2Calc for details."))
  }

  if(nlayers(inputForestMap) != length(years)){
    stop(paste0("Number of layers in inputForestMap and length of years differ. \n",
                "Make sure that for each year you specify a layer is present in inputForestMap."))
  }

  if (!class(inputLossMap)[1] == "RasterLayer"){
    stop(paste0("inputLossMap is not a single RasterLayer.\n", "It must be of class 'RasterLayer').\n","See ?CO2Calc for details."))
  }

  if (!class(inputCO2Map)[1] == "RasterLayer"){
    stop(paste0("inputCO2Map is not a single RasterLayer.\n", "It must be of class 'RasterLayer').\n","See ?CO2Calc for details."))
  }

  if (class(studysite)[1] != "sf"){
    stop(paste0("No valid spatial object specified in 'studysite'.\n",
                "Must be of class 'sf'.",
                "See ?CO2Calc for details."))
  }

  if (nrow(studysite) == 0){
    stop(paste0("A spatial object with 0 features was specified.\n",
                "At least one feature needs to be present.\n",
                "See ?CO2Calc for details."))
  }

  if (!polyName %in% names(studysite)){
    stop(paste0("There is no column named ", polyName," in the aoi object. Please check your \n",
                "input for polyName."))
  }

  if (length(studysite[[polyName]]) != length(unique(studysite[[polyName]]))){
    stop(paste0("Names for the spatial features in specified column are not unique.\n",
                "Specify a column name in the 'polyName' object which has unique values for each feature.\n",
                "See ?CO2Calc for details."))
  }

  if (st_crs(studysite) != st_crs(inputForestMap)){
    stop(paste0("The CRS of the 'studysite' and 'inputForestMap' objects are not identical.\n",
                "Reproject either of these to the CRS of the other (Preferebly reproject 'studysite')."))
  }

  if (st_crs(studysite) != st_crs(inputLossMap)){
    stop(paste0("The CRS of the 'studysite' and 'inputLossMap' objects are not identical.\n", "Reproject either of these to the CRS of the other (Preferebly reproject 'studysite')."))
  }

  if (st_crs(studysite) != st_crs(inputCO2Map)){
    stop(paste0("The CRS of the 'studysite' and 'inputCO2Map' objects are not identical.\n",
                "Reproject either of these to the CRS of the other (Preferebly reproject 'studysite')."))
  }

  if(saveCSV != FALSE){
    if(!file.exists(saveCSV)){
      stop(paste0("The directory you specified does not exist.\n",
                  "Specify a valid directory for .csv output.\n",
                  "See ?CO2Calc for details."))
    }
  }

  #----------------------------- Zonal Stats -----------------------------#
  # splitting studysite object to list for parallel processing
  studysiteList = lapply(1:nrow(studysite), function(i){
    return(studysite[i, ])
  })

  unis = sort(unique(values(inputLossMap)))
  if (length(unis) > 0){
    if (ncores == 1){ # check for sequential processing
      CO2Stats = lapply(studysiteList, function(feature){
        co2_calc_seq(inputForestMap = inputForestMap,
                     inputLossMap = inputLossMap,
                     inputCO2Map = inputCO2Map,
                     studysite = feature,
                     unis = unis)
      })
    }

    if (ncores > 1){
      # calculate CO2 emissions for each single observation year
      CO2Stats = mclapply(studysiteList, function(feature){
        co2_calc_seq(inputForestMap = inputForestMap,
                     inputLossMap = inputLossMap,
                     inputCO2Map = inputCO2Map,
                     studysite = feature,
                     unis = unis)
      }, mc.cores = ncores)
    }
    #-------------------------------- PREPARING OUTPUT --------------------------#
    CO2Stats =  as.data.frame(do.call("rbind", CO2Stats))
    colnames(CO2Stats) <- paste0("co2_",unis+2000)
    # add missing years with 0
    for (i in years){
      if (!i %in% (unis + 2000) )
        CO2Stats[paste0("co2_", i)] = 0
    }

    # reorder columns to mirror years
    index = unlist(lapply(years, function(i) pmatch(paste0("co2_",i), names(CO2Stats))))
    CO2Stats = CO2Stats[ , index]
    # add unique identifier to loss stats
    CO2Stats[ ,polyName] = unlist(st_drop_geometry(studysite)[polyName])

  } else { # in case no losses occured
    CO2Stats = as.data.frame(matrix(nrow=nrow(studysite), ncol=length(years)+1, data = 0))
    names(CO2Stats) = c(polyName, paste0("co2_", years, sep=""))
    CO2Stats[polyName] = st_drop_geometry(studysite)[polyName]
  }
  # Save the results if specified by user
  if (saveCSV != FALSE){
    write.csv(CO2Stats, file=paste0(saveCSV,"/C02Statistics.csv"), row.names = FALSE)
  }
  studysite = suppressMessages(left_join(studysite, CO2Stats))
  return(studysite)
}# end of function

#' CO2 emission calculation single mode (Helper Function)
#'
#' @param inputForestMap A raster object
#' @param inputLossMap A raster layer object
#' @param inputCO2Map A raster layer object
#' @param studysite A sf object
#' @param unis A numeric vector indicating yearly cell values
#'
#' @return A data.frame object with Yearly sums of co2 emissions by tree cover loss
#' @export co2_calc_seq
#' @keywords internal
#' @importFrom sf st_transform
#' @importFrom raster projection crop stack
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
co2_calc_seq <- function(inputForestMap, inputLossMap, inputCO2Map, studysite,  unis){

  studysite2 <- st_transform(studysite, projection(inputLossMap))
  # create dummy raster for current feature extent
  ratio = coverratio(inputForestMap, studysite2)

  if(ratio > 10){
    lossyear = crop(inputLossMap, studysite2)
    treecover = crop(inputForestMap, studysite2)
    emission = crop(inputCO2Map, studysite2)
  } else {
    lossyear = inputLossMap
    treecover = inputForestMap
    emission = inputCO2Map
    rm(inputForestMap); gc()
    rm(inputLossMap); gc()
    rm(inputCO2Map); gc()
  }

  # set all pixels to 0 which are no forest in treecover
  index = sum(treecover)
  lossyear[index == 0] = 0
  emission[index == 0] = 0
  rm(treecover, index); gc()

  # calculate CO2 emissions for each single observation year
  DummyAnLoss = stack(lapply(unis, function(i){
    # exclude other loss years
    index = lossyear
    index[lossyear != i] = 0
    rasterReturn = emission
    rasterReturn[index == 0] = 0

    # mask co2 raster by index
    return(rasterReturn)
  }))

  results <- exact_extract(DummyAnLoss, studysite2, "sum")
  rm(DummyAnLoss, studysite2, lossyear, emission) ; gc()
  return(results)
}

