#' Area of forest change
#'
#' Calculates the changes in forest area based on a \code{rasterObject} forest
#'   mask and a \code{SpatialPolygonsDataFrame} containing areas of interest.
#'
#' @param inputForestMap A \code{RasterLayer} object with forest cover represented
#'  in binary (e.g 0 represents no forest; 1 represents forest). Each layer is
#'  expected to represent one year in the \code{years} object in a consecutive order.
#' @param studysite An object of type \code{sf} with a given
#'   number of polygons defining the areas of interest. Forest area statistics
#'   will be calculated for features in the \code{studysite} object.
#' @param ncores The number of cores to use, i.e. at most how many child
#'   processes will be run simultaneously.
#' @param latlon \code{logical}: Indicates whether or not the
#'   \code{inputForestMap} object is based on a geographic coordinate system
#'   or is projected to a planar coordinate system. In the former case,
#'   area is approximated by \code{\link[raster:area]{raster::area}}.
#' @param polyName \code{charachter} of length 1. Indicates the column in the
#'   data frame of the \code{studysite} object to uniquely identify the features
#'   of interest. The function will fail if there is no unique identification of
#'   the polygons
#' @param saveCSV Default is \code{FALSE}. You can specify a directory on your
#'   local machine where the results area saved in \code{.csv} format. The
#'   features will be identified by the column specified in \code{polyName}.
#' @param years A vector of type \code{numeric} indicating the years which
#'  are represented by pixels. For GFW data and the default these are 2001 to 2018.
#'
#' @return The \code{studysite} object with its data frame appended columnwise
#'  for every single entry in the \code{years} object representing the total area
#'  of forest in a given layer of the \code{inputForestMap} object.
#'  If \code{latlon=TRUE} the returned area is in \emph{km²}, otherwise in the
#'  squared unit of the input projection (most commonly in meters).
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
#' @export AreaCalc
#' @import raster
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
#'                                  package = "mapme.forest"))
#' result = AreaCalc(inputForestMap = yearlyRaster,
#'                   studysite = aoi[1,],
#'                   latlon = TRUE,
#'                   polyName = "id",
#'                   ncores = 1,
#'                   saveCSV = FALSE,
#'                   years = 2000:2018)
#' str(result)
#'
AreaCalc <- function (inputForestMap=NULL,
                      studysite=NULL,
                      latlon=NULL,
                      polyName=NULL,
                      ncores=1,
                      saveCSV=FALSE,
                      years = 2001:2018) {


  #--------------------------- CHECK FOR ERROS IN INPUT -----------------------#


  if (!class(inputForestMap)[1] %in% c("RasterLayer", "RasterStack", "RasterBrick")){
    stop(paste0("No valid raster object specified in 'inputForestMap'.\n","Must be of class 'RasterLayer','RasterStack'or 'RasterBrick').\n","See ?AreaCalc for details."))
  }

  if (nlayers(inputForestMap) == 0){
    stop(paste0("A raster object with 0 layers was specified.\n","At least one layer with a forest mask needs to be present.\n","See ?AreaCalc for details."))
  }

  if(nlayers(inputForestMap) != length(years)){
    stop(paste0("Number of layers in inputForestMap and length of years differ. \n","Make sure that for each year you specify a layer is present in inputForestMap."))
  }

  if (class(studysite)[1] != "sf"){
    stop(paste0("No valid spatial object specified in 'studysite'.\n", "Must be of class 'sf'.", "See ?AreaCalc for details."))
  }

  if (nrow(studysite) == 0){
    stop(paste0("A spatial object with 0 features was specified.\n", "At least one feature needs to be present.\n", "See ?AreaCalc for details."))
  }

  if (!polyName %in% names(studysite)){
    stop(paste0("There is no column named ", polyName," in the aoi object. Please check your \n", "input for polyName."))
  }

  if (length(studysite[[polyName]]) != length(unique(studysite[[polyName]]))){
    stop(paste0("Names for the spatial features in specified column are not unique.\n", "Specify a column name in the 'polyName' object which has unique values for each feature.\n", "See ?AreaCalc for details."))
  }

  if (!latlon %in% c(TRUE, FALSE)){
    stop(paste0("No valid input for 'latlon' found.\n", "Choose 'TRUE' when 'inputForestMap' are in geographic coordinates, 'FALSE' otherwise.\n", "See ?AreaCalc for details."))
  }

  if (st_crs(studysite) != st_crs(inputForestMap)){
    stop(paste0("The CRS of the 'studysite' and 'inputForestMap' objects are not identical.\n", "Reproject either of these to the CRS of the other (Preferebly reproject 'studysite')."))
  }

  if(saveCSV != FALSE){
    if(!file.exists(saveCSV)){
      stop(paste0("The directory you specified does not exist.\n", "Specify a exisiting directory for .csv output.\n","See ?AreaCalc for details."))
    }
  }

  #----------------------------- DATA PREPARATION -----------------------------#

  # splitting studysite object to list for parallel processing
  studysiteList = lapply(1:nrow(studysite), function(i){
    return(studysite[i, ])
  })

  if (ncores == 1){ # sequential mode
    areaStats = lapply(studysiteList, function(feature){
        area_stats_seq(studysite = feature,
                                          inputForestMap = inputForestMap,
                                          latlon=latlon)
      })
  } else { # parallel mode
    areaStats = mclapply(studysiteList, function(feature){
        area_stats_seq(studysite = feature,
                                          inputForestMap = inputForestMap,
                                          latlon=latlon)
      }, mc.cores = ncores)
  }
  #-------------------------------- PREPARING OUTPUT --------------------------#

  # prepare result data
  areaStats =  as.data.frame(do.call("rbind", areaStats))
  colnames(areaStats) = paste0("area_",years)
  areaStats[,polyName] = st_drop_geometry(studysite)[ ,polyName]
  # Save the results if specified by user
  if (saveCSV != FALSE){
    write.csv(areaStats, file=paste0(saveCSV,"/AreaStatistics.csv"), row.names = FALSE)
  }
  studysite =  suppressMessages(left_join(studysite, areaStats))
  return(studysite)

}# end of function


#' Forest area calculation single mode (Helper Function)
#'
#' @param studysite An sf object
#' @param inputForestMap A raster object
#' @param latlon Logical indicating if raster is unprojected or not
#'
#' @return A dataframe with estimated areas
#' @export area_stats_seq
#' @keywords internal
#' @importFrom sf st_transform st_as_sfc st_bbox st_difference st_area
#' @importFrom raster projection crop area xres yres
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
area_stats_seq <- function(studysite, inputForestMap, latlon){

  studysite2 = st_transform(studysite, projection(inputForestMap))
  # calculate bounding boxes for raster and shapefile
  ratio = coverratio(inputForestMap, studysite2)

  if(ratio > 10){ # threshold of 10 % of the area
    treecover = crop(inputForestMap, studysite2)
  } else{
    treecover = inputForestMap
  }

  # Binary raster is multiplied by its cell resolution when 'latlon'=FALSE
  # otherwise area is estimated by raster::area()
  if(latlon){
    # approximation of area in km2, see ?raster::area for details
    rasterIn =  treecover*area(treecover)
  }else{
    # uses projected raster units as inputs
    rasterIn =  treecover*(xres(treecover)[1]*yres(treecover)[1])
  }

  #---------------------------- ZONAL STATISTICS --------------------------#
  stats <- exact_extract(rasterIn, studysite2, "sum")
  rm(rasterIn, treecover, studysite2, studysite, inputForestMap, ratio); gc()
  return(stats)
}
