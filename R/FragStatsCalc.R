#' Calculates a user defined set of landscape fragmentation indices for patch
#' types identified in a \code{raster}*Object.
#'
#' @param inputRasterFiles A \code{raster}-Object. Fragmentation statistics area
#'   calculated seperatly for every layer.
#' @param FragStats Character vector of forest fragmentation parameters to be
#' calculated. Currently available statistics are:
#' \itemize{
#' \item \strong{n.patches} the number of patches of a particular patch type or in a
#' class
#' \item \strong{total.area} the sum of the areas (m2) of all patches of the
#' corresponding patch type
#' \item \strong{prop.landscape} the proportion of the total lanscape represented by
#' this class
#' \item \strong{patch.density} the numbers of patches of the corresponding patch type
#' divided by total landscape area
#' \item \strong{total.edge} the total edge length of a particular patch type
#' \item \strong{edge.density} edge length on a per unit area basis that facilitates
#' comparison among landscapes of varying size
#' \item \strong{mean.patch.area} average area of patches
#' \item \strong{mean.frac.dim.index} mean of fractal dimension index
#' \item \strong{lanscape.division.index} based on the cumulative patch area distribution
#' and is interpreted as the probability that two randomly chosen pixels in the
#' landscape are not situated in the same patch
#' \item \strong{patch.cohesion.index} measures the physical connectedness of the
#' corresponding patch type
#' \item \strong{landscape.shape.index} a standardized measure of total edge or edge
#' density that adjusts for the size of the landscape
#' \item \strong{largest.patch.index} largest patch index quantifies the percentage of
#' total landscape area comprised by the largest patch
#' \item \strong{sd.patch.area}standard deviation of patch areas
#' \item \strong{min.patch.area} the minimum patch area of the total patch areas
#' \item \strong{max.patch.area} the maximum patch area of the total patch areas
#' \item \strong{perimeter.area.frac.dim} perimeter-area fractal dimension equals 2
#' divided by the slope of regression line obtained by regressing the
#' logarithm of patch area (m2) against the logarithm of patch perimeter (m)
#' \item \strong{mean.perim.area.ratio} the mean of the ratio patch perimeter. The
#' perimeter-area ratio is equal to the ratio of the patch perimeter (m) to
#' area (m2)
#' \item \strong{sd.perim.area.ratio} standard deviation of the ratio patch perimeter
#' \item \strong{max.perim.area.ratio} maximum perimeter area ratio
#' \item \strong{mean.shape.index} mean of shape index
#' \item \strong{sd.shape.index} standard deviation of shape index
#' \item \strong{min.shape.index} the minimum shape index
#' \item \strong{max.shape.index} the maximum shape index
#' \item \strong{sd.frac.dim.index} standard deviation of fractal dimension index
#' \item \strong{min.frac.dim.index} the minimum fractal dimension index
#' \item \strong{max.frac.dim.index} the maximum fractal dimension index
#' \item \strong{total.core.area}the sum of the core areas of the patches (m2)
#' \item \strong{prop.landscape.core} proportional landscape core
#' \item \strong{mean.patch.core.area} mean patch core area
#' \item \strong{sd.patch.core.area} standard deviation of patch core area
#' \item \strong{min.patch.core.area} the minimum patch core area
#' \item \strong{max.patch.core.area} the maximum patch core area
#' \item \strong{prop.like.adjacencies} calculated from the adjacency matrix, which
#' shows the frequency with which different pairs of patch types (including
#' like adjacencies between the same patch type) appear side-by-side on the
#' map (measures the degree of aggregation of patch types)
#' \item \strong{aggregation.index} computed simply as an area-weighted mean class
#' aggregation index, where each class is weighted by its proportional area
#' in the landscape
#' \item \strong{izesplitting.index} based on the cumulative patch area distribution and
#' is interpreted as the effective mesh number, or number of patches with a
#' constant patch size when the landscape is subdivided into S patches, where
#' S is the value of the splitting index
#' \item \strong{effective.mesh.size} equals 1 divided by the total landscape area
#' (m2) multiplied by the sum of patch area (m2) squared, summed across all
#' patches in the landscape
#' }
#'
#' @return An \code{sf}-object with it's dataframe ammended by the specified
#'   fragmentation statistics with one column for every layer
#'   in \code{inputRasterFiles} representing observation years. Additionally trend
#'   statistics and the difference between the first and last observation
#'   are automatically calculated for every single parameter based on
#'   \code{\link[trend:sens.slope]{trend:sens.slope}}.
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
#' @import raster
#' @import sf
#' @importFrom trend sens.slope
#' @importFrom tidyr gather unite spread as_tibble
#' @importFrom utils write.csv
#' @export FragStatsCalc
#' @name FragStatsCalc
#' @inheritParams AreaCalc
#' @examples
#' library(sf)
#' library(raster)
#' library(mapme.forest)
#'
#' aoi = st_read(system.file("extdata", "aoi_polys.gpkg", package = "mapme.forest"))
#' yearlyRaster = stack(system.file("extdata", "pkgTest_yearlyCover.tif",
#'                                  package = "mapme.forest"))
#' names(yearlyRaster) = paste("y_", as.character(2000:2018))
#'
#' result = FragStatsCalc(inputRasterFiles = yearlyRaster,
#'                        studysite = aoi[1,],
#'                        FragStats = c("n.patches", "total.area",
#'                                      "prop.landscape", "patch.density"),
#'                        latlon = TRUE,
#'                        polyName = "id",
#'                        ncores = 2,
#'                        saveCSV = FALSE)
#' str(result)
#'
FragStatsCalc <- function (inputRasterFiles=NULL,
                           latlon=NULL,
                           polyName=NULL,
                           studysite=NULL,
                           FragStats="all",
                           ncores=1,
                           saveCSV=FALSE) {

  # some error checks

  if (!class(inputRasterFiles)[1] %in% c("RasterLayer", "RasterStack", "RasterBrick")){
    stop(paste0("No valid raster object specified in 'inputForestMap'.\n",
                    "Must be of class 'RasterLayer','RasterStack'or 'RasterBrick').\n",
                    "See ?FragStatsCalc for details."))
  }

  if (class(studysite)[1] != "sf"){
    stop(paste0("No valid spatial object specified in 'studysite'.\n",
                    "Must be of class 'sf'.",
                    "See ?FragStatsCalc for details."))
  }

  if (nrow(studysite) == 0){
    stop(paste0("A spatial object with 0 features was specified.\n",
                    "At least one feature needs to be present.\n",
                    "See ?FragStatsCalc for details."))
  }

  if (!polyName %in% names(studysite)){
    stop(paste0("There is no column named ", polyName," in the aoi object. Please check your \n",
                    "input for polyName."))
  }

  if (length(studysite[[polyName]]) != length(unique(studysite[[polyName]]))){
    stop(paste0("Names for the spatial features in specified column are not unique.\n",
                    "Specify a column name in the 'polyName' object which has unique values for each feature.\n",
                    "See ?FragStatsCalc for details."))
  }

  if (!latlon %in% c(TRUE, FALSE)){
    stop(paste0("No valid input for 'latlon' found.\n",
                    "Choose 'TRUE' when 'inputForestMap' are in geographic coordinates, 'FALSE' otherwise.\n",
                    "See ?FragStatsCalc for details."))
  }

  if (st_crs(studysite) != st_crs(inputRasterFiles)){
    stop(paste0("The CRS of the 'studysite' and 'inputForestMap' objects are not identical.\n",
                    "Reproject either of these to the CRS of the other (Preferebly reproject 'studysite')."))
  }

  if(saveCSV != FALSE){
    if(!file.exists(saveCSV)){
      stop(paste0("The directory you specified does not exist.\n",
                      "Specify a exisiting directory for .csv output.\n",
                      "See ?FragStatsCalc for details."))
    }
  }

  if("perimeter.area.frac.dim" %in% FragStats & sum(!c("n.patches", "total.area") %in% FragStats) != 0){
    message(paste0("perimeter.area.frac.dim cannot be calculated neither without n.patches nor total.area.\n",
               "Adding missing parameters to FragStats and proceeding."))
    if(! "n.patches" %in% FragStats){FragStats = c(FragStats, "n.patches")}
    if(! "total.area" %in% FragStats){FragStats = c(FragStats, "total.area")}
  }


  # splitting polygons to list
  studysiteList = lapply(1:nrow(studysite), function(i){
    return(studysite[i, ])
  })

  if (ncores == 1){ # check for sequential processing

    FragStatisticsAll = lapply(studysiteList, function(feature){
      calcFragStats_seq(studysite = feature,
                        inputRasterFiles = inputRasterFiles,
                        FragStats = FragStats,
                        latlon = latlon)
    })

  } else {
    FragStatisticsAll = mclapply(studysiteList, function(feature){
      calcFragStats_seq(studysite = feature,
                        inputRasterFiles = inputRasterFiles,
                        FragStats = FragStats,
                        latlon = latlon)
    }, mc.cores = ncores)
  }
  for (i in 1:length(FragStatisticsAll)){
    FragStatisticsAll[[i]][ ,polyName] = unlist(rep(st_drop_geometry(studysite)[i, polyName], length(FragStats)))
  }
  # transform the structure of the results, suitable for storing as *.csv
  var <- layer <- value <- NULL # initiate varbiable names to prevent R CMD check from throwing notes
  FragStatisticsAll = do.call("rbind", FragStatisticsAll)
  statsAll = FragStatisticsAll %>%
    as_tibble() %>%
    gather("layer", "value", -var, -polyName) %>%
    unite(col="colnames", var, layer, sep="_") %>%
    spread(colnames, value)

  # Save the results if specified by user
  if (saveCSV != FALSE){
    write.csv(statsAll, file=paste0(saveCSV,"/FragStatistics.csv"), row.names = FALSE)
  }

  studysite = merge(studysite, statsAll)
  return(studysite)

}# end of function


#' Helper function to calculate Fragstats for a single polygon in sequential mode
#'
#' @param studysite A one-row sf-object
#' @param inputRasterFiles A Raster*-Object.
#' @param FragStats A charachter vector.
#'
#' @export calcFragStats_seq
#' @importFrom sf st_transform
#' @importFrom raster projection crop
#' @importFrom trend sens.slope
#' @keywords internal
#' @author Darius Görgen, Dr. Fabian Löw (MapTailor Geospatial Consulting GbR) \email{info@maptailor.net}
#' \cr
#' \emph{Maintainer:} KfW FZ Evaluierung \email{FZ-Evaluierung@kfw.de}
#' \cr
#' \emph{Contact Person:} Dr. Johannes Schielein
#' \cr
#' \emph{Copyright:} KfW Bankengruppe
#' \cr
#' \emph{License:} GPL-3
calcFragStats_seq <- function(studysite,
                              inputRasterFiles,
                              FragStats,
                              latlon){

  # reproject polygon to raster CRS
  studysite2 = st_transform(studysite, projection(inputRasterFiles))
  FragStatistics = data.frame() # initiate results data frame

  for (layer in 1:nlayers(inputRasterFiles)){ # loop through layers in rasterFiles
    ratio = coverratio(inputRasterFiles, studysite2)
    if(ratio >10 ){
      dummy = crop(inputRasterFiles[[layer]], studysite2)
    } else {
      dummy = inputRasterFiles[[layer]]
    }
    stats <- ClassStat2(inputLayer=dummy,
                        bkgd=0,
                        FragStats=FragStats,
                        cellsize = res(dummy)[1],
                        latlon = latlon)

    stats$class = NULL
    # names(stats) = FragStats
    FragStatistics <- rbind(FragStatistics, stats) # bind layer results together
  } # end of loop through layers

  # Assess trends and total differences
  if(nrow(FragStatistics)>1){
    trendSlopes <- apply(FragStatistics, FUN=funTrendsDir, MARGIN=2)
    trendSigs <-   apply(FragStatistics, FUN=funTrendsSig, MARGIN=2)
    difference <- FragStatistics[nrow(FragStatistics),] - FragStatistics[1,]

    FragStatistics2 <- as.data.frame(t(rbind(FragStatistics, Slope=trendSlopes, Sig=trendSigs, Dif=difference)))
    colnames(FragStatistics2) <- c(names(inputRasterFiles),"Slope","Sig","Dif")
  } else{
    FragStatistics2 = as.data.frame(t(FragStatistics))
  }
  FragStatistics2$var = row.names(FragStatistics2)
  return(FragStatistics2)
}
