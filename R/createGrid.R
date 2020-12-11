#' Creates a grid of squared polygons that fully cover the spatial extent of the study area. Study area is defined by the
#' Raster Layers (e.g. the Hansen data sets) thar are used as inout to calculate the forest fragmentation statistics.
#'
#' @param rasters A raster stack with the forest map rasters.
#' @param spacing A numeric vector defining the spacing of grids in meters, e.g. "500" will create grids of size
#' 500m * 500m. "500" will be the default
#'
#' @export createGrid
#' @name createGrid
#' @keywords internal
#' @return A raster stack with the two layers per class in the 'classValue' object.
#' @importFrom raster raster extent res rasterToPolygons
#' @importFrom sp proj4string
createGrid <- function(rasters = NULL, spacing = 500){

  # some error functions
  if (is.null(rasters))stop("Specify a Raster")

  if (is.null(spacing)){
    message("No spacing defined, will set 500 Meters as default")
    spacing = 500
  }
  grid <- raster(extent(rasters[[1]]))
  res(grid) <- spacing
  proj4string(grid)<-proj4string(rasters[[1]])
  gridpolygon <- rasterToPolygons(grid)
  return(gridpolygon)
}
