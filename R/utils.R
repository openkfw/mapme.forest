#' Calculate Sens Slope Trend Significance
#'
#' This function calculates the sens slope significance at the 95% confidence interval
#'  in cases where more than 75% of the values are not NA. Otherwise NA is
#'  returned.
#' @param y A numeric vector for which to calculate the sens slope.
#'
#' @return The p-value of the sens slope.
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
#' @keywords internal
#' @importFrom trend sens.slope
#' @export
funTrendsSig <- function(y){
  y <- as.numeric(y)
  idNA <- which(is.na(y)!=T)
  if(length(idNA)<0.75*length(y)){
    return(NA)
  }else{
    y <- y[!is.na(y)]
    return(suppressWarnings(as.numeric(sens.slope(y, conf.level = 0.95)$p.value)))

  }
}# end of funTrendsSig

#' Calculate Sens Slope Direction
#'
#' This function calculates the sens slope direction at the 95% confidence interval
#'  in cases where more than 75% of the values ar not NA. Otherwise NA is returned.
#'
#' @param y A numeric vector for which to calculate the sens slope.
#'
#' @return The direction of the sens slope.
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
#' @keywords internal
#' @importFrom trend sens.slope
#' @export
funTrendsDir <- function(y){
  y <- as.numeric(y)
  idNA <- which(is.na(y)!=T)
  if(length(idNA)<0.75*length(y)){
    return(NA)
  }else{
    y <- y[!is.na(y)]
    return(suppressWarnings(as.numeric(trend::sens.slope(y, conf.level = 0.95)$estimates)))

  }
}# end of funTrendsDir




#' Helper Function to calculate cover ratio
#'
#' This function is used inside of the area calculation functions to evaluate
#' the ratio a given raster is not covered by a polygon. Can be used as an indicator
#' if further cropping of the raster ca improve calculation time.
#'
#' @param r A raster object
#' @param p A sf object
#'
#' @return The cover ratio in percent
#' @keywords internal
#' @export coverratio
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
#' @importFrom sf st_as_sfc st_bbox st_difference st_area
coverratio <- function(r, p){
  r_bbox = suppressMessages(st_as_sfc(st_bbox(r)))
  p_bbox = suppressMessages(st_as_sfc(st_bbox(p)))
  rest = suppressMessages(st_difference(r_bbox, p_bbox))
  ratio = as.numeric(st_area(rest) / st_area(r_bbox) * 100)
  return(ratio)
}
