#' Calculates landscape class statistics based on the R package "SDMTools"
#'
#' ClassStat2 calculates a user defined set of landscape fragmentation indices for patch types identified
#'  in a matrix of data or in a raster of class ’asc’ (SDMTools & adehabitat packages), ’RasterLayer’ (raster package)
#'  or ’SpatialGrid-DataFrame’ (sp package). It is actually used as a helper function
#'  within the \code{\link{FragStatsCalc}} function.
#'  It is heavily based on the \code{\link[SDMTools:ClassStat]{SDMTools:ClassStat}} function.
#'
#'
#' @param inputLayer a matrix of data with patches identified as classes (unique integer values) as e.g.,
#'   a binary lanscape of a species distribution or a vegetation map. Matrix can be
#'   a raster of class ’asc’ (adehabitat package), ’RasterLayer’ (raster package) or
#'   `SpatialGridDataFrame`` (sp package).
#' @param cellsize cell size (in meters) is a single value representing the width/height of cell edges (assuming square cells).
#' @param bkgd the background value for which statistics will not be calculated.
#' @param na.rm Logical if NA values should be removed when calculating the statistics. Defaults to TRUE.
#' @param FragStats Character vector list of forest fragmentation parameetrs to be calculated.
#'
#' @return A raster stack with the two layers per class in the 'classValue' object.
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
#' @inheritParams AreaCalc
#' @import raster
#' @import sf
#' @importFrom tibble rownames_to_column
#' @importFrom dplyr bind_cols
#' @importFrom SDMTools asc.from.raster asc.from.sp as.asc PatchStat ConnCompLabel
#' @importFrom stats na.omit sd var
ClassStat2 <- function (inputLayer,
                        cellsize = 30,
                        bkgd = NA,
                        latlon = FALSE,
                        na.rm=TRUE,
                        FragStats="all") {

  # load required package
  # some error functions
  if (is.null(inputLayer)){stop("Specify a Raster")}

  if (is.null(cellsize)){stop("Cellsize was not defined, will be set to 30*30m")}

  if (length(FragStats) == 0){stop("Specify at least ONE forest fragmentation index")}

  if (!class(inputLayer) %in% c("RasterLayer", "asc", "matrix", "SpatialGridDataFrame")){
   stop("inputLayer is not of class RasterLaer, asc, matrix or SpatialGridDataFrame")
  }

  if (class(inputLayer) == "RasterLayer"){
    mat = asc.from.raster(inputLayer)
  }

  if (class(inputLayer) == "SpatialGridDataFrame"){
    mat = asc.from.sp(inputLayer)
  }

  if (is.matrix(inputLayer) == TRUE){
    mat = as.asc(inputLayer)
  }


  # get unique class values from input files
  classes = as.numeric(na.omit(unique(as.vector(mat))))
  classes = classes[order(classes)]

  # check for background value
  if (!is.na(bkgd) && length(unique(classes))!=1){
    classes = classes[-which(classes == bkgd)]
  }

  novalues = F
  if(length(classes) == 0 ){
    mat[] <- 1
    classes <- 1
    novalues = T
  }


  results = lapply(classes, function(c){
    # reclassify asc matrix to class of interes
    if(length(unique(mat))==1){
      mat2 = mat # check if only one value was present
    }else{
      mat2 = mat * 0 # recode all values 0
      mat2[which(mat == c)] = 1 # recode current class value to 1
    }

    out.patch = PatchStat(ConnCompLabel(mat2), cellsize = cellsize,
                                    latlon = latlon)

    L.cell = sum(out.patch$n.cell)
    L.area = sum(out.patch$area)
    if (0 %in% out.patch$patchID){ # this is not a very general way to get rid of backround value
      out.patch = out.patch[-which(out.patch$patchID ==0), ]
    }
    tout = list(class = c)

    # From here, check which of the landscape fragmentation indices were selected in "FragStats"
    if ("n.patches" %in% FragStats | "all" %in% FragStats){ tout$n.patches = nrow(out.patch)}
    if ("total.area" %in% FragStats| "all" %in% FragStats){ tout$total.area = sum(out.patch$area)}
    if ("prop.landscape" %in% FragStats| "all" %in% FragStats){ tout$total.area = sum(out.patch$area); tout$prop.landscape = tout$total.area/L.area}
    if ("patch.density" %in% FragStats| "all" %in% FragStats){ tout$n.patches = nrow(out.patch); tout$patch.density = tout$n.patches/L.area}
    if ("total.edge" %in% FragStats| "all" %in% FragStats){ tout$total.edge = sum(out.patch$perimeter)}
    if ("edge.density" %in% FragStats| "all" %in% FragStats){ tout$total.edge = sum(out.patch$perimeter); tout$edge.density = tout$total.edge/L.area}
    if ("mean.patch.area" %in% FragStats| "all" %in% FragStats){ tout$mean.patch.area = mean(out.patch$area)}
    if ("mean.frac.dim.index" %in% FragStats| "all" %in% FragStats){ tout$mean.frac.dim.index = mean(out.patch$frac.dim.index,na.rm = T)}
    if ("landscape.division.index" %in% FragStats| "all" %in% FragStats){ tout$landscape.division.index = 1 - sum((out.patch$n.cell/L.cell)^2)}
    if ("patch.cohesion.index" %in% FragStats| "all" %in% FragStats){ tout$patch.cohesion.index =
      ((1 -(sum(out.patch$n.edges.internal) / sum(out.patch$n.edges.internal * sqrt(out.patch$n.cell)))) *
         ((1 - 1/sqrt(L.cell))/10)) *100}
    if("landscape.shape.index" %in% FragStats| "all" %in% FragStats){tout$landscape.shape.index = shape.index(sum(out.patch$n.cell),
                                                                                        sum(out.patch$n.edges.perimeter))}
    if("largest.patch.index" %in% FragStats| "all" %in% FragStats){tout$largest.patch.index = max(out.patch$area)/L.area}
    if("sd.patch.area" %in% FragStats| "all" %in% FragStats){tout$sd.patch.area = sd(out.patch$area)}
    if("min.patch.area" %in% FragStats| "all" %in% FragStats){tout$min.patch.area = min(out.patch$area)}
    if("max.patch.area" %in% FragStats| "all" %in% FragStats){tout$max.patch.area = max(out.patch$area)}
    if("perimeter.area.frac.dim" %in% FragStats & "n.patches" %in% FragStats &
       "total.edge" %in% FragStats & "total.area" %in% FragStats | "all" %in% FragStats){tout$perimeter.area.frac.dim =
         2/(((tout$n.patches * sum(log(out.patch$perimeter) +
                                     log(out.patch$area))) -
               (tout$total.edge * tout$total.area))/(tout$n.patches * sum(log(out.patch$perimeter^2)) -
                                                       tout$total.edge^2))}
    if("mean.perim.area.ratio" %in% FragStats | "all" %in% FragStats){mean.perim.area.ratio = mean(out.patch$perim.area.ratio)}
    if("sd.perim.area.ratio" %in% FragStats | "all" %in% FragStats){tout$sd.perim.area.ratio = sd(out.patch$perim.area.ratio)}
    if("min.perim.area.ratio" %in% FragStats | "all" %in% FragStats){tout$min.perim.area.ratio = min(out.patch$perim.area.ratio)}
    if("max.perim.area.ratio" %in% FragStats | "all" %in% FragStats){tout$max.perim.area.ratio = max(out.patch$perim.area.ratio)}
    if("mean.shape.index" %in% FragStats | "all" %in% FragStats){tout$mean.shape.index = mean(out.patch$shape.index, na.rm = T)}
    if("sd.shape.index" %in% FragStats | "all" %in% FragStats){tout$sd.shape.index = sd(out.patch$shape.index, na.rm = T)}
    if("min.shape.index" %in% FragStats | "all" %in% FragStats){tout$min.shape.index = min(out.patch$shape.index, na.rm = T)}
    if("max.shape.index" %in% FragStats | "all" %in% FragStats){tout$max.shape.index = max(out.patch$shape.index, na.rm = T)}
    if("sd.frac.dim.index" %in% FragStats | "all" %in% FragStats){tout$sd.frac.dim.index = sd(out.patch$frac.dim.index, na.rm = T)}
    if("min.frac.dim.index" %in% FragStats | "all" %in% FragStats){tout$min.frac.dim.index = min(out.patch$frac.dim.index,na.rm = T)}
    if("max.frac.dim.index" %in% FragStats | "all" %in% FragStats){tout$max.frac.dim.index = max(out.patch$frac.dim.index, na.rm = T)}
    if("total.core.area" %in% FragStats | "all" %in% FragStats){tout$total.core.area = sum(out.patch$core.area)}
    if("prop.landscape.core" %in% FragStats | "all" %in% FragStats){tout$prop.landscape.core = tout$total.core.area/L.area}
    if("mean.patch.core.area" %in% FragStats | "all" %in% FragStats){tout$mean.patch.core.area = mean(out.patch$core.area)}
    if("sd.patch.core.area" %in% FragStats | "all" %in% FragStats){tout$sd.patch.core.area = sd(out.patch$core.area)}
    if("min.patch.core.area" %in% FragStats | "all" %in% FragStats){tout$min.patch.core.area = min(out.patch$core.area)}
    if("max.patch.core.area" %in% FragStats | "all" %in% FragStats){tout$max.patch.core.area = max(out.patch$core.area)}
    if("prop.like.adjacencies" %in% FragStats | "all" %in% FragStats){tout$prop.like.adjacencies = sum(out.patch$n.edges.internal)/sum(out.patch$n.edges.internal + out.patch$n.edges.perimeter * 2)}
    if("aggregation.index" %in% FragStats | "all" %in% FragStats){tout$aggregation.index = aggregation.index(sum(out.patch$n.cell),sum(out.patch$n.edges.internal)/2)}
    if("splitting.index" %in% FragStats | "all" %in% FragStats){tout$splitting.index = L.area^2/sum(out.patch$area^2)}
    if("effective.mesh.size" %in% FragStats | "all" %in% FragStats){tout$effective.mesh.size = sum(out.patch$area^2)/L.area}

    out = as.data.frame(tout)
  })

  results = do.call("rbind", results)
  if(novalues) results[1,] = NA
  return((results))
} # end of function 'ClassStat2.R'



#' Aggregation index
#'
#' Helper function for the calculation of the aggregation index in \code{\link{ClassStat2}}.
#'
#' @param a A
#' @param g G
#'
#' @return Aggregation index in percent.
#' @keywords internal
#' @export
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
aggregation.index = function(a, g) {
  n = trunc(sqrt(a))
  m = a - n^2
  if (m == 0)
    maxg = 2 * n * (n - 1)
  if (m <= n)
    maxg = 2 * n * (n - 1) + 2 * m - 1
  if (m > n)
    maxg = 2 * n * (n - 1) + 2 * m - 2
  minp = rep(0, length(m))
  for (ii in 1:length(m)) {
    if (m[ii] == 0)
      minp[ii] = 4 * n[ii]
    if (n[ii]^2 < a[ii] & a[ii] <= n[ii] * (1 + n[ii]))
      minp[ii] = 4 * n[ii] + 2
    if (a[ii] > n[ii] * (1 + n[ii]))
      minp[ii] = 4 * n[ii] + 4
  }
  return((g/maxg) * 100)
} # end of aggregation.index

#' Shape index
#'
#' Helper function for the calculation of the shape in \code{\link{ClassStat2}}.
#'
#' @param a A
#' @param p P
#'
#' @return Shape index value
#' @keywords internal
#' @export
#' @author Darius Görgen, Dr. Fabian Löw (MapTailor Geospatial Consulting GbR) \email{info@maptailor.net}
#' \cr
#' \emph{Maintainer:} KfW FZ Evaluierung \email{FZ-Evaluierung@kfw.de}
#' \cr
#' \emph{Contact Person:} Dr. Johannes Schielein
#' \cr
#' \emph{Copyright:} KfW Bankengruppe
#' \cr
#' \emph{License:} GPL-3
shape.index = function(a, p) {
  n = trunc(sqrt(a))
  m = a - n^2
  minp = rep(0, length(m))
  for (ii in 1:length(m)) {
    if (m[ii] == 0)
      minp[ii] = 4 * n[ii]
    if (n[ii]^2 < a[ii] & a[ii] <= n[ii] * (1 + n[ii]))
      minp[ii] = 4 * n[ii] + 2
    if (a[ii] > n[ii] * (1 + n[ii]))
      minp[ii] = 4 * n[ii] + 4
  }
  return(p/minp)
} # end of shape.index

