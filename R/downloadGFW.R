#' Download GFW and CO2-Emission data
#'
#' This function downloads the necessary Global Forest Watch and CO2 Emission
#'  data for a given shapefile. The shapefile is transformed to its bounding
#'  box, projected to its central UTM Zone and a buffer is applied. The default
#'  buffer size is 5000 meteres, but it can be changes by the user. It uses
#'  gdal system calls to speed up the process. The raw files by default will be
#'  deleted, but they also can be kept on the disk. Note, the CO2 emission data
#'  is only available roughly between 30N and 30S and the function will fail if
#'  your input shapfiles extent plus buffer is extending beyond those limits.
#'  This function relies heavily on code published at https://github.com/azvoleff/gfcanalysis.
#'
#' @param shape A \code{sfObject} determining the extent for which to get the GFW data.
#' @param dataset A \code{charachter} specifiying the version of the GFW data to extract.
#'  Defaults to version 1.6 from 2018. The newer version can be downloaded when
#'  you specify \code{dataset = "GFW-2019-v1.7"}.
#' @param basename A \code{charachter} which will be added to the resulting
#'  file names.
#' @param outdir A \code{charachter} for a local directory for the final outputs.
#'  If it is not existing it will be created without a message. Defaults to the
#'  current working directory.
#' @param keepTmpFiles A \code{logical} indicating if the raw tiles should be kept in
#'  the \code{tmpdir} directory. It defaults to \code{FALSE}, which usually makes sense.
#'  However, if you apply a number of analysis for different extents but there might
#'  be overlaps, it could be useful to keep the downloaded files on your machine.
#'  The function will only download the necessary data if it is not alread present
#'  in the \code{tmpdir}.
#' @param .tmpdir A \code{charachter} indicating a directory where to download
#'  the raw tiles to. It defaults to a hidden folder called \code{.tmp} in the
#'  current working directory, but can be set by the user. If the directory does
#'  not exist on your machine it will be created without a message.
#'
#' @return A \code{vector} of type \code{charachter} with all the files matching
#'  the \code{basename} pattern in the \code{outdir} directory.
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
#' @import sf
#' @import raster
#' @import sp
#' @importFrom curl has_internet
#' @importFrom utils download.file
#' @export downloadfGFW
#' @name downloadfGFW
#'
#' @note This function depends on available gdal binaries on your system. Make sure they
#'   are available as environment variables on your machine or use our docker image instead.
#'
#' @examples
#' \dontrun{
#' aoi = st_read(system.file("extdata", "aoi_polys.gpkg", package = "mapme.forest"))
#'
#' raster_files = downloadfGFW(shape = aoi,
#'                             basename = "pkgTest",
#'                             outdir = "./data/",
#'                             keepTmpFiles = T,
#'                             .tmpdir = "./data/tmp")
#'
#' # resulting rasters are not automatically cropped to the extent of aoi
#' rasters = stack(lapply(raster_files, function(f){
#' f = brick(f)
#' f = crop(f, aoi)
#' }))
#' }
#'
downloadfGFW <- function(shape,
                         dataset = "GFC-2018-v1.6",
                         basename = "Hansen_1.6",
                         outdir = ".",
                         keepTmpFiles = F,
                         .tmpdir = "./.tmp/"){

  out = has_internet()
  if (!out){
    stop("There is no internet connection. Cannot proceed downloading required data.")
  }

  parameters = c("treecover2000", "lossyear", "co2_emission_")
  filenames = file.path(outdir, paste(basename, "_", parameters, ".tif", sep = ""))
  if(sum(file.exists(filenames)) == 3){stop("Output files exists. Please delete if you want to rerun.")}

  # this code is heavily copy & pasted from: https://github.com/azvoleff/gfcanalysis
  # co2 emission dataset is restricted roughly between 30N and 30S
  # see http://data.globalforestwatch.org/datasets/d33587b6aee248faa2f388aaac96f92c_0 for more info
  # convert input shape to bounding box
  proj = st_crs(4326)
  if (st_crs(shape)[[2]] != proj) shape = st_transform(shape, proj)
  shape = st_as_sf(st_as_sfc(st_bbox(shape)))
  # calculate utm zone of central point of bounding box
  #centroid = suppressWarnings(st_centroid(shape))
  #zone = floor((st_coordinates(centroid)[1] + 180) / 6) + 1
  #utm = paste0("+proj=utm +zone=", zone," +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
  # transform bounding box to central utm
  #shape = st_transform(shape, utm)
  # buffer bounding box (50 kilometers by default)
  #shape = st_buffer(shape, dist = buffer)
  #shape = st_transform(shape, proj)
  # make the GFC grid
  grid_GFC = makeGFWGrid(mnx=-180, mxx=170, dx=10, mny=-50, mxy=80, dy=10)

  # look for intersections
  hits = suppressMessages(st_intersects(shape, st_as_sf(grid_GFC)))
  hits = grid_GFC[hits[[1]], ]

  # create directories if non-existing
  if(!file.exists(outdir)) dir.create(outdir, showWarnings = F)
  if(!file.exists(.tmpdir)) dir.create(.tmpdir, showWarnings = F)

  # loop through intersecting tiles and download them to outdir
  file_list = list()
  co2tiles = read.csv("https://opendata.arcgis.com/datasets/d33587b6aee248faa2f388aaac96f92c_0.csv")

  for (n in 1:length(hits)){

    tile = hits[n, ]
    min_x <- bbox(tile)[1, 1]
    max_y <- bbox(tile)[2, 2]

    # prepare tile names
    if (min_x < 0) {
      min_x <- paste0(sprintf('%03i', abs(min_x)), 'W')
    } else {
      min_x <- paste0(sprintf('%03i', min_x), 'E')
    }
    if (max_y < 0) {
      max_y <- paste0(sprintf('%02i', abs(max_y)), 'S')
    } else {
      max_y <- paste0(sprintf('%02i', max_y), 'N')
    }


    # url for hansen treecover and lossYear
    baseurl = paste0("https://storage.googleapis.com/earthenginepartners-hansen/", dataset, "/")
    parameters = c("treecover2000", "lossyear")
    filenames = paste0("Hansen_", dataset, "_", parameters, "_", max_y, "_", min_x, ".tif")
    urls = paste0(baseurl, filenames)

    # read co2 spatial index
    tiles = as.character(co2tiles$tile_id)
    target = paste0(max_y, "_", min_x)
    check = target %in% tiles

    if (!check){
      #message(paste0("There is no CO2 dataset available for the extent you queried.\n",
      #               "The function will add an empty raster instead.\n"))
      filenames = c(filenames, paste0("Zarin-2016_co2_emission_", max_y, "_", min_x, ".tif"))
    } else {
      urls_co2 = paste0("http://gfw2-data.s3.amazonaws.com/climate/Hansen_emissions/2018_loss/per_pixel/", max_y, "_" , min_x, "_tCO2_pixel_AGB_masked_by_loss.tif")
      # bind together target urls
      urls = c(urls, urls_co2)
      filenames = c(filenames, paste0("Zarin-2016_co2_emission_", max_y, "_", min_x, ".tif"))
    }

    if(check){  # in cases where co2 data is available

      for (i in 1:length(filenames)){
        localname = file.path(.tmpdir, filenames[i])
        if(file.exists(localname)){
          print(paste0("File ", localname, " already exists. Skipping download."))
          next
        } else {
          download.file(urls[i], localname)
        }
      }

    } else { # create an empty dummy when co2 data is not available

      for (i in 1:2){
        localname = file.path(.tmpdir, filenames[i])
        if(file.exists(localname)){
          print(paste0("File ", localname, " already exists. Skipping download."))
          next
        } else {
          download.file(urls[i], localname)
        }
      }

      localname2 = file.path(.tmpdir, filenames[3])
      if(!file.exists(localname2)) {
        dummy <- raster(localname)
        dummy = st_sf(st_as_sfc(st_bbox(dummy)), file.path(.tmpdir, "tmp.gpkg"))
        dummy$value = 0
        st_write(dummy, file.path(.tmpdir, "tmp.gpkg"))
        file.copy(from = localname, to = localname2)
        ts = raster(localname)
        ts = paste(c(ncol(ts), nrow(ts)), collapse = " ")
        command = paste0('gdal_rasterize -a value -a_nodata 0 -co "COMPRESS=LZW" -ot Float32 -ts ',ts, ' ', file.path(.tmpdir, "tmp.gpkg"), ' ', localname2)
        system(command)
        file.remove(file.path(.tmpdir, "tmp.gpkg"))
      }
    }

    file_list[[n]] = filenames
  }

  # loop through parameters and mosaic them with gdal system calls
  outfiles = unlist(file_list)
  parameters = c("treecover2000", "lossyear", "co2_emission_")

  for (p in parameters){
    tmp = file.path(.tmpdir, outfiles[grep(p, outfiles)])
    filename = file.path(outdir, paste0(basename, "_", p, ".tif"))
    if(file.exists(filename)){
      message("Output file ", filename, " already exists. Skipping translation...")
      next
    } else {
      command = paste0("gdalbuildvrt ", file.path(.tmpdir, "vrt.vrt "), paste(tmp, collapse = " "))
      system(command)
      command = paste0("gdal_translate -ot UInt16 -co COMPRESS=LZW -co BIGTIFF=YES ", file.path(.tmpdir, "vrt.vrt "), filename)
      system(command)
    }
  }

  if (!keepTmpFiles){
    ls = c(file.path(.tmpdir, outfiles), file.path(.tmpdir, "vrt.vrt"))
    file.remove(ls)
    file.remove(.tmpdir)
  }

  out = list.files(outdir, pattern = basename, full.names = T)
  return(out)
}

#' Create the GFW tile grid
#'
#' This function is used in \code{\link{downloadfGFW}} to create a grid representing
#' the GFW tiles.
#'
#' @param mnx Minimum x corrdinate
#' @param mxx Maximum x coordinate
#' @param dx x resoltion
#' @param mny Minimum y coordinated
#' @param mxy Maximum y coordinated
#' @param dy y resolution
#' @param proj projection as EPSG code
#'
#' @return A grid.
#' @keywords internal
#' @export
#' @importFrom sf st_crs
#' @importFrom methods as
#' @importFrom sp GridTopology SpatialGrid
#' @importFrom utils read.csv
#'
makeGFWGrid <- function(mnx, mxx, dx, mny, mxy, dy,
                        proj=NULL) {
  if (is.null(proj)) proj = st_crs(4326)
  ncells = c((mxx - mnx) / dx,
             (mxy - mny) / dy)
  gt = GridTopology(c(mnx+dx/2, mny+dy/2), c(dx, dy), ncells)
  grd = SpatialGrid(gt, proj4string = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" )
  spix = as(grd, "SpatialPixels")
  spol = as(spix, "SpatialPolygons")
  return(spol)
}
