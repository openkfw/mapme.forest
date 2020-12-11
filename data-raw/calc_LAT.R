library(forestIndicators)
library(sf)
library(raster)
library(parallel)
library(gdalUtils)
rasterOptions(chunksize = 1e+9)

# setwd("../data/4_LAT/1_admin/DB/")
# creating necessary output directories
dir.create("data/tmp", recursive = T)
dir.create("raster")

# reading areas
kfw_areas = st_read("2020-07-06-kfwareas.gpkg") # assumes gpkg is found in the current directory
# kfw_areas = kfw_areas[-which(st_is_empty(kfw_areas)== T),] # check for empty geometries

# calculating area of bounding boxes
size = do.call("c", lapply(1:nrow(kfw_areas), function(r){
  x = st_as_sfc(st_bbox(st_geometry(kfw_areas[r, ])))
  st_area(x)
}))

# reorder areas by size (it could make sense to chose decreasing to TRUE)
kfw_areas = kfw_areas[order(size, decreasing = F), ]
kfw_small = kfw_areas[1:round(nrow(kfw_areas) * .75),]
kfw_big = kfw_areas[round(nrow(kfw_areas) * .75 +1):nrow(kfw_areas), ]

# download data

raster_files = downloadfGFW(kfw_areas,
                            dataset = "GFC-2019-v1.7",
                            basename = "LAT",
                            outdir = "data/",
                            .tmpdir = "data/tmp/",
                            keepTmpFiles = T)

# read in raster data
treecover = raster(raster_files[grep("treecover", raster_files)])
lossyear =  raster(raster_files[grep("lossyear", raster_files)])
co2data =  raster(raster_files[grep("co2", raster_files)])

# define thresholds and adjust if necessary
thresholdCover = 20 # in percent
thresholdClump = 6 # in number of pixels
buffersize = 0.01 # in radial degrees
idvar = "OBJECTID" # must be present in kfw_areas data.frame
rasterdir = "raster" # was created earlier
ncores = 3 # adjust if necessary

# parallel lapply to prepare the raster inputs for the area calculations
# this function saves the rasters to disk into the directory specified in rasterdir
# and reads their memory efficient representation back into R


raster_bricks_S = mclapply(1:nrow(kfw_small), function(f) {
  tmp_area = kfw_small[f, ]
  filename = file.path(rasterdir, paste0(st_drop_geometry(tmp_area[, idvar]), ".tif"))

  if(!file.exists(filename)){
    if(length(st_geometry(tmp_area)[[1]])>1){
      tmp_area = st_cast(tmp_area, "POLYGON")
    }

    tmp_raster = lapply(1:nrow(tmp_area), function(i){
      tmp_poly = tmp_area[i, ]
      tmp_treecover = crop(treecover, st_buffer(tmp_poly, buffersize))
      tmp_lossyear = crop(lossyear, st_buffer(tmp_poly, buffersize))
      tmp_co2data = crop(co2data, st_buffer(tmp_poly, buffersize))
      # prep treecover
      tmp_binary = prepTC(tmp_treecover, thresholdCover, thresholdClump)
      names(tmp_binary) = "y2000"
      # prep yearly rasters
      tmp_yearly = getTM(tmp_binary, tmp_lossyear, years = 2001:2019)
      tmp_yearly = stack(tmp_binary, tmp_yearly)
      # stack and rename the results
      tmp_raw = stack(tmp_treecover, tmp_lossyear, tmp_co2data, tmp_binary)
      names(tmp_raw) = c("treecover_raw", "lossyear", "co2data", "treecover_clean")
      tmp_complete = stack(tmp_raw, tmp_yearly)
      # save layer names in variable
      layer_names = names(tmp_complete)
      return(tmp_complete)
    })

    if(length(tmp_raster)>1){

      # create tmp files for the raster individual raster datasets
      filenames = lapply(1:length(tmp_raster), function(i) tempfile(pattern = as.character(i),
                                                                    fileext = ".tif"))
      # write them to disk
      for(i in 1:length(tmp_raster)){writeRaster(tmp_raster[[i]], filename = filenames[[i]])}
      # read in buffered original area
      tmp_area = st_buffer(kfw_areas[f, ], buffersize)
      # create an empty raster template and write it to the output directory
      e = extent(tmp_area)
      template = raster(e)
      writeRaster(template, filename, format = "GTiff")
      # use gdalUtils to mosaic the calculated rasters in the output
      mosaic_rasters(gdalfile = unlist(filenames), dst_dataset = filename, of = "GTiff", co =list("COMPRESS=LZW"), ot = "Float32")

      # garbage collection
      file.remove(unlist(filenames))
      rm(tmp_raster, filenames, e, template, tmp_area)
      gc()
    } else {
      writeRaster(tmp_raster[[1]], filename = filename)
    }
    # read in raster file
  }
  layer_names = c("treecover_raw", "lossyear", "co2data", "treecover_clean", paste("y", 2000:2019, sep = ""))
  tmp_complete = brick(filename)
  names(tmp_complete) = layer_names
  return(tmp_complete)
}, mc.cores = ncores)

kfw_big = kfw_big[-which(kfw_big$OBJECTID %in% c(241396, 240329, 240603)), ]
kfw_big = kfw_big[which(kfw_big$OBJECTID %in% c(241396, 240329, 240603)), ]

raster_bricks_B = lapply(1:nrow(kfw_big), function(f) {
  tmp_area = kfw_big[f, ]
  filename = file.path(rasterdir, paste0(st_drop_geometry(tmp_area[, idvar]), ".tif"))

  if(!file.exists(filename)){
    if(length(st_geometry(tmp_area)[[1]])>1){
      tmp_area = st_cast(tmp_area, "POLYGON")
    }

    tmp_raster = lapply(1:nrow(tmp_area), function(i){
      tmp_poly = tmp_area[i, ]
      tmp_treecover = crop(treecover, st_buffer(tmp_poly, buffersize))
      tmp_lossyear = crop(lossyear, st_buffer(tmp_poly, buffersize))
      tmp_co2data = crop(co2data, st_buffer(tmp_poly, buffersize))
      # prep treecover
      tmp_binary = prepTC(tmp_treecover, thresholdCover, thresholdClump)
      names(tmp_binary) = "y2000"
      # prep yearly rasters
      tmp_yearly = getTM(tmp_binary, tmp_lossyear, years = 2001:2019)
      tmp_yearly = stack(tmp_binary, tmp_yearly)
      # stack and rename the results
      tmp_raw = stack(tmp_treecover, tmp_lossyear, tmp_co2data, tmp_binary)
      names(tmp_raw) = c("treecover_raw", "lossyear", "co2data", "treecover_clean")
      tmp_complete = stack(tmp_raw, tmp_yearly)
      # save layer names in variable
      layer_names = names(tmp_complete)
      return(tmp_complete)
    })

    if(length(tmp_raster)>1){

      # create tmp files for the raster individual raster datasets
      filenames = lapply(1:length(tmp_raster), function(i) tempfile(pattern = as.character(i),
                                                                    fileext = ".tif"))
      # write them to disk
      for(i in 1:length(tmp_raster)){writeRaster(tmp_raster[[i]], filename = filenames[[i]])}
      # read in buffered original area
      tmp_area = st_buffer(kfw_areas[f, ], buffersize)
      # create an empty raster template and write it to the output directory
      e = extent(tmp_area)
      template = raster(e)
      writeRaster(template, filename, format = "GTiff")
      # use gdalUtils to mosaic the calculated rasters in the output
      mosaic_rasters(gdalfile = unlist(filenames), dst_dataset = filename, of = "GTiff", co =list("COMPRESS=LZW"), ot = "Float32")

      # garbage collection
      file.remove(unlist(filenames))
      rm(tmp_raster, filenames, e, template, tmp_area)
      gc()
    } else {
      writeRaster(tmp_raster[[1]], filename = filename)
    }
    # read in raster file
  }
  layer_names = c("treecover_raw", "lossyear", "co2data", "treecover_clean", paste("y", 2000:2019, sep = ""))
  tmp_complete = brick(filename)
  names(tmp_complete) = layer_names
  return(tmp_complete)
})


raster_bricks = c(raster_bricks_S, raster_bricks_B)
# starting area calculation
ncores = 6
area_stats = mclapply(1:nrow(kfw_areas), function(f){
  tmp_area = kfw_areas[f, ]
  tmp_raster = raster_bricks[[f]]
  tmp_area = AreaCalc(inputForestMap = tmp_raster[[grep("y20", names(tmp_raster))]],
                      studysite = tmp_area,
                      latlon = TRUE,
                      polyName = idvar,
                      ncores = 1,
                      saveCSV = F,
                      years = 2000:2019)
  tmp_area = LossCalc(inputForestMap = tmp_raster[[grep("y20", names(tmp_raster))]],
                      inputLossMap = tmp_raster$lossyear,
                      studysite = tmp_area,
                      latlon = TRUE,
                      polyName = idvar,
                      ncores = 1,
                      saveCSV = F,
                      years = 2000:2019)
  tmp_area = CO2Calc(inputForestMap = tmp_raster[[grep("y20", names(tmp_raster))]],
                     inputLossMap = tmp_raster$lossyear,
                     inputCO2Map = tmp_raster$co2data,
                     studysite = tmp_area,
                     ncores = 1,
                     polyName = idvar,
                     saveCSV = FALSE,
                     latlon = TRUE,
                     years = 2000:2019)
  # garbage collection
  rm(tmp_raster)
  gc()
  return(tmp_area)
}, mc.cores = ncores)


area_stats = do.call(rbind, area_stats)
st_write(area_stats, "area_stats_LAT.gpkg")
