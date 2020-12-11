#read and prep sample data

library(sf)
library(raster)
library(exactextractr)
library(dplyr)
library(tibble)
inputForestMap = raster("data/pkgTest_treecover2000.tif")
names(inputForestMap) = "y2000"
inputLossMap = raster("data/pkgTest_co2_emission_.tif")
inputCO2Map = raster("data/pkgTest_lossyear.tif")
studysite = st_read("data/aoi_polys.gpkg")
thresholdCover = 50
thresholdClump = 15
source("R/preprocess.R")
source("R/AreaCalc.R")
source("R/LossCalc.R")
source("R/CO2Calc.R")
source("R/FragStatsCalc.R")
source("R/ClassStat2.R")
source("R/utils.R")

binary = prepTC(inputForestMap,
                  thresholdCover,
                  thresholdClump)
yearly = getTM(inputForestMap = binary,
               inputLossMap = inputLossMap,
               years = 2001:2018)
yearly_series = stack(binary, yearly)
plot(yearly_series$y2000)
plot(studysite, add = T)

yearly_1 = crop(yearly_series, studysite[1,])
studysite = studysite[1,]

# test with latlon
forest_area = AreaCalc(inputForestMap = yearly_series,
                       studysite = studysite,
                       latlon = T,
                       polyName = "id",
                       ncores = 2,
                       years = 2000:2018)

loss_area = LossCalc(inputForestMap = yearly_series,
                     inputLossMap = inputLossMap,
                       studysite = studysite,
                       latlon = T,
                       polyName = "id",
                       ncores = 2,
                       years = 2000:2018)#
co2_area = CO2Calc(inputForestMap = yearly_series,
                   inputLossMap = inputLossMap,
                   inputCO2Map = inputCO2Map,
                   studysite = studysite,
                   latlon = T,
                   polyName = "id",
                   ncores = 1,
                   years = 2000:2018)

frag_stats = FragStatsCalc(inputRasterFiles = yearly_series,
                           latlon = T,
                           polyName = "id",
                           studysite = studysite,
                           FragStats = c("n.patches"),
                           ncores = 2)


# test with utm
centroid = st_centroid(st_set_crs(st_as_sf(as(extent(yearly_series), "SpatialPolygons")), st_crs(yearly_series)))
zone = floor((st_coordinates(centroid)[1] + 180) / 6) + 1
utm = paste0("+proj=utm +zone=", zone," +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
yearly_series_proj = projectRaster(yearly_series, crs = utm)
studysite_proj = st_transform(studysite, utm)
forest_area_proj = AreaCalc(inputForestMap = yearly_series_proj,
                       studysite = studysite_proj,
                       latlon = F,
                       polyName = "id",
                       ncores = 1,
                       years = 2000:2018,
                       gdal_sys = F)
# transform areas to ha
# unprojected unit is km2
forest_area[, 3:21] = st_drop_geometry(forest_area[, 3:21]) * 100
# projected unit is in m2
forest_area_proj[, 3:21] = st_drop_geometry(forest_area_proj[, 3:21]) / 10000

diff = st_drop_geometry(forest_area[, 3:21]) - st_drop_geometry(forest_area_proj[, 3:21])
summary(unlist(diff))
boxplot(unlist(diff))


# test with R in latlon
forest_area_R = AreaCalc(inputForestMap = yearly_series,
                       studysite = studysite,
                       latlon = T,
                       polyName = "id",
                       ncores = 1,
                       years = 2000:2018,
                       gdal_sys = F)
# test with gdal in latlon
forest_area_GDAL = AreaCalc(inputForestMap = yearly_series,
                         studysite = studysite,
                         latlon = T,
                         polyName = "id",
                         ncores = 1,
                         years = 2000:2018,
                         gdal_sys = T)


forest_area_R[, 3:21] = st_drop_geometry(forest_area_R[, 3:21]) * 100
# projected unit is in m2
forest_area_GDAL[, 3:21] = st_drop_geometry(forest_area_GDAL[, 3:21]) / 10000

diff = st_drop_geometry(forest_area_R[, 3:21]) - st_drop_geometry(forest_area_GDAL[, 3:21])
summary(unlist(diff))
boxplot(unlist(diff))



# test with R in utm

forest_area_R_proj = AreaCalc(inputForestMap = yearly_series_proj,
                         studysite = studysite_proj,
                         latlon = F,
                         polyName = "id",
                         ncores = 2,
                         years = 2000:2018,
                         gdal_sys = F)
# test with gdal in latlon
forest_area_GDAL_proj = AreaCalc(inputForestMap = yearly_series_proj,
                            studysite = studysite_proj,
                            latlon = F,
                            polyName = "id",
                            ncores = 2,
                            years = 2000:2018,
                            gdal_sys = T)


forest_area_R_proj[, 3:21] = st_drop_geometry(forest_area_R_proj[, 3:21]) / 10000
# projected unit is in m2
forest_area_GDAL_proj[, 3:21] = st_drop_geometry(forest_area_GDAL_proj[, 3:21]) / 10000

diff = st_drop_geometry(forest_area_R_proj[, 3:21]) - st_drop_geometry(forest_area_GDAL_proj[, 3:21])
summary(unlist(diff))
boxplot(unlist(diff))



