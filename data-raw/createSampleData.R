library(sf)
library(raster)
library(forestIndicators)


treecover = raster("data/pkgTest_treecover2000.tif")
treeco2 = raster("data/pkgTest_co2_emission_.tif")
treeloss = raster("data/pkgTest_lossyear.tif")
plot(treeloss)
aoi = drawExtent()
aoi = st_set_crs(st_as_sf(as(aoi, "SpatialPolygons")), st_crs(treeloss))
st_write(aoi, "data/aoi_extent.gpkg")


treecover = crop(treecover, aoi)
treeloss = crop(treeloss, aoi)
treeco2 = crop(treeco2, aoi)

plot(treeloss)
poly1 = drawPoly()
poly2 = drawPoly()
poly3 = drawPoly()
poly4 = drawPoly()
poly5 = drawPoly()
poly6 = drawPoly()
poly7 = drawPoly()
poly8 = drawPoly()

polys = list(poly1, poly2, poly3, poly4,
             poly5, poly6, poly7, poly8)

polys = lapply(polys, function(x){
  st_set_crs(st_as_sf(x), st_crs(treeloss))
})

polys = do.call(rbind, polys)
polys$id = 1:nrow(polys)
st_write(polys, "data/aoi_polys.gpkg")

writeRaster(treecover, "data/pkgTest_treecover2000.tif", overwrite = T)
writeRaster(treeco2, "data/pkgTest_co2_emission_.tif", overwrite = T)
writeRaster(treeloss, "data/pkgTest_lossyear.tif", overwrite = T)


# write yearly mask
treeCover = raster(system.file("extdata", "pkgTest_treecover2000.tif", package = "forestIndicators"))
names(treeCover) = "tree.cover"
lossYear = raster(system.file("extdata", "pkgTest_lossyear.tif", package = "forestIndicators"))
names(lossYear) = "loss.year"

treeCover_binary = prepTC(treeCover,
                          thresholdClump = 10,
                          thresholdCover = 50)
treeCover_yearly = getTM(treeCover_binary,
                         lossYear)
# applying minimal mapping unit for the yearly layers
# treeCover_yearly = stack(lapply(1:nlayers(treeCover_yearly), function(l){
#   treeCover_yearly[[l]] = prepTC(treeCover_yearly[[l]], thresholdClump = 10)
# }))
treeCover_yearly = stack(treeCover_binary, treeCover_yearly) # adding layer for 2000
names(treeCover_yearly) = paste("y", 2000:2018, sep = "")

writeRaster(treeCover_yearly, filename = "inst/extdata/pkgTest_yearlyCover.tif", overwrite = T)


# binary tree cover base map
treecover = raster::raster(system.file("extdata", "pkgTest_treecover2000.tif", package = "forestIndicators"))
treecover_processed = prepTC(inputForestMap = treecover,
                              thresholdCover = 75,
                              thresholdClump = 25)
writeRaster(treecover_processed, filename = "inst/extdata/pkgTest_binaryCover.tif")
