library(raster)
library(sf)
library(mapme.forest)
library(testthat)

studysite = st_read(system.file("extdata", "aoi_polys.gpkg", package = "mapme.forest"), quiet = T)
inputForestMap = stack(system.file("extdata", "pkgTest_yearlyCover.tif",
                                   package = "mapme.forest"))
inputLossMap = raster(system.file("extdata", "pkgTest_lossyear.tif",
                                  package = "mapme.forest"))
# create projected versions
centroid = suppressWarnings(st_centroid(st_as_sfc(st_bbox(studysite))))
zone = floor((st_coordinates(centroid)[1] + 180) / 6) + 1
utm = paste0("+proj=utm +zone=", zone," +ellps=WGS84 +datum=WGS84 +units=m +no_defs")

inputForestMap_proj = projectRaster(inputForestMap, crs = utm)
studysite_proj = st_transform(studysite, utm)
inputLossMap_proj = projectRaster(inputLossMap, crs = utm, method = "ngb")


describe("testing area calculation", {
  describe("testing of unprojected data",{
    it("Sequential mode",{
      expect_equal(
        round(
          st_drop_geometry(
            LossCalc(inputForestMap = inputForestMap[[9:10]],
                     inputLossMap = inputLossMap,
                     studysite = studysite[1,],
                     latlon = TRUE,
                     polyName = "id",
                     ncores = 1,
                     saveCSV = FALSE,
                     years = 2009:2010)
          ), 7),
        data.frame(id = 1,
                   loss_2009 = 0,
                   loss_2010 = 0.0002058))
    })
    it("parallel mode",{
      skip_on_os("windows")
      expect_equal(
        round(
          st_drop_geometry(
            LossCalc(inputForestMap = inputForestMap[[9:10]],
                     inputLossMap = inputLossMap,
                     studysite = studysite[1,],
                     latlon = TRUE,
                     polyName = "id",
                     ncores = 2,
                     saveCSV = FALSE,
                     years = 2009:2010)
          ), 7),
        data.frame(id = 1,
                   loss_2009 = 0,
                   loss_2010 = 0.0002058))

    })
  })
})

describe("testing area calculation", {
  describe("testing of projected data",{
    it("Sequential mode",{
      expect_equal(
        round(
          st_drop_geometry(
            LossCalc(inputForestMap = inputForestMap_proj[[9:10]],
                                 inputLossMap = inputLossMap_proj,
                                 studysite = studysite_proj[1,],
                                 latlon = FALSE,
                                 polyName = "id",
                                 ncores = 1,
                                 saveCSV = FALSE,
                                 years = 2009:2010)),1),
        data.frame(id = 1,
                   loss_2009 = 2638.9,
                   loss_2010 = 176.1))
    })
    it("parallel mode",{
      skip_on_os("windows")
      expect_equal(
        round(
          st_drop_geometry(
            LossCalc(inputForestMap = inputForestMap_proj[[9:10]],
                     inputLossMap = inputLossMap_proj,
                     studysite = studysite_proj[1,],
                     latlon = FALSE,
                     polyName = "id",
                     ncores = 2,
                     saveCSV = FALSE,
                     years = 2009:2010)),1),
        data.frame(id = 1,
                   loss_2009 = 2638.9,
                   loss_2010 = 176.1))

    })
  })
})

studysite$id = as.character(studysite$id)

describe("check polyName", {
  it("Charachter id column")
 expect_warning(LossCalc(inputForestMap = inputForestMap[[1:3]],
                             inputLossMap = inputLossMap,
                             studysite = studysite[1,],
                             latlon = TRUE,
                             polyName = "id",
                             ncores = 1,
                             saveCSV = FALSE,
                             years = 2000:2002), NA)
})

area_stats = LossCalc(inputForestMap = inputForestMap[[1:3]],
                      inputLossMap = inputLossMap,
                      studysite = studysite[1:4,],
                      latlon = TRUE,
                      polyName = "id",
                      ncores = 1,
                      saveCSV = FALSE,
                      years = 2000:2002)


describe("testing area stats output",{
  it("Check sf class", {
    expect_equal(class(area_stats)[1], "sf")
  })
  it("Check id class", {
    expect_equal(class(area_stats$id), "character")
  })
  it("Check row number", {
    expect_equal(nrow(area_stats),4)
  })
  it("Check col number", {
    expect_equal(ncol(area_stats),5)
  })
  it("Check geometries are equal", {
    expect_equal(st_geometry(area_stats), st_geometry(studysite[1:4,]))
  })
  it("Check ids are equal", {
    expect_equal(st_drop_geometry(area_stats)[,1], st_drop_geometry(studysite)[1:4,1])
  })
})

# check error messages
describe("Checking error messages",{
  it("Wrong format for forest map",{
    expect_error(LossCalc(inputForestMap = "noRaster",
                          inputLossMap = inputLossMap,
                          studysite = studysite[1,],
                          latlon = TRUE,
                          polyName = "id",
                          ncores = 1,
                          saveCSV = FALSE,
                          years = 2000:2002), "No valid raster object")
  })
  it("Wrong format for loss year",{ # check later
    expect_error(LossCalc(inputForestMap = inputForestMap[[1:3]],
                          inputLossMap = inputForestMap,
                          studysite = studysite[1,],
                          latlon = TRUE,
                          polyName = "id",
                          ncores = 1,
                          saveCSV = FALSE,
                          years = 2000:2002), "not a single RasterLayer")
  })
  it("Differing years",{
    expect_error(LossCalc(inputForestMap =inputForestMap[[1:3]],
                          inputLossMap = inputLossMap,
                          studysite = studysite[1,],
                          latlon = TRUE,
                          polyName = "id",
                          ncores = 1,
                          saveCSV = FALSE,
                          years = 2000:2003),  "years differ")
  })
  it("Sp object to function",{
    expect_error(LossCalc(inputForestMap =inputForestMap[[1:3]],
                          inputLossMap = inputLossMap,
                          studysite = as_Spatial(studysite[1,]),
                          latlon = TRUE,
                          polyName = "id",
                          ncores = 1,
                          saveCSV = FALSE,
                          years = 2000:2002),  "No valid spatial object")
  })
  it("No features", {
    expect_error(LossCalc(inputForestMap =inputForestMap[[1:3]],
                          inputLossMap = inputLossMap,
                          studysite = studysite[0,],
                          latlon = TRUE,
                          polyName = "id",
                          ncores = 1,
                          saveCSV = FALSE,
                          years = 2000:2002),  "0 features")
  })
  it("Wrong id specified",{
    expect_error(LossCalc(inputForestMap = inputForestMap[[1:3]],
                          inputLossMap = inputLossMap,
                          studysite = studysite[1,],
                          latlon = TRUE,
                          polyName = "ID",
                          ncores = 1,
                          saveCSV = FALSE,
                          years = 2000:2002), "polyName")
  })

  studysite$id[2] = "1"
  it("ID values are not unique",{
    expect_error(LossCalc(inputForestMap = inputForestMap[[1:3]],
                          inputLossMap = inputLossMap,
                          studysite = studysite[1:2,],
                          latlon = TRUE,
                          polyName = "id",
                          ncores = 1,
                          saveCSV = FALSE,
                          years = 2000:2002), "not unique")
  })
  studysite$id[2] = "2"
  it("Latlon not correctly specified",{
    expect_error(LossCalc(inputForestMap = inputForestMap[[1:3]],
                          inputLossMap = inputLossMap,
                          studysite = studysite[1:2,],
                          polyName = "id",
                          latlon = "True",
                          ncores = 1,
                          saveCSV = FALSE,
                          years = 2000:2002), "No valid input for 'latlon'")
  })

  it("Different crs for forest map and studysite",{
    expect_error(LossCalc(inputForestMap = inputForestMap[[1:3]],
                          inputLossMap = inputLossMap,
                          studysite = studysite_proj[1:2,],
                          polyName = "id",
                          latlon = TRUE,
                          ncores = 1,
                          saveCSV = FALSE,
                          years = 2000:2002), "objects are not identical")
  })

  it("Different crs for loss map and studysite",{
    expect_error(LossCalc(inputForestMap = inputForestMap_proj[[1:3]],
                          inputLossMap = inputLossMap,
                          studysite = studysite_proj[1:2,],
                          polyName = "id",
                          latlon = TRUE,
                          ncores = 1,
                          saveCSV = FALSE,
                          years = 2000:2002), "objects are not identical")
  })

  it("CSV output directory does not exist",{
    expect_error(LossCalc(inputForestMap = inputForestMap[[1:3]],
                          inputLossMap = inputLossMap,
                          studysite = studysite[1:2,],
                          polyName = "id",
                          latlon = TRUE,
                          ncores = 1,
                          saveCSV = "weird/path/to/nowhere",
                          years = 2000:2002), "does not exist")
  })
})


# check for missing values in loss year
inputLossMap[inputLossMap == 7] = 0

loss_stats = LossCalc(inputForestMap = inputForestMap[[6:8]],
                      inputLossMap = inputLossMap,
                      studysite = studysite[1,],
                      latlon = TRUE,
                      polyName = "id",
                      ncores = 1,
                      saveCSV = FALSE,
                      years = 2006:2008)


describe("testing area calculation with a missing year", {
  it("Check that all cloumns are present", {
    expect_equal(ncol(loss_stats), 5)
  })
  it("Check that 2007 is equal to 0",{
    expect_equal(st_drop_geometry(loss_stats)[,3], 0)
  })
})

