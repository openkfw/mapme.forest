library(raster)
library(sf)
library(mapme.forest)
library(testthat)

studysite = st_read(system.file("extdata", "aoi_polys.gpkg", package = "mapme.forest"), quiet = T)
inputForestMap = stack(system.file("extdata", "pkgTest_yearlyCover.tif",
                                   package = "mapme.forest"))
inputLossMap = raster(system.file("extdata", "pkgTest_lossyear.tif",
                                  package = "mapme.forest"))
inputCO2Map = raster(system.file("extdata", "pkgTest_co2_emission.tif",
                                 package = "mapme.forest"))

describe("testing area calculation", {
  describe("testing of unprojected data",{
    it("Sequential mode",{
      expect_equal(
        round(
          st_drop_geometry(CO2Calc(inputForestMap = inputForestMap[[9:10]],
                                   inputLossMap = inputLossMap,
                                   inputCO2Map = inputCO2Map,
                                   studysite = studysite[1,],
                                   polyName = "id",
                                   ncores = 1,
                                   saveCSV = FALSE,
                                   years = 2009:2010)), 1),
         data.frame(id = 1, co2_2009 = 0, co2_2010 = 7.3))
    })
    it("parallel mode",{
      skip_on_os("windows")
      expect_equal(
        round(
          st_drop_geometry(CO2Calc(inputForestMap = inputForestMap[[9:10]],
                                   inputLossMap = inputLossMap,
                                   inputCO2Map = inputCO2Map,
                                   studysite = studysite[1,],
                                   polyName = "id",
                                   ncores = 2,
                                   saveCSV = FALSE,
                                   years = 2009:2010)), 1),
        data.frame(id = 1, co2_2009 = 0, co2_2010 = 7.3))
    })
  })
})

studysite$id = as.character(studysite$id)

describe("check polyName", {
  it("Charachter id column")
  expect_silent(CO2Calc(inputForestMap = inputForestMap[[1:3]],
                            inputLossMap = inputLossMap,
                            studysite = studysite[1,],
                            inputCO2Map = inputCO2Map,
                            polyName = "id",
                            ncores = 1,
                            saveCSV = FALSE,
                            years = 2000:2002))
})

area_stats = CO2Calc(inputForestMap = inputForestMap[[1:3]],
                     inputLossMap = inputLossMap,
                     studysite = studysite[1:4,],
                     inputCO2Map = inputCO2Map,
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
    expect_error(CO2Calc(inputForestMap = "noRaster",
                         inputLossMap = inputLossMap,
                         studysite = studysite[1,],
                         inputCO2Map = inputCO2Map,
                         polyName = "id",
                         ncores = 1,
                         saveCSV = FALSE,
                         years = 2000:2002), "No valid raster object")
  })
  it("Wrong format for loss year",{ # check later
    expect_error(CO2Calc(inputForestMap = inputForestMap[[1:3]],
                         inputLossMap = inputForestMap,
                         studysite = studysite[1,],
                         inputCO2Map = inputCO2Map,
                         polyName = "id",
                         ncores = 1,
                         saveCSV = FALSE,
                         years = 2000:2002), "not a single RasterLayer")
  })
  it("Wrong format for co2 layer",{ # check later
    expect_error(CO2Calc(inputForestMap = inputForestMap[[1:3]],
                         inputLossMap = inputLossMap,
                         studysite = studysite[1,],
                         inputCO2Map = inputForestMap,
                         polyName = "id",
                         ncores = 1,
                         saveCSV = FALSE,
                         years = 2000:2002), "not a single RasterLayer")
  })

  it("Differing years",{
    expect_error(CO2Calc(inputForestMap =inputForestMap[[1:3]],
                         inputLossMap = inputLossMap,
                         studysite = studysite[1,],
                         inputCO2Map = inputCO2Map,
                         polyName = "id",
                         ncores = 1,
                         saveCSV = FALSE,
                         years = 2000:2003),  "years differ")
  })
  it("Sp object to function",{
    expect_error(CO2Calc(inputForestMap =inputForestMap[[1:3]],
                         inputLossMap = inputLossMap,
                         studysite = as_Spatial(studysite[1,]),
                         inputCO2Map = inputCO2Map,
                         polyName = "id",
                         ncores = 1,
                         saveCSV = FALSE,
                         years = 2000:2002),  "No valid spatial object")
  })
  it("No features", {
    expect_error(CO2Calc(inputForestMap =inputForestMap[[1:3]],
                         inputLossMap = inputLossMap,
                         studysite = studysite[0,],
                         inputCO2Map = inputCO2Map,
                         polyName = "id",
                         ncores = 1,
                         saveCSV = FALSE,
                         years = 2000:2002),  "0 features")
  })
  it("Wrong id specified",{
    expect_error(CO2Calc(inputForestMap = inputForestMap[[1:3]],
                         inputLossMap = inputLossMap,
                         studysite = studysite[1,],
                         inputCO2Map = inputCO2Map,
                         polyName = "ID",
                         ncores = 1,
                         saveCSV = FALSE,
                         years = 2000:2002), "polyName")
  })

  studysite$id[2] = "1"
  it("ID values are not unique",{
    expect_error(CO2Calc(inputForestMap = inputForestMap[[1:3]],
                         inputLossMap = inputLossMap,
                         studysite = studysite[1:2,],
                         inputCO2Map = inputCO2Map,
                         polyName = "id",
                         ncores = 1,
                         saveCSV = FALSE,
                         years = 2000:2002), "not unique")
  })
  studysite$id[2] = "2"

  it("CSV output directory does not exist",{
    expect_error(CO2Calc(inputForestMap = inputForestMap[[1:3]],
                         inputLossMap = inputLossMap,
                         studysite = studysite[1:2,],
                         polyName = "id",
                         inputCO2Map = inputCO2Map,
                         ncores = 1,
                         saveCSV = "weird/path/to/nowhere",
                         years = 2000:2002), "does not exist")
  })


  crs(inputForestMap) = "+proj=utm +zone=32 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
  it("Different crs for forest map and studysite",{
    expect_error(CO2Calc(inputForestMap = inputForestMap[[1:3]],
                         inputLossMap = inputLossMap,
                         studysite = studysite[1:2,],
                         polyName = "id",
                         inputCO2Map = inputCO2Map,
                         ncores = 1,
                         saveCSV = FALSE,
                         years = 2000:2002), "objects are not identical")
  })
  crs(inputForestMap) = "+proj=longlat +datum=WGS84 +no_defs "

  crs(inputLossMap) = "+proj=utm +zone=32 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
  it("Different crs for loss map and studysite",{
    expect_error(CO2Calc(inputForestMap = inputForestMap[[1:3]],
                         inputLossMap = inputLossMap,
                         studysite = studysite[1:2,],
                         polyName = "id",
                         inputCO2Map = inputCO2Map,
                         ncores = 1,
                         saveCSV = FALSE,
                         years = 2000:2002), "objects are not identical")
  })
  crs(inputLossMap) = "+proj=longlat +datum=WGS84 +no_defs "

  crs(inputCO2Map) = "+proj=utm +zone=32 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
  it("Different crs for co2map and studysite",{
    expect_error(CO2Calc(inputForestMap = inputForestMap[[1:3]],
                         inputLossMap = inputLossMap,
                         studysite = studysite[1:2,],
                         polyName = "id",
                         inputCO2Map = inputCO2Map,
                         ncores = 1,
                         saveCSV = FALSE,
                         years = 2000:2002), "objects are not identical")
  })
  crs(inputCO2Map) = "+proj=longlat +datum=WGS84 +no_defs "
})

# check for missing values in loss year
inputLossMap[inputLossMap == 7] = 0

co2_stats = CO2Calc(inputForestMap = inputForestMap[[6:8]],
                    inputLossMap = inputLossMap,
                    inputCO2Map = inputCO2Map,
                    studysite = studysite[1,],
                    polyName = "id",
                    ncores = 1,
                    saveCSV = FALSE,
                    years = 2006:2008)


describe("testing area calculation with a missing year", {
  it("Check that all cloumns are present", {
    expect_equal(ncol(co2_stats), 5)
  })
  it("Check that 2007 is equal to 0",{
    expect_equal(st_drop_geometry(co2_stats)[,3], 0)
  })
})

