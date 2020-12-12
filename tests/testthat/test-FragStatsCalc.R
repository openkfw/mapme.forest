library(sf)
library(raster)
library(mapme.forest)
library(testthat)

studysite = st_read(system.file("extdata", "aoi_polys.gpkg", package = "mapme.forest"), quiet = T)
inputRasterFiles = stack(system.file("extdata", "pkgTest_yearlyCover.tif",
                                     package = "mapme.forest"))
names(inputRasterFiles) = paste("y_", as.character(2000:2018))


# create projected versions
centroid = suppressWarnings(st_centroid(st_as_sfc(st_bbox(studysite))))
zone = floor((st_coordinates(centroid)[1] + 180) / 6) + 1
utm = paste0("+proj=utm +zone=", zone," +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
studysite_proj = st_transform(studysite, utm)
inputRasterFiles_proj = projectRaster(inputRasterFiles, crs = utm, method = "ngb")

result = FragStatsCalc(inputRasterFiles = inputRasterFiles,
                       studysite = studysite[1,],
                       FragStats = "all",
                       latlon = TRUE,
                       polyName = "id",
                       ncores = 1,
                       saveCSV = FALSE)


describe("testing frag stats calculation", {
  describe("testing of unprojected data",{
    it("Sequential mode",{
      expect_silent(FragStatsCalc(inputRasterFiles = inputRasterFiles[[1:2]],
                                  studysite = studysite[1,],
                                  FragStats = "all",
                                  latlon = TRUE,
                                  polyName = "id",
                                  ncores = 1,
                                  saveCSV = FALSE))
    })
    it("parallel mode",{
      skip_on_os("windows")
      expect_silent(FragStatsCalc(inputRasterFiles = inputRasterFiles[[1:2]],
                                  studysite = studysite[1:2,],
                                  FragStats = "all",
                                  latlon = TRUE,
                                  polyName = "id",
                                  ncores = 2,
                                  saveCSV = FALSE))

    })
  })
  describe("testing of projected data",{
    it("Sequential mode",{
      expect_silent(FragStatsCalc(inputRasterFiles = inputRasterFiles_proj[[1:2]],
                                  studysite = studysite_proj[1,],
                                  FragStats = "all",
                                  latlon = FALSE,
                                  polyName = "id",
                                  ncores = 1,
                                  saveCSV = FALSE))
    })
    it("parallel mode",{
      skip_on_os("windows")
      expect_silent(FragStatsCalc(inputRasterFiles = inputRasterFiles_proj[[1:2]],
                                  studysite = studysite_proj[1:2,],
                                  FragStats = "all",
                                  latlon = FALSE,
                                  polyName = "id",
                                  ncores = 2,
                                  saveCSV = FALSE))

    })
  })
})

studysite$id = as.character(studysite$id)

describe("check polyName", {
  it("Charachter id column")
  expect_silent(FragStatsCalc(inputRasterFiles = inputRasterFiles_proj[[1:2]],
                              studysite = studysite_proj[1,],
                              FragStats = "all",
                              latlon = FALSE,
                              polyName = "id",
                              ncores = 1,
                              saveCSV = FALSE))
})


# check error messages
describe("Checking error messages",{
  it("Wrong format for forest map",{
    expect_error(FragStatsCalc(inputRasterFiles = "noraster",
                               studysite = studysite_proj[1,],
                               FragStats = "all",
                               latlon = FALSE,
                               polyName = "id",
                               ncores = 1,
                               saveCSV = FALSE), "No valid raster object")
  })
  it("Sp object to function",{
    expect_error(FragStatsCalc(inputRasterFiles = inputRasterFiles[[1:2]],
                               studysite = as_Spatial(studysite[1,]),
                               FragStats = "all",
                               latlon = TRUE,
                               polyName = "id",
                               ncores = 1,
                               saveCSV = FALSE),  "No valid spatial object")
  })
  it("No features", {
    expect_error(FragStatsCalc(inputRasterFiles = inputRasterFiles[[1:2]],
                               studysite = studysite[0,],
                               FragStats = "all",
                               latlon = TRUE,
                               polyName = "id",
                               ncores = 1,
                               saveCSV = FALSE),  "0 features")
  })
  it("Wrong id specified",{
    expect_error(FragStatsCalc(inputRasterFiles = inputRasterFiles[[1:2]],
                               studysite = studysite[1,],
                               FragStats = "all",
                               latlon = TRUE,
                               polyName = "ID",
                               ncores = 1,
                               saveCSV = FALSE), "polyName")
  })
  studysite$id[2] = "1"
  it("ID values are not unique",{
    expect_error(FragStatsCalc(inputRasterFiles = inputRasterFiles[[1:2]],
                               studysite = studysite[1:2,],
                               FragStats = "all",
                               latlon = TRUE,
                               polyName = "id",
                               ncores = 1,
                               saveCSV = FALSE), "not unique")
  })
  studysite$id[2] = "2"
  it("Latlon not correctly specified",{
    expect_error(FragStatsCalc(inputRasterFiles = inputRasterFiles[[1:2]],
                               studysite = studysite[1,],
                               FragStats = "all",
                               latlon = "true",
                               polyName = "id",
                               ncores = 1,
                               saveCSV = FALSE), "No valid input for 'latlon'")
  })

  crs(inputRasterFiles) =  "+proj=utm +zone=32 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
  it("Different crs for forest map and studysite",{
    expect_error(FragStatsCalc(inputRasterFiles = inputRasterFiles[[1:2]],
                               studysite = studysite[1,],
                               FragStats = "all",
                               latlon = TRUE,
                               polyName = "id",
                               ncores = 1,
                               saveCSV = FALSE), "objects are not identical")
  })
  crs(inputRasterFiles) = "+proj=longlat +datum=WGS84 +no_defs"

  it("CSV output directory does not exist",{
    expect_error(FragStatsCalc(inputRasterFiles = inputRasterFiles[[1:2]],
                               studysite = studysite[1,],
                               FragStats = "all",
                               latlon = TRUE,
                               polyName = "id",
                               ncores = 1,
                               saveCSV = "weird/path/to/nowhere"), "does not exist")
  })
})






