# test script for the R preprocessing functions

library(sf)
library(raster)
library(mapme.forest)
library(testthat)
# local_edition(3)
treeCover = raster(system.file("extdata", "pkgTest_treecover2000.tif",
                               package = "mapme.forest"))
lossYear = raster(system.file("extdata", "pkgTest_lossyear.tif",
                              package = "mapme.forest"))
# make lossyear with only 0s
lossYear2 = lossYear
lossYear2[] = 0
# prepare a binary raster
bc = treeCover
bc[treeCover<75] = 0
bc[treeCover>=75] = 1

# Testing the prepTC routine
describe("Testing prepTC", {
  it("only cover threshold supplied", {
    expect_silent(prepTC(treeCover, thresholdCover = 75))
  })
  it("only clump threshold supplied", {
    expect_silent(prepTC(bc, thresholdClump = 25))
  })
  it("both thresholds supplied", {
    expect_silent(prepTC(treeCover, thresholdCover = 75, thresholdClump = 25))
  })
  it("Unsuitable raster for clump removal only",{
    expect_error(prepTC(treeCover, thresholdClump = 25), "clump removal")
  })
})

# testing getTM routine
describe("Testing getTM", {
  it("valid yearly calculation",{
  expect_silent(getTM(inputForestMap = bc, inputLossMap = lossYear, years = 2000:2018))
  })
  it("no forest loss supplied", {
    expect_message(getTM(inputForestMap = bc, inputLossMap = lossYear2, years = 2001:2018), " Replicating inputForestMap")
  })
  it("invalid yearly calculation ",{
  expect_error(getTM(inputForestMap = bc, inputLossMap = lossYear, years = 1999:2018),"Starting year")
  })
})
