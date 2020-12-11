#' Translation pipeline using GRASS
#'
#' @param grass A character vector pointing to the directory where GRASS binaries are installed.
#' @param addon_base A character vector pointing to the directory where GRASS add-on binaries are installed.
#'   In order to work, the (\code{r.area})[https://grass.osgeo.org/grass78/manuals/addons/r.area.html]
#'   add-on needs to be installed. See (here)[https://grass.osgeo.org/download/addons/] to learn
#'   how to install GRASS add-ons.
#' @param areas An \code{sf} object with polygons representing areas of interest for which to
#'   preprocess individual rasters.
#' @param tree_cover A character vector pointing to the raster file representing
#'   tree cover in percentage for the entire study domain.
#' @param tree_loss A character vector pointing to the raster file representing
#'   the year of tree cover loss for the entire study domain.
#' @param tree_co2 A character vector pointing to the raster file representing
#'   co2 emissions from tree cover loss for the entire study domain.
#' @param idcol A character vector identifying the a column name which uniquely
#'   identifies the polygons in the \code{areas} object. Values  will be used to name
#'   the output rasters.
#' @param thresholdClump A numeric value identifying the number of pixels of smallest clumps.
#'   All raster cell clumps smaller than the specified threshold will be removed.
#' @param thresholdCover A numeric value identifying the smallest cover percentage
#'   to be considered as forest. All raster cell values below this threshold will be
#'   removed
#' @param years A numeric vector specifying for which years to calculate an annual
#'   forest mask starting from 2001. (e.g. years = c(2001:2010)).
#' @param outdir A character pointing to a directory where the output files
#'   will be written to. If files with the same name as specified by the values in
#'   \code{idcol} are present, their calculation is skipped without warning.
#' @param saveRaster Logical indicating if raster are to be saved to disk
#' @export statsGRASS
#' @name statsGRASS
#' @importFrom sf st_drop_geometry st_geometry st_crs st_transform st_write
#' @importFrom raster raster brick
#' @importFrom rgrass7 initGRASS execGRASS
#' @importFrom stringr str_sub str_split str_remove
#' @importFrom dplyr bind_cols
#' @author Darius Görgen, Dr. Fabian Löw (MapTailor Geospatial Consulting GbR) \email{info@maptailor.net}
#' \cr
#' \emph{Maintainer:} KfW FZ Evaluierung \email{FZ-Evaluierung@kfw.de}
#' \cr
#' \emph{Contact Person:} Dr. Johannes Schielein
#' \cr
#' \emph{Copyright:} KfW Bankengruppe
#' \cr
#' \emph{License:} GPL-3
statsGRASS <- function(grass, addon_base,
                       areas, tree_cover, tree_loss, tree_co2,
                       idcol, thresholdClump, thresholdCover,
                       years, outdir = NULL, saveRaster){

  # get unique ids from idcol
  if (saveRaster) dir.create(outdir, showWarnings = F)

  ids = unlist(st_drop_geometry(areas[, idcol]))
  if (length(unique(ids)) != length(ids)){
    stop(paste0("Data in column ", idcol, " does not uniquley identify observations."))
  }

  # make geometries to a list
  polies = lapply(1:nrow(areas), function(i){
    st_geometry(areas[i, ])
  })

  # get projection information
  proj_raster = st_crs(raster(tree_cover))
  # create run directory for calculations
  rundir = tempfile()
  dir.create(rundir)

  # initiate GRASS session at PERMANENT
  initGRASS(gisBase = grass,
            gisDbase = file.path(rundir, "gisdb"),
            home = file.path(rundir),
            location = "run",
            addon_base = addon_base,
            mapset = "PERMANENT", override = T)
  execGRASS("g.proj", flags = "c", epsg = 4326)

  if(!file.exists(file.path(addon_base, "bin", "r.area"))){
    dir.create(addon_base, showWarnings = F, recursive = T)
    execGRASS("g.extension", extension = "r.area", prefix = addon_base)
  }

  for (i in 1:length(polies)){

    # check if output file is already created
    id = ids[i]
    if(saveRaster){
      outname = file.path(outdir, paste0(id, ".tif"))
      if(file.exists(outname)){
        message("File already exists in outdir. Using this file to calculate zonal statistics.")
      }
    }


    # write polygon to disk
    poly = st_transform(polies[[i]], proj_raster)
    st_write(poly, file.path(rundir, paste0("poly_",i, ".gpkg")))

    # create mapset in in region and update region to default
    initGRASS(gisBase = grass,
              gisDbase = file.path(rundir, "gisdb"),
              home = file.path(rundir),
              location = "run",
              addon_base = addon_base,
              mapset = i, override = T)
    execGRASS("g.region", flags = "d")



    if(!file.exists(outname)){
      ext = as.numeric(st_bbox(poly))
      # warp rasters to poly extent
      raw_command = paste0('gdalwarp -te ', paste(ext, collapse = " "), ' %s ', file.path(rundir, "%s"))
      system(sprintf(raw_command, tree_cover, paste0(i, "_poly_cover.tif")))
      system(sprintf(raw_command, tree_loss, paste0(i, "_poly_loss.tif")))
      system(sprintf(raw_command, tree_co2, paste0(i, "_poly_co2.tif")))

      # read in rasters
      execGRASS("r.in.gdal", flags = c("o", "overwrite"), input = file.path(rundir, paste0(i, "_poly_cover.tif")), output = "raw_cover")
      execGRASS("r.in.gdal", flags = c("o", "overwrite"), input = file.path(rundir, paste0(i, "_poly_loss.tif")), output = "loss")
      execGRASS("r.in.gdal", flags = c("o", "overwrite"), input = file.path(rundir, paste0(i, "_poly_co2.tif")), output = "co2")

      # set location
      execGRASS("g.region", raster= "raw_cover", flags = "p")
      # recode cover to binary
      writeLines(paste0("0 thru ",thresholdCover-1," = 0\n",thresholdCover," thru 100 = 1"), file.path(rundir, "rcl.txt"))
      execGRASS("r.reclass", input = "raw_cover", output = "bc", rules =  file.path(rundir, "rcl.txt"))
      execGRASS("r.mapcalc", expression = '"binary_cover = bc"')
      # detect clumps
      execGRASS("r.clump", input = "binary_cover", output = "clump")
      # remove smaller areas than threshold
      execGRASS(file.path(addon_base, "bin", "r.area"), input = "clump", output = "clump_rm", lesser = thresholdClump, flags = "b")
      # exclude removed pixels from cover raster
      execGRASS("r.mapcalc", expression = '"y2000 = binary_cover * clump_rm"')
      # system("r.out.gdal -cm --overwrite in=clean_cover out=clean_cover.tif")
      # create output group
      execGRASS("i.group", group = "output", input = "raw_cover,loss,co2,y2000" )
      # loop trhough values to get yearly layers
      vals = as.numeric(str_sub(years, -2, -1))
      for (j in 1:length(vals)){
        writeLines(paste0("0 = 1\n1 thru ", vals[j], " = 0\n", vals[j]+1, " thru 100 = 1"), file.path(rundir, "rcl.txt"))
        # recode loss as all values equal and below val to 0 and all values above val to 1, call result layer loss_t
        execGRASS("r.reclass", input = "loss", output ="lt", rules = file.path(rundir, "rcl.txt"), flags = "overwrite")
        execGRASS("r.mapcalc", expression = '"loss_t = lt"', flags = "overwrite")
        # calculate cover_t based on base cover times loss_t
        execGRASS("r.mapcalc", expression = paste0('"y',years[j],' = y2000 * loss_t"'))
        execGRASS("i.group", group = "output", input = paste0("y",years[j]))
      }

    } else { # in case file is alread present

      execGRASS("r.in.gdal", flags = c("o", "overwrite"), input = outname, output = "infile")
      execGRASS("g.region", raster= "infile.1", flags = "p")
      execGRASS("g.rename", raster=c("infile.1,raw_cover,infile.2,loss,infile.3,co2,infile.4,y2000"))

      execGRASS("i.group", group = "output", input = "raw_cover,loss,co2,y2000" )

      infiles = paste("infile", 5:(4+length(years)), sep = ".")
      outfiles = paste("y", years, sep ="")

      # rename the yearly layers
      for (j in 1:length(infiles)){
        execGRASS("g.rename", raster=paste0(infiles[j],",",outfiles[j]))
      }

      # system("g.list type=raster pattern=*")
    }

    # --------------------------------- ZONAL STATS ---------------------------#
    # calculate zonal stats
    execGRASS("v.in.ogr", input = file.path(rundir, paste0("poly_", i, ".gpkg")), output = "poly" )
    execGRASS("v.to.rast", input = "poly", type = "area", output = "polyr", use = "val", value = 1)
    execGRASS("r.null", map = "polyr", null = 0)
    # calculate yearly forest cover statistics
    layernames = paste("y", c(2000,years), sep="")

    area_estimates = lapply(1:length(layernames), function(r){
      execGRASS("r.mapcalc", expression = paste0('"area_t = ',layernames[r],' * polyr"'), flags = "overwrite")
      # execGRASS("r.univar", map = "area_t")
      estimate = execGRASS("r.stats", input = "area_t", flags = c("n", "a"), intern = TRUE)

      if(length(estimate) == 1){
        if(length(grep("1 ", estimate)) == 1){
          estimate = as.numeric(str_split(estimate, " ")[[1]][[2]])
        } else {
          estimate = 0
        }
      }else{

        estimate = estimate[grep("1 ", estimate)]
        estimate = as.numeric(str_split(estimate, " ")[[1]][[2]])

      }
      return(estimate)
    })
    area_estimates = unlist(area_estimates) / 10000 # convert to ha

    # calculate forest loss by using base year info
    # base area
    loss_estimates = lapply(2:length(area_estimates), function(j){
      area_estimates[j-1] - area_estimates[j]
    })
    loss_estimates = c(0, unlist(loss_estimates))

    # calculate CO2 loss
    index = which(loss_estimates != 0)

    if(length(index) == 0) {
      co2_estimates = loss_estimates
    } else {

      co2_estimates = rep(0, length(loss_estimates))

      for(j in index){
        writeLines(paste0("0 = 0\n1 = 1\n2 = 0"), file.path(rundir, "rcl.txt"))

        execGRASS("r.mapcalc", expression = paste0('"mask = ',layernames[j-1],' + ',layernames[j],'"'), flags = "overwrite")
        execGRASS("r.reclass", input = "mask", output = "mask2", rules =  file.path(rundir, "rcl.txt"), flags = "overwrite")
        execGRASS("r.mapcalc", expression = '"mask2 = mask2"', flags = "overwrite")
        execGRASS("r.mapcalc", expression = '"mask2 = mask2 * polyr"', flags = "overwrite")
        execGRASS("r.mapcalc", expression = '"mask2 = mask2 * co2"', flags = "overwrite")
        # system("r.out.gdal input=mask2 output=mask2.tif --overwrite")
        output = execGRASS("r.univar", map = "mask2", intern = T)

        if(length(output) == 0){ # for cases where no co2 emission is recorded

          co2_estimates[j] = 0

        } else { # otherwise extract the sum

          output = output[grep("sum", output)]
          estimate = as.numeric(str_remove(output, "sum: "))
          co2_estimates[j] = estimate
        }
      }
    }

    # prepare output
    names(area_estimates) = c("area_2000", paste("area_", years, sep = ""))
    names(loss_estimates) = c("loss_2000", paste("loss_", years, sep = ""))
    names(co2_estimates) = c("co2_2000", paste("co2_", years, sep = ""))

    poly = st_sf(poly)

    for (r in 1:(length(years)+1)){
      poly[names(area_estimates)[r]] = area_estimates[r]
    }
    for (r in 1:(length(years)+1)){
      poly[names(loss_estimates)[r]] = loss_estimates[r]
    }
    for (r in 1:(length(years)+1)){
      poly[names(co2_estimates)[r]] = co2_estimates[r]
    }

    if(saveRaster){
      if(!file.exists(outname)){
        # write resulting raster brick to disk
        # system(paste0('r.out.gdal -mc createopt="COMPRESS=LZW" input=output output=',outname,' format=GTiff'))
        execGRASS("r.out.gdal", createopt = '"COMPRESS=LZW"', input = "output", output = outname, format = "GTiff", flags = c("m", "c"))
      }
    }
    # delete all files in current grass mapset
    execGRASS("g.remove", type = "all", pattern = "*", flags = "f")
    # remove file from grass db
    unlink(file.path(rundir, "gisdb", "run", i), recursive = T)
    # remove files from parent dir
    file.remove(list.files(rundir, pattern = as.character(i), full.names = T))
    # save zonal stats
    polies[[i]] = poly
  }
  # bind results
  polies = do.call(rbind, polies)
  polies = st_drop_geometry(polies)
  areas = bind_cols(areas, polies)
  # remove run directory
  unlink(rundir, recursive = T)
  # return
  return(areas)
}

