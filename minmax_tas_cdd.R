# @knitr setup
if(interactive()) { gcm <- "IPSL-CM5A-LR"; rcp <- "rcp85" } else {
  comargs <- (commandArgs(TRUE))
  if(length(comargs)) for(z in 1:length(comargs)) eval(parse(text=comargs[[z]]))
  if(!exists("rcp") | !exists("gcm")) stop("Must provide valid 'rcp' and 'gcm' args.")
}

library(parallel); library(rgdal); library(raster); library(purrr)
rasterOptions(chunksize=10e10, maxmemory=10e11)

setwd("/workspace/Shared/Tech_Projects/EPSCoR_Southcentral/project_data")
rawDir <- "cmip5/prepped"
dsDir <- "downscaled"
vars <- c("tasmin", "tas", "tasmax")
dir.create(outDir <- file.path("/atlas_scratch/mfleonawicz/tas_minmax_ds", gcm, rcp), recur=T, showWarnings=F)
walk(vars[c(1,3)], ~dir.create(file.path(outDir, .x), showWarnings=F))

# @knitr functions
devFromMean <- function(rcp, gcm){
  if(gcm=="NCAR-CCSM4") gcm <- "CCSM4" # temporary fix: raw files have different name
  x <- map(vars, ~readAll(brick(list.files(file.path(rawDir, gcm, rcp, .x), full=T), varname=.x)))
  list(lwr=x[[1]] - x[[2]], upr=x[[3]] - x[[2]])
}

fortify <- function(x){
  r <- raster(extent(0,360,-90,90), nrow=nrow(x), ncol=ncol(x), crs=projection(x))
  round(rotate(resample(x, r)), 1)
}

dsFiles <- function(rcp, gcm){
  v <- list.files(file.path(dsDir, gcm, rcp, vars[2]), full=T)
  yrs <- as.integer(substr(map_chr(strsplit(basename(v), "_"), 8), 1, 4))
  v <- as.character(unlist(split(v, yrs)))
  list(gsub(vars[2], vars[1], v), v, gsub(vars[2], vars[3], v)) %>% setNames(vars)
}

writeMinMax <- function(i, x, y, dir){
  r <- readAll(raster(y$tas[i]))
  rmin <- round(r + projectRaster(subset(x$lwr, i), r), 1)
  rmax <- round(r + projectRaster(subset(x$upr, i), r), 1)
  walk2(list(rmin, rmax), vars[c(1,3)], ~writeRaster(.x, file.path(outDir, .y, basename(y[[.y]][i])), datatype="FLT4S",overwrite=T))
  print(paste("File", i, "of", length(y$tas), "saved."))
}

# @knitr run
deltas <- devFromMean(rcp, gcm) %>% map(~fortify(.x))
files <- dsFiles(rcp, gcm)
mclapply(seq_along(files$tas), writeMinMax, deltas, files, mc.cores=32)
