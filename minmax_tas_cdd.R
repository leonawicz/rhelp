# setup
if(interactive()) { gcm <- "IPSL-CM5A-LR"; rcp <- "rcp85" } else {
  comargs <- (commandArgs(TRUE))
  if(length(comargs)) for(z in 1:length(comargs)) eval(parse(text=comargs[[z]]))
}

library(parallel); library(rgdal); library(raster); library(purrr)

setwd("/workspace/Shared/Tech_Projects/EPSCoR_Southcentral/project_data")
rawDir <- "cmip5/prepped"
dsDir <- "downscaled"
varDirs <- c("tasmin", "tasmax")
dir.create(outDir <- file.path("/atlas_scratch/mfleonawicz/test/tas_minmax_ds", gcm, rcp), recur=T, showWarnings=F)
walk(varDirs, ~dir.create(file.path(outDir, .x), showWarnings=F))

# functions
devFromMean <- function(rcp, gcm, vars=c("tasmin", "tas", "tasmax")){
  x <- map(vars, ~readAll(brick(list.files(file.path(rawDir, gcm, rcp, .x), full=T), varname=.x)))
  list(lwr=x[[1]] - x[[2]], upr=x[[3]] - x[[2]])
}

fortify <- function(x){
  r <- raster(extent(0,360,-90,90), nrow=nrow(x), ncol=ncol(x), crs=projection(x))
  round(rotate(resample(x, r)), 1)
}

dsFiles <- function(rcp, gcm, vars=c("tasmin", "tas", "tasmax")){
  files <- map(vars, ~list.files(file.path(dsDir, gcm, rcp, .x), full=T))
  yrs <- map(files, ~as.integer(substr(map_chr(strsplit(basename(.x), "_"), 8), 1, 4)))
  map(files, ~as.character(unlist(split(.x, yrs)))) %>% setNames(vars)
}

writeMinMax <- function(i, x, y, dir){
  r <- readAll(raster(y$tas[i]))
  rmin <- round(r + projectRaster(subset(x$lwr, i), r), 1)
  rmax <- round(r + projectRaster(subset(x$upr, i), r), 1)
  writeRaster(rmin, file.path(outDir, varDirs[1], basename(y$tasmin[i])), datatype="FLT4S",overwrite=T)
  writeRaster(rmax, file.path(outDir, varDirs[2], basename(y$tasmax[i])), datatype="FLT4S",overwrite=T)
  print(paste("File", i, "of", length(y$tas), "saved."))
}

# run
deltas <- devFromMean(rcp, gcm) %>% map(~fortify(.x))
files <- dsFiles(rcp, gcm)
mclapply(seq_along(files$tas), writeMinMax, deltas, files, mc.cores=32)
