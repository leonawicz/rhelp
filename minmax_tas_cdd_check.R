# @knitr setup
rcp <- "rcp85"
gcm <- "IPSL-CM5A-LR"
mo.idx <- 11
years <- 2006:2100
test.cell <- 1091723

library(rgdal); library(raster); library(dplyr); library(purrr)
rasterOptions(chunksize=10e10, maxmemory=10e11)

truthDir <- "/atlas_scratch/mfleonawicz/tas_minmax_ds"
setwd(truthDir)
dsDir <- "/workspace/Shared/Tech_Projects/EPSCoR_Southcentral/project_data/downscaled"
vars <- c("tasmin", "tas", "tasmax")

# @knitr functions
dsFiles <- function(rcp, gcm){
  v <- list.files(file.path(dsDir, gcm, rcp, vars[2]), full=T)
  yrs <- as.integer(substr(map_chr(strsplit(basename(v), "_"), 8), 1, 4))
  v <- as.character(unlist(split(v, yrs)))
  list(gsub(vars[2], vars[1], v), v, gsub(vars[2], vars[3], v)) %>% setNames(vars)
}

# @knitr run
files <- dsFiles(rcp, gcm)
mos <- as.integer(map_chr(strsplit(basename(files[[1]]), "_"), 7))
yrs <- as.integer(substr(map_chr(strsplit(basename(files[[1]]), "_"), 8), 1, 4))
files <- map(files, ~.x[yrs %in% years & mos==mo.idx])
files.check <- map2(files, vars, ~file.path(dsDir, gcm, rcp, .y, basename(.x)))
files.truth <- map2(files, vars, ~file.path(truthDir, gcm, rcp, .y, basename(.x)))

s.check <- map(files.check, ~stack(.x, quick=TRUE))
s.truth <- map(files.truth[c(1,3)], ~stack(.x, quick=TRUE))

v <- map(s.check, ~as.numeric(raster::extract(.x, test.cell)))
v0 <- map(s.truth, ~as.numeric(raster::extract(.x, test.cell)))

d <- bind_rows(data.frame(Year=years, Min=v$tasmin, Max=v$tasmax, Source="Trial Data"),
          data.frame(Year=years, Min=v0$tasmin, Max=v0$tasmax, Source="Truth Data")) %>%
  reshape2::melt(id.vars=c("Year", "Source"), variable.name="Stat", value.name="Temperature") %>%
  bind_rows(data.frame(Year=years, Source="Truth Data", Stat="Mean", Temperature=v$tas)) %>%
  mutate(Source=factor(Source, levels=c("Truth Data", "Trial Data")), Stat=factor(Stat, levels=c("Min", "Mean", "Max")))

#saveRDS(d, "/workspace/UA/mfleonawicz/tmpDir/minmax_ds_check.rds")
saveRDS(d, "/workspace/UA/mfleonawicz/tmpDir/minmax_ds_check_newdata_092916.rds")
