setwd(wd <- "/workspace/Shared/Users/afloyd8/EPSCoR_RS/Climate/SWI")
library(raster)
library(purrr)

rcp <- "rcp45" # set to rcp45, rcp60, or rcp85

mainDir <- paste0(wd, "/raw_", rcp)
outDir <- file.path(wd, rcp)
months <- c("05", "06", "07", "08", "09")
decades <- c("2010_2019", "2020_2029", "2030_2039", "2040_2049", "2050_2059", "2060_2069", "2070_2079", "2080_2089", "2090_2099")
input <- list.files(mainDir, pattern=".img$", full.names=T, recursive=F)
bi <- basename(input)

input.months <- substr(bi, nchar(bi)-15, nchar(bi)-14) # month vector corresponding to file vector
input.decades <- substr(bi, nchar(bi)-12, nchar(bi)-4) # same as above for decades
input <- input[input.months %in% months & input.decades %in% decades] # use them to index input to the relevant files
idx <- substr(bi, nchar(bi)-12, nchar(bi)-4) # now index is shorter, make a new decades index
input <- split(input, idx) # split on idx

write_swi <- function(x, decade, rcp, dir){
  s <- stack(x)
  s[s < 0] <- 0
  r <- round(calc(s, mean), 1)
  writeRaster(r, file.path(dir, paste0("swi_", rcp, "_c_", decade, ".img")), dataType="HFA", overwrite=T)
}

walk2(input, names(input), ~write_swi(.x, .y, rcp, outDir))
