# non-interactive mode is used to process 2km downscaled outputs
# interactive mode is fine for raw GCM world grids and small 2km domains
comargs <- (commandArgs(TRUE))
if(length(comargs)) for(z in 1:length(comargs)) eval(parse(text=comargs[[z]]))

if(interactive()){ # choose one, comment the other
  setwd("/workspace/Shared/Tech_Projects/EPSCoR_Southcentral/project_data/EPSCOR_SC_DELIVERY_AUG2016/downscaled")
  #setwd("/workspace/Shared/Tech_Projects/EPSCoR_Southcentral/project_data/cmip5/prepped")
  ncells <- 1395 # epscor domain size where not NA
  #ncells <- c()
} else {
  setwd("/workspace/Shared/Tech_Projects/EPSCoR_Southcentral/project_data/downscaled")
  ncells <- 1410983 # AK-CAN extent where not NA
}

gcms <- list.files()
rcps <- list.files(gcms[1])

library(parallel)
library(raster)
library(dplyr)
library(purrr)

# function to do the heavy lifting and create data frames
checkMonotonic <- function(i, x){
  rlist <- map(seq_along(x), ~readAll(raster(x[[.x]][i])))
  id <- strsplit(names(rlist[[1]]), "_")[[1]][5:8]
  r1 <- rlist[[2]] - rlist[[1]]
  r2 <- rlist[[3]] - rlist[[2]]
  min_high <- sum( r1[] < 0, na.rm=TRUE)
  max_low <- sum( r2[] < 0, na.rm=TRUE)
  idx_off <- which(r1[] < 0 | r2[] < 0)
  idx_inv <- which(r1[] < 0 & r2[] < 0)
  idx_okay <- which(r1[] >= 0 & r2[] >= 0)
  mean_off <- if(length(idx_off)) round(mean(rlist[[2]][idx_off]), 1) else NA
  mean_inv <- if(length(idx_inv)) round(mean(rlist[[2]][idx_inv]), 1) else NA
  mean_okay <- if(length(idx_okay)) round(mean(rlist[[2]][idx_okay]), 1) else NA
  data.frame(Scenario=id[2], Model=id[1], Year=as.integer(id[4]), Month=month.abb[as.numeric(id[3])],
         Min_high=as.integer(min_high), Max_low=as.integer(max_low), Inversion=as.integer(length(idx_inv)),
         T_ok=mean_okay, T_off=mean_off, T_inv=mean_inv, stringsAsFactors=FALSE) %>% tbl_df
}

# calling function to traverse files and organize output
getTable <- function(rcps, gcms, verbose=TRUE, n.cores=32){
  d <- vector("list", length(gcms))
  for(i in seq_along(gcms)){
    dlist1 <- vector("list", length(rcps))
    for(j in seq_along(rcps)){
      files <- map(c("tasmin", "tas", "tasmax"),
                   ~list.files(file.path(gcms[i], rcps[j], .x), pattern=".tif$", full=TRUE))
      if(length(files[[1]]) == 0) next
      dlist0 <- mclapply(seq_along(files[[1]]), checkMonotonic, files, mc.cores=n.cores)
      dlist1[[j]] <- bind_rows(dlist0)
      if(verbose) print(paste("Model:", gcms[i], "(", i, "of", length(gcms), ").",
                              "Scenario:", rcps[j], "(", j, "of", length(rcps), ")."))
    }
    d[[i]] <- bind_rows(dlist1)
  }
  bind_rows(d) %>% mutate(Scenario=factor(Scenario, levels=rcps), Month=factor(Month, levels=month.abb)) %>%
    arrange(Scenario, Model, Year, Month)
}

# calling function to traverse files and organize output
getFreqMap <- function(mon.idx, rcps, gcms, verbose=TRUE, n.cores=32){
  slist2 <- vector("list", length(gcms))
  for(i in seq_along(gcms)){
    slist1 <- vector("list", length(rcps))
    for(j in seq_along(rcps)){
      files <- map(c("tasmin", "tas", "tasmax"),
                   ~list.files(file.path(gcms[i], rcps[j], .x), pattern=".tif$", full=TRUE))
      if(length(files[[1]]) == 0) next
      
      f <- function(i, x){
        x <- map(x, ~.x[as.numeric(sapply(strsplit(basename(.x), "_"), "[", 7)) == i])
        for(z in seq_along(x[[1]])){
          rlist <- map(seq_along(x), ~readAll(raster(x[[.x]][z])))
          r1 <- rlist[[2]] - rlist[[1]] < 0
          r2 <- rlist[[3]] - rlist[[2]] < 0
          if(z == 1){
            r_lwr_freq <- r1
            r_upr_freq <- r2
          } else {
            r_lwr_freq <- r_lwr_freq + r1
            r_upr_freq <- r_upr_freq + r2
          }
        }
        list(r_lwr_freq, r_upr_freq)
      }
      
      slist <- mclapply(mon.idx, f, files, mc.cores=n.cores)
      slist1[[j]] <- transpose(slist) %>% map(~stack(.x))
      if(verbose) print(paste("Model:", gcms[i], "(", i, "of", length(gcms), ").",
                              "Scenario:", rcps[j], "(", j, "of", length(rcps), ")."))
    }
    slist2[[i]] <- slist1
  }
  slist2
}

# alternative function to handle raw gcm .nc file inputs
checkMonotonicFromBrick <- function(i, x, rcp, gcm){
  kel <- 273.15
  rlist <- map(seq_along(x), ~readAll(x[[.x]][[i]]))
  id <- strsplit(names(rlist[[1]]), "\\.")[[1]][1:2]
  r1 <- rlist[[2]] - rlist[[1]]
  r2 <- rlist[[3]] - rlist[[2]]
  min_high <- sum( r1[] < 0, na.rm=TRUE)
  max_low <- sum( r2[] < 0, na.rm=TRUE)
  idx_off <- which(r1[] < 0 | r2[] < 0)
  idx_inv <- which(r1[] < 0 & r2[] < 0)
  idx_okay <- which(r1[] >= 0 & r2[] >= 0)
  mean_off <- if(length(idx_off)) round(mean(rlist[[2]][idx_off]), 1) else NA
  mean_inv <- if(length(idx_inv)) round(mean(rlist[[2]][idx_inv]), 1) else NA
  mean_okay <- if(length(idx_okay)) round(mean(rlist[[2]][idx_okay]), 1) else NA
  data.frame(Scenario=rcp, Model=gcm, Year=as.integer(substr(id[1], 2, 5)), Month=month.abb[as.numeric(id[2])],
             Min_high=as.integer(min_high), Max_low=as.integer(max_low), Inversion=as.integer(length(idx_inv)),
             T_ok=mean_okay-kel, T_off=mean_off-kel, T_inv=mean_inv-kel, stringsAsFactors=FALSE) %>% tbl_df
}

# alternative function to handle raw gcm .nc file inputs
getTableFromBrick <- function(rcps, gcms, verbose=TRUE, n.cores=32){
  vars <- c("tasmin", "tas", "tasmax")
  d <- vector("list", length(gcms))
  for(i in seq_along(gcms)){
    dlist1 <- vector("list", length(rcps))
    for(j in seq_along(rcps)){
      files <- map(vars,
                   ~list.files(file.path(gcms[i], rcps[j], .x), pattern=".nc$", full=TRUE))
      if(length(files[[1]]) == 0) next
      blist <- map2(files, vars, ~brick(.x, varname=.y))
      if(j==1){
        r <- subset(blist[[1]], 1)
        ncells <- c(ncells, length(which(!is.na(r[]))))
      }
      for(k in seq_along(blist)) blist[[k]] <- map(1:nlayers(blist[[k]]), ~subset(blist[[k]], .x))
      dlist0 <- mclapply(seq_along(blist[[1]]), checkMonotonicFromBrick, x=blist, rcp=rcps[j], gcm=gcms[i], mc.cores=n.cores)
      dlist1[[j]] <- bind_rows(dlist0)
      if(verbose) print(paste("Model:", gcms[i], "(", i, "of", length(gcms), ").",
                              "Scenario:", rcps[j], "(", j, "of", length(rcps), ")."))
    }
    d[[i]] <- bind_rows(dlist1)
  }
  bind_rows(d) %>% mutate(Scenario=factor(Scenario, levels=rcps), Month=factor(Month, levels=month.abb)) %>%
    arrange(Scenario, Model, Year, Month)
}

tmpDir <- "/workspace/UA/mfleonawicz/tmpDir"
if(interactive()){ # choose one, comment the other
  d <- getTable(rcps, gcms)
  #d <- getTableFromBrick(rcps, gcms[gcms!="log_file_prep.txt"]) # messy input folder at time of last use
  save(d, ncells, file=file.path(tmpDir, "monotonic_temp_vars.RData"))
  #save(d, ncells, file=file.path(tmpDir, "monotonic_temp_vars_raw.RData"))
} else { # the difference is with ak-can extent 2km maps, must process one set of file per slurm job to avoid crash
  slist <- getFreqMap(1:12, rcps[j], gcms[i])
  d <- getTable(rcps[j], gcms[i])
  file <- paste0(tmpDir, "/akcan2km_monotonic_temp_vars_", rcps[j], "_", gcms[i], ".RData")
  if(nrow(d) > 0) save(slist, d, ncells, file=file)
}

# interactive cleanup following slurm jobs
# put in explicit interactive() call to avoid calling during end of jobs
if(interactive()){
  files <- list.files(tmpDir, pattern="^akcan2km_.*.RData$", full=TRUE)
  dlist <- vector("list", length(files))
  for(i in seq_along(files)){
    load(files[i])
    dlist[[i]] <- d
  }
  d <- bind_rows(dlist) %>% mutate(Scenario=factor(Scenario, levels=unique(Scenario))) %>%
    arrange(Scenario, Model, Year, Month)
  rm(dlist)
  save(d, ncells, file=file.path(tmpDir, "monotonic_temp_vars_akcan2km.RData"))
  if(file.exists(file.path(tmpDir, "monotonic_temp_vars_akcan2km.RData")) & length(files)) unlink(files)
}
