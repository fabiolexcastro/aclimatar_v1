
# Load libraries -----------------------------------------------------------
require(pacman)
pacman::p_load(raster, rgdal, future, furrr, trend, forecast, rgeos, stringr, glue, sf, tidyverse, gtools, foreach, doSNOW)

g <- gc(reset = TRUE)
rm(list = ls())

# Functions ---------------------------------------------------------------
create_table <- function(var){
  
  cat('Start', var, '\n')
  tbl.frc <- grep(var, fls.frc, value = TRUE) %>% readRDS()
  cat('To read the future\n')
  if(var == 'ppt'){var = 'prec'}else{var = var}
  rst.ftr <- grep(var, fls.ftr, value = TRUE) %>% stack()
  rst.ftr <- rst.ftr * 1
  cat('To read the historic\n')
  rst.hst <- grep(var, fls.hst, value = TRUE) %>% stack()
  
  cat('To make the for\n')
  rst.frc <- map(.x = 6:17, .f = function(k){
    cat(k, '\n')
    rst <- tbl.frc %>% dplyr::select(Lon, Lat, k) %>% rasterFromXYZ() 
    rsm <- raster::resample(rst, msk, method = 'bilinear')
    rsm <- raster::crop(rsm, msk)
    rsm <- raster::mask(rsm, msk)
    return(rsm)
  }) %>% stack()
  
  cat('The conditional\n')
  if(var == 'ppt'){
    rst.frc <- rst.frc
  } else{
    rst.frc <- rst.frc * 10
  }
  
  cat('To make the stack\n')
  
  tbl.all <- map(.x = 1:12, .f = function(j){
    cat('Start ', j, '\n')
    stk <- stack(rst.hst[[j]], rst.frc[[j]], rst.ftr[[j]])
    names(stk) <- c(glue('{var}_{j}_bsl'), glue('{var}_{j}_frc'), glue('{var}_{j}_ftr'))
    tbl <- rasterToPoints(stk) %>% as_tibble() %>% mutate(gid = 1:nrow(.)) %>% dplyr::select(gid, x, y, everything()) %>% drop_na()
    return(tbl)
  }) %>% purrr::reduce(., inner_join, by = c('gid', 'x', 'y'))
  
  cat('Done\n')
  return(tbl.all)
  
}

# Select the country ------------------------------------------------------
cnt <- 'DOM'

# Load data ---------------------------------------------------------------
fls.frc <- list.files('../rds/clima/hs/frcst', full.names = T, pattern = '.rds$') %>% 
  grep(cnt, ., value = TRUE)
fls.hst <- list.files('../rst/clima/wc/1_4', full.names = T) %>% 
  grep(cnt, ., value = TRUE) %>% 
  list.files(., full.names = TRUE, pattern = '.asc$') %>% 
  mixedsort()
fls.ftr <- list.files('../rst/clima/ft/rcp60/2040_2069', full.names = T) %>% 
  grep(cnt, ., value = TRUE) %>% 
  list.files(., full.names = TRUE, pattern = '.asc$') %>% 
  mixedsort() %>% 
  .[-grep('tmean', ., value = FALSE)]
trg <- read_csv('../tbl/test/climate_gtm.csv')

# Tidy the rasters --------------------------------------------------------
msk <- raster(fls.hst[1]) * 0 + 1

# Create the tables -------------------------------------------------------
vrs <- c('ppt', 'tmax', 'tmin')
tbl <- map(vrs, create_table)
tbl <- tbl %>% purrr::reduce(., inner_join, by = c('gid', 'x', 'y'))
saveRDS(object = tbl, file = glue('../rds/clima/all/tbl_{cnt}_v1.rds'))






                      