

# Load libraries -----------------------------------------------------------
require(pacman)
pacman::p_load(raster, rgdal, rgeos, terra, stringr, glue, sf, tidyverse, gtools, foreach, doSNOW)

g <- gc(reset = TRUE)
rm(list = ls())

# Functions to use --------------------------------------------------------
extMskNC <- function(vr, yr){
  cat('Start\n')
  fle <- grep(vr, fls, value = T) %>% grep(yr, ., value = T)
  bck <- raster::brick(fle)
  nms <- names(bck) %>% gsub('\\.', '_', .)
  trr <- terra::rast(bck)
  trr <- trr[[1:12]]
  lim <- vect(cnt)
  trr <- terra::crop(trr, lim)
  trr <- terra::mask(trr, lim)
  cnt <- unique(cnt$GID_0)
  out <- glue('../rst/clima/hs/{yr}/{cnt}_{vr}_{1:12}.tif')
  ifelse(!dir.exists(unique(dirname(out))), dir.create(unique(dirname(out)), recursive = TRUE), print('Dir already exists'))
  Map('writeRaster', x = trr, filename = out, overwrite = TRUE)
  cat('To convert to a table ', vr, ' ', yr, '\n')
  bck <- stack(trr)
  bck <- rasterToPoints(bck) %>% as_tibble()
  names(bck)[1:2] <- c('Lon', 'Lat')
  bck$gid <- 1:nrow(bck)
  csv <- glue('../tbl/clima/hs/{cnt}_{vr}_{yr}.csv')
  write.csv(bck, csv, row.names = F)
  cat('Done\n')
}

# Load data ---------------------------------------------------------------
pth <- '//dapadfs/workspace_cluster_9/Coffee_Cocoa2/_guatemala/_data/_nc/_world'
fls <- list.files(pth, full.names = TRUE, pattern = '.nc$')
fls <- grep(paste0(1980:2017, collapse = '|'), fls, value = TRUE)

cnt <- 'DOM'
cnt <- raster::getData('GADM', country = cnt, level = 0)
vrs <- c('pet', 'ppt', 'tmax', 'tmin')
yrs <- basename(fls) %>% readr::parse_number() %>% unique()
ext <- extent(cnt)

# Precipitation
map(1:length(yrs), function(z)extMskNC(vr = 'ppt', yr = yrs[z]))

# Maximum temperature
map(1:length(yrs), function(z)extMskNC(vr = 'tmax', yr = yrs[z]))

# Minimum temperature
map(1:length(yrs), function(z)extMskNC(vr = 'tmin', yr = yrs[z]))




