

# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(raster, rgdal, rgeos, gtools, stringr, sf, tidyverse, terra)

g <- gc(reset = TRUE)
rm(list = ls())

# Load data ---------------------------------------------------------------
root <- '//dapadfs/workspace_cluster_9/Coffee_Cocoa2/_cocoa_cam/_data/_tif/_climate/_current/_asc'
fles <- list.files(root, full.names = TRUE, pattern = '.asc$') %>% 
  mixedsort() %>% 
  grep(paste0(c('prec', 'tmax', 'tmean', 'tmin'), collapse = '|'), ., value = TRUE) 

# Extract by mask ---------------------------------------------------------
get_current <- function(country){
  
  cat(country, '\n')
  shp <- raster::getData(name = 'GADM', country = country, level = 0)
  vct <- terra::vect(shp)
  
  trr <- map(1:length(fles), function(i){
    cat(basename(fles[i]), '\n')
    trr <- terra::rast(fles[i])
    trr <- terra::crop(trr, vct)
    trr <- terra::mask(trr, vct)
    cat('Done\n')
    return(trr)
  })
  
  dir <- glue('../rst/clima/wc/1_4/{country}/')
  ifelse(!dir.exists(dir), dir.create(dir, recursive = TRUE), print('Exists'))
  Map('writeRaster', x = trr, filename = glue('{dir}/{basename(fles)}'), overwrite = FALSE)
  cat('---Done---\n')
  
}

# Nicaragua
get_current(country = 'NIC')

# El Salvador
get_current(country = 'SLV')

# Honduras
get_current(country = 'HND')

# Guatemala
get_current(country = 'GTM')

# Dominican Republic
get_current(country = 'DOM')

# End ---------------------------------------------------------------------


