
# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(raster, rgdal, rgeos, gtools, glue, stringr, sf, tidyverse, terra)

g <- gc(reset = TRUE)
rm(list = ls())

# Load data ---------------------------------------------------------------
root <- '//dapadfs/workspace_cluster_9/Coffee_Cocoa2/_cocoa_cam/_data/_tif/_climate/_future/_rcp60/2020_2049'
gcms <- list.files(root, full.names = FALSE)
vars <- paste0(c(glue('prec_{1:12}'), glue('tmax_{1:12}'), glue('tmean_{1:12}'), glue('tmin_{1:12}')))
prds <- c('2020_2049', '2040_2069')

# Get future climate ------------------------------------------------------
get_future <- function(cnt, prd){
  
  cat('Start\n')
  pth <- '//dapadfs/workspace_cluster_9/Coffee_Cocoa2/_cocoa_cam/_data/_tif/_climate/_future/_rcp60'
  fls <- glue('{pth}/{prd}/{gcms}')
  lim <- raster::getData(name = 'GADM', country = cnt, level = 0)
  
  lyr <- map(.x = 1:length(vars), .f = function(i){
    
    cat(i, '\n')
    trr <- glue('{fls}/{vars[i]}') 
    trr <- stack(trr)
    trr <- crop(trr, lim)
    trr <- mask(trr, lim)
    trr <- mean(trr)
    cat('Done\n')
    return(trr)
    
  })
  
  dir <- glue('../rst/clima/ft/rcp60/{prd}/{cnt}/')
  ifelse(!dir.exists(dir), dir.create(dir, recursive = TRUE), print('Exists'))
  Map('writeRaster', x = lyr, filename = glue('{dir}/{vars}.asc'), overwrite = FALSE)
  cat('---Done---\n')
  
}

# Apply the function ------------------------------------------------------
cntrs <- c('SLV', 'HND', 'GTM', 'DOM')

map(1:length(cntrs), .f = function(y){
  map(1:length(prds), .f = function(j){
    cat(cntrs[y], ' ', prds[j], '\n')
    get_future(cnt = cntrs[y], prd = prds[j])
  })
})

get_future(cnt = 'NIC', prd = prds[2])

