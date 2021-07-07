
# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(raster, rgdal, rgeos, future, furrr, gtools, glue, stringr, sf, tidyverse, terra)

g <- gc(reset = TRUE)
rm(list = ls())

# Load data ---------------------------------------------------------------
root <- '//dapadfs/workspace_cluster_9/Coffee_Cocoa2/_cocoa_cam/_data/_tif/_climate/_future/_rcp60/2020_2049'
gcms <- list.files(root, full.names = FALSE)
vars <- paste0(c(glue('prec_{1:12}'), glue('tmax_{1:12}'), glue('tmean_{1:12}'), glue('tmin_{1:12}')))
prds <- c('2020_2049', '2040_2069')
bses <- list.files('../rds/clima/all', full.names = TRUE,  pattern = '.rds')

# Functions ---------------------------------------------------------------
get_future_prc <- function(cnt, prd, bse){
  
  # cnt <- 'DOM'
  # prd <- '2040_2069'
     
  cat('Start\n')
  pth <- '//dapadfs/workspace_cluster_9/Coffee_Cocoa2/_cocoa_cam/_data/_tif/_climate/_future/_rcp60'
  fls <<- glue('{pth}/{prd}/{gcms}')
  fls <<- paste0(fls)
  lim <<- raster::getData(name = 'GADM', country = cnt, level = 0)
  bse <- grep(cnt, bses, value = TRUE) %>% readRDS()
  msk <- bse %>% dplyr::select(2, 3, 4) %>% rasterFromXYZ()
  msk <- msk * 0 + 1
  
  plan(cluster, workers = 12, gc = TRUE)
  rsl <- map(.x = 1:length(vars), .f = function(i){
    
    cat(i, '\n')
    trr <- glue('{fls}/{vars[i]}') 
    trr <- stack(trr)
    trr <- crop(trr, lim)
    trr <- mask(trr, lim)
    q3 <- calc(trr, fun = function(x) quantile(x, .3, na.rm=TRUE))
    q6 <- calc(trr, fun = function(x) quantile(x, .6, na.rm=TRUE))
    q3 <- raster::crop(q3, msk) %>% raster::mask(., msk)
    q6 <- raster::crop(q6, msk) %>% raster::mask(., msk)
    stk <- stack(msk, q3, q6)
    names(stk) <- c('msk', glue('{vars[i]}_p30_ftr'), glue('{vars[i]}_p60_ftr'))
    rsl <- rasterToPoints(stk) %>% as_tibble %>% mutate(gid = 1:nrow(.)) %>% dplyr::select(6, 1, 2, 4, 5)
    cat('Done\n')
    return(rsl)
    
  })
  
  future:::ClusterRegistry("stop")
  gc(reset = T)
  
  rs2 <- rsl %>% purrr::reduce(., inner_join, by = c('gid', 'x', 'y'))
  colnames(rs2)
  pst <- bse %>% colnames() %>% grep('ftr', ., value = FALSE)
  colnames(bse)[pst] <- c(glue('prec_{1:12}_avg_ftr'), glue('tmax_{1:12}_avg_ftr'), glue('tmin_{1:12}_avg_ftr'))
  
  nrow(bse)
  nrow(rs2)
  inner_join(bse, rs2, by = c('gid'))
  
  all <- cbind(bse, rs2[,-c(1:3)])
  write.csv(all, glue('../tbl/clima/all/tbl_{cnt}.csv'))
  colnames(all)
  cat('Done\n')
  
}

# Target table ------------------------------------------------------------
test <- read_csv('../tbl/test/climate_gtm.csv')
done <- readRDS('../rds/clima/all/tbl_NIC_v1.rds')
colnames(test) %>% grep('ftr', ., value = TRUE)
colnames(done) %>% grep('ftr', ., value = TRUE)
