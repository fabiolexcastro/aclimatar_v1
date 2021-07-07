
# Load libraries -----------------------------------------------------------
require(pacman)
pacman::p_load(raster, rgdal, future, furrr, trend, forecast, rgeos, stringr, glue, sf, tidyverse, gtools, foreach, doSNOW)

g <- gc(reset = TRUE)
rm(list = ls())

# Calc forecast --------------------------------------------------------------
get_frcst <- function(cnt, var){
  
  # cnt <- 'NIC'; var <- 'tmax'
  # cnt <- 'NIC'; var <- 'tmin'
  # cnt <- 'NIC'; var <- 'ppt'
  
  cat('Start\n')
  fls <- fles %>%
    grep(var, ., value = TRUE) %>% 
    grep(cnt, ., value = TRUE) %>% 
    mixedsort()
  yrs <- 1980:2017
  dfm <- map(.x = fls, .f = read_csv)
  dfm <- map(.x = 1:length(dfm), .f = function(i) dfm[[i]] %>% mutate(year = yrs[i]))
  dfm <- bind_rows(dfm)
  gid <- unique(dfm$gid)
  crd <- dfm %>% distinct(gid, Lon, Lat)
  
  plan(cluster, workers = 60, gc = TRUE)
  
  rst <- furrr::future_map(.x = gid, .f = function(j){
    
    cat(j, '\n')
    tb <- dfm %>% filter(gid == j)
    
    rs <- map(.x = 3:14, .f = function(k){
      
      tb %>% 
        dplyr::select(Lon, Lat, k) %>% 
        pull() %>% 
        ts() %>% 
        ets() %>% 
        forecast(h = 7) %>% 
        as_tibble() %>% 
        pull(1) %>% 
        mean()
      
    }) %>% unlist()
    
    df <- data.frame(gid = j, forecast = rs, month = month.abb) %>% 
      mutate(month = factor(month, levels = month.abb)) %>% 
      spread(month, forecast)
    
    return(df)
    
  })
  
  
  future:::ClusterRegistry("stop")
  gc(reset = T)
  rst
  rs2 <- bind_rows(rst)
  rs2 <- as_tibble(rs2)
  rs2 <- cbind(crd[,-3], rs2)
  rs2 <- as_tibble(rs2)
  rs2 <- rs2 %>% mutate(country = cnt, variable = var)
  rs2 <- rs2 %>% dplyr::select(gid, Lon, Lat, country, variable, Jan:Dec)
  cat('Done\n')
  saveRDS(object = rs2, file = glue('../rds/clima/hs/frcst/{cnt}_{var}.rds'))
  return(rs2)
  
  
}

# Load data ---------------------------------------------------------------
root <- '../rst/clima/hs'
prds <- list.files('../rst/clima/hs', full.names = T)
vars <- c('ppt', 'tmax', 'tmin')
fles <- list.files('../tbl/clima/hs', full.names = TRUE)

# Apply the function ------------------------------------------------------

# Nicaragua
nic <- map(.x = 1:length(vars), .f = function(k) get_table(cnt = 'NIC', var = vars[k]))
nic <- map(.x = 1:length(nic), .f = function(k) nic[[k]] %>% mutate(variable = vars[k]))
nic <- bind_rows(nic)

# El Salvador
slv <- map(.x = 1:length(vars), .f = function(k) get_table(cnt = 'SLV', var = vars[k]))
slv <- map(.x = 1:length(slv), .f = function(k) slv[[k]] %>% mutate(variable = vars[k]))
slv <- bind_rows(slv)

# Guatemala
gtm <- map(.x = 1:length(vars), .f = function(k) get_table(cnt = 'GTM', var = vars[k]))
gtm <- map(.x = 1:length(gtm), .f = function(k) gtm[[k]] %>% mutate(variable = vars[k]))
gtm <- bind_rows(gtm)

# Honduras
hnd <- map(.x = 1:length(vars), .f = function(k) get_table(cnt = 'HND', var = vars[k]))
hnd <- map(.x = 1:length(hnd), .f = function(k) hnd[[k]] %>% mutate(variable = vars[k]))
hnd <- bind_rows(hnd)

# Republica Dominicana
dom <- map(.x = 1:length(vars), .f = function(k) get_table(cnt = 'DOM', var = vars[k]))
dom <- map(.x = 1:length(dom), .f = function(k) dom[[k]] %>% mutate(variable = vars[k]))
dom <- bind_rows(dom)

# Save these results
dir.create('../rds/clima/hs/', recursive = TRUE)
saveRDS(object = nic, file = '../rds/clima/hs/nic_hs.rds')
saveRDS(object = slv, file = '../rds/clima/hs/slv_hs.rds')
saveRDS(object = gtm, file = '../rds/clima/hs/gtm_hs.rds')
saveRDS(object = hnd, file = '../rds/clima/hs/hnd_hs.rds')
saveRDS(object = dom, file = '../rds/clima/hs/dom_hs.rds')

# Target ------------------------------------------------------------------
test <- read_csv('../tbl/test/climate_gtm.csv')
colnames(test)

# Forecast ----------------------------------------------------------------
nic_frc <- lapply(1:length(vars), function(p){get_frcst(cnt == 'NIC', var = vars[p])})
gtm_frc <- lapply(1:length(vars), function(p){get_frcst(cnt == 'GTM', var = vars[p])})
slv_frc <- lapply(1:length(vars), function(p){get_frcst(cnt == 'SLV', var = vars[p])})
hnd_frc <- lapply(1:length(vars), function(p){get_frcst(cnt == 'HND', var = vars[p])})
dom_frc <- lapply(1:length(vars), function(p){get_frcst(cnt == 'DOM', var = vars[p])})


# End ---------------------------------------------------------------------
# get_table <- function(cnt, var){
#   
#   cat('Start\n')
#   
#   fls <- list.files(prds, full.names = TRUE, pattern = '.tif$') %>%
#     grep(var, ., value = TRUE) %>% 
#     grep(cnt, ., value = TRUE) %>% 
#     mixedsort()
#   
#   rst <- map(.x = 1:length(basename(prds)), .f = function(i){
#     
#     cat(i, '\n')
#     
#     if(var == 'ppt'){
#       cat(var, '\n')
#       rst <- grep(basename(prds)[i], fls, value = TRUE) %>% 
#         terra::rast() %>% 
#         sum()
#     } else{
#       cat(var, '\n')
#       rst <- grep(basename(prds)[i], fls, value = TRUE) %>% 
#         terra::rast() %>% 
#         mean()
#     }
#     
#     cat('Done\n')
#     return(rst)
#     
#   })  
#   
#   crd <- rst[[1]] %>%
#     as.points() %>% 
#     geom() %>% 
#     as_tibble() %>% 
#     mutate(gid = 1:nrow(.)) %>% 
#     dplyr::select(gid, x, y)
#   
#   cat('To create the table\n')
#   
#   tbl <- map(.x = 1:length(rst), .f = function(k){
#     rst[[k]] %>% 
#       as.points() %>% 
#       as.data.frame()
#   }) %>% 
#     bind_cols() %>% 
#     as_tibble()
#   
#   tbl_all <- tbl
#   names(tbl_all) <- paste0('y_', 1980:2017)
#   tbl_all <- cbind(crd, tbl_all)
#   tbl_all <- as_tibble(tbl_all)
#   cat('Done\n')
#   return(tbl_all)
#   
# }
# slp <- map(.x = 1:nrow(tbl), .f = function(k){
#   
#   cat('Start ', k, '\n')
#   slp <- tbl[k,] %>% 
#     as.numeric() %>% 
#     ts() %>%
#     sens.slope() %>% 
#     .$estimate %>% 
#     as.numeric()
#   return(slp)
#   
# }) %>% unlist()
# 
# rsl <- crd %>% mutate(slope = slp)



