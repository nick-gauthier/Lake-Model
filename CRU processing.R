#Import and preprocessing of CRU data

library(raster)
library(dplyr)
library(ncdf4)
library(ggplot2)
library(SPEI)

lake.pt <- c(28.00, 38.62) %>% matrix(nrow = 1) %>% SpatialPoints # create a point over the Lake to extract data
bounds <- extent(25,30,35,40)


getbrick <- function(x){ brick(x) %>% raster::extract(lake.pt, method = 'bilinear') %>% c } # function to import a brick and extract data using a point, then vectorizing it
tmn = getbrick('Data/cru_ts3.22.1901.2013.tmn.dat.nc')
tmx = getbrick('Data/cru_ts3.22.1901.2013.tmx.dat.nc')
pre = getbrick('Data/cru_ts3.22.1901.2013.pre.dat.nc')
cld = getbrick('Data/cru_ts3.22.1901.2013.cld.dat.nc')

wnd <- read.table('Data/grid_10min_wnd.dat', col.names= c('Lat', 'Lon', month.abb))
wnd <- SpatialPointsDataFrame(wnd[2:1], wnd[3:14], proj4string = CRS("+init=epsg:4326")) %>%
     crop(bounds) %>%
     rasterize(raster(bounds, nrows = 30, ncols = 30)) %>%
     subset(2:13) %>%
     raster::extract(lake.pt, method = 'bilinear') %>% c %>% rep(113) %>% multiply_by(.794414732) # power law wind profile

pet <- penman(tmn, tmx, wnd , lat = 38.62, CC = cld, z = 100) %>% c

cru <- data_frame(year = rep(1901:2013, each = 12), month = rep(1:12, 113), 
                  tmn, tmx, pre, cld, wnd, pet) %>%
     mutate(date = year + (month - 1) /12 )

rm(tmn, tmx, pre, cld, wnd, pet)

ggplot(cru, aes(x = date)) + geom_point(aes(y = tmn), color = 'blue') + 
     geom_point(aes(y = tmx), color = 'red') +
     theme_bw()

qplot(x = date, y = pre - pet, data = cru, geom = 'line') + theme_bw()



