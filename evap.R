#Figuring out evapotranspiration calculations

library(Evapotranspiration)

data("climatedata")
climatedata %>% tbl_df

cru.dir <- 
  
tmp <- brick('~/gdrive/climate data/cru/CRU TS 3.22/cru_ts3.22.1901.2013.tmp.dat.nc') %>% 
  extract(salihli.pt, method = 'bilinear') %>% c

tmn <- brick('~/gdrive/climate data/cru/CRU TS 3.22/cru_ts3.22.1901.2013.tmn.dat.nc') %>% 
  extract(salihli.pt, method = 'bilinear') %>% c

tmx <- brick('~/gdrive/climate data/cru/CRU TS 3.22/cru_ts3.22.1901.2013.tmx.dat.nc') %>% 
  raster::extract(salihli.pt, method = 'bilinear') %>% c

pre <- brick('~/gdrive/climate data/cru/CRU TS 3.22/cru_ts3.22.1901.2013.pre.dat.nc') %>% 
  raster::extract(salihli.pt, method = 'bilinear') %>% c

cld <- brick('~/gdrive/climate data/cru/CRU TS 3.22/cru_ts3.22.1901.2013.cld.dat.nc') %>% 
  raster::extract(salihli.pt, method = 'bilinear') %>% c

vap <- brick('~/gdrive/climate data/cru/CRU TS 3.22/cru_ts3.22.1901.2013.vap.dat.nc') %>% 
  raster::extract(salihli.pt, method = 'bilinear') %>% c

dat <- data_frame(Year = rep(1901:2013, each = 12), 
                  Month = rep(1:12, 113),
                  Day = 16,
                  Hour = 12,
                  Tmin.daily = tmn,
                  Tmax.daily = tmx,
                  Pre = pre,
                  Cd.daily = round(cld * .08),
                  Vp.subdaily = vap) %>%
  mutate(vap.calc = 6.108 * exp(17.27 * Tmin.daily / (237.3 + Tmin.daily))) %>%
  group_by(Year) %>% mutate(PA = sum(Pre))
  
dat
data("constants")
constants$PA <- 676.5194
constants$Elev <- 100
constants$lat <- 38.6
constants$lat_rad <- (38.6 * pi) / 180

test <- ReadInputs(dat, constants, timestep = "subdaily", stopmissing = c(99, 30))
test

funname <- 'MortonCRWE'
class(test) <- funname

qplot(Vap, vap.calc, data = dat, geom = 'point')

mort <- ET(test,constants, est = 'shallow lake', solar = 'sunshine hours', Tdew = F)

#The whole tdew vap thing is just messed up. this package is just unuseable as it

#lets go to spei
library(SPEI)



u <- read.table('~/gdrive/Climate Data/cru/CRU 10 Minute Climatology/grid_10min_wnd.dat', col.names= c('Lat', 'Lon', month.abb))
coordinates(u) <- c('Lon','Lat')
proj4string(u) <- CRS("+init=epsg:4326")
wrld <- raster(nrows = 900, ncols = 2160, ymn = -60, ymx = 90, xmn = -180, xmx = 180) %>% crop(bounds)
test <- rasterize(u,wrld)
bounds <- extent(25,30,35,40)
test.em <- crop(test[[2:13]],bounds)
library(rasterVis)
levelplot(test.em)
u2 <- extract(test.em, salihli.pt, method = 'bilinear') %>% c
plot(u2)

dat <- data_frame(Year = rep(1901:2013, each = 12), 
                  Month = rep(1:12, 113),
                  tmp = tmp,
                  tmn = tmn,
                  tmx = tmx,
                  pre = pre,
                  cld = cld,
                  vap = vap)
tho <- thornthwaite(tmp, 38.62) %>% zoo
har <- hargreaves(tmn, tmx, lat = 38.62, Pre = pre) %>% zoo
har.nopre <- hargreaves(tmn, tmx, lat = 38.62) %>% zoo
pen <- penman(tmn, tmx, rep(u2, 113) , lat = 38.6, CC = cld, z = 100) %>% zoo
cru.pet <- zoo(cru$pet, order.by = index(pen))
pet <- merge(har, pen)

library(ggplot2)
autoplot(pet, facets = NULL) +theme_bw()
