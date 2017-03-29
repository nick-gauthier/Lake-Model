# calibration station

p.obs <- p.obs[517:600]
et0.obs <- et0.obs[193:276]
wl <- read.csv('~/gdrive/Gygaia Projects/Marmara/wl_1.txt', sep = '')[,2]
plot(scale(wl), type = 'l')
lines(scale(p.obs - et0.obs), col ='red')

plot(wl-67.82, type = 'l')


cal <- gr2m(rep(p.obs,3),rep(et0.obs,3),150 ,1.2) #%>% data.frame(volume = .) %>% predict.gam(vol.to.depth, .)
plot(cal, type = 'l')

lines(rep(wl,3), col = 'red')

lines(scale(p.obs - et0.obs), col = 'red')
lines(scale(wl), col ='blue')



wl <- d[10:93,3] * 1000000


mc.test <- mapply(gr2m, X2 = runif(400, .5, 1.5), MoreArgs = list(P = p.obs, E = et0.obs, X1=150))

qplot(x = 1:1356, y = vols.out, geom = 'point') + geom_smooth()


wl <- read.csv('~/gdrive/Gygaia Projects/Marmara/wl_2.txt', sep = '')
qplot(x = volume, y = WL, data = wl, geom = 'point') 

gam.depth <- predict(vol.to.depth, data.frame(volume=wl$volume))
ggplot() + geom_point(aes(x = wl$volume, y = wl$WL)) + geom_line(aes(x = wl$volume, y = gam.depth))

#######
#Observed precipitation experimentation
p.obs<- data.frame(date = seq(1939, 2001-1/12, 1/12), p.obs = read.csv('salihli_p.csv')[,5] / 10)
p.obs[is.na(p.obs)] <- 0

salihli.pt <- c(28.0, 38.6) %>% matrix(nrow = 1) %>% SpatialPoints # or c(28.14, 38.48) c(28.0, 38.6)
cru <- data.frame(date = seq(1901,2014-1/12,1/12), cru = (brick('~/gdrive/climate data/cru/CRU TS 3.22/cru_ts3.22.1901.2013.pre.dat.nc') %>% raster::extract(salihli.pt, method = 'bilinear'))[1,]) 

dat <- merge(p.obs, cru, by = 'date', all = F) 

ggplot(dat, aes(x = date, alpha = .7)) + 
  geom_line(aes(y = cru)) +
  geom_line(aes(y = p.obs), color = 'red') +
  theme_bw()


cru <- data.frame(date = seq(1901,2014-1/12,1/12), 
                  pre = (brick('~/gdrive/climate data/cru/CRU TS 3.22/cru_ts3.22.1901.2013.pre.dat.nc') %>% raster::extract(salihli.pt, method = 'bilinear'))[1,],
                  pet = (brick('~/gdrive/climate data/cru/CRU TS 3.22/cru_ts3.22.1901.2013.pet.dat.nc') %>% raster::extract(salihli.pt, method = 'bilinear'))[1,] * 30) 
ggplot(cru, aes(x = date)) + geom_line(aes(y = pre)) + geom_line(aes(y = pet), color = 'red') + theme_bw()
qplot(x = date, y = pre - pet, data = cru, geom = 'point') + geom_smooth()

vols.out <- gr2m(cru$pre, cru$pet, 200, .75)
qplot(x = cru$date, y = vols.out, geom = 'point') + geom_smooth() + scale_x_continuous(breaks = seq(1900,2010,10))
depth.out = predict.gam(vol.to.depth, data.frame(volume = vols.out))
qplot(x = cru$date[826:1221], y = depth.out[826:1221], geom = 'point', color = 'red') + geom_smooth() + scale_x_continuous(breaks = seq(1900,2010,10)) + geom_point(aes(y = wl), color = 'black')



library(dplyr)
nao <- read.csv('~/gdrive/climate data/norm.nao.monthly.b5001.current.ascii', header = F,sep ='')
nao$V2 <- nao$V2 / 12 + 1/12
nao <- nao %>% mutate(date = V1 + V2, nao = V3>0)
test <- merge(cbind(date = nao$date, as.factor(sign(nao$V3))), cbind(date = cru$date, vols.out))

qplot(x = date, y = vols.out, data = test, geom = 'point', color = factor(V2) ) + scale_x_continuous(breaks = seq(1900,2010,10))


b30.pc <- (brick('~/gdrive/Climate Data/b30 simulation ccsm3/b30.031.cam2.h0.PRECC.0800-01_cat_0879-12.nc') %>% 
          raster::extract(salihli.pt, method = 'bilinear'))[1,] %>% 
        multiply_by(60 * 60 * 24 * 30 * 1000)
b30.pl <- (brick('~/gdrive/Climate Data/b30 simulation ccsm3/b30.031.cam2.h0.PRECL.0800-01_cat_0879-12.nc') %>% 
             raster::extract(salihli.pt, method = 'bilinear'))[1,] %>% 
  multiply_by(60 * 60 * 24 * 30 * 1000)
b30 <- b30.pl + b30.pc


ggplot() + stat_ecdf(aes(x = pt.cal)) + 
  stat_ecdf(aes(x = b30), color = 'red') + 
  stat_ecdf(aes(x = dat$cru), color = 'blue') +
  stat_ecdf(aes(x = dat$p.obs), color = 'green') +
  stat_ecdf(aes(x = pt.pred), color = 'yellow') +
  theme_bw()
  
  
qplot(x = cru[457:1200,2], y = p.obs$p.obs, geom = 'point')
qplot(x = p.obs, y = cru, data = dat, geom = 'point') + geom_smooth()
sqrt(mean((dat$p.obs - dat$cru)^2, na.rm = T))


cru$pet
test <- merge(cru, har, by = 'date', all = F)

ggplot(test, aes(x = date, alpha = .7)) + geom_line(aes(y = har)) + geom_line(aes(y = pet), color = 'red')
qplot(x = har, y = pet, data = test, geom = 'point')

cru.temp <- data.frame(date = seq(1901,2014-1/12,1/12), 
                  tmin = (brick('~/gdrive/climate data/cru/CRU TS 3.22/cru_ts3.22.1901.2013.tmn.dat.nc') %>% 
                           raster::extract(salihli.pt, method = 'bilinear'))[1,],
                  tmax = (brick('~/gdrive/climate data/cru/CRU TS 3.22/cru_ts3.22.1901.2013.tmx.dat.nc') %>% 
                           raster::extract(salihli.pt, method = 'bilinear'))[1,],
                  seasons = c(rep('Winter',3), rep('Summer', 6), rep('Winter', 3))) 

har <- hargreaves(ts(cru.temp$tmin, start = c(1901, 1), frequency = 12), 
                      ts(cru.temp$tmax, start = c(1901, 1), frequency = 12), 
                      lat = 38.48, na.rm = T) %>% c %>% cbind(har = ., date = seq(1901,2014-1/12,1/12))# convert to vector from ts at end


sqrt(mean((test$pet - test$har)^2, na.rm = T))



trefht <- (brick('~/gdrive/Climate Data/b30 simulation ccsm3/b30.031.cam2.h0.TREFHT.0800-01_cat_0879-12.nc') %>% raster::extract(salihli.pt, method = 'bilinear'))[1,] %>% subtract(273.15)
ts <- (brick('~/gdrive/Climate Data/b30 simulation ccsm3/b30.031.cam2.h0.TS.0800-01_cat_0879-12.nc') %>% raster::extract(salihli.pt, method = 'bilinear'))[1,] %>% subtract(273.15)
plot(ts, trefht-ts)
temp.correction <- lm(trefht ~ ts)
plot(temp.correction$model)
lines(trefht - 1, col = 'red')
plot(trefht, type = 'l')
lines(ts - 1, col = 'red')
tsmn <- (brick('~/downloads/b30.048.cam2.h0.TSMN.0400-01_cat_0499-12.nc') %>% raster::extract(salihli.pt, method = 'bilinear'))[1,] %>% subtract(273.15)
ts <- (brick('~/downloads/b30.048.cam2.h0.TS.0400-01_cat_0499-12.nc') %>% raster::extract(salihli.pt, method = 'bilinear'))[1,] %>% subtract(273.15)
tsmx <- (brick('~/downloads/b30.048.cam2.h0.TSMX.0400-01_cat_0499-12.nc') %>% raster::extract(salihli.pt, method = 'bilinear'))[1,] %>% subtract(273.15)
trefhtmn <- (brick('~/downloads/b30.048.cam2.h0.TREFMNAV.0400-01_cat_0499-12.nc') %>% raster::extract(salihli.pt, method = 'bilinear'))[1,] %>% subtract(273.15)
trefhtmx <- (brick('~/downloads/b30.048.cam2.h0.TREFMXAV.0400-01_cat_0499-12.nc') %>% raster::extract(salihli.pt, method = 'bilinear'))[1,] %>% subtract(273.15)
trefht <- (brick('~/downloads/b30.048.cam2.h0.TREFHT.0400-01_cat_0499-12.nc') %>% raster::extract(salihli.pt, method = 'bilinear'))[1,] %>% subtract(273.15)

qplot(trefht,trefhtmx)
cor(trefhtmn,tsmn)
test <- cbind(tsmn,tsmx,ts,trefhtmn,trefhtmx,trefht)
library(GGally)
ggpairs(test)

#### naturalizing lake levels
wl <- read.csv('~/gdrive/Gygaia Projects/Marmara/wl_1.txt', sep = '')[4:387,2] %>% data.frame(wl = ., Date=seq(1970,2002-1/12,1/12))
wl2 <- read.csv('~/gdrive/Gygaia Projects/Marmara/wl_2.txt', sep = '')[,2:3]
library(mgcv)
depth.to.vol <- gam(volume ~ WL, data=wl2)
wl$volume <- predict.gam(depth.to.vol, data.frame(WL = wl$wl)) * 1000000

kc <- read.csv('~/gdrive/gygaia projects/lake model/Kum Cay Flow/kumcay_monthly.csv', col.names = c('ID','Date', 'Q'))[,2:3]
kc$Q <- kc$Q * 60 * 60 * 24 * 30
kc <- read.csv('~/gdrive/gygaia projects/lake model/Kum Cay Flow/kumcay_monthly_avg.csv', col.names = c('ID', 'Q'))[,2]
kc <- kc * 60 * 60 * 24 * 30

dat <- merge(kc, wl)

ggplot(dat, aes(x = Date)) + geom_line(aes(y = volume)) + geom_line(aes(y = volume - Q), color = 'red') + theme_bw()

irr <- c(0,0,0,0,0,15* 60 * 60 * 24 * 30, 15 * 60 * 60 * 24 * 30,0,0,0,0,0)

ggplot(dat, aes(x = Date)) + 
  geom_line(aes(y = volume)) + 
  geom_line(aes(y = volume - Q), color = 'red') +
  geom_line(aes(y = volume - Q + irr), color = 'red') +
  theme_bw()


wl <- read.csv('~/gdrive/Gygaia Projects/Marmara/wl_1.txt', sep = '')[1:363,2]
q.in <- rnorm(408, q.in.avg, q.in.sd) * 60 * 60 * 24 * 30
q.out <- rnorm(408, q.out.avg, q.out.sd) * 60 * 60 * 24 * 30

vols.out <- gr2m(pre[781:1188], coredata(pen)[781:1188], 150, .8)
depth.out = predict.gam(vol.to.depth, data.frame(volume = vols.out))
qplot(x = cru[781:1188,1], y = depth.out, geom = 'line', color = 'red') + geom_smooth() + geom_line(aes(y = c(rep(NA,45), wl)), color = 'black')

vols.out <- gr2m(p.obs[325:732,2], et0.obs, 150, .8)
depth.out = predict.gam(vol.to.depth, data.frame(volume = vols.out))
qplot(x = cru[781:1188,1], y = depth.out, geom = 'line', color = 'red') + geom_smooth() + geom_line(aes(y = c(rep(NA,45), wl)), color = 'black')


vols.out <- gr2m(cru$pre, cru$pet, 150, .75)
vols.out %>% seq_along %>% divide_by(12) %>% ceiling %>% split(vols.out,.) %>% lapply(sum) %>% unlist %>% mean %>% divide_by(1000000)
# this is calibrating just on runoff into the basin







#### data from that paper
dsi.dat <- data_frame(
  year = 1980:1991, 
  incoming = c(283.2,327.1,384.5,185.3,355.6,153.8,266.4,309.2,118.9,65,78.8,69.8),
  irrigation = c(193.4,179.2,145.8,117.8,235.2,114.7,180.4,222,115.7,41,20.5,0),
  flood = c(15,52.7,116.5,26.2,0,0,0,0,0,0,0,0),
  evap = c(64.3,73.2,52.7,54.4,63.2,60,63.4,64.8,59.1,52,52.4,48.7),
  leakage = c(7.2,4.2,29.3,25.4,5,12.6,12.1,3.9,0,0,0,0),
  total = c(279.9,309.3,344.3,223.8,303.7,187.3,255.9,290.7,174.8,93,72.9,48.7),
  max = c(78.09,78.09,78.31,76.11,78.56,76.36,77.19,78.17,76.15,74.72,74.31,74.34),
  min = c(73.69,73.61,73.88,73.76,73.61,73.89,73.97,74.35,73.62,72.56,72.56,73.21)) %>%
  mutate(total.lag = lag(total)) %>%
  dplyr::select(-year)
ggpairs(dsi.dat)


dat.mon <- cru[949:1092,3] 
dat <- dat.mon %>% seq_along %>% divide_by(12) %>% ceiling %>% split(dat.mon,.) %>% lapply(sum) %>% unlist
dat
plot(dat * predict(vol.to.area,data.frame(volume = dsi.dat$total * 1000000)) * .001/1000000 ,dsi.dat$evap)
