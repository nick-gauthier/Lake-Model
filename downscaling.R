# Downscaling with CDF-t


by.cen <- function(pred){pred %>% seq_along %>% divide_by(1200) %>% ceiling %>% split(pred,.)}

prect.cen <- by.cen(prect.pred[1:122400])
tsmn.cen <- by.cen(tsmn.pred[1:122400])
tsmx.cen <- by.cen(tsmx.pred[1:122400])

calc.cdft <- function(pred, cal, obs){ }
     #CDFt(obs, cal, pred)$DS}

p.recons <- lapply(prect.cen, calc.cdft, cal = prect.cal, obs = pre) %>% unlist

qplot(y = test$DS, alpha = .7)
#p.recons.2 <- CDFt(pre, prect.cal, prect.pred[1:122400])
qplot(y = p.recons.2$DS, alpha = .7)
qplot(y = prect.pred[1:122400], alpha = .7)
qplot(y = pre, alpha = .7)

spring <- c(F,F,T,T,T,F,F,F,F,F,F,F)
summer <- c(F,F,F,T,T,T,T,T,T,F,F,F)
fall <-  c(F,F,F,F,F,F,F,F,T,T,T,F)
winter <- c(T,T,T,F,F,F,F,F,F,T,T,T)

calc.cdf.season <- function(pred, cal, obs){
    # ct.winter <- CDFt(obs[winter], cal[winter], pred[winter])$DS
     #ct.spring <- CDFt(obs[spring], cal[spring], pred[spring])$DS
     #ct.summer <- CDFt(obs[summer], cal[summer], pred[summer])$DS
     #ct.fall <- CDFt(obs[fall], cal[fall], pred[fall])$DS
     
     recon.out <- 0
     for(i in seq(1, 300, 3)){
          win <- ct.winter[i:(i+2)]
          spr <- ct.spring[i:(i+2)]
          sum <- ct.summer[i:(i+2)]
          fal <- ct.fall[i:(i+2)]
          recon.out <- c(recon.out, win[1:2],spr,sum,fal,win[3])
     }
     return(recon.out[2:1201])
}

p.recon <- lapply(prect.cen, calc.cdf.season, cal = prect.cal, obs = pre) %>% unlist
qplot(y = p.recon, alpha = .7)
tn.recon <- lapply(tsmn.cen, calc.cdf.season, cal = tsmn.cal, obs = tmn) %>% unlist
qplot(y = tn.recon, alpha = .7)
qplot(y = tsmn.pred, alpha = .7)
#qplot(y = CDFt(tmn,tsmn.cal,tsmn.pred)$DS, alpha = .4)

tx.recon <- lapply(tsmx.cen, calc.cdf.season, cal = tsmx.cal, obs = tmx) %>% unlist
qplot(y = tx.recon, alpha = .7)
qplot(y = tsmx.pred, alpha = .7)
#ggplot2::qplot(y = CDFt(tmx,tsmx.cal,tsmx.pred)$DS, alpha = .4)
qplot(y = tmx, alpha = .7)


#ggplot2::qplot(y = CDFt(pre,prect.cal,prect.pred)$DS, alpha = .4)
#ggplot2::qplot(y = CDFt.threshold.Gamma(pre,prect.cal,prect.pred, thObs = .2)$DS, alpha = .4)



# now we test the differences from downscaling trefht and calculating extrems, or calculating extremes and downscaling them
tref.to.trefmn <- gam(tmn ~ s(tmp))
tref.to.trefmx <- gam(tmx ~ s(tmp))
plot(tref.to.trefmn)
plot(trmn,predict(tref.to.trefmn, data.frame(tmp = tref), type = 'response')-trmn)
#errors arent that bad to start



tref.recon <- CDFt(tmp,trefht.cal, trefht.pred)$DS
tmn.recon.1 <- predict(tref.to.trefmn, data.frame(tmp = tref.recon), type = 'response')
tmx.recon.1 <- predict(tref.to.trefmx, newdata = data.frame(tmp = tref.recon), type = 'response')

tmn.recon.2 <- CDFt(predict(tref.to.trefmn, data.frame(tmp = tmp), type = 'response'),
                  predict(tref.to.trefmn, data.frame(tmp = trefht.cal), type = 'response'),
                  predict(tref.to.trefmn, data.frame(tmp = trefht.pred), type = 'response'))$DS
tmx.recon.2 <- CDFt(predict(tref.to.trefmx, data.frame(tmp = tmp), type = 'response'),
                    predict(tref.to.trefmx, data.frame(tmp = trefht.cal), type = 'response'),
                    predict(tref.to.trefmx, data.frame(tmp = trefht.pred), type = 'response'))$DS

qplot(tmn.recon.1,tmn.recon.2)
# so in the graph above, its one to one until we get to around -10degrees, in which case
# the first recon is about one degree warmer. 
qplot(tmx.recon.1, tmx.recon.2)
# in this graph  its a 1-1 fit until around 3 degrees, then drops and is flat, so the weirdness is happening
# in the second method where you downscale the the transformed series
#stick with 1 then
plot(tmn.recon.1)
plot(tmn.recon.2)

#ok so doing this for real now
prec.recon <- CDFt(pre, prect.cal, prect.pred)$DS
trefht.recon <- CDFt(tmp,trefht.cal, trefht.pred)$DS
tmn.recon <- predict(tref.to.trefmn, data.frame(tmp = trefht.recon), type = 'response') %>% c
tmx.recon <- predict(tref.to.trefmx, data.frame(tmp = trefht.recon), type = 'response') %>% c

pet.recon <- hargreaves(tmn.recon, tmx.recon, lat = 38.6, Pre = prec.recon) %>% c
plot(pet.recon)
plot(prec.recon)


winter <- c(T,T,T,T,F,F,F,F,F,F,T,T)
summer <- c(F,F,F,F,T,T,T,T,T,T,F,F)
###### code for pasting together summer and winter series
library(CDFt)
interleave.seasons <- function(winter, summer){
     out <- winter[1:4]
     for(i in seq(1, length(winter), by = 6)){
          out <- c(out, summer[i:(i+5)], winter[(i+4):(i+9)])
     }
     return(out[!is.na(out)])
}

test <- 1:48
interleave.seasons(test[winter], test[summer])


prec.recon <- interleave.seasons(CDFt(pre[winter], prect.cal[winter], prect.pred[winter])$DS,
                                 CDFt(pre[summer], prect.cal[summer], prect.pred[summer])$DS)
trefht.recon <- interleave.seasons(CDFt(tmp[winter], trefht.cal[winter], trefht.pred[winter])$DS,
                                 CDFt(tmp[summer], trefht.cal[summer], trefht.pred[summer])$DS)
tmn.recon <- predict(tref.to.trefmn, data.frame(tmp = trefht.recon), type = 'response') %>% c
tmx.recon <- predict(tref.to.trefmx, data.frame(tmp = trefht.recon), type = 'response') %>% c

pet.recon <- hargreaves(tmn.recon, tmx.recon, Ra = solin, Pre = prec.recon) %>% c
pet.recon2 <- hargreaves(tmn.recon, tmx.recon, lat = 38.6, Pre = prec.recon) %>% c
plot(pet.recon,pet.recon2)

