## gr2m rainfall runoff model

gr2m <- function(P, E, X1, X2){
          phi <- tanh(P / X1)
          psi <- tanh(E / X1)
          
          S1 <- (S + X1 * phi) / (1 + phi * (S / X1)) # soil wetness change from rainfall
          P1 <- P + S - S1 # excess rain
          S2 <- (S1 * (1 - psi)) / (1 + psi * (1 - (S1 / X1))) # soil wetness change from evap
          S <<- S2 / (1 + (S2 / X1)^3)^(1/3)  # new soil wetness level
          P2 <- S2 - S # percolation
          P3 <- P1 + P2 # rainfall reaching the routing reservoir
          R1 <- R + P3 # level in the routing reservoir
          R2 <- X2 * R1 # new level in the tank after water loss
          Q <- R2^2 / (R2 + 60 ) # drainage from reservoir
          R <<- R2 - Q # new level in tank
          
          return(Q)
}


lake.morph.dam <- read.csv('Data/depthcurve_dam.csv') # data output from flood-fill algorithm in GRASS
lake.morph.nodam <- read.csv('Data/depthcurve_nodam.csv') # data output from flood-fill algorithm in GRASS



lake.sim <- function(P, E, X1 = 150, X2 = .85, calib = F){
     
     sim.length <- length(P)
     
     basin.area <- 247725900  # surface area of Lake Marmara watershed in sq. m
     
     S <- 0
     R <- 0
     volume <- 100000000
     
     vols.out <- c(1:sim.length)
     runoff.out <- vols.out
     if(calib == T) {
          q.in <- rnorm(sim.length, q.in.avg, q.in.sd) * 60 * 60 * 24 * 30
          q.out <- rnorm(sim.length, q.out.avg, q.out.sd) * 60 * 60 * 24 * 30
          vol.to.depth <- gam(depth ~ s(volume, k = 45), data = lake.morph.dam) # function for converting volume to depth
          vol.to.area <- gam(area ~ s(volume, k = 45), data = lake.morph.dam) # function for converting volume to area
     } else {
          q.in <- rep(0, sim.length)
          q.out <- rep(0, sim.length)
          vol.to.depth <- gam(depth ~ s(volume, k = 45), data = lake.morph.nodam) # function for converting volume to depth
          vol.to.area <- gam(area ~ s(volume, k = 45), data = lake.morph.nodam) # function for converting volume to area
     }
     
     environment(gr2m) <- environment()
     for (i in 1:sim.length){
          lake.area <- predict.gam(vol.to.area, list(volume = volume))
          basin.area.nolake <- basin.area - lake.area
          
          atmos.ex <- P[i] - E[i] # multiply by open water coefficient kw
          
          runoff <- gr2m(P[i], E[i], X1, X2)

          volume <- volume + runoff *.001 * basin.area.nolake + atmos.ex * .001 * lake.area + q.in[i]
        
          if(lake.area > 3500000){ volume <- volume - q.out[i]}
          if(volume < 0){volume <- 0}
          if(volume > 290000000){volume <- 290000000}
          
          vols.out[i] <- volume
          runoff.out[i] <- runoff *.001 * basin.area.nolake / 1000000
     }
     depth.out <- predict.gam(vol.to.depth, list(volume = vols.out)) %>% c
     area.out <- predict.gam(vol.to.area, list(volume = vols.out)) %>% c
     dat.out <- data.frame(volume = vols.out, depth = depth.out, area = area.out, runoff = runoff.out)
     return(dat.out)
}

wlf <- lake.sim(cru$pre, cru$pet, X2=.55)
plot(wlf$depth + 72.06)
plot(wlf$volume / 1000000, type = 'l')
plot(wlf$area)

plot(wlf$runoff)
mean(wlf$runoff %>% seq_along %>% divide_by(12) %>% ceiling %>% split(wlf$runoff,.) %>% lapply(sum) %>% unlist)
wlf


q.in.min <- cbind(month = c(1,6,12), flow = c(8,.7, 8)) %>% spline(n=12, method = 'p')
q.in.max <- cbind(month = c(1,6,12), flow = c(29,.9, 29)) %>% spline(n=12, method = 'p')
q.in.sd <- (q.in.max$y - q.in.min$y) / 4
q.in.avg <- (q.in.max$y + q.in.min$y) / 2

plot(rnorm(240, q.in.avg,q.in.sd), type = 'l')

q.in2.min <- cbind(month = c(1,6,12), flow = c(11,.02, 11)) %>% spline(n=12, method = 'p')
q.in2.max <- cbind(month = c(1,6,12), flow = c(34,.08, 34)) %>% spline(n=12, method = 'p')
q.in2.sd <- (q.in2.max$y - q.in2.min$y) / 4
q.in2.avg <- (q.in2.max$y + q.in2.min$y) / 2


q.out.min <- cbind(month = c(1,6,12), flow = c(.7, 8, .7)) %>% spline(n=12)
plot(q.out.min)
q.out.max <- cbind(month = c(1,6,12), flow = c(2, 24, 2)) %>% spline(n=12)
q.out.sd <- (q.out.max$y - q.out.min$y) / 4
q.out.avg <- (q.out.max$y + q.out.min$y) / 2
