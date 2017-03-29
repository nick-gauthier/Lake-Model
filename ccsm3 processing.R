## Downscaling with CDF-t
library(magrittr)
library(raster)
library(ncdf4)
library(ggplot2)

#import Trace
trace.import <- function(var, level = 1, seasonal = F){
     
     if(seasonal = F) {
          dir = '~/gdrive/Climate Data/CCSM3/Trace-21k/Monthly'
          var.dir <- paste(dir, var, sep = '/')
          
          list.files(var.dir, full.names = T) %>%        # get a list of all GCM outputs for the given variable
               lapply(brick, level = level) %>%                          # import GCM files as raster bricks
               lapply(raster::extract, lake.pt, method = 'bilinear') %>%  # extract over lake, bilinearly interpolated from neighboring GCM cells
               unlist                                     # combine into a single series
     } else {
          dir = '~/gdrive/Climate Data/CCSM3/Trace-21k/Seasonal'
     }
    
}


prect.pred <- (trace.import('PRECC') + trace.import('PRECL')) * 2.592e+09  # calc total prec and convert to mm/mo
tsmn.pred <- trace.import('TSMN') - 273.15
tsmx.pred <- trace.import('TSMX') - 273.15
cldtot.pred <- trace.import('CLDTOT') * 100

u.pred <- trace.import('U', level = 26, dir = '~/gdrive/Climate Data/CCSM3/Trace-21k/Seasonal')
v.pred <- trace.import('V', level = 26, dir = '~/gdrive/Climate Data/CCSM3/Trace-21k/Seasonal')
wnd60.pred <- sqrt(u.pred ^ 2 + v.pred ^ 2) * .614853515 # convert to 10m height using wind profile power law


by50 <- function(pred){pred %>% seq_along %>% divide_by(600) %>% ceiling %>% split(pred,.)}
