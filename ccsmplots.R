#comparison plots
library(ggplot2)

#looking at cdfs of everything

ggplot(size = 2) + stat_ecdf(aes(x = pre)) + 
     stat_ecdf(aes(x = prect.cal), color = 'red') + 
     stat_ecdf(aes(x = prect.pred), color = 'blue') +
     stat_ecdf(aes(x = prect.pred[121780:122880]), color = 'green') +
     theme_bw()

ggplot(size = 2) + geom_density(aes(x = pre)) + 
     geom_density(aes(x = prect.cal), color = 'red') + 
     geom_density(aes(x = prect.pred), color = 'blue') +
     geom_density(aes(x = prect.pred[121780:122880]), color = 'green') +
     theme_bw()

# the present day control and trace long term are very similar, but interestingly enough
# the trace from the past is a little bit different. maybe this has something to do with the
# transient vs equilibrium configurations of each simulation?\


#now with temperature
ggplot(size = 2) + stat_ecdf(aes(x = tmn)) + 
     stat_ecdf(aes(x = tsmn.cal), color = 'red') + 
     stat_ecdf(aes(x = tsmn.pred), color = 'blue') +
     stat_ecdf(aes(x = tsmn.pred[121780:122880]), color = 'green') +
     theme_bw()

ggplot(size = 2) + geom_density(aes(x = tmn)) + 
     geom_density(aes(x = tsmn.cal), color = 'red') + 
     geom_density(aes(x = tsmn.pred), color = 'blue') +
     geom_density(aes(x = tsmn.pred[121780:122880]), color = 'green') +
     theme_bw()
# ahhh here's its different, the calibration and the observations both show warmer conditions
# the two trace ones are similar but colder. this is because they don't have global warming!


ggplot(size = 2) + stat_ecdf(aes(x = tmx)) + 
     stat_ecdf(aes(x = tsmx.cal), color = 'red') + 
     stat_ecdf(aes(x = tsmx.pred), color = 'blue') +
     stat_ecdf(aes(x = tsmx.pred[121780:122880]), color = 'green') +
     theme_bw()

ggplot(size = 2) + geom_density(aes(x = tmx)) + 
     geom_density(aes(x = tsmx.cal), color = 'red') + 
     geom_density(aes(x = tsmx.pred), color = 'blue') +
     geom_density(aes(x = tsmx.pred[121780:122880]), color = 'green') +
     theme_bw()

ggplot(size = 2) + stat_ecdf(aes(x = tmp)) + 
     stat_ecdf(aes(x = trefht.cal), color = 'red') + 
     stat_ecdf(aes(x = trefht.pred), color = 'blue') +
     stat_ecdf(aes(x = trefht.pred[121780:122880]), color = 'green') +
     theme_bw()

ggplot(size = 2) + geom_density(aes(x = tmp)) + 
     geom_density(aes(x = trefht.cal), color = 'red') + 
     geom_density(aes(x = trefht.pred), color = 'blue') +
     geom_density(aes(x = trefht.pred[121780:122880]), color = 'green') +
     theme_bw()

# what about clouds

ggplot(size = 2) + stat_ecdf(aes(x = cldtot.cal), color = 'red') + 
     stat_ecdf(aes(x = cld), color = 'black') + 
     stat_ecdf(aes(x = cldtot.pred), color = 'blue') +
     stat_ecdf(aes(x = cldtot.pred[121780:122880]), color = 'green') +
     theme_bw()

ggplot(size = 2) + geom_density(aes(x = cld)) + 
     geom_density(aes(x = cldtot.cal), color = 'red') + 
     geom_density(aes(x = cldtot.pred), color = 'blue') +
     geom_density(aes(x = cldtot.pred[121780:122880]), color = 'green') +
     theme_bw()
# yes I do have to bias correct clouds
