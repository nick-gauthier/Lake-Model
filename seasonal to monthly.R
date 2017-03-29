##### cleaning up the kumcay monthly flow data from Yildirim et al 2004
library(dplyr)
library(ggplot2)
library(reshape2)

setwd('~/gdrive/Gygaia Projects/Lake Model/Kum Cay Flow/')

winter <- read.csv('Kumcay winter.txt', sep = '') %>%
  tbl_df %>% transmute(Q = round(y,2)) %>% bind_cols(data.frame(date = 1964:1999))
spring <- read.csv('Kumcay spring.txt', sep = '') %>%
  tbl_df %>% transmute(Q = round(y,2)) %>% bind_cols(data.frame(date = 1964:1999 + 1/4))
summer <- read.csv('Kumcay summer.txt', sep = '') %>%
  tbl_df %>% transmute(Q = round(y,2)) %>% bind_cols(data.frame(date = 1964:1999 + 2/4))
autumn <- read.csv('Kumcay autumn.txt', sep = '') %>%
  tbl_df %>% transmute(Q = round(y,2)) %>% bind_cols(data.frame(date = 1964:1999 + 3/4))

flows <- bind_rows(winter, spring, summer, autumn) %>% arrange(date)

flows.mon <- rep(flows$Q, each = 3)
flows.mon[flows.mon < 0] <- 0

interp <- spline(flows$date, flows$Q, xout = seq(1964, 2000-1/12,1/12))
interp[[2]][interp[[2]] < 0] <- 0

ggplot() + geom_line(aes(x= flows$date, y = flows$Q)) + theme_bw() + geom_line(aes(x = interp$x, y = interp$y), color = 'red', alpha = .5)+
  geom_line(y = flows.mon)

interp.lin <- approx(flows$date, flows$Q, interp$x)

ggplot() + geom_line(aes(x= flows$date, y = flows$Q)) + 
  geom_line(aes(x = interp$x, y = interp$y), color = 'red', alpha = .5) +
  geom_line(aes(x = interp$x, y = interp.lin$y), color = 'blue', alpha = .5) +
  theme_bw()

## stick with splines

write.csv(as.data.frame(interp), 'kumcay_monthly.csv')
write.csv(as.data.frame(flows.mon), 'kumcay_monthly_avg.csv')
