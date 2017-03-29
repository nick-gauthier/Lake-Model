library(dplyr)
library(ggplot2)
library(parallel)
library(reshape2)


db <-src_postgres(host='192.168.2.100', 
                  port = '5432', 
                  dbname = 'gygaia', 
                  user = 'gygaiaro', 
                  password = 'gygaiaro')  


areas <- read.csv('~/Desktop/su_areas.csv') %>% tbl_df %>% 
     select(`SU Year` = su_year, `SU SeqNum` = su_seqnum, area = Area_ha) %>%
     distinct

test <- tbl(db,sql("SELECT * FROM finds.finds")) %>% collect %>%
     select(`SU Year`, `SU SeqNum`, `Chronological range`, `Confidence level`) %>%
     mutate(`Chronological range` = 
               ifelse(`Chronological range` %in% paleo, 'Paleolithic',
               ifelse(`Chronological range` %in% chalc.eba, 'Chalcolithic to Early Bronze',
               ifelse(`Chronological range` %in% mba.lba, 'Middle to Late Bronze',
               ifelse(`Chronological range` %in% iron.hell, 'Iron Age to Hellenistic',
               ifelse(`Chronological range` %in% roman, 'Roman',
               ifelse(`Chronological range` %in% med.mod, 'Medieval to Modern', NA))))))) %>%
     mutate(`Confidence level` = 
               ifelse(grepl('^Not', `Confidence level`), 'Not Confident',
               ifelse(grepl('^Confident', `Confidence level`), 'Confident',
               ifelse(grepl('^Very', `Confidence level`), 'Very Confident', NA)))) %>%
     mutate(conf.no = 
                 ifelse(`Confidence level` == 'Not Confident', 3,
                 ifelse(`Confidence level` == 'Confident', 2, 
                 ifelse(`Confidence level` == 'Very Confident', 1, NA)))) %>%
     rename(Period = `Chronological range`, Confidence = `Confidence level`) %>%
     filter(!is.na(Period) & !is.na(Confidence)) %>%
     distinct %>%
     inner_join(areas) %>% 
     group_by(`SU Year`, `SU SeqNum`, Period) %>%
     summarise(conf.no = min(conf.no), area = mean(area)) %>%
     ungroup %>%
     mutate(Period = factor(Period, levels = c('Paleolithic', 'Chalcolithic to Early Bronze', 'Middle to Late Bronze', 'Iron Age to Hellenistic', 'Roman', 'Medieval to Modern'))) %>%
     arrange(Period) %>% 
     group_by(Period, conf.no) %>%
     tally(area) %>%
     ungroup %>%
     mutate(temporal.weights = n / rep(epochs, each = 3))





mutate(Confidence = factor(Confidence, levels = c('Very Confident', 'Confident', 'Not Confident'))) %>%
     
test

ggplot(test, aes(x = Period, y = n)) + 
     geom_area(stat = 'identity', fill = 'black') +
     theme_minimal()

ggplot(test, aes(x = Period, y = n/5025.982)) + 
     geom_area(stat = 'identity', aes(group = conf.no, fill = conf.no)) +
     theme_minimal()

ggplot(test, aes(x = Period, y = temporal.weights)) + 
     geom_area(stat = 'identity', aes(group = conf.no, fill = conf.no)) +
     theme_minimal()

epochs <- c(300000,2500,1000,600,700, 800)

test <- tbl(db,sql("SELECT * FROM finds.finds")) %>% collect %>%
     select(`SU Year`, `SU SeqNum`, `Chronological range`, `Confidence level`) %>%
     group_by(`Chronological range`) %>%
     tally

ggplot(test, aes(x = `Chronological range`, y = n)) + geom_bar(stat='identity') + theme(axis.text.x=element_text(angle=90, hjust=1))
wut <- test %>% distinct %>% arrange(n)

test <- tbl(db,sql("SELECT * FROM finds.finds")) %>% collect %>%
     select(`SU Year`, `SU SeqNum`,`Chronological range`,`Confidence level`,Material)
write.csv(test,'~/Desktop/clasFinds.csv')

# %>%
     distinct

date.ranges <- read.csv('~/Desktop/chronoranges.csv', header = F) %>% tbl_df %>%
     rename(Period = V1, start = V2, end = V3, start.2 = V4, start.3 = V5) %>%
     mutate(Period = as.character(Period))


## starting again after cleaning data in Numbers
finds <- read.csv('~/Desktop/clasFinds.csv') %>% tbl_df %>%
     select(-Confidence) %>%
     filter(Material == 'Ceramic') 

finds.percent <- finds %>%
     group_by(SU.Year, SU.SeqNum, Period) %>%
     tally %>%
     mutate(su.total = sum(n), find.percent = n / su.total) %>%
     ungroup %>%
     left_join(finds %>% distinct)
     




calc.cen <- function(n, start, end, start2, end2){
     
     ifelse(is.na(start2),
            runif(1, start, end),
            ifelse(rbinom(1, 1, .5) == 1,
                   runif(1, start, end),
                   runif(1, start2, end2))) %>% round
}

finds.row <- finds %>% filter(!is.na(Start)) %>% rowwise 

library(parallel)
cl <- makeCluster(detectCores()-1)
clusterExport(cl=cl, varlist=c("finds.row", "calc.cen", '%>%'))
test <- parSapply(cl, 1:1000, function(i, ...) { dplyr::transmute(finds.row, calc.cen(n, Start, End, Start2, End2))})
stopCluster(cl)

library(reshape2)
names(test) <- 1:length(test)
test.melt <- melt(test)
ggplot(test.melt, aes(x = value)) + 
     geom_line(stat = 'bin', aes(group = L1), origin = -7300, right = T, binwidth = 100, alpha = .01) +
     theme_minimal() + labs(title ='CLAS chronological uncertainty', x = 'Years BP', y = 'Count of diagnostic sherds')+
     scale_x_continuous(breaks = seq(-7300,0, 100))

mcplot <- ggplot(test.melt, aes(x = value)) + 
     geom_line(stat = 'bin', aes(group = L1), origin = -6000, right = T, binwidth = 100, color = 'lightgrey') +
     theme_minimal() + labs(y = 'Count of diagnostic sherds') +
     scale_x_continuous(breaks = seq(-6000,0, 1000)) +
     xlim(-3000, NA)

library(grid)
library(gridExtra)
gA <- ggplotGrob(mcplot)
gB <- ggplotGrob(lake.plot)
gC <- ggplotGrob(carb.plot)
gD <- ggplotGrob(mag.plot)
maxWidth = grid::unit.pmax(gA$widths[2:5], gB$widths[2:5], gC$widths[2:5], gD$widths[2:5])
gA$widths[2:5] <- as.list(maxWidth)
gB$widths[2:5] <- as.list(maxWidth)
gC$widths[2:5] <- as.list(maxWidth)
gD$widths[2:5] <- as.list(maxWidth)
grid.arrange(gA, gB, gC, gD, ncol=1, heights = c(1,1.5,1,1))
ggsave("lake and survey.png")  

gA <- ggplotGrob(mcplot)
gB <- ggplotGrob(mudplot)
maxWidth = grid::unit.pmax(gA$widths[2:5], gB$widths[2:5])
gA$widths[2:5] <- as.list(maxWidth)
gB$widths[2:5] <- as.list(maxWidth)
grid.arrange(gA, gB, ncol=1, heights = c(1,2))
ggsave("lake and survey.png")  

db <-src_postgres(host='192.168.2.100', 
                  port = '5432', 
                  dbname = 'gygaia', 
                  user = 'gygaiaro', 
                  password = 'gygaiaro') 

periods <- tbl(db,sql("SELECT * FROM options.finds_chronology")) %>% collect 

counts <- tbl(db,sql("SELECT * FROM survey.survey_quantities")) %>% collect %>%
     filter(Name != "Total" & `Quantity description` == "Sherds") %>%
     rename(SU.Year = `SU Year`, SU.SeqNum = `SU SeqNum`) %>% 
     group_by(SU.Year, SU.SeqNum) %>%
     tally(wt = Quantity)

qplot(log(counts$n))


finds.weighted <- finds.percent %>% select(-n, -su.total, -Material) %>%
     left_join(counts) %>% mutate(n = round(find.percent * n)) %>% 
     filter(!is.na(Start) & !is.na(n))


calc.cen <- function(n, start, end, start2, end2){
     
     ifelse(is.na(start2),
            runif(1, start/ 10, end / 10) %>% floor * 10,
            ifelse(rbinom(1, 1, .5) == 1,
                   runif(1, start / 10, end / 10) %>% floor * 10,
                   runif(1, start2 / 10, end2 / 10) %>% floor * 10))
     
     #abs(start - end)
}
calc.cen <- function(n, start, end, start2, end2){
     
     ifelse(is.na(start2),
            runif(1, start -50 , end + 50),
            ifelse(rbinom(1, 1, .5) == 1,
                   runif(1, start - 50, end + 50),
                   runif(1, start2 -50, end2 + 50))) %>% round
}


finds.expand <- finds.weighted[rep(seq_len(nrow(finds.weighted)), finds.weighted$n),] %>% rowwise 
cl <- makeCluster(detectCores()-1)
clusterExport(cl=cl, varlist=c("finds.expand", "calc.cen", '%>%'))
test <- parSapply(cl, 1:500, function(i, ...) { dplyr::transmute(finds.expand, calc.cen(n, Start, End, Start2, End2))})
stopCluster(cl)

names(test) <- 1:length(test)
test.melt <- melt(test)
ggplot(test.melt, aes(x = value)) + 
     geom_line(stat = 'bin', aes(group = L1), right = T, origin = -7300, binwidth = 50, alpha = 1) +
     theme_minimal()
