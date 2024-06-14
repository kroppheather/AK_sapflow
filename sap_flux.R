library(lubridate)
library(dplyr)
library(ggplot2)

# read in data
# permafrost spruce
site1 <- read.table("G:/My Drive/research/projects/AK_sapflow/05_03_2024/CR1000_sap_sl2_TableTC.dat",
                    sep=",", header=FALSE, skip=4, na.strings=c("NA","NAN"))

site1 <- site1[,1:12] 


# deciduous non-permafrost
site2 <- read.table("G:/My Drive/research/projects/AK_sapflow/05_03_2024/sapflow2_TableDT.dat",
                    sep=",", header=FALSE, skip=4)

site2 <- site2[,1:13]  


site2$dateF <- ymd_hms(site2[,1])
site2$year <- year(site2$dateF)
site2$doy <- yday(site2$dateF)
site2$hour <- hour(site2$dateF)+(minute(site2$dateF)/60)
site2$DD <- site2$doy + (site2$hour/24)


site1$dateF <- ymd_hms(site1[,1])
site1$year <- year(site1$dateF)
site1$doy <- yday(site1$dateF)
site1$hour <- hour(site1$dateF)+(minute(site1$dateF)/60)
site1$DD <- site1$doy + (site1$hour/24)

dtSite1 <- data.frame(date= rep(site1$date, times = 8), 
                      doy = rep(site1$doy, times = 8),
                      hourD = rep(site1$hour, times = 8),
                      DD = rep(site1$DD, times = 8),
                      sensor = rep(seq(1,8), each = nrow(site1)), 
                      dT = as.numeric(c(site1[,5],
                                        site1[,6],
                                        site1[,7],
                                        site1[,8],
                                        site1[,9],
                                        site1[,10],
                                        site1[,11],
                                        site1[,12])))

dtSite2 <- data.frame(date= rep(site2$date, times = 11), 
                     doy = rep(site2$doy, times = 11),
                     hourD = rep(site2$hour, times = 11),
                     DD = rep(site2$DD, times = 11),
                     sensor = rep(seq(1,11), each = nrow(site2)), 
                     dT = as.numeric(c(site2[,3],
                            site2[,4],
                            site2[,5],
                            site2[,6],
                            site2[,7],
                            site2[,8],
                            site2[,9],
                            site2[,10],
                            site2[,11],
                            site2[,12],
                            site2[,13])))

tail(unique(dtSite2$dT))

ggplot(dtSite2, aes(date, dT, color=as.factor(sensor)))+
         geom_point()+
         geom_line()


ggplot(dtSite2 %>% filter(sensor ==11), aes(date, dT, color=as.factor(sensor)))+
  geom_point()+
  geom_line()

test <- dtSite2 %>% filter(sensor ==8)

ggplot(dtSite1, aes(date, dT, color=as.factor(sensor)))+
  geom_point()+
  geom_line()

ggplot(dtSite1 %>% filter(sensor ==8), aes(date, dT, color=as.factor(sensor)))+
  geom_point()+
  geom_line()       


############################# June ----

site1b <- read.table("G:/My Drive/research/projects/AK_sapflow/06_13_2024/CR1000_sap_sl2_TableTC.dat",
                    sep=",", header=FALSE, skip=4, na.strings=c("NA","NAN"))

site1b <- site1b[,1:12] 



site2b <- read.table("G:/My Drive/research/projects/AK_sapflow/06_13_2024/sapflow2_TableDT.dat",
                    sep=",", header=FALSE, skip=4)

site2b <- site2b[,1:13]  


site2b$dateF <- ymd_hms(site2b[,1])
site2b$year <- year(site2b$dateF)
site2b$doy <- yday(site2b$dateF)
site2b$hour <- hour(site2b$dateF)+(minute(site2b$dateF)/60)
site2b$DD <- site2b$doy + (site2b$hour/24)


site1b$dateF <- ymd_hms(site1b[,1])
site1b$year <- year(site1b$dateF)
site1b$doy <- yday(site1b$dateF)
site1b$hour <- hour(site1b$dateF)+(minute(site1b$dateF)/60)
site1b$DD <- site1b$doy + (site1b$hour/24)

dtsite1b <- data.frame(date= rep(site1b$date, times = 8), 
                      doy = rep(site1b$doy, times = 8),
                      hourD = rep(site1b$hour, times = 8),
                      DD = rep(site1b$DD, times = 8),
                      sensor = rep(seq(1,8), each = nrow(site1b)), 
                      dT = as.numeric(c(site1b[,5],
                                        site1b[,6],
                                        site1b[,7],
                                        site1b[,8],
                                        site1b[,9],
                                        site1b[,10],
                                        site1b[,11],
                                        site1b[,12])))

dtsite2b <- data.frame(date= rep(site2b$date, times = 11), 
                      doy = rep(site2b$doy, times = 11),
                      hourD = rep(site2b$hour, times = 11),
                      DD = rep(site2b$DD, times = 11),
                      sensor = rep(seq(1,11), each = nrow(site2b)), 
                      dT = as.numeric(c(site2b[,3],
                                        site2b[,4],
                                        site2b[,5],
                                        site2b[,6],
                                        site2b[,7],
                                        site2b[,8],
                                        site2b[,9],
                                        site2b[,10],
                                        site2b[,11],
                                        site2b[,12],
                                        site2b[,13])))

ggplot(dtsite2b, aes(date, dT, color=as.factor(sensor)))+
  geom_point()+
  geom_line()

ggplot(dtsite1b, aes(date, dT, color=as.factor(sensor)))+
  geom_point()+
  geom_line()

ggplot(dtsite2b %>% filter(sensor == 11), aes(date, dT, color=as.factor(sensor)))+
  geom_point()+
  geom_line()


ggplot(dtsite1b %>% filter(sensor == 8), aes(date, dT, color=as.factor(sensor)))+
  geom_point()+
  geom_line()

