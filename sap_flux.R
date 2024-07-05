library(lubridate)
library(dplyr)
library(ggplot2)

# read in data
sensors <- read.csv("/Users/hkropp/Library/CloudStorage/GoogleDrive-hkropp@hamilton.edu/My Drive/research/projects/AK_sapflow/sensors.csv")
# permafrost spruce
site1 <- read.table("/Users/hkropp/Library/CloudStorage/GoogleDrive-hkropp@hamilton.edu/My Drive/research/projects/AK_sapflow/07_03_2024/Loranty CR1000_TableTC.dat",
                    sep=",", header=FALSE, skip=4, na.strings=c("NA","NAN"))

site1 <- site1[,1:12] 


# deciduous non-permafrost
site2 <- read.table("/Users/hkropp/Library/CloudStorage/GoogleDrive-hkropp@hamilton.edu/My Drive/research/projects/AK_sapflow/07_03_2024/Sapflow_TableDT.dat",
                    sep=",", header=FALSE, skip=4)

site2 <- site2[,1:13]  


##### allometry -----
# Quiñonez-Piñón and Valero found there was no significant relationship
# between dbh and sapwood between 10-40 cm dbh
# mean value for North was 3.2 cm
# assume mean for all trees
sensors$sapwood <- ifelse(sensors$Species == "PIMA", 3.2,
                          3) # filler until number can be identified



##### organize dates ----


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

dtSite2 <- dtSite2 %>%
  filter(doy >= 97)

dtSite1 <- dtSite1 %>%
  filter(doy >= 97)

ggplot(dtSite2, aes(date, dT, color=as.factor(sensor)))+
         geom_point()+
         geom_line()


ggplot(dtSite2 %>% filter(sensor ==1), aes(date, dT, color=as.factor(sensor)))+
  geom_point()+
  geom_line()

test <- dtSite2 %>% filter(sensor ==8)

ggplot(dtSite1, aes(date, dT, color=as.factor(sensor)))+
  geom_point()+
  geom_line()

ggplot(dtSite1 %>% filter(sensor ==4), aes(date, dT, color=as.factor(sensor)))+
  geom_point()+
  geom_line()     


################### calculations ----


#filter night so maximum in day and sensor is provided

# compare max day
maxnight1S1 <- dtSite1 %>%
  group_by(sensor, doy) %>%
  filter(dT == max(dT),na.rm=TRUE)



maxnight1S2 <- dtSite2 %>%
  group_by(sensor, doy) %>%
  filter(dT == max(dT),na.rm=TRUE)

#remove duplicate maximums that occur for longer than 15 min
#just take earliest measurement
maxnightS1 <- maxnight1S1   %>%
  group_by(sensor, doy) %>%
  filter(hourD == min(hourD),na.rm=TRUE)

maxnightS2 <- maxnight1S2   %>%
  group_by(sensor, doy) %>%
  filter(hourD == min(hourD),na.rm=TRUE)

maxJoinS1 <- data.frame(sensor=maxnightS1$sensor,
                      doy=maxnightS1$doy,
                      maxDT = maxnightS1$dT)

maxJoinS2 <- data.frame(sensor=maxnightS2$sensor,
                        doy=maxnightS2$doy,
                        maxDT = maxnightS2$dT)

sapS1 <- left_join(dtSite1, maxJoinS1, by=c("sensor","doy"))
sapS2 <- left_join(dtSite2, maxJoinS2, by=c("sensor","doy"))

# m3 H2O m–2 (sapwood) s–1 or m s-1
sapS1$K <- (sapS1$maxDT - sapS1$dT)/sapS1$dT
sapS1$velo <- 0.000119*(sapS1$K^1.231)
sapS1$mm_s <- sapS1$velo*1000

sapS2$K <- (sapS2$maxDT - sapS2$dT)/sapS2$dT
sapS2$velo <- 0.000119*(sapS2$K^1.231)
sapS2$mm_s <- sapS2$velo*1000

# filter out sensors
sapS1f <- sapS1 %>%
  filter(sensor != 1)

sapS2f <- sapS2 %>%
  filter(sensor != 5 & sensor != 8)

sapS1f$siteID <- rep(1, nrow(sapS1f))
sapS2f$siteID <- rep(2, nrow(sapS2f))
# join in sensor information
sapAll1 <- rbind(sapS1f, sapS2f)

sapAll <- left_join(sapAll1, sensors, by=c("siteID", "sensor"="sensorID"))
sapAll$Hours <- floor(sapAll$hourD)
sapAll$dayDate <- as.Date(sapAll$date)
# get hourly average for easier plotting
sapHour <- sapAll %>%
  group_by(Hours, doy, dayDate,siteID, sensor, Aspect,  siteName, Genus) %>%
  summarise(sap_mm_s= mean(mm_s, na.rm=TRUE))

sapHour$date <- ymd_hm(paste(sapHour$dayDate, sapHour$Hours, ":00"))
# mm/s * 60s/min*60min/hr
sapHour$mm_h <- sapHour$sap_mm_s*60*60
# start by just looking at North
sapNorth <- sapHour %>%
  filter(Aspect == "N")

sapHourApril <- sapNorth %>%
  filter(doy < 122)

sapHourMay <- sapNorth %>%
  filter(doy >= 122 & doy < 153)

sapHourJune <- sapNorth %>%
  filter(doy >= 153)

ggplot(sapHourApril %>% filter(siteID == 1), 
       aes(x=date, y= mm_h, color=as.factor(sensor)))+
  geom_point()+
  geom_line()+
  theme_classic()

ggplot(sapHourApril %>% filter(siteID == 2), 
       aes(x=date, y= mm_h, color=as.factor(sensor)))+
  geom_point()+
  geom_line()+
  theme_classic()

ggplot(sapHourMay %>% filter(siteID == 1), 
       aes(x=date, y= mm_h, color=as.factor(sensor)))+
  geom_point()+
  geom_line()+
  theme_classic()

ggplot(sapHourMay %>% filter(siteID == 2), 
       aes(x=date, y= mm_h, color=as.factor(sensor)))+
  geom_point()+
  geom_line()+
  theme_classic()

ggplot(sapHourJune %>% filter(siteID == 1), 
       aes(x=date, y= mm_h, color=as.factor(sensor)))+
  geom_point()+
  geom_line()+
  theme_classic()

ggplot(sapHourJune %>% filter(siteID == 2), 
       aes(x=date, y= mm_h, color=as.factor(sensor)))+
  geom_point()+
  geom_line()+
  theme_classic()

# look at averages for site and genus
sapHSite <- sapNorth %>%
  na.omit() %>%
  group_by(Hours, doy, date, siteID, siteName, Genus) %>%
  summarise(sap_mm_h = mean(mm_h),
            sd_mm_h = sd(mm_h), 
            n_mm_h = n())
sapHSite$Name <- paste(sapHSite$siteName, sapHSite$Genus)
sapHSite$se <- sapHSite$sd_mm_h/sqrt(sapHSite$n_mm_h)
sapHSite$lowerE <- sapHSite$sap_mm_h - sapHSite$se
sapHSite$upperE <- sapHSite$sap_mm_h + sapHSite$se
ggplot(sapHSite, aes(x=date, y=sap_mm_h, color=Name))+
  geom_line()+
  geom_point()
  
siteHourApril <- sapHSite %>%
  filter(doy < 122)

siteHourMay <- sapHSite %>%
  filter(doy >= 122 & doy < 153)

siteHourJune <- sapHSite %>%
  filter(doy >= 153)

ggplot(siteHourApril, aes(x=date, y=sap_mm_h, color=Name))+
  geom_line()+
  geom_point()+
  geom_errorbar(aes(ymin =lowerE , ymax = upperE, color=Name), alpha=0.5)+
  labs(x= "date", y=expression(paste("sapflow (mm hr"^-1,")")))+
  theme_classic()+
  scale_y_continuous( expand=c(0.01,0.01))
  
ggplot(siteHourMay, aes(x=date, y=sap_mm_h, color=Name))+
  geom_line()+
  geom_point()+
  geom_errorbar(aes(ymin =lowerE , ymax = upperE, color=Name), alpha=0.5)+
  labs(x= "date", y=expression(paste("sapflow (mm hr"^-1,")")))+
  theme_classic()+
  scale_y_continuous( expand=c(0.01,0.01))

ggplot(siteHourJune, aes(x=date, y=sap_mm_h, color=Name))+
  geom_line()+
  geom_point()+
  geom_errorbar(aes(ymin =lowerE , ymax = upperE, color=Name), alpha=0.5)+
  labs(x= "date", y=expression(paste("sapflow (mm hr"^-1,")")))+
  theme_classic()+
  scale_y_continuous( expand=c(0.01,0.01))


############# old plot code ----
# 122 May 1st
# 153 June 1st

ggplot(sapS1f, aes(date, velo_g_s, color=as.factor(sensor)))+
  geom_point()+
  geom_line()+
  ylim(0,35)
ggplot(sapS1f %>% filter(sensor == 3 & doy < 124), 
       aes(date, velo_g_s, color=as.factor(sensor)))+
  geom_point()+
  geom_line()

ggplot(sapS1f %>% filter(doy < 124), 
       aes(date, velo_g_s, color=as.factor(sensor)))+
  geom_point()+
  geom_line()

ggplot(sapS2f %>% filter(doy < 124), 
       aes(date, velo_g_s, color=as.factor(sensor)))+
  geom_point()+
  geom_line()

ggplot(sapS2f, 
       aes(date, velo_g_s, color=as.factor(sensor)))+
  geom_point()+
  geom_line()+
  theme_classic()
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

