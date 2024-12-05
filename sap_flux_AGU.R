library(lubridate)
library(dplyr)
library(ggplot2)
library(reshape2)
library(tidyr)

dirSave <- "/Users/hkropp/Library/CloudStorage/GoogleDrive-hkropp@hamilton.edu/My Drive/research/projects/AK_sapflow/AGU"

###### read in data -----
sensors <- read.csv("/Users/hkropp/Library/CloudStorage/GoogleDrive-hkropp@hamilton.edu/My Drive/research/projects/AK_sapflow/sensors 2.csv")
# permafrost spruce

site1 <- read.table("/Users/hkropp/Library/CloudStorage/GoogleDrive-hkropp@hamilton.edu/My Drive/research/projects/AK_sapflow/10_08_2024/CR1000_sap_sl2_TableTC.dat",
                    sep=",", header=FALSE, skip=4, na.strings=c("NA","NAN"))

site1 <- site1[,1:12] 


# deciduous non-permafrost
site2 <- read.table("/Users/hkropp/Library/CloudStorage/GoogleDrive-hkropp@hamilton.edu/My Drive/research/projects/AK_sapflow/07_03_2024/Sapflow_TableDT.dat",
                    sep=",", header=FALSE, skip=4)

site2 <- site2[,1:13]  
## weather 
weather <- read.csv("/Users/hkropp/Library/CloudStorage/GoogleDrive-hkropp@hamilton.edu/My Drive/research/projects/AK_sapflow/weather/3822739.csv")

## soil
soil1 <- read.csv("/Users/hkropp/Library/CloudStorage/GoogleDrive-hkropp@hamilton.edu/My Drive/research/projects/AK_sapflow/weather/Oct_2024/smith_lake_2_jan1_sept_2024.csv",
               header=FALSE, skip=6  )
soil1D <- read.csv("/Users/hkropp/Library/CloudStorage/GoogleDrive-hkropp@hamilton.edu/My Drive/research/projects/AK_sapflow/weather/Oct_2024/smith_lake_2_jan1_sept_2024.csv",
                   header=FALSE, skip=5, nrows=1)
soil1T <- read.csv("/Users/hkropp/Library/CloudStorage/GoogleDrive-hkropp@hamilton.edu/My Drive/research/projects/AK_sapflow/weather/Oct_2024/smith_lake_2_jan1_sept_2024.csv",
                   header=FALSE, skip=4, nrows=1)
names1 <- c("timestamp", "air.temp_2", 
            paste0(soil1T[3:13],"_", soil1D[3:13]))
colnames(soil1) <- names1

###### organize soil data ----
soil1DF <- melt(soil1)
type1 <- character()
depth1 <- character()
for(i in 1:nrow(soil1DF)){
  type1[i] <- strsplit(as.character(soil1DF$variable[i]), "_")[[1]][1]
  depth1[i] <- strsplit(as.character(soil1DF$variable[i]), "_")[[1]][2]
}
depth1f <- gsub(" m","", depth1)
soil1DF$type <- type1
soil1DF$depth <- depth1f 

soil1DF$dateF <- mdy_hm(soil1DF$timestamp)
soil1DF$doy <- yday(soil1DF$dateF)
soil1DF$hour <- hour(soil1DF$dateF)+(minute(soil1DF$dateF)/60)
soil1DF$year <- year(soil1DF$dateF)

swc1 <- soil1DF %>%
  filter(type == "Soil moisture")

s_temp1 <- soil1DF %>%
  filter(type == "Temperature")
s_temp1$DD <- (s_temp1$doy-1)+(s_temp1$hour/24)
swc1$DD <- (swc1$doy-1)+(swc1$hour/24)

s_temp2$DD <- (s_temp2$doy-1)+(s_temp2$hour/24)
swc2$DD <- (swc2$doy-1)+(swc2$hour/24)
ggplot(s_temp1 %>%
         filter(doy >= 170 & doy <= 180), aes(DD, value, color=depth))+
  geom_point()+
  geom_line()
ggplot(swc1 %>%
         filter(doy >= 177 & doy <= 180), aes(DD, value, color=depth))+
  geom_point()+
  geom_line()

ggplot(s_temp2 %>%
         filter(doy >= 170 & doy <= 180), aes(DD, value, color=depth))+
  geom_point()+
  geom_line()
ggplot(swc2 %>%
         filter(doy >= 170 & doy <= 180), aes(DD, value, color=depth))+
  geom_point()+
  geom_line()

day_st1 <- s_temp1 %>%
  group_by(doy, year, depth) %>%
  summarise(stemp = mean(value, na.rm=TRUE))
day_swc1 <- swc1 %>%
  group_by(doy, year, depth) %>%
  summarise(swc = mean(value, na.rm=TRUE))



soil2 <- read.csv("/Users/hkropp/Library/CloudStorage/GoogleDrive-hkropp@hamilton.edu/My Drive/research/projects/AK_sapflow/weather/Oct_2024/Bicycle_bumps_2024-01-01_2024-10-01.csv",
                  header=FALSE, skip=4  )
soil2D <- read.csv("/Users/hkropp/Library/CloudStorage/GoogleDrive-hkropp@hamilton.edu/My Drive/research/projects/AK_sapflow/weather/Oct_2024/Bicycle_bumps_2024-01-01_2024-10-01.csv",
                   header=FALSE, skip=3, nrows=1)
soil2T <- read.csv("/Users/hkropp/Library/CloudStorage/GoogleDrive-hkropp@hamilton.edu/My Drive/research/projects/AK_sapflow/weather/Oct_2024/Bicycle_bumps_2024-01-01_2024-10-01.csv",
                   header=FALSE, skip=2, nrows=1)

names2 <- c("timestamp", "air.temp_2", 
            paste0(soil2T[3:7],"_", soil2D[3:7]))
colnames(soil2) <- names2
soil2DF <- melt(soil2)
type2 <- character()
depth2 <- character()
for(i in 1:nrow(soil2DF)){
  type2[i] <- strsplit(as.character(soil2DF$variable[i]), "_")[[1]][1]
  depth2[i] <- strsplit(as.character(soil2DF$variable[i]), "_")[[1]][2]
}
depth2f <- gsub(" m","", depth2)
soil2DF$type <- type2
soil2DF$depth <- depth2f 
soil2DF <- na.omit(soil2DF)


soil2DF$dateF <- mdy_hm(soil2DF$timestamp)
soil2DF$doy <- yday(soil2DF$dateF)
soil2DF$hour <- hour(soil2DF$dateF)+(minute(soil2DF$dateF)/60)
soil2DF$year <- year(soil2DF$dateF)

swc2 <- soil2DF %>%
  filter(type == "Water Content")

s_temp2 <- soil2DF %>%
  filter(type == "Temp" | type == " Temp")

day_st2 <- s_temp2 %>%
  group_by(doy, year, depth) %>%
  summarise(stemp = mean(value, na.rm=TRUE))
day_swc2 <- swc2 %>%
  group_by(doy, year, depth) %>%
  summarise(swc = mean(value, na.rm=TRUE))

###### Organize weather data ------
#RH and Precip
# time in is local standard time
hourW <- weather %>%
  filter(REPORT_TYPE == "FM-15") %>%
  select(starts_with("Hourly") | starts_with("DATE"))

# daily

dailyW <- weather %>%
  filter(REPORT_TYPE == "SOD  ") %>%
  select(starts_with("Daily") | starts_with("DATE"))

dailyW$date <-  ymd_hms(dailyW$DATE, tz="Pacific/Gambier")
dailyW$snowD_cm <- as.numeric(dailyW$DailySnowDepth)*2.54 
dailyW$sDepth_cm <- ifelse(dailyW$snowD_cm > 60, NA, dailyW$snowD_cm)
ggplot(dailyW, aes(date,sDepth_cm))+
  geom_line()
dailyW$month <- month(dailyW$date)
ggplot(dailyW %>% filter(month==4), aes(date,sDepth_cm))+
  geom_line()

AprilD <- dailyW %>% filter(month==4)
# Gambier islands are always in UTC -9 with no daylight savigns
# this is the equivalent of alaksa standard time
hourW$dateS <- ymd_hms(hourW$DATE, tz="Pacific/Gambier")
# convert to local AK time that includes daylight savings
hourW$date <- with_tz(hourW$dateS, tz="America/Anchorage")
hourW$doy <- yday(hourW$date)
hourW$hour <- hour(hourW$date) 
hourW$month <- month(hourW$date) 
hourW$year <- year(hourW$date)
# trace gets converted to NA
hourW$Precip_mm <- as.numeric(hourW$HourlyPrecipitation)*25.4
hourW$TempC <- (as.numeric(hourW$HourlyDryBulbTemperature)- 32) * (5/9)
ggplot(hourW, aes(date,TempC))+
  geom_line()

dailyTemp <- hourW %>%
  group_by(doy, year) %>%
  summarise(minT = min(TempC, na.rm=TRUE),
            maxT = max(TempC, na.rm=TRUE))
###### allometry -----
# Quiñonez-Piñón and Valero found there was no significant relationship
# between dbh and sapwood between 10-40 cm dbh
# mean value for North was 3.2 cm
# assume mean for all trees
sensors$sapwood <- ifelse(sensors$Species == "PIMA", 3.2,
                          3) # filler until number can be identified



###### organize sapflow dates ----


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


###### sapflow calculations ----


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

# sapwood relationship: extract depth allometry from Quiñonez-Piñón for PIGL and PIMA
# sapwood area = pi(sd*DBH-sd^2) calculated from depth

# sapwood area allometry Perron 2023 for PIMA
# Quiñonez-Piñón has relationships between leaf area and sapwood area
# Power 2014 for projected leaf area for PIMA and PIGL


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

###### Organize data for graphing ----
sapHSite <- sapHSite %>%
  ungroup(doy, Hours) %>%
  filter(sap_mm_h < 100) # remove outliers
sapHSite$DD <- sapHSite$doy + (sapHSite$Hours/24)
siteHourGraph <- sapHSite %>%
  filter(doy <= 182)
sapHour1 <- siteHourGraph %>%
  filter(siteID == 1) %>%
  arrange(DD)

sapHour2 <- siteHourGraph %>%
  filter(siteID == 2)%>%
  arrange(DD)
sapHour2p <- sapHour2 %>%
  filter(Genus == "Picea")%>%
  arrange(DD)

sapHour2b <- sapHour2 %>%
  filter(Genus == "Betula")%>%
  arrange(DD)

swcGraph1 <- day_swc1 %>%
  filter(doy <= 182 & doy >= 92) 

stGraph1 <- day_st1 %>%
  filter(doy <= 182 & doy >= 92)


swcGraph2 <- day_swc2 %>%
  filter(doy <= 182 & doy >= 92) 

stGraph2 <- day_st2 %>%
  filter(doy <= 182 & doy >= 92)
dailyW$doy <- yday(dailyW$date)
dailyWj <- left_join(dailyW, dailyTemp, by=c("doy"))
dailyWj$precip_mm <- as.numeric(ifelse(dailyWj$DailyPrecipitation == "T" |
                              dailyWj$DailyPrecipitation == "Ts"  ,
                            0,
                            dailyWj$DailyPrecipitation))*25.4

precipRoll <- rep(NA, 6)
for(i in 7:nrow(dailyWj)){
  precipRoll[i] <- sum(dailyWj$precip_mm[(i-6):i])
  
}
dailyWj$weekPrecip_mm <- precipRoll

dailyWf <- dailyWj %>%
  filter(doy <= 182 & doy >= 92)
dailyWf$SD_m <- dailyWf$sDepth_cm/100
dailyWf$temp_c <- (dailyWf$DailyAverageDryBulbTemperature-32)*(5/9)

###### Graphing parms ----
colsSxS <- c("#C187C7", # permafrost picea
            "#007C57", # bb picea
            "#ADDABC") # bb betula 
colsSxStt <- c("#C187C799", # permafrost picea
             "#007C5799", # bb picea
             "#ADDABC99") # bb betula 

colsSite <- c("#C187C7", #permafrost
              "#007C57")
depthsST1 <-  c("0.1",  "0.4")
depthsST2 <-  c("0.1", "0.5")
depthsSW1 <- c("0.2",  "0.4")
depthsSW2 <- c("0.1", "0.5")
ltyDepth <- c(1,3)
            
cols1ST <- hcl.colors(5, palette = "Purples2")
cols1SW <- c(cols1ST[2],cols1ST[3], cols1ST[4])

cols2ST <- hcl.colors(5, palette = "Greens3")
cols2SW <- c(cols2ST[1], cols2ST[3])
###### Daily/Hourly graphs ----
# make graphs of data by doy



wd <- 15
hd <- 4
# cex axis
ca <- 2.5
# line for first label
ll1 <- 4
# line for doy
llxa <- 1.75
# cex for label text
cma <- 2
# cex for doy labels
cax <- 2
# legend size
lgc <- 2
# mai size
ma <- 0.6
# day seq
dayseq <- seq(90,180, by=10)
dayseql <- c("",seq(100,180, by=10))
# met graph
png(paste0(dirSave, "/met_figure.png"), width=17, height=9, units="in", res=150)
  layout(matrix(seq(1,2),ncol=1), width=lcm(rep(wd*2.54,1)),height=lcm(c(hd,hd)*2.54))
  par(mai= c(ma,ma,ma,ma))
  plot(c(0,1), c(0,1), xlim=c(92,182), ylim=c(-18,30),
       type="n", axes=FALSE, yaxs="i", xaxs="i",
       xlab = " ", ylab= " ")
  points(dailyWf$doy, dailyWf$minT,
         type="l", pch=19, col="cadetblue3", lwd=2)
  points(dailyWf$doy, dailyWf$maxT,
         type="l", pch=19, col="tomato3", lwd=2)
  abline(h=0, lty=2)
  axis(2, seq(-15,25, by=5), cex.axis=ca, las=2)
  axis(1, dayseq , 
       rep("", length(dayseq )),
       cex.axis=ca)
  mtext(dayseql ,
        at=dayseq ,side=1, line=llxa, 
        cex=cax)
  mtext("Air temperature (C)", side=2, line=ll1,
        cex=cma)
  legend("bottomright", c("Daily minimum", "Daily maximum"),
         lty=c(1,1), 
         col=c("cadetblue3","tomato3"),
         bty="n", cex=lgc)
# precip
  par(mai= c(ma,ma,ma,ma))
  plot(c(0,1), c(0,1), xlim=c(92,182), ylim=c(0,40),
       type="n", axes=FALSE, yaxs="i", xaxs="i",
       xlab = " ", ylab= " ")
  for(i in 1:nrow(dailyWf)){
    polygon(c(dailyWf$doy[i]-0.25,dailyWf$doy[i]-0.25,
              dailyWf$doy[i]+0.25,dailyWf$doy[i]+0.25),
            c(0,dailyWf$precip_mm[i],dailyWf$precip_mm[i],0),
    col="lightskyblue1", border=NA)
  }
  points(dailyWf$doy, dailyWf$sDepth_cm,
         type="l", pch=19)
  legend("topright", c("Precipitation", "Snow depth"),
         lty=c(NA,1),pch=c(15,NA), 
         col=c("lightskyblue1","black"),
         bty="n", cex=lgc)
  axis(1, dayseq , 
       rep("", length(dayseq )),
       cex.axis=ca)
  mtext(dayseql ,
        at=dayseq ,side=1, line=llxa, 
        cex=cax)
  axis(2, seq(0,40, by=5), cex.axis=ca, las=2)
  axis(4, seq(0,40, by=5), cex.axis=ca, las=2)
  mtext("Precipitation (mm)", side=2, line=ll1,
        cex=cma)
  mtext("Snow depth (cm)", side=4, line=ll1,
        cex=cma)
  mtext("Day of year", side=1, line=ll1,
        cex=cma)
dev.off()


# sap flow ----
png(paste0(dirSave, "/sapflow_soil.png"), width=17, height=17, units="in", res=150)
  layout(matrix(seq(1,4),ncol=1), width=lcm(rep(wd*2.54,1)),height=lcm(c(hd,hd,hd, hd)*2.54))
  par(mai= c(ma,ma,ma,ma))
  plot(c(0,1), c(0,1), xlim=c(92,182), ylim=c(0,160),
       type="n", axes=FALSE, yaxs="i", xaxs="i",
       xlab = " ", ylab= " ")
  arrows(sapHour1$DD, sapHour1$lowerE,sapHour1$DD,
         sapHour1$upperE, code=0, col=colsSxStt[1])
  points(sapHour1$DD, sapHour1$sap_mm_h, type="b",
         col=colsSxS[1], pch=19)

  axis(1, dayseq , 
       rep("", length(dayseq )),
       cex.axis=ca)
  mtext(dayseql ,
        at=dayseq ,side=1, line=llxa, 
        cex=cax)
  axis(2, seq(0,160, by=20), cex.axis=ca, las=2)
  mtext(expression(paste("Sap flow (mm h"^-1,")")), side=2, line=ll1,
        cex=cma)
  # sap flow bb
  par(mai= c(ma,ma,ma,ma))
  plot(c(0,1), c(0,1), xlim=c(92,182), ylim=c(0,160),
       type="n", axes=FALSE, yaxs="i", xaxs="i",
       xlab = " ", ylab= " ")
  arrows(sapHour2b$DD, sapHour2b$lowerE,sapHour2b$DD,
         sapHour2b$upperE, code=0, col=colsSxStt[3])
  arrows(sapHour2p$DD, sapHour2p$lowerE,sapHour2p$DD,
         sapHour2p$upperE, code=0, col=colsSxStt[2])
  points(sapHour2b$DD, 
         sapHour2b$sap_mm_h, 
         type="b",
         col=colsSxS[3], pch=19)
  points(sapHour2p$DD, 
         sapHour2p$sap_mm_h, 
         type="b",
         col=colsSxS[2], pch=19)
  
  axis(1, dayseq , 
       rep("", length(dayseq )),
       cex.axis=ca)
  mtext(dayseql ,
        at=dayseq ,side=1, line=llxa, 
        cex=cax)
  axis(2, seq(0,160, by=20), cex.axis=ca, las=2)
  mtext(expression(paste("Sap flow (mm h"^-1,")")), side=2, line=ll1,
        cex=cma)
  # soil temp
  par(mai= c(ma,ma,ma,ma))
  plot(c(0,1), c(0,1), xlim=c(92,182), ylim=c(-4,16),
       type="n", axes=FALSE, yaxs="i", xaxs="i",
       xlab = " ", ylab= " ")
  for(i in 1:length(depthsST1)){
    stplot = stGraph1 %>%
      filter(depth == depthsST1[i])
    points(stplot$doy, stplot$stemp, type="l",
           col=colsSite[1], lwd=2, lty=ltyDepth[i])   
  }
  for(i in 1:length(depthsST2)){
    stplot = stGraph2 %>%
      filter(depth == depthsST2[i])
    points(stplot$doy, stplot$stemp, type="l",
           col=colsSite[2], lwd=2, lty=ltyDepth[i])   
  }
  
  legend("topleft",
         c(depthsST1,depthsST2),
         col=rep(colsSite,each=2),
         lwd=2,
         lty=rep(ltyDepth, times=2),
         bty="n", cex=lgc, title="depth (m)")
  
  axis(1, dayseq , 
       rep("", length(dayseq )),
       cex.axis=ca)
  mtext(dayseql ,
        at=dayseq ,side=1, line=llxa, 
        cex=cax)
  axis(2, seq(-4,16, by=4), cex.axis=ca, las=2)
  mtext(expression(paste("Soil temperature (",degree,"C)")), 
                         side=2, line=ll1,
        cex=cma)

  
  par(mai= c(ma,ma,ma,ma))
  plot(c(0,1), c(0,1), xlim=c(92,182), ylim=c(0,0.65),
       type="n", axes=FALSE, yaxs="i", xaxs="i",
       xlab = " ", ylab= " ")
  for(i in 1:length(depthsSW1)){
    swplot = swcGraph1 %>%
      filter(depth == depthsSW1[i])
    points(swplot$doy, swplot$swc, type="l",
           col=colsSite[1], lwd=2, lty=ltyDepth[i])   
  }
  
  for(i in 1:length(depthsSW2)){
    swplot = swcGraph2 %>%
      filter(depth == depthsSW2[i])
    points(swplot$doy, swplot$swc, type="l",
           col=colsSite[2], lwd=2, lty=ltyDepth[i])   
  }
  
  legend("topleft",
         c(depthsST1,depthsST2),
         col=rep(colsSite,each=2),
         lwd=2,
         lty=rep(ltyDepth, times=2),
         bty="n", cex=lgc, title="depth (m)")
  axis(1, dayseq , 
       rep("", length(dayseq )),
       cex.axis=ca)
  mtext(dayseql ,
        at=dayseq ,side=1, line=llxa, 
        cex=cax)
  axis(2, seq(0,0.6, by=0.2), cex.axis=ca, las=2)

  mtext(expression(paste("Soil moisture (m"^3,"m"^-3,")")),
                         side=2, line=ll1,
        cex=cma)
  mtext("Day of year", side=1, line=ll1,
        cex=cma)
  
  

dev.off()





###### hourly trends ----

wd <- 8
hd <- 4
png(paste0(dirSave, "/sapflow_soil_hour.png"), width=17, height=17, units="in", res=150)
layout(matrix(seq(1,2),ncol=1), width=lcm(rep(wd*2.54,1)),height=lcm(c(hd,hd)*2.54))
par(mai= c(ma,ma,ma,ma))
plot(c(0,1), c(0,1), xlim=c(160,170), ylim=c(0,160),
     type="n", axes=FALSE, yaxs="i", xaxs="i",
     xlab = " ", ylab= " ")
arrows(sapHour1$DD, sapHour1$lowerE,sapHour1$DD,
       sapHour1$upperE, code=0, col=colsSxStt[1])
points(sapHour1$DD, sapHour1$sap_mm_h, type="b",
       col=colsSxS[1], pch=19)

axis(1, dayseq , 
     rep("", length(dayseq )),
     cex.axis=ca)
mtext(dayseql ,
      at=dayseq ,side=1, line=llxa, 
      cex=cax)
axis(2, seq(0,160, by=20), cex.axis=ca, las=2)
mtext(expression(paste("Sap flow (mm h"^-1,")")), side=2, line=ll1,
      cex=cma)

par(mai= c(ma,ma,ma,ma))
plot(c(0,1), c(0,1), xlim=c(160,170), ylim=c(0,160),
     type="n", axes=FALSE, yaxs="i", xaxs="i",
     xlab = " ", ylab= " ")
arrows(sapHour2b$DD, sapHour2b$lowerE,sapHour2b$DD,
       sapHour2b$upperE, code=0, col=colsSxStt[3])
arrows(sapHour2p$DD, sapHour2p$lowerE,sapHour2p$DD,
       sapHour2p$upperE, code=0, col=colsSxStt[2])
points(sapHour2b$DD, 
       sapHour2b$sap_mm_h, 
       type="b",
       col=colsSxS[3], pch=19)
points(sapHour2p$DD, 
       sapHour2p$sap_mm_h, 
       type="b",
       col=colsSxS[2], pch=19)

axis(1, dayseq , 
     rep("", length(dayseq )),
     cex.axis=ca)
mtext(dayseql ,
      at=dayseq ,side=1, line=llxa, 
      cex=cax)
axis(2, seq(0,160, by=20), cex.axis=ca, las=2)
mtext(expression(paste("Sap flow (mm h"^-1,")")), side=2, line=ll1,
      cex=cma)
dev.off()
###### max Js ----
#sat vapor in kpa
dailyWf$es <- (610.78 * exp((dailyWf$temp_c / (dailyWf$temp_c +237.3) * 17.2694)))/1000
dailyWf$ea <- dailyWf$es*(dailyWf$DailyAverageRelativeHumidity/100)
dailyWf$VPD <- dailyWf$es-dailyWf$ea

maxJs <- siteHourGraph %>%
  group_by(Name, siteName, doy) %>%
  summarise(maxJs = max(sap_mm_h),
            nmJ = n())
swcGraph1$siteName  <- rep("Permafrost", nrow(swcGraph1)) 
swcGraph2$siteName  <- rep("Bicycle bumps", nrow(swcGraph2)) 
swcDayR <- rbind(swcGraph1, swcGraph2)
swcDay <- swcDayR %>%
  filter(depth == "0.1" | depth == "0.2")

stGraph1$siteName  <- rep("Permafrost", nrow(stGraph1)) 
stGraph2$siteName  <- rep("Bicycle bumps", nrow(stGraph2)) 
stDF1 <- stGraph1 %>%
  filter(depth == "0" | depth == "0.1" | depth == "0.4") %>%
  pivot_wider(names_from= depth, values_from=stemp)
colnames(stDF1)[4:6] <- c("depth0", "depth1", "depth3")

stDF2 <- stGraph2 %>%
  filter(depth == "0" | depth == "0.1" | depth == "0.5") %>%
  pivot_wider(names_from= depth, values_from=stemp)
colnames(stDF2)[4:6] <- c("depth0", "depth1", "depth3")

stDay <- rbind(stDF1, stDF2)
soilDay <- left_join(swcDay, stDay, by=c("doy","year", "siteName"))

dailyMax <- left_join(maxJs, dailyWf, by="doy")
dailyMaxs <- left_join(dailyMax, soilDay, by=c("doy","siteName"))

ggplot(dailyMaxs, aes(maxT, maxJs, color=Name))+
  geom_point()


ggplot(dailyMaxs, aes(depth0, maxJs, color=Name))+
  geom_point()


ggplot(dailyMaxs, aes(depth1, maxJs, color=Name))+
  geom_point()

ggplot(dailyMaxs, aes(log(VPD), maxJs, color=Name))+
  geom_point()




