library(lubridate)
library(dplyr)
library(ggplot2)
library(reshape2)

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

s_temp2 <- soil1DF %>%
  filter(type == "Temperature")

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
sapHSite$DD <- sapHSite$doy + (sapHSite$Hours/24)
siteHourGraph <- sapHSite %>%
  filter(doy <= 182)
sapHour1 <- siteHourGraph %>%
  filter(siteID == 1) %>%
  arrange(DD)

sapHour2 <- siteHourGraph %>%
  filter(siteID == 2)

swcGraph1 <- day_swc1 %>%
  filter(doy <= 182 & doy >= 92) 

stGraph1 <- day_st1 %>%
  filter(doy <= 182 & doy >= 92)


swcGraph2 <- day_swc2 %>%
  filter(doy <= 182 & doy >= 92) 

stGraph2 <- day_st2 %>%
  filter(doy <= 182 & doy >= 92)
dailyW$doy <- yday(dailyW$date)
dailyWf <- dailyW %>%
  filter(doy <= 182 & doy >= 92)
dailyWf$SD_m <- dailyWf$sDepth_cm/100
dailyWf$temp_c <- (dailyWf$DailyAverageDryBulbTemperature-32)*(5/9)
dailyWf$precip_mm <- ifelse(dailyWf$DailyPrecipitation == "T",
                            0,
                            as.numeric(dailyWf$DailyPrecipitation)*25.4)
###### Graphing parms ----
colsSxS <- c("#C187C7", # permafrost picea
            "#009F57", # bb picea
            "#ADDABC") # bb betula 

depthsST1 <-  c("0.1", "0.18", "0.4", "0.75", "1.5")
depthsST2 <-  c("0.1", "0.18", "0.4", "0.75", "1.5")
depthsSW1 <- unique(swcGraph1$depth)
depthsSW2 <- unique(swcGraph2$depth)
            
cols1ST <- hcl.colors(5, palette = "Purples2")
cols1SW <- c(cols1ST[2],cols1ST[3], cols1ST[4])

cols2ST <- hcl.colors(5, palette = "Greens3")
cols2SW <- c(cols2ST[1], cols2ST[3])
###### Daily/Hourly graphs ----
# make graphs of data by doy



wd <- 18
hd <- 8
# cex axis
ca <- 2.5
# line for first label
ll1 <- 4
# cex for label text
cma <- 2.5
# legend size
lgc <- 2.5
# met graph
png(paste0(dirSave, "/met_figure.png"), width=23, height=20, units="in", res=150)
  layout(matrix(seq(1,2),ncol=1), width=lcm(rep(wd*2.54,1)),height=lcm(c(hd,hd)*2.54))
  par(mai= c(1,1,1,1))
  plot(c(0,1), c(0,1), xlim=c(92,182), ylim=c(-15,25),
       type="n", axes=FALSE, yaxs="i", xaxs="i",
       xlab = " ", ylab= " ")
  points(dailyWf$doy, dailyWf$temp_c,
         type="l", pch=19)
  axis(1, seq(90,180, by=10), cex.axis=ca)
  axis(2, seq(-15,25, by=5), cex.axis=ca, las=2)
  mtext("Air temperature (C)", side=2, line=ll1,
        cex=cma)
  par(mai= c(1,1,1,1))
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
         lty=c(NA,1),pch=c(15,NA), col=c("lightskyblue1",NA),
         bty="n", cex=lgc)
  axis(1, seq(90,180, by=10), cex.axis=ca)
  axis(2, seq(0,40, by=5), cex.axis=ca, las=2)
  axis(4, seq(0,40, by=5), cex.axis=ca, las=2)
  mtext("Precipitation (mm)", side=2, line=ll1,
        cex=cma)
  mtext("Snow depth (cm)", side=4, line=ll1,
        cex=cma)
  mtext("Day of year", side=1, line=ll1,
        cex=cma)
dev.off()


# smith lake (permafrost )
png(paste0(dirSave, "/smith_lake_sf_soil.png"), width=25, height=20, units="in", res=300)
  layout(matrix(seq(1,3),ncol=1), width=lcm(rep(wd*2.54,1)),height=lcm(c(hd,hd,hd)*2.54))
  par(mai= c(1,1,1,1))
  plot(c(0,1), c(0,1), xlim=c(92,182), ylim=c(0,160),
       type="n", axes=FALSE, yaxs="i", xaxs="i",
       xlab = " ", ylab= " ")
  points(sapHour1$DD, sapHour1$sap_mm_h, type="b",
         col=colsSxS[1], pch=19)
  # soil temp
  par(mai= c(1,1,1,1))
  plot(c(0,1), c(0,1), xlim=c(92,182), ylim=c(-4,16),
       type="n", axes=FALSE, yaxs="i", xaxs="i",
       xlab = " ", ylab= " ")
  for(i in 1:length(depthsST1)){
    stplot = stGraph1 %>%
      filter(depth == depthsST1[i])
    points(stplot$doy, stplot$stemp, type="l",
           col=cols1ST[i], lwd=2)   
  }
  
  par(mai= c(1,1,1,1))
  plot(c(0,1), c(0,1), xlim=c(92,182), ylim=c(0,0.65),
       type="n", axes=FALSE, yaxs="i", xaxs="i",
       xlab = " ", ylab= " ")
  for(i in 1:length(depthsSW1)){
    swplot = swcGraph1 %>%
      filter(depth == depthsSW1[i])
    points(swplot$doy, swplot$swc, type="l",
           col=cols1SW[i], lwd=2)   
  }

dev.off()