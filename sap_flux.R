library(lubridate)
library(dplyr)
library(ggplot2)
library(tidyr)
library(reshape2)


# read in data
# set date for most current data
endDate <- "11/10/25 15:30"
sensors <- read.csv("/Users/hkropp/Library/CloudStorage/GoogleDrive-hkropp@hamilton.edu/My Drive/research/projects/AK_sapflow/sensors_25.csv")
sensors$endD <- ifelse(sensors$end_date == "current", endDate, sensors$end_date)
sensors$stDate <- mdy_hm(sensors$start_date)
sensors$edDate <- mdy_hm(sensors$endD)
# permafrost spruce
# updated data 11/26:
site1c <- read.table("/Users/hkropp/Library/CloudStorage/GoogleDrive-hkropp@hamilton.edu/My Drive/research/projects/AK_sapflow/11_26_2025/CR1000_sap_sl2_TableTC.dat",
                     sep=",", header=FALSE, skip=4, na.strings=c("NA","NAN"))

site1c <- site1c[,1:12] 
site1_all <- read.table("/Users/hkropp/Library/CloudStorage/GoogleDrive-hkropp@hamilton.edu/My Drive/research/projects/AK_sapflow/11_26_2025/CR1000_sap_sl2_TableTC.dat",
                        sep=",", header=FALSE, skip=4, na.strings=c("NA","NAN"))

site1_batt <- read.table("/Users/hkropp/Library/CloudStorage/GoogleDrive-hkropp@hamilton.edu/My Drive/research/projects/AK_sapflow/11_26_2025/CR1000_sap_sl2_TableTC.dat",
                         sep=",", skip=1, na.strings=c("NA","NAN"))[,c(1,55:60)]
# deciduous non-permafrost
site2 <- read.table("/Users/hkropp/Library/CloudStorage/GoogleDrive-hkropp@hamilton.edu/My Drive/research/projects/AK_sapflow/07_03_2024/Sapflow_TableDT.dat",
                    sep=",", header=FALSE, skip=4)

site2 <- site2[,1:18]  



site2_batt <- read.table("/Users/hkropp/Library/CloudStorage/GoogleDrive-hkropp@hamilton.edu/My Drive/research/projects/AK_sapflow/11_26_2025/CR1000XSeries_TableTC.dat",
                        sep=",", skip=1)[,c(1,165:170)]

# sensor 2 tree died. Moved sensor to new tree with 12.5 cm dbh. Refer to pic for pest damage on 8/21
# sensor 3 had a new sensor swapped in on the same tree and it solved dT anomalies on 8/20

# updated data:
site2c <- read.table("/Users/hkropp/Library/CloudStorage/GoogleDrive-hkropp@hamilton.edu/My Drive/research/projects/AK_sapflow/11_26_2025/CR1000XSeries_TableDT.dat",
                     sep=",", header=FALSE, skip=4, na.strings=c("NA","NAN"))

site2c <- site2c[,1:18] 
# sensor 5 moved to slot 12, sensor 8 moved to slot 16 on 8/20
site2_bind <- rbind(site2, site2c)

##### organize soil and weather data ----
## weather 
weather <- read.csv("/Users/hkropp/Library/CloudStorage/GoogleDrive-hkropp@hamilton.edu/My Drive/research/projects/AK_sapflow/weather/4266886.csv")

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


#RH and Precip
# time in is local standard time
hourW <- weather %>%
  filter(REPORT_TYPE == "FM-15") %>%
  select(starts_with("Hourly") | starts_with("DATE"))

# daily

dailyW <- weather %>%
  filter(REPORT_TYPE == "SOD") %>%
  select(starts_with("Daily") | starts_with("DATE"))

dailyW$date <-  ymd_hms(dailyW$DATE, tz="Pacific/Gambier")
dailyW$snowT <- ifelse(dailyW$DailySnowDepth == "T", "1.27",dailyW$DailySnowDepth)
dailyW$snowD_cm <- as.numeric(dailyW$snowT)/10
dailyW$sDepth_cm <- dailyW$snowD_cm
ggplot(dailyW, aes(date,snowD_cm))+
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
hourW$PrecipT <- ifelse(hourW$HourlyPrecipitation == "T", "0.127",hourW$HourlyPrecipitation)
hourW$Precip_mm <- as.numeric(hourW$PrecipT)
hourW$TempC <- as.numeric(hourW$HourlyDryBulbTemperature)
ggplot(hourW, aes(date,TempC))+
  geom_line()


##### allometry -----
# Quiñonez-Piñón and Valero 2017 equations

sensors$sapwood <- ifelse(sensors$Species == "PIMA", 0.031*sensors$DBH+2.6,
                          ifelse( sensors$Species == "PIGL",0.089*sensors$DBH+0.7,
                          3)) # filler until number can be identified



##### organize dates and combine data ----

colnames(site1c) <- c("Timestamp", "Obs","doy","hour",paste0("slot",seq(1:8)))
site1c <- site1c %>%
  select(!c("Obs","hour", "doy"))

site1_long <- site1c %>%
  pivot_longer(!Timestamp, names_to="slot",values_to="dT")
site1_long$slotID = as.numeric(gsub("slot", "", site1_long$slot))


site1_long$dateF <- ymd_hms(site1_long$Timestamp)
site1_long$year <- year(site1_long$dateF)
site1_long$doy <- yday(site1_long$dateF)
site1_long$hour <- hour(site1_long$dateF)+(minute(site1_long$dateF)/60)
site1_long$DD <- site1_long$doy + (site1_long$hour/24)


# join in sensor data in a way that accounts for swapping slots/sensor trees
site1_sensor <- sensors %>%
  filter(siteID == 1)
site1_sensor$sensDateTime <- seq(1,nrow(site1_sensor))

site1_long$sensDateTime <- rep(NA, nrow(site1_long))

for(i in 1:nrow(site1_sensor)){
  site1_long$sensDateTime <- ifelse(site1_long$slotID == site1_sensor$slotID[i] &
                                    site1_long$dateF >= site1_sensor$stDate[i] &
                                    site1_long$dateF <= site1_sensor$edDate[i], 
                                    site1_sensor$sensDateTime[i],
                                    site1_long$sensDateTime)
  
}
dtSite1 <- inner_join(site1_long, site1_sensor, by=c("sensDateTime"))



colnames(site2_bind) <- c("Timestamp", "Obs",paste0("slot",seq(1:16)))
site2_bind <- site2_bind %>%
  select(!c("Obs", "slot13","slot14","slot15"))

site2_long <- site2_bind %>%
  pivot_longer(!Timestamp, names_to="slot",values_to="dT")
site2_long$slotID = as.numeric(gsub("slot", "", site2_long$slot))

site2_sensor <- sensors %>%
  filter(siteID == 2)


site2_long$sensDateTime <- rep(NA, nrow(site2_long))
site2_long$dateF <- ymd_hms(site2_long$Timestamp)
site2_long$year <- year(site2_long$dateF)
site2_long$doy <- yday(site2_long$dateF)
site2_long$hour <- hour(site2_long$dateF)+(minute(site2_long$dateF)/60)
site2_long$DD <- site2_long$doy + (site2_long$hour/24)
site2_sensor$sensDateTime <- seq(1,nrow(site2_sensor))



for(i in 1:nrow(site2_sensor)){
  site2_long$sensDateTime <- ifelse(site2_long$slotID == site2_sensor$slotID[i] &
                                      site2_long$dateF >= site2_sensor$stDate[i] &
                                      site2_long$dateF <= site2_sensor$edDate[i], 
                                    site2_sensor$sensDateTime[i],
                                    site2_long$sensDateTime)
  
}
dtSite2 <- inner_join(site2_long, site2_sensor, by=c("sensDateTime"))

dtSite2$dT <- as.numeric(dtSite2$dT)


ggplot(dtSite1, aes(dateF, dT, color=as.factor(sensorID)))+
  geom_point()

ggplot(dtSite2 %>% filter(sensorID ==1), aes(dateF, dT, color=as.factor(sensorID)))+
  geom_point()+
  geom_line()


ggplot(dtSite1, aes(dateF, dT, color=as.factor(sensorID)))+
  geom_point()+
  geom_line()

ggplot(dtSite2 %>% filter(sensorID ==4& year==2024), aes(dateF, dT, color=as.factor(sensorID)))+
  geom_line()     

ggplot(dtSite2 %>% filter(sensorID ==1& year==2025), aes(dateF, dT, color=as.factor(sensorID)))+
  geom_line()    
ggplot(dtSite1 %>% filter(sensorID ==1& year==2024), aes(dateF, dT, color=as.factor(sensorID)))+
  geom_line() 



################### calculations ----


#filter night so maximum in day and sensor is provided

# compare max day
maxnight1S1 <- dtSite1 %>%
  group_by(sensorID, doy,year) %>%
  filter(dT == max(dT),na.rm=TRUE)



maxnight1S2 <- dtSite2 %>%
  group_by(sensorID, doy,year) %>%
  filter(dT == max(dT),na.rm=TRUE)

#remove duplicate maximums that occur for longer than 15 min
#just take earliest measurement
maxnightS1 <- maxnight1S1   %>%
  group_by(sensorID, doy,year) %>%
  filter(hour == min(hour),na.rm=TRUE)

maxnightS2 <- maxnight1S2   %>%
  group_by(sensorID, doy, year) %>%
  filter(hour == min(hour),na.rm=TRUE)

maxJoinS1 <- data.frame(sensorID=maxnightS1$sensorID,
                      doy=maxnightS1$doy,
                      year=maxnightS1$year,
                      maxDT = maxnightS1$dT)

maxJoinS2 <- data.frame(sensorID=maxnightS2$sensorID,
                        doy=maxnightS2$doy,
                        year=maxnightS2$year,
                        maxDT = maxnightS2$dT)

sapS1 <- left_join(dtSite1, maxJoinS1, by=c("sensorID","doy","year"))
sapS2 <- left_join(dtSite2, maxJoinS2, by=c("sensorID","doy","year"))

# convert data to NA for sensors with issues
sapS1$dTQC <- ifelse(sapS1$sensorID == 1 & sapS1$year == 2024, NA, 
                  ifelse(sapS1$sensorID == 2 & sapS1$year == 2025 & sapS1$doy < 233, NA,
                         ifelse(sapS1$sensorID == 3 & sapS1$year == 2025 & sapS1$doy < 233,NA,sapS1$dT)))


# m3 H2O m–2 (sapwood) s–1 or m s-1
sapS1$K <- (sapS1$maxDT - sapS1$dTQC)/sapS1$dTQC
sapS1$velo <- 0.000119*(sapS1$K^1.231)
sapS1$mm_s <- sapS1$velo*1000

sapS2$dTQC <- ifelse(sapS2$dT <2, NA,
  ifelse(sapS2$sensorID == 5 & sapS2$year == 2024, NA, 
                     ifelse(sapS2$sensorID == 8 & sapS2$year == 2024, NA, 
                     ifelse(sapS2$sensorID == 5 & sapS2$year == 2025 & sapS2$doy < 233, NA,
                            ifelse(sapS2$sensorID == 8 & sapS2$year == 2025 & sapS2$doy < 233,NA,sapS2$dT)))))

sapS2$K <- (sapS2$maxDT - sapS2$dTQC)/sapS2$dTQC
sapS2$velo <- 0.000119*(sapS2$K^1.231)
sapS2$mm_s <- sapS2$velo*1000

# filter out spikes of abnormal high values
#not a lot of extreme values. Even 99% will filter out real data. Extreme values in 99.9%
quant_site1 <- list()
for(i in 1:8){
  quant_site1[[i]] <- quantile(sapS1f$mm_s[sapS1f$sensorID == i], probs=seq(0,1,by=0.001),na.rm=TRUE)[1000]
  
}

quant_site2 <- list()
for(i in 1:11){
  quant_site2[[i]] <- quantile(sapS2f$mm_s[sapS2f$sensorID == i], probs=seq(0,1,by=0.001),na.rm=TRUE )[1000]
  
}


sapS2$mm_sq <- rep(NA, nrow(sapS2))
for(i in 1:11){
  sapS2$mm_sq <- ifelse(sapS2$sensorID == i & sapS2$mm_s > quant_site2[[i]] | sapS2$mm_s > 0.1,NA,sapS2$mm_s)
}

sapS1$mm_sq <- rep(NA, nrow(sapS1))
for(i in 1:8){
  sapS1$mm_sq <- ifelse(sapS1$sensorID == i & sapS1$mm_s > quant_site2[[i]] | sapS1$mm_s > 0.1,NA,sapS1$mm_s)
}




ggplot(sapS2 %>% filter(year==2025& sensorID==8), aes(dateF, mm_sf,color=as.factor(sensorID)))+
  geom_point()+
  geom_line()

sapS1f <- sapS1 %>%
  select(Timestamp,dateF,year,doy,hour,DD,slot,siteID,siteName,sensorID, TreeID,Aspect,DBH,Species,Genus,sapwood,Notes,dT,maxDT,K,velo,mm_s,dTQC)

sapS2f <- sapS2 %>%
  select(Timestamp,dateF,year,doy,hour,DD,slot,siteID,siteName,sensorID, TreeID,Aspect,DBH,Species,Genus,sapwood,Notes,dT,maxDT,K,velo,mm_s,dTQC)

# join in sensor information
sapAll <- rbind(sapS1f, sapS2f)

sapAll$Hours <- floor(sapAll$hour)
sapAll$dayDate <- as.Date(sapAll$dateF)



# get hourly average for easier plotting
sapHour <- sapAll %>%
  group_by(Hours, doy, year, dayDate,siteID, sensorID, Aspect,  siteName, Genus) %>%
  summarise(sap_mm_s= mean(mm_s, na.rm=TRUE))

sapHour$date <- ymd_hm(paste(sapHour$dayDate, sapHour$Hours, ":00"))
sapHour$DD <- sapHour$doy+(sapHour$Hours/24)
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

ggplot(sapHourApril %>% filter(siteID == 1&year==2024), 
       aes(x=date, y= mm_h, color=as.factor(sensorID)))+
  geom_point()+
  geom_line()+
  theme_classic()
ggplot(sapHourApril %>% filter(siteID == 1&year==2025), 
       aes(x=date, y= mm_h, color=as.factor(sensorID)))+
  geom_point()+
  geom_line()+
  theme_classic()

# look at averages for site and genus
sapHSite <- sapNorth %>%
  na.omit() %>%
  group_by(Hours, doy, year, DD,date, siteID, siteName, Genus) %>%
  summarise(sap_mm_h = mean(mm_h),
            sd_mm_h = sd(mm_h), 
            n_mm_h = n())
sapHSite$Name <- paste(sapHSite$siteName, sapHSite$Genus)
sapHSite$se <- sapHSite$sd_mm_h/sqrt(sapHSite$n_mm_h)
sapHSite$lowerE <- sapHSite$sap_mm_h - sapHSite$se
sapHSite$upperE <- sapHSite$sap_mm_h + sapHSite$se
ggplot(sapHSite, aes(x=date, y=sap_mm_h, color=Name))+
  geom_line()+
  geom_point()+ylim(0,100)

ggplot(sapHour%>%filter(doy>=270&doy<=290&siteID == 1&year==2024), aes(x=DD, y=sap_mm_s, color=as.factor(sensorID)))+
  geom_line()+
  geom_point()+ylim(0,0.025)


ggplot(sapHSite%>%filter(year==2025&doy>245&doy<255), aes(x=date, y=sap_mm_h, color=Name))+
  geom_point()+ylim(0,100)+geom_line()

ggplot(sapHSite%>%filter(Name=="Permafrost Picea"& doy >= 100 & doy <= 130&year==2025), aes(x=DD, y=sap_mm_h, color=as.factor(year)))+
  geom_line()+
  geom_point()+ylim(0,100)

ggplot(sapHSite%>%filter(Name=="Permafrost Picea"& doy >= 130 & doy <= 160&year==2025), aes(x=DD, y=sap_mm_h, color=as.factor(year)))+
  geom_line()+
  geom_point()+ylim(0,100)

ggplot(sapHSite%>%filter(Name=="Permafrost Picea"& doy >= 160 & doy <= 190), aes(x=DD, y=sap_mm_h, color=as.factor(year)))+
  geom_line()+
  geom_point()+ylim(0,100)
  
siteHourApril <- sapHSite %>%
  filter(doy < 122)

siteHourMay <- sapHSite %>%
  filter(doy >= 122 & doy < 153)

siteHourJune <- sapHSite %>%
  filter(doy >= 153 & doy <= 213)

siteHourAug <- sapHSite %>%
  filter( doy >= 213)
soil1DF$DD <- soil1DF$doy + (soil1DF$hour/24)

swc1April <- soil1DF %>%
  filter(doy <= 122 & doy >= 92) %>%
  filter(type == "Soil moisture")


st1April <- soil1DF %>%
  filter(doy <= 122 & doy >= 92)%>%
  filter(type == "Temperature")

ggplot(siteHourApril, aes(x=date, y=sap_mm_h, color=Name))+
  geom_line()+
  geom_point()+
  geom_errorbar(aes(ymin =lowerE , ymax = upperE, color=Name), alpha=0.5)+
  labs(x= "date", y=expression(paste("sapflow (mm hr"^-1,")")))+
  theme_classic()+
  scale_y_continuous( expand=c(0.01,0.01))
ggplot(st1April,aes(x=DD, y=value, color=depth) )+
  geom_line()+
  geom_point()
ggplot(swc1April,aes(x=DD, y=value, color=depth) )+
  geom_line()
  
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

ggplot(siteHourAug, aes(x=date, y=sap_mm_h, color=Name))+
  geom_line()+
  geom_point()+
  geom_errorbar(aes(ymin =lowerE , ymax = upperE, color=Name), alpha=0.5)+
  labs(x= "date", y=expression(paste("sapflow (mm hr"^-1,")")))+
  theme_classic()+
  scale_y_continuous( expand=c(0.01,0.01))

# sapwood relationship: extract depth allometry from Quiñonez-Piñón for PIGL and PIMA
# sapwood area = pi(sd*DBH-sd^2) calculated from depth

# sapwood area allometry Perron 2023 for PIMA
# Quiñonez-Piñón has relationships between leaf area and sapwood area
# Power 2014 for projected leaf area for PIMA and PIGL

