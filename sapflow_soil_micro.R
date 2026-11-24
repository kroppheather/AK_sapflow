library(lubridate)
library(dplyr)
library(ggplot2)
library(tidyr)

# graphing directory
dirSave <- "/Users/hkropp/Library/CloudStorage/GoogleDrive-hkropp@hamilton.edu/My Drive/research/projects/AK_sapflow/AGU26"
#Note: NOAA hourly missing data for some measurements. Look for continuous record of weather data

## read in soil data ----

dirData <- c("/Users/hkropp/Library/CloudStorage/GoogleDrive-hkropp@hamilton.edu/My Drive/research/projects/AK_sapflow/", # mac
             "C:/Users/kropp/Documents/AK_sapflow")
pathI <- 1

### borehole data
# no permafrost
bb_bore <- read.csv(paste0(dirData[pathI],"/11_6_2026/BB1_2024-01-01_2026-11-09_raw.csv"),
                           skip=17, head=FALSE, na.strings="NaN")

colnames(bb_bore) <- c("date","time","dew","rh","solar","tempRH","temp_0","temp_10",
                       "temp_50","temp_120_1","temp_120_2","temp_150","swc_10","swc_50", "empty")

bb_names <- read.csv(paste0(dirData[pathI],"/11_6_2026/BB1_2024-01-01_2026-11-09_raw.csv"),
                              skip=15,nrow=2, head=FALSE)

# permafrost
sml2_bore <- read.csv(paste0(dirData[pathI],"/11_6_2026/SmithLake2_temperature_soilmoisture_112024_1152026.csv"),
                      skip=9, head=FALSE, na.strings=c("NA","NaN"))

sml2_names <- read.csv(paste0(dirData[pathI],"/11_6_2026/SmithLake2_temperature_soilmoisture_112024_1152026.csv"),
                      skip=7, nrow=2,head=FALSE, na.strings=c("NA","NaN"))
colnames(sml2_bore) <- c("times","air_temp","surf_temp","temp_07","temp_15",
                         "temp_23","temp_30","temp_46","temp_61","temp_81","temp_150","temp_200","temp_300",
                         "swc_20","swc_40","swc_65","swc_85")

bb_bore$timestamp <- ymd_hms(paste(bb_bore$date, bb_bore$time))
sml2_bore$timestamp <- mdy_hm(sml2_bore$times)

# Tomst sensors
dirT <- paste0(dirData[pathI],"/tomst/07_27_26")

sensorI <- read.csv(paste0(dirData[pathI], "/tomst/sensor_sn.csv"))[1:10,]


tomstF <- list.files(dirT)
fileSN <- character()
for(i in 1:length(tomstF)){
  fileSN[i] <- as.numeric(strsplit(tomstF, "_")[[i]][2])
}
fileSNn <- as.numeric(fileSN)

# read in files
datT <- list()
# for some reason software is defaulting to East coste despite 
for(i in 1: length(fileSN)){
  datT[[i]] <- read.csv(paste0(dirT,"/",tomstF[i]), sep=";",
                        header=FALSE)[,1:9]
  colnames(datT[[i]])[1:9] <- c("record","date","tz","Tm6","T2","T15","SM","shake","errFlag")
  datT[[i]]$SN <- as.integer(rep(fileSN[i], nrow(datT[[i]])))
  
  datT[[i]]$dateF <- ymd_hm(datT[[i]]$date, tz="Etc/GMT+4") 
  datT[[i]]$akD <- with_tz(datT[[i]]$dateF, tzone="America/Anchorage")
}

tomstAll <- do.call("rbind",datT)
##### BENE sapwood allometry ----
bene <- read.csv(paste0(dirData[pathI], "/BENE allometry.csv"))


##### sapflow ----
# read in data
# set date for most current data
endDate <- "10-28-2026 00:00"
sensors <- read.csv(paste0(dirData[pathI],"/sensors_25.csv"))
sensors$endD <- ifelse(sensors$end_date == "current", endDate, sensors$end_date)
sensors$stDate <- mdy_hm(sensors$start_date)
sensors$edDate <- mdy_hm(sensors$endD)
# permafrost spruce
# updated data 11/26:

site1_batt <- read.table(paste0(dirData[pathI],"/11_6_2026/CR1000_sap_sl2_TableTC.dat"),
                          sep=",", skip=1, na.strings=c("NA","NAN"))[,c(1,55:60)]

site1e <- read.table(paste0(dirData[pathI],"/11_6_2026/CR1000_sap_sl2_TableTC.dat"),
                     sep=",", header=FALSE, skip=4, na.strings=c("NA","NAN"))[,1:12] 
# deciduous non-permafrost
site2 <- read.table(paste0(dirData[pathI],"/07_03_2024/Sapflow_TableDT.dat"),
                    sep=",", header=FALSE, skip=4)

site2 <- site2[,1:18]  



site2_batt <- read.table(paste0(dirData[pathI],"/11_26_2025/CR1000XSeries_TableTC.dat"),
                         sep=",", skip=4)[,c(1,165:170)]
site2_batt$date <- ymd_hms(site2_batt$V1)

ggplot(site2_batt, aes(date,V169))+
  geom_line()
# sensor 2 tree died. Moved sensor to new tree with 12.5 cm dbh. Refer to pic for pest damage on 8/21
# sensor 3 had a new sensor swapped in on the same tree and it solved dT anomalies on 8/20

# updated data:
site2c <- read.table(paste0(dirData[pathI],"/11_26_2025/CR1000XSeries_TableDT.dat"),
                     sep=",", header=FALSE, skip=4, na.strings=c("NA","NAN"))

site2c <- site2c[,1:18] 

site2d <-  read.table(paste0(dirData[pathI],"/06_18_2026/CR1000XSeries_TableDT.dat"),
                      sep=",", header=FALSE, skip=4, na.strings=c("NA","NAN"))

site2d <- site2d[8846:10205,1:18] 


site2e <-  read.table(paste0(dirData[pathI],"/07_27_2026/bb/sapflow_bb_TableDT.dat"),
                      sep=",", header=FALSE, skip=4, na.strings=c("NA","NAN"))

site2e <- site2e[1361:5761,1:18] 

site2e_batt <- read.table(paste0(dirData[pathI],"/07_27_2026/bb/sapflow_bb_TableTC.dat"),
                         sep=",", skip=4)[,c(1,165:169)]

site2e_batt$date <- ymd_hms(site2e_batt$V1)

ggplot(site2e_batt, aes(date,V169))+
  geom_line()

site2f <- read.table(paste0(dirData[pathI],"/11_6_2026/CR1000XSeries_TableTC.dat"),
                     sep=",", header=FALSE, skip=4, na.strings=c("NA","NAN"))[14607:17916,1:18]



# sensor 5 moved to slot 12, sensor 8 moved to slot 16 on 8/20
site2_bind <- rbind(site2, site2c,site2d,site2e,site2f)





##### organize weather data ----
## weather 
weather <- read.csv(paste0(dirData[pathI],"/weather/4431973.csv"))


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
dailyW$precip <- ifelse(dailyW$DailyPrecipitation == "T",0,as.numeric(dailyW$DailyPrecipitation))
pastPr <- numeric()
for(i in 7:nrow(dailyW)){
  pastPr[i] <- sum(dailyW$precip[i:(i-6)])
}
dailyW$pastPrecip <- pastPr

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
# calculate VPD and atmospheric pressure
hourW$e.sat <- 0.611*exp((17.502*hourW$HourlyDryBulbTemperature)/(hourW$HourlyDryBulbTemperature+240.97))
hourW$D <- hourW$e.sat-((hourW$HourlyRelativeHumidity/100)*hourW$e.sat)
range(hourW$HourlyStationPressure, na.rm=TRUE)
# convert pressure to Kpa from mb
hourW$P_KPA <- hourW$HourlyStationPressure*0.1
dayVPD <- hourW |>
  select(doy,month,year,D,P_KPA) |>
  group_by(doy, month, year) |>
  summarize(VPD = mean(D, na.rm=TRUE),
            P = mean(P_KPA, na.rm=TRUE),
            n=n())
dayVPD$date <- as.Date(dayVPD$doy-1, origin = paste0(dayVPD$year,"-01-01"))
#### soil data organization ----
bb_bore <- bb_bore |>
  filter(is.na(tempRH))

bb_bore_day <- bb_bore |>
  group_by(date) |>
  summarise(Tsoil_10 = mean(temp_10, na.rm=TRUE),
            Tsurf = mean(temp_0, na.rm=TRUE),
            Tsoil_50 = mean(temp_50, na.rm=TRUE))
bb_bore_day$date <- ymd(bb_bore_day$date)
ggplot(bb_bore_day, aes(date,Tsoil_10))+
  geom_line()

ggplot(bb_bore_day, aes(date,Tsoil_50))+
  geom_line()

sml2_bore$dates <- as.Date(sml2_bore$timestamp)

sml2_bore_day <- sml2_bore |>
  group_by(dates)|>
  summarise(Tsoil_07 = mean(temp_07),
            Tair_sm = mean(air_temp),
            Tsurf = mean(surf_temp))

ggplot(sml2_bore_day)+
  geom_line(aes(dates,Tsoil_07))+
  geom_line(aes(dates,Tair_sm),color="tomato3")

soil_sites_day <- data.frame(date_day = c(bb_bore_day$date, sml2_bore_day$dates),
                             site = c(rep("permafrost-free",nrow(bb_bore_day)),rep("permafrost",nrow(sml2_bore_day))),
                             location = c(rep("open",nrow(bb_bore_day)),rep("open",nrow(sml2_bore_day))),
                             sitesensor= c(rep("bb_bore",nrow(bb_bore_day)),rep("sml2_bore",nrow(sml2_bore_day))),
                             Tsoil = c(bb_bore_day$Tsoil_10, sml2_bore_day$Tsoil_07))
ggplot(soil_sites_day, aes(date_day,Tsoil,color=site))+
  geom_line()

## tomst soil

# filter out before installation
soilDF <- left_join(tomstAll, sensorI, by="SN") |>
  filter(dateF >= ymd("2025-08-20"))
soilDF$month <- month(soilDF$akD)

# permafrost-free silt loam
#unsure what calibration will be for permafrost. Putting in Peat as placeholder

soilDF$SMcor <- ifelse(soilDF$site == "permafrost-free",
                       (1.70E-8*(soilDF$SM^2)) + (1.18E-4*soilDF$SM) -0.1011,
                       (1.23E-7*(soilDF$SM^2)) - (1.45E-4*soilDF$SM) +0.203)


ggplot(soilDF, aes(date_day, Tm6, color=sitesensor) )+
  geom_line()
soilDF$date_day <- as.Date(soilDF$akD)
soil_t_day <- soilDF |>
  group_by(date_day, site, location, sitesensor ) |>
  summarize(Tsoil = mean(Tm6))

soil_all_app <- rbind(soil_sites_day, soil_t_day)

ggplot(soil_all_app, aes(date_day, Tsoil, color=sitesensor) )+
  geom_line()

soil_all_ave <- soil_all_app |>
  group_by(date_day, site,location) |>
  summarize(Ts = mean(Tsoil),
            sd_Ts = sd(Tsoil),
            n_Ts = n())

ggplot(soil_all_ave, aes(date_day, Ts, color=paste(site,location)))+
  geom_line()

ggplot(soil_sites_day, aes(date_day,Tsoil,color=site))+
  geom_line()

soil_swc_day <- soilDF |>
  group_by(date_day, site, location ) |>
  summarize(swc = mean(SMcor))

##### sap allometry -----
# BENE allometry 

plot(bene$DBH.cm, bene$Sapwood.depth.cm)
plot(bene$DBH.cm, bene$bark.depth.cm)

bene_sap <- lm(bene$Sapwood.depth.cm ~ bene$DBH.cm)
summary(bene_sap)

bene_bark <- lm(bene$bark.depth.cm ~ bene$DBH.cm)
summary(bene_bark)





# Quiñonez-Piñón and Valero 2017 equations

sensors$sapwood <- ifelse(sensors$Species == "PIMA", 0.031*sensors$DBH+2.6,
                          ifelse( sensors$Species == "PIGL",0.089*sensors$DBH+0.7,
                                  0.366*sensors$DBH-0.68))

sensors$bark <- ifelse(sensors$Species == "PIMA",NA,
                          ifelse( sensors$Species == "PIGL",NA,
                                  0.027*sensors$DBH-0.0018))

sensors$hartwood <- ifelse(sensors$Species == "PIMA",NA,
                       ifelse( sensors$Species == "PIGL",NA,
                               sensors$DBH-(sensors$bark*2)-(sensors$sapwood*2)))

# sapwood area equations from Pappas et al 2022
#area is in cm2 and cm dbh
sensors$sapwood_area <- ifelse(sensors$Species == "PIMA", 0.53*(sensors$DBH^1.78),
                               ifelse( sensors$Species == "PIGL",1.73*(sensors$DBH^1.49),
                                       (pi*(((sensors$sapwood/2)+(sensors$hartwood/2))^2))-(pi*((sensors$hartwood/2)^2)))) 

sensors$sap_area_m2 <- sensors$sapwood_area*0.0001


# height estimation from Yarie et al

sensors$height_m <-ifelse(sensors$Species == "PIMA", 0.441+0.703*sensors$DBH,
                        ifelse( sensors$Species == "PIGL",1.47+0.848*sensors$DBH-0.005*(sensors$DBH^2),
                                5.619+0.899*sensors$DBH-0.014*(sensors$DBH^2)))
#LA in m2 Power et al 2014 for PI sp. Yarie et al gives foliage weight in g
# 
#Melvin et al birch specific leaf area 0.0158 m2 per g 
sensors$LA_m2 <-ifelse(sensors$Species == "PIMA", 1.541*(sensors$DBH^3.782)*(sensors$height_m^-2.73),
                          ifelse( sensors$Species == "PIGL",0.1595*(sensors$DBH^3.131)*(sensors$height_m^-1.625),
                                 ( 145.892*sensors$DBH)*0.158))

# leaf area PIMA and PIGL from 
#B1*DBH^B2*H^B3

##### sap flow organize dates and combine data ----
site1_update <- site1e
colnames(site1_update) <- c("Timestamp", "Obs","doy","hour",paste0("slot",seq(1:8)))
site1_update <- site1_update %>%
  select(!c("Obs","hour", "doy"))

site1_long <- site1_update %>%
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

ggplot(dtSite2, aes(dateF, dT, color=as.factor(sensorID)))+
  geom_point()+
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
                            ifelse(sapS2$sensorID == 8 , NA, 
                                   ifelse(sapS2$sensorID == 5 & sapS2$year == 2025 & sapS2$doy < 233, NA,
                                          ifelse(sapS2$sensorID == 8 & sapS2$year == 2025 & sapS2$doy < 233,NA,sapS2$dT)))))

sapS2$K <- (sapS2$maxDT - sapS2$dTQC)/sapS2$dTQC
sapS2$velo <- 0.000119*(sapS2$K^1.231)
sapS2$mm_s <- sapS2$velo*1000

# filter out spikes of abnormal high values
#not a lot of extreme values. Even 99% will filter out real data. Extreme values in 99.9%
quant_site1 <- list()
for(i in 1:8){
  quant_site1[[i]] <- quantile(sapS1$mm_s[sapS1$sensorID == i], probs=seq(0,1,by=0.001),na.rm=TRUE)[1000]
  
}

quant_site2 <- list()
for(i in 1:11){
  quant_site2[[i]] <- quantile(sapS2$mm_s[sapS2$sensorID == i], probs=seq(0,1,by=0.001),na.rm=TRUE )[1000]
  
}


sapS2$mm_sq <- rep(NA, nrow(sapS2))
for(i in 1:11){
  sapS2$mm_sq <- ifelse(sapS2$sensorID == i & sapS2$mm_s > quant_site2[[i]] | sapS2$mm_s > 0.1,NA,sapS2$mm_s)
}

sapS1$mm_sq <- rep(NA, nrow(sapS1))
for(i in 1:8){
  sapS1$mm_sq <- ifelse(sapS1$sensorID == i & sapS1$mm_s > quant_site2[[i]] | sapS1$mm_s > 0.1,NA,sapS1$mm_s)
}




sapS1f <- sapS1 %>%
  select(Timestamp,dateF,year,doy,hour,DD,slot,siteID,siteName,sensorID, TreeID,Aspect,DBH,Species,Genus,sapwood,sapwood_area,LA_m2,Notes,dT,maxDT,K,velo,mm_sq,dTQC)

sapS2f <- sapS2 %>%
  select(Timestamp,dateF,year,doy,hour,DD,slot,siteID,siteName,sensorID, TreeID,Aspect,DBH,Species,Genus,sapwood,sapwood_area,LA_m2,Notes,dT,maxDT,K,velo,mm_sq,dTQC)

# sensor 8 issues, remove
sapS2f <- sapS2f %>%
  filter(sensorID != 8)
# join in sensor information
sapAll <- rbind(sapS1f, sapS2f)

sapAll$Hours <- floor(sapAll$hour)
sapAll$dayDate <- as.Date(sapAll$dateF)



# get hourly average for easier plotting
sapHour <- sapAll %>%
  group_by(Hours, doy, year, dayDate,siteID, sensorID, Aspect,  siteName, Genus, sapwood_area, LA_m2) %>%
  summarise(sap_mm_s= mean(mm_sq, na.rm=TRUE))

sapHour$date <- ymd_hm(paste(sapHour$dayDate, sapHour$Hours, ":00"))
sapHour$DD <- sapHour$doy+(sapHour$Hours/24)
# mm/s * 60s/min*60min/hr
sapHour$mm_h <- sapHour$sap_mm_s*60*60
# start by just looking at North
sapNorth <- sapHour %>%
  filter(Aspect == "N")


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

sapHSite$month <- month(sapHSite$date)

sapDSensor <- sapNorth|>
  na.omit() |>
  group_by( doy, year, siteID, siteName,month, Genus,sensorID) %>%
  summarise(sap_mm_d = sum(mm_h),
            n=n())|>
  filter(n >= 20)

sapDSite <- sapDSensor |>
  group_by( doy, year, month, siteID, siteName, Genus) |>
  summarise(sap_mm_day = mean(sap_mm_d),
            sd_mm_day = sd(sap_mm_d), 
            n_mm_day = n()) |>
  filter(n_mm_day >= 3)
sapDSite$Name <- paste(sapDSite$siteName, sapDSite$Genus)
sapDSite$se <- sapDSite$sd_mm_day /sqrt(sapDSite$n_mm_day)
sapDSite$lowerE <- sapDSite$sap_mm_day - sapDSite$se
sapDSite$upperE <- sapDSite$sap_mm_day + sapDSite$se

##### El ----

# sapflow in mm per hour

# convert back to m per hour
# m3 H2O m–2 (sapwood) s–1 or m s-1
sapNorth$m_s <- ((sapNorth$mm_h / 1000)/60)/60
sapNorth$sapwood_area_m <- sapNorth$sapwood_area*0.0001
# Ewers equation looks for Js (velo) in Kg m-2 s-1
# 1 m3 is 1000 kg
sapNorth$Js <-  sapNorth$m_s * 1000

#Calculate El
# El = Js * Sa/Sl
# Kg m-2 leaf s-1 (also = L m-2 leaf s-1)


sapNorth$El <- sapNorth$Js *(sapNorth$sapwood_area_m/sapNorth$LA_m2)
sapNorth$El_hr <- sapNorth$El*60*60

sapNorth$E_tree_hr <- sapNorth$Js *sapNorth$sapwood_area_m*60*60

##### Canopy stomatal conductance ----
hourCheck <- hourW %>%
  group_by(year,doy,hour) %>%
  summarize(nobs = n())

hourSap <- left_join(sapNorth, hourW, by=c("Hours"="hour","doy","year"))

##### Daily transpiration ----

# Kg (L) m-2 leaf day-1
# sum up for entire day
El_day <- sapNorth %>%
  filter(is.na(El_hr)==FALSE) %>%
  group_by( doy, year, siteID, siteName, Genus, sensorID) %>%
  summarize(El_day = sum(El_hr),
            E_tree = sum(E_tree_hr),
            n_day=n())%>%
  filter(n_day == 24)

El_site <- El_day %>%
  group_by(doy, year, siteID, siteName, Genus) %>%
  summarize(El_site = mean(El_day),
            El_tree_site=mean(E_tree),
            n_tree = n()) %>%
  filter(n_tree >= 3)%>% filter(doy>135&doy<258) # filter out early/late sap flow that may not be transpiration

El_site$date <- as.Date(El_site$doy-1, origin = paste0(El_site$year,"-01-01"))

El_site$ml_m2_day <- El_site$El_site*1000
El_site$month <- month(El_site$date)

El_site <- left_join(El_site,dailyW, by="date")
El_site$site <- ifelse(El_site$siteName == "Bicycle bumps", "permafrost-free",
                       ifelse(El_site$siteName == "Permafrost","permafrost",NA)      )


canopy_micro <- soil_all_ave |>
  filter(location == "canopy")
canopy_swc <- soil_swc_day |>
  filter(location == "canopy")

El_micro1 <- inner_join(El_site, canopy_micro, by=c("site","date"="date_day"))
El_micro2 <- inner_join(El_micro1, canopy_swc, by=c("site","date"="date_day"))

El_micro <- left_join(El_micro2, dayVPD, by="date" ) |>
  filter(year.x == 2026) |>
  filter(doy.x >= 151 & doy.x <= 244)



ggplot(El_micro, aes(Ts,El_site, color=paste(site,Genus)))+
  geom_point()

ggplot(El_micro, aes(swc,El_site, color=paste(site,Genus)))+
  geom_point()

ggplot(El_micro, aes(VPD,El_site, color=paste(site,Genus)))+
  geom_point()

ggplot(El_micro, aes(swc,Ts, color=paste(site,Genus)))+
  geom_point()


bp.El <- El_micro |>
  filter(site == "permafrost-free" & Genus == "Picea")
El.mod.bp <- lm(bp.El$El_site ~ bp.El$Ts + bp.El$VPD)
bp.res <- rstandard(El.mod.bp)
bp.fit <- fitted(El.mod.bp)
qqnorm(bp.res)
qqline(bp.res)
plot(bp.fit,bp.res)
abline(h=0)
summary(El.mod.bp)

bb.El <- El_micro |>
  filter(site == "permafrost-free" & Genus == "Betula")
El.mod.bb <- lm(bb.El$El_site ~ bb.El$swc + bb.El$VPD)
bb.res <- rstandard(El.mod.bb)
bb.fit <- fitted(El.mod.bb)
qqnorm(bb.res)
qqline(bb.res)
plot(bb.fit,bb.res)
summary(El.mod.bb)

pp.El <- El_micro |>
  filter(site == "permafrost")
El.mod.pp <- lm(pp.El$El_site ~ pp.El$swc + pp.El$VPD)
pp.res <- rstandard(El.mod.pp)
pp.fit <- fitted(El.mod.pp)
qqnorm(pp.res)
qqline(pp.res)
plot(pp.fit,pp.res)
summary(El.mod.pp)




############## plotting 
sapNorth$month <- month(sapNorth$date)
soil_sites_day$month <- month(soil_sites_day$date_day)
soil_sites_day$year <- year(soil_sites_day$date_day)


sapDSite$date <- as.Date(sapDSite$doy-1, origin = paste0(sapDSite$year, "-01-01"))

weather_day <- dailyW |>
  select(date, precip, pastPrecip, sDepth_cm,DailyMaximumDryBulbTemperature,DailyMinimumDryBulbTemperature, DailyAverageDryBulbTemperature)
daysap <- left_join(sapDSite, weather_day, by="date")


###### Graphing parms ----
colsSxS <- c("#C187C7", # permafrost picea
             "#007C57", # bb picea
             "#ADDABC") # bb betula 
colsSxStt <- c("#C187C799", # permafrost picea
               "#007C5799", # bb picea
               "#ADDABC99") # bb betula 

colsSite <- c("#C187C7", #permafrost
              "#007C57")

########## graphing: spring patterns ----


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

# x limits
dl <- 93
du <- 135
# day seq
dayseq <- seq(93,180, by=10)
dayseql <- c("",seq(100,180, by=10))


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
dl <- 109
du <-117
dayseq <- seq(dl,du, by=0.5)
dayseql <- seq(dl,du, by=1)

wd <- 8
hd <- 4
png(paste0(dirSave, "/sapflow_soil_hour.png"), width=9.5, height=9, units="in", res=150)
layout(matrix(seq(1,2),ncol=1), width=lcm(rep(wd*2.54,1)),height=lcm(c(hd,hd)*2.54))
par(mai= c(ma,ma,ma,ma))

plot(c(0,1), c(0,1), xlim=c(dl,du), ylim=c(0,160),
     type="n", axes=FALSE, yaxs="i", xaxs="i",
     xlab = " ", ylab=" ")

plot1 <- sapHSite |>
  filter(year == 2024 & siteName == "Permafrost")
points(plot1$DD-1, plot1$sap_mm_h, col=colsSxStt[1], pch=19)

plot2 <- sapHSite |>
  filter(year == 2024 & siteName == "Bicycle bumps" & Genus == "Picea")
points(plot2$DD-1, plot2$sap_mm_h, col=colsSxStt[2], pch=19)


plot3 <- sapHSite |>
  filter(year == 2024 & siteName == "Bicycle bumps" & Genus == "Betula")
points(plot3$DD-1, plot3$sap_mm_h, col=colsSxStt[3], pch=19)




plot(c(0,1), c(0,1), xlim=c(dl,du), ylim=c(0,160),
     type="n", axes=FALSE, yaxs="i", xaxs="i",
     xlab = " ", ylab=" ")

plot1 <- sapHSite |>
  filter(year == 2025 & siteName == "Permafrost")
points(plot1$DD, plot1$sap_mm_h, col=colsSxStt[1], pch=19)

plot2 <- sapHSite |>
  filter(year == 2025 & siteName == "Bicycle bumps" & Genus == "Picea")
points(plot2$DD, plot2$sap_mm_h, col=colsSxStt[2], pch=19)


plot3 <- sapHSite |>
  filter(year == 2025 & siteName == "Bicycle bumps" & Genus == "Betula")
points(plot3$DD, plot3$sap_mm_h, col=colsSxStt[3], pch=19)


plot(c(0,1), c(0,1), xlim=c(dl,du), ylim=c(0,160),
     type="n", axes=FALSE, yaxs="i", xaxs="i",
     xlab = " ", ylab=" ")

plot1 <- sapHSite |>
  filter(year == 2026 & siteName == "Permafrost")
points(plot1$DD, plot1$sap_mm_h, col=colsSxStt[1], pch=19)

plot2 <- sapHSite |>
  filter(year == 2026 & siteName == "Bicycle bumps" & Genus == "Picea")
points(plot2$DD, plot2$sap_mm_h, col=colsSxStt[2], pch=19)


plot3 <- sapHSite |>
  filter(year == 2026 & siteName == "Bicycle bumps" & Genus == "Betula")
points(plot3$DD, plot3$sap_mm_h, col=colsSxStt[3], pch=19)



plot(c(0,1), c(0,1), xlim=c(dl,du), ylim=c(0,100),
     type="n", axes=FALSE, yaxs="i", xaxs="i",
     xlab = " ", ylab=" ")


plot1 <- sapHSite |>
  filter(year == 2024 & siteName == "Permafrost") |>
  arrange(DD)
points(plot1$DD-1, plot1$sap_mm_h, col=colsSxStt[1], pch=19, type="b")

plot2 <- sapHSite |>
  filter(year == 2025 & siteName == "Permafrost") |>
  arrange(DD)
points(plot2$DD, plot2$sap_mm_h, col="dodgerblue4", pch=19, type="b")


plot3 <- sapHSite |>
  filter(year == 2026 & siteName == "Permafrost") |>
  arrange(DD)
points(plot3$DD, plot3$sap_mm_h, col="gray50", pch=19, type="b")


weather_day$doy <- yday(weather_day$date)
weather_day$year <- year(weather_day$date)
plot(c(0,1), c(0,1), xlim=c(dl,du), ylim=c(-40,20),
     type="n", axes=FALSE, yaxs="i", xaxs="i",
     xlab = " ", ylab=" ")

plot1 <- weather_day |>
  filter(year==2024)

points(plot1$doy-1,plot1$DailyMaximumDryBulbTemperature, type="l", col=colsSxStt[1])

points(plot1$doy-1,plot1$DailyMinimumDryBulbTemperature, type="l", col=colsSxStt[1])

plot2 <- weather_day |>
  filter(year==2025)

points(plot2$doy-1,plot2$DailyMaximumDryBulbTemperature, type="l", col="dodgerblue4")

points(plot2$doy-1,plot2$DailyMinimumDryBulbTemperature, type="l", col="dodgerblue4")

plot3 <- weather_day |>
  filter(year==2026)

points(plot3$doy-1,plot3$DailyMaximumDryBulbTemperature, type="l", col="gray30")

points(plot3$doy-1,plot3$DailyMinimumDryBulbTemperature, type="l", col="gray30")

