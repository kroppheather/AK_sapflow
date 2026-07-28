library(lubridate)
library(dplyr)
library(ggplot2)
library(tidyr)

## read in soil data ----

dirData <- c("/Users/hkropp/Library/CloudStorage/GoogleDrive-hkropp@hamilton.edu/My Drive/research/projects/AK_sapflow/", # mac
             "C:/Users/kropp/Documents/AK_sapflow")
pathI <- 2

dirT <- paste0(dirData[pathI],"/tomst/07_27_26")

sensorI <- read.csv(paste0(dirData[pathI], "/tomst/sensor_sn.csv"))

# read in data
# set date for most current data
endDate <- "06-15-2026 14:30"
sensors <- read.csv(paste0(dirData[pathI],"/sensors_25.csv"))
sensors$endD <- ifelse(sensors$end_date == "current", endDate, sensors$end_date)
sensors$stDate <- mdy_hm(sensors$start_date)
sensors$edDate <- mdy_hm(sensors$endD)
# permafrost spruce
# updated data 11/26:
site1c <- read.table(paste0(dirData[pathI],"/11_26_2025/CR1000_sap_sl2_TableTC.dat"),
                     sep=",", header=FALSE, skip=4, na.strings=c("NA","NAN"))

site1c <- site1c[,1:12] 
site1_all <- read.table(paste0(dirData[pathI],"/11_26_2025/CR1000_sap_sl2_TableTC.dat"),
                        sep=",", header=FALSE, skip=4, na.strings=c("NA","NAN"))

site1_batt <- read.table(paste0(dirData[pathI],"/11_26_2025/CR1000_sap_sl2_TableTC.dat"),
                         sep=",", skip=1, na.strings=c("NA","NAN"))[,c(1,55:60)]

#site1d has all recent data since 4/24

site1d <- read.table(paste0(dirData[pathI],"/05_15_2026/CR1000_sap_sl2_TableTC.dat"),
                     sep=",", header=FALSE, skip=4, na.strings=c("NA","NAN"))

site1d <- site1d[,1:12] 

site1_battd <- read.table(paste0(dirData[pathI],"/05_15_2026/CR1000_sap_sl2_TableTC.dat"),
                          sep=",", skip=1, na.strings=c("NA","NAN"))[,c(1,55:60)]
# deciduous non-permafrost
site2 <- read.table(paste0(dirData[pathI],"/07_03_2024/Sapflow_TableDT.dat"),
                    sep=",", header=FALSE, skip=4)

site2 <- site2[,1:18]  



site2_batt <- read.table(paste0(dirData[pathI],"/11_26_2025/CR1000XSeries_TableTC.dat"),
                         sep=",", skip=1)[,c(1,165:170)]

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

site2e <- site2d[1361:5761,1:18] 



# sensor 5 moved to slot 12, sensor 8 moved to slot 16 on 8/20
site2_bind <- rbind(site2, site2c,site2d,site2e)





##### organize weather data ----
## weather 
weather <- read.csv(paste0(dirData[pathI],"/weather/4331627.csv"))


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


##### sap allometry -----
# Quiñonez-Piñón and Valero 2017 equations

sensors$sapwood <- ifelse(sensors$Species == "PIMA", 0.031*sensors$DBH+2.6,
                          ifelse( sensors$Species == "PIGL",0.089*sensors$DBH+0.7,
                                  3)) # filler until number can be identified



##### sap flow organize dates and combine data ----
site1_update <- site1d
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

ggplot(dtSite2 %>% filter(year == 2026), aes(dateF, dT, color=as.factor(sensorID)))+
  geom_point()+
  geom_line()
checkS4 <- dtSite2 %>% filter(year == 2026 & sensorID == 4)

ggplot(dtSite1 %>% filter(year == 2026) , aes(dateF, dT, color=as.factor(sensorID)))+
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
  select(Timestamp,dateF,year,doy,hour,DD,slot,siteID,siteName,sensorID, TreeID,Aspect,DBH,Species,Genus,sapwood,Notes,dT,maxDT,K,velo,mm_sq,dTQC)

sapS2f <- sapS2 %>%
  select(Timestamp,dateF,year,doy,hour,DD,slot,siteID,siteName,sensorID, TreeID,Aspect,DBH,Species,Genus,sapwood,Notes,dT,maxDT,K,velo,mm_sq,dTQC)

# join in sensor information
sapAll <- rbind(sapS1f, sapS2f)

sapAll$Hours <- floor(sapAll$hour)
sapAll$dayDate <- as.Date(sapAll$dateF)



# get hourly average for easier plotting
sapHour <- sapAll %>%
  group_by(Hours, doy, year, dayDate,siteID, sensorID, Aspect,  siteName, Genus) %>%
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


#### organize soil data ----
sensorI$SN <- as.character(sensorI$SN)

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
  datT[[i]]$SN <- rep(fileSN[i], nrow(datT[[i]]))
  
  datT[[i]]$dateF <- ymd_hm(datT[[i]]$date, tz="Etc/GMT+4") 
  datT[[i]]$akD <- with_tz(datT[[i]]$dateF, tzone="America/Anchorage")
}

tomstAll <- do.call("rbind",datT)
# filter out before installation
soilDF <- left_join(tomstAll, sensorI, by="SN")%>%
  filter(dateF >= ymd("2025-08-20"))
soilDF$month <- month(soilDF$akD)

ggplot(soilDF, aes(akD, Tm6, color=sitesensor) )+
  geom_line()

# permafrost-free silt loam
#unsure what calibration will be for permafrost. Putting in Peat as placeholder

soilDF$SMcor <- ifelse(soilDF$site == "permafrost-free",
                      (1.70E-8*(soilDF$SM^2)) + (1.18E-4*soilDF$SM) -0.1011,
                      (1.23E-7*(soilDF$SM^2)) - (1.45E-4*soilDF$SM) +0.203)

ggplot(soilDF %>%filter(site == "permafrost"), aes(akD, Tm6, color=sensorID) )+
  geom_line()


ggplot(soilDF %>%filter(site == "permafrost"&month==4), aes(akD, Tm6, color=sensorID) )+
  geom_line()


ggplot(soilDF %>%filter(site == "permafrost"&month==5), aes(akD, Tm6, color=sensorID) )+
  geom_line()

ggplot(soilDF %>%filter(site == "permafrost-free"), aes(akD, Tm6, color=sensorID) )+
  geom_line()

ggplot(soilDF %>%filter(site == "permafrost"), aes(akD, SM, color=sensorID) )+
  geom_line()

ggplot(soilDF %>%filter(site == "permafrost-free"), aes(akD, SM, color=sensorID) )+
  geom_line()

ggplot(soilDF %>%filter(site == "permafrost-free"), aes(akD, SMcor, color=sensorID) )+
  geom_line()

ggplot(soilDF %>%filter(site == "permafrost"), aes(akD, SMcor, color=sensorID) )+
  geom_line()



############## plotting for microclimate

sapNorth$month <- month(sapNorth$date)
microApril <- sapNorth %>%
  filter(month == 4 & year == 2026)

microOct <- sapNorth %>%
  filter(month == 10 & year == 2025)

soilOct <- soilDF  %>%
  filter(month == 10)

soilApril <- soilDF  %>%
  filter(month == 4)


ggplot(microOct %>% filter(siteID == 1)%>%filter(sensorID == 1 | sensorID ==2 | sensorID ==3), aes(date, sap_mm_s, color=as.factor(sensorID)))+
  geom_line()

ggplot(microApril %>% filter(siteID == 1), aes(date, sap_mm_s, color=as.factor(sensorID)))+
  geom_line()+theme_classic()


ggplot(soilApril %>% filter(site == "permafrost"), aes(akD, Tm6, color=sitesensor))+
  geom_line()+theme_classic()

