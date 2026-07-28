library(lubridate)
library(dplyr)
library(ggplot2)
library(tidyr)

# read in data

dirData <- c("/Users/hkropp/Library/CloudStorage/GoogleDrive-hkropp@hamilton.edu/My Drive/research/projects/AK_sapflow/", # mac
             "C:/Users/kropp/Documents/AK_sapflow")
pathI <- 2

dirT <- paste0(dirData[pathI],"/tomst/07_27_26")

sensorI <- read.csv(paste0(dirData[pathI], "/tomst/sensor_sn.csv"))
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
