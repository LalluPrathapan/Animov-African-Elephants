install.packages("move")
install.packages("getPass")
install.packages("berryFunctions")
install.packages("bcpa")
install.packages("ecmwfr")
install.packages("adehabitatHR")
library(adehabitatHR)
library(ecmwfr)
library(getPass)
library(move)
library(sp)
library(raster)
library(geosphere)
library(lubridate)
library(sf)
library(mapview)
library(moveVis)
library(raster)
library(bcpa)
library(ggplot2)
library(dplyr)

login<-movebankLogin(user="LalluPrathapan",password= getPass::getPass())
study_name<-searchMovebankStudies("African elephants in Etosha National Park",login = login)
study_meta<-getMovebankStudy(study_name[1],login=login)
study_indis<-getMovebankAnimals(study_name,login=login)

#all individuals moveStack
study_data<-getMovebankData(study_name,login = login)

#specific individual move
study_data_indi<-getMovebankData(study_name,animalName = as.character(study_indis$local_identifier[3]),login = login)
study_data_2010 <-(study_data_indi[year(study_data_indi$timestamp)==2010])

#build on sp you can treat move and moveStack as spatial object
study_data_coords<-coordinates(study_data_2010)
study_data_times<-timestamps(study_data_2010)
study_data_lags<-timeLag(study_data_indi[year(study_data_indi$timestamp)==2010],units="mins")
study_ids<-idData(study_data_indi)

#to dataframe converting to sf
study_data_f<-as.data.frame(study_data_2010)
study_data_f<-st_as_sf(study_data_f,coords = c("location_long","location_lat"),crs=st_crs(study_data))

#mapview of study data
mapview(study_data_indi)
hist(study_data_lags)

#calculating speed and distance
study_data_distance<-distance(study_data_2010)
velocity<-speed(study_data_2010)
plot(study_data_distance,velocity,xlim=c(0,10000),ylim=c(0,1.5))
plot(times_df1,study_data_distance)
summary(study_data_distance)
#checking date
data_time_hr<-hour(study_data_times)
hist(data_time_hr)

study_data_angles<-angle(study_data_2010)
study_data_turn_angles<-turnAngleGc(study_data_2010)
plot(data_turn_angles,velocity)

#NSD
nsd<-spDistsN1(study_data_coords[2:3919,],study_data_coords[1,])
nsd
plot(study_data_times[1:3918],nsd)
hist(nsd)

#nsd to dataframe for further merging

nsd_df <- as.data.frame(nsd)# making a data frame out of NSD data for later mergin
times_df<-as.data.frame(study_data_times)
times_df1 <- times_df[-3919,]# adjusting the number of rows to match NSD data frame
nsd_df$time <- times_df1
nsd_df


#derivative of nsd
nsd_vec<-nsd_aggr$mean_X1
x <- 1:length(nsd_vec)
nsd_derivative <- diff(x)/diff(nsd_vec)
plot(nsd_derivative,nsd_vec[-1])
nsd_abs1<-abs(nsd_derivative)
plot(nsd_abs1)



#Precipitation data
precipitation_2010<-read.csv('D:\\EAGLE\\Precipitation.csv',sep=";")
prec<-precipitation_2010$total_precipitation
prectime<-precipitation_2010$Date
x11()
#plot(factor(prectime),prec)
plot(prec,type="l")

#Aggregating nsd with precipitation data
nsd_df$Date <- as.Date(nsd_df$time,"%Y-%m-%d %T") # formatting date to be a date object (for aggregating)nsd_aggr <-  nsd_df %>%
nsd_aggr <-  nsd_df %>%
  mutate(Date = floor_date(Date)) %>%
  group_by(Date) %>%
  summarize(mean_X1 = mean(nsd))

precipitation_2010$Date <- as.Date(precipitation_2010$Date, "%d.%m.%Y") # formatting date to be a date object in precipitation data
nsd_aggr$Date <- as.Date(nsd_aggr$Date, "%Y-%m-%d") 
nsd_Precipitation <- join(precipitation_2010, nsd_aggr, by="Date") # joining data NSD and precipitation
  
# Precipitation and NDS Plots
plot(nsd_Precipitation$Date,nsd_Precipitation$total_precipitation, type="l",xlab="Date", ylab="Total Precipitation") #plotting
plot(nsd_Precipitation$Date,nsd_Precipitation$mean_X1, type="l",xlab="Date", ylab="mean_nsd")

nsd_Precipitation2 <- na.omit(nsd_Precipitation) # deleting NA rows
plot(nsd_Precipitation2$Date,nsd_Precipitation2$total_precipitation, type="l",xlab="Date", ylab="Total Precipitation")
plot(nsd_Precipitation2$Date,nsd_Precipitation2$mean_X1, type="l",xlab="Date", ylab="mean_nsd")

#derivative of nsd
prec_vec<-nsd_Precipitation2$total_precipitation
x <- 1:length(prec_vec)
prec_derivative <- diff(x)/diff(prec_vec)
plot(prec_derivative,prec_vec[-1])
prec_abs1<-abs(prec_derivative)
plot(prec_abs1)



#Aggregation with temperature
temp_2010<-read.csv('D:\\EAGLE\\Tempe.csv',sep=";")
tempt<-temp_2010$mean_air_temperature
temperature<-temp_2010$ï..Date
plot(tempt,type="l")

temp_2010$Date <- as.Date(temp_2010$ï..Date, "%d.%m.%Y") # formatting date to be a date object in precipitation data
nsd_temp <- join(temp_2010, nsd_aggr, by="Date") # joining data NSD and precipitation

# Temperature and NDS Plots
plot(nsd_temp$Date,nsd_temp$mean_temperature, type="l",xlab="Date", ylab="Temperature") #plotting
plot(nsd_temp$Date,nsd_temp$mean_X1, type="l",xlab="Date", ylab="mean_nsd")

nsd_temp2 <- na.omit(nsd_temp) # deleting NA rows
plot(nsd_temp2$Date,nsd_temp$mean_temperature, type="l",xlab="Date", ylab="Temperature")
plot(nsd_temp2$Date,nsd_temp2$mean_X1, type="l",xlab="Date", ylab="mean_nsd")

#correlation coefficient
cor(nsd_temp2$mean_temperature,nsd_temp2$mean_X1)


#BCPA analysis
#Plotting the tracks of the elephant
X<-as.numeric(study_data_coords[,1])
Y<-as.numeric(study_data_coords[,2])
Time <-study_data_times[1:3919]
mytrack <- MakeTrack(X,Y,Time)

#MY elephant track
plot(mytrack)
elephant.VT <- GetVT(mytrack)
head(elephant.VT)
#relating step length with turning angle as plot
plot(elephant.VT$S,elephant.VT$Theta,xlab="Step_Length",ylab="Turning_angle")
#histogram of turning angle
#hist(elephant.VT$Theta)
summary(elephant.VT)
#window sweep
#window size=integer size of analysiswindo
#chr string respv
ele.ws <- WindowSweep(elephant.VT, "V*cos(Theta)", windowsize=30, progress=FALSE, K=2)
head(ele.ws)
summary(ele.ws)
x11()
plot(ele.ws,type="smooth")
plot(ele.ws, type="smooth", threshold=5)
plot(ele.ws, type="flat", clusterwidth=3)
ChangePointSummary(ele.ws, clusterwidth=5)
PathPlot(mytrack, ele.ws, type="flat", clusterwidth = 3, main="Flat BCPA")
