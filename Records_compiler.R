#requires R.matlab package: install.packages('R.matlab') 
library(R.matlab)
#Also uses data.table
library(data.table)
#Also uses dplyr and lubridate
library(dplyr)
library(lubridate)
#Set working directory to directory this script is saved in
this.dir <- dirname(parent.frame(2)$ofile)
setwd(this.dir)
#Load wave height recrods for 2015 and 2016
wave_2015<-readMat('Source Data/wave_char_2015.mat')
wave_2016<-readMat('Source Data/wave_char_2016.mat')
#Merge records from both years
records<-rbind(unique(wave_2015$wave.recs[,1:2]),unique(wave_2016$wave.recs[,1:2]))
rm(wave_2016,wave_2015)
records<-data.table(records)
colnames(records)<-c('matlabdate','Waveheight')
#Removing rows without a recorded waveheight
records<-records[!is.nan(records$Waveheight),]
#converting from Matlab time to R time and adding 5 hours to be showing as central time, delivered in central time but tz=America/chi... tries to convert it 
records$date<-as.POSIXct((records$matlabdate - 719529)*86400+5*60*60, origin = "1970-01-01", tz="America/Chicago")
#Loading weather downloaded for a nearby weather station
weather_2016<-read.table('Source Data/2016_weather.txt')
weather_2015<-read.table('Source Data/2015_weather.txt')
weather<-rbind(weather_2015[,c(1:8,13,14)],weather_2016[,c(1:8,13,14)])
rm(weather_2016,weather_2015)
weather<-data.table(weather)
#Preparing weather data to merge into records data table
weather$date<-as.POSIXct(strptime(with(weather,paste(paste(V1,V2,V3,sep="-"),paste(V4,V5,sep=":"),sep=" ")),"%Y-%m-%d %H:%M"),tz="America/Chicago")
colnames(weather)<-c('y','m','d','h','mn','wind_dir','wind_speed','wind_gust','pressure','temp','date')
weather[,date:=date-(5*60*60)]
weather$datemerge=round(as.double(weather$date))
records$datemerge=round(as.double(records$date))
weather<-subset(weather, select=-c(y,d,m,h,mn))
records<-left_join(records,weather,by="datemerge")
#Loading in data from West Lake Superior Buoy
westbuoy_2015<-read.table('Source Data/2015_w_LkSuperior.txt')
westbuoy_2016<-read.table('Source Data/2016_w_LkSuperior.txt')
westbuoy<-rbind(westbuoy_2015,westbuoy_2016)
rm(westbuoy_2016,westbuoy_2015)
westbuoy<-data.table(westbuoy)
westbuoy$date<-as.POSIXct(strptime(with(westbuoy,paste(paste(V1,V2,V3,sep="-"),paste(V4,V5,sep=":"),sep=" ")),"%Y-%m-%d %H:%M"),tz="America/Chicago")
colnames(westbuoy)<-c('y','m','d','h','mn','wb_wind_dir','wb_wind_speed','wb_wind_gust','wb_wave_height','wb_dominant_period','wb_average_period',
                     'wb_wave_direction','wb_pressure','wb_temp','wb_water_temp','wb_dewp','wb_vis','wb_tide','wb_date')
westbuoy[,wb_date:=wb_date-(5*60*60)]
westbuoy<-subset(westbuoy, select=-c(y,d,m,h,mn,wb_wind_speed,wb_wind_gust,wb_average_period,
                                     wb_pressure,wb_dewp,wb_vis,wb_tide))
westbuoy$datemerge=round(as.double(westbuoy$wb_date))

#Adding modeled waveheights
model<-readMat('C:/Users/mpmey/OneDrive - UW-Madison/Apostle Islands/Qualitative Approach/Josh model outputs/forecast_data.mat')
modeldata<-data.frame(datemerge=model$time[1,],model_waveheight=model$model.SWH[,1],modelmeascheck=model$meas.SWH[1,])
modeldata$datemerge<-as.POSIXct((modeldata$datemerge - 719529)*86400, origin = "1970-01-01", tz="America/Chicago")
modeldata$datemerge<-round(as.double(modeldata$datemerge))
modeldata$modelmeascheck<-modeldata$modelmeascheck*3.28084
modeldata$model_waveheight<-modeldata$model_waveheight*3.28084
records<-left_join(records,modeldata,by="datemerge")
records$joshresid<-records$Waveheight-records$model_waveheight

#filtering data
records<-left_join(records,westbuoy,by="datemerge")
records<-records[!is.na(records$wind_dir),]
#when wind_dir = 0 found to be no reading, windspeed will also be 0, wind_dir=360 is valid
records<-records[records$wind_dir>0,]
#records<-records[!is.na(records$wb_wave_height),]
rownames(records) <- 1:nrow(records)
#Making summary data frame for summarizing all the regressions
summary<-data.frame(name=character(20),r2=double(20),percent_within_.5=double(20),percent_within_1=double(20))
summary$name<-as.character(summary$name)
#converting waveheights to feet
records$wb_wave_heightft<-records$wb_wave_height*3.28084

#adding hour and hour past windspeed as inputs for each line
temprecords<-subset(records,select=c("datemerge","wind_speed","wb_wave_height"))
colnames(temprecords)<-c("datemerge","wind_speed_less60m","wb_wave_height_less60m")
temprecords$datemerge<-temprecords$datemerge+(1*60*60)
records<-left_join(records,temprecords,by="datemerge")
records<-data.table(records)
records[,wsdif60m:=wind_speed-wind_speed_less60m]
records[,wbdif60m:=wb_wave_height-wb_wave_height_less60m]

# #2 hour
temprecords<-subset(records,select=c("datemerge","wind_speed","wb_wave_height"))
colnames(temprecords)<-c("datemerge","wind_speed_less120m","wb_wave_height_less120m")
temprecords$datemerge<-temprecords$datemerge+(2*60*60)
records<-left_join(records,temprecords,by="datemerge")
records<-data.table(records)
records[,wsdif120m:=wind_speed-wind_speed_less120m]
records[,wbdif120m:=wb_wave_height-wb_wave_height_less120m]

#3 hour
temprecords<-subset(records,select=c("datemerge","wind_speed","wb_wave_height"))
colnames(temprecords)<-c("datemerge","wind_speed_less180m","wb_wave_height_less180m")
temprecords$datemerge<-temprecords$datemerge+(3*60*60)
records<-left_join(records,temprecords,by="datemerge")
records<-data.table(records)
records[,wsdif180m:=wind_speed-wind_speed_less180m]
records[,wbdif180m:=wb_wave_height-wb_wave_height_less180m]

#average over last three hours
records[,meanwind:=(wsdif180m+wsdif120m+wsdif60m+wind_speed)/4]

# date compare created for plotting through by date, ignoring year of the data
records$date<-records$date.x
records$datecompare<-records$date
year(records$datecompare)<-2015

#adding GLCFS (already in central time)
GLCFS<-read.csv("Source Data/GLCFS_Nowcast_Point.csv",skip=4)
GLCFS<-GLCFS[,1:2]
names(GLCFS)<-c("Dateinitial","GLCFS_waveheight")
GLCFS$Date<-as.POSIXct(GLCFS$Dateinitial,format="%Y-%m-%d %H:%M",tz="America/Chicago")
GLCFS$datemerge=round(as.double(GLCFS$Date))
GLCFS<-GLCFS[,c(2,4)]
records<-left_join(records,GLCFS,by="datemerge")
records$GLCFS_residual<-records$Waveheight-records$GLCFS_waveheight
#Removing extra columns
records$date.x<-NULL
records$date.y<-NULL
records$wb_date<-NULL
#Create reference csv
write.csv(records,file = "records.csv")