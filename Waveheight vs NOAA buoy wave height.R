library(ggplot2)
#setting working directory to directory this file is saved in
this.dir <- dirname(parent.frame(2)$ofile)
setwd(this.dir)
if(!exists("records")){source('Records_compiler.R')}
#Records analyzed by different wind directions, the below specifies the limits for each bin to be
#analyzed, specified in terms of wind direction degrees
binscust<-c(0,16,41,74,99,155,208,250,270,360)
numrows<-length(binscust)-1
#Creating data frame to populate with regression outputs
regressionsumcustbuoyonly<-data.frame(lowlim=double(numrows),uplim=double(numrows),count=double(numrows),intercept=double(numrows),
                                       slope=double(numrows),
                                       rsquare=double(numrows),percent_within_.5=double(numrows),percent_within_1=double(numrows))
records$binscust<-as.factor(NA)
binsnames<-character(length=numrows)
#For loop categorizes the records by the defined wind direction bins and regresses the Wind speed vs
#wave height correlation within each of those wind bin categories, and records the regression information, 
#also records the residual for each record relative to this regression in the records data frame
for (n in 1:numrows){
  records$binscust<-ifelse(records$wind_dir>binscust[n] & records$wind_dir<=binscust[n+1],n,records$binscust)
  lines<-records$binscust==n
  lines[is.na(lines)]<-FALSE
  lines[is.na(records$wb_wave_height)]<-FALSE
  regressiontemp<-lm(records$Waveheight[lines] ~ records$wb_wave_height[lines])
  regressionsumcustbuoyonly$lowlim[n]<-binscust[n]
  regressionsumcustbuoyonly$uplim[n]<-binscust[n+1]
  regressionsumcustbuoyonly$count[n]<-sum(lines,na.rm=TRUE)
  regressionsumcustbuoyonly$intercept[n]<-regressiontemp$coefficients[1]
  regressionsumcustbuoyonly$slope[n]<-regressiontemp$coefficients[2]
  regressionsumcustbuoyonly$rsquare[n]<-summary(regressiontemp)$r.squared
  records$custbuoyonly_residual[lines]<-regressiontemp$residuals
  records$custbuoyonlypredicted[lines]<-records$Waveheight[lines]-records$custbuoyonly_residual[lines]
  regressionsumcustbuoyonly$percent_within_.5[n]<-sum(records$custbuoyonly_residual[lines]<.5 & records$custbuoyonly_residual[lines]>-.5,na.rm=TRUE)/regressionsumcustbuoyonly$count[n]
  regressionsumcustbuoyonly$percent_within_1[n]<-sum(records$custbuoyonly_residual[lines]<1 & records$custbuoyonly_residual[lines]>-1,na.rm=TRUE)/regressionsumcustbuoyonly$count[n]
  binsnames[n]<-paste(binscust[n],"to",binscust[n+1], sep=" ")
}
#storing the bin label for each line in the records data frame
records$binscust<-as.factor(records$binscust)
levels(records$binscust)<-binsnames
#plotting results
plotcustombuoymodel<-ggplot(records, aes(model_waveheight,Waveheight))+geom_point()+facet_wrap(~binscust,2,4)+geom_smooth(method="lm")+
  ggtitle("Custom bins")
westbuoywindplotcust<-ggplot(records,aes(wb_wave_heightft,Waveheight))+geom_point()+facet_wrap(~binscust,2,5)+geom_smooth(method="lm",se=FALSE)+
  ggtitle("NOAA buoy wave height vs Sea Caves wave height by wind direction")+xlab("NOAA buoy wave height (ft)")+ylab('Sea Caves wave height (ft)')+
  scale_x_continuous(limits = c(0, 5))+
  theme(text = element_text(size=20))
print(westbuoywindplotcust)
#Writing summary of regression data to CSV
write.csv(regressionsumcustbuoyonly,file = "Buoy regression summary.csv")