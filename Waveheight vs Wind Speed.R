library(ggplot2)
#setting working directory to directory this file is saved in
this.dir <- dirname(parent.frame(2)$ofile)
setwd(this.dir)
#Creating records data frame if it doesn't already exist
if(!exists("records")){source('Records_compiler.R')}
#Records analyzed by different wind directions, the below specifies the limits for each bin to be
#analyzed, specified in terms of wind direction degrees
binscust2<-c(0,16,41,74,99,155,208,250,270,360)
numrows<-length(binscust2)-1
regressionsumcust2<-data.frame(lowlim=double(numrows),uplim=double(numrows),count=double(numrows),intercept=double(numrows),slope=double(numrows),
                              rsquare=double(numrows),percent_within_.5=double(numrows),percent_within_1=double(numrows))
records$binscust2<-as.factor(NA)
binsnames<-character(length=numrows)
#For loop categorizes the records by the defined wind direction bins and regresses the Wind speed vs
#wave height correlation within each of those wind bin categories, and records the regression information, 
#also records the residual for each record relative to this regression in the records data frame
for (n in 1:numrows){
  records$binscust2<-ifelse(records$wind_dir>binscust2[n] & records$wind_dir<=binscust2[n+1],n,records$binscust2)
  lines<-records$binscust2==n
  lines[is.na(lines)]<-FALSE
  regressiontemp<-lm(records$Waveheight[lines] ~ records$wind_speed[lines])
  regressionsumcust2$lowlim[n]<-binscust2[n]
  regressionsumcust2$uplim[n]<-binscust2[n+1]
  regressionsumcust2$count[n]<-sum(lines,na.rm=TRUE)
  regressionsumcust2$intercept[n]<-regressiontemp$coefficients[1]
  regressionsumcust2$slope[n]<-regressiontemp$coefficients[2]
  regressionsumcust2$rsquare[n]<-summary(regressiontemp)$r.squared
  records$custompredicted2_residual[lines]<-regressiontemp$residuals
  records$custompredicted2[lines]<-records$Waveheight[lines]-records$custompredicted2_residual[lines]
  regressionsumcust2$percent_within_.5[n]<-sum(records$custompredicted2_residual[lines]<.5 & records$custompredicted2_residual[lines]>-.5,na.rm=TRUE)/regressionsumcust2$count[n]
  regressionsumcust2$percent_within_1[n]<-sum(records$custompredicted2_residual[lines]<1 & records$custompredicted2_residual[lines]>-1,na.rm=TRUE)/regressionsumcust2$count[n]
  binsnames[n]<-paste(binscust2[n],"to",binscust2[n+1], sep=" ")
}
#storing the bin label for each line in the records data frame
records$binscust2<-as.factor(records$binscust2)
levels(records$binscust2)<-binsnames
#plotting results
plotcustom2<-ggplot(records, aes(wind_speed,Waveheight))+geom_point()+facet_wrap(~binscust2,2,5)+geom_smooth(method="lm",se=FALSE)+
  ggtitle("Wave height vs. wind speed by wind direction")+xlab("Wind speed (m/s)")+ylab('Wave height (ft)')+
  scale_x_continuous(limits = c(0, 5))
print(plotcustom2)
#Writing summary of regression data to CSV
write.csv(regressionsumcust2,file = "Wind speed regression summary.csv")