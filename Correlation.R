library(ggplot2)
bins <- c(0,45,90,135,180,225,270,315,360)
numrows<-length(bins)-1
regressionsum<-data.frame(lowlim=double(numrows),uplim=double(numrows),count=double(numrows),intercept=double(numrows),slope=double(numrows),
                          rsquare=double(numrows),percent_within_.5=double(numrows),percent_within_1=double(numrows))
records$bins<-as.factor(NA)
binsnames<-character(length=numrows)
for (n in 1:numrows){
  records$bins<-ifelse(records$wind_dir>bins[n] & records$wind_dir<=bins[n+1],n,records$bins)
  lines<-records$bins==n
  lines[is.na(lines)]<-FALSE
  regressiontemp<-lm(records$Waveheight[lines] ~ records$wind_speed[lines])
  regressionsum$lowlim[n]<-bins[n]
  regressionsum$uplim[n]<-bins[n+1]
  regressionsum$count[n]<-sum(lines,na.rm=TRUE)
  regressionsum$intercept[n]<-regressiontemp$coefficients[1]
  regressionsum$slope[n]<-regressiontemp$coefficients[2]
  regressionsum$rsquare[n]<-summary(regressiontemp)$r.squared
  records$binspredicted_residual[lines]<-regressiontemp$residuals
  records$binspredicted[lines]<-records$Waveheight[lines]-records$binspredicted_residual[lines]
  regressionsum$percent_within_.5[n]<-sum(records$binspredicted_residual[lines]<.5 & records$binspredicted_residual[lines]>-.5,na.rm=TRUE)/regressionsum$count[n]
  regressionsum$percent_within_1[n]<-sum(records$binspredicted_residual[lines]<1 & records$binspredicted_residual[lines]>-1,na.rm=TRUE)/regressionsum$count[n]
  binsnames[n]<-paste(bins[n],"to",bins[n+1], sep=" ")
}
summary$name[1]<-as.character("basic 8 bins")
summary$r2[1]<-1-(sum((records$binspredicted_residual)^2)/(sum((records$Waveheight-mean(records$Waveheight))^2)))
summary$percent_within_.5[1]<-sum(records$binspredicted_residual<.5 & records$binspredicted_residual>-.5,na.rm=TRUE)/sum(records$wind_dir>0)
summary$percent_within_1[1]<-sum(records$binspredicted_residual<1 & records$binspredicted_residual>-1,na.rm=TRUE)/sum(records$wind_dir>0)
records$bins<-as.factor(records$bins)
levels(records$bins)<-binsnames
plot<-ggplot(records, aes(wind_speed,Waveheight))+geom_point()+facet_wrap(~bins,2,4)+geom_smooth(method="lm")+ggtitle("8 bin regular interval breakdown")
print(plot)