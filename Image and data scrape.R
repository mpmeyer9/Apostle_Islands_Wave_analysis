#This script takes the date, time, recorded wave height, and link to a webcam photo taken at that time and
#organizes them into a dataframe and saves them as a .csv
library(XML)
library(stringr)
#Settign working directory to that which this file is saved in
this.dir <- dirname(parent.frame(2)$ofile)
setwd(this.dir)
#link is the website URL, the website HTML is converted to an XML object that 
link = url('http://wavesatseacaves.cee.wisc.edu/wave_pics_2011.html')
a<-readLines(link)
html<-htmlTreeParse(a, useInternalNodes = TRUE)
rootnode<-xmlRoot(html)
# a and b are the first and last lines of the site (loaded as an XML object) to be loaded
a = 55
b = 8000
d = b - a
e=1
final <- data.frame()
# For loop adds a line to the data table for each of the specified lines in the page
for (n in a:b)
{
  c <- rootnode[[2]][[2]][[2]][[6]][[n]][[1]]
    if (!is.null(c))
    {
    attrs<-xmlAttrs(rootnode[[2]][[2]][[2]][[6]][[n]][[1]])
    #date stores the date node from the xml, and converts it to date class for storage in data frame (includes time)
    date<-xmlValue(rootnode[[2]][[2]][[2]][[6]][[n]][[1]])
    date<-strsplit(noquote(date), " ")
    date<-unlist(date)
    datedate<-as.Date.character(date[1],"%m-%d-%Y")
    if (is.na(datedate)){break}
    date<-paste(datedate,date[2])
    date<-as.POSIXct(date)
    #waveheight taken and converted to number
    waveheight<-xmlValue(rootnode[[2]][[2]][[2]][[6]][[n]][[2]])
    waveheight<-as.numeric(str_extract(waveheight, "\\d+\\.*\\d*"))
    #line added to data frame including date, waveheight, and text link to webcam photo
    final[e,1]<-e
    final[e,2]<-attrs["href"]
    final[e,3]<-date
    final[e,4]<-waveheight
    e=e+1
    }
}
#data frame columns named and csv file created
colnames(final)<- c("number","pic_link","date","wave_height")
#formats the date column
final$date<- as.POSIXct(final$date,origin = "1970-01-01")
#only complete lines retained
final<-final[complete.cases(final),]
close(link)
write.csv(final,file = "2011_waves.csv")