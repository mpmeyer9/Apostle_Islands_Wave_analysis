# Apostle_Islands_Wave_analysis
This folder is provided as an example of my data manipulation work done in R. The scripts included all relate to a project
to predict waveheights at a Sea Caves location in the Apostle Islands that is popular with kayakers based on other
available sources of data including wave heights measured by buoys at other locations in Lake Superior, wind data measured
at a nearby weather station, and the wave height predicted at the location by a wave model operated by the Great Lakes
Environmental Research Laboratory. Data from all of these sources was available in near real time through the internet, but
historical records of this data were used in the analysis.
Link to the study location for reference: https://goo.gl/maps/AgpZPcg9BBB2

The project approach was to correlate the data from the various sources to the wave height measured at the location by a
sensor installed at the location during the summers of 2015 and 2016. The included scripts produce graphs illustrating
the relationship between the specified variable and the waveheight and a CSV quantitatively detailing the correlation.
Wind direction was found to be a significant variable determining the relationship of other variables to waveheight (since
the study location was near the shore, other conditions being constant waves were generally larger when winds were coming 
from the direction of open water as opposed to the direction of shore or a direction that was "blocked" by nearby islands). 
Because of this, the data was organized into wind direction categories and analyzed within those categories.

The included scripts are not nearly a complete presentation of the analysis done for this project but are provided as a 
means of demonstrating my ability to clean, organize, relate, and present data in R.

The figures created by the scripts are shown below:

![alt text](https://github.com/mpmeyer9/Apostle_Islands_Wave_analysis/blob/master/Figures/Buoy.png)
![alt text](https://github.com/mpmeyer9/Apostle_Islands_Wave_analysis/blob/master/Figures/Wind%20speed.png)

Included scripts (necessary add on packages listed below):

Records_compiler - Loads and compiles into a single data frame the data included in the "Source Data" folder, 
records from the different sources are related by date and time

Waveheight vs Wind Speed - Uses the data frame created in Records_Compiler (runs that script 
if the data frame is not already created) to graph and compute the correlation between waveheights recorded at the 
study location with windspeeds recorded at a nearby weather station. The data is divided by wind direction at the time
of the wave height recorded, and correlations are created for each wind direction category.

Waveheight vs NOAA buoy wave height - Uses the data frame created in Records_Compiler (runs that script 
if the data frame is not already created) to graph and compute the correlation between waveheights recorded at the 
study location with waveheights recorded at a NOAA buoy in western Lake Superior. The data is divided by wind direction 
at the time of the wave height recorded, and correlations are created for each wind direction category.

Image and data scrape - This a script that isn't use directly with the above listed scripts but is included to
demonstrate web scraping that was done to create some of the data used for this project. It draws waveheight records
from a website and writes them to a CSV file along with the date, time, and a link to a photo taken by a webcam at the
study location at the time of the waveheight measurement

Packages used and necssary to run the above scripts:
XML
stringr
R.matlab
data.table
dplyr
lubridate
ggplot2
