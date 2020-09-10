## ---------------------------
##
## Script name: chlorophil worldwide 2015:2020
##
## Purpose of script:
## Preliminary analysis and plotting of Worldwide Chlorophil towards PAN-E
##
## Author: Victor Manuel Vazquez Manzanares & Christian Requena-Mesa
##
## Date Created: 09-08-2020
##
##
## Email: crequ@bgc-jena.mpg.de
##
## ---------------------------
##
## Notes: requires nlme and ggplot2
##        
##        Based on method 7 of PAN-E demo.pdf
##   
##
## ---------------------------

library("nlme")
library("ggplot2")
library("tidyr")

setwd("C:/Users/crequ/Desktop/PAN-E_chlorophil")


#####################################################################
#####################################################################
####
####  Merge all .csv into a single tidy data.frame 
####  Takes long due to unoptimized code: do once, 
####  save and load next times
####  will need some amount of RAM
####
####

top_lat = 400
lowest_lat = 1600

chlorophil = data.frame()#Chl=NA,Month=NA,Year=NA,ID=NA)
for(year in 2015:2020){
  data_dir = paste("data",year,sep="/")
  file_paths = list.files(data_dir)
  n_months = length(file_paths)
  n_lats = lowest_lat-top_lat
  for(month in 1:length(file_paths)){
    print(paste("Merging month ",month, " of year ",year, sep=""))
    chl_raw = read.csv(paste(data_dir, file_paths[month], sep="/"), header = TRUE)
    
    chl_month1 = data.frame()
    for( column in 2:1802){
      if(column%%100==0){
        print(paste("Longitude ", column/10," of 360", sep=""))
      }
      single = data.frame(Chl=chl_raw[top_lat:lowest_lat,column], Month=month, Year=year,LocationID=(column-1)*n_lats+top_lat:lowest_lat,Lat=-((top_lat:lowest_lat)+top_lat-900)/10, Lon=(column)/10-180)
      chl_month1 = rbind(chl_month1,single)
    }
    chlorophil= rbind(chlorophil, chl_month1)
    chl_month2 = data.frame()
    for( column in 1802:3601){
      if(column%%100==0){
        print(paste("Longitude ", column/10," of 360", sep=""))
      }
      single = data.frame(Chl=chl_raw[top_lat:lowest_lat,column], Month=month, Year=year,LocationID=(column-1)*n_lats+top_lat:lowest_lat,Lat=-((top_lat:lowest_lat)+top_lat-900)/10, Lon=(column)/10-180)
      
      chl_month2 = rbind(chl_month2,single)
    }
    chlorophil= rbind(chlorophil, chl_month2)
  }
}

#Remove no data
chlorophil = chlorophil[complete.cases(chlorophil), ]
chlorophil = chlorophil[!chlorophil$Chl==99999,]
chlorophil = chlorophil[complete.cases(chlorophil), ]
write.csv(chlorophil,"data/tidy_noNa_-50_50_lat_lon_2015-2020.csv", row.names = FALSE)


#####################################################################
#####################################################################
####
####  Analysis according to method 7. Autocorr on Months and Latitude
####  Dataset is huge, will take long
####

chlorophil = read.csv("data/tidy_noNa_-50_50_lat_lon_2015-2020.csv", header = TRUE)

chlorophil["Covid"] = "aa_before"
chlorophil$Covid[chlorophil$Year==2020] ="after"

chlorophil$LocationID <- as.factor(chlorophil$LocationID)
chlorophil$Chl <- as.numeric(chlorophil$Chl)

#########
# If lme() runs out of RAM, do sample fewer datapoints
small_chl = chlorophil[sample(1:length(chlorophil$Chl), 1000000),]

m7 <- lme(data=small_chl, Chl ~ Covid, corr=corARMA(form=~1|Lat/Month, q=2), random= ~1|Lat)

s <- summary(m7)
capture.output(s, file = "results_summary.txt")
s
