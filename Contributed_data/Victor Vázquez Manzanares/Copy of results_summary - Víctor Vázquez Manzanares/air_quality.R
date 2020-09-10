## ---------------------------
##
## Script name: air_quality
##
## Purpose of script:
## Preliminary analysis and plotting of Air Quality dataset worldwide 608 cities towards PAN-E
##
## Author: Victor Vazquez & Christian Requena-Mesa
##
## Date Created: 16-08-2020
##
## Email: vazquez@uma.es
## Email: crequ@bgc-jena.mpg.de
##
## ---------------------------
##
## Notes: requires nlme, readxl and ggplot2
##        
##        Based on method 7 of PAN-E demo.pdf
##   
##
## ---------------------------

library("nlme")
library("readxl")
library("ggplot2")
library("stringr")

setwd("C:/Users/crequ/Desktop/PAN-E_Air_quality")

data_dir = "data/"
file_paths = list.files(data_dir)

###################################
##### Shaping the dataset
air_quality = data.frame()
for(file in file_paths){
  print(file)
  quarterly_data = read.csv(paste(data_dir, file, sep="/"), header = TRUE,skip = 4)
  air_quality = rbind(air_quality,quarterly_data)
}

###################################
##### Data cleaning. variable maker

air_quality$Specie = as.factor(air_quality$Specie)
air_quality$City = as.factor(air_quality$City)
species = levels(air_quality$Specie)
cities = levels(air_quality$City)
write.csv(cities,"cities.csv", row.names = FALSE)

print(length(cities))
print(length(species))

Date = str_split_fixed(air_quality$Date, "-", 3)

air_quality$year = as.numeric(Date[,1])
air_quality$month = as.numeric(Date[,2])
air_quality$julian_day = (as.numeric(Date[,2])-1)*30+as.numeric(Date[,3])
air_quality$julian_day = (as.numeric(Date[,2])-1)*30+as.numeric(Date[,3])
air_quality$total_day = ((as.numeric(Date[,2])-1)*30+as.numeric(Date[,3]))/360+as.numeric(Date[,1])

air_quality$median[air_quality$median == 0] = NA
air_quality = air_quality[complete.cases(air_quality), ]

air_quality$Covid = "before"
air_quality$Covid[air_quality$year==2020 & air_quality$month>2] ="after" #Covid After from March 1st

###############################################
#Loop trough species. Analysis and plotting

for(specie in c('aqi','co','no2','o3','pm1','pm10','pm25','so2')){
  print(paste("Analyzing ",specie,sep=""))
  air_quality_specie = air_quality[air_quality$Specie %in% specie,]
  
  m7 <- lme(data=air_quality_specie, median ~ Covid, corr=corARMA(form=~1|City/julian_day, q=2), random= ~1|City)
  capture.output(summary(m7), file = paste("results/",specie,"_summary.txt",sep=""))
  
  #For plotting, we select 30 random cities if more than 30
  aq_species_cities = levels(air_quality_specie$City)
  if(length(aq_species_cities)> 30 & specie != 'aqi'){
    print("More than 30 cities. Decimating to 30 for plotting.")
    air_quality_specie_cities = data.frame()
    air_quality_specie_cities = air_quality_specie[air_quality_specie$City %in% sample(aq_species_cities,30),]
  }
    
  p <- ggplot(air_quality_specie_cities, aes(x=total_day, y=median, shape=Covid, group=City, color=City)) + 
      geom_point()+
      geom_line()+
      theme_minimal()
  ggsave(paste("figures/",specie,".png",sep=""), p, width = 25, height = 15, dpi = 200)           
}