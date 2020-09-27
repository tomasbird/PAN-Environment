## ---------------------------
##
## Script name: urban_noise
##
## Purpose of script:
## Preliminary analysis and plotting of Urban Noise dataset for Spain towards PAN-E
##
## Author: Victor Vazquez & Christian Requena-Mesa
##
## Date Created: 31-08-2020
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

setwd("C:/Users/crequ/Desktop/PAN-E_Urban_Noise_ES")

urban_noise <- read_excel("urban_noise.xlsx", sheet ='Daily all data')


urban_noise["Julian_day"] = 30*(urban_noise["Month"]-1)+urban_noise["Day"]
urban_noise["total_day"] = (30*(urban_noise["Month"]-1)+urban_noise["Day"])/360+urban_noise["Year"]
urban_noise["Covid"] = "aa_before"
urban_noise$Covid[urban_noise$Year==2020 & urban_noise$Julian_day>75] ="after"

urban_noise = urban_noise[complete.cases(urban_noise), ]
urban_noise$ID <- as.factor(urban_noise$ID)

m7 <- lme(data=urban_noise, UrbanNoise ~ Covid, corr=corARMA(form=~1|ID/Julian_day, q=2), random= ~1|ID)

s <- summary(m7)
capture.output(s, file = "results_summary.txt")
s

p <- ggplot(urban_noise, aes(x=total_day, y=UrbanNoise, group=ID, color=ID, shape=Covid)) + 
      geom_line()+
      geom_point()+
      #scale_color_brewer(palette="Accent")+
      theme_minimal()
p
ggsave("all_locations.png", p, width = 25, height = 15, dpi = 700)

###########################################
##### Compute averages for everyday and location


mean_values = aggregate(urban_noise, list(urban_noise$Day,urban_noise$Month, urban_noise$Location, urban_noise$Covid), mean)
std_values = aggregate(urban_noise$UrbanNoise, list(urban_noise$Day,urban_noise$Month, urban_noise$Location, urban_noise$Covid), sd)

mean_values = subset(mean_values, select=c(Year, Day, Month, UrbanNoise, Julian_day, Group.3))
mean_values$sd = std_values$x
mean_values$Year[mean_values$Year < 2020] <- "2015-2019"
write.csv(mean_values,"mean_values.csv", row.names = FALSE)


