### Air traffic analysis
### Tom Bird 
#to access data, download from https://zenodo.org/record/3901482#.X3Z6QJNKjOQ
# April 2019 data, when unzipped, are saved as "flightlist_20190401_20190430.csv"
# April 2020 data,  are saved as "flightlist_20200401_20190430.csv"

library(dplyr)
library(reshape2)
library(countrycode)
library(RCurl)
library(mgcv)
library(lubridate)
library(ggplot2)
library(sp)
library(rnaturalearth)
library(tidyr)
library(chron)
library(viridisLite)
library(forcats)

### Get world shapefiles for aggregation
world <- ne_countries(scale = "medium", returnclass = "sp")
  
# Rename column 
world <- world %>% mutate(ISO3 = adm0_a3) 

##Process 2019 data
Air19=read.csv("Data/OpenSky/flightlist_20190401_20190430.csv") %>% 
  select(c("origin", "destination", "lastseen", "latitude_1", "longitude_1", "latitude_2", "longitude_2")) %>%
  subset(!is.na(latitude_2) &!is.na(longitude_2) & !is.na(latitude_1) & !(is.na(longitude_1)))%>%
  mutate(Date=format(as.Date(strptime(lastseen, format="%Y-%m-%d %H:%M:%S")), format="%Y/%m/%d"))

orig.xy=Air19[,c("longitude_1","latitude_1")]
coordinates(orig.xy) =~longitude_1+latitude_1
proj4string(orig.xy)=proj4string(world)
Air19$orig.ISO=over(orig.xy, world)$adm0_a3
Air19$orig.region=over(orig.xy, world)$region_un

dest.xy=Air19[,c("longitude_2","latitude_2")]
coordinates(dest.xy) =~longitude_2+latitude_2            
proj4string(dest.xy)=proj4string(world)
Air19$dest.ISO=over(dest.xy, world)$adm0_a3
Air19$dest.region=over(dest.xy, world)$region_un



Air20=read.csv("Data/OpenSky/flightlist_20200401_20200430.csv") %>% 
  select(c("origin", "destination", "lastseen", "latitude_1", "longitude_1", "latitude_2", "longitude_2")) %>%
  subset(!is.na(latitude_2) &!is.na(longitude_2) & !is.na(latitude_1) & !(is.na(longitude_1)))%>%
  mutate(Date=format(as.Date(strptime(lastseen, format="%Y-%m-%d %H:%M:%S")), format="%Y/%m/%d"))
  
orig.xy=Air20[,c("longitude_1","latitude_1")]
coordinates(orig.xy) =~longitude_1+latitude_1
proj4string(orig.xy)=proj4string(world)
Air20$orig.ISO=over(orig.xy, world)$adm0_a3
Air20$orig.region=over(orig.xy, world)$region_un
  
dest.xy=Air20[,c("longitude_2","latitude_2")]
coordinates(dest.xy) =~longitude_2+latitude_2            
proj4string(dest.xy)=proj4string(world)
Air20$dest.ISO=over(dest.xy, world)$adm0_a3
Air20$dest.region=over(dest.xy, world)$region_un
  
# combine years
Air19.20=rbind(Air19, Air20) %>%
  na.omit() 

### Country.level
Air.orig=Air19.20 %>% 
  select(-contains("travel")) %>%
  group_by(orig.ISO, Date, .drop=TRUE) %>%
  summarise(n.orig=n()) %>%
  mutate(ISO3=orig.ISO) 
Air.dest=Air19.20 %>% 
  select(-contains("travel")) %>%
  group_by(dest.ISO, Date, .drop=TRUE) %>%
  summarise(n.dest=n()) %>%
  mutate(ISO3=dest.ISO)

Air.OD= left_join(Air.orig, Air.dest) %>%
  mutate(traffic=(n.orig+n.dest)) %>%
  na.omit() %>% 
  mutate(Date=ymd(Date),
        weekday=weekdays(Date), 
        Year=as.character(year(Date))) 

  
## Perchchg fn
Perchg=function(mod, subdat){
  subdat$predicted=predict(mod, newdata=subdat)
  with(subdat, (mean(predicted[Year=="2020"])-mean(predicted[Year=="2019"]))/mean(predicted[Year=="2019"]) )
}

## model
total.mod=list()
total.chg=list()
for(i in unique(Air.OD$ISO3)){
  subdat=subset(Air.OD, ISO3==i)
  if((nrow(subdat)>30) & length(unique(subdat$Year))>=2){
  total.mod[[i]]=gam(traffic~ Year + weekday, data=subdat)
  total.chg[[i]]=Perchg(mod=total.mod[[i]], subdat=subdat)}
print(i)
}

## Aggregate results
airtraffic.effects=data.frame(total.t=unlist(lapply(total.mod, FUN=function(x) summary(x)$p.table[["Year2020",3]])),
                              total.p=unlist(lapply(total.mod, FUN=function(x) summary(x)$p.table[["Year2020",4]])),
                              total.r=unlist(lapply(total.mod, FUN=function(x) summary(x)$r.sq)),
                              total.AIC=unlist(lapply(total.mod, AIC)),
                              total.Perc.chg=unlist(total.chg),
                              ISO3=names(total.mod))

airtraffic.effects=na.omit(airtraffic.effects)


## Write output results
write.csv(airtraffic.effects, file="Airtraffic.effects.csv")


