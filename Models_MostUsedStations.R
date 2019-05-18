###########################
# Quantile regression demo
# Divvy bike data
# BHO
# 5.18.2019
###########################

#Load common functions
source('Functions.R')

#Load Libraries
packages <- c('actuar', 'ggplot2', 'cowplot', 'ggpubr', 'bikedata', 'revgeo',
              'sp', 'geosphere', 'ggmap', 'maps', 'mapdata', 'quantreg')
package.check(packages)

#Dummy data examples
dat <- rllogis(n = 1000, shape = 2.28, scale = 8.99)
x <- seq(0,80, length.out = 1000)
df <- data.frame(x = x, y = dllogis(x, shape = 2.28, scale = 8.99))

Dispersal.example <-
ggplot() +
  geom_line(data = df, aes(x = x, y = y), color = '#00BBBB', lwd = 1) + 
  coord_cartesian(xlim = c(0,80))+
  scale_x_continuous(breaks = c(0,70), labels = c('Home', 'Far away'), name = 'Location') +
  scale_y_continuous(name = 'Density', breaks = c(0, 0.08), labels = c('Low', 'High')) +
  theme(axis.ticks = element_blank())

#Print plot
# tiff("Figures/Dispersal_Example.tiff", width = 4, height = 4, units = 'in', res=200)
# Dispersal.ex
# dev.off()


ols <- data.frame(x = rnorm(n = 5, mean = 50, sd = 10))
ols$y <- ols$x*3 + rnorm(n = 5, mean = 0, sd = 50)

residual.ex <- ggplot() +
  geom_point(data = ols, aes(x = x, y=y), color = 'grey40', size = 5)+
  geom_smooth(data = ols, aes(x = x, y = y), method = 'lm', se = FALSE, color = '#00BBBB')


# tiff("Figures/OLS_Example.tiff", width = 8, height = 4, units = 'in', res=200)
# residual.ex
# dev.off()


ols <- data.frame(x = rnorm(n = 100, mean = 50, sd = 10))
ols$y <- ols$x*3 + rnorm(n = 100, mean = 0, sd = 10)*(ols$x/2)

heterogeneous.variance.ex <- ggplot() +
  geom_point(data = ols, aes(x = x, y=y), color = 'grey40', size = 2)

tiff("Figures/VarIncrease_Example.tiff", width = 4, height = 4, units = 'in', res=200)
heterogeneous.variance.ex
dev.off()


#Divvy bike example
#Data provided in DivvyBikeData.csv

#Download Divvy data from 2016
bike.db <- file.path (tempdir (), "bikedb.sqlite")

dir.create ("bikedata-ch") # will create at "."
dl_bikedata(city = "ch", data_dir = "bikedata-ch", dates = "2016") # then works
store_bikedata(bikedb = "bikedb", data_dir = "bikedata-ch")
index_bikedata_db (bikedb = "bikedb")

bike_tripmat (bikedb = "bikedb", city = 'ch', long = TRUE)

#Which stations are used the most? The least?
stns <- bike_stations (bikedb = "bikedb", city = 'ch')
ntrips <- bike_tripmat (bikedb = "bikedb", city = 'ch', long = TRUE)

trips.start <- aggregate(ntrips$numtrips ~ ntrips$start_station_id, FUN = sum)
colnames(trips.start) <- c('station_id', 'numtrips.start')

trips.stop <- aggregate(ntrips$numtrips ~ ntrips$end_station_id, FUN = sum)
colnames(trips.stop) <- c('station_id', 'numtrips.stop')


trips <- plyr::join(trips.start, trips.stop, by = 'station_id')
trips$netflow <- trips$numtrips.stop - trips$numtrips.start
trips$use <- trips$numtrips.start + trips$numtrips.stop

#Add in covariates

#Station location
names(stns)[3] <- 'station_id'
stns <- stns[,c(3,5,6)]
stns.ag <- aggregate(stns[c('longitude', 'latitude')], by = stns['station_id'], 
                     FUN = mean)

trips <- plyr::join(stns.ag, trips, by = 'station_id')

#Add zip - if try too many records, photon will throttle down speed
zip<-revgeo(longitude = trips$longitude, latitude = trips$latitude, output = 'frame', item = 'zip')
zip.use <- zip$zip
trips <- cbind(trips, zip.use)
colnames(trips)[8] <- 'zip'
trips$zip <- as.numeric(as.character(trips$zip))

#Area disadvantage index (higher = poorer)
adi <- read.csv(file = 'ADI.csv')
colnames(adi)[3] <- 'zip'
adi <- adi[, c(3,6,7)]

adi.agg <- aggregate(as.numeric(as.character(adi$ADI_STATERNK)) ~ adi$zip, FUN = mean)
colnames(adi.agg) <- c('zip', 'adi_state_rank')
trips2 <- plyr::join(trips, adi.agg, by = 'zip')

adi.agg <- aggregate(as.numeric(as.character(adi$ADI_NATRANK)) ~ adi$zip, FUN = mean)
colnames(adi.agg) <- c('zip', 'adi_national_rank')
trips <- plyr::join(trips2, adi.agg, by = 'zip')

#Restaurants nearby
food <- read.csv("Chicago_Restaurant.csv")
food <- food[,c(3, 14, 15)]
colnames(food) <- c('id', 'latitude', 'longitude')
food.ag.long <- aggregate(food$longitude ~ food$id, FUN = mean)
food.ag.lat <- aggregate(food$latitude ~ food$id, FUN = mean)

colnames(food.ag.lat) <- c('id', 'latitude')
colnames(food.ag.long) <- c('id', 'longitude')
food.ag <- plyr::join(food.ag.lat, food.ag.long, by = "id")

#Within buffer

food.sp <- food.ag
coordinates(food.sp) <- ~longitude + latitude
stns.sp <- stns.ag
coordinates(stns.sp) <- ~longitude + latitude

stn.dat <- data.frame(food_min_dist = as.numeric(),
                         food_500 = as.numeric())

for(i in 1:nrow(stns.sp)){
  d <- distGeo(stns.sp[i,], food.sp)
  food_min_dist <- min(d)
  food_500 <- plyr::count(d < 500)
  rec.dat <- cbind(food_min_dist, food_500[2,2])
  stn.dat <-rbind(stn.dat, rec.dat)
}

stns.ag <- cbind(as.data.frame(stns.ag), stn.dat)
colnames(stns.ag)[5] <- 'food_500'

#Metro stops
cta_bus <- read.csv('CTA_Stops.csv')
cta_bus.sp <- cta_bus
coordinates(cta_bus.sp) <- ~longitude + latitude

stn.dat <- data.frame(bus_min_dist = as.numeric(),
                      bus_500 = as.numeric())

for(i in 1:nrow(stns.sp)){
  d <- distGeo(stns.sp[i,], cta_bus.sp)
  bus_min_dist <- min(d)
  bus_500 <- plyr::count(d < 500)
  rec.dat <- cbind(bus_min_dist, bus_500[2,2])
  stn.dat <-rbind(stn.dat, rec.dat)
}

stns.ag <- cbind(as.data.frame(stns.ag), stn.dat)
colnames(stns.ag)[7] <- 'bus_500'

trips <- plyr::join(trips, stns.ag, by = 'station_id')

#Clean data

#Put in 0's for _500 NAs
trips$food_500 <- ifelse(is.na(trips$food_500), 0, trips$food_500)
trips$bus_500 <- ifelse(is.na(trips$bus_500), 0, trips$bus_500)

#Impute mean for adi if missing
trips$adi_national_rank <- ifelse(is.na(trips$adi_national_rank), 
                                  mean(trips$adi_national_rank, na.rm = TRUE),
                                  trips$adi_national_rank)
trips$adi_state_rank <- ifelse(is.na(trips$adi_state_rank), 
                                  mean(trips$adi_state_rank, na.rm = TRUE),
                                  trips$adi_state_rank)
trips <- trips[,-c(11,12)]

#write.csv(trips, "DivvyBikeData2.csv", row.names = FALSE)

#Visualize stations

#Plug in google API key here
#register_google(key = "")

sf <- get_map(location = "chicago", maptype = "terrain", source = 'google', zoom = 12)
sf <- get_map(location = c(left = -87.80287, bottom = 41.73665, right = -87.54939, top = 42.064),
              maptype = "terrain", source = 'google', zoom = 12)

map <- ggmap(sf, extent = 'device')

station <-
 map + geom_point(data = trips, aes( x = longitude, y = latitude),
                  size = 1.2, color = '#7F0000')+
  theme_nothing()

#Print
# tiff("Figures/Stations.tiff", width = 4, height = 4, units = 'in', res=200)
# station
# dev.off()

cta_bus$type <- rep('Transit stop', nrow(cta_bus))
cta_bus <- cta_bus[,c(19, 18, 25)]
food.ag$type <- rep('Restaurant', nrow(food.ag))
food.ag <- food.ag[,-1]
food.bus <- rbind(food.ag, cta_bus)

food.bus <-
  map + geom_point(data = food.bus, aes( x = longitude, y = latitude, 
                                      color = type), size = 1.2)+
    scale_color_manual(values = c('#004448', '#00BBBB'))+
    theme_nothing()

#Print
  # tiff("Figures/FoodBus.tiff", width = 4, height = 4, units = 'in', res=200)
  # food.bus
  # dev.off()

#Station use by area
  state_df <- map_data("state")
  il_df <- subset(state_df, region == "illinois")
  
  counties <- map_data("county")
  il_county <- subset(counties, region == "illinois")
  
map + geom_point(data = trips,  aes(x = longitude, y = latitude, color = use,
                 size = use), alpha = 0.4) +
  scale_colour_gradient(name = '# Uses', low="purple", high="red")
                   

#Collinearlity check
cor(trips[,9:14])

#Summary Plots

#Plots of Y ~ X
adi<-
  ggplot(data = trips, aes(y = use, x = adi_state_rank))+
  geom_jitter(color = 'grey50', width = 0.05)+
  xlab('ADI rank') + 
  ylab('Station use')

bus<-
  ggplot(data = trips, aes(y = use, x = bus_500))+
  geom_jitter(color = 'grey50', width = 0.5)+
  xlab('Transit stops in 500 m') + 
  ylab('Station use')

bus.n<-
  ggplot(data = trips, aes(y = use, x = bus_min_dist))+
  geom_jitter(color = 'grey50', width = 0.5)+
  xlab('Distance (m) to nearest transit stop') + 
  ylab('Station use')

food<-
  ggplot(data = trips, aes(y = use, x = food_500))+
  geom_jitter(color = 'grey50', width = 0.5)+
  xlab('Restaurants in 500 m') + 
  ylab('Station use')

food.n<-
  ggplot(data = trips, aes(y = use, x = food_min_dist))+
  geom_jitter(color = 'grey50', width = 0.5)+
  xlab('Distance (m) to nearest restaurant') + 
  ylab('Station use')

graphics::hist(trips$use, breaks = 50, xlab = 'Station use')

#Analyze

#USE
t <- seq(0.1, 0.9,  0.1)

full.lm <-lm( use ~ adi_state_rank + food_min_dist + food_500 + bus_min_dist + bus_500,
            data = trips)
full <- rq(use ~ adi_state_rank + food_min_dist + food_500 + bus_min_dist + bus_500,
                    data = trips, tau= t)
full.1 <-rq(use ~ adi_state_rank + food_min_dist + food_500 + bus_min_dist + bus_500,
          data = trips, tau=c(.1))
full.9 <-rq(use ~ adi_state_rank + food_min_dist + food_500 + bus_min_dist + bus_500,
            data = trips, tau=c(.9))

full.5 <-rq(use ~ adi_state_rank + food_min_dist + food_500 + bus_min_dist + bus_500,
            data = trips, tau=c(.5))

s.5 <- summary(full.5)
s.5<-s.5$coefficients
s.9 <- summary(full.9)
s.9<-s.9$coefficients

confint(full)
anova(full.1, full.5)
anova(full.1, full.9)
anova(full.5, full.9)

sumqr<-summary(full, se='boot', R = 1000, covariance = T)

plot(summary(full))

#Plots of models
quantile(trips$use, probs = .9)

trips$color.code<-ifelse(trips$use > quantile(trips$use, probs = .9), 1,
                         ifelse(trips$use < quantile(trips$use, probs = .1), 2, 0))

trips$color.code<-as.factor(trips$color.code)

new.dat<-expand.grid(adi_state_rank = mean(trips$adi_state_rank),
                     food_min_dist = mean(trips$food_min_dist),
                     food_500 = seq(0,700, 50),
                     bus_min_dist = mean(trips$bus_min_dist),
                     bus_500 = mean(trips$bus_500),
                     color.code='0')

conf_interval.9 <- predict(full.9, newdata=new.dat, interval="confidence", level = 0.95, se.fit=T)
conf_interval.1 <- predict(full.1, newdata=new.dat, interval="confidence", level = 0.95, se.fit=T)

new.dat<-cbind(new.dat,conf_interval.9)
new.dat<-cbind(new.dat,conf_interval.1)
colnames(new.dat)[10:12] <- c('fit.1', 'lower.1', 'higher.1')

bike_food500 <-
  ggplot(data=trips, aes(x=food_500, y=use, shape=color.code, color=color.code)) + 
  geom_jitter(width=3)+
  geom_line(data=new.dat, aes(x=food_500, y=lower), color='black', lty='dashed')+
  geom_line(data=new.dat, aes(x=food_500, y=higher), color='black', lty='dashed')+
  geom_line(data=new.dat, aes(x=food_500, y=fit), color='black')+
  geom_line(data=new.dat, aes(x=food_500, y=lower.1), color='#004448', lty='dashed')+
  geom_line(data=new.dat, aes(x=food_500, y=higher.1), color='#004448', lty='dashed')+
  geom_line(data=new.dat, aes(x=food_500, y=fit.1), color='#004448')+
  scale_shape_manual(values = c(3,19, 15))+
  scale_color_manual(values = c('grey50','#7F0000', '#00BBBB' ))+
  scale_x_continuous(name='Restaurants in 500 m')+
  scale_y_continuous(name='Station use')+
  guides(shape=F, color=F)
  
  
new.dat<-expand.grid(adi_state_rank = seq(4.8, 6.7, 0.2),
                     food_min_dist = mean(trips$food_min_dist),
                     food_500 = mean(trips$food_500),
                     bus_min_dist = mean(trips$bus_min_dist),
                     bus_500 = mean(trips$bus_500),
                     color.code='0')

conf_interval.9 <- predict(full.9, newdata=new.dat, interval="confidence", level = 0.95, se.fit=T)
conf_interval.1 <- predict(full.1, newdata=new.dat, interval="confidence", level = 0.95, se.fit=T)

new.dat<-cbind(new.dat,conf_interval.9)
new.dat<-cbind(new.dat,conf_interval.1)
colnames(new.dat)[10:12] <- c('fit.1', 'lower.1', 'higher.1')


bike_adi <-
  ggplot(data=trips, aes(x=adi_state_rank, y=use, shape=color.code, color=color.code)) + 
  geom_jitter(width=0.1)+
  geom_line(data=new.dat, aes(x=adi_state_rank, y=lower), color='black', lty='dashed')+
  geom_line(data=new.dat, aes(x=adi_state_rank, y=higher), color='black', lty='dashed')+
  geom_line(data=new.dat, aes(x=adi_state_rank, y=fit), color='black')+
  geom_line(data=new.dat, aes(x=adi_state_rank, y=lower.1), color='#004448', lty='dashed')+
  geom_line(data=new.dat, aes(x=adi_state_rank, y=higher.1), color='#004448', lty='dashed')+
  geom_line(data=new.dat, aes(x=adi_state_rank, y=fit.1), color='#004448')+
  scale_shape_manual(values = c(3,19, 15))+
  scale_color_manual(values = c('grey50','#7F0000', '#00BBBB' ))+
  scale_x_continuous(name='ADI rank')+
  scale_y_continuous(name='Station use')+
  guides(shape=F, color=F)

 
new.dat<-expand.grid(adi_state_rank = mean(trips$adi_state_rank),
                     food_min_dist = mean(trips$food_min_dist),
                     food_500 = mean(trips$food_500),
                     bus_min_dist = seq(0,6000,500),
                     bus_500 = mean(trips$bus_500),
                     color.code='0')

conf_interval.9 <- predict(full.9, newdata=new.dat, interval="confidence", level = 0.95, se.fit=T)
conf_interval.1 <- predict(full.1, newdata=new.dat, interval="confidence", level = 0.95, se.fit=T)

new.dat<-cbind(new.dat,conf_interval.9)
new.dat<-cbind(new.dat,conf_interval.1)
colnames(new.dat)[10:12] <- c('fit.1', 'lower.1', 'higher.1')

bike_dbus <-
  ggplot(data=trips, aes(x=bus_min_dist, y=use, shape=color.code, color=color.code)) + 
  geom_jitter(width=3)+
  geom_line(data=new.dat, aes(x=bus_min_dist, y=lower), color='black', lty='dashed')+
  geom_line(data=new.dat, aes(x=bus_min_dist, y=higher), color='black', lty='dashed')+
  geom_line(data=new.dat, aes(x=bus_min_dist, y=fit), color='black')+
  geom_line(data=new.dat, aes(x=bus_min_dist, y=lower.1), color='#004448', lty='dashed')+
  geom_line(data=new.dat, aes(x=bus_min_dist, y=higher.1), color='#004448', lty='dashed')+
  geom_line(data=new.dat, aes(x=bus_min_dist, y=fit.1), color='#004448')+
  scale_shape_manual(values = c(3,19, 15))+
  scale_color_manual(values = c('grey50','#7F0000', '#00BBBB' ))+
  scale_x_continuous(name='Dist (m) to nearest transit')+
  scale_y_continuous(name='Station use')+
  guides(shape=F, color=F)

#Print
# tiff("Figures/qr_Divvy.tiff", width = 10, height = 3, units = 'in', res=200)
# plot_grid(bike_adi, bike_food500, bike_dbus, ncol = 3)
# dev.off()