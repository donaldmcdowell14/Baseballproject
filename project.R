library(choroplethr)
library(tidyverse)
library(dplyr)
library(ggmap)
library(RCurl)
library(maps)
library(mapproj)
library(usmap)

getwd()
setwd("/Users/DonaldMcDowell/Desktop/George Mason/AIT580/project")
getwd()


dat <- read_csv("Master.csv")

usdat <- dat %>%
  filter(birthCountry == "USA")


###A little test here
test <- usdat[1:10,]

testlocation <- paste(test$birthCity, ",", test$birthState)

test_df <- as.data.frame(testlocation)
test_df$testlocation <- as.character(test_df$testlocation)
register_google(key = "AIzaSyAv3FJVlKTk8DUKsUr39mXOyJpZXAn-CJc")
test_loc_df <- mutate_geocode(test_df, testlocation)

###So this is great, my test worked. Now to get the gps locaions of all cities


loc <- paste(usdat$birthCity, ",", usdat$birthState)
loc_df <- as.data.frame(loc)
loc_df$loc <- as.character(loc_df$loc)
dist_loc <- distinct(loc_df)
register_google(key = "AIzaSyAv3FJVlKTk8DUKsUr39mXOyJpZXAn-CJc")
citygps <- mutate_geocode(dist_loc, loc)

write.csv(citygps, file = "citygps.csv")

locwithdup <- left_join(loc_df,citygps)


#locmap <- ggmap(get_map(maptype = c("satellite"))) +
#  geom_point(data = locwithdup, aes(x = lon, y = lat), color = "red")

percity <- locwithdup %>%
  group_by(loc) %>%
  summarise(count = n()) %>%
  arrange(desc(count))


percitylatlong <- left_join(citygps, percity)


map <- get_map(c(left = -125, right = -66, bottom = 24, top = 50)) %>% ggmap()
map +
  geom_point(data = percitylatlong, aes(x = lon, y = lat, size = count), color = "red", 
             alpha = .5)




#For the record, the following code to convert gps coords. to FIPS codes is not mine.
#It is courtesy of Arthur Small as found at https://gist.github.com/ramhiser/f09a71d96a4dec80994c
#This is just a test of the function
latlong2fips <- function(latitude, longitude) {
  url <- "https://geo.fcc.gov/api/census/block/find?format=json&latitude=%f&longitude=%f"
  url <- sprintf(url, latitude, longitude)
  json <- RCurl::getURL(url)
  json <- RJSONIO::fromJSON(json)
  as.character(json$County['FIPS'])
}

latlong2fips(latitude=35.139236, longitude=-97.394753)

#So as we can see the function works as intended. We will now apply this function to our
#GPS coordinates

#we also are going to toss out the NA lat long values as they throw an error.

sum(is.na(percitylatlong$lon))

percitylatlong <- percitylatlong[-c(523, 1633, 3004, 5124),]

#fips <- c()
for (i in 1:nrow(percitylatlong)){
  fipsloop <- latlong2fips(latitude = percitylatlong$lat[i], longitude = percitylatlong$lon[i])
  fips <- c(fips,fipsloop)
}

percitylatlong$fips <- fips


finaldat <- left_join(locwithdup, percitylatlong)
colnames(finaldat) <- c("loc", "lat", "lon", "value", "region")
finaldat <- finaldat[-c(745,3101,6675,7387,8038,12116,14763,15995,16114)]
regionnum <- as.numeric(finaldat$region)
finaldat$region <- regionnum




#colnames(percitylatlong) <- c("loc", "lon", "lat", "value", "region")
#percitylatlong <- percitylatlong[-c(2807,4212,4838)]
#reg <- as.numeric(percitylatlong$region)
#percitylatlong$region <- reg



usmapdat <- aggregate(percitylatlong[,4], by = list(percitylatlong$region), FUN = sum)
colnames(usmapdat) <- c("fips", "value")


usmap <- map_data("county")


#Again not my code. Courtesy of 

maps::county.fips %>%
  as_tibble %>% 
  extract(polyname, c("region", "subregion"), "^([^,]+),([^,]+)$") -> dfips

## some county maps & left_join

map_data("county") %>% 
  left_join(dfips) -> data

data2 <- left_join(data, usmapdat)


countymap <- data2 %>%   ggplot( aes(long, lat, group = group)) +
  geom_polygon(aes(fill=value)) +
  coord_map() +
  theme_void() +
  scale_fill_gradient2(low = "white",
                       mid = "blue",
                      high = "red") +
  ggtitle("US Birth Locations of MLB Players 1871-2014", 
          subtitle = "Data Courtesy of Lahman Baseball Database")
countymap

#This map is cool but skewed by counties with tons of players in select locations

mean(locwithdup$lat, na.rm = T)
mean(locwithdup$lon, na.rm = T)

usdat$lat <- locwithdup$lat
usdat$lon <- locwithdup$lon

usdebut <- usdat$debut
usdebutyr <- as.numeric(substr(usdebut,1,4))
usdat$debutyr <- usdebutyr

u1900 <- usdat %>%
  filter(debutyr <=1900) 
latmean(u1900$lat, na.rm = T)
mean(u1900$lon, na.rm = T)
##Under 1900 center of pop (COP) = (40.4515, -79.9647). This gps coordinate is in 
##Pittsburgh, PA
b0025 <- usdat %>%
  filter(debutyr > 1900) %>%
  filter(debutyr <= 1925)
mean(b0025$lat, na.rm = T)
mean(b0025$lon, na.rm = T)
##1900-1925 COP = (39.54174, -85.58261). This gps coordinate is in East Central Indiana
##This is already a significant shift in location
b2550 <- usdat %>%
  filter(debutyr > 1925) %>%
  filter(debutyr <= 1950)
mean(b2550$lat, na.rm = T)
mean(b2550$lon, na.rm = T)
##1925-1950 COP = (38.31266, -88.33684). This gps coordinate is in South Central Illinois
##Another significant shift west.
b5075 <- usdat %>%
  filter(debutyr > 1950) %>%
  filter(debutyr <= 1975)
mean(b5075$lat, na.rm = T)
mean(b5075$lon, na.rm = T)
##1950-1975 COP = (38.09142, -91.40442). This gps coordinate is in East Central Missouri
##Another shift west. An obvious trend is appearing
b7500 <- usdat %>%
  filter(debutyr > 1975) %>%
  filter(debutyr <= 2000)
mean(b7500$lat, na.rm = T)
mean(b7500$lon, na.rm = T)
##1975-2000 COP = (37.05543, -95.13702). This gps coordinate is in Southeast Kansas
##Another shift west but were also seeing a shift south. More gradual but noticable
b200025 <- usdat %>%
  filter(debutyr > 2000) %>%
  filter(debutyr <= 2025)
mean(b200025$lat, na.rm = T)
mean(b200025$lon, na.rm = T)
##2000-Present COP = (35.90363, -96.10782). This gps coordinate is in Northeastern Oklahoma
##Finally the southern trend I've been expecting
##
coplat <- c(40.4515, 39.54174, 38.31266, 38.09142, 37.05543, 35.90363)
coplong <- c(-79.9647, -85.58261, -88.33684, -91.40442, -95.13702, -95.13702)
cop <- data.frame(coplong, coplat)
cop$labels <- c("Before 1900", "1900-1925", "1925-1950", "1950-1975", "1975-2000", "2000-Present")

state <- map_data("state")

statemp <- qplot(long, lat, data=state, group=group, fill = NULL, geom="polygon")
copstate <- statemp +
  geom_point(aes(coplong, coplat,color = labels,group = NULL), size = 1.5,data=cop) +
  theme(legend.position = c(0.86, 0.25), 
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  ggtitle("Center of Birth Locations of MLB players over Time") +
  scale_y_continuous(name = "") +
  scale_x_continuous(name = "")
copstate



#Lets just make a coropleth map by state counts

usdat$count <- rep(1,16504)
statedat <- aggregate(usdat[,25], by = list(usdat$birthState), FUN = sum)

colnames(statedat) <- c("state", "value")

statemap <- plot_usmap(data = statedat, regions = "states", values = "value") +
  scale_fill_continuous(name = "Number of Players", low = "white", high = "red") +
  theme(legend.position = "right") +
  ggtitle("US Birth States of MLB players 1871-2014",
          subtitle = "Data Courtesy of Lahman Baseball Database")
statemap


bardat <- usdat %>%
  filter(birthState == "CA" |
         birthState == "PA" |
         birthState == "NY" |
         birthState == "IL" |
         birthState == "OH" |
         birthState == "TX" |
         birthState == "MA" |
         birthState == "MO" |
         birthState == "FL" |
         birthState == "MI" |
        birthState == "NJ")



statebar <- ggplot(data = bardat, aes(x=birthState)) +
  geom_bar(fill = "steelblue") +
  scale_x_discrete(name = "Birth State") +
  scale_y_continuous(name = "Frequency", breaks = seq(0,2500,500), limits = c(0,2500)) +
  ggtitle("Birth States of MLB Players 1871-2014", subtitle = "Data Courtesy of Lahman Baseball Database")
statebar


###Now I'm just going to explore some other qualities of the data set

r <- dat %>%
  filter(bats == "R")
mean(as.numeric(r$weight), na.rm = T)

l <- dat %>%
  filter(bats == "L")
mean(as.numeric(l$weight), na.rm = T)


debut <- dat$debut
debutyr <- as.numeric(substr(debut,1,4))
dat$debutyr <- debutyr


###I'm going to toss out row 8416. It is a player who had one at bat and is 3'7''
dat <- dat %>%
  filter(height>50)

plot(dat$debutyr, dat$height, na.rm = T)
htyr <- ggplot(data = dat) +
  geom_jitter(aes(x = debutyr, y = height)) +
  scale_y_continuous(name = "Height (in)", breaks = seq(60,85, 5), limits = c(60,85)) +
  scale_x_continuous(name = "MLB Debut Year", breaks = seq(1870,2020,15)) +
  ggtitle("Height by Debut Year", subtitle = "Data Courtesy of Lahman Baseball Database")
htyr




plot(dat$debutyr, dat$weight)

wtyr <- ggplot(data = dat) +
  geom_jitter(aes(x = debutyr, y = weight)) +
  scale_y_continuous(name = "Weight (lb)", breaks = seq(100,325, 25), limits = c(120,325)) +
  scale_x_continuous(name = "MLB Debut Year", breaks = seq(1870,2020,15)) +
  ggtitle("Weight by Debut Year", subtitle = "Data Courtesy of Lahman Baseball Database")
wtyr





##Interesting possible correlation here
cor.test(dat$debutyr, dat$height)
cor.test(dat$debutyr, dat$weight)
##Pretty strong correlation actually

usdat$batbin <- ifelse(usdat$bats == "R", 1, 0)
reg <- lm(usdat$weight~usdat$debutyr+usdat$batbin)
summary(reg)

cor.test(usdat$weight, usdat$batbin)

older <- dat %>%
  filter(debutyr < 1950)
newer <- dat %>%
  filter(debutyr >= 1950)
t.test(older$weight, newer$weight)
t.test(older$height, newer$weight)
###we can see from the t-test that there is a significant difference in the weight between
###players debuting before and after 1950. Same results from t-test regarding height.


usdat$throws <- as.factor(usdat$throws)
usdat$bats <- as.factor(usdat$bats)





bat <- filter(usdat, !is.na(bats)) %>% 
ggplot() +
  geom_boxplot(aes(x = bats, y = weight), fill = "steelblue") +
  scale_x_discrete(name = "Batting Handedness") +
  scale_y_continuous(name = "Weight (lbs)") +
  ggtitle("Weight by Handedness of MLB players 1871-2015",
          subtitle = "Data Courtesy of Lahman Baseaball Database")
bat



