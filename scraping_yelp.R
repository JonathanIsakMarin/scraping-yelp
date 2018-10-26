library(tidyverse)
library(dplyr)
library(rvest)
library(purrr)
library(XML)
library(htmltab)
library('xml2')
install.packages("tidyverse")


#---------------------------------#
##Scraping www.yelp.dk##
##Running several statistical learning models using data from yelp.dk
#---------------------------------#

css.selector = ".biz-name"
#Creating a vector of links with all pages we need to scrape
base.link = "https://www.yelp.dk/search?find_desc=Restauranter&find_loc=K?benhavn&start="
pages = seq(0, 100, by = 10)
yelp.links.1 = paste0(base.link, pages)
yelp.links.1

#Creating a function that runs through every restaurant name on the given page 
#and extracts the links to each restaurant
extract_function = function(yelp.links.1, css.select){
  out = yelp.links.1 %>% 
    read_html(encoding = "UTF-8") %>% 
    html_nodes(css = css.selector) %>%
    html_attr(name = "href")
  return(out)
}

#We run the function on all the pages to extract links to restaurants and put them in an empty vector. 
extracted.links = yelp.links.1 %>%
  map(extract_function) 
print(extracted.links)

#The links do not include "yelp.dk" in front of them. We need to use a string command to add
#this to all the links. Otherwise the links will be dead.
yelp.links.2 = unlist(extracted.links)
yelp.links.2

final.links = paste0("https://www.yelp.dk", yelp.links.2)
final.links

#scrape yelp.dk for the price, rating, name, number of reviews and location of each restaurant and stores these data in a data frame
scrape_yelp = function(final.links){
  parsed.link = final.links %>% 
    read_html(encoding = "UTF-8")
  rating = parsed.link %>% 
    html_nodes("#wrap > div.biz-country-dk > div > div.top-shelf > div > div.biz-page-header.clearfix > div.biz-page-header-left > div.biz-main-info.embossed-text-white > div.rating-info.clearfix > div.biz-rating.biz-rating-very-large.clearfix > div > i") %>% 
    html_attr(name = "title")
  if (length(rating) == 0){ rating = NA }
  restaurant = parsed.link %>% 
    html_nodes(".shortenough") %>% 
    html_text()
  if (length(restaurant) == 0){ restaurant = NA }
  address = parsed.link %>% 
    html_nodes(".street-address span:nth-child(1)") %>% 
    html_text()
  if (length(address) == 0){ address = NA }
  price = parsed.link %>% 
    html_nodes(".price-description") %>% 
    html_text()
  if (length(price) == 0){ price = NA }
  count.reviews = parsed.link %>% 
    html_nodes(".biz-rating-very-large .review-count") %>% 
    html_text()
  if (length(count.reviews) == 0){ count.reviews = NA }
  return(data.frame(rating=rating, restaurant=restaurant, address=address, price=price,
                    count.reviews=count.reviews))
}

firsthalf.df = final.links %>%
  map_df(scrape_yelp)
View(firsthalf.df)

#We now scrape yelp.dk for all of the remaining information about each restaurant and store the data in a second data frame
scrape_yelp = function(link){
  parsed.link = link %>% 
    read_html(encoding = "UTF-8")
  list = parsed.link %>% 
    html_nodes(".ylist .attribute-key") %>% 
    html_text()
  if (length(list) == 0){ list = NA }
  address = parsed.link %>% 
    html_nodes(".street-address span:nth-child(1)") %>% 
    html_text()
  if (length(address) == 0){ address = NA }
  janej = parsed.link %>% 
    html_nodes(".ylist dd") %>% 
    html_text()
  if (length(janej) == 0){ janej = NA }
  return(data.frame(list=list, janej = janej, address=address))
}

secondhalf.df = final.links %>%
  map_df(scrape_yelp)
View(secondhalf.df)

#making sure that the names are unique occurences as characters:
nn<-reshape(secondhalf.df, v.names = "janej", timevar="list",idvar="address",direction="wide")
names(nn)[-1]<-as.character(unique(secondhalf.df$list))

#We now change the orientation of dataframe --> long to wide. "Each row is an observation, and each column a variable"
wide.df = secondhalf.df %>% spread(list, janej)
View(wide.df)

#Now we join the two dataframes by restaurant address (identifier variable) 
full.data = left_join(firsthalf.df, nn, by=c("address"="address"))

#We now have a full data frame of the scraped data
View(full.data)


#-----------------------------------------------
## TIDYING/CLEANING THE DATASET ##
#-----------------------------------------------

#New column names
colnames(full.data)[1] = "rating"
colnames(full.data)[2] = "name"
colnames(full.data)[3] = "address"
colnames(full.data)[4] = "price"
colnames(full.data)[5] = "reviews"
colnames(full.data)[6] = "reservations"
colnames(full.data)[7] = "delivery"
colnames(full.data)[8] = "takeaway"
colnames(full.data)[9] = "takescard"
colnames(full.data)[10] = "mealtype"
colnames(full.data)[11] = "parking"
colnames(full.data)[12] = "kidfriendly"
colnames(full.data)[13] = "groupfriendly"
colnames(full.data)[14] = "dresscode"
colnames(full.data)[15] = "atmosphere"
colnames(full.data)[16] = "noise"
colnames(full.data)[17] = "alcohol"
colnames(full.data)[18] = "sitoutside"
colnames(full.data)[19] = "wifi"
colnames(full.data)[20] = "tv"
colnames(full.data)[21] = "serving"
colnames(full.data)[22] = "catering"
colnames(full.data)[23] = "drivethru"
colnames(full.data)[24] = "pokestop"
colnames(full.data)[25] = "handicapfriendly"
colnames(full.data)[26] = "dogsallowed"
colnames(full.data)[27] = "unidentified"
colnames(full.data)[28] = "dancing"
colnames(full.data)[29] = "happyhour"
colnames(full.data)[30] = "closet"
colnames(full.data)[31] = "smoking"
colnames(full.data)[32] = "nightlife"
colnames(full.data)[33] = "workfriendly"
colnames(full.data)[34] = "music"
colnames(full.data)[35] = "pooltable"
colnames(full.data)[36] = "age"
colnames(full.data)[37] = "appointment"


#Now we proceed to changing class of variables:

#Change rating to a numeric value
full.data = full.data %>% 
  mutate(rating = (str_replace_all(rating, " stjernet vurdering", "")))
full.data$rating = as.numeric(full.data$rating)
class(full.data$rating)

#Change number of reviews to a numeric value
full.data = full.data %>% 
  mutate(reviews = (str_replace_all(reviews, " anmeldelser", "")))
full.data$reviews = as.numeric(full.data$reviews)
class(full.data$count.reviews)

#Change price to a factor. 
full.data$price = as.factor(full.data$price)
class(full.data$price)

#Change dresscode to a factor. 
full.data$dresscode = as.factor(full.data$dresscode)
class(full.data$dresscode)

#Change mealtype to a factor. 
full.data$mealtype = as.factor(full.data$mealtype)
class(full.data$mealtype)

#Change alcohol yes/no to a factor
full.data$alcohol = as.factor(full.data$alcohol)
class(full.data$alcohol)

#Change music to a factor. 
full.data$music = as.factor(full.data$music)
class(full.data$music)

#Change parking to a factor. 
full.data$parking = as.factor(full.data$parking)
class(full.data$parking)

#Change smoking to a factor. 
full.data$smoking = as.factor(full.data$smoking)
class(full.data$smoking)

#Change mood to a factor. 
full.data$atmosphere = as.factor(full.data$atmosphere)
class(full.data$atmosphere)

#Change mood to a factor. 
full.data$noise = as.factor(full.data$noise)
class(full.data$noise)

#Change wifi to a factor. 
full.data$wifi = as.factor(full.data$wifi)
class(full.data$wifi)


#Turning yes/no variables into dummy-variables, 1 = yes and 0 = no

#Accept creditcard into 1 and 0
full.data = full.data %>% 
  mutate(
    yesindicator = full.data$takescard %>% 
      str_detect("Ja"),
    takescard = if_else(yesindicator, 1, 0))

#kidfriendly into 1 and 0
full.data = full.data %>% 
  mutate(
    yesindicator = full.data$kidfriendly %>% 
      str_detect("Ja"),
    kidfriendly = if_else(yesindicator, 1, 0))

#catering into 1 and 0
full.data = full.data %>% 
  mutate(
    yesindicator = full.data$catering %>% 
      str_detect("Ja"),
    catering = if_else(yesindicator, 1, 0))
sum(full.data$catering, na.rm = TRUE)

#drivethru into 1 and 0
full.data = full.data %>% 
  mutate(
    yesindicator = full.data$drivethru %>% 
      str_detect("Ja"),
    drivethru = if_else(yesindicator, 1, 0))

#coatcheck into 1 and 0
full.data = full.data %>% 
  mutate(
    yesindicator = full.data$closet %>% 
      str_detect("Ja"),
    closet = if_else(yesindicator, 1, 0))

#dancefloor into 1 and 0
full.data = full.data %>% 
  mutate(
    yesindicator = full.data$dancing %>% 
      str_detect("Ja"),
    dancing = if_else(yesindicator, 1, 0))

#happyhour into 1 and 0
full.data = full.data %>% 
  mutate(
    yesindicator = full.data$happyhour %>% 
      str_detect("Ja"),
    happyhour = if_else(yesindicator, 1, 0))

#pooltable into 1 and 0
full.data = full.data %>% 
  mutate(
    yesindicator = full.data$pooltable %>% 
      str_detect("Ja"),
    pooltable = if_else(yesindicator, 1, 0))

#delivery into 1 and 0
full.data = full.data %>% 
  mutate(
    yesindicator = full.data$delivery %>% 
      str_detect("Ja"),
    delivery = if_else(yesindicator, 1, 0))

#serving into 1 and 0
full.data = full.data %>% 
  mutate(
    yesindicator = full.data$serving %>% 
      str_detect("Ja"),
    serving = if_else(yesindicator, 1, 0))

#reservation into 1 and 0
full.data = full.data %>% 
  mutate(
    yesindicator = full.data$reservations %>% 
      str_detect("Ja"),
    reservations = if_else(yesindicator, 1, 0))

#takeaway into 1 and 0
full.data = full.data %>% 
  mutate(
    yesindicator = full.data$takeaway%>% 
      str_detect("Ja"),
    takeaway = if_else(yesindicator, 1, 0))

#handicapfriendly into 1 and 0
full.data = full.data %>% 
  mutate(
    yesindicator = full.data$handicapfriendly%>% 
      str_detect("Ja"),
    handicapfriendly = if_else(yesindicator, 1, 0))

#tv into 1 and 0
full.data = full.data %>% 
  mutate(
    yesindicator = full.data$tv%>% 
      str_detect("Ja"),
    tv = if_else(yesindicator, 1, 0))

#outdoorseating into 1 and 0
full.data = full.data %>% 
  mutate(
    yesindicator = full.data$sitoutside%>% 
      str_detect("Ja"),
    sitoutside = if_else(yesindicator, 1, 0))

#workfriendly into 1 and 0
full.data = full.data %>% 
  mutate(
    yesindicator = full.data$workfriendly%>% 
      str_detect("Ja"),
    workfriendly = if_else(yesindicator, 1, 0))

#groupfriendly into 1 and 0
full.data = full.data %>% 
  mutate(
    yesindicator = full.data$groupfriendly%>% 
      str_detect("Ja"),
    groupfriendly = if_else(yesindicator, 1, 0))

#happy hour into 1 and 0
full.data = full.data %>% 
  mutate(
    yesindicator = full.data$happyhour%>% 
      str_detect("Ja"),
    happyhour = if_else(yesindicator, 1, 0))

#Removing irrelevant variables and variables with too many missing values
full.data$pokestop = NULL
full.data$dogsallowed = NULL
full.data$nightlife = NULL
full.data$yesindicator = NULL
full.data$reservation = NULL
full.data$appointment = NULL
full.data$age = NULL
full.data$nightlife = NULL
full.data$unidentified = NULL
full.data$drivethru = NULL

#Renaming dataset.
final.data = full.data

## To reorder the levels:
final.data$price = factor(final.data$price,levels(final.data$price)[c(4,1,2,3)])
#ordering from morning to nighttime 
final.data$mealtype = factor(final.data$mealtype,levels(final.data$mealtype)[c(7,8, 9, 1, 3, 4, 5, 6, 2, 10)])
#ordering parking levels
final.data$parking = factor(final.data$parking, levels(final.data$parking)[c(6, 3, 4, 5, 2, 1)])
#ordering dresscode levels
final.data$dresscode = factor(final.data$dresscode, levels(final.data$dresscode)[c(3, 2, 1)])
#ordering atmosphere
final.data$atmosphere = factor(final.data$atmosphere, levels(final.data$atmosphere)[c(11, 12, 1, 2, 9, 10, 3, 4, 5, 6, 7, 8)])
#ordering noise levels
final.data$noise = factor(final.data$noise, levels(final.data$noise)[c(4, 2, 1, 3)])
#ordering alcohol
final.data$alcohol = factor(final.data$alcohol, levels(final.data$alcohol)[c(2, 1, 3)])
#ordering wifi
final.data$wifi = factor(final.data$wifi, levels(final.data$wifi)[c(2, 1, 3)])
#ordering smoking
final.data$smoking = factor(final.data$smoking, levels(final.data$smoking)[c(2, 3, 1)])
#ordering music levels
final.data$music = factor(final.data$music, levels(final.data$music)[c(1, 4, 5, 6, 2, 3)])



#Now we need to change all danish specific letter into their equivalent in english (Addresses).
#Otherwise gmapsdistance (which we use to compute our "distance/location variable") can't read the addresses:

full.data = full.data %>% 
  mutate(address = (str_replace_all(address, "?", "aa")))

full.data = full.data %>% 
  mutate(address = (str_replace_all(address, "?", "Aa")))

full.data = full.data %>% 
  mutate(address = (str_replace_all(address, "?", "ae")))

full.data = full.data %>% 
  mutate(address = (str_replace_all(address, "?", "Ae")))

full.data = full.data %>% 
  mutate(address = (str_replace_all(address, "?", "oe")))

full.data = full.data %>% 
  mutate(address = (str_replace_all(address, "?", "Oe")))

#Naming back the data set 
final.data = full.data

#---------------------------------------------------------
##Computing the location of the restaurants and their distance from the city center##
#---------------------------------------------------------

#First we compute the distance to the city center of Copenhagen and add this variable
#to our data set
place.names = final.data$address
place.names

distance_list = mapdist(from=place.names, to="Noerreport")
distance_list
colnames(distance_list)[1] = "address"

final.data = left_join(final.data, unique(distance_list), by=c("address"="address"))

# We now compute the longitude and lattitude of all the restaurants in our data set
#based on their addresses and add these variables to the data set
address.vector = final.data$address

df = data.frame(
  address = address.vector,
  stringsAsFactors = FALSE)

geolocate.data.frame = df %>% 
  mutate_geocode(address)

colnames(geolocate.data.frame)[1] = "address"
colnames(geolocate.data.frame)[2] = "longitude"
colnames(geolocate.data.frame)[3] = "lattitude"

#Because these variables are perfectly ordered by address we can just add them to the data set
final.data$longitude = c(geolocate.data.frame$longitude)
final.data$lattitude = c(geolocate.data.frame$lattitude)


#---------------------------------------------------------
##REMOVING MISSING VALUES##
#---------------------------------------------------------

final.data[22:30] = NULL #remove kolonne 22-30
final.data[14:15] = NULL #remove dresscode og atmosphere
final.data[6:8] = NULL #remove reservations, delivery and takeaway
final.data[7:8] = NULL #remove mealtype og parkering
final.data[14] = NULL #remove serving
final.data = final.data[complete.cases(data[,4:13]),]
#Leaving us with 327 observations


#------------------------------------------------------
##VISUALIZING DATA##
#------------------------------------------------------

#Figure 1: shows the average rating for restaurants at different price levels
mm = ddply(final.data, "price", summarise, AverageRating = mean(rating))
ggplot(mm, aes(x = factor(price), y = AverageRating)) + 
  geom_bar(stat = "identity", width=.6, position = "dodge") + 
  coord_flip() +
  labs(title = "Average rating for restaurants in different price levels") +
  labs(y = "Average rating") +
  labs(x = "Price level") +scale_y_continuous(expand = c(0, 0), limits = c(0,5)) 

#Figure 2:
map <- get_map(
  location = c("55.6836695, 12.571585"), 
  source = "google", zoom = 14, maptype = "roadmap"
)
ggmap(map)
#Map of Copenhagen with restaurants colored by rating across the city
map.rating.colour = ggmap(map) + geom_point(aes(x = longitude, y = lattitude, colour=final.data$rating, size=3),
                                            data = final.data) + theme_nothing(legend = TRUE) +
  labs(title = "Restaurants in Copenhagen coloured by rating", fill = "") +
  guides(size=FALSE) +
  labs(colour="Rating")
map.rating.colour


