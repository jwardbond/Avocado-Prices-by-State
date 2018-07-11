library(ggplot2)
library(maps)
library(dplyr)
library(stringr)
library(viridis)

source("string-parsing.R")


#*************************************************************************************
#I want to explore the relationship between avocado prices and location in the US. The 
#first step is to read in the avocado data set and format it the way we want. I am 
#summarizing the data early to hopefully make the next steps faster.
#*************************************************************************************

#read in the dataset as characters, no factors
avocado.df <- read.csv('avocado.csv', stringsAsFactors = FALSE)

#compute average price by region name
avocado.df <- avocado.df %>%
  group_by(region) %>%
  summarise(mean.price = mean(AveragePrice))


#***********************************************************************************
#Particular to this data set, some data points in the region column contain two city 
#names. The workaround is to grab the first word unless the word is a common prefix
#or suffix(e.g. New_*, ft_*, *_City ). Most of paired cities have similar latitudes
#***********************************************************************************

#vectorize cells with two cities and grab the first city from every vectorized cell
avocado.df$region <- sapply(avocado.df$region, strsplit.citiesBySpace)

#check if vector and take first element
firstCity <- function(cellContent) {
  
  if (is.vector(cellContent)) {
    result <- cellContent[1]
  }
  else {
    result <- cellContent
  }
  return(result)
}
avocado.df$region <- sapply(avocado.df$region, firstCity)


#************************************************************************************
#The avocado dataset doesn't have state information, so regions like "Albany" - which 
#could be Albany GA or NY - are ambiguous. I will assume that the "region"
#column in the avocado dataset refers to the largest city with that name, so if there 
#are duplicates in the us.cities data, I will just use the ones with the highest 
#populations.
#************************************************************************************

#get list of US cities
cities.df <- select(us.cities,name, country.etc, pop, long, lat)

#remove state names from end of city names
pattern <- "\\s[A-Z]{1,2}$"
replacement <- ""
cities.df$name <- sapply(cities.df$name, sub, pattern=pattern, replacement=replacement, perl=TRUE)

#remove duplicates with the lowest population (i.e. keep duplicates with highest)
cities.df <- cities.df[order(cities.df$name, -cities.df$pop),] %>%
  distinct(name, .keep_all=TRUE)


#************************************************************************************
#Need to connect regions(cities) in the avocado dataset with their appropriate coord
#One option is the google geocode API, however the ambiguity of some city names will 
#cause problems. An inner join with the prepared df of us cities will remove any 
#regions that aren't cities and will associate any ambiguous city names with the most
#populous city in the US bearing that name.
#************************************************************************************

#get state names
stateNames.df <- data.frame(state.abb, state.name, stringsAsFactors=FALSE)

#clean up colnames
colnames(cities.df) <- c("city", "state", "pop", "long", "lat")
colnames(avocado.df) <- c("city", "avg.price")
colnames(stateNames.df) <- c("state", "state.full")

#join tables
avocado.df <- inner_join(avocado.df, cities.df, by="city")
avocado.df <- right_join(avocado.df, stateNames.df, by="state")

#average state prices
avocado.df <- avocado.df %>%
  group_by(state.full) %>%
  summarise(avg.price = mean(avg.price))

avocado.df$state.full <- tolower(avocado.df$state.full)

#get US map data
states <- map_data("state")

ggplot() +
  geom_map(data=states, map=states, 
          aes(long, lat, map_id=region),
          color="#ffffff", fill="#ffffff") +
  geom_map(data=avocado.df, map=states, 
          aes(fill=avg.price, map_id=state.full)) +
  scale_fill_viridis() +
  coord_map("albers", lat0 = 39, lat1 = 45) +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_blank(),
        plot.margin = margin(0,0,0,0),
        plot.background = element_blank())

