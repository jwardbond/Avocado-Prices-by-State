library(ggplot2)
library(ggmap)
library(maps)
library(dplyr)
library(stringr)

source("string-parsing.R")


#*************************************************************************************
#I want to explore the relationship between avocado prices and latitude. The first 
#step is to read in the avocado data set and format it the way we want. I am summarizing
#the data early to hopefully make the next steps faster
#*************************************************************************************

#read in the dataset as characters, no factors
avocado.df <- read.csv('avocado-raw.csv', stringsAsFactors = FALSE)

#insert spaces between region names based on uppercase to make it more readable
avocado.df$region <- sapply(avocado.df$region, addSpace.uppercase)
write.csv(avocado.df, file="avocado.csv")

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
cities.df <- select(us.cities,name, country.etc, pop, lat)

#remove state names from end of city names
pattern <- "\\s[A-Z]{1,2}$"
replacement <- ""
cities.df$name <- sapply(cities.df$name, sub, pattern=pattern, replacement=replacement, perl=TRUE)

#remove duplicates with the lowest population (i.e. keep duplicates with highest)
cities.df <- cities.df[order(cities.df$name, -cities.df$pop),] %>%
  distinct(name, .keep_all=TRUE)


#************************************************************************************
#Need to connect regions(cities) in the avocado dataset with their appropriate latitudes
#One option is the google geocode API, however the ambiguity of some city names will 
#cause problems. An inner join with the prepared df of us cities will remove any 
#regions that aren't cities and will associate any ambiguous city names with the most
#populous city in the US bearing that name.
#************************************************************************************

#clean up colnames
colnames(cities.df) <- c("city", "state", "pop", "lat")
colnames(avocado.df) <- c("city", "avg.price")

#join tables
avocado.df <- inner_join(avocado.df, cities.df, by="city")

ggplot(avocado.df, aes(x=lat, y=avg.price)) +
  geom_point(color="#6a403a", size=3) +
  ggtitle("Avocado Prices in the United States by Latitude") +
  xlab("Latitude (°)") +
  ylab("Average Avocado Price (USD)") +
  theme(
    axis.line = element_line(color="#000000"),
    axis.text = element_text(face="bold"),
    axis.title = element_text(face="bold", size=12),
    panel.background = element_rect(fill="#dde56a"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(face="bold", size=14, hjust=0.5)
  )
