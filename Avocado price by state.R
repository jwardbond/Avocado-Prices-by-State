library(ggplot2)
library(ggmap)
library(maps)
library(dplyr)
library(stringr)

source("string-parsing.R")

#Read in the dataset as characters, no factors
avocado.df <- read.csv('avocado-raw.csv', stringsAsFactors = FALSE)

#insert spaces between region names based on uppercase to make it more readable
avocado.df$region <- sapply(avocado.df$region, addSpace.uppercase)

write.csv(avocado.df, file="avocado.csv")

#**********************************
#Particular to this data set, some data points in the region column contain two city names
#The workaround is to grab the first word unless the word is a common prefixes (e.g. New, ft, North)
#**********************************

#Regex-only attempt
# #create the regex pattern /^.*?(?=(?<!commonPrefixes)\s)/i
# commonPrefix <- "San|Las|Los|New|Ft|Fort|St|Saint|Grand|North|South|East|West"
# pattern <- paste0("(?i)^.*?(?=(?<!", commonPrefix, ")\\s)")
# 
# #extract the substring
# avocado.df$region <- sapply(avocado.df$region, str_extract, pattern=pattern)

#Vectorize cells with two cities and grab the first city from every vectorized cell
avocado.df$region <- sapply(avocado.df$region, strsplit.citiesBySpace)

firstCity <- function(cellContent) {
  #check if vector and take first element
  if (is.vector(cellContent)) {
    result <- cellContent[1]
  }
  else {
    result <- cellContent
  }
  return(result)
}

avocado.df$region <- sapply(avocado.df$region, firstCity)

#Get the unique city names
uniqueCities <- levels(as.factor(avocado.df$region))
latlon <- geocod

#Test some plots
ggplot(avocado.df.subset, aes(x=Total.Volume, y=AveragePrice)) + 
  geom_point(aes(color=AveragePrice)) 

ggplot(mtcars, aes(x=wt, y=mpg)) + 
  geom_point(color='darkblue')


#export data
write.csv(avocado, "avocado.csv")

