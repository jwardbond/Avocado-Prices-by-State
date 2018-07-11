#read in the dataset as characters, no factors
avocado.df <- read.csv('avocado-raw.csv', stringsAsFactors = FALSE)

#insert spaces between region names based on uppercase to make it more readable
avocado.df$region <- sapply(avocado.df$region, addSpace.uppercase)
write.csv(avocado.df, file="avocado.csv")