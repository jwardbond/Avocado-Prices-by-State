library(ggplot2)
library(maps)
library(dplyr)
source("string-parsing.R")

avocado <- read.csv('avocado-raw.csv', stringsAsFactors = FALSE)
avocado.subset <- filter(avocado, region=="BaltimoreWashington" & year=="2015")

avocado$region <- sapply(avocado$region, strsplit.uppercase)

write.csv(avocado, "avocado.csv")