library(magrittr)

#Add spaces between the uppercase letters in a string
addSpace.uppercase <- function(toSplit, acronyms=FALSE) {
  if (acronyms==TRUE) {
    gsub("(?<=[a-z])(?=[A-Z])", " \\1", toSplit, perl=TRUE)
  }
  else {
    gsub("(?!^)([A-Z])", " \\1", toSplit, perl=TRUE)
  }
}

#Will separate strings by spaces, but ignore common prefixes on cities
strsplit.citiesBySpace <- function(toSplit, ignore_case=TRUE) {
  commonPrefix <- c("San", "Las", "Los", "New", "Ft", "Fort", "St", "Saint", "Grand", "Great", "Greater",
                    "North", "South", "East", "West")

  #Prepare the common prefixes for regex
  expression <- commonPrefix %>% 
    paste0("(?<!\\w)", ., collapse="|") %>%
    paste0("(?<!", ., ")\\s")
  
  #Add ignore case modifier
  if (ignore_case==TRUE) {
    expression <- paste0("(?i)",expression)
  }
  strsplit(toSplit, expression, perl=TRUE)
}

#will grab the first word of a string unless it is preceded by a common city prefix
