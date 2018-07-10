#**********************************************************************************
#Add spaces between the uppercase letters in a string
#**********************************************************************************
addSpace.uppercase <- function(toSplit, acronyms=FALSE) {
  if (acronyms==TRUE) {
    gsub("(?<=[a-z])(?=[A-Z])", " \\1", toSplit, perl=TRUE)
  }                             
  else {
    gsub("(?!^)([A-Z])", " \\1", toSplit, perl=TRUE)
  }
}


#**********************************************************************************
#Will separate strings by spaces, but ignore common prefixes and Suffixes on cities
#**********************************************************************************
strsplit.citiesBySpace <- function(toSplit, ignore_case=TRUE) {
  commonPrefix <- c("Las", "Los", "New", "Ft", "Fort", "St", "Saint", "San", "Santa", "El", "Salt", 
                    "Long", "Mount", "Palo", "Palm", "Grand", "Great", "Greater",
                    "North", "South", "East", "West")
  
  commonSuffix <- c("Beach", "City", "Falls", "Harbour", "Heights", "Hill", "Hills", "Land", "Valley")

  #Prepare the common pre/suffixes for regex /(?<!...(?<!\w)commonPrefixes)...\s/
  commonPrefix <- paste0("(?<!\\w)", commonPrefix, collapse="|")
  commonSuffix <- paste0(commonSuffix, "(?!\\w)", collapse="|")
   
  expression <- paste0("(?<!", commonPrefix, ")\\s(?!", commonSuffix, ")")
  
  #Add ignore case modifier
  if (ignore_case==TRUE) {
    expression <- paste0("(?i)",expression)
  }
  strsplit(toSplit, expression, perl=TRUE)
}