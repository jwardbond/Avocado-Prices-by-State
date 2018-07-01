#Split strings by uppercase. Will separate acronyms
strsplit.uppercase <- function(toSplit, acronyms=FALSE) {
  if (acronyms==TRUE) {
    gsub("(?<=[a-z])(?=[A-Z])", " \\1", toSplit, perl=TRUE)
  }
  else {
    gsub("(?!^)([A-Z])", " \\1", toSplit, perl=TRUE)
  }
}