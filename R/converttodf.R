df <- data.frame(a=c(1,2,4,2), b=c("a","b","c","d"),stringsAsFactors = F)
getparts <- function(x,df) {
  name <- names(df)[x]
  data <- df[,x]
  if (class(df[,x]) %in% c("character","factor")) {
    string <- paste(shQuote(data), collapse=", ")
    string <-paste0(name,"=c(", string, ")")
  }
  else {
    string <- paste(name,"=c(",paste(data,collapse = ","), ")")
  }
  
}

reversedf <- function(df) {
  
  dfname <- deparse(substitute(df))
  x <- lapply(1:length(df), getparts, df)
  paste(dfname, " <- data.frame(", paste(x, collapse = ", "), ")")
  
}
test <- reversedf(df)
test                                                                                     
