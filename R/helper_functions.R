
#' Writes to file, appending data
#' 

writer <- function(x, ..., outfile=fileConn, sep="\n") {
  cat(paste0(x, ...), file=outfile, append=TRUE, sep=sep)
}

#'  reates a rmarkdown wrapper  
#'  

chunk.wrapper <- function(x, ..., outfile=fileConn, options=c("echo=FALSE", "warning=FALSE"), label=NULL) {
  writer(paste0("```{r ", ifelse(is.null(label), ", ", paste0(label, ", ")),
                paste0(options, collapse=", "), "}"),
         outfile = outfile)
  writer(x, ..., outfile = outfile)
  writer("```", outfile = outfile)
  writer("")
}

#' An rmarkdown wrapper for a figure
#' 

fig.wrapper <- function(x, ..., outfile=fileConn, options=c("echo=FALSE", "fig.width=4",
                                                            "fig.height=3", "message=FALSE",
                                                            "warning=FALSE"), label=NULL) {
  chunk.wrapper(x, outfile=outfile, options=options, label=label)
  #I get an error when label stuff is there
}

#' A special wrapper for the library section
#' 

secretChunk.wrapper <- function(x, ..., outfile=fileConn, options=c("echo=FALSE", "include=FALSE",
                                                                    "warning=FALSE", "message=FALSE",
                                                                    "error=FALSE"), label=NULL) {
  chunk.wrapper(x, outfile=outfile, options=options, label=label)
}

#' Wraps two pieces of code into a 2/3rds column row. 
#' Note these rows won't show in built in browser but will show up in regular browser 
#' 

row.wrapper <- function(x,y){
  writer("<div class = \"row\">")
  writer("<div class = \"col-lg-8\">")
  writer(x)
  writer("</div>")
  writer("<div class = \"col-lg-4\">")
  writer(y)
  writer("</div>")
  writer("</div>")
}

#' takes a vector and determines type, with some special considerations
#' 

getDisplayType <- function(var){
  displayType=""
  
  #regardless of type, calculate number of unique values
  numlevels <- length(unique(var))
  
  #if  <15 levels return barlow
  if (numlevels<15){
    displayType <- "barLow"
  }
  #else if factor/character return barHigh
  else if (is.factor(var) | is.character(var)){
    displayType <- "barHigh"
  }
  #else if numeric return histogram
  else if (is.numeric(var)){
    displayType <- "histogram"
    
  } else {
    displayType <- "other"
  }

  return (displayType)
}





vnames <- names(iris)
for (i in 1:length(iris)){
  print(vnames[i])
}

myfunc <- function(v1) {
  deparse(substitute(v1))
}

myfunc(foo)
var <- iris[[1]]

plotVariable <- function(var){
  #we are passed a variable
  displayType <- getDisplayType(var)
  
  if (displayType=="barLow"){
    var <- factor(var)
    ggplot() +
      geom_bar(aes(x=var))
  }else if (displayType=="barHigh"){
    var<-factor(var)
    ggplot() +
      geom_bar(aes(x=var)) 
  }else if (displayType=="histogram"){
    gg <- paste0("ggplot() +
                 geom_histogram(aes(x=var))")
  } else if (displayType=="other"){
    
  }
  
}

aggregateForBarplot <- function(v) {
  outF <- data.frame(table(v))
  names(outF) <- c("x", "y")
  outF
}


#plotVariable(df$hp)


# t1 <- c(T,F,T,F,T,F)
# is.character(iris$Species)
# i <- iris
# df <- mtcars
# 
# df$name <- rownames(df)
# getVariableType(i$Sepal.Width)
# getVariableType(i$Species)
# getVariableType(df$carb)
# getVariableType(df$hp)
# getVariableType(df$name)
