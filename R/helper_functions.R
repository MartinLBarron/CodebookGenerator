
#' Writes to file, appending data
#' 

dummyData <- function(n=1000, seed=1284232){
set.seed(seed)

df2 <- data.frame(
  id = seq(1,n,by=1), 
  char = LETTERS[1:8],
  fact = factor(letters[1:4]),
  num = sample(1:600,n, replace = TRUE), 
  log = ifelse(sign(rnorm(n))==-1,T,F),
  dt = seq(from=as.Date("1970/1/1"), by="day", length.out = 1000),
  stringsAsFactors = F
)

return(df2)
}

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

row.wrapper <- function(leftCol,rightCol){
  writer("<div class = \"row\">")
  writer("<div class = \"col-lg-8\", style=\"background-color: lightblue\">")
  writer(leftCol)
  writer("</div>")
  writer("<div class = \"col-lg-4\", style=\"background-color: lightgreen\">")
  writer(rightCol)
  writer("</div>")
  writer("</div>")
}

row.wrapper1 <- function(leftCol,midCol, rightCol){
  writer("<div class = \"row\">")
  writer("<div class = \"col-lg-4\">")
  writer(leftCol)
  writer("</div>")
  writer("<div class = \"col-lg-4\">")
  writer(midCol)
  writer("</div>")
  writer("<div class = \"col-lg-4\">")
  writer(rightCol)
  writer("</div>")
  writer("</div>")
}

# row.wrapper1 <- function(leftCol,midCol, rightCol){
#   writer("<div class = \"row\">")
#   writer("<div class = \"col-lg-4\", style=\"background-color: lightblue\">")
#   writer(leftCol)
#   writer("</div>")
#   writer("<div class = \"col-lg-4\", style=\"background-color: pink\">")
#   writer(midCol)
#   writer("</div>")
#   writer("<div class = \"col-lg-4\", style=\"background-color: lightgreen\">")
#   writer(rightCol)
#   writer("</div>")
#   writer("</div>")
# }


#' takes a vector and determines type, with some special considerations
#' @param var Varible to determine display type

getDisplayType <- function(var){
  displayType=""
  
  #regardless of type, calculate number of unique values
  numlevels <- length(unique(var))
  print(numlevels)
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

var <-iris$Sepal.Length

codebookMetadataSummarize <-function(var){
  
  #Calculate basic information
  variableType=class(var)
  numMissing=sum(is.na(var))
  numValid=sum(!is.na(var))
  numMissing = paste0(numMissing, " (", (numMissing/length(var))*100, "%)")
  numUnique=length(unique(var))
  
  # Construct dataset of basic information
  results <- data.frame(Feature=c(
    "Class",
    "Valid obs.",
    "NA obs.",
    "Unique"),
    Results = c(
      variableType,
      numValid,
      numMissing,
      numUnique
    )
  )
  return(results)
}

codebookVisualize <- function(var, varName){
  varClass <- class(var)
  numUnique <- length(unique(var))
  
  #if there are fewer than 10 levels or it's a character/factor, we'll use a bar
  if (numUnique<=10 | varClass %in% c("factor","character")){
    
    vis <- sprintf("dftest[,'%s']<-factor(dftest[['%s']])\n
                  ggplot(data=dftest)+
                   geom_bar(aes(%s))+
                   theme_minimal()",
                   paste0(varName,"_new"),
                   varName,
                   paste0(varName,"_new"))
    
  } else {
    #all other variables get a histogram
    vis <- sprintf("var <- dftest[['%s']]\nbw=(max(var, na.rm=T)-min(var, na.rm=T))/30\nm <- mean(var, na.rm=T)\nsd <- sd(var, na.rm=T)\nn <- length(!is.na(var))\n
                   
                  class(dftest[['%s']]) <- NULL
                  ggplot(data=dftest)+
                   geom_histogram(aes(%s),color='black',bins=30)+
                   theme_minimal()+
                         stat_function(fun = function(x, mean, sd, n, bw){
                   dnorm(x = x, mean = mean, sd = sd) * bw * n},
                   args = c(mean = m, sd = sd, n = n, bw = bw), color='red')
                   ",varName, varName, varName)
    
    



  }
  
}



codebookDataTableSummarize <-function(var){
  
  varClass <- class(var)
  
  if(varClass %in% c("numeric", "integer")){
    
    varMean=mean(var)
    varSD=sd(var)
    p100 = quantile(var,1)
    p75 = quantile(var,.75)
    p50 = quantile(var,.50)
    p25 = quantile(var,.25)
    p0 = quantile(var,0)
    
    results <- data.frame(Feature=c(
      "Mean",
      "Standard Deviation",
      "100% (Max)",
      "75%",
      "50% (Median)",
      "25%",
      "0% (Min)"), 
      Results = c(
        varMean,
        varSD,
        p100,
        p75,
        p50,
        p25,
        p0
      )
    )
  }
  else{
    x <- as.data.frame(table(var))
    x$percent <- x$Freq/length(var)
    x$validPercent <- x$Freq/sum(!is.na(var))
    results <-x
  }
  
  
  return(results)
}





calcSummaryTable <- function(df){
  #construct a dataset of labels, variables, class, unique, and missings
  vars <- names(df)
  uniques <- sapply(df, function(x)length(unique(x)))
  missings <-sapply(df, function(x)sum(is.na(x))/length(x))
  labs <-sapply(df, function(x)ifelse(is.null(attr(x, "label")[[1]]),"",attr(x, "label")[[1]]))
  labMissing<-sum(sapply(df, function(x)is.na(ifelse(is.null(attr(x, "label")[[1]]),NA,attr(x, "label")[[1]]))))
  
  classes <- rep("coming",  length(df))
  
  #if all labels are missing
  if (length(vars)==labMissing){
    sumTab <- data.frame(Variable=vars, Class=classes, Unique=uniques, pMissing=missings, row.names = NULL)
    
  }else{
  sumTab <- data.frame(labels=labs, Variable=vars, Class=classes, Unique=uniques, pMissing=missings, row.names = NULL)
    
  }
  return(sumTab)
}











