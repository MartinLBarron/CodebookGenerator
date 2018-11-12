

# Create a dummy dataset --------------------------------------------------
dummyData <- function(n=1000, seed=1284232){
  set.seed(seed)

  df <- data.frame(
    id = seq(1,n,by=1), 
    char = replicate(n, paste(sample(LETTERS, 2, replace=TRUE), collapse="")),
    const = 3,
    allNA1 = NA,
    allNA2 = "",
    catg1 = factor(letters[1:4]),
    catg2 = letters[1:10],
    num = sample(1:600,n, replace = TRUE), 
    num2 = runif(n,0,100),
    log1 = ifelse(sign(rnorm(n))==-1,T,F),
    log2 = ifelse(sign(rnorm(n))==-1,"true","false"),
    log3 = ifelse(sign(rnorm(n))==-1,"t","f"),
    log4 = ifelse(sign(rnorm(n))==-1,"yes","no"),
    log5 = ifelse(sign(rnorm(n))==-1,1,0),
    dt1 = seq(from=as.Date("1970/1/1"), by="day", length.out = n),
    dt2 = rep("1970/1/1", n),
    dt3 = rep("September 1, 2000", ),
    dt4 = rep("12/18/1973", n),
    
    stringsAsFactors = F
  )
  return(df)
}

# writes output to file ---------------------------------------------------
writer <- function(x, ..., outfile=fileConn, sep="\n") {
  cat(paste0(x, ...), file=outfile, append=TRUE, sep=sep)
}
writer.p <- function(x, ..., outfile=fileConn, sep="\n") {
  cat(paste0("<div class=\"classholder\" >",x,"</div>", ...), file=outfile, append=TRUE, sep=sep)
}

# creates a R markdown wrapper --------------------------------------------

chunk.wrapper <- function(x, ..., outfile=fileConn, options=c("echo=FALSE", "warning=FALSE"), label=NULL) {
  writer(paste0("```{r ", ifelse(is.null(label), ", ", paste0(label, ", ")),
                paste0(options, collapse=", "), "}"),
         outfile = outfile)
  writer(x, ..., outfile = outfile)
  writer("```", outfile = outfile)
  writer("")
}


# An rmarkdown wrapper for a figure

fig.wrapper <- function(x, ..., outfile=fileConn, options=c("echo=FALSE", "fig.width=4",
                                                            "fig.height=3", "message=FALSE",
                                                            "warning=FALSE"), label=NULL) {
  chunk.wrapper(x, outfile=outfile, options=options, label=label)
  #I get an error when label stuff is there
}

# A special wrapper for the library section

secretChunk.wrapper <- function(x, ..., outfile=fileConn, options=c("echo=F", "include=F",
                                                                    "warning=F", "message=F",
                                                                    "error=F"), label=NULL) {
  chunk.wrapper(x, outfile=outfile, options=options, label=label)
}

# Wraps  pieces of code into columns. 
# Note these rows won't show in built in browser but will show up in regular browser 

row.wrapper_4col <- function(aCol, leftCol,midCol, rightCol, varClass){
  writer("<div class = \"row\">")
  writer(paste0("<div class = \"col-lg-1 ",varClass, "\">"))
  writer(aCol)
  writer("</div>")
  writer("<div class = \"col-lg-3\">")
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


printsummarizeVariable <- function(varName,varClass){
  paste0("tab <- summarizeVariable(codebookDF[[\"",varName,"\"]], \"",varClass,"\")\n kable(tab, digits=1) %>%
  kable_styling(bootstrap_options = c(\"striped\", \"hover\",\"condensed\"))")
}

#produce variable level summary
printVariableOverview <-function(var,varName){
  
  string <- paste0("tab <- variableOverview(codebookDF[[\"",varName,"\"]])\n kable(tab, digits=1) %>%
  kable_styling(bootstrap_options = c(\"striped\", \"hover\",\"condensed\")) ")
  
  return(string)
  
}














