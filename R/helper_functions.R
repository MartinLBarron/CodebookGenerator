

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














