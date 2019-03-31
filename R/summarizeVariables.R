#' Produces sumamry table of all variables in dataframe 
#' 
#' @param df Dataframe containing variables to be summarized
#' 
#' @return Dataframe with summary information
#' 
#' @export
#' 

summarizeVariables <- function(df){
  #construct a dataset of labels, variables, class, unique, and missings
  varOrder <- seq(1:length(df))
  vars <- sapply(names(df), function(x) paste0("<a href=\"#",x,"\">",x,"</a>"))
  uniques <- sapply(df, function(x)length(unique(x)))
  missings <-sapply(df, function(x)paste0(prettyNum(sum(is.na(x))/length(x)*100,digits=2),"%"))
  labs <-sapply(df, function(x)ifelse(is.null(attr(x, "label")[[1]]),"",attr(x, "label")[[1]]))
  labMissing<-sum(sapply(df, function(x)is.na(ifelse(is.null(attr(x, "label")[[1]]),NA,attr(x, "label")[[1]]))))
  classes <-sapply(names(df),determineVarType, df=df)
  #classes <- rep("coming",  length(df))
  
  #if all labels are missing
  if (length(vars)==labMissing){
    sumTab <- data.frame(Variable=varOrder, vars, Class=classes, Unique=uniques, pMissing=missings, row.names = NULL)
    
  }else{
    sumTab <- data.frame(VarOrder=varOrder, labels=labs, Variable=vars, Class=classes, Unique=uniques, pMissing=missings, row.names = NULL)
    
  }
  return(sumTab)
}