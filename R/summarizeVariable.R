#' Produces summary table of  variable  
#' 
#' @param var Variable to be summarized
#' @param varClass Variable Class to be summarized
#' 
#' @return Dataframe with summary information
#' 
#' @import forcats
#' 
#' @export
 
# library(forcats)
# var <- dd$dt1
summarizeVariable <- function(var, varClass){
  #"Empty", "Constant", "Logical", "Date","Categorical", "Identifier", "Continous", "Character", "Unclassified"
  
  #, "Constant", "Logical", ,"Categorical", "Character", "Unclassified"
  
  
    if(varClass %in% c("Continous","Identifier")){
      
      varMean=mean(var)
      p100 = quantile(var,1, na.rm = T)
      p75 = quantile(var,.75, na.rm = T)
      p50 = quantile(var,.50, na.rm = T)
      p25 = quantile(var,.25, na.rm = T)
      p0 = quantile(var,0, na.rm = T)
      
      x <- as.Date("2018-03-21") + 0:10
      probs <- 1:9/10
      
      as.Date(quantile(unclass(x), probs), origin = "1970-01-01")
      
      
      results <- data.frame(Feature=c(
        "Mean",
        "100% (Max)",
        "75%",
        "50% (Median)",
        "25%",
        "0% (Min)"), 
        Results = c(
          varMean,
          p100,
          p75,
          p50,
          p25,
          p0
        )
      )
      } else if(varClass %in% c("Date")){
  
        varMean=mean(var)
        varSD=sd(var)
        p100 = as.Date(quantile(unclass(var),1, na.rm = T), origin="1970-01-01")
        p75 = as.Date(quantile(unclass(var),.75, na.rm = T), origin="1970-01-01")
        p50 = as.Date(quantile(unclass(var),.50, na.rm = T), origin="1970-01-01")
        p25 = as.Date(quantile(unclass(var),.25, na.rm = T), origin="1970-01-01")
        p0 = as.Date(quantile(unclass(var),0, na.rm = T), origin="1970-01-01")
        
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
    } else if(varClass %in% c("Empty")) {
      #recode variables
      var[var==""] <- "\"\""
      var[var==" "] <- "\" \""
      var[var=="  "] <- "\"  \""
      var[var=="   "] <- "\"   \""
      var[var=="    "] <- "\"    \""
      var[var=="     "] <- "\"     \""
      x <- as.data.frame(table(var, useNA='ifany'))
      x$percent <- x$Freq/length(var)
      x$validPercent <- x$Freq/sum(!is.na(var))
      results <-x    
    
    } else{
      #deterimine number of levels
      numLevels <- length(levels(factor(var)))
      if(numLevels>25){
        var <- forcats::fct_lump(factor(var), n=20, other_level="...Other...",)
      }
      x <- as.data.frame(table(var))
      x$percent <- x$Freq/length(var)
      x$validPercent <- x$Freq/sum(!is.na(var))
      results <-x
    }
    
    return(results)
}
  