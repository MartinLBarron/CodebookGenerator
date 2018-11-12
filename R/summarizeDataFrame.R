#' Creates a table summarizing key data frame dimensions
#'
#' @param df that will be summarized
#' 
#' @return A dataframe with summary statistics
#' 
#' @import dplyr
#' 
#' @export 
#' 
summarizeDataFrame <- function(df){
  #create statistic labels
  labels <- c("Number of observations", "Number of variables", "Unique Cases", "Complete Cases")

  #calc values
  u <- nrow(unique(df))
  
  values <- c(nrow(df), length(df),u, nrow(na.omit(df)))
  
  #combine labels and calc into dataframe.
  overview <- data_frame(Feature=labels, Value=values)
  
}