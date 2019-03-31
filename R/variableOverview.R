#' creates variable overview showing type, no. missing, etc. 
#' 
#' @param var Variable to be summarized
#' 
#' @return Dataframe with overview information
#' 
#' @export
#' 
# var <- crime[["ID"]]
variableOverview <-function(var){
  
  #Calculate basic information
  variableType=class(var)
  numMissing=sum(is.na(var))
  numValid=sum(!is.na(var))
  numMissing = paste0(numMissing, " (", prettyNum(numMissing/length(var)*100, digits=2), "%)")
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