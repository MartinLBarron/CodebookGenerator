#' Uses some basic heuristics to classify type of variable
#' @description This function uses some basic heuristics to guess the basic
#'   variable type. The options are "Empty", "Constant", "Logical", "Date"
#'   "Categorical" "Continous" or "Undeclared". It guesses using these rules,
#'   in order:
#'   
#'   - If all values are NA or all are "" (up to 5 spaces) then it is "Empty"
#'   - If there is only one value, not including NAs, it it "Constant"
#'   - If class is Logical or levels contain only (case insensitive) TRUE/FALSE/ T/F, YES/NO, Y/N, or 0/1 then it is "Logical"
#'   - If class is Date then it is "Date"
#'   - If class is factor or unique values is less 20 then it is "Categorical"
#'   - if variable come contains case insensitive "id" then it is "Identifier"
#'   - If class is integer or numeric it is "Continous"
#'   - If class is Character then it is "Character"
#'   - Otherwise it is "Unclassified"
#' @param varname Variable to which type is to be determined.
#' @param df Dataset in which variable resides
#' 
#' @return A variable type of "ID", "Likely ID", 
#' 
#' @import dplyr
#' 
#' @export 
#' 
#library(anytime)
#sapply(names(x),determineVarType, df=x)
determineVarType <- function(varname,df){
  var <- df[[varname]] 
  
  #"Empty", "Constant", "Logical", "Date","Categorical", "Identifier", "Continous", "Character", "Unclassified"
  
  vartype=""

  #regardless of type, calculate number of unique values minus NAs
  var_noNA <- var[!is.na(var)]
  numlevels <- length(unique(var_noNA))
  varLevels <- levels(factor(var_noNA))

  ## Is this empty variable variable
 if (numlevels==0 | 
     setequal(varLevels, c("")) |
     setequal(varLevels, c(" ")) |
     setequal(varLevels, c("  ")) |
     setequal(varLevels, c("   ")) |
     setequal(varLevels, c("    "))
     ){
  vartype="Empty"
  
  ## Is this an Constant variable
 } else  if (numlevels==1){
   vartype="Constant"
   
  ## Is this Logical variable?
  } else if (class(var)=="logical" |
              setequal(toupper(varLevels), c("TRUE","FALSE")) | 
              setequal(toupper(varLevels), c("T","F")) |
              setequal(toupper(varLevels), c("Y","N")) |
              setequal(toupper(varLevels), c("YES","NO")) |
              setequal(toupper(varLevels), c("0","1"))) {
    vartype="Logical"
    
    ## Is this an Date variable
} else if (class(var)=="Date") { # |
           #|sum(is.na(anydate(var_noNA)))==sum(is.na(var_noNA))) { #if conversion to date doesn't make na's
    vartype="Date"

    ## Is this an categorical variable
} else if (class(var)=="factor" |
           numlevels<20){
  vartype="Categorical"

## Is this an ID variable
} else if (grepl("id$",varname,ignore.case=T)){
  vartype="Identifier"

## Is this an continous variable
} else if (class(var) %in% c("integer", "numeric")){
  vartype="Continous"

## Is this an character variable
} else if (class(var)=="character"){
  vartype="Character"

## Is this an unclassifiable variable
} else {
  vartype="Unclassified"
}

}





 