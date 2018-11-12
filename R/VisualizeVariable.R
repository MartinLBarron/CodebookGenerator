# build a string that is the code that will be run to produce visual
VisualizeVariable_string <- function(var, varName){
  varClass <- class(var)
  numUnique <- length(unique(var))
  
  #if there are fewer than 10 levels or it's a character/factor, we'll use a bar
  if (numUnique<=10 | varClass %in% c("factor","character")){
    
    vis <- sprintf("codebookDF['%s']<-factor(codebookDF[['%s']])\n
                  ggplot(data=codebookDF)+
                   geom_bar(aes(%s))+
                   theme_minimal()",
                   paste0(varName,"_new"),
                   varName,
                   paste0(varName,"_new"))
    
  } else {
    #all other variables get a histogram
    vis <- sprintf("var <- codebookDF[['%s']]\nbw=(max(var, na.rm=T)-min(var, na.rm=T))/30\nm <- mean(var, na.rm=T)\nsd <- sd(var, na.rm=T)\nn <- length(!is.na(var))\n
                   
                  class(codebookDF[['%s']]) <- NULL
                  ggplot(data=codebookDF)+
                   geom_histogram(aes(%s),color='black',bins=30)+
                   theme_minimal()+
                         stat_function(fun = function(x, mean, sd, n, bw){
                   dnorm(x = x, mean = mean, sd = sd) * bw * n},
                   args = c(mean = m, sd = sd, n = n, bw = bw), color='red')
                   ",varName, varName, varName)
  }
  
}

df <- iris
varName <- "Sepal.Width"
VisualizeVariable <- function(df, varName){
  varClass <- class(df[,varName])
  numUnique <- length(unique(df[,varName]))
  
  #if there are fewer than 10 levels or it's a character/factor, we'll use a bar
  if (numUnique<=10 | varClass %in% c("factor","character")){
    
    codebookDF[,paste0(varname,"_new")]<-factor(codebookDF['%s'])
    ggplot(data=codebookDF)+
      geom_bar(aes(paste0(varname,"_new")))+
      theme_minimal()
  } else {
    #all other variables get a histogram
    var <- codebookDF[['%s']]
    bw=(max(var, na.rm=T)-min(var, na.rm=T))/30
    m <- mean(var, na.rm=T)
    sd <- sd(var, na.rm=T)
    n <- length(!is.na(var))
    class(codebookDF[[varName]]) <- NULL
    ggplot(data=codebookDF)+
      geom_histogram(aes(varName),color='black',bins=30)+
      theme_minimal()+
      stat_function(fun = function(x, mean, sd, n, bw){
        dnorm(x = x, mean = mean, sd = sd) * bw * n},
        args = c(mean = m, sd = sd, n = n, bw = bw), color='red')
  }
  
}