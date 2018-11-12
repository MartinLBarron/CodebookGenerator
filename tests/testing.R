testing <- function(){

  # get datasets
  gss <- readRDS("temp/data/gss_short.rds")
  crime <- readRDS("temp/data/crime2018.rds")
  crime <-crime[1:5000,]
  dd <- dummyData()
  
  #create test datasets
  createCodebook(iris, outputName="iris")
  createCodebook(gss, outputName = "gss")
  createCodebook(dd, outputName="dd")
  createCodebook(crime, outputName="crime")
  createCodebook(mtcars, outputName="mtcars")
  
  #other visualizations
# df <-crime
#   summary(df)
#   library(dplyr)
#   glimpse(df)
#   library(skimr)
#   skim(df)
#   library(visdat)
#   vis_miss(df,warn_large_data = F)
#   vis_dat(df)
#   library(DataExplorer)
# create_report(df)
  
}