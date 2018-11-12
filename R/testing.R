testing <- function(){

## testing
library(forcats)
data(gss_cat)
#createCodebook(iris, outname="iris")

# gss <- readRDS("gss_short.rds")
# df <-gss
createCodebook(gss, outname = "gss")
#createCodebook(dummyData(), outname="dummyData")
createCodebook(iris, outname="iris")
createCodebook(mtcars, outname="mtcars")


library(haven)
reach <- read_sas("/researchdata/CPS/assessments/reach/reach_2017.sas7bdat")
createCodebook(reach)
}