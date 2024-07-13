#We are graphing, grouped by cohort:
# TODO total numbers of monocytes
#   TODO paired changes for total numbers of monocytes by group, 173 to 203
# TODO monocytes by subset
#   TODO classical monocytes
#   TODO classical monocytes by group (2 graphs), 173 to 203dpi
#   TODO intermediate monocytes
#   TODO intermediate monocytes by group (2 graphs), 163 to 203dpi
#   TODO nonclassical monocytes by group
#   TODO nonclassical monocytes by group (2 graphs), 173 to 203 dpi
# TODO brdu monocytes
#   TODO brdu monocytes by group (2 graphs), 173 to 203 dpi

#a graphing function makes sense here
#longitudinal_graph - a function, arguments are y value of interest
  #1. for each cohort, summarizes for each dpi
  #2. graphs mean +/- sem in a line graph, grouping by cohort
  #3. makes annotations (currently optional, we just need the data)
#spaghetti_graph - a function, arguments are y value of interest
  #1. subsets data from 173-203 dpi \\ subsetting should be performed outside of the function
  #2. splits data into a list of two datasets, splitting by cohort
  #3. for each cohort (lapply), make a spaghetti plot from 173-203 dpi
  #4. returns a list of graphs
library(tidyverse)
library(readxl)
library(stringr)

source("scripts/1 clean_data.R")
df <- return_data() ### again, delete this later. I better not catch you running all your analyses in this script.
p <- ggplot(df, aes(x=dpi, y=(as.numeric(no_mono)*classical/100), color=cohort)) + geom_point() ### delete this code later, it's just for testing

#do your subsetting outside of the function, for cleanliness and readability
long_graph <- function(data, yvar, xvar = NULL) { #X variable is dpi as default
  if (is.null(xvar)) { xvar <- "dpi" } #setting dpi as the default
  
  
  ggplot(data, aes(x=.data[[xvar]], y=.data[[yvar]], color=cohort)) + geom_point()
}
long_graph(df,yvar = "no_mono")
