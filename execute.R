# ////// PURPOSE: calls data cleaning, graphing, and analyzing functions from other scripts ///////
# CHECK: There's a seriously strange datapoint at 161dpi that can't possibly be right. 2024-07-13 I'm excluding it for now.
#Logic:
#1. load libraries and source scripts \\ DONE
#2. Load clean data \\ DONE
#3. Reorder cohort factor so ART+BLZ is first, rename cohort to ART+BLZ and ART.\\ DONE
#4. Calculate abs. numbers of monocytes \\ DONE
#5. Graph 
  #We are graphing, grouped by cohort:
  # total numbers of monocytes // DONE
  #   TODO paired changes for total numbers of monocytes by group, 173 to 203
  # monocytes by subset // DONE
  #   classical monocytes // DONE
  #   TODO classical monocytes by group (2 graphs), 173 to 203dpi
  #   intermediate monocytes // DONE
  #   TODO intermediate monocytes by group (2 graphs), 163 to 203dpi
  #   nonclassical monocytes by group // DONE
  #   TODO nonclassical monocytes by group (2 graphs), 173 to 203 dpi
  # TODO brdu monocytes
  #   TODO brdu monocytes by group (2 graphs), 173 to 203 dpi
#6. data analyses
  # TODO dpi 173-203
  # TODO dpi 203 onwards
source("./scripts/1 clean_data.R")
source("./scripts/2 graph.R")
source("./scripts/3 stats.R")

library(tidyverse)
library(ggplot2)
library(lme4)

data <- return_data()

#reorder factors here
data$cohort <- factor(data$cohort, 
                      levels = c("ART on and off (2022)", "ART+CSF1R late (2023)")) 
data$cohort <- factor(data$cohort, labels = c("ART", "ART+BLZ"))
data <- data %>%
  mutate(abs_classical = no_mono * (classical/100),
         abs_intermediate = no_mono * (intermediate/100),
         abs_nonclassical = no_mono * (nonclassical/100),
         abs_brdu_mop = no_mono * (brdu_mop/100)) %>%
  ###################ATTENTION#####################
  #161dpi is being DROPPED, insane outlier on nonclassicals. 2024-07-13#
  # TODO DISCUSS WITH KW #
  filter(dpi != 161)

#Creating a list of things to graph I can't get lapply to work, fuck it i guess
# graph_vars <- list(
#   "no_mono",
#   "abs_classical",
#   "abs_intermediate",
#   "abs_nonclassical",
#   "brdu_mop")

#seriously, clean this up into something real
#Generate graphs
long_graphs <- list(
  long_graph(data, "no_mono"),
  long_graph(data, "abs_classical"),
  long_graph(data, "abs_intermediate"),
  long_graph(data, "abs_nonclassical"),
  long_graph(data, "brdu_mop")
)

#Spaghetti graph, subset data from 173 to 203
data_blz <- data %>%
  filter(dpi >= 173, dpi <=203)

data_post_blz <- data %>%
  filter(dpi >=203)

spaghetti_blz <- list(
  spaghetti_plot(data_blz, "no_mono"),
  spaghetti_plot(data_blz, "abs_classical"),
  spaghetti_plot(data_blz, "abs_intermediate"),
  spaghetti_plot(data_blz, "abs_nonclassical"),
  spaghetti_plot(data_blz, "brdu_mop"))

spaghetti_post_blz <- list(
    spaghetti_plot(data_post_blz, "no_mono"),
    spaghetti_plot(data_post_blz, "abs_classical"),
    spaghetti_plot(data_post_blz, "abs_intermediate"),
    spaghetti_plot(data_post_blz, "abs_nonclassical"),
    spaghetti_plot(data_post_blz, "brdu_mop"))

##Statistical testing
#Total of 10 tests, for each blz and post blz for each

p_vals_blz <- list(
  linear_test(data_blz, "no_mono"),
  linear_test(data_blz, "abs_classical"),
  linear_test(data_blz, "abs_intermediate"),
  linear_test(data_blz, "abs_nonclassical"),
  linear_test(data_blz, "brdu_mop"))

p_vals_post <- list(
  linear_test(data_post_blz, "no_mono"),
  linear_test(data_post_blz, "abs_classical"),
  linear_test(data_post_blz, "abs_intermediate"),
  linear_test(data_post_blz, "abs_nonclassical"),
  linear_test(data_post_blz, "brdu_mop"))


#saving graphs
