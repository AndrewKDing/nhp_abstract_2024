#a graphing function makes sense here
# longitudinal_graph - a function, arguments are y value of interest \\ DONE
  #1. for each cohort, summarizes for each dpi
  #2. graphs mean +/- sem in a line graph, grouping by cohort
  #3. makes annotations (currently optional, we just need the data)
# TODO spaghetti_graph - a function, arguments are y value of interest
  #1. splits data into a list of two datasets, splitting by cohort
  #2. for each cohort (lapply), make a spaghetti plot from 173-203 dpi
  #3. returns a list of graphs

library(tidyverse)
library(readxl)
library(stringr)

long_graph <- function(data, yvar, xvar = NULL, group = NULL) { #X variable is dpi as default
  ifelse(is.null(xvar), xvar <- data[["dpi"]], data[[xvar]]) #setting dpi as the default
  ifelse(is.null(group), group <- data[["cohort"]], data[[group]]) #default grouping is by cohort
  yvar = data[[yvar]]
  # steps:
  # select relevant variables from dataframe
  # remove NA values
  # group by cohort and dpi group_by(cohort, dpi)
  # summarize mean and sem
  # ungroup
  # sort by dpi for readability?
  # check that everything is fine?
  
  data_filtered <- data %>%
    mutate(group, xvar, yvar, .keep = "none") %>% #select is not working for me, i think i need to call names() to get the name of the cohort variable
    drop_na() %>% #removing NA values
    group_by(group, xvar) %>% #cohort, then dpi
    summarize(mean = mean(yvar),
              n = n(),
              sem = sd(yvar)/sqrt(n)) %>%
    arrange(xvar, group) %>%
    ungroup()
  
    p <- ggplot(data_filtered, aes(x=xvar, 
                                   y=mean, 
                                   pch=group,
                                   linetype=group)) + #set linetypes and pch later in the script. You also need to reorder factors
      geom_line(aes(linetype=group)) + 
      geom_point(aes(pch=group)) + #draw points over lines
      geom_errorbar(aes(ymax = mean+sem, ymin=mean-sem),#plot SEM
                    width = 10,
                    #position = "dodge"
                    ) + 
      scale_shape_manual(values = c(1,19)) +
      scale_linetype_manual(values = c("dashed","solid")) +
      scale_x_continuous(breaks = c(-21, 0, 21, 105, 173, 203, 238)) +
      #scale_y_continuous(limits = c(0, NA), expand = c(0,0)) +
      #lets fix the labels
      xlab("Day post infection") 
    
    
    #fixing theme settings
    p <- p + theme_bw() +
      theme(text = element_text(family = "Arial",
                                size = 15,
                                color = "black"),
            legend.position = c(1,1),
            legend.justification = c(1,1),
            legend.margin = margin(6, 6, 6, 6, "pt"),
            scale_shape_manual(values = 3),
            scale_linetype_manual(values = 3))
    
    #annotating ART timepoints
    p <- p + annotate("rect", xmin = 21, xmax = 203, ymin = -Inf, ymax = Inf, alpha = .2)
    #return(data_filtered)
    return(p)
}

#again, do your subsetting outside of the function
spaghetti_plot <- function(data, yvar, xvar = NULL, group = NULL) { #X variable is dpi as default
  ifelse(is.null(xvar), xvar <- data[["dpi"]], data[[xvar]]) #setting dpi as the default
  ifelse(is.null(group), group <- data[["cohort"]], data[[group]]) #default grouping is by cohort
  yvar = data[[yvar]]
  
  data_filtered <- data %>%
    mutate(animal_id, group, xvar, yvar, .keep = "none") %>%
    drop_na()
  
  p <- ggplot(data_filtered, aes(x = xvar, y = yvar)) + 
    geom_line(aes(group = animal_id, linetype = group)) +
    geom_point(aes(pch = group)) +
    facet_wrap(~group)
  
  #set aesthetics
  p <- p +
    theme_bw() +
    scale_shape_manual(values = c(1,19)) +
    scale_linetype_manual(values = c("dotted","solid")) +
    theme(
      text = element_text(family = "Arial",
                          size = 15,
                          color = "black"),
      legend.position = "none",
      scale_shape_manual(values = 3),
      scale_linetype_manual(values = 3))
  
  #annotate timepoints
  p <- p + 
    annotate("rect", xmin = -Inf, xmax = 203, ymin = -Inf, ymax = Inf, alpha = .2) #inf because I assume I'm only plotting dpi = 173 up
  return(p)
}


