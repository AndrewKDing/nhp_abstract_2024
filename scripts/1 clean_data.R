#style notes - snake_case for variables, per tidyverse style guide
#This script selects the animals I want, and joins CBC data and flow data appropriately
#A critical step in this script is the calculation of DPI, conversion to weeks, and back to DPI to resolve any "one-day-off" issues

# // TODO // DPI 231 for ART off is missing from PA's data - follow up?
# // TODO DPI conversion prior to merging all data files
        # Convert PA data files \\ DONE
        # Calculate and convert CBC data files \\ TODO
        # Calculate and convert my flow data files \\ TODO
# // TODO variable names need to be the same between my data and PA data

library(dplyr)
library(tidyr)
library(ggplot2)
library(readxl)
library(stringr)

#defining a function to cleanup variable names
cleanup <- function(data){
    df <- data %>%
    rename_with(~ str_replace_all(., pattern = c("%" = "", "\\+" = "p", "\\-" = "m"))) %>%
    rename_with(~ str_replace_all(., "\\s+", "_")) %>% 
    rename_with(~ str_trim(.)) %>%#convert to snake case
    rename_with(tolower) %>% #might as well get rid of uppercase
    mutate_if(is.character, as.factor) 
}

#Loading cohort information
cohort <- read_xlsx("./data/cleaned/2024-06-17 cohort.xlsx") %>%
  filter(Cohort == "ART on and off (2022)" | Cohort == "ART+CSF1R late (2023)") %>%
  select(c("Animal ID", 
            "Cohort", 
            "Infection Date", 
            "Necropsy Date", 
            "DPI Necropsy", 
            "Treatment", 
            "Treatment Start",
            "Treatment End",
            "ageYr")) %>%
  mutate_if(is.character, as.factor) %>%
  cleanup()

#Note to self: CSF1R treatment begins at 173 until 203 DPI, or necropsy for animals necropsied at 204

#Loading flow and CBC data files
#Data files to load:
# - Monocyte flow data for CSF1R (in master analysis sheet) \\ DONE
# - BrdU flow data for CSF1R (in master analysis sheet)     \\ DONE
# - Some of PA's brdu data                                  \\ DONE
# - For ART controls, PA's analysis for monocytes           \\ DONE
# - For ART controls, PA's analysis for BrdU                \\ DONE
# - CBC files for LA79                                      \\ DONE
# - CBC files for LA26                                      \\ DONE
# - CBC files from everything else                          \\ DONE
# - CBC files for necropsy of CSF1R interruption animals    \\ DONE
# - CBC files from master analysis sheet                    \\ DONE

csf1r_monos <- read_xlsx("./data/cleaned/analysis master 2024-06-27.xlsx", sheet = "monos")
  #renaming variables for consistency w/PA flow
csf1r_brdu <- read_xlsx("./data/cleaned/analysis master 2024-06-27.xlsx", sheet = "brdu") %>%
  #renaming variables for consistency w/PA flow

pa_list <- paste0("./data/raw/", list.files(path="./data/raw/", pattern="PA"))
pa_flow <- lapply(pa_list, function(file){
  read_xlsx(file) %>%
    pivot_longer(cols = -1) %>%
      rename_with(~ { #this is an anonymous function that cleans up variable names
        new_name <- str_replace_all(.[1], c("%" = "", "\\+" = "p", "\\-" = "m")) #removes weird characters
        new_name <- str_trim(new_name) #ensures that there aren't any leading spaces
        new_name <- str_replace_all(new_name, "\\s+", "_") #transforms to snake case
        .[3] <- new_name #sets the name of the 3rd column to the new name
        . }) %>% #the period returns the vector of column names
      rename(animal_id = names(.)[1], dpi = name) %>%
      rename_with(tolower) %>% #might as well get rid of uppercase
      mutate(dpi = as.numeric(
        ifelse(dpi=="PRE",-21,str_extract(dpi, "[[:digit:]]+")))) %>% #If the timepoint is pre, rename to -21. Otherwise, pull out the dpi
      mutate(dpi = 7 * round(dpi/7, 0)) %>% #normalization of DPI
      mutate_if(is.character, as.factor) 
  })
#collapse PA data

#Makes a list of the data files that start with cbc, then reads and binds_rows for all of them
cbc_list <- paste0("./data/raw/", list.files(path="./data/raw/", pattern="cbc"))
cbc_grouped <- bind_rows(lapply(cbc_list, read_xlsx)) %>%
  semi_join(cohort, by = join_by(Animal == animal_id)) %>% #filter out un-needed animal IDs
  left_join(cohort, by = join_by(Animal == animal_id)) %>% #adding in cohort information
  cleanup() %>%
  rename(animal_id = animal)
# select necessary variables
# calculate DPI
# normalize DPI

  #Set variable names to be the same between pa_flow and in my data, so things can be easily merged

