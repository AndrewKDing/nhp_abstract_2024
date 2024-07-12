#style notes - snake_case for variables, per tidyverse style guide
#This script selects the animals I want, and joins CBC data and flow data appropriately
#A critical step in this script is the calculation of DPI, conversion to weeks, and back to DPI to resolve any "one-day-off" issues

# // TODO // DPI 231 for ART off is missing from PA's data - follow up?
# // DPI conversion prior to merging all data files \\ DONE
        # Convert PA data files \\ DONE
        # Calculate and convert CBC data files \\ DONE
        # alculate and convert my flow data files \\ DONE 2/2
# // Converted my variable names to PA variable names \\ DONE
# // merge all data frames into one frame \\ DONE god I hate coalescing after a join
# // cohort needs to calculate dpi earlier or later \\ DONE
# // PA's brdu values are correctly merged? \\ DONE
# // WARNING MA93 was swapped right? It's an ART off animal but your cohort sheet has it as ART, 
#           it doesn't matter though

library(tidyverse)
library(readxl)
library(stringr)

#defining a function to cleanup variable names
#this is a nice function
return_data <- function(){
  cleanup <- function(data){
    df <- data %>%
      rename_with(~ str_replace_all(., pattern = c("%" = "", "\\+" = "p", "\\-" = "m", "\\#" = "no_"))) %>%
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
  
  csf1r_monos <- read_xlsx("./data/cleaned/analysis master 2024-06-27.xlsx", sheet = "monos") %>%
    #renaming variables for consistency w/PA flow
    rename(c(x14p16m = per_classical, x14p16p = per_intermediate, x14m16p = per_nonclassical)) %>%
    mutate(animal_id = as.factor(animal_id)) %>% #fixing factor data format
    mutate(across(where(is.character), ~ case_when(
      str_detect(., "%") ~ as.numeric(str_replace(., "%", "")) / 100,
      TRUE ~ as.numeric(.)))) %>%
    mutate(x14p16m = x14p16m * 100, 
           x14p16p = x14p16p * 100,
           x14m16p = x14m16p * 100, #get the percentages as a double, it's how PA's data is
           dpi = 7 * round(dpi/7, 0), #normalizing data
           key = str_c(animal_id, dpi, sep="_"), #fixing key value
           .keep = "none") %>%
    select(-c(dpi))
  
  csf1r_brdu <- read_xlsx("./data/cleaned/analysis master 2024-06-27.xlsx", sheet = "brdu") %>%
    rename(brdu_mop = per_brdu) %>%
    mutate(animal_id = as.factor(animal_id),
           dpi = 7 * round(dpi/7, 0),
           key = str_c(animal_id, dpi, sep="_"),
           animal_id,
           brdu_mop) %>%
    select(key, brdu_mop)
  #renaming variables for consistency w/PA flow
  
  pa_list <- paste0("./data/raw/", list.files(path="./data/raw/", pattern="PA"))
  pa_flow <- lapply(pa_list, function(file){
    read_xlsx(file) %>%
      pivot_longer(cols = -1) %>%
      rename_with(~ { #this is an anonymous function that cleans up variable names
        new_name <- str_replace_all(.[1], c("%" = "", "\\+" = "p", "\\-" = "m")) #removes weird characters
        new_name <- str_trim(new_name) #ensures that there aren't any leading spaces
        new_name <- str_replace_all(new_name, "\\s+", "_") #transforms to snake case
        new_name <- str_replace_all(new_name, "^(\\d)", "x\\1") #a band-aid fix for numeric variables
        .[3] <- new_name #sets the name of the 3rd column to the new name
        . }) %>% #the period returns the vector of column names
      rename(animal_id = names(.)[1], dpi = name) %>%
      rename_with(tolower) %>% #might as well get rid of uppercase
      mutate(dpi = as.numeric(
        ifelse(dpi=="PRE",-21,str_extract(dpi, "[[:digit:]]+")))) %>% #If the timepoint is pre, rename to -21. Otherwise, pull out the dpi
      mutate(dpi = 7 * round(dpi/7, 0), #normalizing dpi
             key = str_c(animal_id, as.character(dpi), sep="_")) %>% #adding master key
      mutate_if(is.character, as.factor) %>%
      relocate(key, .before = animal_id) %>%
      select(-c(animal_id, dpi))#make sure key goes first
  })
  
  #Makes a list of the data files that start with cbc, then reads and binds_rows for all of them
  cbc_list <- paste0("./data/raw/", list.files(path="./data/raw/", pattern="cbc")) 
  cbc_grouped <- bind_rows(lapply(cbc_list, read_xlsx)) %>%
    semi_join(cohort, by = join_by(Animal == animal_id)) %>% #filter out un-needed animal IDs
    left_join(cohort, by = join_by(Animal == animal_id)) %>% #adding in cohort information
    cleanup() %>%
    rename(animal_id = animal) %>%
    mutate(animal_id, 
           cohort, 
           dpi = as.numeric((sample_date-infection_date), units="days"),
           no_mono,
           .keep = "none") %>%
    mutate(dpi = 7 * round(dpi/7, 0)) %>%
    filter(dpi >= -21) %>%
    mutate(key = str_c(animal_id, dpi, sep="_")) %>%
    relocate(key, .before = animal_id) %>%
    relocate(cohort, .before = animal_id) %>%
    relocate(dpi, .after = animal_id) 
  
  master_values <- c(list(cbc_grouped),pa_flow,list(csf1r_brdu,csf1r_monos)) %>%
    reduce(left_join, by="key") %>%
    mutate(
      key,
      animal_id,
      dpi,
      no_mono,
      cohort,
      brdu_mop = coalesce(brdu_mop, brdu_mop.x, brdu_mop.y),
      classical = coalesce(x14p16m.x, x14p16m.y),
      intermediate = coalesce(x14p16p.x, x14p16p.y),
      nonclassical = coalesce(x14m16p.x,x14m16p.y),
      .keep = "none"
    ) %>%
    relocate(key, .before = animal_id) %>%
    relocate(cohort, .after = animal_id) %>%
    relocate(dpi, .after = animal_id) %>%
    arrange(desc(dpi))
  
  return(master_values)
}