#load packages
library(readxl)
library(dplyr)
library(tidyverse)

#import data sets
X2024_06_17_cohort <- read_excel("/Users/lilyceraso/Library/Containers/com.microsoft.Excel/Data/Downloads/2024-06-17 cohort.xlsx")
analysis_master_2024_06_27 <- read_excel("/Users/lilyceraso/Library/Containers/com.microsoft.Excel/Data/Downloads/analysis master 2024-06-27.xlsx", 
                                         sheet = "CBCs")

#edit cohort list 
x2024.1 <- X2024_06_17_cohort[,c("Animal ID", "Infection Date", "Treatment", "ART type")]
x2024.2 <- x2024.1[x2024.1$Treatment != "non-infected",] 

#trim down cbc data set
cbc_data <- analysis_master_2024_06_27[,c("Animal", "Sample Date", "#Neut", "#Lymph", "#Mono", "#Eosin", "#Baso")]

#Rename x2024
names(x2024.2)[names(x2024.2)== "Animal ID"] <- "Animal"

#left join data sets together based on Animal
cbc_2024_data <- left_join(cbc_data, x2024.2, by = "Animal")
cbc_2024_data <- cbc_2024_data |>
  rename(Sample_Date = `Sample Date`)
cbc_2024_data <- cbc_2024_data |>
  rename(Infection_Date = `Infection Date`)

#add DPI
cbc_2024_dpi <- cbc_2024_data |>
  mutate(DPI = Sample_Date - Infection_Date)
cbc_2024_dpi$DPI <- as.numeric(cbc_2024_dpi$DPI)
cbc_2024_dpi$DPI <- cbc_2024_dpi$DPI / 24
cbc_2024_dpi$DPI <- round(cbc_2024_dpi$DPI)

#assigning ART type 
cbc_2024_dpi$`ART type` <- ifelse(cbc_2024_dpi$Treatment == "ART" & is.na(cbc_2024_dpi$`ART type`), "old", 
                                  ifelse(cbc_2024_dpi$Treatment == "N" & is.na(cbc_2024_dpi$`ART type`), "none", "new"))

#count monkeys
num_monkeys <- unique(cbc_2024_dpi$Animal)
num_unique <- length(num_monkeys)

#create separate data frames for each cell type
baso_data <- cbc_2024_dpi[,c("Animal", "#Baso", "ART type", "DPI")]
   baso_data$`#Baso` <- as.numeric(baso_data$`#Baso`)
mono_data <-cbc_2024_dpi[,c("Animal", "#Mono", "ART type", "DPI")]
   mono_data$`#Mono` <- as.numeric(mono_data$`#Mono`)
lymph_data <- cbc_2024_dpi[,c("Animal", "#Lymph", "ART type", "DPI")]
   lymph_data$`#Lymph` <- as.numeric(lymph_data$`#Lymph`)
neut_data <- cbc_2024_dpi[,c("Animal", "#Neut", "ART type", "DPI")]
   neut_data$`#Neut` <- as.numeric(neut_data$`#Neut`)
eosin_data <- cbc_2024_dpi[,c("Animal", "#Eosin", "ART type", "DPI")]
   eosin_data$`#Eosin` <- as.numeric(eosin_data$`#Eosin`)

#graphing counts per animal 
   #baso
   ggplot(mono_data, aes(x = DPI, y = `#Mono`, color = Animal))+
     geom_point()+
     geom_line()+
     scale_x_continuous(breaks = seq(0, 232, by = 7))+
     xlim(0, 232) 

   

