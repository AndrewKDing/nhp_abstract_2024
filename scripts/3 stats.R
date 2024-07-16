#Purpose: linear modeling for change in monocytes during and after BLZ treatment
#Our model will take the format y = dpi*cohort + (1|Animal ID)

library(tidyverse)
library(lme4)


linear_test <- function(data, yvar){
  
  fm <- as.formula(paste(yvar, " ~ dpi + dpi:cohort + (1|animal_id)"))
  lm <- lmer(fm, data = data, REML = FALSE)
  
  #interpretation
  #dpi is significant -> change is due to DPI
  #dpi:cohort is significant -> BLZ has an effect
  
  pval_dpi <- anova(lm, update(lm, . ~ . - dpi - dpi:cohort))$Pr[[2]]
  pval_interaction <- anova(lm, update(lm, . ~ . - dpi:cohort))$Pr[[2]]
  
  results <- tibble(dpi = pval_dpi, interact = pval_interaction)
  return(results)
}
