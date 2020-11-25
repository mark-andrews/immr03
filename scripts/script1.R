# Load packages -----------------------------------------------------------
library(tidyverse)

# Load data ---------------------------------------------------------------

rats_df <- read_csv("https://raw.githubusercontent.com/mark-andrews/immr03/master/data/rats.csv")

# get batch 42
rats_df_42 <- filter(rats_df, batch == 42)


# Binomial logistic regression --------------------------------------------

M42 <- glm(cbind(m, n-m) ~ 1, 
           family = binomial(link = 'logit'),
           data = rats_df_42)
