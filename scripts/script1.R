# Load packages -----------------------------------------------------------
library(tidyverse)

# Load data ---------------------------------------------------------------

rats_df <- read_csv("https://raw.githubusercontent.com/mark-andrews/immr03/master/data/rats.csv")

# get batch 42
rats_df_42 <- filter(rats_df, batch == 42)


# Binomial logistic regression --------------------------------------------

# or batch 42
M42 <- glm(cbind(m, n-m) ~ 1, 
           family = binomial(link = 'logit'),
           data = rats_df_42)

summary(M42)$coefficients
coef(M42)
# inferred value, or estimate, or theta
plogis(coef(M42))
plogis(confint.default(M42))

# for all batches

rats_df <- mutate(rats_df, batch = as.character(batch))

Mf <- glm(cbind(m, n-m) ~ 0 + batch, 
          family = binomial(link = 'logit'),
          data = rats_df)
# the inferred probability (theta) for each batch
round(plogis(coef(Mf)), 3)

# multilevel model --------------------------------------------------------

library(lme4)

Mml <- glmer(cbind(m, n-m) ~ 1 + (1|batch),
             family = binomial(link = 'logit'),
             data = rats_df)
