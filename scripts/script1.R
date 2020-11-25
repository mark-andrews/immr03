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

b <- -1.9369
tau <- 0.6646

# probability distribution
tibble(beta = seq(-4.5, 1, length.out = 1000),
       p = dnorm(beta, mean = b, sd = tau)) %>%
  ggplot(aes(x = beta, y = p)) + geom_line()

# probability distibution over theta 
tibble(beta = seq(-4.5, 1, length.out = 1000),
       p = dnorm(beta, mean = b, sd = tau)) %>%
  ggplot(aes(x = plogis(beta), y = p)) + geom_line()


# View random effects -----------------------------------------------------

ranef(Mml)$batch
head(ranef(Mml)$batch + b)
head(coef(Mml)$batch)
plogis(coef(Mml)$batch[['(Intercept)']])



# Normal random effects ---------------------------------------------------

alcohol_df <- read_csv("https://raw.githubusercontent.com/mark-andrews/immr03/master/data/alcohol.csv")
