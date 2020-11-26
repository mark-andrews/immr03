library(tidyverse)

classroom_df <- read_csv("https://raw.githubusercontent.com/mark-andrews/immr03/master/data/classroom.csv")

# Visualize data  ---------------------------------------------------------


ggplot(classroom_df,
       aes(x = ses, y = mathscore)
) + geom_point() + facet_wrap(~schoolid) +
  stat_smooth(method = 'lm', se = F)


M13 <- lme4::lmer(mathscore ~ 1 + ses + (1 + ses | schoolid) + (1 + ses | classid),
                  data = classroom_df)

M13a <- lme4::lmer(mathscore ~ 1 + ses + (1 + ses | schoolid) + (1 + ses || classid),
                   data = classroom_df)

# 
M13b <- lme4::lmer(mathscore ~ 1 + ses + (1 + ses | schoolid) + (1 | classid),
                   data = classroom_df)

M13c <- lme4::lmer(mathscore ~ 1 + ses + (1 + ses | schoolid) + (1 | schoolid:classid2),
                   data = classroom_df)

# M14a and M14b are the same
M14a <- lme4::lmer(mathscore ~ 1 + ses + (1 | schoolid) + (1|classid),
                   data = classroom_df)

M14b <- lme4::lmer(mathscore ~ 1 + ses + (1 | schoolid/classid2),
                   data = classroom_df)

M14c <- lme4::lmer(mathscore ~ 1 + ses + (1|schoolid) + (1 | schoolid:classid2),
                   data = classroom_df)


# lm(y ~ x * z)
# lm(y ~ x + z + x:z)

# crossed structures ------------------------------------------------------

blp_df <- read_csv("https://raw.githubusercontent.com/mark-andrews/immr03/master/data/blp-short2.csv")

M15 <- lmer(rt ~ 1 + (1|spelling)  + (1|participant), data = blp_df)


# group level predictors ---------------------------------------------------

mathachschool_df <- read_csv("https://raw.githubusercontent.com/mark-andrews/immr03/master/data/mathachieveschool.csv")
mathach_df <- read_csv("https://raw.githubusercontent.com/mark-andrews/immr03/master/data/mathachieve.csv")
mathach2_df <- inner_join(mathachschool_df, mathach_df, by = 'school')

M16 <- lmer(mathach ~ ses + himinty + ses:himinty + (1 + ses | school),
            data = mathach2_df)


# generalized linear models -----------------------------------------------

sleepstudy2 <- mutate(sleepstudy, fast_rt = Reaction < median(Reaction))

M17 <- glmer(fast_rt ~ 1 + Days + (1 + Days | Subject),
             data = sleepstudy2,
             family = binomial(link = 'logit'))



# brms bayesian sleepstudy ------------------------------------------------

M8 <- lmer(Reaction ~ Days + (Days | Subject),
           data = sleepstudy)

library(brms)

M18 <- brm(Reaction ~ Days + (Days | Subject),
           data = sleepstudy)
