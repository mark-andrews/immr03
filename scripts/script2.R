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


# M13b and M13c are the same
M13b <- lme4::lmer(mathscore ~ 1 + ses + (1 + ses | schoolid) + (1 | classid),
                   data = classroom_df)

M13c <- lme4::lmer(mathscore ~ 1 + ses + (1  + ses | schoolid) + (1|schoolid:classid2),
                   data = classroom_df)

# M14a and M14b and M14c are the same
M14a <- lme4::lmer(mathscore ~ 1 + ses + (1 | schoolid) + (1|classid),
                   data = classroom_df)

M14b <- lme4::lmer(mathscore ~ 1 + ses + (1 | schoolid/classid2),
                   data = classroom_df)

M14c <- lme4::lmer(mathscore ~ 1 + ses + (1 | schoolid) + (1|schoolid:classid2),
                   data = classroom_df)



