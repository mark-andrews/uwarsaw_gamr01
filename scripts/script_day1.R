library(tidyverse)
source("https://raw.githubusercontent.com/mark-andrews/uwarsaw_gamr01/main/scripts/get_data.R")

# weight as a function of height
M_1 <- lm(weight ~ height, data = weight_df)

summary(M_1)
confint(M_1)

# weight as a function of height and age
M_2 <- lm(weight ~ height + age, data = weight_df)
summary(M_2)

anova(M_1, M_2)

summary(M_2)$r.sq
summary(M_2)$adj.r.sq

AIC(M_1, M_2)

# logistic regression
# but first linear ....
M_3 <- lm(logrt ~ logfreq + nletters, data = blp_df)
summary(M_3)

M_4 <- glm(accuracy ~ logfreq + nletters, 
           data = blp_df, 
           family = binomial(link = 'logit'))
summary(M_4)

exp(1.362338) # odds ratio
# factor by which odds increases for unit change

M_5 <- glm(accuracy ~ nletters, 
           data = blp_df, 
           family = binomial(link = 'logit'))

anova(M_5, M_4, test = 'Chisq')
anova(M_5, M_4)

# visualize eye tracking data
ggplot(eyefix_df_avg,
       aes(x = Time, y = mean_fix, colour = Object)
) + geom_point() + geom_line()

ggplot(eyefix_df_avg_targ,
       aes(x = Time, y = mean_fix)
) + geom_point()


# if we wanted to do a line of best fit .. 
M_6 <- lm(mean_fix ~ Time, data = eyefix_df_avg_targ)

# and polynomial regression

# degree 3 polynomial
M_7 <- lm(mean_fix ~ poly(Time, degree = 3, raw = TRUE), 
          data = eyefix_df_avg_targ)

# what is poly doing?
x <- seq(-1, 1, by = 0.1)
poly(x, degree = 5 , raw = TRUE)

M_8 <- map(seq(10),
           ~lm(mean_fix ~ poly(Time, degree = ., raw = TRUE), 
               data = eyefix_df_avg_targ)
)

M_8_aic <- map_dbl(M_8, AIC)
M_8_rsq <- map_dbl(M_8, ~summary(.)$r.sq)

library(modelr)

add_predictions(eyefix_df_avg_targ, M_8[[2]]) %>% 
  ggplot(aes(x = Time, y = mean_fix)) +
  geom_point() +
  geom_line(aes(y = pred), colour = 'red')

add_predictions(eyefix_df_avg_targ, M_8[[9]]) %>% 
  ggplot(aes(x = Time, y = mean_fix)) +
  geom_point() +
  geom_line(aes(y = pred), colour = 'red')

# a facet wrap plot
map_dfr(M_8,
        ~add_predictions(eyefix_df_avg_targ, model = .),
        .id = 'degree') %>% 
  mutate(degree = as.numeric(degree)) |> 
  ggplot(aes(x = Time, y = mean_fix)) + geom_point() +
  geom_line(aes(y = pred), colour = 'red') +
  facet_wrap(~degree)

# using different colours per line plot
map_dfr(M_8,
        ~add_predictions(eyefix_df_avg_targ, model = .),
        .id = 'degree') %>% 
  mutate(degree = as.numeric(degree)) |> 
  ggplot(aes(x = Time, y = mean_fix)) + geom_point() +
  geom_line(aes(y = pred, colour = factor(degree)))

