library(tidyverse)
library(modelr)

ggplot(gssvocab_df,
       aes(x = age, y = vocab)
) + geom_point()

M_9 <- map(seq(15),
    ~lm(vocab ~ poly(age, degree = ., raw = T), 
        data = gssvocab_df)
)

map_dbl(M_9, AIC)
map_dbl(M_9, AIC) %>% min()
map_dbl(M_9, AIC) %>% which.min()
map_dbl(M_9, AIC) %>% plot()


map_dfr(M_9,
        ~add_predictions(gssvocab_df, model = .),
        .id = 'degree') %>% 
  mutate(degree = as.numeric(degree)) |> 
  ggplot(aes(x = age, y = vocab)) + geom_point() +
  geom_line(aes(y = pred), colour = 'red') +
  facet_wrap(~degree)

M_10 <- lm(vocab ~ poly(age, degree = 5, raw = T), data = gssvocab_df)
M_11 <- lm(vocab ~ poly(age, degree = 5), data = gssvocab_df)

summary(M_10)$r.sq
summary(M_11)$r.sq
AIC(M_10)
AIC(M_11)

ggplot(eyefix_df_avg,
       aes(x = Time, y = mean_fix, colour = Object)
) + geom_point() + geom_line()

M_12 <- lm(mean_fix ~ Object + poly(Time, degree = 3),
           data = eyefix_df_avg)

add_predictions(eyefix_df_avg, M_12) %>% 
  ggplot(aes(x = Time, y = mean_fix, colour = Object)) +
  geom_point() +
  geom_line(aes(y = pred))

M_13 <- lm(mean_fix ~ Object * poly(Time, degree = 3, raw = T),
           data = eyefix_df_avg)

add_predictions(eyefix_df_avg, M_13) %>% 
  ggplot(aes(x = Time, y = mean_fix, colour = Object)) +
  geom_point() +
  geom_line(aes(y = pred))

summary(M_13)

knots <- seq(-1000, 3000, by = 500)

library(splines)
# cubic b spline regression
M_14 <- lm(mean_fix ~ bs(Time, knots = knots),
           data = eyefix_df_avg_targ)

add_predictions(eyefix_df_avg_targ, M_14) %>% 
  ggplot(aes(x = Time, y = mean_fix)) +
  geom_point() +
  geom_line(aes(y = pred))
