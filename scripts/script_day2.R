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


library(mgcv)

mcycle <- MASS::mcycle

ggplot(mcycle, aes(x = times, y = accel)) + geom_point()

M_15 <- gam(accel ~ times, data = mcycle)
summary(M_15)
plot(M_15)

M_16 <- gam(accel ~ s(times), data = mcycle)
plot(M_16, residuals = T)

summary(M_16)
anova(M_15, M_16, test = 'F')
anova(M_15, M_16, test = 'Chisq')

# gam(accel ~ s(times) + X + Y + Z, data = mcycle)
# gam(accel ~ s(times) + s(X) + s(Y) + s(Z), data = mcycle)

k.check(M_16)
gam.check(M_16)

M_17 <- gam(accel ~ s(times, k =5), data = mcycle)
anova(M_17, M_16, test = 'Chisq')
plot(M_17)
gam.check(M_17)
anova(M_17, M_16, test = 'Chisq')
AIC(M_17, M_16)


M_18 <- gam(mean_fix ~ s(Time) + Object, data = eyefix_df_avg)
summary(M_18)
plot(M_18)

eyefix_df_avg <- mutate(eyefix_df_avg, Object = factor(Object))
M_19 <- gam(mean_fix ~ s(Time, by = Object), data = eyefix_df_avg)
plot(M_19, pages = 1)
            

add_predictions(eyefix_df_avg, M_19) %>% 
  ggplot(aes(x = Time, y = mean_fix, colour = Object)) +
  geom_point() +
  geom_line(aes(y = pred))

summary(M_19)

M_20 <- gam(mean_fix ~ s(Time, by = Object) + Object, 
            data = eyefix_df_avg)


add_predictions(eyefix_df_avg, M_20) %>% 
  ggplot(aes(x = Time, y = mean_fix, colour = Object)) +
  geom_point() +
  geom_line(aes(y = pred))


M_21 <- gam(mean_fix ~ s(Time,Object, bs = 'fs'), 
            data = eyefix_df_avg)

add_predictions(eyefix_df_avg, M_21) %>% 
  ggplot(aes(x = Time, y = mean_fix, colour = Object)) +
  geom_point() +
  geom_line(aes(y = pred))

anova(M_20, M_21, test = 'Chisq')
AIC(M_20, M_21)


# binary outcome
ggplot(eyetrackr_df,
       aes(x = time, y = 1*Animate,colour = target)
) + geom_point() + geom_smooth()

M_22 <- gam(Animate ~ s(time), 
            data = eyetrackr_df,
            family = binomial())
plot(M_22)

M_23 <- gam(Animate ~ s(time, target, bs = 'fs'), 
            data = eyetrackr_df,
            family = binomial())

add_predictions(eyetrackr_df, M_23, type = 'response') %>% 
  ggplot(aes(x = time, y = 1*Animate, colour = target)) +
  geom_point() +
  geom_line(aes(y = pred))


M_24 <- gam(Animate ~ target + s(time, target, bs = 'fs'), 
            data = eyetrackr_df,
            family = binomial())

add_predictions(eyetrackr_df, M_24, type = 'response') %>% 
  ggplot(aes(x = time, y = 1*Animate, colour = target)) +
  geom_point() +
  geom_line(aes(y = pred))

anova(M_23, M_23, test = 'Chisq')
