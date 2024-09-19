source("https://raw.githubusercontent.com/mark-andrews/uwarsaw_gamr01/main/scripts/get_data.R")

M_25 <- gam(copper ~ s(x, y), data = meuse_df)
plot(M_25)
plot(M_25, scheme = 1)

M_26 <- gam(copper ~ s(x) + s(y), data = meuse_df)
anova(M_26, M_25)

M_27 <- gam(copper ~ te(x, y, elev), data = meuse_df)
plot(M_27)

M_28 <- gam(copper ~ s(x,y) + s(elev) + ti(x, y, elev), data = meuse_df)
M_28b <- gam(copper ~ te(x,y) + te(elev) + ti(x, y, elev), data = meuse_df)

anova(M_27, M_28b)
AIC(M_27, M_28b)
AIC(M_27,M_28, M_28b)

summary(M_27)

M_29 <- gam(copper ~ s(x,y) + s(elev), data = meuse_df)

summary(M_28)

M_16 <- gam(accel ~ s(times), data = mcycle)
M_16_alt <- gam(accel ~ times + s(times), data = mcycle)

# Mixed effects -----------------------------------------------------------

library(lme4)

ggplot(sleepstudy,
       aes(x = Days, y = Reaction, colour = Subject)
) + geom_point() + 
  stat_smooth(method = 'lm', se = F) +
  facet_wrap(~Subject)

library(lmerTest)
M_30 <- lmer(Reaction ~ Days + (Days|Subject), data = sleepstudy)
summary(M_30)

confint(M_30)

# random intercepts only
M_31 <- lmer(Reaction ~ Days + (1|Subject), data = sleepstudy)
M_32 <- gam(Reaction ~ Days + s(Subject, bs = 're'), data = sleepstudy)
#M_32 <- gam(Reaction ~ Days + re(Subject), data = sleepstudy)

VarCorr(M_31)
gam.vcomp(M_32)
fixef(M_31)
t(ranef(M_31)$Subject)
coef(M_32)

# random slopes ONLY
M_33 <- lmer(Reaction ~ Days + (0 + Days|Subject), data = sleepstudy)
M_34 <- gam(Reaction ~ Days + s(Days, Subject, bs = 're'), data = sleepstudy)
VarCorr(M_33)
gam.vcomp(M_34)
t(ranef(M_33)$Subject)
coef(M_34)

M_35 <- lmer(Reaction ~ Days + (Days||Subject), data = sleepstudy)
M_35a <- lmer(Reaction ~ Days + (1|Subject) + (0 + Days|Subject), 
              data = sleepstudy)
summary(M_35)

M_36 <- gam(Reaction ~ Days + s(Subject, bs = 're') + s(Days, Subject, bs = 're'),
            REML = T,
            data = sleepstudy)

VarCorr(M_35)
gam.vcomp(M_36)

M_37 <- gam(Reaction ~ s(Days) + s(Subject, bs = 're') + s(Days, Subject, bs = 're'),
            REML = T,
            data = sleepstudy)

summary(M_37)
plot(M_37)


# Bayesian ----------------------------------------------------------------


M_38 <- lm(weight ~ height + age, data = weight_df)
M_39 <- brm(weight ~ height + age, data = weight_df)

bayes_R2(M_39)
