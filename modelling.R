#### Load data ####

data_bin <- read.csv("data_bin.csv")
data_pp <- read.csv("data_pp.csv")

data_bin_riskset <- read.csv("data_bin_riskset.csv")
data_pp_riskset <- read.csv("data_pp_riskset.csv")




#### Models ####

# model 1 - policy adoption #

# without limited influences
m1 <- glm(pa_b ~ time_spatial_lag + time_lang_lag + time_trade_lag + 
            time_env_igo_lag + tradeo +
            time_rel_lag + gdppc + gdpg +
            odaprop + nrr + popd + region + fhipr + b_prop, data = data_bin_riskset,
          family = binomial(link = "logit"))

summary(m1)

anova(m1, test = 'Chi')

AIC(m1)



# model 2 - policy expansion #

m2 <- lm(pa_pp ~ time_spatial_lag_pp + time_lang_lag_pp + 
           time_rel_lag_pp + time_trade_lag_pp + 
           time_env_igo_lag_pp + gdppc + tradeo + fdiprop + 
           odaprop + nrr + popd + fhipr + av_pp + US + 
           country - 1, data = data_pp_riskset)

summary(m2)

AIC(m2)



# model 3 - policy expansion - conditionality #

# a # exposure #

m3a <- lm(pa_pp ~ time_spatial_lag_pp + time_lang_lag_pp + 
            time_rel_lag_pp + time_trade_lag_pp + 
            time_env_igo_lag_pp + 
            time_env_igo_lag_pp:env_igo_nb + time_trade_lag_pp:tradeo + 
            env_igo_nb + gdppc + tradeo + fdiprop + 
            odaprop + nrr + popd + fhipr + US + av_pp +
            country - 1, data = data_pp_riskset_no)

summary(m3a)

AIC(m3a)




# b # conditional responsiveness #

# natural resource rents

m3b <- lm(pa_pp ~ time_spatial_lag_pp + time_lang_lag_pp + 
            time_rel_lag_pp + time_trade_lag_pp + 
            time_env_igo_lag_pp + 
            time_lang_lag_pp:nrr + 
            time_rel_lag_pp:nrr + time_trade_lag_pp:nrr + 
            time_env_igo_lag_pp:nrr + 
            env_igo_nb + gdppc + tradeo + fdiprop + 
            odaprop + nrr + popd + fhipr + US + av_pp +
            country - 1, data = data_pp_riskset_no)

summary(m3b)

AIC(m3b)


# pop density

m3c <- lm(pa_pp ~ time_spatial_lag_pp + time_lang_lag_pp + 
            time_rel_lag_pp + time_trade_lag_pp + 
            time_env_igo_lag_pp + 
            time_lang_lag_pp:popd + 
            time_rel_lag_pp:popd + time_trade_lag_pp:popd + 
            time_env_igo_lag_pp:popd + 
            env_igo_nb + gdppc + tradeo + fdiprop + 
            odaprop + nrr + popd + fhipr + US + av_pp +
            country - 1, data = data_pp_riskset_no)

summary(m3c)

AIC(m3c)




# state capacity

m3d <- lm(pa_pp ~ time_spatial_lag_pp + time_lang_lag_pp + 
            time_rel_lag_pp + time_trade_lag_pp + 
            time_env_igo_lag_pp + 
            time_lang_lag_pp:gdppc + 
            time_rel_lag_pp:gdppc + time_trade_lag_pp:gdppc + 
            time_env_igo_lag_pp:gdppc + 
            env_igo_nb + gdppc + tradeo + fdiprop + 
            odaprop + nrr + popd + fhipr + US + av_pp +
            country - 1, data = data_pp_riskset_no)

summary(m3d)

AIC(m3d)


# optimal model


m3e <- lm(pa_pp ~ time_spatial_lag_pp + time_lang_lag_pp + 
            time_rel_lag_pp + time_trade_lag_pp + 
            time_env_igo_lag_pp +
            time_env_igo_lag_pp:popd + time_lang_lag_pp:nrr + 
            env_igo_nb + gdppc + tradeo + fdiprop + 
            odaprop + nrr + popd + fhipr + US + av_pp +
            country - 1, data = data_pp_riskset_no)

summary(m3e)

AIC(m3e)







#### Assumptions checks checks model 1 ####

# without limiting influences

# predict values
probs_m1 <- predict(m1, type = "response")
pred_m1 <- ifelse(probs_m1 > 0.5, "adoption", "no adoption")
head(pred_m1)


## linearity assumption ##

data_ass_bin <- data_bin_riskset %>%
  dplyr::select_if(is.numeric) 

data_ass_bin <- data_ass_bin[, c('year', 'pa_b', "time_spatial_lag", "time_rel_lag", "time_trade_lag",           
                                 "time_lang_lag", "time_legs_lag", "time_col_lag", "time_igo_lag", "time_env_igo_lag",
                                 "gdppc", "tradeo", "gdpg", "fdiprop", "odaprop", "nrr", "popg", "popd", "fhicl", "fhipr",            
                                 "b_prop", "time_trade_lag_20", "time_igo_lag_20", "time_env_igo_lag_20")]

data_ass_bin_m1 <- data_ass_bin[complete.cases(data_ass_bin[, c('year', 'pa_b', "time_spatial_lag", "time_trade_lag",           
                                                                   "time_legs_lag", "time_col_lag", "time_igo_lag", "time_env_igo_lag",
                                                                   "gdppc", "tradeo", "gdpg", "fdiprop", "odaprop", "nrr", "popg", "popd", "fhipr",            
                                                                   "b_prop")]),]

data_ass_bin_m1 <- data_ass_bin_m1[, c('year', 'pa_b', "time_spatial_lag", "time_trade_lag",           
                                             "time_legs_lag", "time_col_lag", "time_igo_lag", "time_env_igo_lag",
                                             "gdppc", "tradeo", "gdpg", "fdiprop", "odaprop", "nrr", "popg", "popd", "fhipr",            
                                             "b_prop")]


predictors_bin_m1 <- colnames(data_ass_bin_m1)

# bind the logit predictions and the dataset
data_ass_bin_m1 <- data_ass_bin_m1 %>%
  mutate(logit = log(probs_m1/(1-probs_m1))) %>%
  gather(key = "predictors_bin_m1", value = "predictor.value", -logit)

# plot association between explanatory variables and logit predicted values
ggplot(data_ass_bin_m1, aes(logit, predictor.value))+
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess") + 
  theme_bw() + 
  facet_wrap(~ predictors_bin_m1, scales = "free_y")

# linear: fhipr, gdpg, nrr, popg, time_col_lag, time_trade_lag, odaprop, time_spatial_lag, fdiprop, popg
# not very linear: b_prop, gdpc, popd, time_env_igo_lag, time_legs_lag, tradeo


# performing linear transformations of some variables to improve linearity between predicted valeus and predictors
data_bin_riskset$log_gdppc <- log(data_bin_riskset$gdppc)
data_bin_riskset$log_tradeo <- log(data_bin_riskset$tradeo)
data_bin_riskset$log_b_prop <- log(data_bin_riskset$b_prop)
data_bin_riskset$log_popd <- log(data_bin_riskset$popd)

m1 <- glm(pa_b ~ time_spatial_lag + time_col_lag + time_trade_lag + 
            time_env_igo_lag + 
            time_legs_lag + 
            + log_gdppc + gdpg + log_tradeo + fdiprop +
            odaprop + nrr + popg + log_popd + region + fhipr + log_b_prop, data = data_bin_riskset,
          family = binomial(link = "logit"))

summary(m1)


data_ass_bin <- data_bin_riskset %>%
  dplyr::select_if(is.numeric) 

data_ass_bin <- data_ass_bin[, c('year', 'pa_b', "time_spatial_lag", "time_rel_lag", "time_trade_lag",            
                                 "time_lang_lag", "time_legs_lag", "time_col_lag", "time_igo_lag", "time_env_igo_lag",
                                 "gdppc", "tradeo", "gdpg", "fdiprop", "odaprop", "nrr", "popg", "popd", "fhicl", "fhipr",            
                                 "b_prop", "time_trade_lag_20", "time_igo_lag_20", "time_env_igo_lag_20", "log_gdppc", 
                                 "log_b_prop", "log_popd", "log_tradeo")]

data_ass_bin_m1 <- data_ass_bin[complete.cases(data_ass_bin[, c('year', 'pa_b', "time_spatial_lag", "time_trade_lag",           
                                                                "time_legs_lag", "time_col_lag", "time_igo_lag", "time_env_igo_lag",
                                                                "gdppc", "tradeo", "gdpg", "fdiprop", "odaprop", "nrr", "popg", "popd", "fhipr",            
                                                                "b_prop")]),]

data_ass_bin_m1 <- data_ass_bin_m1[, c('year', 'pa_b', "time_spatial_lag", "time_trade_lag",           
                                       "time_legs_lag", "time_col_lag", "time_igo_lag", "time_env_igo_lag",
                                       "gdppc", "tradeo", "gdpg", "fdiprop", "odaprop", "nrr", "popg", "popd", "fhipr",            
                                       "b_prop", "log_gdppc", "log_b_prop", "log_popd", "log_tradeo")]


predictors_bin_m1 <- colnames(data_ass_bin_m1)

# Bind the logit and tidying the data for plot
data_ass_bin_m1 <- data_ass_bin_m1 %>%
  mutate(logit = log(probs_m1/(1-probs_m1))) %>%
  gather(key = "predictors_bin_m1", value = "predictor.value", -logit)

# plot association between predictors and predicted values
ggplot(data_ass_bin_m1, aes(logit, predictor.value))+
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess") + 
  theme_bw() + 
  facet_wrap(~ predictors_bin_m1, scales = "free_y")




## influential cases ##

# look at influential cases
plot(m1, which = 4, id.n = 5) # observation n. 2666 has very high Cook's distance

# Extract model results
m1_data <- augment(m1) %>% 
  mutate(index = 1:n()) 

View(m1_data %>% top_n(3, .cooksd))

ggplot(m1_data, aes(index, .std.resid)) + 
  geom_point(aes(color = pa_b), alpha = .5) +
  theme_bw()

m1_data %>% 
  filter(abs(.std.resid) > 3)


# fit the model without the two very influential variables (n. 2666 & 2593)
data_bin_riskset_i <- data_bin_riskset[-c(2666, 2593), ]

m1i <- glm(pa_b ~ time_spatial_lag + time_col_lag + time_trade_lag + 
                   time_env_igo_lag + 
                   time_legs_lag + 
                   + log_gdppc + gdpg + log_tradeo + fdiprop +
                   odaprop + nrr + popg + log_popd + region + fhipr + log_b_prop,
           data = data_bin_riskset_i,
                 family = binomial(link = "logit"))

summary(m1i)
coeftest(m1i, vcov = vcovHC(m1i, type = "HC0"))
summary(m1)
coeftest(m1, vcov = vcovHC(m1, type = "HC0"))

# results do differ between the two models -> keep the model without the influential cases

data_bin_riskset <- data_bin_riskset[-c(2666, 2593), ]


## multicollinearity ##

car::vif(m1) # very high multicollinearity of most predictors



#### Assumptions checks model 2 ####

# linearity assumption #

ggplot(data_pp_riskset, aes(x = time_spatial_lag_pp, y = pa_pp)) +
 geom_point() + geom_smooth(method = "loess") + ylim(-1, 1)

ggplot(data_pp_riskset, aes(x = time_rel_lag_pp, y = pa_pp)) +
  geom_point() + geom_smooth(method = "loess") + ylim(-1, 1)

ggplot(data_pp_riskset, aes(x = time_trade_lag_pp, y = pa_pp)) +
  geom_point() + geom_smooth(method = "loess") + ylim(-1, 1)

ggplot(data_pp_riskset, aes(x = time_lang_lag_pp, y = pa_pp)) +
  geom_point() + geom_smooth(method = "loess") + ylim(-1, 1)

ggplot(data_pp_riskset, aes(x = time_legs_lag_pp, y = pa_pp)) +
  geom_point() + geom_smooth(method = "loess") + ylim(-1, 1)

ggplot(data_pp_riskset, aes(x = time_col_lag_pp, y = pa_pp)) +
  geom_point() + geom_smooth(method = "loess") + ylim(-1, 1)

ggplot(data_pp_riskset, aes(x = time_igo_lag_pp, y = pa_pp)) +
  geom_point() + geom_smooth(method = "loess") + ylim(-1, 1)

ggplot(data_pp_riskset, aes(x = time_env_igo_lag_pp, y = pa_pp)) +
  geom_point() + geom_smooth(method = "loess") + ylim(-1, 1)

ggplot(data_pp_riskset, aes(x = gdppc, y = pa_pp)) +
  geom_point() + geom_smooth(method = "loess") + ylim(-1, 1)

ggplot(data_pp_riskset, aes(x = tradeo, y = pa_pp)) +
  geom_point() + geom_smooth(method = "loess") + ylim(-1, 1)

ggplot(data_pp_riskset, aes(x = gdpg, y = pa_pp)) +
  geom_point() + geom_smooth(method = "loess") + ylim(-1, 1)

ggplot(data_pp_riskset, aes(x = popd, y = pa_pp)) +
  geom_point() + geom_smooth(method = "loess") + ylim(-1, 1)

ggplot(data_pp_riskset, aes(x = popg, y = pa_pp)) +
  geom_point() + geom_smooth(method = "loess") + ylim(-1, 1)

ggplot(data_pp_riskset, aes(x = fdiprop, y = pa_pp)) +
  geom_point() + geom_smooth(method = "loess") + ylim(-1, 1)

ggplot(data_pp_riskset, aes(x = odaprop, y = pa_pp)) +
  geom_point() + geom_smooth(method = "loess") + ylim(-1, 1)

ggplot(data_pp_riskset, aes(x = nrr, y = pa_pp)) +
  geom_point() + geom_smooth(method = "loess") + ylim(-1, 1)

ggplot(data_pp_riskset, aes(x = fhicl, y = pa_pp)) +
  geom_point() + geom_smooth(method = "loess") + ylim(-1, 1)

ggplot(data_pp_riskset, aes(x = fhipr, y = pa_pp)) +
  geom_point() + geom_smooth(method = "loess") + ylim(-1, 1)

ggplot(data_pp_riskset, aes(x = av_pp, y = pa_pp)) +
  geom_point() + geom_smooth(method = "loess") + ylim(-1, 1)

ggplot(data_pp_riskset, aes(x = time_trade_lag_pp_20, y = pa_pp)) +
  geom_point() + geom_smooth(method = "loess") + ylim(-1, 1)

ggplot(data_pp_riskset, aes(x = time_igo_lag_pp_20, y = pa_pp)) +
  geom_point() + geom_smooth(method = "loess") + ylim(-1, 1)

ggplot(data_pp_riskset, aes(x = time_env_igo_lag_pp_20, y = pa_pp)) +
  geom_point() + geom_smooth(method = "loess") + ylim(-1, 1)

# all variables seem quite linearly related with outcome variable


# heteroskedasticity #

residualPlots(m2) # Tukey test significant
ncvTest(m2) # pb with heteroskedasticity


# normal distribution of residuals #

plot(density(rstudent(m2), na.rm = T)) # appears very normal

qqPlot(m2) # appears linear

# check for outliers #

outlierTest(m2)
influencePlot(m2)

# run the model without the largest outliers

m2_no <- update(m2, subset = rownames(data_pp_riskset) != 6097)
m2_no <- update(m2_no, subset = rownames(data_pp_riskset) != 5064)
m2_no <- update(m2_no, subset = rownames(data_pp_riskset) != 5977)

compareCoefs(m2, m2_no)

# results are slightly different but do not substantively differ

# checking for multicollinearity #

vif(m2)

# very few variables display high multicollinearity 



#### Assumptions checks model 3 ####

# linearity assumption already checked with model 3 #

# heteroskedasticity #

residualPlots(m3e) # Tukey test significant
ncvTest(m3e) # pb with heteroskedasticity



# normal distribution of residuals #

plot(density(rstudent(m3e), na.rm = T)) # appears very normal

qqPlot(m3e) # appears very normal


# check for outliers #
outlierTest(m3e)
influencePlot(m3e)

# run the model without the largest outliers
m3e_no <- update(m3e, subset = rownames(data_pp_riskset) != 7222)
m3e_no <- update(m3e_no, subset = rownames(data_pp_riskset) != 6097)
m3e_no <- update(m3e_no, subset = rownames(data_pp_riskset) != 5233)

compareCoefs(m3e, m3e_no)
summary(m3e_no)
coeftest(m3e_no, vcov = vcovHC(m3e_no, type = "HC0"))
summary(m3e)
coeftest(m3e, vcov = vcovHC(m3e, type = "HC0"))

# some substantive changes in the results

# remove the problematic observations from the dataset
data_pp_riskset_no <- data_pp_riskset[-c(7222, 6097, 5233), ]

# checking for multicollinearity #

vif(m3e)

# very few variables display high multicollinearity
# but higher levels generally speaking than model 3
# model with limited influences displays low multicollinearity



#### Final models (1-3) ####

# model 1 - policy adoption #

# without limited influences
m1 <- glm(pa_b ~ time_spatial_lag + time_lang_lag + time_trade_lag + 
            time_env_igo_lag + log_tradeo +
            time_rel_lag + log_gdppc + gdpg +
            odaprop + nrr + log_popd + region + fhipr + log_b_prop, data = data_bin_riskset,
          family = binomial(link = "logit"))

summary(m1)

anova(m1, test = 'Chi')

AIC(m1)



# model 2 - policy expansion #

# without limited influences
m2 <- lm(pa_pp ~ time_spatial_lag_pp + time_lang_lag_pp + 
           time_rel_lag_pp + time_trade_lag_pp + 
           time_env_igo_lag_pp + gdppc + tradeo + fdiprop + 
           odaprop + nrr + popd + fhipr + av_pp + US + 
           country - 1, data = data_pp_riskset)

summary(m2)

AIC(m2)




# model 3 - policy expansion - conditionality #

# a # exposure #

m3a <- lm(pa_pp ~ time_spatial_lag_pp + time_lang_lag_pp + 
           time_rel_lag_pp + time_trade_lag_pp + 
           time_env_igo_lag_pp + 
           time_env_igo_lag_pp:env_igo_nb + time_trade_lag_pp:tradeo + 
           env_igo_nb + gdppc + tradeo + fdiprop + 
           odaprop + nrr + popd + fhipr + US + av_pp +
           country - 1, data = data_pp_riskset)

summary(m3a)

AIC(m3a)


# b # conditional responsiveness #

# natural resource rents

m3b <- lm(pa_pp ~ time_spatial_lag_pp + time_lang_lag_pp + 
            time_rel_lag_pp + time_trade_lag_pp + 
            time_env_igo_lag_pp + 
            time_lang_lag_pp:nrr + 
            time_rel_lag_pp:nrr + time_trade_lag_pp:nrr + 
            time_env_igo_lag_pp:nrr + 
            env_igo_nb + gdppc + tradeo + fdiprop + 
            odaprop + nrr + popd + fhipr + US + av_pp +
            country - 1, data = data_pp_riskset_no)

summary(m3b)

AIC(m3b)



# pop density

m3c <- lm(pa_pp ~ time_spatial_lag_pp + time_lang_lag_pp + 
            time_rel_lag_pp + time_trade_lag_pp + 
            time_env_igo_lag_pp + 
            time_lang_lag_pp:popd + 
            time_rel_lag_pp:popd + time_trade_lag_pp:popd + 
            time_env_igo_lag_pp:popd + 
            env_igo_nb + gdppc + tradeo + fdiprop + 
            odaprop + nrr + popd + fhipr + US + av_pp +
            country - 1, data = data_pp_riskset_no)

summary(m3c)

AIC(m3c)




# state capacity

m3d <- lm(pa_pp ~ time_spatial_lag_pp + time_lang_lag_pp + 
            time_rel_lag_pp + time_trade_lag_pp + 
            time_env_igo_lag_pp + 
            time_lang_lag_pp:gdppc + 
            time_rel_lag_pp:gdppc + time_trade_lag_pp:gdppc + 
            time_env_igo_lag_pp:gdppc + 
            env_igo_nb + gdppc + tradeo + fdiprop + 
            odaprop + nrr + popd + fhipr + US + av_pp +
            country - 1, data = data_pp_riskset_no)

summary(m3d)

AIC(m3d)



# optimal model

m3e <- lm(pa_pp ~ time_spatial_lag_pp + time_lang_lag_pp + 
            time_rel_lag_pp + time_trade_lag_pp + 
            time_env_igo_lag_pp +
            time_env_igo_lag_pp:popd + time_lang_lag_pp:nrr + 
            env_igo_nb + gdppc + tradeo + fdiprop + 
            odaprop + nrr + popd + fhipr + US + av_pp +
            country - 1, data = data_pp_riskset_no)

summary(m3e)

AIC(m3e)




#### Huber-White standard errors ####

coeftest(m1, vcov = vcovHC(m1, type = "HC0"), cluster = ~country)
coeftest(m2, vcov = vcovHC(m2, type = "HC0"), cluster = ~country)

coeftest(m1_20, vcov = vcovHC(m1_20, type = "HC0"), cluster = ~country)
coeftest(m2_20, vcov = vcovHC(m2_20, type = "HC0"), cluster = ~country)

coeftest(m3a, vcov = vcovHC(m3a, type = "HC0"), cluster = ~country)
coeftest(m3b, vcov = vcovHC(m3b, type = "HC0"), cluster = ~country)
coeftest(m3c, vcov = vcovHC(m3c, type = "HC0"), cluster = ~country)
coeftest(m3d, vcov = vcovHC(m3d, type = "HC0"), cluster = ~country)
coeftest(m3e, vcov = vcovHC(m3e, type = "HC0"), cluster = ~country)






#### Robustness checks ####

#### Check whether the results still hold with a restricted number of influences on countries ####

# model 1 # policy adoption #

m1_20 <- glm(pa_b ~ time_spatial_lag + time_lang_lag + time_trade_lag_20 + 
               time_env_igo_lag_20 + 
               time_rel_lag + log_gdppc + log_tradeo + gdpg +
               odaprop + nrr + log_popd + region + fhipr + log_b_prop, data = data_bin_riskset,
             family = binomial(link = "logit"), control = glm.control(maxit = 100))

summary(m1_20)

anova(m1_20, test = 'Chi')

AIC(m1_20)


# model 2 # policy expansion #

m2_20 <- lm(pa_pp ~ time_spatial_lag_pp + time_lang_lag_pp + 
              time_rel_lag_pp + time_trade_lag_pp_20 + 
              time_env_igo_lag_pp_20 + 
              gdppc + tradeo + fdiprop + 
              odaprop + nrr + popd + fhipr + av_pp + US + 
              country - 1, data = data_pp_riskset)

summary(m2_20)

AIC(m2_20)


# model 3 # conditionality of policy diffusion #

# exposure #

m3_20a <- lm(pa_pp ~ time_spatial_lag_pp + time_lang_lag_pp + 
               time_rel_lag_pp + time_trade_lag_pp_20 + 
               time_env_igo_lag_pp_20 + 
               time_env_igo_lag_pp_20:env_igo_nb + time_trade_lag_pp_20:tradeo + 
               env_igo_nb + gdppc + tradeo + fdiprop + 
               odaprop + nrr + popd + fhipr + US + av_pp +
               country - 1, data = data_pp_riskset)

summary(m3_20a)

AIC(m3_20a)


# receptiveness #

# Natural resources rents
m3_20b <- lm(pa_pp ~ time_spatial_lag_pp + time_lang_lag_pp + 
               time_rel_lag_pp + time_trade_lag_pp_20 + 
               time_env_igo_lag_pp_20 + 
               time_lang_lag_pp:nrr + 
               time_rel_lag_pp:nrr + time_trade_lag_pp_20:nrr + 
               time_env_igo_lag_pp_20:nrr + env_igo_nb + gdppc + tradeo + fdiprop + 
               odaprop + nrr + popd + fhipr + US + av_pp +
               country - 1, data = data_pp_riskset)

summary(m3_20b)

AIC(m3_20b)


# Population density
m3_20c <- lm(pa_pp ~ time_spatial_lag_pp + time_lang_lag_pp + 
               time_rel_lag_pp + time_trade_lag_pp_20 + 
               time_env_igo_lag_pp_20 + 
               time_lang_lag_pp:popd + 
               time_rel_lag_pp:popd + time_trade_lag_pp_20:popd + 
               time_env_igo_lag_pp_20:popd + env_igo_nb + gdppc + tradeo + fdiprop + 
               odaprop + nrr + popd + fhipr + US + av_pp +
               country - 1, data = data_pp_riskset)

summary(m3_20c)

AIC(m3_20c)



# state capacity
m3_20d <- lm(pa_pp ~ time_spatial_lag_pp + time_lang_lag_pp + 
               time_rel_lag_pp + time_trade_lag_pp_20 + 
               time_env_igo_lag_pp_20 + 
               time_lang_lag_pp:gdppc + 
               time_rel_lag_pp:gdppc + time_trade_lag_pp_20:gdppc + 
               time_env_igo_lag_pp_20:gdppc + env_igo_nb + gdppc + tradeo + fdiprop + 
               odaprop + nrr + popd + fhipr + US + av_pp +
               country - 1, data = data_pp_riskset)

summary(m3_20d)

AIC(m3_20d)


# optimal model

m3_20e <- lm(pa_pp ~ time_spatial_lag_pp + time_lang_lag_pp + 
               time_rel_lag_pp + time_trade_lag_pp_20 + 
               time_env_igo_lag_pp_20 +
               time_env_igo_lag_pp_20:popd + time_lang_lag_pp:nrr + 
               env_igo_nb + gdppc + tradeo + fdiprop + 
               odaprop + nrr + popd + fhipr + US + av_pp +
               country - 1, data = data_pp_riskset)

summary(m3_20e)

AIC(m3_20e)



# robust standard errors
coeftest(m3_20a, vcov = vcovHC(m3_20a, type = "HC0"), cluster = ~country)
coeftest(m3_20b, vcov = vcovHC(m3_20b, type = "HC0"), cluster = ~country)
coeftest(m3_20c, vcov = vcovHC(m3_20c, type = "HC0"), cluster = ~country)
coeftest(m3_20d, vcov = vcovHC(m3_20d, type = "HC0"), cluster = ~country)
coeftest(m3_20e, vcov = vcovHC(m3_20e, type = "HC0"), cluster = ~country)


#### Checking each lag significantly improves model fit ####



# model 1 - policy adoption #

# dataset to which the original model was fitted
d1 <- data_bin_riskset[complete.cases(data_bin_riskset[, c('pa_b', 'time_spatial_lag', 'time_lang_lag', 'time_trade_lag', 
                                                           'time_env_igo_lag', 'log_tradeo', 
                                                           'time_rel_lag', 'log_gdppc', 'gdpg', 
                                                           'odaprop', 'nrr', 'log_popd', 'region', 'fhipr', 'log_b_prop')]),]

# spatial

m1s <- glm(pa_b ~ time_lang_lag + time_trade_lag + 
             time_env_igo_lag + log_tradeo +
             time_rel_lag + log_gdppc + gdpg +
             odaprop + nrr + log_popd + region + fhipr + log_b_prop, data = data_bin_riskset,
           family = binomial(link = "logit"))

summary(m1s)

AIC(m1s)

AIC(m1)

anova(m1s, m1, test = "Chi")


# emulation

m1e <- glm(pa_b ~ time_spatial_lag + time_trade_lag + 
             time_env_igo_lag + log_tradeo +
             log_gdppc + gdpg +
             odaprop + nrr + log_popd + region + fhipr + log_b_prop, data = data_bin_riskset,
           family = binomial(link = "logit"))

# fit to the same size of dataset as the original model
m1ed <- glm(pa_b ~ time_spatial_lag + time_trade_lag + 
              time_env_igo_lag + log_tradeo +
              log_gdppc + gdpg +
              odaprop + nrr + log_popd + region + fhipr + log_b_prop, data = d1,
            family = binomial(link = "logit"))


summary(m1e)

AIC(m1e)

AIC(m1)

anova(m1ed, m1, test = "Chi")


# trade

m1t <- glm(pa_b ~ time_lang_lag + time_spatial_lag + 
             time_env_igo_lag + log_tradeo +
             time_rel_lag + log_gdppc + gdpg +
             odaprop + nrr + log_popd + region + fhipr + log_b_prop, data = data_bin_riskset,
           family = binomial(link = "logit"))

# fit to the same size of dataset as the original model
m1td <- glm(pa_b ~ time_lang_lag + time_spatial_lag + 
              time_env_igo_lag + log_tradeo +
              time_rel_lag + log_gdppc + gdpg +
              odaprop + nrr + log_popd + region + fhipr + log_b_prop, data = d1,
            family = binomial(link = "logit"))


summary(m1t)

AIC(m1t)

AIC(m1)

anova(m1td, m1, test = "Chi")



# env IGO

m1ei <- glm(pa_b ~ time_lang_lag + time_trade_lag + 
              time_spatial_lag + log_tradeo +
              time_rel_lag + log_gdppc + gdpg +
              odaprop + nrr + log_popd + region + fhipr + log_b_prop, data = data_bin_riskset,
            family = binomial(link = "logit"))

summary(m1ei)

AIC(m1ei)

AIC(m1)

anova(m1ei, m1, test = "Chi")




# model 2 - policy expansion #

# dataset to which model 2 was originally fitted
d2 <- data_pp_riskset[complete.cases(data_pp_riskset[, c('pa_pp', 'time_spatial_lag_pp', 'time_lang_lag_pp',  
                                                         'time_rel_lag_pp', 'time_trade_lag_pp',  
                                                         'time_env_igo_lag_pp', 'gdppc', 'tradeo', 'fdiprop',  
                                                         'odaprop', 'nrr', 'popd', 'fhipr', 'av_pp', 'US',  
                                                         'country')]),]

# spatial lag

m2s <- lm(pa_pp ~ time_lang_lag_pp + 
            time_rel_lag_pp + time_trade_lag_pp + 
            time_env_igo_lag_pp + gdppc + tradeo + fdiprop + 
            odaprop + nrr + popd + fhipr + av_pp + US + 
            country - 1, data = data_pp_riskset)

# fit to the same size of dataset as the original model
m2sd <- lm(pa_pp ~ time_lang_lag_pp + 
             time_rel_lag_pp + time_trade_lag_pp + 
             time_env_igo_lag_pp + gdppc + tradeo + fdiprop + 
             odaprop + nrr + popd + fhipr + av_pp + US + 
             country - 1, data = d2)

AIC(m2s)

AIC(m2)

anova(m2sd, m2, test = "Chi")


# emulation lag

m2e <- lm(pa_pp ~ time_spatial_lag_pp + time_trade_lag_pp + 
            time_env_igo_lag_pp + gdppc + tradeo + fdiprop + 
            odaprop + nrr + popd + fhipr + av_pp + US + 
            country - 1, data = data_pp_riskset)

# fit to the same size of dataset as the original model
m2ed <- lm(pa_pp ~ time_spatial_lag_pp + time_trade_lag_pp + 
             time_env_igo_lag_pp + gdppc + tradeo + fdiprop + 
             odaprop + nrr + popd + fhipr + av_pp + US + 
             country - 1, data = d2)


AIC(m2e)

anova(m2ed, m2)



# trade lag

m2t <- lm(pa_pp ~ time_spatial_lag_pp + time_lang_lag_pp + 
            time_rel_lag_pp + 
            time_env_igo_lag_pp + gdppc + tradeo + fdiprop + 
            odaprop + nrr + popd + fhipr + av_pp + US + 
            country - 1, data = data_pp_riskset)

# fit to the same size of dataset as the original model
m2td <- lm(pa_pp ~ time_spatial_lag_pp + time_lang_lag_pp + 
             time_rel_lag_pp + 
             time_env_igo_lag_pp + gdppc + tradeo + fdiprop + 
             odaprop + nrr + popd + fhipr + av_pp + US + 
             country - 1, data = d2)


anova(m2td, m2)



# environmental IGO lag

m2ei <- lm(pa_pp ~ time_spatial_lag_pp + time_lang_lag_pp + 
             time_rel_lag_pp + time_trade_lag_pp + 
             gdppc + tradeo + fdiprop + 
             odaprop + nrr + popd + fhipr + av_pp + US + 
             country - 1, data = data_pp_riskset)

# fit to the same size of dataset as the original model
m2eid <- lm(pa_pp ~ time_spatial_lag_pp + time_lang_lag_pp + 
              time_rel_lag_pp + time_trade_lag_pp + 
              gdppc + tradeo + fdiprop + 
              odaprop + nrr + popd + fhipr + av_pp + US + 
              country - 1, data = d2)


AIC(m2ei)

anova(m2eid, m2)

# with limited influences
m2_20ei <- lm(pa_pp ~ time_spatial_lag_pp + time_lang_lag_pp + 
                time_rel_lag_pp + time_trade_lag_pp_20 + 
                gdppc + tradeo + fdiprop + 
                odaprop + nrr + popd + fhipr + av_pp + US + 
                country - 1, data = data_pp_riskset)

summary(m2_20ei)

AIC(m2_20ei)




#### Checking the interaction terms significantly improve model fit ####

# fit m2 to the same size of dataset as m3e

m2no <- lm(pa_pp ~ time_spatial_lag_pp + time_lang_lag_pp + 
             time_rel_lag_pp + time_trade_lag_pp + 
             time_env_igo_lag_pp + gdppc + tradeo + fdiprop + 
             odaprop + nrr + popd + fhipr + av_pp + US + 
             country - 1, data = data_pp_riskset_no)


anova(m2no, m3e)





#### Export models ####


tab_model(m1, m1_20,
          dv.labels = c("Without limited influences", "With limited influences"),
          string.pred = "Coefficient",
          show.ci = F, show.se = T,
          p.style = "stars", digits = 2,
          vcov.type = "HC0",
          file = "Models policy diffusion.doc")

tab_model(m2, m2_20,
          dv.labels = c("Without limited influences", "With limited influences"),
          string.pred = "Coefficient",
          show.ci = F, show.se = T,
          p.style = "stars", digits = 2,
          vcov.type = "HC0",
          file = "Models policy expansion.doc")

tab_model(m3a, m3b, m3c, m3d, m3e,
          dv.labels = c("Without limited influences", "With limited influences"),
          string.pred = "Coefficient",
          show.ci = F, show.se = T,
          p.style = "stars", digits = 2,
          vcov.type = "HC0",
          file = "Models conditional policy expansion.doc")

tab_model(m3_20a, m3_20b, m3_20c, m3_20d, m3_20e,
          dv.labels = c("Without limited influences", "With limited influences"),
          string.pred = "Coefficient",
          show.ci = F, show.se = T,
          p.style = "stars", digits = 2,
          vcov.type = "HC0",
          file = "Models conditional policy expansio.doc")





#### Save the countries used in each model ####

write.csv(countrycode(unique(d1$country), origin = "iso3c", destination = "country.name"), "countries_mod1.csv")
write.csv(countrycode(unique(d2$country), origin = "iso3c", destination = "country.name"), "countries_mod2.csv")


