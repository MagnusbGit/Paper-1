# Load data
setwd("C:/Users/magnusb/Filr/My Files/Oppgave/Data spørreundersøkelse/RETTredigert/Arbeidsfil Paper 1")
data <- read.csv(file="DatasetSpørreundersøkelseOkt2019.csv",header=T,sep=";")
# Load packages
library(tidyverse)
library(rstanarm)
library(dplyr)
library(MASS)
head(data)

# m10 <- polr(q4_10~Alder+Kjønn+ArtTilstede+q2_6.5+q3_1average+q6_average,mydata,Hess=T) # including one model from glm for testing

data <- 
  data %>% 
  dplyr::rename(trust_r_science = q4_10) %>% 
  dplyr::select(RESPID, Alder, ArtTilstede, q2_6.5,trust_r_science,Kjønn) %>%
  dplyr::mutate(ArtTilstede = as.factor(ArtTilstede)) %>%
  dplyr::mutate(trust_r_science = as.factor (trust_r_science))
data$trust_r_science<-factor(data$trust_r_science, levels=c("1", "2", "3","4","5"), ordered=TRUE)
data$Kjønn[data$Kjønn==1] = "Male" 
data$Kjønn[data$Kjønn==2] = "Female" 
data$Kjønn = factor(data$Kjønn,ordered=FALSE)
data$q2_6.5<-data$q2_6.5[data$q2_6.5==0] <- "Damage"
data$q2_6.5<-data$q2_6.5[data$q2_6.5==1] <- "Not damage"
data$q2_6.5 = factor(data$q2_6.5,ordered=FALSE)

str(data)
head(data)

#data %>% 
#  ggplot(aes(x = ph, y = change_seahare_mass_g_fw, colour = fnutrients)) + 
#  geom_point()



#### Distributions #### 
# Trust
tab_trust_r_science <- data %>%
  group_by(trust_r_science) %>%dplyr::summarize(Freq = n()) %>%mutate(Prop = Freq/sum(Freq)) %>%arrange(desc(Prop))
tab_trust_r_science
ggplot(data, aes(trust_r_science)) + 
  geom_bar(aes(y = (..count..)/sum(..count..))) + 
  scale_y_continuous(labels=scales::percent) +
  ylab("Prosent")+
  scale_x_discrete(limits = c("1","2","3","4","5")) 

# Alder
hist(data$Alder)
hist(log(data$Alder))

# Kjønn
tab_Kjønn <- data %>%
  group_by(Kjønn) %>%dplyr::summarize(Freq = n()) %>%mutate(Prop = Freq/sum(Freq)) %>%arrange(desc(Prop))
tab_Kjønn

# ArtTilstede
tab_ArtTilstede <- data %>%
  group_by(ArtTilstede) %>%dplyr::summarize(Freq = n()) %>%mutate(Prop = Freq/sum(Freq)) %>%arrange(desc(Prop))
tab_ArtTilstede



#### Fit model - Rstanarm #### 
# Example from glm:
#m_glm <- glm(trust_r_science ~ ArtTilstede + q2_6.5 + Kjønn + Alder , data = data)
#m_rst <- stan_glm(trust_r_science ~ ArtTilstede + q2_6.5 + Kjønn + Alder,
#                  data = data,
#                  family = gaussian(), # likelihood
#                  iter = 2000,
#                  chains = 4#,
#                  #prior_intercept = normal (0,1),
#                 #prior = normal(0, 2.5),
#                 #prior_aux = exponential(rate=1)
#)

# Ordinal regression model
polrmodel <- polr(trust_r_science ~ ArtTilstede + Kjønn + Alder,data=data,Hess=T) # 

# Bayesian model
# run with few chains and iterations first time when checking that everything is OK
postm <- stan_polr(trust_r_science ~ ArtTilstede + Kjønn + Alder,data=data, 
                   prior = R2(0.1), prior_counts = dirichlet(1),
                   seed = 12345, iter = 1000, chains = 1)

# run model with more chains and iterations (default settings here)
postm <- stan_polr(trust_r_science ~ ArtTilstede + Kjønn + Alder,data=data, 
                   prior = R2(0.25), prior_counts = dirichlet(1),
                   seed = 12345)

#postm <- stan_polr(trust_r_science ~ ArtTilstede + Kjønn + Alder,data=data, 
#                   prior = R2(0.25), prior_counts = dirichlet(1),
#                   seed = 12345, iter = 1000, chains = 1)

# rename when happy and check the model to see if it is okay
m1 <- postm
prior_summary(m1)


#### Check model - Rstanarm####  

# compare prior to the posterior
posterior_vs_prior(m1)
plot(m1, plotfun = "trace") # Should not be able to distinguish chains, they should be centered on parameter estimates in the posterior - looks OK
#?summary.stanreg 
summary(m1) # Rhat are 1.0 = OK. n_eff are high = OK

#?plot.stanreg
plot(m1, plotfun = "intervals") # to show the whole distribution. 
plot(m1, plotfun = "dens_overlay") # the acutal posterior densisty for each chain for each parameter - can report this
plot(m1, plotfun = "areas") # should not be close to zero. 

#library(shinystan)
#launch_shinystan(m_rst) # gives you a broad diagnistics of your model

# posterior predictive check
pp_check(m1, plotfun = "dens_overlay", nreps = 30) # makes 30 lines instead of the total number of samples in that is 4000. 
# you want the lines to be similar to the datasetline. Here we had a lot that missed. A lot of noise in the predictions in the model. 
# this model could potentially be improved. We should try that. 
# each line is one variation of the prediction of the model (all are plausible predictions from the model).
# inlcude extra predictiors if you have some. So go back to parameter selection and model selection. 
pp_check(m_rst, plotfun = "hist", nreps = 10)
pp_check(m_rst, plotfun = "intervals")

# how to improve it? 
# alt1 try log
# try to fit quadratic model (do square on the continious variables) because we have a bell shape when plotting data
# gam - generalized .. model 


data <- 
  data %>%
  dplyr::mutate (Alder = Alder^2)
str(data)

postm2 <- stan_polr(trust_r_science ~ ArtTilstede + Kjønn + Alder,data=data, 
                   prior = R2(0.25), prior_counts = dirichlet(1),
                   seed = 12345, iter=1000, chains = 1)

plot (postm2, plotfun="trace")

summary (postm2)

pp_check(m1, plotfun = "dens_overlay", nreps = 30)
pp_check(postm2, plotfun = "dens_overlay", nreps = 30)
pp_check(postm2, plotfun = "hist", nreps = 10)
pp_check(postm2, plotfun = "intervals")

?waic.stanreg # do this for each model and compare the waic value. If they are the same then model is equally bad. 
waic(m1)
waic(postm2)



#### Loo ####
# loo - Leave One Out. Takes a lot of time. It is a cross-validation (not using a separate dataset).
# k-cross validation - another way is to cut the dataset to use folds. k = cuts 
#?loo



loo1 <- loo(m1) # 
loo2 <- loo(postm2)

loo_compare(loo1,loo2) # comuting the difference between the loo values of the model. 

postm3 <- (change_seahare_mass_g_fw ~ ph + nutrients + ph:nutrients,
                   data = d,
                   family = gaussian(), # likelihood
                   iter = 2000,
                   chains = 4
)
## other ways to improve model? 
# interactions?
# interactions with log-variable?
# qadratic?

# fit the potential models and compare loos as above? 
#example: loo_compare(loo1,loo2,loo3,loo4,loo5)

library(sjPlot)
plot_model(m1)
plot_model(postm2)
# plot_model(m_rst, type = "pred", terms = "nutrients") # possible with continous variables



## Fit more models when we have fixed for problems above ##
#m2<-stan_polr(trust_r_science ~ ArtTilstede + Kjønn + Alder,data=data, 
#              prior = R2(0.1), prior_counts = dirichlet(1),
#              seed = 12345, iter = 1000, chains = 1) 
#summary(m2)


#### brms? Fit model ####

