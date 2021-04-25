
# survival prediction 
# https://rviews.rstudio.com/2017/09/25/survival-analysis-with-r/?aliId=eyJpIjoiNFJcL0ZJa3ROSVo0d3VYQmwiLCJ0IjoiM2lCMG96YXdtUGNlR1wvXC9pS0NGVCt3PT0ifQ%253D%253D


library(survival)
library(ranger)
library(ggplot2)
library(dplyr)
library(ggfortify)


head(veteran) # dataset from survival package 
?veteran
str(veteran)

# KM estimates of survival probability over time 
survobj = with(veteran, Surv(time = time, event = status))
survobj # + means censoring 
km_fit = survfit(Surv(time, status) ~ 1, data = veteran)
summary(km_fit)
autoplot(km_fit) # ggfortify  

km_fit_tx = survfit(Surv(time, status) ~ trt, 
                    data = veteran)
autoplot(km_fit_tx) # ggfortify  

vet = mutate(veteran, AG = ifelse(age>=60, "OV60", "LT60"), 
       AG = factor(AG))
km_fit_age = survfit(Surv(time, status) ~ AG, vet)
autoplot(km_fit_age)

# Cox proportional hazard regression model 
vet = within(vet, {
  trt = factor(trt, labels = c("standard", "test"))
  prior = factor(prior, labels = c("no", "yes"))
  rm(AG)
  })

cox = coxph(Surv(time, status) ~ ., data = vet) 
# coxph from survival package
# fit cox regression model to estimate cumulative hazard of subject i over time
summary(cox)
cox_fit = survfit(cox) 
# create survival curves from formula (KM), Cox model, or AFT model
autoplot(cox_fit)

# variable selection; backward selection 
library(rms) # regression modeling strategies 
form = Surv(time, status) ~ trt + celltype + karno + 
  age + prior + diagtime
cox = cph(form, data = vet, surv = TRUE) # cph from rms packagee 
cox_bw = fastbw(cox, rule = 'aic') # backward selection with Akaike information criteria
cox_bw$names.kept
form_new = reformulate(cox_bw$names.kept, form[[2]])
fit_cox = cph(form_new, data = vet, surv = TRUE) 

# generalze; backward selection, cox model 
selectCox <- function(formula,data,rule="aic"){
  require(rms)
  require(prodlim)
  fit <- cph(formula,data,surv=TRUE)
  bwfit <- fastbw(fit,rule=rule)
  if (length(bwfit$names.kept)==0){
    newform <- reformulate("1",formula[[2]])
    newfit <- prodlim(newform, data=data)
  }
  else{
    newform <- reformulate(bwfit$names.kept, formula[[2]])
    newfit <- cph(newform,data,surv=TRUE)
  }
  out <- list(fit=newfit,In=bwfit$names.kept)
  out$call <- match.call()
  class(out) <- "selectCox"
  out
}

cox_bw = selectCox(formula = form, data = vet, rule = 'aic')
cox_bw$fit 

# Aalen's additive regression model 
aareg = aareg(Surv(time, status) ~ ., data = vet)
aareg
# aa_fit = survfit(aareg)
# ?survfit 
autoplot(aa_fit)

# random survival forest model 
# use ranger package 
rf_fit <- ranger(Surv(time, status) ~ .,
                data = vet,
                mtry = 4, 
                importance = "permutation",
                splitrule = "extratrees",
                verbose = TRUE)
# ?ranger
# by default, num.trees = 500
# see importance, splitrule 

# Average the survival models
death_times <- rf_fit$unique.death.times 
surv_prob <- data.frame(rf_fit$survival) # survival probability estimates over time
avg_prob <- sapply(surv_prob,mean)

# Plot the survival models for each patient
plot(rf_fit$unique.death.times,rf_fit$survival[1,], 
     type = "l", 
     ylim = c(0,1),
     col = "red",
     xlab = "Days",
     ylab = "survival",
     main = "Patient Survival Curves")

#
cols <- colors() # 657 colors 
for (n in sample(c(2:dim(vet)[1]), 20)){
  lines(rf_fit$unique.death.times, rf_fit$survival[n,], 
        type = "l", col = cols[n])
}
lines(death_times, avg_prob, lwd = 2)
legend(500, 0.7, legend = c('Average = black'), 
       bty = 'n')

# variable importance 
vi <- data.frame(sort(round(rf_fit$variable.importance, 4), 
                      decreasing = TRUE))
names(vi) <- "importance"
head(vi)

cat("Prediction Error = 1 - Harrell's c-index = ", 
    r_fit$prediction.error)
## Prediction Error = 1 - Harrell's c-index =  0.3087233

# Set up for ggplot
kmi <- rep("KM",length(km_fit$time))
km_df <- data.frame(km_fit$time,km_fit$surv,kmi)
names(km_df) <- c("Time","Surv","Model")

coxi <- rep("Cox",length(cox_fit$time))
cox_df <- data.frame(cox_fit$time,cox_fit$surv,coxi)
names(cox_df) <- c("Time","Surv","Model")

rfi <- rep("RF",length(rf_fit$unique.death.times))
rf_df <- data.frame(rf_fit$unique.death.times,avg_prob,rfi)
names(rf_df) <- c("Time","Surv","Model")

plot_df <- rbind(km_df,cox_df,rf_df)

p <- ggplot(plot_df, aes(x = Time, y = Surv, color = Model))
p + geom_line()

# personalized prediction 
set.seed(1)
sel = sample(dim(vet)[1], 3)
test = vet[sel,]
train = vet[-sel,]
# fit random forest model w/ ranger
rf_fit <- ranger(Surv(time, status) ~ .,
                 data = train,
                 mtry = 4, 
                 importance = "permutation",
                 splitrule = "extratrees",
                 verbose = TRUE)
# predict in test data 
pred = predict(rf_fit, data = test)

# plot average survival curve 
# plot survival curve for the individual 
# prediction error curves for the average and individual 

death_times <- rf_fit$unique.death.times 
surv_prob <- data.frame(rf_fit$survival) # survival probability estimates over time
avg_prob <- sapply(surv_prob,mean)

rfi = rep("RF", length(rf_fit$unique.death.times))
rf_df <- data.frame(rf_fit$unique.death.times,avg_prob, rfi)
names(rf_df) <- c("Time","Surv", "Src")

indiv = rep("Individual", length(pred$unique.death.times)) 
pred_df = data.frame(pred$unique.death.times, 
                     pred$survival, indiv)
names(pred_df) = c("Time", "Surv", "Src")

df = rbind(rf_df, pred_df)
p <- ggplot(df, aes(x = Time, y = Surv, col = Src))
p + geom_line() 

# random survival forest model 
# use randomSurvivalForest package 

require(randomForestSRC)
fit_rf = rfsrc(form, train, ntree = 1000, 
      mtry = 4, 
      splitrule = "logrank", 
      importance = "permute") 

# survival curve 
# prediction error curve 
library(pec)

predictSurvProb(fit_cox, newdata = test, times = 30)
predictSurvProb(fit_rf, newdata = test, times = 30) 

plotPredictSurvProb(fit_cox, newdata = test, lty = 1)
plotPredictSurvProb(fit_rf, newdata = test, lty = 2, add = T)




