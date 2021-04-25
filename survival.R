library(survival)
str(lung)
# see that status is coded 1 or 2
lung$sex = factor(lung$sex, levels = c(1, 2),
                  labels = c("male", "female"))
  
?Surv
# see how event should be coded, in our case, 1 or 2 (death)
?survfit

km.fit = survfit(Surv(time = time, event = status) ~ 1, 
                 data = lung)
# fit survival curve (KM survival probability estimates)

km.fit
summary(km.fit)
# this is survival function table 

library(survminer)
ggsurvplot(km.fit)
?ggsurvplot

km.fit.sex = survfit(Surv(time = time, event = status) ~ sex, 
                 data = lung)

ggsurvplot(km.fit.sex, 
           pval = T,
           risk.table = T, 
           conf.int = T, 
           surv.median.line = 'hv', 
           fun = "pct", # "event', 'cumhaz'
           palette = c("blue", "red"), 
           legend.labs = c("male", "female"), 
           legend.title = "", 
           xlab = "Time (Days)")

summary(km.fit.sex)$table

# coxph regression

cox = coxph(Surv(time = time, event = status) ~ 
              age + sex + ph.ecog, data = lung)
summary(cox)
ggforest(cox, data = lung)

cox.fit = survfit(cox, data = lung) 
# note that survfit takes formula Surv object ~ categorical variable, 
# or previously fitted model just like in this case 
ggsurvplot(cox.fit)

sex_df = with(lung, data.frame(
  sex = c("male", "female"),
  age = rep(mean(age, na.rm = T), 2), 
  ph.ecog = rep(mean(ph.ecog, na.rm = T), 2)
))
  
cox.fit.sex = survfit(cox, newdata = sex_df, data = lung) 
# note that you should provide newdata, and original data as well 
ggsurvplot(cox.fit.sex, 
           ggtheme = theme_bw())

# test the assumption of constant hazard ratio over time 
cox.zph(cox)



