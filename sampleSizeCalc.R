# two sample t-test 
# two sample wilcoxon rand sum test 

install.packages('pwr')
library(pwr)

# one sample proportion test; to calculate sample size 
pwr.p.test(h = ES.h(p1=0.75, p2=0.5), 
           sig.level = 0.05, 
           power = 0.8,
           alternative = "greater")
# If we’re correct that our coin lands heads 75% of the time, 
# we need to flip it at least 23 times to have an 80% chance of 
# correctly rejecting the null hypothesis at the 0.05 significance level.

p.out <- pwr.p.test(h = ES.h(p1 = 0.75, p2 = 0.50),
                    sig.level = 0.05, 
                    power = 0.80, 
                    alternative = "greater") # alternative = "two-sided" (default)
plot(p.out)

# one sample propotion test; to calculate power
pwr.p.test(h = ES.h(p1 = 0.75, p2 = 0.50),
           sig.level = 0.01, 
           n = 40,
           alternative = "greater")

# more on effect size 

# effect size as “the degree to which the null hypothesis is false.” 
# In our coin flipping example, this is the difference 
# between 75% and 50%. 
# We could say the effect was 25% but recall that 
# we had to transform the absolute difference in proportions 
# to another quantity using the ES.h function. 
# This is a crucial part of using the pwr package correctly: 
#   You must provide an effect size on the expected scale. 
# arcsine transformation function; ES.h()

pwr.p.test(h = c(0.2,0.5,0.8),
           n = 20,
           sig.level = 0.05)

n <- seq(10,100,10)
p.out <- pwr.p.test(h = 0.5,
                    n = n,
                    sig.level = 0.05)
data.frame(n, power = sprintf("%.2f%%", p.out$power * 100))

# two-sample proportion test 
pwr.2p.test(h = ES.h(p1=0.55, p2=0.5), sig.level = 0.05, power = 0.8)
# We need to sample 1,565 males and 1,565 females 
# to detect the 5% difference with 80% power

pwr.2p.test(h = ES.h(p1 = 0.10, p2 = 0.05), sig.level = 0.05, power = .80)

# Even though the absolute difference between proportions is the same
# (5%), the optimum sample size is now 424 per group. 
# 10% vs 5% is actually a bigger difference than 55% vs 50%. 
# A heuristic approach for understanding why is 
# to compare the ratios: 55/50 = 1.1 while 10/5 = 2.
                                                                     
# two-sample test for proportions, unequal sample sizes 
pwr.2p2n.test(h = ES.h(p1 = 0.5, p2 = 0.55), n1 = 543, n2 = 675, sig.level = 0.05)

# t tests 
pwr.t.test(n=30, d=0.5, sig.level = 0.05)

# effect size: d (cf. h in proportion test)
# d = (m1 - m2)/sigma 
# sigma: common standard deviation 

pwr.t.test(d = 0.5, sig.level = 0.05, power = 0.90, alternative = "greater", 
           type = "one.sample") 
# default type = "two.sample" 
# if you want paired t-test, set type = "paired" 
# note that in that case standard deviation should be for 
# the difference in pairs, not within pairs. 
# if you have only standard deviation for within pairs, 
# you should multiply it by sqrt(2)

# chisq.test: goodness of fit 
null <- rep(0.25, 4)
alt <- c(3/8, rep((5/8)/3, 3))
ES.w1(null,alt)

pwr.chisq.test(w=ES.w1(null,alt), N=100, df=(4-1), sig.level=0.05)
# effect size argument: w 
# function to calculate effect size: ES.w1() 
pwr.chisq.test(w=ES.w1(null,alt), df=(4-1), power=0.8, sig.level = 0.05)

# chisq.test: test of association 
prob = matrix(c(0.1,0.2, 0.4, 0.3), ncol=2, 
              dimnames = list(c("M", "F"), c("Floss", "No Floss")))
prob
ES.w2(prob)
pwr.chisq.test(w=ES.w2(prob), df=(2-1)*(2-1), sig.level = 0.05, 
               power = 0.8)
# you can do the same using pwr.p.test
pwr.2p.test(h=ES.h(p1=1/5, p2=2/5), sig.level = 0.05, power = 0.8)

# 

delta_tx = 11 
delta_placebo = 6
delta_sd = seq(6,8,0.1)
d1 = (delta_tx - delta_placebo)/delta_sd

p.out = c()
for (i in 1:length(d1)) {
  p.out[i] = pwr.t.test(d = d1[i], sig.level = 0.05, power = 0.8)$n
}

# Wilcoxon rank sum test 

z1 = qnorm((1-0.05/2), 0, 1) # upper (alpha/2)th percentile in standard normal distribution 
z2 = qnorm((1-0.2), 0, 1) # upper (beta)th percentile in standard normal distribution 

t = 1/2 # treatment fraction
delta_est = 10 # estimate of the difference in means (from summary statistics)
sd_est = 9.5 # estimate of the common standard deviation 
pi_est = 0.5 + delta_est/(2*sd_est*sqrt(pi)) # equation #16 (ref. Rahardja D. et al., Statistics in Biopharmaceutical Research, 2009)

N = (z1+z2)^2/(12*t*(1-t)*(pi_est-0.5)^2) # equation #10 (ref. the same as above)
N # sample size of each group (according to Noether's method)










