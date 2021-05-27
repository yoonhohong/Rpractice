# Cell viability assay (cck-8)
# LPS treatment 
# BV2 cell line 

# two-way ANOVA
# dependent variables: concentration, time 
# independent variable: value (absorbance?), relative to the value at 50ng/mL for 0 hour.

lps = read.csv("lps_bv2.csv")
head(lps)

lps$concentration = factor(lps$concentration, 
                           levels = c("50ng/mL", 
                                      "100ng/mL",
                                      "500ng/mL",
                                      "1000ng/mL"))
lps$time = factor(lps$time, 
                  levels = c("0h", "8h", "24h"))

res = aov(value ~ concentration*time, data = lps)
summary(res)
# no significant effects of concentration, time and 
# time:concentration interaction 


# bar plot with error bar 
# error bar; standard error of mean 

library(dplyr)

temp = lps %>%
  filter(time == "0h" & concentration == "50ng/mL") %>%
  select(value)
base = mean(temp$value) # base = mean value at LPS 50ng/mL for 0h 

lps$rel_value = lps$value/base # relative value 
  
df = lps %>%
  group_by(concentration, time) %>%
  summarise(mean = mean(rel_value), 
            sd = sd(rel_value),
            n = n()) %>%
  mutate(se = sd/n)

library(ggplot2)

ggplot(df, aes(concentration, mean, fill = time)) + 
  geom_bar(stat = 'identity', position = position_dodge()) + 
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se), 
                width = 0.2, 
                position = position_dodge(0.9)) + 
  labs(y = "Cell viability (relative)")




