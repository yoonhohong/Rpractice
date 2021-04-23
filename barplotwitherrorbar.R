# bar plot with error bars 
# error can represent sd (standard deviation) or se (standard error)
# se = sd / sqrt(n) 

df = ToothGrowth
str(df)
df$dose = factor(df$dose)
levels(df$dose)

library(dplyr)

summary_df = df %>%
  group_by(supp, dose) %>%
  summarise(mean = mean(len), 
            sd = sd(len))

library(ggplot2)  
p = ggplot(summary_df, aes(dose, mean, fill = supp)) + 
  geom_bar(stat="identity", position=position_dodge()) + 
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), 
                  width=.2,
                  position=position_dodge(.9)) 

p = p + labs(title = "Tooth length per dose", 
         x = "Dose (mg)", 
         y = "Length") + 
  theme_bw()
p + scale_fill_brewer(palette = "Spectral")

cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
p + scale_fill_manual(values = cbbPalette)


# http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/

  
# generalize solution!

# http://www.sthda.com/english/wiki/ggplot2-error-bars-quick-start-guide-r-software-and-data-visualization

data_summary <- function(data, varname, groupnames){
  require(plyr)
  summary_func <- function(x, col){
    c(mean = mean(x[[col]], na.rm=TRUE),
      sd = sd(x[[col]], na.rm=TRUE))
  }
  data_sum<-ddply(data, groupnames, .fun=summary_func, varname)
  # ddply: split dataframe by groupnames, and apply .fun
  data_sum <- plyr::rename(data_sum, c("mean" = varname)) 
  # dplyr::rename vs. plyr::rename
  return(data_sum)
}

df2 = data_summary(df, varname = "len", 
                   groupnames = c('supp', 'dose'))

p = ggplot(df2, aes(dose, len, fill = supp)) + 
  geom_bar(stat="identity", position=position_dodge()) + 
  geom_errorbar(aes(ymin=len-sd, ymax=len+sd), 
                width=.2,
                position=position_dodge(.9)) 
p




