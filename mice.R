library(dplyr)
library(mice)
library(foreign) # to import Stata DTA files
library(car)     # for recode

set.seed(145)

## Import ANES dataset
anesimp <- read.dta("anesimputation.dta", 
                    convert.factors = FALSE, 
                    missing.type = TRUE)

for (i in 1:ncol(anesimp)){
  anesimp[,i] <- ifelse(anesimp[,i]<0, NA, anesimp[,i])
}

anesocc <- read.csv("anesocc.csv",sep=";",na.strings=c("","NA"))

# Selecting occupation now and industry now variables
anesocc2 <- anesocc %>%
  dplyr::select(caseid, dem_occnow, dem_indnow)

# Coding any text that includes "manu" in it as respondent working in
# manufacturing, excluding manuver

anesocc2 <- anesocc2 %>% 
  mutate(manuf = case_when((grepl("manu",dem_occnow)&!grepl("manuver",dem_occnow)) ~ 1,
                           grepl("manu",anesocc2$dem_indnow) ~ 1,
                           is.na(dem_occnow) ~ NA_real_,
                           is.na(dem_indnow) ~ NA_real_,
                           !is.na(dem_occnow) ~ 0,
                           !is.na(dem_indnow) ~ 0)
  )


anesocc2 <- anesocc2 %>% 
  dplyr::select(manuf)

# combining by columns as they are sorted in the same order
anesimp <- cbind(anesimp,anesocc2)

maimp <- read.dta("ma.dta")
anesimp <- merge(x=anesimp, y=maimp, by=c("sample_state"))

# Recode variables 

anesimp$patriot_amident <- recode(anesimp$patriot_amident, 
                                  "5=0; 4=1; 3=2; 2=3; 1=4")

anesimp$econ_ecnext_x <- recode(anesimp$econ_ecnext_x, 
                                "1=0; 2=1; 3=2; 4=3; 5=4")

anesimp$pid_x <- recode(anesimp$pid_x, 
                        "1=0; 2=1; 3=2; 4=3; 5=4; 6=5; 7=6")

anesimp$dem_edugroup_x <- recode(anesimp$dem_edugroup_x, 
                                 "1=0; 2=1; 3=2; 4=3; 5=4")

# Treat manuf as a factor 
anesimp$manuf <- as.factor(anesimp$manuf)

# Save the dataframe as another object so that we can use the original dataframe
# for multiple imputation
anesimpor <- anesimp 

## Transform variables for regression
# Treat nationalism as continuous
anesimpor$patriot_amident <- as.numeric(anesimpor$patriot_amident)
# Treat party id as continuous 
anesimpor$pid_x <- as.numeric(anesimpor$pid_x)
# Treat china_econ as dichotomous 
anesimpor$china_econ <- recode(anesimpor$china_econ, "1=0; 3=0; 2=1")
anesimpor$china_econ <- as.factor(anesimpor$china_econ)

# Take the log of Chinese M&A variables - add a small number as variable
# contains 0s
anesimpor$LogMANO <- log(anesimpor$MANo+1.01)

## Estimate an OLS regression

fitols <- lm(ft_hclinton ~ manuf + pid_x + patriot_amident + 
               china_econ + LogMANO, data=anesimpor)

summary(fitols)













