##########
# Script for statistical analyses on golf ball project
##########

#load packages
library(ggplot2); theme_set(theme_bw())
library(tidyr)
library(scales)
library(ggExtra)
library(dplyr)
library(lubridate)

#Set WD, read in data

GB = read.csv("GolfBallsbySite.csv")
BS = read.csv("BallStage.csv")
GB_full = read.csv("GolfBallsComplete.csv")
Mass_loss = read.csv("BallMassLoss.csv")
Mass_loss$Stage = as.factor(Mass_loss$Stage)

#change date column to date object for regression analyses
GB_full$Date <- mdy(GB_full$Date)
#GB_full$Date_lm <-  as.numeric(ymd(GB_full$Date) - years(5)) 
GB_full$Date_num <- as.numeric(GB_full$Date) # changing date to numeric so it can be analyzed

plot(GB_full$Date_num, GB_full$Recovery.Rate)
abline(lm(GB_full$Recovery.Rate ~ GB_full$Date_num))

# run simple linear regressions for different sites
PB <- dplyr::filter(GB_full, Site == "PB")

plot(PB$Date_num, PB$Recovery.Rate)
abline(lm(PB$Recovery.Rate ~ PB$Date_num))

# run simple linear regressions for different sites
PB8_1 <- dplyr::filter(GB_full, Subsite == "8_1" & Season == 1)

plot(PB8_1$Date_num, PB8_1$Recovery.Rate)
abline(lm(PB8_1$Recovery.Rate ~ PB8_1$Date_num))

LM_PB8_1 <- lm(PB8_1$Recovery.Rate~PB8_1$Date_num)
summary(LM_PB8_1)

# run simple linear regressions for different sites
PB8_2 <- dplyr::filter(GB_full, Subsite == "8_2" & Season == 1)

plot(PB8_2$Date_num, PB8_2$Recovery.Rate)
abline(lm(PB8_2$Recovery.Rate ~ PB8_2$Date_num))

LM_PB8_2 <- lm(PB8_2$Recovery.Rate~PB8_2$Date_num)
summary(LM_PB8_2)

PB18_1 <- dplyr::filter(GB_full, Subsite == "18-1")
LM_PB18_1 <- lm(PB18_1$Date_num ~ PB18_1$Recovery.Rate)

plot(PB18_1$Date_num, PB18_1$Recovery.Rate)
abline(lm(PB18_1$Recovery.Rate ~ PB18_1$Date_num))

summary(LM_PB18_1)
