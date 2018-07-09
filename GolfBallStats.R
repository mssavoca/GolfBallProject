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
GB_full$Season <- as.factor(GB_full$Season) 

Mass_loss = read.csv("BallMassLoss.csv")
Mass_loss$Stage = as.factor(Mass_loss$Stage)

#change date column to date object for regression analyses
GB_full$Date <- mdy(GB_full$Date)
#GB_full$Date_lm <-  as.numeric(ymd(GB_full$Date) - years(5)) 
GB_full$Date_num <- as.numeric(GB_full$Date) # changing date to numeric so it can be analyzed

# this plot works Recovery rate (balls found per unit effort)
GB_full$Date = as.Date(GB_full$Date, format='%m/%d/%y')
GB_RR=ggplot(data=GB_full,aes(x=Date, y = Recovery.Rate)) + 
  geom_point() + 
  geom_smooth(method = "lm", size = 0.5, alpha = 0.25) +
  facet_grid(.~Season, scales="free") +
  scale_x_date(labels = date_format("%b-%y")) +
  #ggtitle("Golf Ball Recovery Rate") +
  theme_bw() +
  theme(plot.title = element_text(vjust = 0.5)) +
  ylab("Recovery Rate (balls collected per unit effort)")

GB_RR

#looking at Recovery Rate of PB sites 18-1, 18-2, 8-1, 8-2 by season
PB <- dplyr::filter(GB_full, Site == "PB")

GB_PB=ggplot(data=PB[PB$Subsite != "6" ,],aes(x=Date, y = Recovery.Rate)) +   # Removes Subsite 6
  geom_point(aes(x=Date, y = Recovery.Rate, color = Season)) + 
  geom_smooth(aes(color = Season), method = "lm", size = 0.5, alpha = 0.25) +
  facet_grid(Subsite~Season, scales="free") +
  scale_x_date(labels = date_format("%b-%y")) +
  #ggtitle("Golf Ball Recovery Rate") +
  theme_bw() +
  theme(plot.title = element_text(vjust = 0.5)) +
  ylab("Recovery Rate (balls collected per unit effort)")

GB_PB



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
