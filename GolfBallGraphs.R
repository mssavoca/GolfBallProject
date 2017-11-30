#####
# Script to graph golf ball data
#####

#load packages
library(ggplot2); theme_set(theme_bw())
library(tidyr)
library(scales)
library(ggExtra)
library(dplyr)

knitr::opts_chunk$set(message = FALSE, warning = FALSE, fig.show = "animate")
# Example from https://github.com/dgrtwo/gganimate
library(ggplot2)
library(gganimate)

#Set WD, read in data

GB = read.csv("GolfBallsbySite.csv")
BS = read.csv("BallStage.csv")
GB_full = read.csv("GolfBallsComplete.csv")
Mass_loss = read.csv("BallMassLoss.csv")
Mass_loss$Stage = as.factor(Mass_loss$Stage)


# this plot works, all data
GB_plot=ggplot()+geom_point(data=GB,aes(x=Date,y=Number,color=Site))
GB_plot=GB_plot + facet_wrap(~Site, scales="free_x")
GB_plot

# this plot works Recovery rate (balls found per unit effort)
GB_full$Date = as.Date(GB_full$Date, format='%m/%d/%y')
GB_RR=ggplot() + 
  geom_point(data=GB_full,aes(x=Date, y = Recovery.Rate)) + 
  geom_smooth(data=GB_full,aes(x=Date, y = Recovery.Rate), method = "lm", size = 0.5, alpha = 0.25) +
  facet_wrap(Site~Subsite, scales="free") +
  scale_x_date(labels = date_format("%b-%y")) +
  #ggtitle("Golf Ball Recovery Rate") +
  theme_bw() +
  theme(plot.title = element_text(vjust = 0.5)) +
  ylab("Recovery Rate (balls collected per unit effort)")

GB_RR

#data = GB_full[GB_full$Subsite %in% c("15","2") == F,] # to remove the sites with only one collection


# bar plot of ball degredation stage
ball_stage <-ggplot(BS, aes(Ball.Stage, Number)) + 
                      geom_bar(stat = "identity") +
                      xlab("Ball stage") +
                      theme_bw()

ball_stage

# should also do above plot by weight loss by stage


# plot of mass by stage
BM = ggplot(Mass_loss, aes(Mass..g., fill = Stage, colour = Stage)) +
  geom_density(alpha = 0.1) +
  #xlim(-1, 10) +
  xlab("Mass (g)")
theme_bw()

ggMarginal(BM, type = "boxplot", fill="transparent")

BM

#Creating a data frame for vertical lines
d1 = Mass_loss %>% 
      group_by(Stage) %>%
      summarize(Median = median(Mass.loss..g., na.rm=TRUE),
                Mean = mean(Mass.loss..g., na.rm=TRUE))

info = ggplot_build(ML)$data

# Density plot of mass loss by stage
ML = ggplot(Mass_loss, aes(x = Mass.loss..g., fill = Stage, colour = Stage)) +
  geom_density(alpha = 0.1) +
  xlim(-1, 10) +
  xlab("Mass loss (g)") +
  ggtitle("Golf Ball Mass Loss by Degradation Stage") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_vline(xintercept=d1$Median, linetype = "dashed", 
             colour = c("#F8766D", "#A3A500", "#00BF7D", "#00B0F6", "#E76BF3"))


#ggMarginal(ML,aes(x=Mass.loss..g.), 
#                type = "boxplot", fill="transparent") #doesnt look great, prob bc this is a histogram and not a scatterplot

ML

# Violin or boxlpot plot of mass loss by stage
ML_V = ggplot(Mass_loss, aes(x = Stage, y = log(Mass.loss..g.), fill = Stage, colour = Stage)) +
    geom_violin(alpha = 0.3) +
    #geom_boxplot(alpha = 0.3) +
    geom_jitter(width = 0.25, alpha = 0.5) +
    coord_flip() +
    # xlim(-1, 10) +
    xlab("Stage") +
    ylab("Log of mass loss") +
    ggtitle("Golf Ball Mass Loss by Degradation Stage") +
    theme(plot.title = element_text(hjust = 0.5))


#ggMarginal(ML,aes(x=Mass.loss..g.), 
#                type = "boxplot", fill="transparent") #doesnt look great, prob bc this is a histogram and not a scatterplot

ML_V

info = ggplot_build(ML)$data

# bar plot of average mass loss by stage
Ave_ML_stage <-ggplot(d1, aes(Stage, Mean)) + 
  geom_bar(stat = "identity") +
  xlab("Ball stage") +
  ylab("Mean mass loss (g)") +
  ggtitle("Average Mass Loss by Degradation Stage") +
  theme(plot.title = element_text(hjust = 0.5)) 

Ave_ML_stage


#Scatterplot of mass loss by stage
ML_point = ggplot(Mass_loss, aes(x = as.numeric(Mass_loss$Stage), y = Mass.loss..g., 
                                colour = Stage)) +
            ylim(-1, 10) +
            geom_jitter(aes(colour = Stage))
            #geom_boxplot()
  
ML_point




#########################################
# Junk code below here
#########################################
GB$Date = as.Date(GB$Date, "%m/%d/%y")

GB_plot = ggplot(GB, aes(x=JulianDate, y = Number, color = Subsite))
GB_plot + geom_point() + facet_wrap(~Site, scales="free_x")

GB_plot = ggplot(GB, aes(x=JulianDate, y = Number, color = Site))
GB_plot + geom_point() 

GB_plot = ggplot(GB, aes(x=Date, y = Number, color = Subsite) + geom_point() # +
                scale_x_date(format = "%b-%Y")) +scale_color_manual("")


#geom_jitter(aes(colour = Stage)) +
#geom_boxplot(aes(x=Mass.loss..g., y = Stage) )+
#geom_point(aes(x = Mass.loss..g., y = 0, colour = Stage)) +
#rug(jitter(Mass_loss$Mass.loss..g., y = 0)) +