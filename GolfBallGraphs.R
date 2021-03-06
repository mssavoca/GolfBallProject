#####
# Script to graph golf ball data
#####

#load packages ----
library(ggplot2); theme_set(theme_bw())
library(tidyr)
library(scales)
library(ggExtra)
library(dplyr)
library(scales)

knitr::opts_chunk$set(message = FALSE, warning = FALSE, fig.show = "animate")
# Example from https://github.com/dgrtwo/gganimate
library(ggplot2)
library(gganimate)

#Set WD, read in data

GB = read.csv("GolfBallsbySite.csv")
BS = read.csv("BallStage.csv")
BS_D_SS = read.csv("BallStageByDateAndSite_Tidy.csv")
GB_full = read.csv("GolfBallsComplete.csv")
Mass_loss = read.csv("BallMassLoss.csv")
Mass_loss$Stage = as.factor(Mass_loss$Stage)
Mass_loss$mg_Lost = Mass_loss$Mass.loss..g.*1000


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
BS$Season <- as.factor(BS$Season) 
BS$Ball.Stage <- as.factor(BS$Ball.Stage) 

ball_stage_num <-ggplot(BS, aes(Ball.Stage, Number)) + 
  #scale_fill_brewer(palette="BuGu") +
  geom_bar(aes(fill = Season), stat = "identity", position = "stack") +
  xlab("Ball stage") + ylab("Total number") +
  theme(axis.text.x = element_text(size=12), axis.text.y = element_text(size=12),  
        axis.title.x = element_text(size=14),
        axis.title.y = element_text(size=14))


ball_stage_num

ball_stage_prop <-ggplot(BS, aes(Ball.Stage, Prop.within.season)) + 
  geom_bar(aes(fill = Season), stat = "identity", position = "dodge") +
  xlab("Ball stage") + ylab("Proportion of total (within season)") +
  theme_bw(text = element_text(size=20))

ball_stage_prop

# ANOVA on this
m1 <- lm(Number ~ Season*Ball.Stage, data=BS)
fit <- aov(m1)

# should also do above plot by weight loss by stage

##############
# pie and line charts of ball staging by date and subsite
##############
BS_D_SS$Date = as.Date(BS_D_SS$Date, format='%m/%d/%y')
BS_D_SS$Stage = as.factor(BS_D_SS$Stage)

# create a dataframe where each subsite gets an average ball number and proportion per stage
pie_chart_data <- filter(BS_D_SS, Site == "PB") %>%
  group_by(Subsite, Stage) %>%
  summarise(Total.Num = sum(Num.in.stage),
            Ave.Num = mean(Num.in.stage), 
            Ave.Prop = mean(Prop.in.stage)) 

blank_theme <- theme_minimal()+
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.border = element_blank(),
    panel.grid=element_blank(),
    axis.ticks = element_blank(),
    plot.title=element_text(size=14, face="bold")
  )

## Order
try.at.pie$Stage <- ordered(try.at.pie$Stage, levels = c("5", "4", "3", "2","1"))

try.at.pie <- ggplot(pie_chart_data, aes(x = "", y=Ave.Prop, fill=Stage))+
  geom_bar(width = 1, stat = "identity", position = "fill", alpha = 0.9) +
  scale_fill_manual(values = c("#E76BF3", "#00B0F6", "#00BF7D","#A3A500","#F8766D")) +
  coord_polar("y", start=0) +
  blank_theme +
  facet_grid(.~Subsite) +
  theme(axis.text.x=element_blank()) #+
#  geom_text(aes(y = Ave.Num/5 + c(0, cumsum(Ave.Num)[-length(Ave.Num)]), 
#                label = percent(Ave.Num/100)), size=5)

try.at.pie



BS_date_subsite <- ggplot(data=BS_D_SS, aes(Date, Prop.Stage.5, color = Subsite)) +
  geom_line()

BS_date_subsite


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

## Order
Mass_loss$Stage <- ordered(Mass_loss$Stage, levels = c("5", "4", "3", "2","1"))

# Violin or boxlpot plot of mass loss by stage
ML_V = ggplot(Mass_loss, aes(x = Stage, y = log(mg_Lost), fill = Stage)) +
    geom_violin(alpha = 0.5) +
    geom_jitter(width = 0.25, alpha = 0.3) +
    scale_fill_manual(values = c("#006D2C", "#31A354", "#74C476","#BAE4B3","#EDF8E9")) +
    ylim(1.5, 11) +
    xlab("Stage") +
    ylab("Log of Mass Loss (mg)") +
   # ggtitle("Golf Ball Mass Loss by Degradation Stage") +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(axis.text.x = element_text(size=12), axis.text.y = element_text(size=12),  
        axis.title.x = element_text(size=14),
        axis.title.y = element_text(size=14)) +
    theme(legend.position="none")

ML_V + coord_flip()


, fill = Stage, colour = Stage)

# Extracting the colors used for Violin plot above to use in pir chart
g <- ggplot_build(ML_V)
unique(g$data[[1]]["fill"])

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

BS_date_subsite <- ggplot(data=BS_D_SS, aes(Date, Prop.Stage.5, color = Subsite)) +
  geom_line()

#geom_jitter(aes(colour = Stage)) +
#geom_boxplot(aes(x=Mass.loss..g., y = Stage) )+
#geom_point(aes(x = Mass.loss..g., y = 0, colour = Stage)) +
#rug(jitter(Mass_loss$Mass.loss..g., y = 0)) +

#ggMarginal(ML,aes(x=Mass.loss..g.), 
#                type = "boxplot", fill="transparent") #doesnt look great, prob bc this is a histogram and not a scatterplot

