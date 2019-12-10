## Analysis for Umbers et al. (2019) Conservation Science and Practice
## Educating the enemy: Harnessing learned avoidance behaviour in wild predators to increase survival of reintroduced Southern Corroboree Frogs
## Contact: k.umbers@westernsydney.edu.au and/or julia.riley87@gmail.com

# ---- CLEAR WORKSPACE ---- #
cat("\014") 
rm(list=ls())

# Load libraries
library(lme4)
library(lsmeans)
library(readxl)
library(ggplot2)
library(gridExtra)
library(plyr)
source("https://gist.githubusercontent.com/benmarwick/2a1bb0133ff568cbe28d/raw/fb53bd97121f7f9ce947837ef1a4c65a73bffb3f/geom_flat_violin.R")



# ##-------------------## ANALYSIS FOR DETECTION EXPERIMENT ##-------------------##

# Load and Review the Data
data <- read.csv("frog_detect_data.csv")
str(data)
head(data)

# Generalised Linear Mixed Effect Model
# Binomial Distribution
modelint <- glmer(seen ~ treatment + distance + (1|number), data=data, family = binomial(link="logit"))
summary (modelint)
attr(VarCorr(modelint), "sc")^2 #Calculation of Residual Variance

# Multiple Comparisons between Model Colours
lsmeans(modelint, pairwise ~ treatment, adjust="Tukey")

# Back-calculation of the Probability of Dectection based on GLMM predictions
a<-lsmeans(modelint, pairwise ~ treatment, adjust="Tukey")
summary(a$lsmeans, type="response") #Displays the predicted probability of attack for each model colour


# Plotting the Probability of Detection

# Summary Table for Raw Data
totals <- aggregate(seen ~ treatment, data, sum)
str(totals)
totals

# Define Theme for the Plots

# Reference Material
source("https://gist.githubusercontent.com/benmarwick/2a1bb0133ff568cbe28d/raw/fb53bd97121f7f9ce947837ef1a4c65a73bffb3f/geom_flat_violin.R")

raincloud_theme = theme(
  text = element_text(size = 10),
  axis.title.x = element_text(size = 13),
  axis.title.y = element_text(size = 13),
  axis.text = element_text(size = 10),
  axis.text.x = element_text(angle = 0, vjust = 0.5),
  legend.title=element_text(size=16),
  legend.text=element_text(size=16),
  legend.position = "right",
  plot.title = element_text(lineheight=.8, face="bold", size = 16),
  panel.border = element_blank(),
  panel.grid.minor = element_blank(),
  panel.grid.major = element_blank(),
  axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
  axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid'))


# Plot for the Raw Count Data
plot_detect <- ggplot(data=totals, aes(x = treatment, y = seen, fill = treatment)) +
  geom_bar(stat="identity", alpha=0.8) +
  scale_fill_manual(values=c("gray19", "seagreen", "goldenrod1")) +
  theme_bw() +
  raincloud_theme +
  theme(legend.position="none") +
  labs(x = "Treatment", y = "Number Seen") +
  scale_x_discrete(labels = c('Black','Striped','Yellow')) +
  theme(axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
        axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid'),
        axis.text.x=element_text(size=10,angle=0,hjust=0.5),
        axis.text.y=element_text(size=10,angle=0,hjust=0.5),
        axis.title.x=element_text(size=13),
        axis.title.y=element_text(size=13)) 

plot_detect



# ##-------------------## ANALYSIS FOR BIRD ATTACK EXPERIMENT ##-------------------##

# Load and Review the Data
deploy12 <- read.csv("deploy12_recovered.csv")
str(deploy12)
deploy12$deployment <- as.factor(deploy12$deployment)

# Generalised Linear Mixed Effect Model
# Binomial Distribution
birdattacks12 <- glmer(attacked_bird ~ treatment * deployment + (1|row) + (1|location), data = deploy12, family = binomial(link = "logit"))
summary(birdattacks12)
attr(VarCorr(birdattacks12), "sc")^2 #Calculation of Residual Variance

# Multiple Comparisons between Model Colours

# Reference Material
vignette("using-lsmeans")

# Between Deployments for each Model Colour seperately
lsmeans(birdattacks12, pairwise ~ deployment|treatment, adjust="Tukey")

# Between Model Colours for Each Deployment seperately
lsmeans(birdattacks12, pairwise ~ treatment|deployment, adjust="Tukey")
# Back-calculation of the Probability of Dectection based on GLMM predictions
x<-lsmeans(birdattacks12, pairwise ~ treatment|deployment, adjust="Tukey")
summary(x$lsmeans, type="response") #shows the average probability of attack


# Plotting the Probability of Attacks

# Deployment 1
# Raw Summary of the Number of Bird Attacks
deploy1 <- subset(deploy12, deployment=="1")
totals1 <- aggregate(cbind(attacked_bird, recovered) ~ treatment, deploy1, sum)
totals1$prop = totals1$attacked_bird/totals1$recovered
str(totals1)
totals1

# Plot for the Raw Data
plot1 <- ggplot(data=totals1, aes(x = treatment, y = prop, fill = treatment)) +
  ylim(0, 0.3) +
  geom_bar(stat="identity", alpha=0.8) +
  scale_fill_manual(values=c("gray50", "seagreen", "goldenrod1")) +
  theme_bw() +
  raincloud_theme +
  theme(legend.position="none") +
  labs(x = "Treatment", y = "Proportion Attacked by Birds") +
  scale_x_discrete(labels = c('Black','Striped','Yellow')) +
  theme(axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
        axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid'),
        axis.text.x=element_text(size=10,angle=0,hjust=0.5),
        axis.text.y=element_text(size=10,angle=0,hjust=0.5),
        axis.title.x=element_text(size=13),
        axis.title.y=element_text(size=13)) 

plot1


# Plot Predicted Bird Attacks for Deployment 1
# Get Predictions
deploy12$fit <- predict(birdattacks12, type="response")
fit1<-deploy12[which(deploy12$deployment=="1"),]

# Calculate Upper and Lower Confidence Intervals
lb <- function(x) mean(x) - sd(x)
ub <- function(x) mean(x) + sd(x)
sumld <- ddply(fit1, ~treatment, summarise, mean = mean(fit), median = median(fit), lower = lb(fit), upper = ub(fit))
head(sumld)

# Make Plot
plot2 <- ggplot(data = fit1, aes(y = fit, x = treatment, fill = treatment)) +
  ylim(0, 1)+
  geom_flat_violin(position = position_nudge(x = 0.2, y = 0), alpha = 0.8, show.legend=FALSE) +
  geom_point(aes(y = fit, color = treatment), position = position_jitter(width = .15), size = .5, alpha = 0.8, show.legend=FALSE) +
  #geom_point(data = sumld, aes(x = treatment, y = mean), position = position_nudge(x = 0.3), size = 2.5) +
  #geom_errorbar(data = sumld, aes(ymin = lower, ymax = upper, y = mean), position = position_nudge(x = 0.3), width = 0) + 
  geom_boxplot(width = .1, outlier.shape = NA, alpha = 0.5, show.legend=FALSE) +
  #expand_limits(x = 5.25) +
  #guides(fill = FALSE) +
  #guides(color = FALSE) +
  scale_colour_manual(values=c("grey50", "seagreen", "goldenrod1")) +
  scale_fill_manual(values=c("grey50", "seagreen", "goldenrod1")) +
  theme_bw() +
  #show.legend
  #theme(legend.title = element_blank())+
  #theme(legend.position = "none")+
  raincloud_theme +
  labs(x = "Treatment", y = "Probability of Bird Attack") +
  scale_x_discrete(labels = c('Black','Striped','Yellow'))

plot2



# Deployment 2
# Raw Summary of the Number of Bird Attacks
deploy2 <- subset(deploy12, deployment=="2")
totals2 <- aggregate(cbind(attacked_bird, recovered) ~ treatment, deploy2, sum)
totals2$prop = totals2$attacked_bird/totals2$recovered
str(totals2)
totals2


# Plot for the Raw Data
plot1.2 <- ggplot(data=totals2, aes(x = treatment, y = prop, fill = treatment)) +
  geom_bar(stat="identity", alpha=0.8) +
  scale_fill_manual(values=c("gray50", "seagreen", "goldenrod1")) +
  theme_bw() +
  ylim(0, 0.3) +
  raincloud_theme +
  theme(legend.position="none") +
  labs(x = "Treatment", y = "Proportion Attacked by Birds") +
  scale_x_discrete(labels = c('Black','Striped','Yellow')) +
  theme(axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
        axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid'),
        axis.text.x=element_text(size=10,angle=0,hjust=0.5),
        axis.text.y=element_text(size=10,angle=0,hjust=0.5),
        axis.title.x=element_text(size=13),
        axis.title.y=element_text(size=13)) 

plot1.2


# Plot Predicted Bird Attacks for Deployment 1
# Get Predictions
fit2<-deploy12[which(deploy12$deployment=="2"),]

# Calculate Upper and Lower Confidence Intervals
lb <- function(x) mean(x) - sd(x)
ub <- function(x) mean(x) + sd(x)
sumld<- ddply(fit2, ~treatment, summarise, mean = mean(fit), median = median(fit), lower = lb(fit), upper = ub(fit))
head(sumld)

# Make Plot
plot2.2 <- ggplot(data = fit2, aes(y = fit, x = treatment, fill = treatment)) +
  ylim(0, 1) +
  geom_flat_violin(position = position_nudge(x = 0.2, y = 0), alpha = 0.8, show.legend=FALSE) +
  geom_point(aes(y = fit, color = treatment), position = position_jitter(width = .15), size = .5, alpha = 0.8, show.legend=FALSE) +
  #geom_point(data = sumld, aes(x = treatment, y = mean), position = position_nudge(x = 0.3), size = 2.5) +
  #geom_errorbar(data = sumld, aes(ymin = lower, ymax = upper, y = mean), position = position_nudge(x = 0.3), width = 0) + 
  geom_boxplot(width = .1, outlier.shape = NA, alpha = 0.5, show.legend=FALSE) +
  #expand_limits(x = 5.25) +
  #guides(fill = FALSE) +
  #guides(color = FALSE) +
  scale_colour_manual(values=c("grey19", "seagreen", "goldenrod1")) +
  scale_fill_manual(values=c("grey19", "seagreen", "goldenrod1")) +
  theme_bw() +
  raincloud_theme +
  labs(x = "Treatment", y = "Probability of Bird Attack") +
  scale_x_discrete(labels = c('Black','Striped','Yellow'))

plot2.2
grid.arrange(plot1, plot2, plot1.2, plot2.2, ncol=2, nrow=2, widths = c(1, 2), padding=5)


