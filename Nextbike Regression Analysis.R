library(ggplot2)
library(dplyr)
library(tidyverse)
library(moderndive)
library(tidyr)
library(corrplot)
library(ggpubr)
library(leaps)
library(MASS)
library(car)
library(glmnet)

trips <- read.csv("NextBike Regression Data.csv", sep=',' ,header=T)

#remove riverside museum
trips <- trips[-15,]

#log transform ridership 
trips$logcount=log10(trips$COUNT)


h1 <- trips %>%
  ggplot(aes(COUNT)) + geom_histogram(binwidth=800, colour="black", fill="grey") +
  theme_minimal() +
  labs(x="Station Ridership", y="Count") + theme(axis.title.x = element_text(size=14), axis.text.x = element_text(size=14), axis.text.y = element_text(size=14), axis.title.y = element_text(size=14))

h2 <- trips %>%
  ggplot(aes(logcount)) + geom_histogram(binwidth=0.09, colour="black", fill="grey") + theme_minimal() +
  labs(x="Log of Station Ridership", y="Count") + theme(axis.title.x = element_text(size=14), axis.text.x = element_text(size=14), axis.text.y = element_text(size=14), axis.title.y = element_text(size=14))

#plot 2 histograms together 
ggarrange(h1, h2,
          ncol = 2, nrow = 1)

#scatterplots
trips %>% 
  ggplot(aes(x=NO_CAR,y=COUNT)) + 
  geom_point() + theme_minimal() + theme(axis.title.x = element_text(size=14), axis.title.y = element_text(size=14), axis.text.x = element_text(size=14), axis.text.y = element_text(size=14)) + ylab("Log of Station Ridership\n") + xlab("\nSlope (in degrees)")

trips %>% 
  ggplot(aes(x=JOB_DENSITY,y=logcount)) + 
  geom_point() + theme_minimal() + theme(axis.title.x = element_text(size=14), axis.title.y = element_text(size=14), axis.text.x = element_text(size=14), axis.text.y = element_text(size=14)) + ylab("Log of Station Ridership\n") + xlab("\nSlope (in degrees)")

#box plot 
trips %>%
  ggplot(aes(x = ST_NUM1, y = logcount)) + geom_boxplot() + ylab("Daily Ridership\n") + theme(axis.title.x=element_blank(), axis.title.y = element_text(size=10)) 


p1 <- trips %>% 
  ggplot(aes(x=uni,y=logcount)) + 
  labs(x="\nUniversity Percentage (Ages 17-21)", y="Log of Station Ridership\n") +
  geom_point() + theme_minimal() + theme(axis.title.x = element_text(size=14), axis.text.x = element_text(size=14), axis.text.y = element_text(size=14), axis.title.y = element_text(size=14))

p2 <- trips %>% 
  ggplot(aes(x=population_density,y=logcount)) + 
  labs(x="\nPopulation Density (people per square km)", y="") +
  geom_point() + theme_minimal() + theme(axis.title.x = element_text(size=14), axis.text.x = element_text(size=14), axis.text.y = element_text(size=14), axis.title.y = element_text(size=14))

#plot 2 scatterplots together 
ggarrange(p1, p2,
          ncol = 2, nrow = 1)

trips %>% 
  ggplot(aes(x=CYL_PER1,y=COUNT)) + 
  labs(x="JOB_DENSITY", y="Yearly Ridership") +
  geom_point() + theme_minimal()


#spearman's rank correlation plot
columns <- trips[,-c(1,2, 3, 8, 14, 17, 19)]
cor <- cor(columns, method="spearman")
colnames(cor) <- c("Ridership", "Transit Station Distance", "No Car (%)", "University (%)", "Income Deprived (%)", "Employment Deprived (%)", "Cycling Lane Distance", "Slope", "Station Capacity", 
                   "Downtown Distance", "Population Density", "Age 16-34 (%)", "Log Ridership")
rownames(cor) <- c("Ridership", "Transit Station Distance", "No Car (%)", "University (%)", "Income Deprived (%)", "Employment Deprived (%)", "Cycling Lane Distance", "Slope", "Station Capacity", 
                   "Downtown Distance", "Population Density", "Age 16-34 (%)", "Log Ridership")
corrplot(cor)


#LASSO REGRESSION
trips_selection  <- trips[,-c(1,2, 3, 8, 16, 17)]

# Predictor variables
x <- model.matrix(logcount~., trips_selection)

# Outcome variable
y <- trips_selection$logcount

glmnet(x, y, alpha = 1, lambda = NULL)
set.seed(123) 
cv <- cv.glmnet(x, y, alpha = 1)

# Display the best lambda value
cv$lambda.min

model <- glmnet(x, y, alpha = 1, lambda = cv$lambda.min)

# Display regression coefficients
coef(model)

#lasso regression model
lm.all <- lm(logcount ~  ST_DIST + DOWNTOWN_DIST + CYL_PER1 + employment + uni + population_density, data=trips)
summary(lm.all)

#check multicollinearity
vif(lm.all)

#STEPWISE REGRESSION
full.model <- lm(logcount~., data = trips_selection)

step.model <- stepAIC(full.model, direction = "both",
                      trace = FALSE)
summary(step.model)

#multicollinearity 
vif(step.model)

plot(step.model,which=1, sub.caption = "", main = "")
hist(step.model$residuals, xlab="Residuals", main="")
qqplot<-plot(step.model,which=2, sub.caption = "", main="")

#REMOVING 3 STATIONS WITH TOP TRIPS

trips1 <- trips[-c(10,12, 13),]
trips1$logcount=log10(trips1$COUNT)

trips_selection1  <- trips1[,-c(1,2, 3, 8, 16, 17)]
full.model1 <- lm(logcount~., data = trips_selection1)

step.model1 <- stepAIC(full.model1, direction = "both",
                       trace = FALSE)
summary(step.model1)
vif(step.model1)

lm.all1 <- lm(logcount ~  NEAR_DIST + DOWNTOWN_DIST + population_density + CYL_PER1 + employment + uni, data=trips1)
summary(lm.all1)

plot(step.model,which=1, sub.caption = "", main = "")
hist(step.model$residuals, xlab="Residuals", main="")
qqplot<-plot(step.model,which=2, sub.caption = "", main="")
