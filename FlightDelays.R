# STAT 512: Group Project
# @author: Aastha Jairath
# @date: May 1, 2020

# Dataset: On-Time Performance: US Domestic Airlines (January 2020)
# Source: United States Bureau of Transportation Statistics

library(dplyr)
library(MASS)
library(corrplot)
library(ggplot2)
library(ggcorrplot)

data <- read.csv("ontime_data.csv", header=TRUE)
summary(data)
dim(data) 

# We focus on flights originating from the top 10 busiest airports of US for 2019-20 (www.faa.gov)
data <- subset(data, data$ORIGIN=='ATL' | data$ORIGIN=='LAX' | data$ORIGIN=='ORD' | data$ORIGIN=='DFW' | data$ORIGIN=='DEN' | data$ORIGIN=='JFK' | data$ORIGIN=='SFO' | data$ORIGIN=='LAS' | data$ORIGIN=='SEA' | data$ORIGIN=='CLT')
dim(data)

# Target variable: ARR_DELAY (Delay in Arrival)

# Only cancelled and diverted flights have missing values for DEP_DELAY and/or ARR_DELAY
# Removing rows with missing values for DEP_DELAY and/or ARR_DELAY for further analysis
# Dropping the ORIGIN_CITY_NAME, DEST_CITY_NAME, CANCELLED and DIVERTED variables
data <- data[complete.cases(data), ] 
unique(data$CANCELLED)
unique(data$DIVERTED)
data <- within(data, rm('ORIGIN_CITY_NAME', 'DEST_CITY_NAME', 'CANCELLED', 'DIVERTED'))

y <- as.Date(data$FL_DATE, format="%m/%d/%Y")
data$FL_DATE <- as.numeric(format(y,"%d")) # converting format of FL_DATE from M/D/Y to D
unique(data$FL_DATE)

data$FL_DATE[which(data$FL_DATE==5 | data$FL_DATE==12 | data$FL_DATE==19 | data$FL_DATE==26)] <- "Sun"
data$FL_DATE[which(data$FL_DATE==6 | data$FL_DATE==13 | data$FL_DATE==20 | data$FL_DATE==27)] <- "Mon"
data$FL_DATE[which(data$FL_DATE==7 | data$FL_DATE==14 | data$FL_DATE==21 | data$FL_DATE==28)] <- "Tues"
data$FL_DATE[which(data$FL_DATE==1 | data$FL_DATE==8 | data$FL_DATE==15 | data$FL_DATE==22 | data$FL_DATE==29)] <- "Wed"
data$FL_DATE[which(data$FL_DATE==2 | data$FL_DATE==9 | data$FL_DATE==16 | data$FL_DATE==23 | data$FL_DATE==30)] <- "Thurs"
data$FL_DATE[which(data$FL_DATE==3 | data$FL_DATE==10 | data$FL_DATE==17 | data$FL_DATE==24 | data$FL_DATE==31)] <- "Fri"
data$FL_DATE[which(data$FL_DATE==4 | data$FL_DATE==11 | data$FL_DATE==18 | data$FL_DATE==25)] <- "Sat"
data$FL_DATE[data$FL_DATE=="Sun"] <- 1
data$FL_DATE[data$FL_DATE=="Mon"] <- 2
data$FL_DATE[data$FL_DATE=="Tues"] <- 3
data$FL_DATE[data$FL_DATE=="Wed"] <- 4
data$FL_DATE[data$FL_DATE=="Thurs"] <- 5
data$FL_DATE[data$FL_DATE=="Fri"] <- 6
data$FL_DATE[data$FL_DATE=="Sat"] <- 7
unique(data$FL_DATE) # values from 1-7, representing days of the week from Sun-Sat

# Converting all qualitative continuous variables to numeric discrete
FL_DAY <- as.numeric(as.character(data$FL_DATE))
OP_UNIQUE_CARRIER <- as.numeric(data$OP_UNIQUE_CARRIER)
ORIGIN <- as.numeric(data$ORIGIN)
DEST <- as.numeric(data$DEST)


matrix <- data.frame(FL_DAY, OP_UNIQUE_CARRIER, ORIGIN, DEST, data$DEP_DELAY, data$ARR_DELAY, data$AIR_TIME, data$DISTANCE)
names(matrix) <- c('FL_DAY', 'OP_UNIQUE_CARRIER', 'ORIGIN', 'DEST', 'DEP_DELAY', 'ARR_DELAY', 'AIR_TIME', 'DISTANCE')

# Plotting the correlation matrix
ggcorrplot(cor(matrix), title="Correlation Matrix", outline.color = "darkgrey", colors = c("navy", "gray97", "coral"), tl.cex=10, tl.col="black", legend.title="Correlation")

# Boxplots of ARRIVAL_DELAY and DEPARTURE_DELAY
ggplot(data, aes(x = "", y = DEP_DELAY)) + geom_boxplot() + ggtitle("Delay in Departure Time (in minutes)") + ylab("Delay in Departure Time")
ggplot(data, aes(x = "", y = ARR_DELAY)) + geom_boxplot() + ggtitle("Delay in Arrival Time (in minutes)") + ylab("Delay in Arrival Time")

# Removing outliers: all flights with ARRIVAL_DELAY and/or DEPARTURE_DELAY >= 1440 minutes
matrix <- subset(matrix, (data$DEP_DELAY<1440 && data$ARR_DELAY<1440))
matrix <- na.omit(matrix) 

# Observing the scatterplot of ARR_DELAY vs DEP_DELAY
ggplot(matrix, aes(x=DEP_DELAY, y=ARR_DELAY)) + geom_point() + geom_abline(aes(col="red", intercept=0, slope=1)) + scale_color_identity(labels="Line y=x", guide="legend", name="Legend") + ggtitle("Delay in Arrival Time vs Delay in Departure Time (in minutes)") + xlab("Delay in Departure Time") + ylab("Delay in Arrival Time")

# Converting ARR_DELAY and DEP_DELAY to positive variable to assess transformations
ARRIVAL_DELAY <- matrix$ARR_DELAY - min(matrix$ARR_DELAY) + 1
DEPARTURE_DELAY <- matrix$DEP_DELAY - min(matrix$DEP_DELAY) + 1

# Assessing need for transformation
boxcox(ARRIVAL_DELAY ~ FL_DAY+OP_UNIQUE_CARRIER+ORIGIN+DEST+DEPARTURE_DELAY+AIR_TIME+DISTANCE, data=matrix)

# Fitting a linear regression model on ARRIVAL_DELAY
m <- lm(ARRIVAL_DELAY ~ FL_DAY+OP_UNIQUE_CARRIER+ORIGIN+DEST+DEPARTURE_DELAY+AIR_TIME+DISTANCE, data=matrix)

# Assessing linear regression assumptions
par(mfrow = c(2, 2))
plot(m)

# Backward stepwise regression to look for the subset with most optimal fit
step <- stepAIC(m, direction="backward")
step # original model gives us the best fit

# ANOVA test
anova(m) # corroborates previous findings

# Final results
summary(m) # all parameter estimates, standard errors, t-statistics, p-values and R-squared value
confint(m) # 95% CI for all parameter estimates