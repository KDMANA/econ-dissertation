install.packages("stargazer")
install.packages("foreign")
install.packages("dplyr")
install.packages("coefplot")

library(stargazer)
library(dplyr)
library(foreign)
library(ggplot2)
library(coefplot)

# Assign the variable 'mydata' with the excel data file
mydata <- read.csv("C:/BScData.csv")

# Display the structure of the data
str(mydata)

# Display variable names
names(mydata)

# Display summary statistics of the dataset
summary(mydata)

# Run a multiple linear regression on the original variables
regression <- lm(GDP ~ FE.Wales + FE.Scotland + FE.North.East + FE.North.West + FE.Yorkshire...the.Humber + FE.London + FE.West.Midlands + FE.East.Midlands + FE.South.East + FE.South.West + FE.East.of.England + FE.Northern.Ireland, data = mydata)
summary(regression)

# Display regression results using stargazer
stargazer(regression, title = "Multiple regression of GDP (£m) on the number of females employed in different regions of the UK (1000s)")

# Create a new dataset with log-transformed variables
mydata2 <- mutate(mydata, lnGDP = log(GDP), lnFE.Wales = log(FE.Wales), lnFE.Scotland = log(FE.Scotland), lnFE.North.East = log(FE.North.East), lnFE.North.West = log(FE.North.West), lnFE.Yorkshire...the.Humber = log(FE.Yorkshire...the.Humber), lnFE.London = log(FE.London), lnFE.West.Midlands = log(FE.West.Midlands), lnFE.East.Midlands = log(FE.East.Midlands), lnFE.South.East = log(FE.South.East), lnFE.South.West = log(FE.South.West), lnFE.East.of.England = log(FE.East.of.England), lnFE.Northern.Ireland = log(FE.Northern.Ireland))

# Run a multiple linear regression on log-transformed variables
regression2 <- lm(lnGDP ~ lnFE.Wales + lnFE.Scotland + lnFE.North.East + lnFE.North.West + lnFE.Yorkshire...the.Humber + lnFE.London + lnFE.West.Midlands + lnFE.East.Midlands + lnFE.South.East + lnFE.South.West + lnFE.East.of.England + lnFE.Northern.Ireland, data = mydata2)
summary(regression2)

# Display regression results using stargazer
stargazer(regression2, title = "The percentage change of GDP with respect to the percentage change in the number of females employed in each region of the UK")

# Plot residuals against fitted values
residualPlot <- ggplot(aes(x = .fitted, y = .resid), data = fortify(regression)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  labs(x = "Fitted Values", y = "Residuals") +
  geom_point(color = "#FF6699")
residualPlot

# Get predicted values
predicted <- predict(regression)

# Add residuals to the original dataset
mydata$residuals <- residuals(regression)

# Plot predicted vs. residuals
predictedPlot <- ggplot(mydata, aes(x = .fitted, y = residuals)) +
  geom_segment(aes(xend = .fitted, yend = predicted), alpha = 0.2) +
  geom_point(aes(color = residuals)) +
  scale_color_gradient2(low = "#FFCC00", mid = "red", high = "#FF33FF") +
  guides(color = FALSE) +
  geom_point(aes(y = predicted), shape = 16) +
  theme_bw()
predictedPlot

# Subset the data for the first 10 observations
mydata3 <- head(mydata, 10)

# Run a regression on the subset
regression3 <- lm(GDP ~ FE.Wales + FE.Scotland + FE.North.East + FE.North.West + FE.Yorkshire...the.Humber + FE.London + FE.West.Midlands + FE.East.Midlands + FE.South.East + FE.South.West + FE.East.of.England + FE.Northern.Ireland, data = mydata3)
summary(regression3)