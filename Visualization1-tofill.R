# Intro to Visualization in R using ggplot2

###
# LEARNING OUTCOMES: 
# PART 1 first graphs with ggplot2 package, varying symbols and colors, adding title, saving plot
# PART 2 combining with linear regression, making a transformation to have a better regression model, adding regression line and confidence intervals

WHO = read.csv("/Users/jaimegarcia/Desktop/DESKTOP_101/code/R_code/SMU/WHO.csv")

str(WHO)

# PART 1
# Plot from first lecture on R

plot(WHO$GNI, WHO$FertilityRate)

# Let's redo this using ggplot 

#install.packages("ggplot2")
library(ggplot2)

# Can use function qplot or ggplot
# ggplot is more flexible

# first we create the object we want to graph
scatterplot0 = qplot(x = GNI, y = FertilityRate, data = WHO)
# then we graph it
scatterplot0 + geom_point()

# same with ggplot
scatterplot = ggplot(WHO, aes(x = GNI, y = FertilityRate))
scatterplot + geom_point()

# Q1 Make a line graph instead:
### Q1 ANSWER HERE
scatterplot + geom_line()

# Switch back to our points:
scatterplot + geom_point()

# Q2 Redo the plot with blue triangles (shape 17) instead of circles and increase size from default of siZe 1 to size 2 and 3:
### Q2 ANSWER HERE
scatterplot + geom_point(color = "blue", size = 3, shape = 17)

# Q3 What do you obtain if you select shape 8 and color darkred?
### Q3 ANSWER HERE
scatterplot + geom_point(color = "green", size = 3, shape = 8)

# Q4 Add title "Fertility Rate vs. Gross National Income" using function ggtitle to the plot obtained in Q2:
### Q4 ANSWER HERE 
fertilityGNIplot = scatterplot + geom_point(color = "blue", size = 2) + ggtitle("Fertility Rate vs. Gross National Income")
fertilityGNIplot
# Q5 Save your plot by typing:
# fertilityGNIplot = (put answer here)
### Q5 ANSWER HERE

pdf("/Users/jaimegarcia/Desktop/DESKTOP_101/code/R_code/SMU/PlotFertilityGNI.pdf")
print(fertilityGNIplot)
# closing the connection with PDF output
dev.off()

# PART 2
# MORE ADVANCED SCATTERPLOTS 


# Color the points by region 
ggplot(WHO, aes(x = GNI, y = FertilityRate, color = Region)) + geom_point()

# Q6 Color the points according to life expectancy:
### Q6 ANSWER HERE
ggplot(WHO, aes(x = GNI, y = FertilityRate, color = LifeExpectancy)) + geom_point()

# Is the fertility rate of a country was a good predictor of the percentage of the population under 15?
# Plotting percentage of the population under 15 as a function of fertility rate
ggplot(WHO, aes(x = FertilityRate, y = Under15)) + geom_point()

mod0 = lm(Under15 ~ FertilityRate, data = WHO)
summary(mod0)
ggplot(WHO, aes(x = FertilityRate, y = Under15)) + geom_point() + geom_smooth(method = "lm")
# without the confidence intervals, line in red
ggplot(WHO, aes(x = FertilityRate, y = Under15)) + geom_point() + geom_smooth(method = "lm", se = FALSE, color = "red")
# with 99% confidence intervals (default is 0.95)
ggplot(WHO, aes(x = FertilityRate, y = Under15)) + geom_point() + geom_smooth(method = "lm", level = 0.99, color = "red")


#going back to original graph
ggplot(WHO, aes(x = FertilityRate, y = Under15)) + geom_point()

# Q7 What transformation could we make? Transform the data, re-run and show regression line on plot
### Q7 ANSWER HERE
ggplot(WHO, aes(x = log(FertilityRate), y = Under15)) + geom_point() + geom_smooth(method = "lm")
