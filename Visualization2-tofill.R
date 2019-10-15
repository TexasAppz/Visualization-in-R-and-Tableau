# Visualization for Predictive Policing

# Learning outcomes
# Part 1 Data formatting, heat maps, ordering factors on graphs
# Part 2 Geographic maps

# Loading the data and making sure it is in a format we can use
mvt = read.csv("/Users/jaimegarcia/Desktop/DESKTOP_101/code/R_code/SMU/Visualization in R/mvt.csv")
str(mvt)
mvt = read.csv("/Users/jaimegarcia/Desktop/DESKTOP_101/code/R_code/SMU/Visualization in R/mvt.csv", stringsAsFactors=FALSE)
str(mvt)
# Convert the Date variable to a format that R will recognize:
mvt$Date = strptime(mvt$Date, format="%m/%d/%y %H:%M")
# Extract the hour and the day of the week:
mvt$Weekday = weekdays(mvt$Date)
mvt$Hour = mvt$Date$hour
str(mvt)

# PART 1

# we want to plot the total number of crimes by day of the week so first we have to compute this information
table(mvt$Weekday)
WeekdayCounts = table(mvt$Weekday)

# Save this table as a data frame:
WeekdayCounts = as.data.frame(table(mvt$Weekday))
str(WeekdayCounts) 

# Now we are ready to plot!
library(ggplot2)
ggplot(WeekdayCounts, aes(x=Var1, y=Freq)) + geom_line()  
# We get this error message because Var1 is a factor, need to tell R we want to connect the points with a single line
ggplot(WeekdayCounts, aes(x=Var1, y=Freq)) + geom_line(aes(group=1))  
# Q1 What is wrong with this graph? What do the next lines do?
### Q1 ANSWER HERE
WeekdayCounts$Var1 = factor(WeekdayCounts$Var1, ordered=TRUE, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday","Saturday"))
ggplot(WeekdayCounts, aes(x=Var1, y=Freq)) + geom_line(aes(group=1))

#Q2 How do we add "Day of the week" as x-label using xlab and "Total Motor Vehicle Thefts" as y-label using ylab?
### Q2 ANSWER HERE
ggplot(WeekdayCounts, aes(x=Var1, y=Freq)) + geom_line(aes(group=1)) + xlab("Day of the Week") + ylab("Total motor Vehicle Theft")

# Adding the Hour of the Day

# Q3 Instead of creating a table about counts by weekdays only, update the code above to create a counts table for the weekday and hour 
# and save it as data frame DayHourCounts
### Q3 ANSWER HERE
DayHourCounts = as.data.frame(table(mvt$Weekday, mvt$Hour))
str(DayHourCounts)

# Q4 What is the issue with Var2 and how does the next line do?
DayHourCounts$Hour = as.numeric(as.character(DayHourCounts$Var2))
### Q4 ANSWER HERE

# trying a plot (wrong way)
ggplot(DayHourCounts, aes(x=Hour, y=Freq)) + geom_line()
# Q5 how do we tell R to group the data according to Var1 and color according to Var1 in the aesthetic mapping?
### Q5 ANSWER HERE
ggplot(DayHourCounts, aes(x=Hour, y=Freq)) + geom_line(aes(group=Var1, color=Var1))

# Fix the order of the days and redo the plot:
DayHourCounts$Var1 = factor(DayHourCounts$Var1, ordered=TRUE, levels=c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))


# Separate the weekends from the weekdays and redo the plot, coloring by type:
DayHourCounts$Type = ifelse((DayHourCounts$Var1 == "Sunday") | (DayHourCounts$Var1 == "Saturday"), "Weekend", "Weekday")
ggplot(DayHourCounts, aes(x=Hour, y=Freq)) + geom_line(aes(group=Var1, color=Type, alpha = 0.5)) 


# Make a heatmap using geom_tile:
ggplot(DayHourCounts, aes(x = Hour, y = Var1)) + geom_tile(aes(fill = Freq))

# Change the label on the legend, and get rid of the y-label:
ggplot(DayHourCounts, aes(x = Hour, y = Var1)) + geom_tile(aes(fill = Freq)) + scale_fill_gradient(name="Total MV Thefts", low = "white", high = "red") + theme(axis.title.y = element_blank())
  
# Q6 Change the color scheme by saying you want low values in white and high in red inside scale_fill_gradient
### Q6 ANSWER HERE


# PART 2
# Maps

# Install and load two new packages:
install.packages("maps")
# Windows users
install.packages("ggmap")
# Mac users
install.packages("ggmap", type = "source")
library(maps)
library(ggmap)

# Load a map of Chicago into R:
chicago = get_map(location = "chicago", zoom = 11)

# Look at the map
ggmap(chicago)

# Plot the first 100 motor vehicle thefts:
ggmap(chicago) + geom_point(data = mvt[1:100,], aes(x = Longitude, y = Latitude))
# Q7 What happens if we try to plot the entire mvt data set? data=mvt
# ANSWER Q7 HERE

# Trying to create a table to create crime counts by latitude and longitude
LatLonCounts = as.data.frame(table(mvt$Longitude, mvt$Latitude))
# Round our latitude and longitude to 2 digits of accuracy, and create a crime counts data frame for each area to fix the error
LatLonCounts = as.data.frame(table(round(mvt$Longitude,2), round(mvt$Latitude,2)))

str(LatLonCounts)

# Convert our Longitude and Latitude variable to numbers, 1st attempt:
LatLonCounts$Long = as.numeric(LatLonCounts$Var1)
LatLonCounts$Lat = as.numeric(LatLonCounts$Var2)
str(LatLonCounts)
# Q8 What looks wrong in this output?
### Q8 ANSWER HERE

LatLonCounts$Long = as.numeric(as.character(LatLonCounts$Var1))
LatLonCounts$Lat = as.numeric(as.character(LatLonCounts$Var2))
str(LatLonCounts)


# Plot these points on our map:
ggmap(chicago) + geom_point(data = LatLonCounts, aes(x = Long, y = Lat, color = Freq, size=Freq))

# Q9 Change the color scheme to have yellow for low values and red for high values using scale_colour_gradient:
### Q9 ANSWER HERE

# We can also use the geom_tile geometry (alpha measures the transparency of the tile which is supposed to be of color fill)
ggmap(chicago) + geom_tile(data = LatLonCounts, aes(x = Long, y = Lat, alpha = Freq), fill="red")


### NEW PLOTS WITH MURDERS.CSV FILE
# Geographical Map on US

murders = read.csv("/Users/jaimegarcia/Desktop/DESKTOP_101/code/R_code/SMU/Visualization in R/murders.csv")
str(murders)

# Load the map of the US and plot it
statesMap = map_data("state")
str(statesMap)
ggplot(statesMap, aes(x = long, y = lat, group = group)) + geom_polygon(fill = "white", color = "black") 

# Q9 Comment on State in both
### Q9 ANSWER HERE

# Create a new variable called region with the lowercase names to match the statesMap:
murders$region = tolower(murders$State)

# Join the statesMap data and the murders data into one dataframe:
murderMap = merge(statesMap, murders, by="region")
str(murderMap)

# Plot the number of murder on our map of the United States:
ggplot(murderMap, aes(x = long, y = lat, group = group, fill = Murders)) + geom_polygon(color = "black") + scale_fill_gradient(low = "white", high = "red", guide = "legend")

# Plot a map of the population:
ggplot(murderMap, aes(x = long, y = lat, group = group, fill = Population)) + geom_polygon(color = "black") + scale_fill_gradient(low = "white", high = "red", guide = "legend")

# Create a new variable that is the number of murders per 100,000 population:
murderMap$MurderRate = murderMap$Murders / murderMap$Population * 100000

# Redo our plot with murder rate:
ggplot(murderMap, aes(x = long, y = lat, group = group, fill = MurderRate)) + geom_polygon(color = "black") + scale_fill_gradient(low = "white", high = "red", guide = "legend")

# Q10 Redo the plot, removing any states with murder rates above 10:
### Q10 ANSWER HERE
ggplot(murderMap, aes(x = long, y = lat, group = group, fill = MurderRate)) + geom_polygon(color = "black") + scale_fill_gradient(low = "white", high = "red", guide = "legend", limits = c(0,10))


