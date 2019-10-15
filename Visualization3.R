# Advanced Visualizations

# Load ggplot library
#install.packages("ggplot2")
library(ggplot2)
# Load our data
intl = read.csv("/Users/jaimegarcia/Desktop/DESKTOP_101/code/R_code/SMU/Visualization in R/intl.csv")
str(intl)
intl

# Let's make a bar plot with region on the x axis and Percentage on the y-axis.
ggplot(intl, aes(x=Region, y=PercentOfIntl)) + geom_bar(stat="identity") + geom_text(aes(label=PercentOfIntl))
# stat has 2 options, identity and count. Below is what the graph looks like with stat="count" - clearly not what we want.
# a bad start
ggplot(intl, aes(x=PercentOfIntl)) + geom_bar(stat="count") 
# the correct answer again
ggplot(intl, aes(x=Region, y=PercentOfIntl)) + geom_bar(stat="identity") + geom_text(aes(label=PercentOfIntl))

# We would like to order the bars from the tallest to the shortest for greater visibility
# Make Region an ordered factor with the re-order command and transform command. 
intl = transform(intl, Region = reorder(Region, -PercentOfIntl))
str(intl)
intl

# Make the percentages out of 100 instead of fractions
intl$PercentOfIntl = intl$PercentOfIntl * 100

# Make the plot
ggplot(intl, aes(x=Region, y=PercentOfIntl)) + geom_bar(stat="identity") + geom_text(aes(label=PercentOfIntl))
# Improve the plot 
ggplot(intl, aes(x=Region, y=PercentOfIntl)) + geom_bar(stat="identity", fill="dark blue") + 
  geom_text(aes(label=PercentOfIntl), vjust=-0.4) + ylab("Percent of International Students") + 
  theme(axis.title.x = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1))


# World map
# This part investigates the number of international students (UG and G) at a university 

# Load the ggmap package
#install.packages("ggmap")
library(ggmap)
intlall = read.csv("/Users/jaimegarcia/Desktop/DESKTOP_101/code/R_code/SMU/Visualization in R/intlall.csv")
str(intlall)
intlall = read.csv("/Users/jaimegarcia/Desktop/DESKTOP_101/code/R_code/SMU/Visualization in R/intlall.csv",stringsAsFactors=FALSE)
str(intlall)

# Lets look at the first few rows
head(intlall)

# Those NAs are really 0s, and we can replace them easily
intlall[is.na(intlall)] = 0

# Now lets look again
head(intlall) 

# Lets look for China
table(intlall$Citizenship) 

# Lets "fix" that in the intlall dataset
intlall$Citizenship[intlall$Citizenship=="China (People's Republic Of)"] = "China"


# Load the world map
map.world = map_data("world")
str(map.world)

# Lets merge intlall into world_map using the merge command
world_map = merge(map.world, intlall, by.x ="region", by.y = "Citizenship")
str(world_map)

###


gg <- ggplot()
gg <- gg + theme(legend.position="none")
gg <- gg + geom_map(data=world_map, map=map.world, aes(map_id=region, fill=Total)) + borders("world") 
gg <- gg + scale_fill_gradient(low = "white", high = "blue", guide = "colourbar")
gg <- gg + coord_equal() 
gg


# Line Charts
#install.packages("ggplot2")
library(ggplot2)
households = read.csv("/Users/jaimegarcia/Desktop/DESKTOP_101/code/R_code/SMU/Visualization in R/households.csv")
str(households)
head(households)

# Load reshape2
#install.packages("reshape2")
library(reshape2)

# Lets look at the first two columns of our households dataframe
households[,1:2]

# First few rows of our melted households dataframe
head(melt(households, id="Year"))

households[,1:3]

melt(households, id="Year")[1:10,3]
melt(households, id="Year")[1:10,]

# Plot it
ggplot(melt(households, id="Year"),       
       aes(x=Year, y=value, color=variable)) +
  geom_line(size=1) + geom_point(size=3) +  
  ylab("Percentage of Households")

