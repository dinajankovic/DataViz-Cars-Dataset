## DATA VISUALIZATION IN R AND GGOBI ##

#First we load the data set, which is in .csv format in our working directory
cars = read.csv("cars.csv",header=TRUE)
head(cars)

#Is "cars" a data frame? Yes it is!
is.data.frame(cars)
#The attach command allows us to use the column names without refering to "cars" with
#the $ sign (if we wish to).
attach(cars)

#Let's see a brief summary of our data set. The last column (Origin) was dropped.
summary(cars[,1:6])


#DATA VISUALIZATION IN R

#Let's start off with some histograms. First we install a package that allows us
#to access some nice colours.
install.packages("RColorBrewer")
library(RColorBrewer)

#The idea is to use the region names instead of numbers 1, 2, 3 that don't tell us anything.
#1 = "American"
#2 = "European"
#3 = "Japanese"
Origin2 = factor(Origin, labels=c("American","European","Japanese"))


par(mfrow=c(2,2))   #we want 4 plots in 1 picture
hist(MPG, breaks = 10, main = "Miles per Gallon", col = brewer.pal(9,"Greys"))
hist(Horsepower, breaks = 10, main = "Horsepower", col = brewer.pal(9,"Blues"))
hist(Acceleration, breaks = 7, xlab = "acceleration in seconds", main = "Time to accelerate from 0 to 60 mph", col = "Yellow")
hist(Weight, breaks = 100, main = "Vehicle Weight in lbs.", col = "Purple")
graphics.off()

#Now let's create some boxplots.
par(mfrow=c(2,2))
boxplot(MPG~Origin2, col = "green", main = "Miles per Gallon")
boxplot(Year~Origin2, col = heat.colors(3), main = "Year")
boxplot(Acceleration~Origin2, col = topo.colors(3), main = "Time to accelerate from 0 to 60 mph")
boxplot(Weight~Origin2, col = "purple", main = "Vehicle Weight in lbs.")
#click zoom to see the boxplots clearly
graphics.off()

#Let's also create a stacked bar plot using ggplot.
install.packages("ggplot2")
library(ggplot2)

#cars2 is a copy of cars with slighly modified features that we need for the ggplot
#(some of the variables need to be factored, such as Cylinders and Year)
cars2 = cars
cars2$Cylinders = factor(cars2$Cylinders)
cars2$Origin = Origin2
cars2$Year = factor(cars2$Year)

detach(cars)
attach(cars2)

ggplot(data = cars2, aes(x = Year, fill = Cylinders)) +
  geom_bar() +
  facet_wrap(~Origin, ncol = 1) +
  xlab("Year") +
  ylab("Frequency") +
  ggtitle("Cars")


detach(cars2)
attach(cars)
#the correlation matrix, with correlation coefficents rounded up to 2 decimals
#note that column Origin was omitted, since it's categorical
round(cor(cars[,1:6]), digits=2)

#scatterplot
pairs(cars2, cex.labels = 2)


#these two scatterplots are fancier than the previous one
pairs(cars2, upper.panel = panel.cor, diag.panel = panel.hist, cex.labels = 2)
pairs(cars2, col = Origin + 1, cex.labels = 2)

#creating a correlogram using the "corrgram" package
install.packages("corrgram")
library(corrgram)

corrgram(cars[,1:6],
         main="Correlogram",
         lower.panel=panel.shade, upper.panel=panel.pie,
         diag.panel=panel.minmax, cex.labels = 2)

#conditional plots
coplot(Weight ~ Horsepower | Cylinders, data = cars, overlap = 0.1, col = Origin + 1, pch = 16)
coplot(Weight ~ Horsepower | Acceleration, data = cars, overlap = 0.1, col = Origin + 1, pch = 18)
coplot(Weight ~ Horsepower | Year, data = cars, overlap = 0.1, col = Origin + 1, pch = 15)

library(lattice)

#another conditional plot
cond1 = equal.space(Horsepower, 6)
xyplot(MPG ~ Weight | cond1, data = cars2, pch = 19)


install.packages("rggobi")
library(rggobi)
g = ggobi(cars2)

display(g[1],"Scatterplot Matrix")

display(g[1],"Parallel Coordinates Display")
#The rest was done using Ggobi directly
