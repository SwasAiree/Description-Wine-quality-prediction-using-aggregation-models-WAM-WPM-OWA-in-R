##################################
#Question 1 - Understand the Data
##################################


# Importing data Redwine.txt from the working directory

the.data = as.matrix(read.table("RedWine.txt ")) 

# Creating a subset of 500 random data points from the.data 

set.seed(224389999)
my.data = the.data[sample(1:1599,500),c(1:6)]

#Assigning variable names to columns 

data.variable.names = c("citric acid", "chlorides", "total sulfur dioxide", "pH", "alcohol", "quality")
colnames(my.data) <-  data.variable.names

# Creating histogram of all 6 variables 

hist(my.data[,1], main=" Frequency Distribution of Citric Acid", xlab="Citric Acid", col="red")
hist(my.data[,2], main=" Frequency Distribution of Chlorides", xlab="Chlorides", col="blue")
hist(my.data[,3], main=" Frequency Distribution of Total Sulphur Dioxide", xlab="Total Sulphur Dioxide", col="green")
hist(my.data[,4], main=" Frequency Distribution of  pH", xlab="pH", col="yellow")
hist(my.data[,5], main=" Frequency Distribution of Alcohol", xlab="Alcohol", col="orange")
hist(my.data[,6], main=" Frequency Distribution of Quality", xlab="Quality", col="pink")

# Creating scatter plots for each X variable against the variable of interest Y(Quality)

plot(my.data[,1], my.data[,6], xlab="Citric Acid", ylab="Quality", col="red")
plot(my.data[,2], my.data[,6], xlab="Chlorides", ylab="Quality", col="blue")
plot(my.data[,3], my.data[,6], xlab="Total Sulphur Dioxide", ylab="Quality", col="green")
plot(my.data[,4], my.data[,6], xlab="pH", ylab="Quality", col="yellow")
plot(my.data[,5], my.data[,6], xlab="Alcohol", ylab="Quality", col="orange")


# Boxplot to identify outliers

boxplot(my.data[,1],main="Boxplot of citric acid data")
boxplot(my.data[,2],main="Boxplot of Chlorides data" )
boxplot(my.data[,3],main="Boxplot of Total Sulphur Dioxide data")
boxplot(my.data[,4],main="Boxplot of pH data" )
boxplot(my.data[,5],main="Boxplot of alcohol data" )
boxplot(my.data[,6],main="Boxplot of quality data" )

# Skewness of all variables to identify all the variables with skewness outside the range of 0.5 to -0.5

skewness(my.data[,1])
skewness(my.data[,2])
skewness(my.data[,3])
skewness(my.data[,4])
skewness(my.data[,5])
skewness(my.data[,6])

# Correlation of all variables to identify the direction of correlation of each x variable with Y variable (quality) 

cor(my.data[,1],my.data[,6])
cor(my.data[,2],my.data[,6])
cor(my.data[,3],my.data[,6])
cor(my.data[,4],my.data[,6])
cor(my.data[,5],my.data[,6])

################################
#Question 2 - Transform the Data
################################

# Creating copy to my.data to do transormation

I = c("citric acid", "total sulfur dioxide", "pH", "alcohol", "quality")
data.transform = my.data[,I]

# Summary of all variables (to check if data range is between 0-1)

summary(data.transform)

# Skewness of all variables to identify all the variables with skewness outside the range of 0.5 to -0.5

skewness(data.transform[,1])
skewness(data.transform[,2])
skewness(data.transform[,3])
skewness(data.transform[,4])
skewness(data.transform[,5])

# Correlation of all variables to identify the direction of correlation of each x variable with Y variable (quality) 

cor(data.transform[,1],data.transform[,5])
cor(data.transform[,2],data.transform[,5])
cor(data.transform[,3],data.transform[,5])
cor(data.transform[,4],data.transform[,5])

# Boxplot to identify outliers

boxplot(data.transform[,1],main="Boxplot of citric acid data")
boxplot(data.transform[,2],main="Boxplot of total sulfur dioxide data" )
boxplot(data.transform[,3],main="Boxplot of pH data")
boxplot(data.transform[,4],main="Boxplot of alcohol data" )
boxplot(data.transform[,5],main="Boxplot of quality data" )

#There are only 1 or 2 outliers in each column thus they do not need to be adressed as they will not have a significant effect on the properties of the data set 

# Transformation of column 1 (citric acid) not requried 
# as data is in range of 0-1 and skewness is between 0.5 to -0.5

hist(data.transform[,1],col="red")

# Transformation of column 2 (Total Sulphur Dioxide)

# Liner Feature Scaling 

data.transform[,2]= (data.transform[,2]-min(data.transform[,2]))/(max(data.transform[,2])-min(data.transform[,2]))


# Polynomial Transformation using p =0.3


data.transform[,2]= data.transform[,2]^(.3)
hist(data.transform[,2],col="green")
skewness(data.transform[,2])
cor(data.transform[,2],data.transform[,5])

# negation transformation


data.transform[,2] = max(data.transform[,2]) + min(data.transform[,2])- data.transform[,2]
hist(data.transform[,2],col="green")
skewness(data.transform[,2])
cor(data.transform[,2],data.transform[,5])

# Transformation of column 3 (pH)

# Liner Feature Scaling 

data.transform[,3]= (data.transform[,3]-min(data.transform[,3]))/(max(data.transform[,3])-min(data.transform[,3]))
hist(data.transform[,3],col="yellow")
skewness(data.transform[,3])
cor(data.transform[,3],data.transform[,5])


# negation transformation


data.transform[,3] = max(data.transform[,3]) + min(data.transform[,3])- data.transform[,3]
hist(data.transform[,3],col="yellow")
skewness(data.transform[,3])
cor(data.transform[,3],data.transform[,5])


# Transformation of column 4 (alcohol)

#Liner Feature Scaling 

data.transform[,4]= (data.transform[,4]-min(data.transform[,4]))/(max(data.transform[,4])-min(data.transform[,4]))
hist(data.transform[,4],col="orange")
skewness(data.transform[,4])
cor(data.transform[,4],data.transform[,5])

# Polynomial Transformation using p=0.5

data.transform[,4]= data.transform[,4]^(.5)
skewness(data.transform[,4])
cor(data.transform[,4],data.transform[,5])
hist(data.transform[,4],col="orange")

# Transformation of column 5 (Quality)

#Liner Feature Scaling 

data.transform[,5]= (data.transform[,5]-min(data.transform[,5]))/(max(data.transform[,5])-min(data.transform[,5]))
skewness(data.transform[,5])

hist(data.transform[,5],col="Pink")

write.table(data.transform, "Swastik.txt")

##########################################
#Question 3 - Build models and investigate
##########################################

library(lpSolve)
source("AggWaFit718.R")

# import your saved data
saved.data <- as.matrix(read.table("Swastik.txt"))


## Fitting model for weighted arithmetic mean

fit.QAM(saved.data[,c(1:4,5)]) #by default it will use AM
# It produces two files, namely "output1.txt" and "stats1.txt," in the current working directory.

##  Weighted power means with p=0.5, the outputs files are 2nd_output.txt and 2nd_stats.txt
fit.QAM(saved.data[,c(1:4,5)],output.1="2nd_output.txt",stats.1="2nd_stats.txt", g=PM05,g.inv = invPM05) # p = 0.5

## Weighted power means with p=2, the outputs files are 3rd_output.txt and 3rd_stats.txt
fit.QAM(saved.data[,c(1:4,5)],output.1="3rd_output.txt",stats.1="3rd_stats.txt",g=QM,g.inv = invQM) # p = 2

## OWA, the outputs files are 4th_output.txt and 4th_stats.txt 
fit.OWA(saved.data[,c(1:4,5)],"4th_output.txt","4th_stats.txt")

#stats1: weighted arithmetic mean will be the best fitting one because of low RMSE and Average Absolute Error and Pearson and Spearman correlation is closest to 1 and weights are distributed in all variables

#######################################
#Question 4 - Use Model for Prediction
#######################################

provided_input=c(0.9, 0.65, 38, 2.53, 7.1)

#taking all variables except chloride as previously done in task 2
Y = provided_input[c(1,3,4,5)]  

# Transforming the four variables in the same way as in Task-2
# Applying Min-Max Transformation On Y[1],Y[2],Y[3], Y[4]

Y[1]=(Y[1]-min(my.data[,1]))/(0.9-min(my.data[,1])) ##my.data data is the data before transformation.
Y[1]
Y[2]=(Y[2]-min(my.data[,3]))/(max(my.data[,3])-min(my.data[,3]))
Y[2]
Y[3]=(Y[3]-2.53)/(max(my.data[,4])-2.53)
Y[3]
Y[4]=(Y[4]-7.1)/(max(my.data[,5])-7.1)
Y[4]

# Applying polynomial transformations on Y[1],Y[2],Y[4]
Y[2]=Y[2]^(0.3)
Y[2]
Y[4]=Y[4]^(0.5)
Y[4]

## Transformed New input
transformed_Y <- Y
transformed_Y

##Utilizing the transformed input with the optimal model, which is the weighted arithmetic mean.
w <- c(0.00861502381904488,0.106953353466349,0.192342947335229,0.692088675379377)

## best fitting function

transformed_quality = QAM(transformed_Y,w,g=AM,g.inv=invAM)
transformed_quality
 
#y=0.07658686

##To revert the transformation and convert the predicted Y values back to their original scale.


y_predicted = (max(my.data[,6])-min(my.data[,6]))*transformed_quality + min(my.data[,6])
y_predicted

round(y_predicted)
