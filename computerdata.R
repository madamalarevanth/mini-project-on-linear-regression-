#1 linear regression model
#predicting the selling prices in the real estate industry 

#import the computer data from the .csv file 
data<-read.csv("C:/Users/REVANTH/Desktop/data/Computer_Data.csv")

#to view the data 
View(data)

#as the first columns of the data arent important for the prediction we drop those columns from the data frame 
#to drop the 1st variable from the data set
data<-data[-c(1)]

#data exploration

#gives the names of the variables in the data set
colnames(data)

#displays top 10 observations of the data set
head(data)

#displays bottom  10 observations of the data set
tail(data)

#displays structure of the variables in the data set
str(data)

#gives the summary of the variables of the data set 
summary(data)

#gives the dimensions of the data set
dim(data)


#library for the sample.split function
library(caTools)

#splitting the data into training and testing data 
# sample the input data with 70% for training and 30% for testing
sample<-sample.split(data$price,SplitRatio = 0.7) 

#this sample contains true and false values of data depending on the splitting ratio
sample

#assign the splitted data using subset command
training_data<-subset(data,split=="TRUE")
testing_data<-subset(data,split=="FALSE")

#multiple linear regression model 
model<-lm(price~.,data = training_data)

summary(model)

prediction<-predict(model,testing_data)
prediction

predtest1<-data.frame(prediction)

#bind it to test data
final_data <-cbind(test_data,predtest1)
final_data

write.csv(final_data,"linear_output.csv")

plot(testing_data$price,type = "l",lty = 1.8,col = "green")
lines(prediction,type = "l",col = "blue")
