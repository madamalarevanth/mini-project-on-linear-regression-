#predicting the selling prices in the real estate industry 

#import the real estate industry data from the .csv file 
data1 <-read.csv("C:/Users/REVANTH/Desktop/data/regression data.csv",header= TRUE,sep=",")

#to view the data 
View(data1)

#as the first columns of the data arent important for the prediction we drop those columns from the data frame 
#to drop the 1st variable from the data set
data1 <- data1[-c(1)]

#data exploration

#gives the dimensions of the data set
dim(data1)

#gives the names of the variables in the data set
names(data1)

#displays top 10 observations of the data set
head(data1)

#displays bottom top 10 observations of the data set
tail(data1)

#displays structure of the variables in the data set
str(data1)

#gives the summary of the variables of the data set 
summary(data1)


#library for the sample.split function
library(caTools)

#splitting the data into training and testing data 
# sample the input data with 70% for training and 30% for testing
sample <- sample.split(data1$selling.price,SplitRatio=0.70)

#this sample contains true and false values of data depending on the splitting ratio
sample

#assign the splitted data using subset command
train_data <- subset(data1,sample==TRUE)#split of the data using subset command 
View(train_data)

test_data <- subset(data1,sample==FALSE)
View(test_data)

#MLR EQUATION : 
  
#Y = B0+B1X1+B2X2+B3X3+........BNXN +ERROR TERM 

#Y - DEP VAR ( SELING PRICE ) 

#B0 - INTERCEPT OR CONSTANT 
#B1 - SLOPE FOR X1 
#X1 - IST INDEP VAR ( local.selling.price.in.hundred.of.dollars ) 
#B2 - SLOPE FOR X2 
#X2 - 2ND INDEP VAR ( number.of.bathrooms  ) 
#B3 - SLOPE FOR X3 
#X3 - 3RD INDEP VAR ( area.of.the.site  ) 

#multiple linear regression model 
model <- lm(selling.price ~ ., data = train_data)

summary(model)

#pvalues is pr 
#size of living space has p value less than 0.05 so it is sig var
#slope is the estimate value and look it for sig var
#slope is +vely correlated 
#rsq value
#SELLING PRICE OF THE HOUSE IS 93 % EXPLAINED BY  INDEPENDENT SIGNIFICANT VARIABLES .

#fitted values
pred<- model$fitted.values
head(pred)

pred1 <- data.frame(pred)
pred1

#residual values
resed <- model$residuals
head(resed)
resed<-data.frame(resed)

#predict on unforseen dataset that is on testing dataset
predtest<-predict(model,newdata = test_data)
head(predtest)

#attach it to data frame
predtest1<-data.frame(predtest)

#bind it to test data
final_data <-cbind(test_data,predtest1)
final_data

write.csv(final_data,"linear_output.csv")

 