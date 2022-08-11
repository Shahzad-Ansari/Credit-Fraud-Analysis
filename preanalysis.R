library(mlbench)
library(dplyr)
library(ibd)
library(tidyverse)
library(reshape2)
library(datasets)
library(devtools)
library(scatterplot3d)
library(rgl)
library(ggplot2)
library(ggbiplot)
library(caret)
library(MASS)
library(HSAUR2)
library(outliers)
library(data.table)
library(formattable)
library(gridExtra)



test <- read_csv('C:\\Users\\ShahzadAnsari\\Documents\\School\\DSA5103\\Project\\fraudTest.csv')

train <- data.frame(read.csv(file = 'C:\\Users\\ShahzadAnsari\\Documents\\School\\DSA5103\\Project\\fraudTrain.csv', stringsAsFactors = TRUE))

head(train)


#Creat a subset of train that only has numeric columns
factor_cols <- unlist(lapply(train, is.factor)) 
factorTrain <- train[ , factor_cols]

merchentFactors = count(subset(factorTrain,select = 'merchant'))
count(merchentFactors)
barplot(merchentFactors$freq)



percentagesDf = data.frame(colnames(factorTrain))
for(i in 1:ncol(factorTrain)) {       # for-loop over columns
  percentagesDf[i,2] = (round((sum(is.na(factorTrain[,i])))
                              /dim(factorTrain)[1],5))*100
}
names(percentagesDf)[1] = "Column_Name"
names(percentagesDf)[2] = "Missing_Percentage"
percentagesDf = as.data.frame(percentagesDf)

percentagesDf


gridExtra::grid.table(percentagesDf)



levelCount = data.frame(colnames(factorTrain))
for(i in 1:ncol(factorTrain)) {       # for-loop over columns
  levelCount[i,2] = nrow(count(factorTrain[,i]))
    
    #(round((sum(is.na(factorTrain[,i])))
                             # /dim(factorTrain)[1],5))*100
}

levelCount

names(levelCount)[1] = 'Column Name'

names(levelCount)[2] = 'Unique Levels'
levelCount

gridExtra::grid.table(levelCount)


genderFactors = count(subset(factorTrain,select = 'gender'))
count(genderFactors)
barplot(genderFactors$freq)


ggplot(data=genderFactors, aes(x=gender, y=freq)) +
  geom_bar(stat="identity",fill="steelblue")+
  geom_text(aes(label=freq),vjust=2,color="white",size=3.5)+
  theme_minimal()+
  scale_y_continuous(labels = comma)+
  ylab("Frequency")+
  xlab("Gender")+
  ggtitle("Frequency of purchases by males and females")


categoryFactors = count(subset(factorTrain,select = 'category'))
count(categoryFactors)


categoryByFraud = aggregate(is_fraud ~ category, train, sum)
categoryFactors$fraud =  categoryByFraud$is_fraud
categoryFactors



ggplot(data=categoryFactors, aes(factor,x=reorder(category, -freq), y=freq)) +
  geom_bar(stat="identity",fill="steelblue",aes(fill=categoryFactors$freq))+
  theme_minimal()+
  scale_y_continuous(labels = comma)+
  ylab("Frequency")+
  xlab("Categories")+
  ggtitle("Frequency of purchases by categories")+
  coord_flip()




ggplot(data=categoryFactors, aes(x=reorder(category, -freq))) +
  geom_bar(aes(y=freq), stat="identity", position ="identity", alpha=.3, fill='lightblue', color='lightblue4') +
  geom_bar(aes(y=fraud), stat="identity", position="identity", alpha=.8, fill='pink', color='red')+
  theme_minimal()+
  scale_y_continuous(labels = comma)+
  ylab("Frequency")+
  xlab("Categories")+
  ggtitle("Frequency of purchases by categories")+
  coord_flip()

categoryFactors$fraudRatio = categoryFactors$fraud/categoryFactors$freq

categoryFactors = categoryFactors[order(categoryFactors$fraudRatio),]

categoryFactors

gridExtra::grid.table(categoryFactors)




