#mydata <- read.csv("E:/data.csv", header = TRUE,sep = ",")

irisData <- read.csv("E:/Iris.csv", header = TRUE, sep = ",")
mydata <- irisData
mydata[5:10,] #[row:range, col:range]
mydata$PetalWidthCm #specific column
subset(mydata,Species=="Iris-setosa") #Condition wise 
subset(mydata, PetalLengthCm>=1.6 & Species=="Iris-setosa" ) #multiple condition

install.packages("dplyr")
library(dplyr)

distinct(mydata)  #remove duplicate
distinct(mydata,Species, .keep_all = TRUE)   #show distinct value
select(mydata, Species) #select specific column

#see mutate() and TransMutate
vars<-select(mydata, Id, SepalLengthCm,SepalWidthCm, PetalLengthCm, PetalWidthCm) #select specific column
summary(vars)

sd(mydata$SepalWidthCm,mydata$SepalLengthCm)
