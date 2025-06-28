library(dplyr)
dataset <- read.csv("E:/weather_classification_data.csv",header = TRUE,sep = ",")
summary(dataset)
str(dataset)
colSums(is.na(dataset))
dataset<-na.omit(dataset)

cor(dataset$Temperature , dataset$Humidity, method = "pearson")
cor(dataset$Humidity , dataset$Precipitation...., method = "pearson")

cor(dataset$Wind.Speed , dataset$Atmospheric.Pressure, method = "spearman")

dataset$Cloud.Cover.Num <- as.numeric(factor(dataset$Cloud.Cover,
                                             levels = c("clear", "partly cloudy", "cloudy", "overcast"),
                                             ordered = TRUE))
cor(dataset$Cloud.Cover.Num, dataset$Temperature, method = "kendall")
#another approch add .test after each cor, i.e.
#cor.test(dataset$Cloud.Cover.Num, dataset$Temperature, method = "kendall")
table_data <- table(dataset$Cloud.Cover, dataset$Weather.Type)
chisq.test(table_data)

table_data <- table(dataset$Season, dataset$Weather.Type)
chisq.test(table_data)
