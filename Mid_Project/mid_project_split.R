dataset <- read.csv("E:/Dataset_MIdterm_sectoin(D).csv",header = TRUE,sep = ",")
dataset[dataset == ""] <- NA #replace all "" with NA
install.packages("dplyr")
library(dplyr)

dim(dataset)      #dimension of dataset
names(dataset)     # variable name
View(dataset)      # view dataset

head(dataset)
tail(dataset)
str(dataset)
summary(dataset)
vars<-select(dataset,Age,Gender,BloodPressure,Cholesterol,Heart_Rate,QuantumPatternFeature,)
summary(vars)


is.na(dataset)
colSums(is.na(dataset))
missingVal<-colSums(is.na(dataset))
sum(is.na(dataset))
which(is.na(dataset$Age))
which(is.na(dataset$Gender))
which(is.na(dataset$BloodPressure))
which(is.na(dataset$Heart_Rate))
which(is.na(dataset$HeartDisease))
barplot(missingVal,main = "Missing Values per Column",
        col = "red", las = 2)

noMissing<-na.omit(dataset)
 #noMissing %>% summarise_if(is.numeric, sd)
colSums(is.na(noMissing))

meanData<-dataset
meanValAge<- round(mean(meanData$Age, na.rm = TRUE))
meanData$Age[is.na(meanData$Age)] <- meanValAge

medianData<-dataset
medianValAge<- round(median(medianData$Age, na.rm = TRUE))
medianData$Age[is.na(medianData$Age)] <- medianValAge


modeData<-dataset
modeValAge <- as.numeric(names(which.max(table(modeData$Age))))
modeData$Age[is.na(modeData$Age)] <- modeValAge

modeValGender <- as.numeric(names(which.max(table(dataset$Gender))))
modeData$Gender[is.na(modeData$Gender)] <- modeValGender

dataset<-modeData
dataset$BloodPressure <- as.numeric(gsub("[^0-9]", "", dataset$BloodPressure))
meanValBP <- round(mean(dataset$BloodPressure, na.rm = TRUE))
dataset$BloodPressure[is.na(dataset$BloodPressure)] <- meanValBP

modeValHR <-names(which.max(table(dataset$Heart_Rate)))
dataset$Heart_Rate[is.na(dataset$Heart_Rate)] <- modeValHR
colSums(is.na(dataset))


all_duplicates <- dataset[duplicated(dataset) | duplicated(dataset, fromLast = TRUE), ]
all_duplicates #shows duplicate rows

unique_data <- unique(dataset)     # removing duplicate
subset(unique_data, Age < 0 | Age > 120)
unique_data$Age<-abs(unique_data$Age) #absolute value of age remove negative value
unique_data<-subset(unique_data, Age >= 0 & Age <= 120)
str(unique_data)

# Step 2: Copy dataset
idata <- dataset

# Step 3: Check class distribution
class_dist <- table(idata$HeartDisease)
class_dist

# Step 4: Split into majority and minority
if (class_dist[1] > class_dist[2]) {
  majority <- filter(idata, HeartDisease == 0)
  minority <- filter(idata, HeartDisease == 1)
} else {
  majority <- filter(idata, HeartDisease == 1)
  minority <- filter(idata, HeartDisease == 0)
}

# ---------- Oversampling ----------
set.seed(123)
os_minority <- minority %>% sample_n(nrow(majority), replace = TRUE)
os_balanced_data <- bind_rows(majority, os_minority)
table(os_balanced_data$HeartDisease)

# ---------- Undersampling ----------
set.seed(123)
us_majority <- majority %>% sample_n(nrow(minority))
us_balanced_data <- bind_rows(minority, us_majority)
table(us_balanced_data$HeartDisease)

dataset<-os_balanced_data
# Set seed for reproducibility
set.seed(123)

# Total number of rows
n <- nrow(dataset)

# Randomly sample row indices for training
random_index <- sample(1:n, size = 0.8 * n)

# Split the dataset
train_data <- dataset[random_index, ]
test_data  <- dataset[-random_index, ]
