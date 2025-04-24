install.packages("dplyr")
library(dplyr)

dataset <- read.csv("E:/Dataset_MIdterm_sectoin(D).csv",header = TRUE,sep = ",")
dataset
dim(dataset)
names(dataset)
View(dataset)

head(dataset)
tail(dataset)
str(dataset)

summary(dataset)

unique_dataset <- unique(dataset)
print(dim(unique_dataset))

unique_dataset[unique_dataset == ""] <- NA 

is.na(unique_dataset)
colSums(is.na(unique_dataset))
sum(is.na(unique_dataset))

missingVal<-colSums(is.na(unique_dataset))
barplot(missingVal,main = "Missing Values per Column", col = "red", las = 1, cex.names = 0.5)


which(is.na(unique_dataset$Age))
which(is.na(unique_dataset$Gender))
which(is.na(unique_dataset$BloodPressure))
which(is.na(unique_dataset$Heart_Rate))
which(is.na(unique_dataset$HeartDisease))


unique_dataset_noMissing<-na.omit(unique_dataset)
dim(unique_dataset_noMissing)
colSums(is.na(unique_dataset_noMissing))

missing_val_data<-unique_dataset
colSums(is.na(missing_val_data))
mean_Val_Age<- round(mean(missing_val_data$Age, na.rm = TRUE))
print(mean_Val_Age)
missing_val_data$Age[is.na(missing_val_data$Age)] <- mean_Val_Age

missing_val_data<-unique_dataset
medianValAge<- round(median(missing_val_data$Age, na.rm = TRUE))
print(medianValAge)
missing_val_data$Age[is.na(missing_val_data$Age)] <- medianValAge

missing_val_data<-unique_dataset
modeValAge <- as.numeric(names(which.max(table(missing_val_data$Age))))
print(modeValAge)
missing_val_data$Age[is.na(missing_val_data$Age)] <- modeValAge

mode_Val_Gender <- as.numeric(names(which.max(table(missing_val_data$Gender))))
missing_val_data$Gender[is.na(missing_val_data$Gender)] <- mode_Val_Gender
mode_Val_Gender

invalid_bp <- grep("[^0-9]", missing_val_data$BloodPressure, value = TRUE)
invalid_rows <- which(missing_val_data$BloodPressure %in% invalid_bp)
print(paste("Found", length(invalid_rows), "invalid BloodPressure values:"))
print(invalid_bp)
invalid_data_remove <- missing_val_data[-invalid_rows, ]

missing_val_data$BloodPressure <- as.numeric(missing_val_data$BloodPressure)
mean_Val_BP <- round(mean(missing_val_data$BloodPressure, na.rm = TRUE))
missing_val_data$BloodPressure[is.na(missing_val_data$BloodPressure)] <- mean_Val_BP

missing_val_data$Heart_Rate <- factor(missing_val_data$Heart_Rate, 
                      levels = c("Low", "High"), labels = c(0, 1))
str(missing_val_data$Heart_Rate)
summary(missing_val_data$Heart_Rate)

mode_Val_HR <-names(which.max(table(missing_val_data$Heart_Rate)))
missing_val_data$Heart_Rate[is.na(missing_val_data$Heart_Rate)] <- mode_Val_HR

missing_val_data$Gender <- factor(missing_val_data$Gender, 
                          levels = c(0, 1), labels = c("Female", "Male"))
summary(missing_val_data$Gender)

colSums(is.na(missing_val_data))
which(is.na(missing_val_data$Heart_Rate))

missing_val_data %>% summarise_if(is.numeric, sd)
noMissing <- missing_val_data

Q1 <- quantile(noMissing$Age , 0.25, na.rm = TRUE)
Q3 <- quantile(noMissing$Age, 0.75, na.rm = TRUE)
IQR_value <- Q3 - Q1
lower_bound <- Q1 - 1.5 * IQR_value
upper_bound <- Q3 + 1.5 * IQR_value

outlier_indices <- which(noMissing$Age < lower_bound | noMissing$Age > upper_bound)
print(outlier_indices)
data_deleting_outlier <- noMissing[-outlier_indices, ]

median_value <- median(noMissing$Age, na.rm = TRUE)
noMissing$Age[outlier_indices] <- median_value
cleaned_data_without_outlier <- noMissing

normal_bp <- subset(cleaned_data_without_outlier, 
                    BloodPressure >= 80 & BloodPressure <=120)
Young_heart_patients <- subset(cleaned_data_without_outlier, 
                               Age >= 30 & Age <= 35 & HeartDisease == 1)


normalize_dataset <-cleaned_data_without_outlier
normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}

normalize_dataset$Age <- normalize(normalize_dataset$Age)
normalize_dataset$BloodPressure <- normalize(normalize_dataset$BloodPressure)
normalize_dataset$Cholesterol <- normalize(normalize_dataset$Cholesterol)
normalize_dataset$QuantumPatternFeature <- normalize(normalize_dataset$QuantumPatternFeature)
normalize_dataset

idata <- normalize_dataset
class_dist <- table(idata$HeartDisease)
class_dist
barplot(table(idata$HeartDisease), main = "Original Class Distribution", col = "skyblue")

if (class_dist[1] > class_dist[2]) {
  majority <- filter(idata, HeartDisease == 0)
  minority <- filter(idata, HeartDisease == 1)
} else {
  majority <- filter(idata, HeartDisease == 1)
  minority <- filter(idata, HeartDisease == 0)
}

set.seed(123)
os_minority <- minority %>% sample_n(nrow(majority), replace = TRUE)
os_balanced_data <- bind_rows(majority, os_minority)
table(os_balanced_data$HeartDisease)

us_majority <- majority %>% sample_n(nrow(minority))
us_balanced_data <- bind_rows(minority, us_majority)
table(us_balanced_data$HeartDisease)

barplot(table(os_balanced_data$HeartDisease), main = "Oversampled Distribution", col = "lightgreen")
barplot(table(us_balanced_data$HeartDisease), main = "Undersampled Distribution", col = "orange")

final_data <- os_balanced_data 

n <- nrow(final_data)
random_index <- sample(1:n, size = 0.8 * n)
train_data <- final_data[random_index, ]
test_data  <- final_data[-random_index, ]
dim(final_data)
dim(train_data)
dim(test_data)

final_data<-cleaned_data_without_outlier
age_by_gender <- final_data %>%
  group_by(Gender) %>%
  summarise(
    Mean_Age = mean(Age, na.rm = TRUE),
    Median_Age = median(Age, na.rm = TRUE),
    Mode_Age = as.numeric(names(which.max(table(Age))))
  )

age_by_gender

age_by_hr <- final_data %>%
  group_by(Heart_Rate) %>%
  summarise(
    Mean_Age = mean(Age, na.rm = TRUE),
    Median_Age = median(Age, na.rm = TRUE),
    Mode_Age = as.numeric(names(which.max(table(final_data$Age))))
  )
age_by_hr

age_spread <- final_data %>%
  group_by(Gender) %>%
  summarise(
    Range_Age = max(Age, na.rm = TRUE) - min(Age, na.rm = TRUE),
    IQR_Age = IQR(Age, na.rm = TRUE),
    Variance_Age = var(Age, na.rm = TRUE),
    SD_Age = sd(Age, na.rm = TRUE)
  )
age_spread
