#Bill Stanton
library(class)

#load in data from device
heart_disease_data <- read.csv("C:\Users\STANTONWJ23\OneDrive - Grove City College\Desktop\Personal Projects\kNN-Cancer-Cell-Detector\processed_cleveland.csv", na.strings = "?")

#add binary Diagnosis column
heart_disease_data$Diagnosis <- ifelse(heart_disease_data$num == 0, 0, 1)

#remove original 'num' column
heart_disease_data$num <- NULL

#fill in missing data
heart_disease_data[] <- lapply(heart_disease_data, function(x) {
  if (is.numeric(x)) {
    x[is.na(x)] <- mean(x, na.rm = TRUE)
  }
  return(x)
})

#normalize data
heart_disease_norm <- heart_disease_data
heart_disease_norm[, 1:12] <- scale(heart_disease_data[, 1:12])

#split data into 70% train and 30% test for model
set.seed(1234)
sample_indices <- sample(nrow(heart_disease_norm), 0.7 * nrow(heart_disease_norm)) # random sample of ints from 0 to nrows
train_data <- heart_disease_norm[sample_indices, ] #rows with those random integers (creates random sample)
test_data <- heart_disease_norm[-sample_indices, ] #rows with the ints not selected in the random sample (other 30%)

#create 100 knn models (k = 1 to 100) to see which one is most accurate
knn_test <- list()
results <- numeric() #used to store accuracy for each k value

for (k in 1:100) {
  knn_test[[k]] <- knn(train_data[, 1:12], test_data[, 1:12], train_data$Diagnosis, k, prob=TRUE) #performs the knn test
  results[k] <- sum(knn_test[[k]] == test_data$Diagnosis) / length(test_data$Diagnosis) * 100 #calculates the accuracy of the knn test
}

#find which k value led to the most accurate test
best_k <- which.max(results)
best_k

#confusion matrix for k=6 (best k)
conf_matrix <- table(test_data$Diagnosis, knn_test[[best_k]])
print(conf_matrix) 

test <- table(test_data$Diagnosis, test_data$Diagnosis)
print(test) #3 false positives, 11 false negatives

test_accuracy <- max(results)
test_accuracy

#knn test with k=6 was 84.6% accurate
