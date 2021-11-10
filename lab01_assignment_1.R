#' lab 1 
#' Assignment 1
#' Muhammad Uzair 
#' liu-id muhuz668
library(kknn)

optdigits <- read.csv("~/R/Machine Learning/lab01/data/optdigits.csv", header=FALSE)

# split data into training, validation and testing (50%/25%/25%)  
# taking independent sample 

n <- nrow(optdigits)
# n
# d <- dim(optdigits)
summary(optdigits$V65)

set.seed(1234)
ind_1 <- sample(1:n, floor(n*0.5))
length(ind_1)
# typeof(ind)
# length(ind)
# training variable

optd_training_data = optdigits[ind_1,]
#the test function return the data set that do not exist in the second argument
diff_set_not_exist_in_training_sample_ind_1 <- setdiff(1:n, ind_1) # validation data set lets say

# now separate validation set

ind_2 <- sample(diff_set_not_exist_in_training_sample_ind_1, floor(n*0.25))
validation_data_set <- optdigits[ind_2,]


 #now separate test data set
diff_set_not_exist_in_training_sample_ind_2 <- setdiff(ind_1, ind_2)
test_data <- optdigits[diff_set_not_exist_in_training_sample_ind_2,]

kknn_matrix <- kknn(as.factor(V65)~., train = optd_training_data , test = test_data, k=30, kernel = "rectangular")

kknn_matrix_pred = predict(kknn_classifier_matrix)

kknn_matrix_test <- kknn(as.factor(V65)~., train = optd_training_data , test = test_data, k = 30, distance = 2, kernel = "rectangular")
knn_matrix_test_pred <- predict(kknn_matrix_test)
kknn_matrix_test

#confusion matrix
table(kknn_matrix_pred, optd_training_data$V65)

#consufion matrix
table(knn_matrix_test_pred,test_data$V65)
































