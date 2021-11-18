library("dplyr")
df=read.csv("parkinsons.csv")
df_scale = df

feature_list = c("Jitter...", "Jitter.Abs.", "Jitter.RAP", "Jitter.PPQ5","Jitter.DDP", "Shimmer","Shimmer.dB.","Shimmer.APQ3",  "Shimmer.APQ5",  "Shimmer.APQ11",
                 "Shimmer.DDA", "NHR", "HNR",  "RPDE", "DFA","PPE" )
# 2.1
# scale the columns in the feature_list
df_scale <- df %>% mutate_at(feature_list, ~(scale(.) %>% as.vector))
n=dim(df_scale)[1]
set.seed(12345)
id=sample(1:n, floor(n*0.6))
train=df_scale[id,]
test=df_scale[-id,]

# 2.2
fit1=lm(motor_UPDRS~ 0 + Jitter...+Jitter.Abs.+Jitter.RAP+Jitter.PPQ5+Jitter.DDP+Shimmer+Shimmer.dB.+Shimmer.APQ3+Shimmer.APQ5+Shimmer.APQ11+Shimmer.DDA+NHR+HNR+RPDE+DFA+PPE, data=train)
MSE_train = mean(fit1$residuals^2)   # 512.82
Test_fitted=predict(fit1, test)
MSE_test = mean((test$motor_UPDRS-Test_fitted)^2) #513.95
#summary(fit1)
# From the result of the summary of the training model, the significant variables are 
# DFA, NHR, HNR, and PPE. 

# 2.3
new_train  <- subset(train, select=c("motor_UPDRS", "Jitter...", "Jitter.Abs.", "Jitter.RAP", "Jitter.PPQ5","Jitter.DDP", "Shimmer","Shimmer.dB.","Shimmer.APQ3",  "Shimmer.APQ5",  "Shimmer.APQ11",
                                     "Shimmer.DDA", "NHR", "HNR",  "RPDE", "DFA","PPE"))
Y = as.matrix(new_train[,1])
X = as.matrix(new_train[,-1])
new_test  <- subset(test, select=c("motor_UPDRS", "Jitter...", "Jitter.Abs.", "Jitter.RAP", "Jitter.PPQ5","Jitter.DDP", "Shimmer","Shimmer.dB.","Shimmer.APQ3",  "Shimmer.APQ5",  "Shimmer.APQ11",
                                     "Shimmer.DDA", "NHR", "HNR",  "RPDE", "DFA","PPE"))
Y_test = as.matrix(new_test[,1])
X_test = as.matrix(new_test[,-1])
# a
loglikelihood <- function(Input_par, X,Y){
  # 3.20 in the book MLFC 
  theta = matrix(Input_par[-1],nrow = length(Input_par)-1, ncol = 1)
  sigma = Input_par[1]
  n = length(Y)
  - n/2*log(2*pi*sigma^2) - 1/2/(sigma^2)*sum((X%*%theta-Y)^2)
  
}

# b
Ridge <- function(Input_par, lamda, X,Y){
  theta = matrix(Input_par[-1],nrow = length(Input_par)-1, ncol = 1)
  -loglikelihood(Input_par, X,Y) + lamda*sum(theta^2)
}

# c
RidgeOpt <- function(lamda, X,Y){
  size_theta = dim(X)[2]
  optim(par = c(1, matrix(1,nrow = size_theta, ncol = 1)),fn=Ridge, lamda = lamda, X=X, Y=Y, method = "BFGS")
}

# d
DF <- function(lamda, X){
  P <- X %*% solve(t(X) %*% X + lamda* diag(ncol(X))) %*% t(X)
  df <- sum(diag(P))

}

# 2.4
Cal_MSE <- function(a,b){
  MSE = mean((a-b)^2)
  return(MSE)
}
MSE_train_vec = c()
MSE_test_vec = c()
DF_vec = c()
Theta_matrix = matrix(0,nrow = dim(X)[2], ncol = 3)

col_i = 1
for (lamda_i in c(1,100,1000)){
  result_RO = RidgeOpt(lamda_i,X,Y)
  theta = matrix(result_RO$par[-1],nrow = length(result_RO$par)-1, ncol = 1)
  Theta_matrix[,col_i] = theta
  Y_tilda_train = X %*% theta
  MSE_train_vec = c(MSE_train_vec, Cal_MSE(Y_tilda_train,Y))
  Y_tilda_test = X_test %*% theta
  MSE_test_vec = c(MSE_test_vec, Cal_MSE(Y_tilda_test, Y_test))
  DF_vec = c(DF_vec, DF(lamda_i,X))
  col_i = col_i+1
}


# MSE_train_vec [1] 515.2628 520.8695 521.2756
# MSE_test_vec [1] 512.3771 516.4353 516.9274
# lamda = 1 achieves the smallest MSE for both training and test data, so it is most appropriate.
# DF_vec [1] 13.862811  9.939085  5.643351
# In machine learning, degrees of freedom is the number of parameters of a model.
# The models with lamda equal to 1, 100 and 1000 corresponds to models with around 14, 10, and 6 effective parameters.
# They becomes less complexed with increacing lamda. As the dataset is from 42 people, 42 is larger than all three degree of freedoms. 
# In this way, it is fine to choose lamda = 1, which achieves lowest MSE for both training and test data. 


