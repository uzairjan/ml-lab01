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


