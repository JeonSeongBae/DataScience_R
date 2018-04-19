# rm(list=ls()) # 워크 스페이스 클리어

# setwd(C:\Users\tjdqo_000\DataScience_R)

rawdata = read.csv("./lab02/toyota.csv") # 워킹 디렉토리 설정 및 데이터 로드

colnames(rawdata) = c("gender", "length", "diameter", "height", "whole_weight", "shell_weight", "ring") # Column 이름 부여

unique_gender = unique(rawdata$gender) # Categorical 변수 처리를 위한 unique elements 조사

gender_dummy = as.data.frame(matrix(0, nrow(rawdata), length(unique_gender)-1)) # 1-of-C coding을 위한 dummy variables 생성

for (i in 1:(length(unique_gender)-1)) {
  tmp = unique_gender[i]
  
  tmp_idx = which(rawdata$gender == tmp)
  
  gender_dummy[tmp_idx, i ] = 1
  
  colnames(gender_dummy)[i] = sprintf("gender_%s", tmp) # Dummy variables의 column 이름 부여
  
}

prdata = rawdata[, -1]
prdata = cbind(gender_dummy, prdata) # Categorical variable을 dummy variable로 교체

# Linear regression & stepwise linear regression 학습

#data partition
trn_ratio = 0.7
trn_idx = sample(1:nrow(prdata), round(trn_ratio*nrow(prdata))) # 데이터 partition
tst_idx = setdiff(1:nrow(prdata), trn_idx)

trn_data = prdata[trn_idx,]
tst_data = prdata[tst_idx,]

# Linear regression
fit_lr = lm(ring ~., data = trn_data) # Linear regression 학습
fit_lr
summary(fit_lr)
pred_lr = predict(fit_lr, tst_data) # Linear regression 테스트

plot(tst_data$ring, pred_lr)

mse_lr = mean((tst_data$ring-pred_lr)^2) # MSE 계산

# Stepwise

step_lr = step(fit_lr, direction = "both") # Stepwise linear regression 학습
summary(step_lr)

pred_step = predict(step_lr, tst_data) # Stepwise linear regrssion 테스트

mse_step = mean((tst_data$ring-pred_step)^2) # MSE 계산