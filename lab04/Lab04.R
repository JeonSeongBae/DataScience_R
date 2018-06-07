## 워크스페이스 클리어
rm(list=ls())

## 워킹 디렉토리 설정(optional)
setwd('C:/Users/tjdqo_000/DataScience_R')

## 데이터 테이블 가져오기
rawdata = read.table('./lab04/Dataset.data')

## 칼럼 이름 부여
colnames(rawdata) = c("gender", "length", "diameter", "height", "whole_weight", "shucked_weight", "viscera_weight", "shell_weight", "ring")

#### Target인 Gender가 M, F, I Factor로 되어있어서 그냥 사용할수도 있으나
#### 숫자로 변환해놓고 쓰는게 범용적으로 편해서 숫자로 변환

unique_gender = unique(rawdata$gender)

## 변환된 숫자 target을 저장할 matrix 생성
new_gender = matrix(0, nrow(rawdata), 1)

## gender unique를 하나씩 보며 숫자로 변환
for(i in 1:length(unique_gender)){
  tmp_idx = which(rawdata$gender == unique_gender[i])
  new_gender[tmp_idx] = i
}

## M, F, I 로 되어있던 gender 변수 삭제
rawdata = rawdata[, -1]

#### Data preprocessing

## 각 변수별 Min-Max Scaling
## Target 변수는 scling 대상이 아니니 rawdata 테이블에서만 해줍니다.

for(i in 1:ncol(rawdata)){
  rawdata[,i] = (rawdata[,i] - min(rawdata[,i])) / (max(rawdata[,i] - min(rawdata[,i])))
}

## Scale된 raw data와 숫자로 변환환 target 변수 합체
prdata = cbind(rawdata, new_gender)

## 기본적인 binary classification을 하기 위해서
## 원래 target은 3개 였으나(M, F, I)
## 임의로 2개의 class만 선정(1번 3번 선택)하여 prdata로 가져옴
tmp_idx = union(which(prdata$new_gender == 1), which(prdata$new_gender == 3))
prdata = prdata[tmp_idx,]
prdata$new_gender[which(prdata$new_gender == 3)] = 0 # 편의성을 위해 3번 클래스를 0번 클래스로 변환
prdata$new_gender = as.factor(prdata$new_gender) # 역시 편의성을 위해 target 변수를 factor 형으로 지정

## data partition
trn_ratio = 0.7
trn_idx = sample(1:nrow(prdata), round(trn_ratio*nrow(prdata)))
tst_idx = setdiff(1:nrow(prdata), trn_idx)

trn_data = prdata[trn_idx,]
tst_data = prdata[tst_idx,]

## K-NN
library(class)
out_knn = knn(trn_data[,1:(ncol(trn_data)-1)], tst_data[,1:(ncol(tst_data)-1)], trn_data[,ncol(trn_data)], k=5, prob=TRUE)

model_lr = glm(new_gender ~., data = trn_data, family = "binomial")
out_lr = predict(model_lr, tst_data)

library(rpart)
model_tree = rpart(new_gender ~., data = trn_data, control = rpart.control(minsplit = 10))
out_tree = predict(model_tree, tst_data)

plot(model_tree)
text(model_tree, use.n = TRUE)

library(nnet)

model_nn = nnet(new_gender ~., data = trn_data, size=20, linout=FALSE, maxit=300)
out_nn = predict(model_nn, tst_data, type="class")


library(devtools)
source_url('https://gist.githubusercontent.com/fawda123/7471137/raw/466c1474d0a505ff044412703516c34f1a4684a5/nnet_plot_update.r')
library(reshape2)
plot.nnet(model_nn)

library(e1071)
model_svm = svm(new_gender ~ ., data = trn_data, type="C-classification", kernel="radial", gamma=10, cost=100)
out_svm = predict(model_svm, tst_data)

target = tst_data[,ncol(tst_data)]
outs = cbind(target, out_knn)

tmp_idx1 = which(out_lr >= 0)
tmp_idx2 = which(out_lr < 0)
out_lr2 = out_lr
out_lr2[tmp_idx1] = 1
out_lr2[tmp_idx2] = 0
outs = cbind(outs, out_lr2)

out_tree2 = out_tree[,2]
tmp_idx1 = which(out_tree2 >= 0.5)
tmp_idx2 = which(out_tree2 < 0.5)
out_tree2[tmp_idx1] = 1
out_tree2[tmp_idx2] = 0
outs = cbind(outs, out_tree2)

outs = cbind(outs, out_nn, out_svm)
library(caret)

for(i in 1:ncol(outs)){
  if(length(which(outs[,i] == 2)) == 0){
    next
  }
  
  tmp_idx1 = which(outs[,i] == 2)
  tmp_idx2 = which(outs[,i] == 1)
  
  outs[tmp_idx1,i] = 1
  outs[tmp_idx2,i] = 0
}

predicted <- outs[,2]
actual <- outs[,1]

confusionMatrix(factor(outs[,2]), factor(outs[,1]))
confusionMatrix(factor(outs[,3]), factor(outs[,1]))
confusionMatrix(factor(outs[,4]), factor(outs[,1]))
confusionMatrix(factor(outs[,5]), factor(outs[,1]))
confusionMatrix(factor(outs[,6]), factor(outs[,1]))


library(pROC)
plot(roc(tst_data$new_gender,out_lr,direction="<"),col="red",lwd=3,main="ROC")