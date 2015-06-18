# library
library(randomForest)
library(rpart)
library(nnet)
library(caret)
library(gbm)
library(earth)
library(AUC)
library(adabag)
library(e1071)
library(mlbench)
library(AUCRF)

# load data
#bids <- read.csv("../input/bids.csv")
#save(bids,file="../input/bids.RData")
#load("../input/bids.RData")
#train <- read.csv("../input/train.csv")
#test <- read.csv("../input/test.csv")
#train <- read.csv("../input/train_full.csv")
#test <- read.csv("../input/test_full.csv")
train <- read.csv("../input/train_withAuctionCols.csv")
test <- read.csv("../input/test_withAuctionCols.csv")

# load submission result file
sub.result <- read.csv("../result.csv")

# ===================
# preprocessing - only run once
# for each bidder in train/test, match time of bidding in bids
# get necessary data so we can drop bids table
# save new data
# ===================

bidTrainIndex <- match(bids$bidder_id,train$bidder_id)
bidTrain <- bids[!is.na(bidTrainIndex),]
bidTestIndex <- match(bids$bidder_id,test$bidder_id)
bidTest <- bids[!is.na(bidTestIndex),]
bidTrain$bidder_id <- sapply(bidTrain$bidder_id,as.character)
bidTest$bidder_id <- sapply(bidTest$bidder_id,as.character)
train$bidder_id <- sapply(train$bidder_id,as.character)
test$bidder_id <- sapply(test$bidder_id,as.character)
#save(bidTrain,file="bidTrain.RData")
#save(bidTest,file="bidTest.RData")

train$numBids <- NA
train$numAuction <- NA
train$numMerchandise <- NA
train$numDevice <- NA
train$numCountry <- NA
train$numIp <- NA
train$numUrl <- NA
train$avgTimeDiff <- NA
train$minTimeDiff <- NA

test$numBids <- NA
test$numAuction <- NA
test$numMerchandise <- NA
test$numDevice <- NA
test$numCountry <- NA
test$numIp <- NA
test$numUrl <- NA
test$avgTimeDiff <- NA
test$minTimeDiff <- NA

country_levels <- levels(bids$country)
for (c in country_levels){
  train[paste("country",c,sep="_")] <- 0
  test[paste("country",c,sep="_")] <- 0
}
merchandise_levels <- levels(bids$merchandise)
for (c in merchandise_levels){
  train[paste("merchandise",c,sep="_")] <- 0
  test[paste("merchandise",c,sep="_")] <- 0
}
auction_levels <- levels(bids$auction)
for (c in auction_levels){
  train[paste("auction",c,sep="_")] <- 0
  test[paste("auction",c,sep="_")] <- 0
}
  
for (i in 1:nrow(train)){
  print(i)
  temp <- bidTrain[bidTrain$bidder_id == train$bidder_id[i],]
  train$numBids[i] <- nrow(temp)
  train$numAuction[i] <- length(table(strsplit(toString(temp$auction),", ")))
  train$numMerchandise[i] <- length(table(strsplit(toString(temp$merchandise),", ")))
  train$numDevice[i] <- length(table(strsplit(toString(temp$device),", ")))
  train$numCountry[i] <- length(table(strsplit(toString(temp$country),", ")))
  train$numIp[i] <- length(table(strsplit(toString(temp$ip),", ")))
  train$numUrl[i] <- length(table(strsplit(toString(temp$url),", ")))
  tempTime <- sort(temp$time)
  if (length(tempTime) == 1){
    train$avgTimeDiff[i] <- tempTime
  }
  else{
    train$avgTimeDiff[i] <- mean(tempTime[2:length(tempTime)] - tempTime[1:length(tempTime)-1])
  }
  if (length(tempTime) == 1){
    train$minTimeDiff[i] <- tempTime
  }
  else{
    timeDiff <- tempTime[2:length(tempTime)] - tempTime[1:length(tempTime)-1]
    train$minTimeDiff[i] <- min(timeDiff)
  }
  if (nrow(temp) != 0){
    country_table <- table(sapply(temp$country,as.character))
    for (level in names(country_table)){
      train[paste("country",level,sep="_")][i,] <- country_table[which(names(country_table)==level)]
    } 
    merchandise_table <- table(sapply(temp$merchandise,as.character))
    for (level in names(merchandise_table)){
      train[paste("merchandise",level,sep="_")][i,] <- merchandise_table[which(names(merchandise_table)==level)]
    }
    auction_table <- table(sapply(temp$auction,as.character))
    for (level in names(auction_table)){
      train[paste("auction",level,sep="_")][i,] <- auction_table[which(names(auction_table)==level)]
    }
  }
}
train$avgTimeDiff[is.na(train$avgTimeDiff)] <- 9999999999999999   # no bid
train$minTimeDiff[train$minTimeDiff == Inf] <- 9999999999999999   # no bid
train[is.na(train)] <- 0

for (i in 1:nrow(test)){
  print(i)
  temp <- bidTest[bidTest$bidder_id == test$bidder_id[i],]
  test$numBids[i] <- nrow(temp)
  test$numAuction[i] <- length(table(strsplit(toString(temp$auction),", ")))
  test$numMerchandise[i] <- length(table(strsplit(toString(temp$merchandise),", ")))
  test$numDevice[i] <- length(table(strsplit(toString(temp$device),", ")))
  test$numCountry[i] <- length(table(strsplit(toString(temp$country),", ")))
  test$numIp[i] <- length(table(strsplit(toString(temp$ip),", ")))
  test$numUrl[i] <- length(table(strsplit(toString(temp$url),", ")))
  tempTime <- sort(temp$time)
  if (length(tempTime) == 1){
    test$avgTimeDiff[i] <- tempTime
  }
  else{
    test$avgTimeDiff[i] <- mean(tempTime[2:length(tempTime)] - tempTime[1:length(tempTime)-1])
  }
  if (length(tempTime) == 1){
    test$minTimeDiff[i] <- tempTime
  }
  else{
    timeDiff <- tempTime[2:length(tempTime)] - tempTime[1:length(tempTime)-1]
    test$minTimeDiff[i] <- min(timeDiff)
  }
  if (nrow(temp) != 0){
    country_table <- table(sapply(temp$country,as.character))
    for (level in names(country_table)){
      test[paste("country",level,sep="_")][i,] <- country_table[which(names(country_table)==level)]
    } 
    merchandise_table <- table(sapply(temp$merchandise,as.character))
    for (level in names(merchandise_table)){
      test[paste("merchandise",level,sep="_")][i,] <- merchandise_table[which(names(merchandise_table)==level)]
    }
    auction_table <- table(sapply(temp$auction,as.character))
    for (level in names(auction_table)){
      test[paste("auction",level,sep="_")][i,] <- auction_table[which(names(auction_table)==level)]
    }
  }
}
test$avgTimeDiff[is.na(test$avgTimeDiff)] <- 9999999999999999   # no bid
test$minTimeDiff[test$minTimeDiff == Inf] <- 9999999999999999   # no bid
test[is.na(test)] <- 0

# get columns that train and test are all 0 (in auction_xxxxx ?)


write.csv(train,"../input/train_withAuctionCols.csv",quote=F,row.names=F)
write.csv(test,"../input/test_withAuctionCols.csv",quote=F,row.names=F)


# set random seed
set.seed(1)

# =====
# 1. random forest
# outcome ~ # of bids
# =====

# train model
m1 <- randomForest(outcome~numBids,data=train)
# predicton
m1.pred <- predict(m1,test)
# prepare submission
m1.sub <- data.frame(bidder_id=test$bidder_id,
                     prediction=m1.pred)
# write file
write.csv(m1.sub,"../submissions/submission_rf.csv",quote=F,row.names=F)

# =====
# 2. random forest
# outcome ~ # of bids, # of auctions, # of countries, # of merchandise, # of device, # of ip, # of url
# =====

# train model
# outcome ~ numBids, numAuction, numMerchandise, numDevice, numCountry, numIp, numUrl
m2 <- randomForest(outcome~numBids+numAuction+numMerchandise+numCountry+numIp+numUrl,data=train)
# predicton
m2.pred <- predict(m2,test)
m2.pred[m2.pred < 0] <- 0
# prepare submission
m2.sub <- data.frame(bidder_id=test$bidder_id,
                     prediction=m2.pred)
# write file
write.csv(m2.sub,"../submissions/submission_rf2.csv",quote=F,row.names=F)

# variable importance
importance(m2)
# IncNodePurity
# numBids         22.113448247
# numAuction      13.724779262
# numMerchandise   0.004689571
# numCountry      11.149875523
# numIp           18.497962064
# numUrl          15.682294645

# =====
# 3. cart
# outcome ~ # of bids, # of auctions, # of countries, # of merchandise, # of device, # of ip, # of url
# =====

# train model
m3 <- rpart(outcome~numBids+numAuction+numMerchandise+numCountry+numIp+numUrl,data=train)
# predicton
m3.pred <- predict(m3,test)
# prepare submission
m3.sub <- data.frame(bidder_id=test$bidder_id,
                     prediction=m3.pred)
# plot tree
plot(m3,uniform=T,main="Classification Tree")
text(m3, use.n=TRUE, all=TRUE, cex=.8)
# write file
write.csv(m3.sub,"../submissions/submission_cart.csv",quote=F,row.names=F)

# =====
# 4. glm
# outcome ~ # of bids, # of auctions, # of countries, # of merchandise, # of device, # of ip, # of url
# =====

# train model
m4 <- glm(outcome~numBids+numAuction+numMerchandise+numCountry+numIp+numUrl,data=train,family=binomial(link=logit))
# prediction
m4.pred <- predict(m4,test,"response")
# prepare submission
m4.sub <- data.frame(bidder_id=test$bidder_id,
                     prediction=m4.pred)
# write file
write.csv(m4.sub,"../submissions/submission_glm.csv",quote=F,row.names=F)

# =====
# 5. neural network
# outcome ~ # of bids, # of auctions, # of countries, # of merchandise, # of device, # of ip, # of url
# =====

# train model
m5 <- train(outcome~numBids+numAuction+numMerchandise+numCountry+numIp+numUrl,data=train,method="nnet",
            trControl=trainControl(method="cv"))
# prediction
m5.pred <- predict(m5,test)
# prepare submission
m5.sub <- data.frame(bidder_id=test$bidder_id,
                     prediction=m5.pred)
# write file
write.csv(m5.sub,"../submissions/submission_nnet.csv",quote=F,row.names=F)

# =====
# 6. gradient boosting machine
# wrong prediction values
# =====

# train model
m6 <- gbm(outcome~numBids+numAuction+numMerchandise+numCountry+numIp+numUrl,data=train,n.trees=50,shrinkage=0.005,cv.folds=10)
m6.best.iter <- gbm.perf(m6,method="cv")
# prediction
m6.pred <- predict(m6,test,"prob")
# prepare submission
m6.sub <- data.frame(bidder_id=test$bidder_id,
                     prediction=m6.pred)
# write file
write.csv(m6.sub,"../submissions/submission_gbm.csv",quote=F,row.names=F)

# =====
# 7. multivariate adaptive regression splines
# outcome ~ # of bids, # of auctions, # of countries, # of merchandise, # of device, # of ip, # of url
# =====

# train model
m7 <- earth(outcome~numBids+numAuction+numMerchandise+numCountry+numIp+numUrl,data=train)
# prediction
m7.pred <- predict(m7,test)[,1]
m7.pred[m7.pred < 0] <- 0
# prepare submission
m7.sub <- data.frame(bidder_id=test$bidder_id,
                     prediction=m7.pred)
# write file
write.csv(m7.sub,"../submissions/submission_mars.csv",quote=F,row.names=F)

# =====
# 8. random forest
# add average time difference
# =====

# train model
train_sub <- train[!is.na(train$avgTimeDiff),]
m8 <- randomForest(outcome~numBids+numAuction+numMerchandise+numCountry+numIp+numUrl+avgTimeDiff,data=train_sub)
# predicton
m8.pred <- predict(m8,test)
m8.pred[is.na(m8.pred)] <- 0
# prepare submission
m8.sub <- data.frame(bidder_id=test$bidder_id,
                     prediction=m8.pred)
# write file
write.csv(m8.sub,"../submissions/submission_rf3.csv",quote=F,row.names=F)

# =====
# 9. average of all model results
# =====

# calculate mean
m9.pred <- (m2.pred + m3.pred + m4.pred + m5.pred + m7.pred + m8.pred)/6
# prepare submission
m9.sub <- data.frame(bidder_id=test$bidder_id,
                     prediction=m9.pred)
# write file
write.csv(m9.sub,"../submissions/submission_avg.csv",quote=F,row.names=F)

# =====
# 10. weighted average of all model results
# =====

# results weight by Kaggle score
subresult <- c(0.76556,0.86620,0.82075,0.70065,0.74074,0.86656)
subresult <- subresult/sum(subresult)
# calculate mean
m10.pred <- m2.pred*subresult[1] + m3.pred*subresult[2] + m4.pred*subresult[3] + 
  m5.pred*subresult[4] + m7.pred*subresult[5] + m8.pred*subresult[6]
# prepare submission
m10.sub <- data.frame(bidder_id=test$bidder_id,
                      prediction=m10.pred)
# write file
write.csv(m10.sub,"../submissions/submission_wavg.csv",quote=F,row.names=F)

# =====
# 11. random forest
# add minimum bid time gap
# =====

# train model
m11 <- randomForest(outcome~numBids+numAuction+numMerchandise+numCountry+numIp+numUrl+minTimeDiff+avgTimeDiff,data=train)
# predicton
m11.pred <- predict(m11,test)
# prepare submission
m11.sub <- data.frame(bidder_id=test$bidder_id,
                      prediction=m11.pred)
# write file
write.csv(m11.sub,"../submissions/submission_rf4.csv",quote=F,row.names=F)

# variable importance
importance(m11)
# IncNodePurity
# numBids         14.497051019
# numAuction       9.807298331
# numMerchandise   0.008597496
# numCountry       8.880043719
# numIp           12.437473844
# numUrl          11.859954674
# minTimeDiff      5.873232072
# avgTimeDiff     16.874167270

# =====
# 12. weighted average of previous submissions
# =====

# calculate mean
subresult <- sub.result$submission_score / sum(sub.result$submission_score)
m12.pred <- m1.pred*subresult[1] + m2.pred*subresult[2] + m3.pred*subresult[3] + 
  m4.pred*subresult[4] + m5.pred*subresult[5] + m7.pred*subresult[6] + 
  m8.pred*subresult[7] + m11.pred*subresult[8]
  
# prepare submission
m12.sub <- data.frame(bidder_id=test$bidder_id,
                      prediction=m12.pred)
# write file
write.csv(m12.sub,"../submissions/submission_wavg2.csv",quote=F,row.names=F)

# =====
# 13. added more columns
# purity of auction
# purity of merchandise
# purity of country
# purity of ip
# (purity = mode / total number) and the mode
# =====

# clean data
train$modeAuction <- NA
train$percAuction <- NA
train$modeMerchandise <- NA
train$percMerchandise <- NA
train$modeCountry <- NA
train$percCountry <- NA
train$modeIp <- NA
train$percIp <- NA
for (i in 1:nrow(train)){
  print(i)
  tempAuc <- toString(train$auction[i])
  if (tempAuc != ""){
    tempAuc <- table(strsplit(tempAuc,", ")[[1]])
    train$modeAuction[i] <- names(tempAuc)[tempAuc == max(tempAuc)][1]
    train$percAuction[i] <- (tempAuc[tempAuc == max(tempAuc)][1])/sum(tempAuc)
  }
  
  tempMer <- toString(train$merchandise[i])
  if (tempMer != ""){
    tempMer <- table(strsplit(tempMer,", ")[[1]])
    train$modeMerchandise[i] <- names(tempMer)[tempMer == max(tempMer)][1]
    train$percMerchandise[i] <- (tempMer[tempMer == max(tempMer)][1])/sum(tempMer)
  }
  
  tempCon <- toString(train$country[i])
  if (tempCon != ""){
    tempCon <- table(strsplit(tempCon,", ")[[1]])
    train$modeCountry[i] <- names(tempCon)[tempCon == max(tempCon)][1]
    train$percCountry[i] <- (tempCon[tempCon == max(tempCon)][1])/sum(tempCon)
  }
  
  tempIp <- toString(train$ip[i])
  if (tempIp != ""){
    tempIp <- table(strsplit(tempIp,", ")[[1]])
    train$modeIp[i] <- names(tempIp)[tempIp == max(tempIp)][1]
    train$percIp[i] <- (tempIp[tempIp == max(tempIp)][1])/sum(tempIp)
  }
}
write.csv(train,"../input/train_full.csv",quote=F,row.names=F)

test$modeAuction <- NA
test$percAuction <- NA
test$modeMerchandise <- NA
test$percMerchandise <- NA
test$modeCountry <- NA
test$percCountry <- NA
test$modeIp <- NA
test$percIp <- NA
for (i in 1:nrow(test)){
  print(i)
  tempAuc <- toString(test$auction[i])
  if (tempAuc != ""){
    tempAuc <- table(strsplit(tempAuc,", ")[[1]])
    test$modeAuction[i] <- names(tempAuc)[tempAuc == max(tempAuc)][1]
    test$percAuction[i] <- (tempAuc[tempAuc == max(tempAuc)][1])/sum(tempAuc)
  }
  
  tempMer <- toString(test$merchandise[i])
  if (tempMer != ""){
    tempMer <- table(strsplit(tempMer,", ")[[1]])
    test$modeMerchandise[i] <- names(tempMer)[tempMer == max(tempMer)][1]
    test$percMerchandise[i] <- (tempMer[tempMer == max(tempMer)][1])/sum(tempMer)
  }
  
  tempCon <- toString(test$country[i])
  if (tempCon != ""){
    tempCon <- table(strsplit(tempCon,", ")[[1]])
    test$modeCountry[i] <- names(tempCon)[tempCon == max(tempCon)][1]
    test$percCountry[i] <- (tempCon[tempCon == max(tempCon)][1])/sum(tempCon)
  }
  
  tempIp <- toString(test$ip[i])
  if (tempIp != ""){
    tempIp <- table(strsplit(tempIp,", ")[[1]])
    test$modeIp[i] <- names(tempIp)[tempIp == max(tempIp)][1]
    test$percIp[i] <- (tempIp[tempIp == max(tempIp)][1])/sum(tempIp)
  }
}
write.csv(test,"../input/test_full.csv",quote=F,row.names=F)

# train model
train$modeAuction <- as.factor(train$modeAuction)
train$modeMerchandise <- as.factor(train$modeMerchandise)
train$modeCountry <- as.factor(train$modeCountry)
train$modeIp <- as.factor(train$modeIp)
m13 <- randomForest(outcome~numBids+numAuction+numMerchandise+numCountry+numIp+numUrl+minTimeDiff+avgTimeDiff+percAuction+
                      +percMerchandise+percCountry+percIp,data=train[complete.cases(train),])
# predicton
m13.pred <- predict(m13,test)
m13.pred[is.na(m13.pred)] <- 0
m13.pred[m13.pred < 0] <- 0
# prepare submission
m13.sub <- data.frame(bidder_id=test$bidder_id,
                      prediction=m13.pred)
# write file
write.csv(m13.sub,"../submissions/submission_rf5.csv",quote=F,row.names=F)

# variable importance
importance(m13)
# IncNodePurity
# numBids          1.204453e+01
# numAuction       6.549000e+00
# numMerchandise   0.000000e+00
# numCountry       5.397983e+00
# numIp            8.357539e+00
# numUrl           8.595251e+00
# minTimeDiff      4.997338e+00
# avgTimeDiff      1.450803e+01
# percAuction      6.608663e+00
# percMerchandise  1.387779e-19
# percCountry      5.957550e+00
# percIp           1.394434e+01

# =====
# 14. nnet with new features
# =====

# train model
m14 <- train(outcome~numBids+numAuction+numMerchandise+numCountry+numIp+numUrl+minTimeDiff+avgTimeDiff+percAuction+
               +percMerchandise+percCountry+percIp,data=train,method="nnet",
            trControl=trainControl(method="cv"))
# prediction
m14.pred <- predict(m14,test[complete.cases(test),])
# prepare submission
m14.subProb <- rep(NA,nrow(test))
m14.subProb[!complete.cases(test)] <- 0
m14.subProb[complete.cases(test)] <- m14.pred
m14.sub <- data.frame(bidder_id=test$bidder_id,
                     prediction=m14.subProb)
# write file
write.csv(m14.sub,"../submissions/submission_nnet2.csv",quote=F,row.names=F)

# =====
# 15. svm
# =====

# train model
m15 <- svm(outcome~numBids+numAuction+numMerchandise+numCountry+numIp+numUrl+minTimeDiff+avgTimeDiff+percAuction+
               +percCountry+percIp,data=train)
# prediction
m15.pred <- predict(m15,test)
# prepare submission
m15.subProb <- rep(NA,nrow(test))
m15.subProb[!complete.cases(test)] <- 0
m15.subProb[complete.cases(test)] <- m15.pred
m15.sub <- data.frame(bidder_id=test$bidder_id,
                      prediction=m15.subProb)
# write file
write.csv(m15.sub,"../submissions/submission_svm.csv",quote=F,row.names=F)

# =====
# 16. random forest
# dummy variables for categorical variables (country and goods)
# 0.90580 *
# =====

# train model
names(train)[substr(names(train),1,12) == "merchandise_"] <- c("merchandise_auto","merchandise_books","merchandise_clothing",
                                                               "merchandise_computers","merchandise_furniture","merchandise_home",
                                                               "merchandise_jewelry","merchandise_mobile","merchandise_office",
                                                               "merchandise_sproting")
names(test)[substr(names(test),1,12) == "merchandise_"] <- c("merchandise_auto","merchandise_books","merchandise_clothing",
                                                             "merchandise_computers","merchandise_furniture","merchandise_home",
                                                             "merchandise_jewelry","merchandise_mobile","merchandise_office",
                                                             "merchandise_sproting")
m16 <- randomForest(outcome~.-bidder_id-payment_account-address,data=train)
# predicton
m16.pred <- predict(m16,test)
# prepare submission
m16.sub <- data.frame(bidder_id=test$bidder_id,
                      prediction=m16.pred)
# write file
write.csv(m16.sub,"../submissions/submission_rf6.csv",quote=F,row.names=F)

# variable importance
imp16 <- importance(m16)

# =====
# 17. random forest
# feature selection
# 0.89..
# =====

# define control
control <- rfeControl(functions=rfFuncs,method="cv",number=10)
# run RFE algorithm
results <- rfe(outcome~.-bidder_id-payment_account-address,data=train,rfeControl=control)
# summarize
print(results)
# list the chosen features
predictors(results)
# plot the results
plot(results,type=c("g","o"))
# choose 219 varibles
m17 <- randomForest(y=train$outcome,x=train[,predictors(results)])
# predicton
m17.pred <- predict(m17,test)
m17.pred[m17.pred<0] <- 0
# prepare submission
m17.sub <- data.frame(bidder_id=test$bidder_id,
                      prediction=m17.pred)
# write file
write.csv(m17.sub,"../submissions/submission_rf7.csv",quote=F,row.names=F)

# =====
# 18. random forest
# AUCRF
# 0.88..
# =====

# select variables
train$outcome <- as.factor(train$outcome)
m18.select <- AUCRF(outcome~.,data=train[,!(colnames(train) %in% c("bidder_id","payment_account","address"))])
# train model
m18 <- randomForest(y=train$outcome,x=train[,names(summary(m18.select)$ranking)[1:16]])
# predict
m18.pred <- predict(m18,test,"prob")[,2]
# prepare submission
m18.sub <- data.frame(bidder_id=test$bidder_id,
                      prediction=m18.pred)
# write file
write.csv(m18.sub,"../submissions/submission_rf8.csv",quote=F,row.names=F)

# =====
# 19. glm
# 0.80...
# =====

# train model
m19 <- glm(as.formula(paste("outcome",paste(names(summary(m18.select)$ranking)[1:16],collapse="+"),sep="~")),data=train,family=binomial(link=logit))
# predict
m19.pred <- predict.glm(m19,test,"response")
# prepare submission
m19.sub <- data.frame(bidder_id=test$bidder_id,
                      prediction=m19.pred)
# write file
write.csv(m19.sub,"../submissions/submission_glm2.csv",quote=F,row.names=F)

# =====
# 20. random forest
# =====

# define control
control <- rfeControl(functions=rfFuncs,method="cv",number=10)
# run RFE algorithm
results <- rfe(outcome~.-bidder_id-payment_account-address,data=train,rfeControl=control)
# summarize
print(results)
# list the chosen features
predictors(results)
# plot the results
plot(results,type=c("g","o"))
# choose 219 varibles
m20 <- randomForest(y=train$outcome,x=train[,predictors(results)])
# predicton
m20.pred <- predict(m20,test)
m20.pred[m20.pred<0] <- 0
# prepare submission
m20.sub <- data.frame(bidder_id=test$bidder_id,
                      prediction=m20.pred)
# write file
write.csv(m20.sub,"../submissions/submission_rf9.csv",quote=F,row.names=F)
