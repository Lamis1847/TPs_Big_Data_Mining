library(mlbench)
library(rpart)
library(randomForest)
data(Shuttle)
data=Shuttle
dim(data)
/////////
[1] 58000    10
///////
head(Shuttle)
n=dim(data)[1]
index = sample(n, 0.7 * n)
Appren = data[index, ]
Test = data[-index, ]
T1= Sys.time()
T1
/////////
[1] "2022-05-08 09:38:34 WAT"
////////////
nb.model <- svm(Type ~ ., data = Appren)
nb.model
///////////////////////
Call:
svm(formula = Class ~ ., data = Appren)


Parameters:
   SVM-Type:  C-classification 
 SVM-Kernel:  radial 
       cost:  1 

Number of Support Vectors:  1197
//////////////////////////////
Pred <- predict(nb.model, newdata=Test)
Test.mod <- cbind(Test, Pred)
head(Test.mod, 5)
/////////////////////
              V1 V2 V3 V4 V5 V6 V7 V8 V9    Class     Pred
4  37  0 76  0 28 18 40 48  8 Rad.Flow Rad.Flow
7  56  0 81  0 -4 11 25 86 62     High     High
8  55 -1 95 -3 54 -4 40 41  2 Rad.Flow Rad.Flow
9  53  8 77  0 28  0 23 48 24     High     High
11 37  0 78 -2 12  0 42 65 24 Rad.Flow Rad.Flow

/////////////////////
Confusion = table(Test.mod$Class, Test.mod$Pred)
Confusion
////////////////
     Rad.Flow Fpv.Close Fpv.Open  High Bypass Bpv.Close Bpv.Open
  Rad.Flow     13618         0        0     0      0         0        1
  Fpv.Close        3         6        0     5      2         0        0
  Fpv.Open        12         0       41     0      1         0        0
  High             4         0        0  2700      1         0        0
  Bypass           3         0        0     0    993         0        0
  Bpv.Close        1         0        0     0      0         1        0
  Bpv.Open         8         0        0     0      0         0        0

//////////////////
err <- 1-sum(diag(Confusion))/sum(Confusion)
err
////////////
[1] 0.002356322
/////////////
T2= Sys.time()
T2
[1] "2022-05-08 09:45:54 WAT"
///////
TEShuttle= T2-T1
TEShuttle
///////////
Time difference of 7.327361 mins
plot(cmdscale(dist(Shuttle[,-5])),col = as.integer(Shuttle[,5]),pch = c("o","+")[1:150 %in% nb.model$index + 1])
/////
//// NaiveBayes////////
T1= Sys.time()
T1
nb.model <- naiveBayes(Class ~ ., data = Appren)
nb.model
Confusion = table(Test.mod$Class, Test.mod$Pred)
Confusion
err <- 1-sum(diag(Confusion))/sum(Confusion)
err
////[1] 0.002356322
T2= Sys.time()
T2
TDBayes = T2-T1
TDBayes
////[1] "2022-05-08 09:55:29 WAT"
////// Random Forest //////////////
T1= Sys.time()
T1
nb.model <- randomForest(Class ~ ., data = Appren)
nb.model
RF=randomForest(Class~., data=Shuttle)
RF
////
Call:
 randomForest(formula = Class ~ ., data = Shuttle) 
               Type of random forest: classification
                     Number of trees: 500
No. of variables tried at each split: 3

        OOB estimate of  error rate: 0.01%
Confusion matrix:
          Rad.Flow Fpv.Close Fpv.Open High Bypass Bpv.Close Bpv.Open
Rad.Flow     45585         0        1    0      0         0        0
Fpv.Close        0        49        0    1      0         0        0
Fpv.Open         1         0      170    0      0         0        0
High             0         1        1 8901      0         0        0
Bypass           0         0        0    0   3267         0        0
Bpv.Close        0         0        0    1      1         8        0
Bpv.Open         0         0        0    0      0         0       13
           class.error
Rad.Flow  2.193656e-05
Fpv.Close 2.000000e-02
Fpv.Open  5.847953e-03
High      2.246434e-04
Bypass    0.000000e+00
Bpv.Close 2.000000e-01
Bpv.Open  0.000000e+00     
/////
T2= Sys.time()
T2
TDForest = T2-T1
TDForest
//Time difference of 2.298249 mins
////Tunning ////

tune.randomForest(Class~.,data=Shuttle,ntree=200)
RN <- randomForest(Class~ ., data = Appren, ntree = 200)

//// Rpart////////
T1= Sys.time()
T1
nb.model <- rpart(Class ~ ., data = Appren)
nb.model
Confusion = table(Test.mod$Class, Test.mod$Pred)
Confusion
err <- 1-sum(diag(Confusion))/sum(Confusion)
err
///// [1] 0.002356322
T2= Sys.time()
T2
TDRpart = T2-T1
TDRpart
////Time difference of 1.312882 mins
