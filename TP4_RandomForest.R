library( randomForest)
library(e1071)
library(rpart)
library(rpart.plot)
data= iris
dim(data)
head(iris)
RF=randomForest(Species~., data=iris)
RF
#Call:
# randomForest(formula = Species ~ ., data = iris) 
#               Type of random forest: classification
#                     Number of trees: 500
#No. of variables tried at each split: 2

#        OOB estimate of  error rate: 4%
#Confusion matrix:
#           setosa versicolor virginica class.error
#setosa         50          0         0        0.00
#versicolor      0         47         3        0.06
#virginica       0          3        47        0.06

