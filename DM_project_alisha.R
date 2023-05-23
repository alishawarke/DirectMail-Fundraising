
setwd("/Users/alishawarke/MS/MSIS 672 Data Mining /DM project")
FutureFundraising.df <- read.csv("FutureFundraising.csv")
is.null(FutureFundraising.df$TARGET_B)
Fundraising.df <- read.csv("Fundraising.csv")
#Fundraising.df <- Fundraising.df[ , -c(1,2,3,4,5,6,10,21,24)]
#FR.df <- Fundraising.df[ , c(9,18,20,22,23)]
#str(FR.df)
Fundraising.df <-Fundraising.df[ , -c(24)]
#Fundraising.df <- Fundraising.df[ , c(18,20,23)]
is.null(Fundraising.df)
data_1<-Fundraising.df[Fundraising.df$TARGET_B=="0", ]
nrow(data_1)
data_2 <-Fundraising.df[Fundraising.df$TARGET_B=="1", ]
nrow(data_2)


xtotal <- model.matrix(~ INCOME + WEALTH, data = Fundraising.df) 


xtotal <- as.data.frame(model.matrix(~ 0+ as.factor(INCOME) + as.factor(WEALTH), data = Fundraising.df))
t(t(names(xtotal))) # check the names of the dummy variables head(xtotal)
head(xtotal)

set.seed(12345)
train.index <- sample(nrow(Fundraising.df), nrow(Fundraising.df)*0.6)
tra.df <- Fundraising.df[train.index, ]

val.df <- Fundraising.df[-train.index, ]
library(rpart)
library(rpart.plot)

class.tree <- rpart(TARGET_B  ~ ., data = Fundraising.df, method = "class")
rpart.control(minsplit = 5, minbucket = round(5/3), cp = 1,maxdepth = 3)
prp(class.tree, type = 0, extra = "auto", under = TRUE, split.font = 1, varlen = -10)
rpart.rules(class.tree)



#library(caret)
class.tree.pred.train <- predict(class.tree, tra.df, type = "class")
confusionMatrix(class.tree.pred.train, as.factor(tra.df$TARGET_B))

class.tree.pred.valid <- predict(class.tree, val.df, type = "class")
confusionMatrix(class.tree.pred.valid, as.factor(val.df$TARGET_B))

rpart.control(minsplit = 20, minbucket = round(5/3), maxdepth = 30)

accuracy_tune <- function(class.tree.pred.train) {
  class.tree.pred.train <- predict(class.tree, tra.df, type = "class")
  confusionMatrix(class.tree.pred.train, as.factor(tra.df$TARGET_B))
}
control <- rpart.control(minsplit = 7,
                         minbucket = round(7 / 3),
                         maxdepth = 30,
                         cp = 0.5)
tune_fit <- rpart(TARGET_B~., data = tra.df, method = 'class', control = control)
accuracy_tune(tune_fit)

deeper.ct <- rpart(TARGET_B ~ ., data = tra.df, method = "class", cp = 0, minsplit = 1) # count number of leaves
length(deeper.ct$frame$var[deeper.ct$frame$var == "<leaf>"])
# plot tree
prp(deeper.ct, type = 1, extra = 1, under = TRUE, split.font = 1, varlen = -10, 
    box.col=ifelse(deeper.ct$frame$var == "<leaf>", 'gray', 'white'))
accuracy_tune(deeper.ct)

cv.ct <- rpart(TARGET_B ~ ., data = tra.df, method = "class",
               cp = 0.00001, minsplit = 5, xval =5) 
printcp(cv.ct)
# use printcp() to print the table. printcp(cv.ct)

pruned.ct <- prune(cv.ct,
                   cp = cv.ct$cptable[which.min(cv.ct$cptable[,"xerror"]),"CP"]) 

length(pruned.ct$frame$var[pruned.ct$frame$var == "<leaf>"]) 
prp(pruned.ct, type = 1, extra = 1, split.font = 1, varlen = -10)
accuracy_tune(pruned.ct)

library(adabag)
library(rpart)
library(caret)
boosting(TARGET_B ~ ., data = tra.df) 
pred <- predict(boost, valid.df) 
confusionMatrix(pred$class, val.df$TARGET_B)

##################

library(randomForest)# need to install radomForest package
## random forest

rf <- randomForest(as.factor(TARGET_B) ~ ., data = tra.df, ntree = 1000, 
                   mtry = 4, nodesize = 5, importance = TRUE)  

## variable importance plot
varImpPlot(rf, type = 1)

#library(caret)

## confusion matrix
rf.pred <- predict(rf, val.df)
confusionMatrix(rf.pred, as.factor(val.df$TARGET_B))
