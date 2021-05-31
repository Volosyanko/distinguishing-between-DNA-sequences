# Setup & Feature Extraction

install.packages("tree")
install.packages("randomForest")
install.packages("adabag")
library(tree)
library(randomForest)
library(adabag)
set.seed(1234)

dna = read.table("human-phage.txt")
dna[sapply(dna, is.character)] = lapply(dna[sapply(dna, is.character)], as.factor)

numAs = vector(mode = "integer", length = nrow(dna))
for (i in 1:nrow(dna)) {
  numAs[i] = length(which(dna[i,] == "A"))
}

numTs = vector(mode = "integer", length = nrow(dna))
for (i in 1:nrow(dna)) {
  numTs[i] = length(which(dna[i,] == "T"))
}

numCs = vector(mode = "integer", length = nrow(dna))
for (i in 1:nrow(dna)) {
  numCs[i] = length(which(dna[i,] == "C"))
}

numGs = vector(mode = "integer", length = nrow(dna))
for (i in 1:nrow(dna)) {
  numGs[i] = length(which(dna[i,] == "G"))
}

numATs = vector(mode = "integer", length = nrow(dna))
for (i in 1:nrow(dna)){
  for (j in 1:(ncol(dna)-1)){
    numATs[i] = numATs[i] + ((dna[i,j] == "A") & (dna[i,j+1] == "T"))
  }
}

numCGs = vector(mode = "integer", length = nrow(dna))
for (i in 1:nrow(dna)){
  for (j in 1:(ncol(dna)-1)){
    numCGs[i] = numCGs[i] + ((dna[i,j] == "C") & (dna[i,j+1] == "G"))
  }
}

n = nrow(dna)
trainIndex = sample(1:n, size = round(0.7*n), replace=FALSE)
train = dna[trainIndex,]
test = dna[-trainIndex,]
dnanew = data.frame(dna$V1, numAs, numCs, numGs, numTs, numATs, numCGs)
n = nrow(dnanew)
trainIndex = sample(1:n, size = round(0.7*n), replace=FALSE)
train2 = dnanew[trainIndex,]
test2 = dnanew[-trainIndex,]


## Decision trees
set.seed(1234)
tree.hp = tree(V1~., data = train)
plot(tree.hp)
text(tree.hp,pretty = 0, cex = 0.6)
#train
tree.pred = predict(tree.hp, train[,-1], type="class")
table(predicted = tree.pred, true = train[,1])
accuracy=(197+186)/dim(train)[1]
accuracy
#test
tree.pred = predict(tree.hp, test[,-1], type="class")
table(predicted = tree.pred, true = test[,1])
accuracy=(43+55)/dim(test)[1]
accuracy

#Pruning decision trees
set.seed(1234)
cv.hp = cv.tree(tree.hp, FUN = prune.misclass)
plot(cv.hp)
prune.hp = prune.misclass(tree.hp, best = 13)
plot(prune.hp)
text(prune.hp,pretty = 0, cex = 0.6)

#train
set.seed(1234)
tree.pred = predict(prune.hp, train[,-1], type="class")
table(predicted = tree.pred, true = train[,1])
accuracy=(176+163)/dim(train)[1]
accuracy
#test
tree.pred = predict(prune.hp, test[,-1], type="class")
table(predicted = tree.pred, true=test[,1])
accuracy=(51+55)/dim(test)[1]
accuracy


#Random Forest
set.seed(1234)
rf = randomForest(train[,1]~., data = train[,-1], importance=TRUE)
rf
rfpred = predict(rf, test[,-1], type="class")
table(predicted = rfpred, true = test[,1])
accuracy=(69+59)/dim(test)[1]
accuracy

##Extracted features
#Decision tree
tree.hp2 = tree(dna.V1~., data=train2)
plot(tree.hp2)
text(tree.hp2, cex = 0.6)
tree.pred2 = predict(tree.hp2, train2[,-1], type="class")
table(predicted = tree.pred2, true = train2[,1])
accuracy=(214+182)/dim(train2)[1]
accuracy

tree.pred2 = predict(tree.hp2, test2[,-1], type="class")
table(predicted=tree.pred2, true=test2[,1])
accuracy=(73+85)/dim(test2)[1]
accuracy

#Pruning
set.seed(1234)
cv.hp2 = cv.tree(tree.hp2, FUN = prune.misclass)
plot(cv.hp2)
prune.hp2 = prune.misclass(tree.hp2, best = 5)
plot(prune.hp2)
text(prune.hp2, cex = 0.6)
tree.pred2 = predict(prune.hp2, test2[,-1], type="class")
table(predicted = tree.pred2, true=test2[,1])
accuracy=(64+95)/dim(test)[1]
accuracy

#Random forest
rf2 = randomForest(train2[,1] ~ ., data = train2[,-1], importance = TRUE)
rf2

rf.pred2 = predict(rf2, test2[,-1], type="class")
table(predicted = rf.pred2, true = test2[,1])
accuracy=(72+95)/dim(test)[1]
accuracy

#Boosting
set.seed(1234)
boost= boosting(dna.V1 ~ ., data = train2, control = rpart.control(maxdepth = 1))
predboosting = predict.boosting(boost, newdata=train2[,-1])
table(true = train2[,1], predicted = predboosting$class)
predboosting = predict.boosting(boost, newdata=test2[,-1])
table(true = test2[,1], predicted = predboosting$class)

barplot(boost$imp[order(boost$imp, decreasing = TRUE)], ylim = c(0, 100), 
        ylab = "percentage", xlim = c(0,10), main = "Variables Relative Importance", col = "blue")


