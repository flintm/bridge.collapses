CompareLMvNN <- function(BridgesDataFrame, predCols, trainRatio = 1,  hidden = c(5,3), predVar = "TFAIL_ERR"){
  require(reshape2)
  require(ggplot2)

predColsNumer    <- predCols[sapply(predCols, function(j) !is.factor(BridgesDataFrame[,j]) & !is.logical(BridgesDataFrame[,j]) & !is.character(BridgesDataFrame[,j]))]
predColsNonNumer <- predCols[!(predCols %in% predColsNumer)]

# normalize
maxs <- as.numeric(apply(BridgesDataFrame[,predColsNumer], 2, max))
mins <- as.numeric(apply(BridgesDataFrame[,predColsNumer], 2, min))
scaledNumeric <- as.data.frame(sapply(1:length(predColsNumer), function(j) (BridgesDataFrame[,predColsNumer[j]] - mins[j])/(maxs[j]-mins[j])))
colnames(scaledNumeric) <- predColsNumer
data <- scaledNumeric

# deal with factors
if (length(predColsNonNumer)>=1){
  m <- as.data.frame(model.matrix(as.formula(paste("~", paste(predColsNonNumer, collapse = " + "))) ,
                                  data = BridgesDataFrame),stringsAsFactors = FALSE)
  data[,colnames(m)[2:ncol(m)]] <- m[,2:ncol(m)]
}

# variable to predict
data[,"PRED_VAR"] <- BridgesDataFrame[,predVar]

# set up training and test data
index <- sample(1:nrow(data),round(trainRatio*nrow(data))) 
train <- data[index,] 
test  <- data[-index,]

# linear model
lm.fit <- glm(PRED_VAR~., data=train)
pr.lm  <- predict(lm.fit,test)
pr.lm.train <- predict(lm.fit,train)
MSE.lm <- sum((pr.lm - test$PRED_VAR)^2)/nrow(test)
MSE.lm.train <- sum((pr.lm.train - train$PRED_VAR)^2)/nrow(train)
MAE.lm <- sum(abs(pr.lm - test$PRED_VAR))/nrow(test)
MAE.lm.train <- sum(abs(pr.lm - train$PRED_VAR))/nrow(train)

# neural network
n      <- names(train) 
f      <- as.formula(paste("PRED_VAR ~", paste(n[!n %in% "PRED_VAR"], collapse = " + "))) 
nn     <- neuralnet(f,data=train, hidden= hidden, linear.output=T)
pr.nn  <- as.numeric(compute(nn,test[,n[!n %in% "PRED_VAR"]])$net.result)
pr.nn.train <- as.numeric(compute(nn,train[,n[!n %in% "PRED_VAR"]])$net.result)
MSE.nn <- sum((pr.nn - test$PRED_VAR)^2)/nrow(test)
MSE.nn.train <- sum((pr.nn.train - train$PRED_VAR)^2)/nrow(train)
MAE.nn <- sum(abs(pr.nn - test$PRED_VAR))/nrow(test)
MAE.nn.train <- sum(abs(pr.nn - train$PRED_VAR))/nrow(train)

MSE <- c(lm = MSE.lm, lm.train = MSE.lm.train, nn = MSE.nn, nn.train = MSE.nn.train)
MAE <- c(lm = MAE.lm, lm.train = MAE.lm.train, nn = MAE.nn, nn.train = MAE.nn.train)

plotData <- data.frame(model = c(rep("LM",nrow(data)),
                               rep("NN",nrow(data)) ),
                       pred.val = c(pr.lm.train, pr.lm, pr.nn.train, pr.nn),
                       obs.val  = c(data[index,"PRED_VAR"],
                                    data[-index,"PRED_VAR"],
                                    data[index,"PRED_VAR"],
                                    data[-index,"PRED_VAR"]),
                       data = c(rep("TRAIN", nrow(train)),
                                rep("TEST", nrow(test)),
                                rep("TRAIN", nrow(train)),
                                rep("TEST", nrow(test))))

resplot <- ggplot(plotData, aes(x = obs.val, y = pred.val, color = model, shape = data)) + 
  coord_equal() + geom_point(size = 6)  + ylim(c(min(data$PRED_VAR),max(data$PRED_VAR))) +
  scale_shape_manual(values = c("o","x"))
return(list(MSE = MSE, MAE = MAE, plot = resplot))
}