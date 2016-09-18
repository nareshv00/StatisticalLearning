x$indicator=0
x.test$indicator=1
xnew=rbind(x,x.test)
pCAx=prcomp(xnew[,1:200],scale=TRUE)
summary(pCAx)
dtaLM=data.frame(cbind(pCAx$x[,1:5],indicator=xnew$indicator))
pCAx$x[10,1:6]
train=subset(dtaLM,dtaLM$indicator==0)
test=subset(dtaLM,dtaLM$indicator==1)

str(train)
trainFull=cbind(train,y)
testFull=cbind(test,y.test)
lm1=lm(y~.-indicator,data = trainFull)
preds=predict(lm1,newdata=testFull)
mse=mean((preds-testFull$y.test)^2)
mse

summary(lm1)

trainNormal=cbind(x,y)
testNormal=cbind(x.test,y.test)
lm2=lm(y~.,data=trainNormal)
summary(lm2)

preds=predict(lm2,newdata=testNormal)
mse=mean((preds-testNormal$y.test)^2)
mse

