
####################################
## Chapter 4 logistics regression ##
####################################

require(ISLR)
head(Smarket)
names(Smarket)
summary(Smarket)
pairs(Smarket, col=Smarket$Direction)
## Logistic regression
glm.fit=glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,
            data=Smarket, family=binomial)
summary(glm.fit)
glm.probs=predict(glm.fit,type="response")
glm.probs[1:5]
glm.pred=ifelse(glm.probs>0.5,"Up","Down")
attach(Smarket)
table(glm.pred, Direction)
mean(glm.pred==Direction)
## Make training and test set
train=Year<2005
glm.fit=glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,
            data=Smarket, family=binomial, subset = train)
glm.probs=predict(glm.fit,newdata = Smarket[!train,], type="response")
glm.pred=ifelse(glm.probs>0.5,"Up","Down")
Direction.2005=Smarket$Direction[!train]
table(glm.pred,Direction.2005)
mean(glm.pred==Direction.2005)
##fit smaller model
glm.fit=glm(Direction~Lag1+Lag2,
            data=Smarket, family=binomial, subset = train)
glm.probs=predict(glm.fit,newdata = Smarket[!train,], type="response")
glm.pred=ifelse(glm.probs>0.5,"Up","Down")
table(glm.pred,Direction.2005)
mean(glm.pred==Direction.2005)
summary(glm.fit)
