---
title: "Chapter 4: Classification"
output: rmarkdown::github_document
---

```{r setup, include=FALSE }
knitr::opts_chunk$set(echo = TRUE)
```

### R Markdown

This is an R Markdown document. Markdown is a simple formatting syntax for authoring HTML, PDF, and MS Word documents. For more details on using R Markdown see <http://rmarkdown.rstudio.com>.

When you click the **Knit** button a document will be generated that includes both content as well as the output of any embedded R code chunks within the document. You can embed an R code chunk like this:

```{r cars}
summary(cars)
```

### Including Plots

You can also embed plots, for example:

```{r pressure, echo=FALSE }
head(pressure)
plot(pressure)
```

Note that the `echo = FALSE` parameter was added to the code chunk to prevent printing of the R code that generated the plot.

### Logistics regression

First we attach the dataset by loading the package of ISLR:

```{r echo=T, results='hide', message=FALSE}
require(ISLR)
```

Next we can have a look at the data of Smarket:
```{r Smarket}
head(Smarket)
summary(Smarket)
pairs(Smarket, col=Smarket$Direction)
```

Now we start to generate the models:
```{r}
glm.fit=glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,
            data=Smarket, family=binomial)
summary(glm.fit)
```
As you can see, there is no significant variable found.
```{r}
glm.probs=predict(glm.fit,type="response")
glm.probs[1:5]
glm.pred=ifelse(glm.probs>0.5,"Up","Down")
attach(Smarket)
table(glm.pred, Direction)
mean(glm.pred==Direction)
```
Now we are trying to make the training and test set
```{r}
train=Year<2005
glm.fit=glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,
            data=Smarket, family=binomial, subset = train)
glm.probs=predict(glm.fit,newdata = Smarket[!train,], type="response")
glm.pred=ifelse(glm.probs>0.5,"Up","Down")
Direction.2005=Smarket$Direction[!train]
table(glm.pred,Direction.2005)
mean(glm.pred==Direction.2005)
```
And then we are trying to fit a smaller model
```{r}
glm.fit=glm(Direction~Lag1+Lag2,
            data=Smarket, family=binomial, subset = train)
glm.probs=predict(glm.fit,newdata = Smarket[!train,], type="response")
glm.pred=ifelse(glm.probs>0.5,"Up","Down")
table(glm.pred,Direction.2005)
mean(glm.pred==Direction.2005)
summary(glm.fit)
```
