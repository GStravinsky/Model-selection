load("/Users/MacBook_Retina_2015_256Gb/Documents/Statistics Warwick/ST952/Assignment_2/czechgold.Rdata")


if(!require(glmnet)){
  install.packages("glmnet")
  library(glmnet)
}

if(!require(PRROC)){
  install.packages("PRROC")
  library(PRROC)
}

if(!require(xtable)){
  install.packages("xtable")
  library(xtable)
}

if(!require(stargazer)){
  install.packages("stargazer")
  library(stargazer)
}

library(plyr)

install.packages("caret")
library(caret)

library(ggplot2)
setwd("~/Documents/Statistics Warwick/ST952/Assignment_2")

summary(czechgold)
# gold to factor
# string to dummies


gold <- czechgold
gold$Gold <- factor(gold$Gold)

gold$sex <- as.factor(gold$sex)
gold$second <- as.factor(gold$second)
gold$frequency <- as.factor(gold$frequency)
gold$type <- as.factor(gold$type)
gold$carduse <- as.factor(gold$carduse)
summary(gold)
str(gold)



attach(gold)
# curious about the zero values.....  they mean something
# but only  cash withrawals and cash credits seem correlated and to some 
# extent cash widtdrawals and credit withdrawals. 
par(mfrow=c(2,2))
hist(age,40)
hist(mcardwdl[mcardwdl!=0], 40)
hist(mcashcr[mcashcr!=0], 40)
hist(mcashwd, 40)

# WITHOUT ZEROS
hist(age)
hist(mcardwdl[mcardwdl!=0])
hist(mcashcr[mcashcr!=0])
hist(mcashwd[mcashwd!=0])
# Not even close to normal but oh well
cor(gold[,c(2:5)])
# same conclussion as the pair plot. 

barplot(prop.table(table(second)))
barplot(prop.table(table(sex)))
barplot(prop.table(table(frequency)))
barplot(prop.table(table(carduse)))
barplot(prop.table(table(type)))

par(mfrow=c(1,1))
length(mcardwdl[mcardwdl==0])
length(carduse[carduse=="No"])
# perfect explanation

### LOG ODDS ####

myemplogit <- function(yvar=y,xvar=x,maxbins=10,sc=1,...){
  breaks  <<- unique(quantile(xvar, probs=0:maxbins/maxbins))
  levs  <<- (cut(xvar, breaks, include.lowest=FALSE))
  num <<- as.numeric(levs)
  c.tab <- count(num,'levs')
  c.tab$levs <- factor(c.tab$levs, levels = levels(addNA(c.tab$levs)), labels = c(levels(c.tab$levs),
                                                                                  paste("[",min(xvar),"]",sep="")), exclude = NULL)
  c.tab <- c.tab[c(nrow(c.tab),1:nrow(c.tab)-1),]
  sc <- (max(c.tab$freq)/min(c.tab$freq)/sc)^2
  zcex <<- c.tab$freq/sc
  print(c.tab);print(zcex);print(sc)
  emplogitplot1(yvar~xvar,breaks=breaks,cex=zcex,...)
}


png("log.odds.png", units = "cm", width = 10, height = 8)
par(mfrow=c(2,2))
myemplogit(Gold,mcardwdl,30,sc=3,xlab="Amount of credit card withdrawals")
myemplogit(Gold,age,40,sc=15,xlab="Age")
myemplogit(Gold,mcashwd,20,sc=10,xlab="Amount of cash withdrawls")
myemplogit(Gold,mcashcr,50,sc=2,xlab="Amount of cash credit")
dev.off()

par(mfrow=c(1,1))



############# Part 2 ##########
# turn off scientific display of numbers
options(scipen = 999)
options(digits=4)

a <- glm(Gold~age:carduse+., family = binomial, data = gold)
y.pred_A <- predict(a,type="response")
table <- summary(a)


roc_A <- roc.curve(scores.class0 = y.pred_A, weights.class0 = y, curve=T)
plot(roc_A)

# age, carduseno, credit withdrawals, sex, interaction

b_min1 <- glm(Gold~age*carduse+mcardwdl+mcashcr+mcashwd+second+sex+type, family = binomial, data = gold)
summary(b_min1)
b_min2 <-  glm(Gold~age*carduse+mcardwdl+mcashwd+second+sex+type, family = binomial, data = gold)
summary(b_min2)
b_min3 <-  glm(Gold~age*carduse+mcashwd+mcardwdl+sex+type, family = binomial, data = gold)
summary(b_min3)
b_min4 <-  glm(Gold~age*carduse+mcashwd+mcardwdl+sex, family = binomial, data = gold)
summary(b_min4)
anova(b_min1, a, test = "Chisq")
anova(b_min2, a, test = "Chisq")
anova(b_min3, a, test = "Chisq")
anova(b_min4, a, test = "Chisq" )

# I think it is good when insignificant. Meaning that bigger model does not give extra meaning.
# So go for parsimonious. 

anova(b_min2, b_min1, test = "Chisq")
anova(b_min3, b_min2, test = "Chisq")
anova(b_min4, b_min3, test = "Chisq" )

b_min51 <-  glm(Gold~age*carduse+mcashwd+sex, family = binomial, data = gold)
anova(b_min51, b_min4, test = "Chisq" )
# significance jumps to 0.074 Bigger model has lower deviance.
b_min52 <-  glm(Gold~age*carduse+mcardwdl+sex, family = binomial, data = gold)
anova(b_min52, b_min4, test = "Chisq" )
#0.068
b_min53 <-  glm(Gold~age+carduse+mcardwdl+mcashwd+sex, family = binomial, data = gold)
anova(b_min53, b_min4, test = "Chisq" )
#0.028
b_min54 <-  glm(Gold~age*carduse+mcardwdl+mcashwd, family = binomial, data = gold)
anova(b_min53, b_min4, test = "Chisq" )
#0.028
# stop with the model b_min4!

y.pred_B <- predict(b_min4,type="response")

roc_B <- roc.curve(scores.class0 = y.pred_B, weights.class0 = y, curve=T)
plot(roc_B)

##### 2C
different <- glm(Gold~age:carduse+., family = binomial, data = gold)
stepwise <- step(different, direction = "both", test = "Chisq")
# - frequency, -mcashcr. - second. - type
# leave mcardwdl, mcashwd, interaction, sex, age, carduse. AIC = 891
# a bit different from a suggestion of simple individual significance
a1 <- glm(Gold~age*carduse+ mcardwdl + mcashwd + sex, family = binomial, data = gold)
summary(a1)

stepwise <- step(different, direction = "both", test = "Chisq")
y.pred_C <- predict(stepwise,type="response")

roc_C <- roc.curve(scores.class0 = y.pred_C, weights.class0 = y, curve=T)
plot(roc_C)

gold2 <- gold[,c(1:7, 9:10)]
#gold2$Af_tr <- ifelse(gold$frequency=="After_Tr", 1,0)
gold2$Weekly <- ifelse(gold$frequency=="Weekly", 1,0)
gold2$Monthly <- ifelse(gold$frequency=="Monthly", 1,0)
gold2$interaction <- ifelse(gold2$carduse=="Yes", gold2$age, 0)

updates_dof <- glm(Gold~., family = binomial, data = gold2)
summary(updates_dof)

stepwise <- step(updates_dof, direction = "backward", test = "Chisq")

####### 2D

#Put  all x-variables into a matrix - use dummy variables rather than factor
x <- gold[,c(2:10)]
x$second <- ifelse(x$second=="Y", 1,0)
x$sex <- ifelse(x$sex=="F", 1,0)
x$carduse <- ifelse(x$carduse=="Yes", 1,0)
x$type <- ifelse(x$type=="OWNER", 1,0)

frq <- dummyVars(~ frequency, data = x, levelsOnly = TRUE)
frq_full <- predict(frq, x)
x <- merge(x = x, y = frq_full, by.x = 0, by.y = 0)
x <- x[,c(2:7, 9:13)]
x <- as.matrix(x)

y <- gold[,1]
y <- ifelse(y==1,1,0)

xy <- as.matrix(gold[,c(2:10)])

# Fit lasso regression
fit1 <- glmnet(x,y, family="binomial", alpha=1)
par(mfrow=c(1,1))
#Crude plot trace
plot(fit1)
# plot of log(lambda) and label traces
plot(fit1,"lambda",label = T)
# Print out solution for lambda=1 (log(lambda) = 0)
coef(fit1,s=0)

# I THINK THIS IS WEIRD BECAUSE OF ZEROS. 
# Cross validation
set.seed(2000)
cvfit1 = cv.glmnet(x,y, family="binomial",alpha=1)
# Plot this
plot(cvfit1)
#  find optimum
cvfit1$lambda.min
# find optimum that is one standard error from optimum 
cvfit1$lambda.1se
# Add lines to mark these two possibilities
abline(v=log(cvfit1$lambda.1se),col="red")
abline(v=log(cvfit1$lambda.min),col="blue")
#scroll back through plots and re-rum above two lines to add these to trace plot against log(lambda)

# Add legend - first put mse plot in window
legend("top",legend=c("Minimum lambda", "1 standard error larger lambda"),lty=c(1,1),col=c("blue","red"), ins=0.05)
# then trace plot against log(lambda)
legend("topright",legend=c("Minimum lambda", "1 standard error larger lambda"),lty=c(1,1),col=c("blue","red"), ins=0.05)
coef(cvfit1, s = "lambda.min")
coef(cvfit1, s = "lambda.1se")

y.pred_D <-predict(cvfit1, s="lambda.min", newx=x, type="response")

roc_D <- roc.curve(scores.class0 = y.pred_D, weights.class0 = y, curve=T)
plot(roc_D)


###### Predictions

betas <- coefficients(stepwise)
betas <- as.matrix(betas)
values <- c(1, 42, 500, 4000, 0, 1, 1)
values <- as.matrix(values)

logistic <- function(X, beta) {
  p <- exp(X %*% beta)/(1+exp(X%*%beta))
  p
}

logistic(X=t(values), beta = betas)
# 1.3 %, so no