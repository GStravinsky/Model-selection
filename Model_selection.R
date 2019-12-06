################################
#  Check and install packages  #
################################
library(car)
library(carData)
library(MASS)
library(leaps)
library(glmnet)
library(PRROC)
library(ggplot2)
library(GGally)
library(gridExtra)
library(plyr)


####################################
# Exploratory analysis of the data #
####################################


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


# Plot1
p1 <- ggplot(gold, aes(x=age,color=Gold,fill=Gold)) + 
  geom_histogram(bins = 40, alpha=.5, position = 'identity')+
  scale_color_manual(values=c("steelblue", "orange"))+ 
  labs(x = "Age") +
  scale_fill_manual(values=c("steelblue", "orange"))+
  facet_grid(Gold ~ .,scales="free_y") +
  theme(legend.position='none',
        axis.text.y= element_text(size=6),
        axis.title.y = element_blank())

p2 <- ggplot(gold, aes(x=mcardwdl,color=Gold,fill=Gold)) + 
  geom_histogram(bins = 40, alpha=.5, position = 'identity')+
  scale_color_manual(values=c("steelblue", "orange"))+
  scale_fill_manual(values=c("steelblue", "orange"))+
  labs(x = "Card withdrawals") +
  facet_grid(Gold ~ .,scales="free_y")+
  theme(legend.position='none',
        axis.text.y= element_text(size=6),
        axis.title.y = element_blank())

p2.1 <- ggplot(gold[gold$mcardwdl>0,], aes(x=mcardwdl,color=Gold,fill=Gold)) + 
  geom_histogram(bins = 40, alpha=.5, position = 'identity')+
  scale_color_manual(values=c("steelblue", "orange"))+
  scale_fill_manual(values=c("steelblue", "orange"))+
  labs(x = "Card withdrawals (no zeros)") +
  facet_grid(Gold ~ .,scales="free_y")+
  theme(legend.position='none',
        axis.text.y= element_text(size=6),
        axis.title.y = element_blank())

p2.all <- grid.arrange(p2,p2.1,nrow = 1)

p3 <-ggplot(gold, aes(x=mcashcr,color=Gold,fill=Gold)) + 
  geom_histogram(bins = 40, alpha=.5, position = 'identity')+
  scale_color_manual(values=c("steelblue", "orange"))+
  scale_fill_manual(values=c("steelblue", "orange"))+
  facet_grid(Gold ~ .,scales="free_y")+
  theme(legend.position='none',
        axis.text.y= element_text(size=6),
        axis.title.y = element_blank()) +
  labs(x = "Cash credit") 

p3.1 <-ggplot(gold[gold$mcashcr>0,], aes(x=mcashcr,color=Gold,fill=Gold)) + 
  geom_histogram(bins = 40, alpha=.5, position = 'identity')+
  scale_color_manual(values=c("steelblue", "orange"))+
  scale_fill_manual(values=c("steelblue", "orange"))+
  facet_grid(Gold ~ .,scales="free_y")+
  theme(legend.position='none',
        axis.text.y= element_text(size=6),
        axis.title.y = element_blank())+
  xlab("Cash credit (no zeros)") 

p3.all <- grid.arrange(p3,p3.1,nrow = 1)

p4 <-ggplot(gold, aes(x=mcashwd,color=Gold,fill=Gold)) + 
  geom_histogram(bins = 40, alpha=.5, position = 'identity')+
  scale_color_manual(values=c("steelblue", "orange"))+
  scale_fill_manual(values=c("steelblue", "orange"))+
  facet_grid(Gold ~ .,scales="free_y")+
  theme(legend.position='none',
        axis.text.y= element_text(size=6),
        axis.title.y = element_blank())+
  xlab("Cash withdrawals") 

p5 <-ggplot(gold, aes(x=second)) + 
  geom_bar(alpha=.5, position = 'identity',
           color=c("steelblue","steelblue","orange","orange"),
           fill=c("steelblue","steelblue","orange","orange"))+
  facet_grid(Gold ~ .,scales="free_y")+
  theme(legend.position='none',
        axis.text.y= element_text(size=6),
        axis.title.y = element_blank())+
  xlab("Second card") 

p6 <-ggplot(gold, aes(x=sex)) + 
  geom_bar(alpha=.5, position = 'identity',
           color=c("steelblue","steelblue","orange","orange"),
           fill=c("steelblue","steelblue","orange","orange"))+
  facet_grid(Gold ~ .,scales="free_y")+
  theme(legend.position='none',
        axis.text.y= element_text(size=6),
        axis.title.y = element_blank())+
  xlab("Sex") 

p7 <-ggplot(gold, aes(x=frequency)) + 
  geom_bar(alpha=.5, position = 'identity',
           color=c("steelblue","steelblue","steelblue",
           "orange","orange","orange"),
           fill=c("steelblue","steelblue","steelblue",
           "orange","orange","orange"))+
  facet_grid(Gold ~ .,scales="free_y")+
  theme(legend.position='none',
        axis.text.y= element_text(size=6),
        axis.title.y = element_blank())+
  xlab("Frequency of statements") 

p8 <-ggplot(gold, aes(x=carduse)) + 
  geom_bar(alpha=.5, position = 'identity',
           color=c("steelblue","steelblue","orange","orange"),
           fill=c("steelblue","steelblue","orange","orange"))+
  facet_grid(Gold ~ .,scales="free_y")+
  theme(legend.position='none',
        axis.text.y= element_text(size=6),
        axis.title.y = element_blank())+
  xlab("Card user") 

p9 <-ggplot(gold, aes(x=type)) + 
  geom_bar(alpha=.5, position = 'identity',
           color=c("steelblue","steelblue","orange","orange"),
           fill=c("steelblue","steelblue","orange","orange"))+
  facet_grid(Gold ~ .,scales="free_y")+
  theme(legend.position='none',
        axis.text.y= element_text(size=6),
        axis.title.y = element_blank())+
  scale_x_discrete(labels=c("OWNER" = "O", "USER" = "U"))+
  xlab("Type") 


lay <- rbind(c(1,1,2,2,2,2),
             c(3,3,4,4,4,4),
             c(5,6,7,7,8,9))
grid.arrange(p1,p2.all,p4,p3.all,p5,p6,p7,p8,p9,layout_matrix = lay)

#plot 2

attach(czechgold)
myemplogit <- function(yvar=y,xvar=x,maxbins=30,sc=1, xlabel){
  yvar.fac <- as.factor(yvar)
  breaks  <- unique(quantile(xvar, probs=0:maxbins/maxbins))
  levs  <- (cut(xvar, breaks, include.lowest=FALSE))
  num <- as.numeric(levs)
  c.tab <- do.call(data.frame,aggregate(yvar~addNA(levs)
  ,na.action=na.pass,FUN=function(x)
    c(length(x),sum(x))))
  mean.tab <- aggregate(xvar~addNA(levs),na.action=na.pass,FUN=mean)
  c.tab <- cbind(c.tab,mean.tab[,2])
  colnames(c.tab) <- c("levs","n","y","mean")
  # c.tab <<- count(num,'levs')
  c.tab$levs <- factor(c.tab$levs, levels = c.tab$levs, labels =
                         c(levels(c.tab$levs)[-length(c.tab$levs)],
                         paste("[",min(xvar),"]",sep="")), 
                       exclude = NULL)
  c.tab <- c.tab[c(nrow(c.tab),1:nrow(c.tab)-1),]
  
  c.tab$odds <-  (c.tab$y+0.5)/(c.tab$n - c.tab$y + 0.5)
  c.tab$logit <- log(c.tab$odds)
  
  ggplot(c.tab, aes(x=mean, y=logit)) + 
    geom_point(aes(size=n),color="steelblue")+
    geom_smooth(method="lm",se=F,color="orange")+
    xlab(deparse(substitute(xvar))) + ylab("Log(Odds)")+xlab(xlabel)+
    theme(legend.position='none')
}

p1 <- myemplogit(Gold,age,maxbins=40, xlabel= "Age")
p2 <- myemplogit(Gold,mcardwdl,maxbins=40, xlabel= "Card withdrawals")
p3 <- myemplogit(Gold,mcashcr,maxbins=40, xlabel= "Cash credit")
p4 <- myemplogit(Gold,mcashwd,maxbins=40, xlabel= "Cash withdrawals")

Ratio.plot <- function(variable, var.lab){
  formula <- paste("Gold ~ ",variable,sep = "")
  df <- aggregate(as.formula(formula),data=czechgold,FUN=mean)
  colnames(df)[2] <- "Ratio"
  if (variable!="frequency"){
    return(ggplot(df, aes_string(x=variable, y="Ratio")) +  xlab(var.lab) +
             geom_bar(stat="identity", color=c("steelblue","orange"),
                      fill=c("steelblue","orange"))) + xlab(var.lab)
  } else {
    return(ggplot(df, aes_string(x=variable, y="Ratio")) + xlab(var.lab) +
             geom_bar(stat="identity", color=c("steelblue","lightblue","orange"),
                      fill=c("steelblue","lightblue","orange"))) 
  } 
}

col.fac <- c("second","sex","frequency","carduse","type")
lab.fac <- c("Second card", "Sex", "Frequency of the statements", 
"Card user", "Type")
for (i in 1:length(col.fac)){
  assign(paste("p",as.character(i+4),sep=""), Ratio.plot(variable = col.fac[i], 
  var.lab = lab.fac[i]))
}

grid.arrange(p1,p2,p3,p4,p5,p6,p7,p8,p9,nrow = 3)

detach(czechgold)



##turn off scientific display of numbers
options(scipen = 999)
options(digits= 7)


#################################
## Full model plus age:carduse #
################################

#transform Data
get_dummy <- function(df, col){
  col.fac <- factor(df[[col]])
  dummy <- model.matrix(~col.fac+0)
  colnames(dummy) <- gsub("col.fac",paste(col,"_",sep=""),
  colnames(dummy),fixed = TRUE)
  df <- cbind(df, dummy)
  return(df)
}

czechgold1 <- czechgold
for (i in 6:10){
  cols <- colnames(czechgold1)
  czechgold1 <- get_dummy(czechgold1, cols[i])
}
czechgold1$interact <- czechgold1$age * czechgold1$carduse_Yes

col.slt <- colnames(czechgold1)[-c(6:10,11,13,15,18,20)]
czechgold2 <- czechgold1[,col.slt]
#using transformed data in all model

full.fit <- glm(Gold~., family = binomial, data = czechgold2)
summary(full.fit)

########################
# 2b                   #
# Analysis of deviance #
########################
a <- glm(Gold~., family = binomial, data = czechgold2)
Anova(a, type = 2)
# /Weekly
b_min1 <- glm(Gold~.-frequency_Weekly, family = binomial, data = czechgold2)
Anova(b_min1, type = 2)
# /mcashcr
b_min2 <-  glm(Gold~.-frequency_Weekly-mcashcr, family = binomial,
data = czechgold2)
Anova(b_min2, type = 2)
# /carduse
b_min3 <-  glm(Gold~.-frequency_Weekly-mcashcr-carduse_Yes, family = binomial, 
data = czechgold2)
Anova(b_min3, type = 2)
# /Monthly
b_min4 <-  glm(Gold~.-frequency_Weekly-mcashcr-frequency_Monthly-carduse_Yes,
family = binomial, data = czechgold2)
Anova(b_min4, type = 2)
# /second_Yes
b_min5 <-  glm(Gold~.-frequency_Weekly-mcashcr-frequency_Monthly-carduse_Yes
-second_Y, family = binomial, data = czechgold2)
Anova(b_min5, type = 2)
# /type_USER
b_min6 <-  glm(Gold~.-frequency_Weekly-mcashcr-frequency_Monthly-carduse_Yes
-second_Y-type_USER, family = binomial, data = czechgold2)
Anova(b_min6, type = 2)
# /age
b_min7 <-  glm(Gold~.-age-frequency_Weekly-mcashcr-frequency_Monthly-carduse_Yes
-second_Y-type_USER, family = binomial, data = czechgold2)
Anova(b_min7, type = 2)
# /mcashwd
b_min8 <-  glm(Gold~.-age-mcashwd-frequency_Weekly-mcashcr-frequency_Monthly
-carduse_Yes-second_Y-type_USER, family = binomial, data = czechgold2)
Anova(b_min8, type = 2)

summary(b_min8)
# STOP - b_min8 is the chosen model.


####################
# 2c               #
# Step-wise method #
####################

k.quantile <- qchisq(0.05, 1, lower.tail = FALSE)
# k=3.814146

stepwise <- step(a, direction = "both", test = "Chisq", k = k.quantile )
# Identical to the deviance model. 

#########
# 2d    #
# LASSO #
#########

x <- czechgold2[,c(2:12)]
x <- as.matrix(x)

y <- czechgold2[,1]
y <- ifelse(y==1,1,0)

# model unstable - seed required
set.seed(200)

# Fit lasso regression
fit1 <- glmnet(x,y, family="binomial", alpha=1)
par(mfrow=c(1,1))
#Crude plot trace
plot(fit1)
# plot of log(lambda) and label traces
plot(fit1,"lambda",label = T)
# all coefficients shrank to zero

# CROSS VALIDATION

cvfit1 = cv.glmnet(x,y, family="binomial",alpha=1)
# Plot this
plot(cvfit1)
#  find optimum
cvfit1$lambda.min
# 0.001363988
cvfit1$lambda.1se
# 0.004571546

coef(cvfit1, s = "lambda.min")
# mcashwd, sex, type, interaction remain
coef(cvfit1, s = "lambda.1se")
# all zeros

plot(fit1,"lambda",label = T)
abline(v=log(cvfit1$lambda.1se),col="red")
abline(v=log(cvfit1$lambda.min),col="blue")
legend("bottomright",legend=c("Minimum lambda", "1 s.e. larger lambda")
,lty=c(1,1),col=c("blue","red"), ins=0.00)

##################################
# Interpretation and predictions #
##################################

###### Predictions

betas <- coefficients(stepwise)
betas <- as.matrix(betas)
values <- c( 1, 500, 0, 42)
values <- as.matrix(values)

logistic <- function(X, beta) {
  p <- exp(X %*% beta)/(1+exp(X%*%beta))
  p
}

logistic(X=t(values), beta = betas)
# 4 %

means <- c(1, 359.3, 1, 46.28)
credit <- c(1, 1359.3, 1, 46.28)
female <- c(1, 359.3, 0, 46.28)
age <- c(1, 359.3, 1, 56.28)

# change for taking 1000 more card withdrawls
logistic(X=t(means), beta = betas) - logistic(X=t(credit), beta = betas)
# change for being female
logistic(X=t(means), beta = betas) - logistic(X=t(female), beta = betas)
# change for being 10 years older given being a card user.
logistic(X=t(means), beta = betas) - logistic(X=t(age), beta = betas)


##################################
# Further Validation Plots #
##################################


avPlots(stepwise)

y.pred <- predict(stepwise,type="response")

pr <- pr.curve(scores.class0 = y.pred, weights.class0 = y.train, curve=T)
plot(pr)

##################################
# EXTRA: data without duplicates #
##################################

#################################################
##for accounts which have users                 #
##if user has gold card||owner has gold card    #
##set both user and owener has gold card(gold=1)#
##delete data of user                           #
##delete 'type'                                 #
#################################################
data1 = czechgold[,-10]
repeat_data = data1[duplicated(data1[,-1]),]
user_gold = repeat_data[(repeat_data$Gold == 1),]
data1[as.numeric(row.names(user_gold))-1,]$Gold = 1
deleted_data = data1[!duplicated(data1[,-1]),]

deleted_data$Gold <- factor(deleted_data$Gold)
deleted_data$sex <- as.factor(deleted_data$sex)
deleted_data$second <- as.factor(deleted_data$second)
deleted_data$frequency <- as.factor(deleted_data$frequency)
deleted_data$carduse <- as.factor(deleted_data$carduse)


deleted_data$Af_tr <- ifelse(deleted_data$frequency=="After_Tr", 1,0)
deleted_data$Weekly <- ifelse(deleted_data$frequency=="Weekly", 1,0)
deleted_data$Monthly <- ifelse(deleted_data$frequency=="Monthly", 1,0)
deleted_data$interaction <- ifelse(deleted_data$carduse=="Yes",
deleted_data$age, 0)
fit.deleted <- glm(Gold~.-Af_tr, family = binomial,data=deleted_data)
fit.step<- step(fit.deleted, direction = "both", test = "Chisq",k=3.841)
y_DEL <- deleted_data[,1]
y_DEL <- ifelse(y_DEL==1,1,0)
summary(fit.step)


coefs = summary(fit.step)$coef[,1]
customer <- c(1,500,0,0,42)
 p = customer*coefs
 p = sum(customer*coefs)
 p

exp(p) / (1 + exp(p))
##0.04054554
 y.pred_DEL <- predict(fit.step,type="response")
 roc_DEL <- roc.curve(scores.class0 = y.pred_DEL, weights.class0 = y_DEL,
 curve=T)
 plot(roc_DEL)
 

 
