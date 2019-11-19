load("//pedley.ads.warwick.ac.uk/user62/u/u1620789/Documents/ST952/Ass_2_R/czechgold.Rdata")

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
pairs(age~., data=gold[,c(2:5)])
# curious about the zero values.....  they mean something
# but only  cash withrawals and cash credits seem correlated and to some 
# extent cash widtdrawals and credit withdrawals. 
par(mfrow=c(2,2))
hist(age)
hist(mcardwdl)
hist(mcashcr)
hist(mcashwd)

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

length(mcardwdl[mcardwdl==0])
length(carduse[carduse=="No"])
# perfect explanation


############# Part 2 ##########
# turn off scientific display of numbers
options(scipen = 999)
options(digits=4)

a <- glm(Gold~age*carduse+., family = binomial, data = gold)
summary(a)
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
summary(b_min4)

##### 2C
step(a, direction = "both", test = "Chisq")
# - frequency, -mcashcr. - second. - type
# leave mcardwdl, mcashwd, interaction, sex, age, carduse. AIC = 891
# a bit different from a suggestion of simple individual significance
a1 <- glm(Gold~age*carduse+ mcardwdl + mcashwd + sex, family = binomial, data = gold)
summary(a1)
