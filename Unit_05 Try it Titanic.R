# Unit 5. Try it. 
# Titanic data
library(data.table); library(epiDisplay)

# Read titanic data, skip 1st column
titanic = fread("http://biostat.mc.vanderbilt.edu/wiki/pub/Main/DataSets/titanic.txt", 
                stringsAsFactors = F, data.table = F, drop = 1)

for(i in c("pclass", "embarked", "room", "sex")) titanic[, i] = as.factor(titanic[, i] )

# Make "survived" more readable
titanic$survived = factor(titanic$survived, levels = c(0,1), labels = c("No", "Yes"))

# Logistic Regression
titanic_logit = glm(survived ~ pclass + age + sex, family = "binomial", data = titanic)
epiDisplay::logistic.display( titanic_logit)

# relevel() to change reference group/level
titanic$sex = relevel(titanic$sex, ref = "male")
titanic$pclass = relevel(titanic$pclass, ref = "3rd")
titanic$age = factor(ifelse(titanic$age <= 18, "Child", "Adult"))

titanic_logit = glm(survived ~ pclass + age + sex, family = "binomial", data = titanic)
epiDisplay::logistic.display( titanic_logit)

# Getting probabilities
# Jack
predict(titanic_logit, newdata = data.frame(pclass = "3rd", name = NA, 
                                            age = "Adult", embarked =  NA, 
                                            home.dest = NA, room = NA, 
                                            ticket = NA, boat = NA, 
                                            sex = "male"), type = "response")
# Rose 
predict(titanic_logit, newdata = data.frame(pclass = "1st", name = NA, 
                                            age = "Adult", embarked =  NA, 
                                            home.dest = NA, room = NA, 
                                            ticket = NA, boat = NA, 
                                            sex = "female"), type = "response")


