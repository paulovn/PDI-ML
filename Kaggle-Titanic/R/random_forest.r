library(randomForest)

#Functions trying to estimate age
find_wife <- function(name, allnames)
{
    transf.name <- sub("Mr.", "Mrs.", name, fixed=TRUE)
    #print("possible wife:");
    #print(transf.name)
    return(grep(transf.name, allnames))
}

find_husband <- function(name, allnames) {
    transf.name <- sub("Mrs.", "Mr.", name, fixed=TRUE)
    transf.name <- sub(" \\(..*\\)", "", transf.name, fixed=FALSE)
    #print("possible husband:");
    #print(transf.name)
    return(grep(transf.name, allnames))
}

find_spouse_age <- function(person, allpersons, allnames) {
    transf.name <- sub(" \"..*\"", "", person$name, fixed=FALSE)
    #print("person:")
    #print(transf.name)

    if (person$sex == "female") {
        if ((person$sibsp > 0) & (length(grep("Mrs.", transf.name)) > 0)) {
            husband <- find_husband(transf.name, allnames)
            #print("husband found")
            #print(husband)
            if (length(husband) > 0) {
                spouse.age <- allpersons[husband,]$age;
                if (is.na(spouse.age) == FALSE) {
                  return(spouse.age - 7)
                }
            }
            return (0)
        }
        return (0)
    } else {
        if (person$sibsp > 0) {
            wife <- find_wife(transf.name, allnames)
            #print("wife found")
            #print(wife)
            if (length(wife) > 0) {
                spouse.age <- allpersons[wife,]$age;
                if (is.na(spouse.age) == FALSE) {
                return(spouse.age + 7)
                }
            }
            return (0)
        }
        return (0)
    }
}



#Data loading 
train_all <- read.csv("d:\\Mis documentos\\ML\\Kraggle\\Titanic\\train.csv", header=TRUE, as.is=TRUE)
test_all  <- read.csv("d:\\Mis documentos\\ML\\Kraggle\\Titanic\\test.csv", header=TRUE,  as.is=TRUE)

#Trying to work with cabins. Unsuccessful
cabin_letter<-substr(train_all$cabin,1,1)
train_all <- cbind(train_all, cabin_letter)
cabin_letter<-substr(test_all$cabin,1,1)
test_all <- cbind(test_all, cabin_letter)

allsets <- rbind(train_all[,-1], test_all)
allnames <- allsets$name
ntrain=length(train_all$name)
ntest=length(test_all$name)

#Selecting fields
train <- data.frame( survived=train_all$survived,
                     age=train_all$age,
                     fare=train_all$fare,
                     pclass=train_all$pclass,
                     sex=as.integer(factor(train_all$sex)) )
test  <- data.frame( age=test_all$age,
                     fare=test_all$fare,
                     pclass=test_all$pclass,
                     sex=as.integer(factor(test_all$sex)) )

#Just a quick test for married
names <- train_all$name
married = c(1:length(train_all$name))
married[] <- 0
married[grep("Mrs.", names)] <- 1

# "female" -> 1, "male" -> 2

#Completing missing age
#First trying to get the spouse age
age<-train_all$age
sum(is.na(train_all$age)) 
for (i in 1:ntrain) {
    if (is.na(age[i])) {
        #print("age NA")
        #print(i)
        #print(train_all[i,])
        new.age <- find_spouse_age(train_all[i,], allsets, allnames)
        if (new.age[1] != 0) {
            #print("new age:")
            #print(new.age)
            age[i] <- new.age
        }
    }
}
sum(is.na(age)) 
#Then, some general heuristics
age[is.na(age) & (train_all$sex == "female") & (married == 1)] <- 40
sum(is.na(age))
age[is.na(age) & (train_all$sex == "female") & (train_all$parch == 0)] <- 25
sum(is.na(age))
age[is.na(age) & (train_all$sex == "female")] <- 10
sum(is.na(age))
age[is.na(age) & ((train_all$sibsp > 1) | (train_all$parch > 0))] <- 10
sum(is.na(age))
age[is.na(age) & (train_all$sibsp == 1)] <- 50
sum(is.na(age))
age[is.na(age) & (train_all$sibsp == 0)] <- 30
sum(is.na(age))
train$age<-age

#Just a quick test for married, for test
names <- test_all$name
married = c(1:length(test_all$name))
married[] <- 0
married[grep("Mrs.", names)] <- 1

#Same age estimation for test dataset
age<-test_all$age
for (i in 1:ntest) {
    if (is.na(age[i])) {
        new.age <- find_spouse_age(test_all[i,], allsets, allnames)
        if ((is.na(new.age) == FALSE) && (new.age[1] > 0)) {
            age[i] <- new.age
        }
    }
}
sum(is.na(age))
age[is.na(age) & (test_all$sex == "female") & (married == 1)] <- 40
sum(is.na(age))
age[is.na(age) & (test_all$sex == "female") & (test_all$parch == 0)] <- 25
sum(is.na(age))
age[is.na(age) & (test_all$sex == "female")] <- 10
sum(is.na(age))
age[is.na(age) & ((test_all$sibsp > 1) | (test_all$parch > 0))] <- 10
sum(is.na(age))
age[is.na(age) & (test_all$sibsp == 1)] <- 50
sum(is.na(age))
age[is.na(age) & (test_all$sibsp == 0)] <- 30
sum(is.na(age))
test$age<-age

#There are fare missing values in test. Anyway, we set both datasets
train$fare[ is.na( train$fare) ] <- 0
test$fare[ is.na( test$fare) ]   <- 0

#Try to have special "sex" for children and old people. Not working
#train$sex[train$age < 16] <- 3
#test$sex[test$age < 16] <- 3
#train$sex[train$age > 65] <- 4
#test$sex[test$age > 65] <- 4

#Finally
labels <- as.factor(train[,1])
train <- train[,-1]
rf <- randomForest(train, labels, xtest=test, ntree=5000,do.trace=TRUE, proximity=TRUE)
print(rf)
plot(rf, log="y")
varImpPlot(rf)
MDSplot(rf, labels)

