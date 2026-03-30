library(tidyverse)
library(mice)
library(ggmice)
library(Hmisc)
library(VIM)
library(magrittr)
library(tree)
library(glmnet)
library(randomForest)
set.seed(24101107)

df2 = readxl::read_xlsx("book1.xlsx")
df2 %<>% select(SUBJECTID, FACILITYID, PHI_DOB_MIN, KEVAL_WGT, KEVAL_WGT_UNITS, KEVAL_HGT, KEVAL_HGT_UNITS,
               KHL_ALCOHOL_HX, KHL_ILLDRUG_HX, KHL_TOBACCO_HX, KHL_COMO_CHRPAIN, KOP_DODIS_MIN)
sum(df2$KOP_DODIS_MIN > 100, na.rm = TRUE)

#data cleanup and setup
df = readxl::read_xlsx("book1.xlsx")

#removing 98 patients with no data, outliers, and those who do not have a subject ID
df = df[99:9049, ]
df %<>% filter(!is.na(SUBJECTID))
df %<>% filter(!(KOP_DODIS_MIN > 100))
df %<>% select(SUBJECTID, FACILITYID, PHI_DOB_MIN, KEVAL_WGT, KEVAL_WGT_UNITS, KEVAL_HGT, KEVAL_HGT_UNITS,
                                 KHL_ALCOHOL_HX, KHL_ILLDRUG_HX, KHL_TOBACCO_HX, KHL_COMO_CHRPAIN, KOP_DODIS_MIN)

#changing the weights and heights to one consistent unit 
for(i in 1:8713){
  if(df$KEVAL_WGT_UNITS[i] == 1 & !is.na(df$KEVAL_WGT_UNITS[i])){
    df$KEVAL_WGT[i] = df$KEVAL_WGT[i]/2.2
  }
}
df$KEVAL_WGT_UNITS = NULL
for(i in 1:8713){
  if(df$KEVAL_HGT_UNITS[i] == 1 & !is.na(df$KEVAL_HGT_UNITS[i])){
    df$KEVAL_HGT[i] = df$KEVAL_HGT[i]*2.54
  }
}
df$KEVAL_HGT_UNITS = NULL
#changing 998s to NA
df$KHL_ILLDRUG_HX = ifelse(df$KHL_ILLDRUG_HX == 998, NA, df$KHL_ILLDRUG_HX)
df$KHL_ALCOHOL_HX = ifelse(df$KHL_ALCOHOL_HX == 998, NA, df$KHL_ALCOHOL_HX)
df$KHL_TOBACCO_HX = ifelse(df$KHL_TOBACCO_HX == 998, NA, df$KHL_TOBACCO_HX)
df$KHL_COMO_CHRPAIN = ifelse(df$KHL_COMO_CHRPAIN == 998, NA, df$KHL_COMO_CHRPAIN)

#changing the date of birth variable to age and removing DOB

df %<>% mutate(PHI_DOB_MIN, Age = PHI_DOB_MIN/(-365))

colnames(df) <- c("SubjectID", "FacilityID", "DateofBirth", "Weight", "Height",
                  "HOfAlcohol", "HOfDrugs", "HOfTob", "ChrPain", "Discharge", "Age")

df$DateofBirth = NULL

mean(df$Weight, na.rm = TRUE)
sd(df$Weight, na.rm = TRUE)
mean(df$Height, na.rm = TRUE)
sd(df$Height, na.rm = TRUE)
mean(df$Age, na.rm = TRUE)
sd(df$Age, na.rm = TRUE)
mean(df$Discharge, na.rm = TRUE)
sd(df$Discharge, na.rm = TRUE)

df$FacilityID %>% table() %>% prop.table()
df$HOfAlcohol %>% table() %>% prop.table()
df$HOfDrugs %>% table() %>% prop.table()
df$HOfTob %>% table() %>% prop.table()
df$ChrPain %>% table() %>% prop.table()
#changing the categorical variables to factors and setting up testing vs training data

df$FacilityID = factor(df$FacilityID, ordered = FALSE)
df$HOfAlcohol = factor(df$HOfAlcohol, ordered = FALSE)
df$HOfDrugs = factor(df$HOfDrugs, ordered = FALSE)
df$HOfTob = factor(df$HOfTob, ordered = FALSE)
df$ChrPain = factor(df$ChrPain, ordered = TRUE)
df$ChrPain = fct_rev(df$ChrPain)

selectionvector = sample.int(8713, 1000)
testvector = c()
for(i in 1:8713){
  if(!i %in% selectionvector){
    testvector = c(testvector, i)
  }
}
selectionvector = sort(selectionvector)
trainingdata = df[selectionvector, ]
testdata = df[testvector,]

#missing data plots
plot_pattern(df, rotate = TRUE)
plot_pattern(trainingdata, rotate = TRUE)

#Imputation and log-linear model

class = mice(data=trainingdata , m=10, method=c("","", "pmm", "pmm", "logreg", "logreg", "polyreg", "polyreg", "pmm","polr"), maxit = 10, print=FALSE)

ncds_mi_mod <- with(data=class, exp=glm(log(Discharge) ~ Weight + Height + HOfAlcohol + HOfTob + HOfDrugs + ChrPain + Age, family="gaussian"))
pool(ncds_mi_mod)
summary(pool(ncds_mi_mod))
complete = complete(class)
pooled_lm = ncds_mi_mod$analyses[[1]]

#Log Linear predictions

predictLL = exp(predict(pooled_lm, trainingdata = testdata))
predictLL = ifelse(is.na(predictLL), mean(complete$Discharge), predictLL)

sum(is.na(predictLL))
summary(predictLL)
MSELLPred = c()
for(i in 1:7713){
  MSELLPred[i] = (predictLL[i] - testdata$Discharge[i])^2
}
MSELL = mean(MSELLPred, na.rm = TRUE)
sqrt(MSELL)

#Tree regressions

tree = tree(data = complete %>% select(FacilityID, Weight, Height, HOfAlcohol, HOfTob, HOfDrugs, ChrPain, Age, Discharge), formula = Discharge ~ .)
plot(tree)
text(tree, pretty = 0)

forest = randomForest(formula = Discharge ~ ., data = complete, ntree=1000)
#Tree predictions
predTree = predict(tree, newdata = testdata)
predForest = predict(forest, newdata = testdata)
MSETree = c()
for(i in 1:7713){
  MSETree[i] = (predTree[i] - testdata$Discharge[i])^2
}
MeanSquareTree = mean(MSETree, na.rm = TRUE)
sqrt(MeanSquareTree)
MSEForest = c()
for(i in 1:7713){
  MSEForest[i] = (predForest[i] - testdata$Discharge[i])^2
}
MeanSquareForest = mean(MSEForest, na.rm = TRUE)
summary(predForest)
summary(predTree)
print(forest)
plot(forest, type = "l")
plot(getTree(forest))
#Lasso regression

x = model.matrix(Discharge ~ FacilityID + Weight + Height + HOfAlcohol + HOfTob + HOfDrugs + ChrPain + Age, complete)
y = complete$Discharge

lasso = glmnet(x, y, alpha = 0)
cv = cv.glmnet(x, y, alpha = 0)

lambda = cv$lambda.min

completecases = complete.cases(testdata)

newx = model.matrix(Discharge ~ FacilityID + Weight + Height + HOfAlcohol + HOfTob + HOfDrugs + ChrPain + Age, testdata)
newy = testdata$Discharge
LassoPredictions = predict(cv, s = lambda,newx = newx, type = "response")

tempy = newy
for(i in 1:7713){
  if(!completecases[i]){
    tempy[i] = NA
  }
}
tempy <- tempy[!is.na(tempy)]
MSELasso = c()
for(i in 1:length(tempy)){
  MSELasso[i] = (LassoPredictions[i] - tempy[i])^2
}
MeanSquareLasso = mean(MSELasso, na.rm = TRUE)
sqrt(MeanSquareTree)

plot(cv)
plot(lasso, xvar = "lambda")

mean(predTree)
mean(predForest, na.rm = TRUE)
mean(LassoPredictions)
mean(predictLL)
