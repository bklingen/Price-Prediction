## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, warnings = FALSE, message = FALSE)
knitr::opts_chunk$set(tidy.opts=list(width.cutoff=70))


## ----libraries, include=FALSE-------------------------------------------------
library(dplyr)
library(tidyverse)


## -----------------------------------------------------------------------------
## Load Training Data
path_traindata <- 'https://raw.githubusercontent.com/bklingen/Price-Prediction/main/train.csv'
train <- read_csv(path_traindata)
dim(train)
## Load Testing Data
path_testdata <- 'https://raw.githubusercontent.com/bklingen/Price-Prediction/main/test.csv'
test <- read_csv(path_testdata)
dim(test)


## -----------------------------------------------------------------------------
setdiff(colnames(test), colnames(train))


## -----------------------------------------------------------------------------
setdiff(colnames(train), colnames(test))


## -----------------------------------------------------------------------------
train_quantPredictors = train %>% select(where(is.numeric)) %>% select(-SalePrice) 
train_catPredictors = train %>% select(where(is.character))
dim(train_quantPredictors)
dim(train_catPredictors)


## -----------------------------------------------------------------------------
test_quantPredictors = test %>% select(where(is.numeric))
test_catPredictors = test %>% select(where(is.character))


## -----------------------------------------------------------------------------
train_catPredictors = train_catPredictors %>% transmute_all(as.factor)
test_catPredictors = test_catPredictors %>% transmute_all(as.factor)


## ----eval=FALSE---------------------------------------------------------------
## for(i in 1:ncol(train_catPredictors)) {
##   print(colnames(train_catPredictors)[i])
##   print("----")
##   print(as.data.frame(fct_count(unlist(train_catPredictors[,i]))))
##   print("--------------")
## }


## -----------------------------------------------------------------------------
sum(is.na(train$MSZoning))
sum(is.na(test$MSZoning))


## -----------------------------------------------------------------------------
fct_count(train$MSZoning)
fct_count(test$MSZoning)

## ----warning=FALSE------------------------------------------------------------
mszoning.collapse <- function(x) factor(x,
                                     levels=c("FV", "RL", "RP", "RM", "RH"),
                                     labels=c("FV", "RL", "RL", "RO", "RO")) %>%
                                
                  fct_explicit_na(., "other")

train <- train %>% mutate(MSZoning = as.factor(MSZoning), MSZoning = mszoning.collapse(MSZoning))
test <- test %>% mutate(MSZoning = as.factor(MSZoning), MSZoning = mszoning.collapse(MSZoning))

## -----------------------------------------------------------------------------
fct_count(train$MSZoning)


## -----------------------------------------------------------------------------
sum(is.na(train$MSSubClass))
sum(is.na(test$MSSubClass))


## -----------------------------------------------------------------------------
mssubclass.collapse <- function(x) factor(x,
                                     levels=c("20", "30", "40", "45", "50", "60", "70", "75", "80", "85"),
                                     labels=c("1S SF 1946 & newer", "1S SF other", "1S SF other", "1S SF other", "1S SF other", "multi-level SF non PUD", "multi-level SF non PUD", "multi-level SF non PUD", "multi-level SF non PUD", "multi-level SF non PUD")) %>%
  fct_explicit_na(., "other")

train <- train %>% mutate(MSSubClass = as.factor(MSSubClass), MSSubClass = mssubclass.collapse(MSSubClass))
test <- test %>% mutate(MSSubClass = as.factor(MSSubClass), MSSubClass = mssubclass.collapse(MSSubClass))

## -----------------------------------------------------------------------------
fct_count(train$MSSubClass)


## -----------------------------------------------------------------------------
sum(is.na(train$Condition1))
sum(is.na(test$Condition1))
sum(is.na(train$Condition2))
sum(is.na(test$Condition2))


## ----warning=FALSE------------------------------------------------------------
condition.collapse <- function(x) factor(x, 
                                         levels=c("RRNn", "RRAn", "RRNe", "RRAe", "PosN", "PosA", "Artery", "Feedr", "Norm"), 
                                         labels=c('RR', 'RR', 'RR', 'RR', 'Pos', 'Pos', 'St', 'St', "Norm"))

train <- train %>% mutate_at(vars(Condition1, Condition2), condition.collapse)
test <- test %>% mutate_at(vars(Condition1, Condition2), condition.collapse)

## -----------------------------------------------------------------------------
fct_count(train$Condition1)


## -----------------------------------------------------------------------------
roof_price <- train %>% group_by(RoofStyle) %>% summarize(count=n(),
  mean(SalePrice), sd(SalePrice))

roof_price


## -----------------------------------------------------------------------------
train$RoofStyle <- fct_collapse(train$RoofStyle, Other = c("Flat", "Shed"))
train$RoofStyle <- fct_collapse(train$RoofStyle, Gable = c("Gable", "Gambrel", "Mansard"))


## -----------------------------------------------------------------------------
test$RoofStyle <- fct_collapse(test$RoofStyle, Other = c("Flat", "Shed"))
test$RoofStyle <- fct_collapse(test$RoofStyle, Gable = c("Gable", "Gambrel", "Mansard"))


## -----------------------------------------------------------------------------
bldg_price <- train %>% group_by(BldgType) %>% summarize(count=n(),
  mean(SalePrice), sd(SalePrice))

bldg_price


## -----------------------------------------------------------------------------
train$BldgType <- fct_collapse(train$BldgType, MultiFam = c("2fmCon", "Duplex"))


## -----------------------------------------------------------------------------
test$BldgType <- fct_collapse(test$BldgType, MultiFam = c("2fmCon", "Duplex"))


## -----------------------------------------------------------------------------
style_price <- train %>% group_by(HouseStyle) %>% summarize(count=n(),
  mean(SalePrice), sd(SalePrice))

style_price 


## -----------------------------------------------------------------------------
train$HouseStyle <- fct_collapse(train$HouseStyle, Less2story = c("1Story", "1.5Fin", "SFoyer", "SLvl"))
train$HouseStyle <- fct_collapse(train$HouseStyle, EqMore2story = c("2Story", "2.5Fin"))


## -----------------------------------------------------------------------------
test$HouseStyle <- fct_collapse(test$HouseStyle, Less2story = c("1Story", "1.5Fin", "SFoyer", "SLvl"))
test$HouseStyle <- fct_collapse(test$HouseStyle, EqMore2story = c("2Story", "2.5Fin"))


## -----------------------------------------------------------------------------
poolqc.collapse <- function(x) factor(x,
                                     levels=c("Ex", "Gd", "TA", "Fa"),
                                     labels=c("Ex", "Gd", "TA", "Fa")) %>%
  fct_explicit_na(., "Other")

train <- train %>% mutate(PoolQC = as.factor(PoolQC), PoolQC = poolqc.collapse(PoolQC))
test <- test %>% mutate(PoolQC = as.factor(PoolQC), PoolQC = poolqc.collapse(PoolQC))


## -----------------------------------------------------------------------------
cleanfence <- as.character(train_catPredictors$Fence)
cleanfence[is.na(cleanfence)] <- "none"
cleanfence <- as.factor(cleanfence)


## -----------------------------------------------------------------------------
cleanfunc <- as.character(train_catPredictors$Functional)
cleanfunc[cleanfunc == 'Min1' | cleanfunc == 'Min2'] <- "Minor"
cleanfunc[cleanfunc == 'Maj1' | cleanfunc == 'Maj2'] <- "Major"
cleanfunc[cleanfunc == 'Sev' | cleanfunc == 'Sal'] <- "Severe"
cleanfunc <- as.factor(cleanfunc)


## -----------------------------------------------------------------------------
train$Fence <- cleanfence
train$Functional <- cleanfunc


## -----------------------------------------------------------------------------
cleanfence <- as.character(test_catPredictors$Fence)
cleanfence[is.na(cleanfence)] <- "none"
cleanfence <- as.factor(cleanfence)


## -----------------------------------------------------------------------------
cleanfunc <- as.character(test_catPredictors$Functional)
cleanfunc[cleanfunc == 'Min1' | cleanfunc == 'Min2'] <- "Minor"
cleanfunc[cleanfunc == 'Maj1' | cleanfunc == 'Maj2'] <- "Major"
cleanfunc[cleanfunc == 'Sev' | cleanfunc == 'Sal'] <- "Severe"
cleanfunc <- as.factor(cleanfunc)


## -----------------------------------------------------------------------------
test$Fence <- cleanfence
test$Functional <- cleanfunc


## -----------------------------------------------------------------------------
# Heating: Collapsed categores with low frequencies into "other"
heating <- as.factor(train_catPredictors$Heating)
heating <- fct_other(heating, keep=c("GasA", "GasW"))
train$Heating <- heating


## -----------------------------------------------------------------------------
# Electrical: Collapsed similar categories together and handled missing values
electrical.collapse <- function(x) factor(x,
                                     levels=c("FuseA", "FuseF", "FuseP"),
                                     labels=c("Fuse", "Fuse", "Fuse")) %>%
  fct_explicit_na(., "Other")

train <- train %>% mutate(Electrical = as.factor(Electrical), Electrical = electrical.collapse(Electrical))
test <- test %>% mutate(Electrical = as.factor(Electrical), Electrical = electrical.collapse(Electrical))


## -----------------------------------------------------------------------------
# Fireplace: Handled missing values
fireplace <- as.character(train_catPredictors$FireplaceQu)
fireplace[is.na(fireplace)] <- "none"
train$FireplaceQu <- as.factor(fireplace)


## -----------------------------------------------------------------------------
# Heating: Collapsed categores with low frequencies into "other"
heating <- as.factor(test_catPredictors$Heating)
heating <- fct_other(heating, keep=c("GasA", "GasW"))
test$Heating <- heating


## -----------------------------------------------------------------------------
# Fireplace: Handled missing values
fireplace <- as.character(test_catPredictors$FireplaceQu)
fireplace[is.na(fireplace)] <- "none"
test$FireplaceQu <- as.factor(fireplace)


## -----------------------------------------------------------------------------
train <- select(train, -c(RoofMatl))
test <- select(test, -c(RoofMatl))


## -----------------------------------------------------------------------------
train$Exterior2nd[train$Exterior2nd=='Wd Shng'] <- 'WdShing'
train$Exterior2nd[train$Exterior2nd=='CmentBd'] <- 'CemntBd'
train$Exterior2nd[train$Exterior2nd=='Brk Cmn'] <- 'BrkComm'
train$Exterior2nd <- train$Exterior1st!=train$Exterior2nd
train$Exterior1st <- fct_collapse(train$Exterior1st, Other = c("AsbShng","AsphShn","CBlock","ImStucc","BrkComm","Stone","Stucco","WdShing"))

test$Exterior2nd[test$Exterior2nd=='Wd Shng']<- 'WdShing'
test$Exterior2nd[test$Exterior2nd=='CmentBd']<- 'CemntBd'
test$Exterior2nd[test$Exterior2nd=='Brk Cmn']<- 'BrkComm'
test$Exterior2nd <- test$Exterior1st!=test$Exterior2nd
test$Exterior1st <- fct_collapse(test$Exterior1st, Other = c("AsbShng","AsphShn","CBlock","ImStucc","BrkComm","Stone","Stucco","WdShing"))


## -----------------------------------------------------------------------------
table(train$ExterCond)
table(test$ExterCond)


## -----------------------------------------------------------------------------
train$ExterCond = fct_collapse(train$ExterCond, other=c("Ex", "Po"))
test$ExterCond = fct_collapse(test$ExterCond, other=c("Ex", "Po"))

summary(train$ExterCond)
summary(test$ExterCond)


## -----------------------------------------------------------------------------
train$SaleType <- fct_collapse(train$SaleType, Other = c("ConLD", "ConLw", "ConLI", "CWD", "Oth", "Con"))
test$SaleType <- fct_collapse(test$SaleType, Other = c("ConLD", "ConLw", "ConLI", "CWD", "Oth", "Con"))


## -----------------------------------------------------------------------------
### Neighborhood ###
# Collapse categores with low frequencies into "other"

#Explore counts
train_catPredictors %>% count(Neighborhood, sort = TRUE)


## -----------------------------------------------------------------------------
#Factorize
neighborhood <- as.factor(train_catPredictors$Neighborhood)

#Convert to "Other" any category that represents less than 2% of the data
neighborhood <- fct_collapse(neighborhood, Other = c("MeadowV","BrDale", "Veenker", "NPkVill", "Blueste", "IDOTRR", "ClearCr", "StoneBr" ,"SWISU", "Blmngtn"))

levels(neighborhood) #New levels of the factor


## -----------------------------------------------------------------------------
#Update column with new values
train$Neighborhood <- neighborhood


## -----------------------------------------------------------------------------
#Factorize
neighborhood <- as.factor(test_catPredictors$Neighborhood)

#Convert to "Other" any category that represents less than 2% of the data
neighborhood <- fct_collapse(neighborhood, Other = c("MeadowV","BrDale", "Veenker", "NPkVill", "Blueste", "IDOTRR", "ClearCr", "StoneBr" ,"SWISU", "Blmngtn"))
levels(neighborhood) #New levels of the factor


## -----------------------------------------------------------------------------
#Update column with new values
test$Neighborhood <- neighborhood


## -----------------------------------------------------------------------------
table(train$Neighborhood)
table(test$Neighborhood)


## -----------------------------------------------------------------------------
### GarageType ###

#Explore counts
train_catPredictors %>% count(GarageType, sort = TRUE)


## -----------------------------------------------------------------------------
#Handle NAs
#According to the data description, NA means no garage. 
#Change NA category to "none" to avoid issues.
garageType <- as.character(train_catPredictors$GarageType)
garageType[is.na(garageType)] <- "none"
garageType <- as.factor(garageType)


## -----------------------------------------------------------------------------
#Collapse into "Other" categries that represent less than 5% of the data
garageType <- garageType %>% 
  fct_lump(prop=0.05, other_level='Other')

#levels(garageType) #New levels of the factor


## -----------------------------------------------------------------------------
#Update column with new values
train$GarageType <- garageType


## -----------------------------------------------------------------------------
garageType <- as.character(test$GarageType)
garageType[is.na(garageType)] <- "none"
garageType <- as.factor(garageType)
garageType <- garageType %>% 
  fct_lump(prop=0.05, other_level='Other')
levels(garageType)
levels(train$GarageType)
test$GarageType <- garageType


## -----------------------------------------------------------------------------
### GarageFinish ###

#Explore counts
train_catPredictors %>% count(GarageFinish, sort = TRUE)

## -----------------------------------------------------------------------------
#Handle NAs
#According to the data description, NA means no garage. 
#Change NA category to "none" to avoid issues.
garageFinish <- as.character(train_catPredictors$GarageFinish)
garageFinish[is.na(garageFinish)] <- "none"
garageFinish <- as.factor(garageFinish)

#No need to collapse categories


## -----------------------------------------------------------------------------
#Update column with new values
train$GarageFinish <- garageFinish


## -----------------------------------------------------------------------------
#Handle NAs
#According to the data description, NA means no garage. 
#Change NA category to "none" to avoid issues.
garageFinish <- as.character(test_catPredictors$GarageFinish)
garageFinish[is.na(garageFinish)] <- "none"
garageFinish <- as.factor(garageFinish)
#No need to collapse categories


## -----------------------------------------------------------------------------
#Update column with new values
test$GarageFinish <- garageFinish


## -----------------------------------------------------------------------------
garagequal.collapse <- function(x) factor(x,
                                     levels=c("Ex", "Gd", "TA", "Fa", "Po"),
                                     labels=c("Gd", "Gd", "TA", "Po", "Po")) %>%
  fct_explicit_na(., "none")

train <- train %>% mutate(GarageQual = as.factor(GarageQual), GarageQual = garagequal.collapse(GarageQual))
test <- test %>% mutate(GarageQual = as.factor(GarageQual), GarageQual = garagequal.collapse(GarageQual))


## ----eval=FALSE, include=FALSE------------------------------------------------
## ### GarageQual ###
## 
## #Explore counts
## train_catPredictors %>% count(GarageQual, sort = TRUE)


## ----eval=FALSE, include=FALSE------------------------------------------------
## #Handle NAs
## #According to the data description, NA means no garage.
## #Change NA category to "none" to avoid issues.
## garageQual <- as.character(train_catPredictors$GarageQual)
## garageQual[is.na(garageQual)] <- "none"
## garageQual <- as.factor(garageQual)


## ----eval=FALSE, include=FALSE------------------------------------------------
## #Collapse categories:
## # - Let's collapse Ex	(Excellent) and Gd	(Good) into 1 category: Gd
## # - Let's collapse Fa	(Fair) and Po	(Poor) into 1 category: Po
## # - None and TA remains the same
## 
## garageQual <- fct_collapse(garageQual, Gd = c("Ex","Gd"))
## garageQual <- fct_collapse(garageQual, Po = c("Fa","Po"))


## ----eval=FALSE, include=FALSE------------------------------------------------
## #Update column with new values
## train$GarageQual <- garageQual


## ----eval=FALSE, include=FALSE------------------------------------------------
## #Handle NAs
## #According to the data description, NA means no garage.
## #Change NA category to "none" to avoid issues.
## garageQual <- as.character(test_catPredictors$GarageQual)
## garageQual[is.na(garageQual)] <- "none"
## garageQual <- as.factor(garageQual)


## ----eval=FALSE, include=FALSE------------------------------------------------
## #Collapse categories:
## # - Let's collapse Ex	(Excellent) and Gd	(Good) into 1 category: Gd
## # - Let's collapse Fa	(Fair) and Po	(Poor) into 1 category: Po
## # - None and TA remains the same
## 
## garageQual <- fct_collapse(garageQual, Gd = c("Ex","Gd"))
## garageQual <- fct_collapse(garageQual, Po = c("Fa","Po"))


## ----eval=FALSE, include=FALSE------------------------------------------------
## #Update column with new values
## test$GarageQual <- garageQual


## -----------------------------------------------------------------------------
### GarageCond ###

#Explore counts
train_catPredictors %>% count(GarageCond, sort = TRUE)


## -----------------------------------------------------------------------------
#Handle NAs
#According to the data description, NA means no garage. 
#Change NA category to "none" to avoid issues.
garageCond <- as.character(train_catPredictors$GarageCond)
garageCond[is.na(garageCond)] <- "none"
garageCond <- as.factor(garageCond)


## -----------------------------------------------------------------------------
#Collapse categories: 
# - Let's collapse Ex	(Excellent) and Gd	(Good) into 1 category: Gd
# - Let's collapse Fa	(Fair) and Po	(Poor) into 1 category: Po
# - None and TA remains the same

garageCond <- fct_collapse(garageCond, Gd = c("Ex","Gd"))
garageCond <- fct_collapse(garageCond, Po = c("Fa","Po"))



## -----------------------------------------------------------------------------
#Update column with new values
train$GarageCond <- garageCond


## -----------------------------------------------------------------------------
#Handle NAs
#According to the data description, NA means no garage. 
#Change NA category to "none" to avoid issues.
garageCond <- as.character(test_catPredictors$GarageCond)
garageCond[is.na(garageCond)] <- "none"
garageCond <- as.factor(garageCond)


## -----------------------------------------------------------------------------
#Collapse categories: 
# - Let's collapse Ex	(Excellent) and Gd	(Good) into 1 category: Gd
# - Let's collapse Fa	(Fair) and Po	(Poor) into 1 category: Po
# - None and TA remains the same

garageCond <- fct_collapse(garageCond, Gd = c("Ex","Gd"))
garageCond <- fct_collapse(garageCond, Po = c("Fa","Po"))



## -----------------------------------------------------------------------------
#Update column with new values
test$GarageCond <- garageCond


## -----------------------------------------------------------------------------
sum(is.na(train$LotShape))
sum(is.na(test$LotShape))
sum(is.na(train$LotConfig))
sum(is.na(test$LotConfig))
sum(is.na(train$LandContour))
sum(is.na(test$LandContour))


## -----------------------------------------------------------------------------
fct_count(train$LotShape)
fct_count(test$LotShape)
fct_count(train$LotConfig)
fct_count(test$LotConfig)
fct_count(train$LandContour)
fct_count(test$LandContour)


## -----------------------------------------------------------------------------
train$LotShape <- fct_collapse(train$LotShape, Irregular = c("IR1", "IR2", "IR3"))
train$LotConfig <- fct_collapse(train$LotConfig, Other = c("Corner","CulDSac", "FR2", "FR3"))
train$LandContour <- fct_collapse(train$LandContour, NonLvl = c("Bnk", "HLS", "Low"))


## -----------------------------------------------------------------------------
fct_count(train$LotShape)
fct_count(train$LotConfig)
fct_count(train$LandContour)


## -----------------------------------------------------------------------------
test$LotShape <- fct_collapse(test$LotShape, Irregular = c("IR1", "IR2", "IR3"))
test$LotConfig <- fct_collapse(test$LotConfig, Other = c("Corner","CulDSac", "FR2", "FR3"))
test$LandContour <- fct_collapse(test$LandContour, NonLvl = c("Bnk", "HLS", "Low"))


## -----------------------------------------------------------------------------
train %>%
  summarize(across(everything(), ~sum(is.na(.x)))) %>%
  sort(decreasing = TRUE)


## -----------------------------------------------------------------------------
misc.collapse <- function(x) factor(x,
                                     levels=c("Elev", "Gar2", "Othr", "Shed", "TenC"),
                                     labels=c("Elev", "Gar2", "Other", "Shed", "TenC")) %>%
  fct_explicit_na(., "None")

train <- train %>% mutate(MiscFeature = as.factor(MiscFeature), MiscFeature = misc.collapse(MiscFeature))
test <- test %>% mutate(MiscFeature = as.factor(MiscFeature), MiscFeature = misc.collapse(MiscFeature))
       
       
train$MiscFeature = fct_explicit_na(train$MiscFeature, na_level="none")
test$MiscFeature = fct_explicit_na(test$MiscFeature, na_level="none")

train$Alley = fct_explicit_na(train$Alley, na_level="none")
test$Alley = fct_explicit_na(test$Alley, na_level="none")


## -----------------------------------------------------------------------------
train = train %>% select(-LotFrontage, -GarageYrBlt)
test = test %>% select(-LotFrontage, -GarageYrBlt)


## -----------------------------------------------------------------------------
train$BsmtQual = fct_explicit_na(train$BsmtQual, na_level="none")
train$BsmtCond = fct_explicit_na(train$BsmtCond, na_level="none")
train$BsmtExposure = fct_explicit_na(train$BsmtExposure, na_level="none")
train$BsmtFinType1 = fct_explicit_na(train$BsmtFinType1, na_level="none")
train$BsmtFinType2 = fct_explicit_na(train$BsmtFinType2, na_level="none")

test$BsmtQual = fct_explicit_na(test$BsmtQual, na_level="none")
test$BsmtCond = fct_explicit_na(test$BsmtCond, na_level="none")
test$BsmtExposure = fct_explicit_na(test$BsmtExposure, na_level="none")
test$BsmtFinType1 = fct_explicit_na(test$BsmtFinType1, na_level="none")
test$BsmtFinType2 = fct_explicit_na(test$BsmtFinType2, na_level="none")

train %>%
  summarize(across(everything(), ~sum(is.na(.x)))) %>%
  sort(decreasing = TRUE)


## -----------------------------------------------------------------------------
summary(train$MasVnrType)
train$MasVnrType = fct_explicit_na(train$MasVnrType, na_level="missing")
train$MasVnrArea[is.na(train$MasVnrArea)] = 0

test$MasVnrType = fct_explicit_na(test$MasVnrType, na_level="missing")
test$MasVnrArea[is.na(test$MasVnrArea)] = 0


## -----------------------------------------------------------------------------
dim(train)
dim(train %>% drop_na())


## -----------------------------------------------------------------------------
train = train %>% mutate(across(where(is.character), as.factor))
test = test %>% mutate(across(where(is.character), as.factor))


## -----------------------------------------------------------------------------
train %>%
  summarize(across(everything(), ~length(unique(.x)))) %>%
  sort()


## -----------------------------------------------------------------------------
fit2 = lm(log(train$SalePrice) ~ . , data=train[,2:9])
summary(fit2)


## -----------------------------------------------------------------------------
summary(train$Utilities)


## -----------------------------------------------------------------------------
train[train$Utilities == 'NoSeWa',]


## -----------------------------------------------------------------------------
train %>%
  summarize(across(everything(), ~length(unique(.x)))) %>%
  sort()


## -----------------------------------------------------------------------------
summary(train$Utilities)


## -----------------------------------------------------------------------------
train =  train %>% select(-Utilities)
test =  test %>% select(-Utilities)


## -----------------------------------------------------------------------------
isNAtest = apply(test,1,function(x) any(is.na(x)))
sum(isNAtest)


## -----------------------------------------------------------------------------
test %>%
  summarize(across(everything(), ~sum(is.na(.x)))) %>%
  sort(decreasing = TRUE)


## -----------------------------------------------------------------------------
summary(train$MSZoning)
summary(test$MSZoning)
test$MSZoning = fct_explicit_na(test$MSZoning, na_level="other")
summary(test$MSZoning)


## -----------------------------------------------------------------------------
summary(train$BsmtFullBath)
summary(test$BsmtFullBath)
test$BsmtFullBath[is.na(test$BsmtFullBath)] = 0


## -----------------------------------------------------------------------------
summary(train$BsmtHalfBath)
summary(test$BsmtHalfBath)
test$BsmtHalfBath[is.na(test$BsmtHalfBath)] = 0


## -----------------------------------------------------------------------------
summary(train$Functional)
summary(test$Functional)
train$Functional = train$Functional == "Typ"
test$Functional = test$Functional == "Typ"
test$Functional[is.na(test$Functional)] = TRUE
summary(train$Functional)
summary(test$Functional)


## -----------------------------------------------------------------------------
summary(train$Exterior1st)
summary(test$Exterior1st)
test$Exterior1st <- fct_explicit_na(test$Exterior1st, na_level="Other")
summary(test$Exterior1st)


## -----------------------------------------------------------------------------
summary(train$Exterior2nd)
summary(test$Exterior2nd)
test$Exterior2nd[is.na(test$Exterior2nd)] = FALSE


## -----------------------------------------------------------------------------
summary(train$BsmtFinSF1)
summary(test$BsmtFinSF1)
test$BsmtFinSF1[is.na(test$BsmtFinSF1)] = 0


## -----------------------------------------------------------------------------
summary(train$BsmtFinSF2)
summary(test$BsmtFinSF2)
test$BsmtFinSF2[is.na(test$BsmtFinSF2)] = 0


## -----------------------------------------------------------------------------
summary(train$BsmtUnfSF)
summary(test$BsmtUnfSF)
test$BsmtUnfSF[is.na(test$BsmtUnfSF)] = 460


## -----------------------------------------------------------------------------
summary(train$TotalBsmtSF)
summary(test$TotalBsmtSF)
test$TotalBsmtSF[is.na(test$TotalBsmtSF)] = 988


## -----------------------------------------------------------------------------
summary(train$KitchenQual)
summary(test$KitchenQual)
test$KitchenQual[is.na(test$KitchenQual)] = "TA"


## -----------------------------------------------------------------------------
summary(train$GarageCars)
summary(test$GarageCars)
test$GarageCars[is.na(test$GarageCars)] = 1.766


## -----------------------------------------------------------------------------
summary(train$GarageArea)
summary(test$GarageArea)
test$GarageArea[is.na(test$GarageArea)] = 480


## -----------------------------------------------------------------------------
summary(train$SaleType)
summary(test$SaleType)
test$SaleType[is.na(test$SaleType)] = "Other"

