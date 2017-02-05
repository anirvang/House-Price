setwd("/home/home/Documents/003_Kaggle_Projects/004_HousePrice-ART/")
source("code/02_R_Codes/00_Regression_Functions.R")

core_raw_data <- read.csv("input/train.csv", stringsAsFactors = F)
test_raw <- read.csv("input/test.csv", stringsAsFactors = F)
par(mfrow = c(2,1))
hist(core_raw_data$SalePrice, main = 'Histogram of SalePrice')
core_raw_data$MSSubClass <- as.character(core_raw_data$MSSubClass)
core_raw_data$SalePrice <- log(core_raw_data$SalePrice)
hist(core_raw_data$SalePrice, main = 'Histogram of SalePrice after transformation')
test_raw$MSSubClass <- as.character(test_raw$MSSubClass)
str(core_raw_data)
testdata <- test_raw[, -which(colnames(core_raw_data) %in% c('Id'))]

library(randomForest)
library(car)
library(dummies)
library(MASS)


smp_size <- floor(0.75 * nrow(core_raw_data))
set.seed(123)
train_ind <- sample(seq_len(nrow(core_raw_data)), size = smp_size)

core_train_data <- core_raw_data[train_ind, -which(colnames(core_raw_data) %in% c('Id'))]
core_test_data <- core_raw_data[-train_ind, -which(colnames(core_raw_data) %in% c('Id'))]



core_train_data <- missing_value_trt(core_train_data)
core_test_data <- missing_value_trt(core_test_data)
testdata <- missing_value_trt(testdata)
core_train_data$SalePrice <- as.numeric(core_train_data$SalePrice)
core_test_data$SalePrice <- as.numeric(core_test_data$SalePrice)

#---------------------End---------------------------------------#





core_train_data <- outlierTrtmt(core_train_data)
core_test_data <- outlierTrtmt(core_test_data)
testdata <- outlierTrtmt(testdata)

#-------------------End------------------------------------#



bivariate_plots(core_train_data, depvar="SalePrice", png_dir="plots/bivariate/")

#--------------------------------------End------------------------------------------#



core_train_data <- createDummy(df = core_train_data, dfname="core_train_data")
core_test_data <- createDummy(df = core_test_data, dfname = "core_test_data")
testdata <- createDummy(df= testdata, dfname = "testdata")
#--------------------------End------------------------------------------------------#

#-----------------------Creating Combined Dummies-----------------------------------#

ifelse(core_train_data$Alley %in% c("Missing", "Pave"), core_train_data$Alley_Missing_Pave <- 1, 0)
ifelse(core_train_data$BldgType %in% c("1Fam", "TwnhsE"), core_train_data$BldgType_1Fam_TwnhsE <- 1, 0)
ifelse(core_train_data$BsmtExposure %in% c("Av", "Mn"), core_train_data$BsmtExposure_Av_Mn <- 1, 0)
ifelse(core_train_data$BsmtFinType1 %in% c("ALQ", "BLQ", "LwQ", "Rec", "Unf"), core_train_data$BsmtFinType1_ALQ_BLQ_LwQ_Rec_Unf <- 1, 0)
ifelse(core_train_data$BsmtFinType2 %in% c("ALQ", "BLQ", "LwQ", "Rec", "Unf"), core_train_data$BsmtFinType2_ALQ_BLQ_LwQ_Rec_Unf <- 1, 0)
ifelse(core_train_data$BsmtFullBath %in% c("1", "2", "3"), core_train_data$BsmtFullBath_1_2_3 <- 1, 0)
ifelse(core_train_data$BsmtQual %in% c("Fa", "Gd", "Missing", "TA"), core_train_data$BsmtQual_Fa_Gd_Miss_Ta <- 1, 0)
ifelse(core_train_data$Condition2 %in% c("PosA", "PosN"), core_train_data$Condition2_PosA_PosN <- 1, 0)
ifelse(core_train_data$Electrical %in% c("SBrkr", "Missing"), core_train_data$Electrical_SBrkr_Miss <- 1, 0)
ifelse(core_train_data$ExterCond %in% c("Ex", "Gd", "TA"), core_train_data$ExterCond_Ex_Gd_TA <- 1, 0)
ifelse(core_train_data$ExterQual %in% c("Fa", "Gd", "TA"), core_train_data$ExterQual_Fa_Gd_TA <- 1, 0)
ifelse(core_train_data$Fence %in% c("GdPrv", "Missing"), core_train_data$Fence_GdPrv_Miss <- 1, 0)
ifelse(core_train_data$FireplaceQu %in% c("Fa", "Gd", "Missing", "Po", "TA"), core_train_data$FireplaceQu_Fa_Gd_Miss_Po_TA <- 1, 0)
ifelse(core_train_data$ExterCond %in% c("Ex", "Gd", "TA"), core_train_data$ExterCond_Ex_Gd_TA <- 1, 0)
ifelse(core_train_data$Fireplaces %in% c("2", "3"), core_train_data$Fireplaces_2_3 <- 1, 0)
ifelse(core_train_data$Foundation %in% c("BrkTil", "CBlock", "Slab", "Stone", "Wood"), core_train_data$Foundation_BrkTil_CBlock_Slab_Stone_Wood <- 1, 0)
ifelse(core_train_data$FullBath %in% c("1", "2"), core_train_data$FullBath_1_2 <- 1, 0)
ifelse(core_train_data$Functional %in% c("Maj1", "Min1", "Min2", "Mod", "Sev", "Typ"), core_train_data$Functional_Maj1_Min1_Min2_Mod_Sev <- 1, 0)
ifelse(core_train_data$GarageCars %in% c("2", "4"), core_train_data$GarageCars_2_4 <- 1, 0)
ifelse(core_train_data$GarageCond %in% c("Ex", "Fa", "Po"), core_train_data$GarageCond_Ex_Fa_Po <- 1, 0)
ifelse(core_train_data$GarageQual %in% c("Ex", "Fa", "Po"), core_train_data$GarageQual_Ex_Fa_Po <- 1, 0)
ifelse(core_train_data$GarageType %in% c("2Types", "Attchd", "Basment"), core_train_data$GarageType_2Types_Attchd_Basment <- 1, 0)
ifelse(core_train_data$GarageType %in% c("CarPort", "Detchd", "Missing"), core_train_data$GarageType_CarPort_Detchd_Miss <- 1, 0)
ifelse(core_train_data$HeatingQC %in% c("Fa", "Gd", "TA"), core_train_data$HeatingQC_Fa_Gd_TA <- 1, 0)
ifelse(core_train_data$LandSlope %in% c("Mod", "Sev"), core_train_data$LandSlope_Mod_Sev <- 1, 0)
ifelse(core_train_data$LotConfig %in% c("CulDSac", "FR3"), core_train_data$LotConfig_CulDSac_FR3 <- 1, 0)
ifelse(core_train_data$LotShape %in% c("IR2", "IR3"), core_train_data$LotShape_IR2_IR3 <- 1, 0)
ifelse(core_train_data$MiscFeature %in% c("Gar2", "Missing", "Shed"), core_train_data$MiscFeature_Gar2_Miss_Shed <- 1, 0)
ifelse(core_train_data$OverallCond %in% c("6", "7", "8"), core_train_data$OverallCond_6_7_8 <- 1, 0)
ifelse(core_train_data$OverallCond %in% c("5", "9"), core_train_data$OverallCond_5_9 <- 1, 0)
ifelse(core_train_data$OverallCond %in% c("2", "3"), core_train_data$OverallCond_2_3 <- 1, 0)
ifelse(core_train_data$PoolQC %in% c("Fa", "Gd", "Missing"), core_train_data$PoolQC_Fa_Gd_Miss <- 1, 0)
ifelse(core_train_data$RoofStyle %in% c("Flat", "Hip", "Mansard"), core_train_data$RoofStyle_Flat_Hip_Mansard <- 1, 0)
ifelse(core_train_data$SaleCondition %in% c("Abnorml", "Alloca", "Family", "Normal"), core_train_data$SaleCondition_Abnorml_Alloca_Family_Normal <- 1, 0)
ifelse(core_train_data$YrSold %in% c("2006", "2008", "2009"), core_train_data$YrSold_06_08_09 <- 1, 0)

#------------------------------------End--------------------------------------------#

core_train_data_num <- core_train_data[sapply(core_train_data, is.numeric)]
core_test_data_num <- core_test_data[sapply(core_test_data, is.numeric)]
testdata_num <- testdata[sapply(testdata, is.numeric)]

#-------------------------------Correlation-----------------------------------------#

correlation <- cor(core_train_data_num,core_train_data_num$SalePrice, method = 'pearson')
write.csv(correlation , file="correlation.csv")

#-----------------------------------End--------------------------------------------#

#---------------------------Random Forest---------------------------------------------#

rfmodel <- randomForest(SalePrice ~ ., data = core_train_data_num, ntree = 100)
summary(rfmodel)
rfvarimp <- data.frame(rfmodel$importance)
rfvarimp$variables <- rownames(rfvarimp)
rfvarimp <- rfvarimp[order(- rfvarimp$IncNodePurity), ]
write.csv(rfvarimp, file = 'rfvarimp.csv', row.names = FALSE)



vars <- c(
  'OverallQual'
 , 'GrLivArea'
 , 'GarageArea'
 , 'BsmtQual_Ex'
 , 'TotalBsmtSF'
 , 'BsmtFinSF1'
 , 'YearBuilt'
 , 'LotArea'
 , 'KitchenQual_Ex'
 , 'WoodDeckSF'
 , 'OverallCond'
 , 'BsmtExposure_Gd'
 , 'MSZoning_RM'
 , 'KitchenAbvGr'
 , 'LotShape_IR3'
 , 'Foundation_PConc'
 , 'SaleType_New'
 , 'HeatingQC_Ex'
 , 'Exterior2nd_BrkFace'
 , 'LandContour_Bnk'
 , 'Neighborhood_Edwards'
 , 'FireplaceQu_Gd'
 , 'Condition1_Feedr'
 , 'LandContour_HLS'
 , 'SaleCondition_Normal'
)


lm_model <- lm(core_train_data_num$SalePrice ~ .
               , data = core_train_data_num[, which(colnames(core_train_data_num) %in% vars)])

summary(lm_model)
vif(lm_model)


rfmodel2 <- randomForest(core_train_data_num$SalePrice ~ .
                         , data = core_train_data_num[, which(colnames(core_train_data_num) %in% vars)]
                         , ntree = 100, maxnodes = 10)




train_pred <- predict(lm_model, core_train_data_num)
test_pred <- predict(lm_model, core_test_data_num)
train_pred2 <- predict(rfmodel2, core_train_data_num)
test_pred2 <- predict(rfmodel2, core_test_data_num)





train_error_rate <- (1 - train_pred/core_train_data_num$SalePrice)*100
test_error_rate <- (1 - test_pred/core_test_data_num$SalePrice)*100
train_error_rate2 <- (1 - train_pred2/core_train_data_num$SalePrice)*100
test_error_rate2 <- (1 - test_pred2/core_test_data_num$SalePrice)*100



print(paste("Train Min Error rate: ", min(train_error_rate)
            , "Train Max Error rate: ", max(train_error_rate)
            , 'Train MAPE: ', mean(abs(train_error_rate))
            , sep = " <--> "))
print(paste("Test Min Error rate: ", min(test_error_rate)
            , "Test Max Error rate: ", max(test_error_rate)
            , 'Test MAPE: ', mean(abs(test_error_rate))
            , sep = " <--> "))
print(paste("Train Min Error rate: ", min(train_error_rate2)
            , "Train Max Error rate: ", max(train_error_rate2)
            , 'Train MAPE: ', mean(abs(train_error_rate2))
            , sep = " <--> "))
print(paste("Test Min Error rate: ", min(test_error_rate2)
            , "Test Max Error rate: ", max(test_error_rate2)
            , 'Test MAPE: ', mean(abs(test_error_rate2))
            , sep = " <--> "))


pred_value <- data.frame(predict(lm_model, testdata_num))
pred_value2 <- data.frame(predict(rfmodel2, testdata_num))
pred_value$Id <- test_raw$Id
pred_value2$Id <- test_raw$Id
pred_data <- data.frame(pred_value$Id)
pred_data2 <- data.frame(pred_value2$Id)
pred_data <- cbind(pred_data, pred_value$predict.lm_model..testdata_num.)
pred_data2 <- cbind(pred_data2, pred_value2$predict.rfmodel2..testdata_num.)
pred_data <- setNames(pred_data, c('Id', 'SalePrice'))
pred_data2 <- setNames(pred_data2, c('Id', 'SalePrice'))
pred_data$SalePrice <- exp(pred_data$SalePrice)
pred_data2$SalePrice <- exp(pred_data2$SalePrice)
write.csv(pred_data, file = "Pred_SalePrice_lm.csv", row.names = FALSE)
write.csv(pred_data2, file = "Pred_SalePrice_rf.csv", row.names = FALSE)
#-------------------------------------------------------------------------------------------------#

