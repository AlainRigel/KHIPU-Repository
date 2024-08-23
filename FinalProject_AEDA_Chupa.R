##### this is the code for the final project of the course AEDA
##### It was done by Alain Chupa Ballesteros and Birger Vedder-Stute 
rm(list = ls()) # delete all variables
# load packages
library(rpart)
library(dplyr)
library(ggplot2)
library(caret)
library(kableExtra)
library(HSAUR3)
library(MASS) 
library(gridExtra)
library(randomForest)
library(stats)
library(gplots)
library(cluster)
library(factoextra)
library(FactoMineR)
# load the data
setwd("C:/Users/Birge/OneDrive - Technische Universität Dortmund/Semester/Auslandsemester (8.)/Advanced Engineering Data Analysis/FinalProject/house-prices-advanced-regression-techniques")
getwd()
Data <- read.csv("train.csv", header = TRUE)
# general overview over the data
head(TrainData)
summary(TrainData)
# check for missing data
missing_values <- is.na(TrainData)
colSums(missing_values)
### prework on data:
# replace other "NA" values with different values
Data$Alley <- ifelse(is.na(Data$Alley), "NoAll", Data$Alley)
Data$BsmtQual <- ifelse(is.na(Data$BsmtQual), "NoBsmt", Data$BsmtQual)
Data$BsmtCond <- ifelse(is.na(Data$BsmtCond), "NoBsmt", Data$BsmtCond)
Data$BsmtExposure <- ifelse(is.na(Data$BsmtExposure), "NoBsmt", Data$BsmtExposure)
Data$BsmtFinType1 <- ifelse(is.na(Data$BsmtFinType1), "NoBsmt", Data$BsmtFinType1)
Data$BsmtFinType2 <- ifelse(is.na(Data$BsmtFinType2), "NoBsmt", Data$BsmtFinType2)
Data$FireplaceQu <- ifelse(is.na(Data$FireplaceQu), "NoFP", Data$FireplaceQu)
Data$GarageType <- ifelse(is.na(Data$GarageType), "NoGarage", Data$GarageType)
Data$GarageFinish <- ifelse(is.na(Data$GarageFinish), "NoGarage", Data$GarageFinish)
Data$GarageQual <- ifelse(is.na(Data$GarageQual), "NoGarage", Data$GarageQual)
Data$GarageCond <- ifelse(is.na(Data$GarageCond), "NoGarage", Data$GarageCond)
Data$PoolQC <- ifelse(is.na(Data$PoolQC), "NoPool", Data$PoolQC)
Data$Fence <- ifelse(is.na(Data$Fence), "NoFence", Data$Fence)
Data$MiscFeature <- ifelse(is.na(Data$MiscFeature), "NoFeat", Data$MiscFeature)
# replace with 0, maybe they need to be deleted instead
Data$LotFrontage <- ifelse(is.na(Data$LotFrontage), 0, Data$LotFrontage)
Data$GarageYrBlt <- ifelse(is.na(Data$GarageYrBlt), 0, Data$GarageYrBlt)
# replacing yes/no with 1/0 to turn binary
Data$CentralAir <- ifelse(Data$CentralAir == "N", 0, 1)
# now remove the rest which is really missing data, but check if that's the case, should delete 9 rows:
missing_values <- is.na(Data)
missing_rows <- which(rowSums(is.na(Data)) > 0)
Data <- Data[-missing_rows, ]
# deleting the ID column which is just a continuous number without value
Data <- Data[, -1]
## replace wordings for quality with a rising number, for example EX is 5 (excellent) till PO is 1 (poor)
# replacing the Ex-Gd-TA-Fa-Po rating
Data$BsmtQual <- ifelse(Data$BsmtQual == "Ex", 5,
                        ifelse(Data$BsmtQual == "Gd", 4,
                               ifelse(Data$BsmtQual == "TA", 3,
                                      ifelse(Data$BsmtQual == "Fa", 2,
                                             ifelse(Data$BsmtQual == "Po", 1, 0)))))
Data$ExterQual <- ifelse(Data$ExterQual == "Ex", 5,
                         ifelse(Data$ExterQual == "Gd", 4,
                                ifelse(Data$ExterQual == "TA", 3,
                                       ifelse(Data$ExterQual == "Fa", 2,
                                              ifelse(Data$ExterQual == "Po", 1, 0)))))
Data$ExterCond <- ifelse(Data$ExterCond == "Ex", 5,
                         ifelse(Data$ExterCond == "Gd", 4,
                                ifelse(Data$ExterCond == "TA", 3,
                                       ifelse(Data$ExterCond == "Fa", 2,
                                              ifelse(Data$ExterCond == "Po", 1, 0)))))
Data$BsmtCond <- ifelse(Data$BsmtCond == "Ex", 5,
                        ifelse(Data$BsmtCond == "Gd", 4,
                               ifelse(Data$BsmtCond == "TA", 3,
                                      ifelse(Data$BsmtCond == "Fa", 2,
                                             ifelse(Data$BsmtCond == "Po", 1, 0)))))
Data$HeatingQC <- ifelse(Data$HeatingQC == "Ex", 5,
                         ifelse(Data$HeatingQC == "Gd", 4,
                                ifelse(Data$HeatingQC == "TA", 3,
                                       ifelse(Data$HeatingQC == "Fa", 2,
                                              ifelse(Data$HeatingQC == "Po", 1, 0)))))
Data$KitchenQual <- ifelse(Data$KitchenQual == "Ex", 5,
                           ifelse(Data$KitchenQual == "Gd", 4,
                                  ifelse(Data$KitchenQual == "TA", 3,
                                         ifelse(Data$KitchenQual == "Fa", 2,
                                                ifelse(Data$KitchenQual == "Po", 1, 0)))))
Data$FireplaceQu <- ifelse(Data$FireplaceQu == "Ex", 5,
                           ifelse(Data$FireplaceQu == "Gd", 4,
                                  ifelse(Data$FireplaceQu == "TA", 3,
                                         ifelse(Data$FireplaceQu == "Fa", 2,
                                                ifelse(Data$FireplaceQu == "Po", 1, 0)))))
Data$GarageQual <- ifelse(Data$GarageQual == "Ex", 5,
                          ifelse(Data$GarageQual == "Gd", 4,
                                 ifelse(Data$GarageQual == "TA", 3,
                                        ifelse(Data$GarageQual == "Fa", 2,
                                               ifelse(Data$GarageQual == "Po", 1, 0)))))
Data$GarageCond <- ifelse(Data$GarageCond == "Ex", 5,
                          ifelse(Data$GarageCond == "Gd", 4,
                                 ifelse(Data$GarageCond == "TA", 3,
                                        ifelse(Data$GarageCond == "Fa", 2,
                                               ifelse(Data$GarageCond == "Po", 1, 0)))))
Data$PoolQC <- ifelse(Data$PoolQC == "Ex", 5,
                      ifelse(Data$PoolQC == "Gd", 4,
                             ifelse(Data$PoolQC == "TA", 3,
                                    ifelse(Data$PoolQC == "Fa", 2,
                                           ifelse(Data$PoolQC == "Po", 1, 0)))))
# and the rest of the ratings:
Data$BsmtExposure <- ifelse(Data$BsmtExposure == "Gd", 4,
                            ifelse(Data$BsmtExposure == "Av", 3,
                                   ifelse(Data$BsmtExposure == "Mn", 2,
                                          ifelse(Data$BsmtExposure == "No", 1, 0))))
Data$BsmtFinType1 <- ifelse(Data$BsmtFinType1 == "GLQ", 6,
                            ifelse(Data$BsmtFinType1 == "ALQ", 5,
                                   ifelse(Data$BsmtFinType1 == "BLQ", 4,
                                          ifelse(Data$BsmtFinType1 == "Rec", 3,
                                                 ifelse(Data$BsmtFinType1 == "LwQ", 2,
                                                        ifelse(Data$BsmtFinType1 == "Unf", 1, 0))))))
Data$BsmtFinType2 <- ifelse(Data$BsmtFinType2 == "GLQ", 6,
                            ifelse(Data$BsmtFinType2 == "ALQ", 5,
                                   ifelse(Data$BsmtFinType2 == "BLQ", 4,
                                          ifelse(Data$BsmtFinType2 == "Rec", 3,
                                                 ifelse(Data$BsmtFinType2 == "LwQ", 2,
                                                        ifelse(Data$BsmtFinType2 == "Unf", 1, 0))))))
Data$Functional <- ifelse(Data$Functional == "Typ", 7,
                          ifelse(Data$Functional == "Min1", 6,
                                 ifelse(Data$Functional == "Min2", 5,
                                        ifelse(Data$Functional == "Mod", 4,
                                               ifelse(Data$Functional == "Maj1", 3,
                                                      ifelse(Data$Functional == "Maj2", 2,
                                                             ifelse(Data$Functional == "Sev", 1, 0)))))))
Data$GarageFinish <- ifelse(Data$GarageFinish == "Fin", 3,
                            ifelse(Data$GarageFinish == "RFn", 2,
                                   ifelse(Data$GarageFinish == "Unf", 1, 0)))
Data$Fence <- ifelse(Data$Fence == "GdPrv", 4,
                     ifelse(Data$Fence == "MnPrv", 3,
                            ifelse(Data$Fence == "GdWo", 2,
                                   ifelse(Data$Fence == "MnWw", 1, 0))))
## One-Hot-Encoding of complete data:
# remove some categorical data, since it gets too big with one-hot-endcoding (maybe)
# Perform one-hot encoding for non-numeric variables
encoded_data <- model.matrix(~ ., data = Data)
encoded_data <- encoded_data[, -1] # Remove the first column of the encoded data to avoid collinearity
### unsupervised analysis of the data:
## Clustering
# just with numeric data:
# scaling:
TraDatNumSca <- scale(encoded_data)
ClustTDNS <- dist(TraDatNumSca, method = "euclidean")
agnes.ward <- agnes(ClustTDNS, method = "ward")
pltree(agnes.ward, cex = 0.6, hang = -1, main = "Dendrogram")
(clust <- cutree(agnes.ward, k = 5))
fviz_cluster(list(data = ClustTDNS, cluster = clust), geom = "point")
# Analysis of Cluster-data
table(clust)
# Initialize empty vectors for each number
position_vectors <- vector("list", length = max(clust))
# Loop through the matrix to find positions of each number
for (i in 1:length(clust)) {
  number <- clust[i]
  position_vectors[[number]] <- c(position_vectors[[number]], i)}
Data_split <- list()
Data_split_num <- list()
for (i in 1:length(position_vectors)) {
  Data_split[[i]] <- Data[position_vectors[[i]], ]
numerical_columns <- sapply(Data_split[[i]], is.numeric)
Data_split_num[[i]] <- Data_split[[i]][,numerical_columns]
}
summary(Data_split_num[[1]])
summary(Data_split_num[[2]])
summary(Data_split_num[[3]])
summary(Data_split_num[[4]])
summary(Data_split_num[[5]])
## PCA
# just use the numerical data
numerical_columns <- sapply(Data, is.numeric)
Data_num <- Data[,numerical_columns]
# do the PCA
pc<-princomp(Data_num,cor=TRUE) 
summary(pc,loadings=TRUE)
# plot of cummulative and single Partion in variance
pr.var <- pc$sdev^2
pve <- pr.var/sum(pr.var)
par(mfrow = c(1, 2))
plot(pve, xlab="Principal Component",
     ylab="Proportion of Variance Explained",
     ylim=c(0,1), type='b')
plot(cumsum(pve), xlab="Principal Component",
     ylab="Cumulative Proportion of Variance Explained",
     ylim=c(0,1), type='b')
# plot with PC1 and PC2 - number
par(pty="s")
plot(pc$scores[,1],pc$scores[,2],
     xlab="general condition",ylab="dimensions",type="n",lwd=2)
text(pc$scores[,1],pc$scores[,2],
     labels=abbreviate(1:1451),cex=0.7,lwd=2)
# Plot with PC1 and PC2 - color
plot(pc$scores[,1], pc$scores[,2],
     ylim=range(pc$scores[,2]), xlim=range(pc$scores[,1]),
     xlab="general condition", ylab="dimensions", type="n", lwd=2)
points(pc$scores[,1], pc$scores[,2], col="red", pch=20)
######################################
### supervised analysis
#set seed to ensure reproducible results
set.seed(2002)
# create training and testing samples
training.samples <- Data$SaleCondition %>%
  createDataPartition(p = 0.8, list = FALSE)
TrainData <- Data[training.samples, ]
TestData <- Data[-training.samples, ]
paste0("Proportion of training is ", round((nrow(TrainData)/nrow(Data))*100,2),"%")
paste0("Proportion of testing is ", round((nrow(TestData)/nrow(Data))*100,2),"%")
## Random Forest implementation
# Identify numeric variables
numeric_variables <- TrainData[, sapply(TrainData, is.numeric)]
# Function to identify binary variables within numeric variables
is_binary <- function(x) {
  length(unique(x)) == 2
}
# Identify binary variables within numeric variables
binary_columns <- sapply(numeric_variables, is_binary)
binary_variables <- numeric_variables[, binary_columns]
# Select non-binary numeric variables
non_binary_numeric_variables <- numeric_variables[, !binary_columns]
# Identify non-numeric variables
non_numeric_variables <- TrainData[, !sapply(TrainData, is.numeric)]
# Subset the DataFrame to include only numerical columns
TrainData.Numeric <- non_binary_numeric_variables
TrainData.Binary <- binary_variables
TrainData.NonNumeric <- non_numeric_variables
TrainData.NonNumeric <- cbind(TrainData.NonNumeric, SalePrice = TrainData$SalePrice) # Goal variable "SalePrice" adding
## Random Forest Model (DEFAULT) considering all the data
RF_Model1 <- randomForest(SalePrice~., data=TrainData, type = "regression")
## Random Forest Model (DEFAULT) just considering numeric data
RF_Model2 <- randomForest(SalePrice~., data=TrainData.Numeric, type = "regression")
## Random Forest Model (DEFAULT) just considering non-numeric data
RF_Model3 <- randomForest(SalePrice~., data=TrainData.NonNumeric, type = "regression")
##Prediction for all data
predicted1 <- predict(RF_Model1,TestData)
error1 <- (abs(predicted1-TestData$SalePrice)/TestData$SalePrice)*100
RF_Comparison1 <- cbind(TestData$SalePrice, predicted1, error1)
misclassRate1 <- mean(RF_Comparison1[,3])
cat("The error accuracy  rate with the default random forest (all data) on test data is:",
    misclassRate1, "%\n")
##Prediction for numeric data
predicted2 <- predict(RF_Model2,TestData)
error2 <- (abs(predicted2-TestData$SalePrice)/TestData$SalePrice)*100
RF_Comparison2 <- cbind(TestData$SalePrice, predicted2, error2)
misclassRate2 <- mean(RF_Comparison2[,3])
cat("The error accuracy  rate with the default random forest (numeric data) on test data is:",
    misclassRate2, "%\n")
##Prediction for non-numeric data
predicted3 <- predict(RF_Model3,TestData)
error3 <- (abs(predicted3-TestData$SalePrice)/TestData$SalePrice)*100
RF_Comparison3 <- cbind(TestData$SalePrice, predicted3, error3)
misclassRate3 <- mean(RF_Comparison3[,3])
cat("The error accuracy  rate with the default random forest (non-numeric data) on test data is:",
    misclassRate3, "%\n")
## Optimized Random Forest Model to estimate the sale price based on this features
RF_Model <- randomForest(SalePrice~., data=TrainData, type = "regression", ntree=850, mtry= 15)
print(RF_Model)

#Prediction
predicted <- predict(RF_Model,TestData)
error <- (abs(predicted-TestData$SalePrice)/TestData$SalePrice)*100

RF_Comparison <- cbind(TestData$SalePrice, predicted, error)

misclassRate <- mean(RF_Comparison[,3])
cat("The error accuracy rate with the default random forest on test data is:",
    misclassRate, "%\n")
# sample of the classified data
random_indices <- sample(nrow(RF_Comparison), 15)
random_rows <- RF_Comparison[random_indices, ]
print(kable(random_rows))

