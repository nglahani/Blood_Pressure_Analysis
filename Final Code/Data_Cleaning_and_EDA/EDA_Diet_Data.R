library(dplyr)
#Read in DataTable
x = read.csv("C:/Users/nlahanis/OneDrive - Huron Consulting Group/Documents/GT/MGT6203/Team-68/Data/Original/diet.csv")

#Read in Response Variable 
y = read.csv("C:/Users/nlahanis/OneDrive - Huron Consulting Group/Documents/GT/MGT6203/Team-68/Data/Cleaned/mgt_6203_labeled_bp_data.csv")

#Merge Tables
full_df  = merge(x, y, by = "SEQN")

#Remove Sparse Features
col_NAs = colSums(is.na(full_df))
keep_cols = toString(names(col_NAs[col_NAs <= 1000]))
keep_cols = as.list(strsplit(keep_cols, ", "))[[1]]
final_trim_1 =  subset(full_df, select = keep_cols)
glimpse(final_trim_1)

#Impute Missing Values with mean of Column

for(i in 1:ncol(final_trim_1)) {       # for-loop over columns
  print(median(final_trim_1[,i],na.rm=TRUE))
  final_trim_1[ , i][is.na(final_trim_1[ , i])] <- median(final_trim_1[,i],na.rm=TRUE)
}

glimpse(final_trim_1)

#Remove Outliers with Cook's Distance
mod <- lm(elevated_bp_flag ~ ., data = final_trim_1)
cooksd <- cooks.distance(mod)
influential <- as.numeric(names(cooksd)[(cooksd > (4/nrow(final_trim_1)))])
final_trim_2 = final_trim_1[-influential, ]

#Determine Correlation Matrix and Remove Correlated Variables
cormat = cor(final_trim_2)
cormat[upper.tri(cormat)] <- 0
diag(cormat) <- 0

final_corr <- 
  final_trim_2[, !apply(cormat, 2, function(x) any(abs(x) > .6, na.rm = TRUE))]
head(final_corr)


library(regclass)
#Determine Multicollinearity and Remove Features with high VIF
model1 <- lm(elevated_bp_flag~., data = final_corr)

# Remove Linearly Dependent Variables
ld.vars <- attributes(alias(model1)$Complete)$dimnames[[1]]
final_corr[,ld.vars] = NULL

#Run VIF Model and Remove Values with high VIF  
model2 <- lm(elevated_bp_flag~., data = final_corr)
VIF = VIF(model2)

for(j in 1:length(VIF)) {
  if (VIF[j] > 5){
    final_corr[,j] = NULL
  }
}

#Run Elastic-Net to Determine Feature Selection

#Train Elastic-Net Regression Model
library(caret)
library(glmnet)

elastic <- train(
  elevated_bp_flag ~., data = final_corr, method = "glmnet",
  trControl = trainControl("cv", number = 10),
  tuneLength = 10
)
# Model coefficients
coeff = coef(elastic$finalModel, elastic$bestTune$lambda)
a = coeff@i
a = append(a,ncol(final_corr))
final_dataset = final_corr[,a]

#Remove Features With Low Variance
var_names = names(final_dataset)[nearZeroVar(final_dataset)]
for (i in var_names){
  final_dataset[,i] = NULL
}

library(readxl)
#Map Features to Actual Column Names
mapping_table = read_excel("C:/Users/nlahanis/OneDrive - Huron Consulting Group/Documents/GT/MGT6203/Team-68/Data/Original/NAHNES 2014 Dictionary.xlsx")
final_df = data.frame('Variable Name' = character(),'Variable Description' = character())


for (code in names(final_dataset)){
  a = mapping_table[which(mapping_table$'Variable Name' == code), ] 
  if (nrow(a) != 0){
    final_df[nrow(final_df) + 1,] = a
  }
}

#Upload to .csv

write.csv(final_dataset,"C:/Users/nlahanis/OneDrive - Huron Consulting Group/Documents/GT/MGT6203/Team-68/Data/Cleaned/diet_clean.csv", row.names = FALSE)
write.csv(final_df,"C:/Users/nlahanis/OneDrive - Huron Consulting Group/Documents/GT/MGT6203/Team-68/Data/Cleaned/diet_clean_names.csv", row.names = FALSE)

