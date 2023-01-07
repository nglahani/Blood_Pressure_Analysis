
library(caret)
library(glmnet)
library(regclass)

#Blood Pressure Data
bp_csv = (read.csv("C:/Users/nlahanis/OneDrive - Huron Consulting Group/Documents/GT/MGT6203/Team-68/Data/Cleaned/mgt_6203_labeled_bp_data.csv"))

#Diet Data
diet_csv = (read.csv("C:/Users/nlahanis/OneDrive - Huron Consulting Group/Documents/GT/MGT6203/Team-68/Data/Cleaned/diet_clean.csv"))
diet_bp = diet_csv

elastic <- train(
  elevated_bp_flag ~., data = diet_bp, method = "glmnet",
  trControl = trainControl("cv", number = 10),
  tuneLength = 10
)
coeff = coef(elastic$finalModel, elastic$bestTune$lambda)
a = coeff@i
a = append(a,ncol(diet_bp))
diet_final_dataset = diet_bp[,a]

var_names = names(diet_final_dataset)[nearZeroVar(diet_final_dataset)]
for (i in var_names){
  diet_final_dataset[,i] = NULL
}

write.csv(diet_final_dataset,"C:/Users/nlahanis/OneDrive - Huron Consulting Group/Documents/GT/MGT6203/Team-68/Data/Cleaned/Further_Cleaning/diet_clean.csv", row.names = FALSE)

#Examination Data
exam_csv = (read.csv("C:/Users/nlahanis/OneDrive - Huron Consulting Group/Documents/GT/MGT6203/Team-68/Data/Cleaned/examination_cleaned.csv"))
exam_bp = merge(exam_csv, bp_csv, by = "SEQN")

elastic <- train(
  elevated_bp_flag ~., data = exam_bp, method = "glmnet",
  trControl = trainControl("cv", number = 10),
  tuneLength = 10
)
coeff = coef(elastic$finalModel, elastic$bestTune$lambda)
a = coeff@i
a = append(a,ncol(exam_bp))
exam_final_dataset = exam_bp[,a]

var_names = names(exam_final_dataset)[nearZeroVar(exam_final_dataset)]
for (i in var_names){
  exam_final_dataset[,i] = NULL
}

write.csv(exam_final_dataset,"C:/Users/nlahanis/OneDrive - Huron Consulting Group/Documents/GT/MGT6203/Team-68/Data/Cleaned/Further_Cleaning/exam_clean.csv", row.names = FALSE)

#Labs Data
lab_csv = (read.csv("C:/Users/nlahanis/OneDrive - Huron Consulting Group/Documents/GT/MGT6203/Team-68/Data/Cleaned/labs_clean.csv"))
lab_bp = merge(lab_csv, bp_csv, by = "SEQN")

elastic <- train(
  elevated_bp_flag ~., data = lab_bp, method = "glmnet",
  trControl = trainControl("cv", number = 10),
  tuneLength = 10
)

coeff = coef(elastic$finalModel, elastic$bestTune$lambda)
a = coeff@i
a = append(a,ncol(lab_bp))
lab_final_dataset = lab_bp[,a]

var_names = names(lab_final_dataset)[nearZeroVar(lab_final_dataset)]
for (i in var_names){
  lab_final_dataset[,i] = NULL
}

write.csv(lab_final_dataset,"C:/Users/nlahanis/OneDrive - Huron Consulting Group/Documents/GT/MGT6203/Team-68/Data/Cleaned/Further_Cleaning/lab_clean.csv", row.names = FALSE)

#Medications Data
medication_csv = read.csv("C:/Users/nlahanis/OneDrive - Huron Consulting Group/Documents/GT/MGT6203/Team-68/Data/Cleaned/medications_cleaned.csv", stringsAsFactors = TRUE)
medication_csv[sapply(medication_csv, is.factor)] <- data.matrix(medication_csv[sapply(medication_csv, is.factor)])
medication_bp = merge(medication_csv, bp_csv, by = "SEQN")
medication_bp <- medication_bp[ - as.numeric(which(apply(medication_bp, 2, var) == 0))]

elastic <- train(
  elevated_bp_flag ~., data = medication_bp, method = "glmnet",
  trControl = trainControl("cv", number = 10),
  tuneLength = 10
)

coeff = coef(elastic$finalModel, elastic$bestTune$lambda)
a = coeff@i
a = append(a,ncol(medication_bp))
medication_final_dataset = medication_bp[,a]

var_names = names(medication_final_dataset)[nearZeroVar(medication_final_dataset)]
for (i in var_names){
  medication_final_dataset[,i] = NULL
}

write.csv(medication_final_dataset,"C:/Users/nlahanis/OneDrive - Huron Consulting Group/Documents/GT/MGT6203/Team-68/Data/Cleaned/Further_Cleaning/medication_clean.csv", row.names = FALSE)

#Demographic Data
demographic_csv = (read.csv("C:/Users/nlahanis/OneDrive - Huron Consulting Group/Documents/GT/MGT6203/Team-68/Data/Cleaned/non_null_demographic_seqn.csv"))
demographic_csv[sapply(demographic_csv, is.factor)] <- data.matrix(demographic_csv[sapply(demographic_csv, is.factor)])
demographic_bp = merge(demographic_csv, bp_csv, by = "SEQN")
demographic_bp <- demographic_bp[ - as.numeric(which(apply(demographic_bp, 2, var) == 0))]


elastic <- train(
  elevated_bp_flag ~., data = demographic_bp, method = "glmnet",
  trControl = trainControl("cv", number = 10),
  tuneLength = 10
)

coeff = coef(elastic$finalModel, elastic$bestTune$lambda)
a = coeff@i
a = append(a,ncol(demographic_bp))
demographic_final_dataset = demographic_bp[,a]

var_names = names(demographic_final_dataset)[nearZeroVar(demographic_final_dataset)]
for (i in var_names){
  demographic_final_dataset[,i] = NULL
}

write.csv(demographic_final_dataset,"C:/Users/nlahanis/OneDrive - Huron Consulting Group/Documents/GT/MGT6203/Team-68/Data/Cleaned/Further_Cleaning/demographic_clean.csv", row.names = FALSE)

