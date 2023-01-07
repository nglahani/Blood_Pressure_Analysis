library(ggfortify)
library(ggplot2)

#Blood Pressure Data
bp_csv = (read.csv("C:/Users/nlahanis/OneDrive - Huron Consulting Group/Documents/GT/MGT6203/Team-68/Data/Cleaned/mgt_6203_labeled_bp_data.csv"))

#Diet Data
diet_csv = (read.csv("C:/Users/nlahanis/OneDrive - Huron Consulting Group/Documents/GT/MGT6203/Team-68/Data/Cleaned/diet_clean.csv"))
diet_bp = diet_csv
diet_pc = prcomp(diet_bp[,-length(diet_bp)], scale = TRUE, center = TRUE, rank = 2)
autoplot(diet_pc, data = diet_bp, colour = 'elevated_bp_flag') + ggtitle("Diet")

#Examination Data
exam_csv = (read.csv("C:/Users/nlahanis/OneDrive - Huron Consulting Group/Documents/GT/MGT6203/Team-68/Data/Cleaned/examination_cleaned.csv"))
exam_bp = merge(exam_csv, bp_csv, by = "SEQN")
exam_pc = prcomp(exam_bp[,-length(exam_bp)], scale = TRUE, center = TRUE  )
autoplot(exam_pc, data = exam_bp, colour = 'elevated_bp_flag') + ggtitle("Examinations")

#Labs Data
lab_csv = (read.csv("C:/Users/nlahanis/OneDrive - Huron Consulting Group/Documents/GT/MGT6203/Team-68/Data/Cleaned/labs_clean.csv"))
lab_bp = merge(lab_csv, bp_csv, by = "SEQN")
lab_pc = prcomp(lab_bp[,-length(lab_bp)], scale = TRUE , center = TRUE)
autoplot(lab_pc, data = lab_bp, colour = 'elevated_bp_flag') + ggtitle("Labs")

#Medications Data
medication_csv = read.csv("C:/Users/nlahanis/OneDrive - Huron Consulting Group/Documents/GT/MGT6203/Team-68/Data/Cleaned/medications_cleaned.csv", stringsAsFactors = TRUE)
medication_csv[sapply(medication_csv, is.factor)] <- data.matrix(medication_csv[sapply(medication_csv, is.factor)])
medication_bp = merge(medication_csv, bp_csv, by = "SEQN")
medication_bp <- medication_bp[ - as.numeric(which(apply(medication_bp, 2, var) == 0))]
medication_pc = prcomp(medication_bp[,-length(medication_bp)], scale = TRUE, center = TRUE)
autoplot(medication_pc, data = medication_bp, colour = 'elevated_bp_flag') + ggtitle("Medications")

#Demographic Data
demographic_csv = (read.csv("C:/Users/nlahanis/OneDrive - Huron Consulting Group/Documents/GT/MGT6203/Team-68/Data/Cleaned/non_null_demographic_seqn.csv"))
demographic_csv[sapply(demographic_csv, is.factor)] <- data.matrix(demographic_csv[sapply(demographic_csv, is.factor)])
demographic_bp = merge(demographic_csv, bp_csv, by = "SEQN")
demographic_bp <- demographic_bp[ - as.numeric(which(apply(demographic_bp, 2, var) == 0))]
demographic_pc = prcomp(demographic_bp[,-length(demographic_bp)], scale = TRUE, Center = TRUE)
autoplot(demographic_pc, data = demographic_bp, colour = 'elevated_bp_flag') + ggtitle("Demographics")
