#Map Features to Actual Column Names
mapping_table = read_excel("C:/Users/nlahanis/OneDrive - Huron Consulting Group/Documents/GT/MGT6203/Team-68/Data/Original/NAHNES 2014 Dictionary.xlsx")
final_df = data.frame('Variable Name' = character(),'Variable Description' = character())


for (code in names(final_dataset)){
  a = mapping_table[which(mapping_table$'Variable Name' == code), ] 
  if (nrow(a) != 0){
    final_df[nrow(final_df) + 1,] = a
  }
}