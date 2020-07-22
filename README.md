# FIFA_KaggleModel_Allendorf
Predicting FIFA salaries in group project for Statistics 101A at UCLA which scored in top 10 of all groups across three lectures. 

Selecting predictors from dataset of 79 variables, prioritizing simplicity and model validity. 

Dataframes:
  * FIFA = FifaTrainNew.csv
  * FIFAtest = FifaNoY.csv
  * FIFAclean = FIFA without any rows containing NAs (using is.na function)
  * train = cross-validation taking 70% of FIFAclean
  * test = cross-validation with remaining 30% of FIFAclean
  
R Scripts:
  * Clean_Shaping_Data.R = loading data, cleaning, creating cross-validation dataframes, final list of new/dummy variables, strategies for re-creating the Club variable
  * Main_Models.R = Some transformation variables, initial potential dummy variables, models 
  * Model_Testing_Template.R = checking correlations, single variable regressions, visualization of categorical variables, t tests, interaction plots, AIF and BIF tests, checking model assumptions, checking diagnostics, checking need for transformations, remove bad leverage points, predict function, write model and FIFAclean to new csvs
  
Word Documents:
  * Lec1_GroupP_FinalPaper = Final report co-authored by all four group members detailing the process and methodology ending with our final model, its diagnostics, and its limitations. 
