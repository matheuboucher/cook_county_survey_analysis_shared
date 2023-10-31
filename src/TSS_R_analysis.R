# The following is an R script for cleaning the Tenant Satisfaction Survey that
# is downloaded from Microsoft Forms. When running this script, it will generate
# two Excel workbooks that have data that is cleaned and sorted. 
# The first output, "cleaned_survey_r.xlsx", is a cleaned version of the tenant
# satisfaction survey, i.e. extra columns have been removed and scores that are
# recorded as strings in this way: "3 -- somewhat satisfied" have been converted
# to integers, in this case 3. The second output, "analyzed_survey_r.xlsx",
# contains several tables that are used in the presentation of the survey
# results, like the number of respondents by facility compared to past years,
# and tenant satisfaction by question and facility (e.g. how satisfied tenants
# are with the outdoor spaces at their facilities).

# Getting Started ---------------------------------------------------------

# Start with a clean workspace rm() removes everything
rm(list=ls())

# R file with needed functions
source("./TSS_R_functions.R")
#source("./TSS_R_deeperdives.R")

# Survey Analysis ---------------------------------------------------------

# Table with the weights for each of the questions
question_weights = read_excel("../current_survey/question_weight.xlsx")

# Responses by year with totals and percentage change from 2021
n_responses = compare_to_past(current_survey, resp_by_facility)

# Table with just the questions that are used to calculate satisfaction scores
satisf_table = satisfaction_table(current_survey)

# Table with the average satisfaction rating for each question at each facility
mean_sat_table = mean_satisfaction_by_facility(current_survey)

# Table with all of the written responses (please choose, please specify, etc.)
answ_table = answer_table(current_survey)

# Table with just the transportation data (when do you telecommute, etc.)
transpo_table = transportation_table(current_survey)

# Get the mean total satisfaction score and compare to past years
mean_total = compare_to_past(current_survey, total_mean_sat)

# Get the mean satisfaction by question and compare to past years
mean_by_q = compare_to_past(current_survey, mean_by_question)

# Table with the satisfaction rating for each facility using weighted mean
mean_by_f = compare_to_past(current_survey, weighted_mean_f)

# Get the mean satisfaction by category and compare to past years
mean_by_c = compare_to_past(current_survey, total_cat_sat)

#Get the mean satisfaction by category for facilities and compare to past years
mean_by_c_f = mean_cat_sat_by_f(current_survey)


# Excel Output ------------------------------------------------------------
#define lists with sheets and sheet names for the Excel workbooks
list_tables = list("current_survey"=current_survey, "satisf_table"=satisf_table,
                   "answ_table"=answ_table, "question_weights"=question_weights)
list_analysis = list("Total Mean Satisfaction"=mean_total,
                     "Number of Respondents"=n_responses,
                     "Mean Sat by Q and F"=mean_sat_table,
                     "Mean Sat by C and F"=mean_by_c_f,
                     "Per Change by Question"=mean_by_q,
                     "Per Change by Facility"=mean_by_f,
                     "Per Change by Category"=mean_by_c,
                     "Question Weights"=question_weights)

#Create blank workbooks
tables = createWorkbook()
analysis = createWorkbook()

#Add some sheets to the workbooks
fill_wb(tables, names(list_tables))
fill_wb(analysis, names(list_analysis))

#Set Excel column widths
setColWidths(tables, sheet = 1, cols = 1, widths = 46.33)
setColWidths(tables, sheet = 2, cols = 1, widths = 46.33)
setColWidths(tables, sheet = 3, cols = c(1:16), widths = 46.33)
excelformat_weights(tables, 4)

setColWidths(analysis, sheet = 3, cols = 1, widths = 46.33)
excelformat_compare(analysis, c(1, 2, 5, 6, 7))
setColWidths(analysis, sheet = 4, cols = 1, widths = 20.00)
excelformat_weights(analysis, 8)

#write the data to the sheets
write_sheets(tables, list_tables)
write_sheets(analysis, list_analysis)

#Export the files
saveWorkbook(tables, "../analysis/cleaned_survey_r.xlsx")
saveWorkbook(analysis, "../analysis/analyzed_survey_r.xlsx")


# Quadrant Plots ----------------------------------------------------------

#This will run the quadrant plot file that will generate the quadrant plots
#and save them in the quadrant plots folder
source("./TSS_R_quadrantplots.R")

# Word clouds --------------------------------------------------------------

#This will run the word cloud file that will generate the desired word clouds
#and save them in the wordclouds folder
source("./TSS_R_wordclouds.R")
