## THIS FILE CONTAINS FUNCTIONS TO ANALYZE A TENANT SATISFACTION SURVEY ##

# Packages --------------------------------------------------
# Below package start-up message suppression code is taken from Stack Overflow
# from an answer provided by the user Greenleaf: 
# https://stackoverflow.com/questions/18931006/how-to-suppress-warning-messages-when-loading-a-library
shhh = suppressPackageStartupMessages # It's a library, so shhh!
if (require(pacman)==FALSE) {install.packages("pacman")}
shhh(library(pacman))

shhh(p_load(tidyverse))
shhh(p_load(readxl))
shhh(p_load(readr))
shhh(p_load(magrittr))
shhh(p_load(openxlsx))
shhh(p_load(janitor))
shhh(p_load(ggrepel))
shhh(p_load(tm))
shhh(p_load(wordcloud))

# Names and Variables ----------------------------------------------------

## Question variables
wkdays = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")
satisfaction_questions = c("How satisfied are you with",
                           "Please rate your level of")
affirmative_yes_no_questions = c("Does", "Do you have adequate storage space",
                                 "Are you familiar")
negative_yes_no_questions = c("Do you have any unused")
answer_questions = c("Please indicate", "Please specify", "What aspect",
                     "What improvements")

## Questions
## NOTE: IF THERE HAS BEEN A CHANGE TO THE LIST OF QUESTIONS FOR YOUR SURVEY
##       IT MUST BE REFLECTED HERE AND ABOVE

# ------ This section has been hidden for privacy reasons ------ #

## Changing the question names
## NOTE: IF THERE HAS BEEN A CHANGE TO THE LIST OF QUESTIONS FOR YOUR SURVEY
##       IT MUST BE REFLECTED HERE AND ABOVE

# ------ This section has been hidden for privacy reasons ------ #

## columns for written/elaborated ("choose one") responses
answer_columns = c("ID", "facility", "department", 
                   "exterior",
                   "common areas",
                   "restroom condition",
                   "mechanical equipment",
                   "wifi",
                   "air quality",
                   "technological devices",
                   "issue resolution",
                   "natural light",
                   "winter temperature",
                   "summer temperature",
                   "effective performance",
                   "best aspect",
                   "possible improvements")

# Sorting and Cleaning ----------------------------------------------------

## Sort and Clean Surveys
clean_survey = function(survey) {
  unnec_cols = c("Name", "Completion time", "Start time", "Email")
  survey = survey[, sort(colnames(survey))]
  colnames(survey)[grepl('department',colnames(survey))] = 'department'
  colnames(survey)[grepl('select the facility',colnames(survey))] = 'facility'
  survey %<>%
    relocate(c(starts_with("ID"), starts_with("facility"), 
               contains(all_satisf_questions), any_of(wkdays),
               contains("department"))) %>%
    select(-any_of(unnec_cols)) %>%
    mutate(across(where(is.double), as.character)) %>% 
    mutate(across(contains(satisfaction_questions), ~(strtoi(substring(., 1, 1))))) %>% 
    arrange(., facility)
  return(survey)
}
#TEST: a = clean_survey(original_current_survey)

#round table values to a specified number of decimals
round_values_in_table = function(survey, decimals) {
  rounded_tbl = survey %>% 
    mutate(across(where(is.numeric), ~round(., decimals)))
  return(rounded_tbl)
}

# Load Surveys ------------------------------------------------------------

# Current survey
original_current_survey = read_excel("../current_survey/current_survey.xlsx")
current_survey = clean_survey(original_current_survey)

# Tables ------------------------------------------------------------------

# Satisfaction table without averages
satisfaction_table = function(survey) {
  survey %>% 
    group_by(facility) %>% 
    select(c("ID", "facility", contains(satisfaction_questions),
             contains(affirmative_yes_no_questions),
             contains(negative_yes_no_questions))) %>% 
    mutate(across(contains(affirmative_yes_no_questions),
                  ~(if_else(grepl("Y", .x, ignore.case=TRUE),
                            5, 1, missing=NULL)))) %>% 
    mutate(across(contains(negative_yes_no_questions),
                  ~(if_else(grepl("Y", .x, ignore.case=TRUE),
                            1, 5, missing=NULL)))) %>% 
    relocate(ID, facility, contains(all_satisf_questions)) %>%
    arrange(., facility) %>% 
    question_names(.) %>% 
    mutate(across(ID, as.character))
}
#TEST: a = satisfaction_table(current_survey)

# All of the transportation questions without averages
transportation_table = function(survey) {
  survey %>% 
    select(c("ID", "facility", "department", all_of(wkdays), contains("drove"),
             contains("vehicle")))
}

# Table with all written/elaborated responses
answer_table = function(survey) {
  answ_tbl = survey %>%
    select("ID", "facility", "department", contains(answer_questions))
  colnames(answ_tbl) = answer_columns
  return(answ_tbl)
}


# Mean Satisfaction Tables ------------------------------------------------

#Get the mean satisfaction by facility and question (not a weighted mean)
mean_satisfaction_by_facility = function(survey) {
  mean_sat = satisfaction_table(survey) %>%
    summarise(across(where(is.numeric), \(x) mean(x, na.rm = TRUE))) %>% 
    round_values_in_table(., 2)
  return(mean_sat)
}
#TEST: a = mean_satisfaction_by_facility(current_survey)


# Num Responses -----------------------------------------------------------

# Count by facility
count_by_facility = function(survey) {
  survey %>% 
    count(facility, sort = T)
}
  
# Get the number of responses by facility
resp_by_facility = function(survey) {
  count_by_facility(survey) %>% 
    adorn_totals(., fill = "-", na.rm = TRUE)
}

num_responses_column = function(survey) {
  out = count_by_facility(survey)
  ret = left_join(mean_satisfaction_by_facility(survey), out, by = "facility") %>% 
    relocate(., n, .after = "facility")
}
#TEST: a = num_responses_column(current_survey)

# Weighted Mean -----------------------------------------------------------

# Table with the weights for each of the questions
question_weights = read_excel("../current_survey/question_weight.xlsx")

# Get the categories
cats = question_weights[,4]
q_ws = question_weights[,3]
c_ws = question_weights[,2]

fc_ws = question_weights[1:7,2]
pw_ws = question_weights[8:15,2]
fm_ws = question_weights[16:24,2]
bs_ws = question_weights[25:30,2]
fm_ws = question_weights[31:34,2]

#Get the weighted mean by facility for the year using the weights for each question
weighted_mean_f = function(survey) {
  table = as.data.frame(t(column_to_rownames(mean_satisfaction_by_facility(survey), "facility")))
  a = apply(table, 2, function(x) weighted.mean(as.data.frame(x), q_ws, na.rm = TRUE))
  ret = round_values_in_table(as.data.frame(a), 3) %>% 
    rownames_to_column(., "facility")
  return(ret)
}

#Get the weighted mean by question
mean_by_question = function(survey) {
  table = num_responses_column(survey) %>%
    mutate(across(-c(facility, n), function(x) n*x)) %>% 
    select(-c(facility, n))
  total_resps = sum(num_responses_column(survey)$n)
  sum_cols = as.data.frame(colSums(table))
  colnames(sum_cols)[1] = 'total_by_q'
  table_divided = mutate(sum_cols, mean = total_by_q/total_resps) %>% 
    rownames_to_column(., var = "question") %>% 
    select(., -total_by_q)
}
#TESTS:
# true_test = compare_to_past(current_survey, weighted_mean_by_q)

#Get the total satisfaction score for the year
total_mean_sat = function(survey) {
  mean_by_ques = mean_by_question(survey) %>% 
    add_column(., q_ws, .before = 1) %>% 
    mutate(across(-c(Weight2,question), function(x) Weight2*x)) %>% 
    select(-c(Weight2, question))
  ret = as.data.frame(colSums(mean_by_ques)) %>% 
    rownames_to_column(., var = "description")
}
#TESTS:
# tot_sat_test2 = total_mean_sat(current_survey)
# comp_test = compare_to_past(current_survey, total_mean_sat)

#Get the total satisfaction score by category for the year
total_cat_sat = function(survey) {
  mean_by_q = mean_by_question(survey) %>% 
    add_column(., c_ws, .before = 1) %>% 
    add_column(., cats, .before = 1) %>% 
    mutate(across(-c(Weight, question, Category), function(x) Weight*x)) %>% 
    group_by(., Category) %>%
    select(-c(Weight, question)) %>% 
    summarise(across(everything(), function(x) sum(x)))
}
#TESTS:
#cat_sat_test = total_cat_sat(clean_2021)
#comp_test2 = compare_to_past(current_survey, total_cat_sat)

# Gets the weighted mean by category for each facility for the year
mean_cat_sat_by_f = function(survey) {
  mean_by_f = rownames_to_column(row_to_names(
    as.data.frame(t(mean_satisfaction_by_facility(survey))), 1), var = "question")  %>% 
    add_column(., c_ws, .before = 1) %>% 
    add_column(., cats, .before = 1) %>% 
    mutate(across(-c(Weight, question, Category), as.numeric)) %>% 
    mutate(across(-c(Weight, question, Category), function(x) Weight*x)) %>% 
    group_by(., Category) %>% 
    select(-c(Weight, question)) %>% 
    summarise(across(everything(), function(x) sum(x)))
}
#TEST: mean_cat_sat_by_f_test = mean_cat_sat_by_f(current_survey)

# Comparing to Past -------------------------------------------------------

#names of past surveys
past_survey_names = sort(list.files("../references/past_surveys"), decreasing = TRUE)
#list of past survey years
past_survey_years = substr(past_survey_names, 1, 4)
#past survey file paths
past_survey_paths =
  lapply(paste("../references/past_surveys/", past_survey_names, sep = ""), read_excel)
#clean past surveys
clean_past_list = lapply(past_survey_paths, clean_survey)
#apply resp_by_facility across past survey results
clean_past_resps_list =
  lapply(clean_past_list, resp_by_facility)

#comparing to past for questions
compare_to_past = function(survey, f) {
  current_result = f(survey)
  join_by_column = colnames(current_result)[1]
  table_columns = c(join_by_column, "current", past_survey_years)
  past_tables = lapply(clean_past_list, f)
  
  all_tables = c(list(current_result), past_tables)
  make_numeric = all_tables %>% 
    reduce(left_join, by = join_by_column)
  colnames(make_numeric) = table_columns
  
  make_numeric %<>%
    mutate(across(starts_with('2'),
                  ~ (current - .x), .names = 'change_from_{.col}')) %>%
    mutate(across(starts_with('2'),
                  ~ (((current - .x)/.x)*100),
                  .names = 'pct_change_from_{.col}')) %>%
    round_values_in_table(., 3)
  arrange(make_numeric, -current)
}
#TESTS:
#mean_by_q = compare_to_past(current_survey, mean_satisfaction_by_question)
#num_responses = compare_to_past(current_survey, resp_by_facility)

# Excel Functions and Lists --------------------------------------------------

#function to fill a workbook with list of sheets
fill_wb = function(wb, the_list) {for (x in the_list) {addWorksheet(wb, x)}}

#get number of surveys in past surveys folder
num_past_surveys = length(list.files("../references/past_surveys"))

#format for compare to past tables
excelformat_compare = function(wb, sheet_nums) {
  compare_index=num_past_surveys+2
  for (n in sheet_nums) {
    setColWidths(wb, sheet = n, cols = 1, widths = 46.33)
    setColWidths(wb, sheet = n, cols = c(2:compare_index), widths = 12.00)
    setColWidths(wb, sheet = n, 
                 cols =
                   c((compare_index+1):(compare_index+(2*num_past_surveys))),
                 widths = 21.00)
  }
}

#format for question weights
excelformat_weights = function(wb, sheet_num) {
  setColWidths(wb, sheet = sheet_num, cols = 1, widths = 43.00)
  setColWidths(wb, sheet = sheet_num, cols = c(2, 3), widths = 10.00)
  setColWidths(wb, sheet = sheet_num, cols = 4, widths = 18.00)
}

#function for writing data to workbooks
write_sheets = function(wb, the_list) {
  for (i in 1:length(the_list))
  {writeDataTable(wb, sheet = names(the_list[i]), x = the_list[[i]],
                  tableStyle = "TablestyleMedium6")}
}
