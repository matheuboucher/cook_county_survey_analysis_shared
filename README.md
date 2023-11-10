
**NOTICE:** This repository is for viewing purposes only, and it is a public version of a repository that I created to analyze an annual tenant satisfaction survey as an Applied Data Fellow in Cook County's Bureau of Asset Management. You can find the source code in the src folder. Each of the files in this folder contains a brief description of its purpose. The files also contain comments throughout that provide descriptions of the functions and variables that are defined. All of the data in the current_survey, references, and past_surveys folders, which usually contain the data used in these R scripts, have been removed. Thus, the R scripts contained in this repository will not run properly.

**PURPOSE:** The goal of this project was to make the survey analysis process simple. The new process works like this: the user saves a satisfaction survey downloaded from Microsoft Forms as current_survey.xlsx in the current_survey folder; then, they would source or run the TSS_R_analysis.R file which would use the functions in the other R files to generate tables, wordclouds, charts, and quadrant plots from the survey that would then populate the various folders in the analysis folder; the user would then use these visualizations in a presentation of the survey results.

**BRIEF FILE DESCRIPTIONS:**
  - TSS_R_analysis.R: This is the file that is intended to be executed by the user. It applies the functions from the TSS_R_functions, TSS_R_quadrantplots, and TSS_R_wordclouds files to the current       survey (which is also defined in the functions file) and saves the outputs to the various folders in the analysis folder.
  - TSS_R_functions.R: This file contains the functions that clean the survey, calculate various weighted means based on the data, and subset and group the data for various purposes. 
  - TSS_R_quadrantplots.R: This file contains the functions that generate two different quadrant plots, one that shows tenant satisfaction by facility compared to the previous year, and another that
    shows tenant satisfaction by question compared to the previous year.
  - TSS_R_wordclouds.R: This file contains the functions that generate wordclouds for specific facilities based on the responses from the tenants of those facilities.

**AUTHOR:** Matheu Boucher
