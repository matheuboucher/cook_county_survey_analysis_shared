## THIS FILE GENERATES WORDCLOUDS FROM TENANTS' RESPONSES USING THE ##
## TSS_R_FUNCTIONS FILE ##

#Source the functions file
source("./TSS_R_functions.R")

# Word clouds ---------------------------------------------------------------

#Make a name of a facility file friendly, i.e. remove special characters, etc.
make_name_file_friendly = function(the_string) {
  a = gsub(" ", "", the_string) %>% 
    gsub('[/]', "_", .) %>% 
    gsub('[.]', "_", .) %>% 
    gsub('[&]', "_", .) %>% 
    gsub('[(]', "_", .) %>% 
    gsub('[)]', "", .) %>% 
    gsub('[-]', "_", .) %>% 
    gsub('[,]', "", .)
}

#Generate a word cloud for overall or specific facility
gen_wordcloud = function(survey, facility_wc=NULL) {
  ans_table = answer_table(survey)
  if (is.null(facility_wc)) {
    sub_table = ans_table
    ind = "overall"
  }
  else {
    sub_table = subset(ans_table, ans_table$facility==facility_wc)
    ind = facility_wc
  }
  wordcountcheck = sub_table$`possible improvements`
  #Clean word cloud
  worddocs = VCorpus(VectorSource(wordcountcheck)) %>%
    tm_map(removeNumbers) %>%
    tm_map(removePunctuation) %>%
    tm_map(stripWhitespace) %>% 
    tm_map(content_transformer(tolower)) %>% 
    tm_map(removeWords, stopwords("english"))
  #Generate word data frame
  dtm = TermDocumentMatrix(worddocs) 
  wordmatrix = as.matrix(dtm) 
  words = sort(rowSums(wordmatrix),decreasing=TRUE) 
  worddf = data.frame(word = names(words),freq=words)
  #Generate word cloud
  set.seed(1234) # for reproducibility
  jpeg(file=paste0("../analysis/wordclouds/", make_name_file_friendly(ind), "_wordcloud.jpeg"), width=1080, height=1080, res=180)
  wordcloud(words = worddf$word, freq = worddf$freq, scale=c(2.5,0.25),
            min.freq = 1, max.words=200, random.order=FALSE, rot.per=0.35,
            colors=brewer.pal(8, "Dark2"))
  dev.off()
}

# Generate Word clouds -------------------------------------------------------

#list of building names
temp_mean_sat = mean_satisfaction_by_facility(current_survey)
all_facilities = as.list(temp_mean_sat$facility)

# Read in the facilities to be used to create word clouds from the
# facilities_for_wordclouds.txt file.
wordcloud_facilities = as.list(read_lines(
  "../current_survey/facilities_for_wordclouds.txt", skip = 11,
  n_max = 25000))

# Generate the overall word cloud
gen_wordcloud(current_survey)

# Generate the word clouds for each facility in facilities_for_wordclouds.txt
for (building in wordcloud_facilities) {
  #if you would like to generate word clouds for all facilities, and not just
  #those in the text file, replace "wordcloud_facilities" in this for loop with
  #"all_buildings"
  gen_wordcloud(current_survey, building)
}

