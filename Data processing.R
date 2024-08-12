# Set up
library(rdhs)
set_rdhs_config(email = "kelvin.yip21@imperial.ac.uk", 
                project = "Exploring the effects of children losing one
                or both parents on their education outcomes across
                many countries in Africa.")

library(data.table)

# Create function to pull data from surveys
# Includes recodes with "terms" and extra recodes in function code
collect_data <- function(countries, terms, maxage, filetype="PR", latest_surv=TRUE){
  
  # Request all DHS surveys from countries of interest
  survs <- dhs_surveys(countryIds = countries, surveyType = "DHS")
  
  # Use the latest survey available from each country
  if(latest_surv){
    latest_surveys <- lapply(countries, function(country){
      surveys_of_country <- survs[survs$DHS_CountryCode == country, ]
      last_survey = tail(surveys_of_country, n=1)
      return(last_survey)
    })
    
    # Create dataframe of most recent surveys for all countries
    latest_surveys_df <- do.call(rbind.data.frame, latest_surveys)
    
    # Select the desired datasets (Missing PR for Sudan)
    datasets <- dhs_datasets(surveyIds = latest_surveys_df$SurveyId, 
                             fileFormat = "FL", 
                             fileType = filetype)
  }
  
  # Search for questions in the household member recode (PR) for eduation
  edu_questions <- search_variable_labels(datasets$FileName, search_terms = terms)
  unique_variables <- unique(edu_questions$variable)
  
  # Find the unique survey years
  unique_years <- unique(datasets$SurveyYear)
  
  # Count the number of latest surveys from each year
  year_freq <- c()
  for(i in 1:length(unique_years)){
    year_freq[i] = sum(datasets$SurveyYear == unique_years[i])
  }
  year_freq_df <- do.call(rbind, Map(data.frame, A=unique_years, B=year_freq))
  
  # Find the countries per survey year
  country_surv_year <- c()
  for(i in 1:length(unique_years)){
    datasets_surv <- datasets[datasets$SurveyYear == unique_years[i], ]
    country_surv_year[[length(country_surv_year)+1]] = datasets_surv$CountryName
  }
  
  # Find education questions by year
  edu_questions_year <- c()
  for(i in 1:length(unique_years)){
    datasets_year <- datasets[datasets$SurveyYear == unique_years[i], ]
    unique_vars <- unique(search_variable_labels(datasets_year$FileName, search_terms = terms)$variable)
    edu_questions_year[[length(edu_questions_year)+1]] <- unique_vars
  }
  edu_questions_year_df <- data.frame(Key = unique_years, Value = I(edu_questions_year))
  edu_questions_year_df$Frequency <- year_freq
  edu_questions_year_df$Countries <- country_surv_year
  
  dt_list <- list()
  
  # Double for loop
  # Loop over all survey years
  for(i in 1:length(unique_years)){
    
    # Create a list of education recodes, and other recodes of interest
    extra_ques <- c("hv105", "hv111", "hv113", "hv104")
    year_quest_with_age <- append(edu_questions_year_df$Value[[i]], extra_ques)
    datasets_year_age <- datasets[datasets$SurveyYear == unique_years[i], ]
    downloads_year_age <- get_datasets(datasets_year_age$FileName)
    questions_year_age <- search_variables(datasets_year_age$FileName,
                                           variables = year_quest_with_age)
    extract_year_age <- extract_dhs(questions_year_age, add_geo = TRUE)
    #extract_year_age <- data.table(extract_year_age)
    #extract_year_age[, country:= countries]
    #extract_year_age[, year:= ]
    
    fin_data_year <- list()
    
    # Loop over all countries that have that year as latest survey
    for(j in 1:length(edu_questions_year_df[i, 4][[1]])){
      
      # Do the child extraction for each country
      extract_year_temp <- extract_year_age[[j]]
      extract_bound_year_temp <- rbind_labelled(extract_year_temp)
      child_temp = extract_year_temp[which(extract_year_temp$hv105 <= maxage), ]
      
      fin_data_year[[j]] <- data.table(child_temp)
    }
    
    dt_list <- append(dt_list, fin_data_year)
  }
  #return(list(ret, edu_questions_year_df, datasets))
  return(dt_list)
}

# Testing function 
countries <- c("AO", "BJ", "BF", "BU", "CM", "CF", "TD", "KM", "CG",
               "CD", "CI", "ET", "GA", "GM", "GH", "GN", "KE", "LS",
               "LB", "MD", "MW", "ML", "MR", "MZ", "NM", "NI", "NG",
               "RW", "ST", "SN", "SL", "ZA", "SD", "TZ", "TG", "UG",
               "ZM", "ZW")
terms = c("Education", "school")
maxage = 18
result <- collect_data(countries, terms, maxage, filetype="PR", latest_surv=TRUE)

# Get rid of hb68 and hc68 in Egypt (LS2014DHS), since it was missing labels
result[[18]][,hb68:=NULL]
result[[18]][,hc68:=NULL]

# Get rid of columns in Kenya (KE2022DHS), missing labels
result[[33]][,ha68:=NULL]
result[[33]][,hc68:=NULL]
result[[33]][,hb68:=NULL]
result[[33]][,shcureduc:=NULL]
result[[33]][,shprveduc:=NULL]

# Wrong variable in BF (same name, different meaning)
result[[5]][,sh17aa:=NULL]

# Merge all datatables, to collect data in single data table
all_data <- result[[1]]
count <- 0
for(i in 2:length(result)){
  all_data <- merge(result[[i]], all_data, all = TRUE)
  num_dim <- dim(result[[i]])
  print(num_dim)
  count = count + num_dim[1]
}
count = count + dim(result[[1]])[1]

# Filtering columns
# Should get rid of all those not related to orphans
recodes_edu <- c("hv106", "hv109", "hv121")
other_recodes <- c("hv105", "hv111", "hv113", "SurveyId")
recodes_of_interest <- append(recodes_edu, other_recodes)

data_filtered <- all_data[, ..recodes_of_interest]

# Get rid of NA values
data_filtered <- data_filtered[complete.cases(data_filtered)]
# Get rid of 8 or 9 values in hv106
data_filtered <- data_filtered[!(hv106 %in% c(8, 9))]
# Get rid of 9 value in hv121
data_filtered <- data_filtered[!(hv121 %in% c(9))]
# Get rid of 8 or 9 value in hv111
data_filtered <- data_filtered[!(hv111 %in% c(8, 9))]
# Get rid of 8 or 9 value in hv113
data_filtered <- data_filtered[!(hv113 %in% c(8, 9))]

# Create an orphan column
mother_status <- as.integer(data_filtered$hv111)
father_status <- as.integer(data_filtered$hv113)
orphan_status <- mother_status * father_status

# Replace mother and father columns with orphan column
data_filtered[,hv111:=NULL]
data_filtered[,hv113:=NULL]
data_filtered[,Orphan:=orphan_status]

# Extract the hv106 data
hv106_data <- data_filtered[, c("hv106", "Orphan", "SurveyId")]
# Split the data by country
hv106_split_data <- split(hv106_data, by = "SurveyId")

# Extract the hv121 data
hv121_data <- data_filtered[, c("hv121", "Orphan", "SurveyId")]
# Split the data by country
hv121_split_data <- split(hv121_data, by = "SurveyId")

## Turn hv121 into binary data, since varying number of 1's and 2's
# To fix the issue, combine the values 1 and 2, turn 2 into 1
for(i in 1:length(hv121_split_data)){
  hv121_split_data[[i]][hv121 == 2, hv121 := 1]
  print(table(hv121_split_data[[i]]))
}