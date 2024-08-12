## Model checking plots
library(gtools)
print(dim(po$units_baseline))
po[["units_baseline"]][1,1]
# Find probability of orphan attending school (1) for each country
# Test for single value
single_baseline <- po[["units_baseline"]][1,1]
print(single_baseline)
print(length(po[["units_baseline"]][,1]))
print(length(po[["baseline_hyper_mean"]]))
# Obtain samples of baseline for country 1 (Lesotho)
country1_intercept_samples <- po[["baseline_hyper_mean"]] + po[["units_baseline"]][,1]
country1_orp_eff_samples <- po[["trt_hyper_mean"]] + po[["units_trt"]][,1]
# Do inv logit to find prob orphan attending school (1)
country1_prob1 <- inv.logit(country1_intercept_samples + country1_orp_eff_samples)
# Find median and quantiles
country1_prob1_quant <- quantile(country1_prob1, prob = c(0.5, 0.025, 0.975))
# Do the same for probability of non orphan attending school (2)
country1_prob2 <- inv.logit(country1_intercept_samples)
country1_prob2_quant <- quantile(country1_prob2, prob = c(0.5, 0.025, 0.975))
# Compare to the actual values in the Lesotho data
country1_prob1_actual <- hv121_stan_data222[2, School] / hv121_stan_data222[2, Total]
country1_prob2_actual <- hv121_stan_data222[1, School] / hv121_stan_data222[1, Total]

# Do this for all countries
country_prob1_list <- c()
country_prob1_list_actual <- c()
country_prob2_list <- c()
country_prob2_list_actual <- c()
for(i in 1:35){
  # Taking the median?
  country_prob1_list <- append(country_prob1_list,
                               quantile(inv.logit(po[["baseline_hyper_mean"]] + po[["units_baseline"]][,i] +
                                                    po[["trt_hyper_mean"]] + po[["units_trt"]][,i]), prob = c(0.5, 0.025, 0.975))[1])
  country_prob2_list <- append(country_prob2_list,
                               quantile(inv.logit(po[["baseline_hyper_mean"]] + po[["units_baseline"]][,i]),
                                        prob = c(0.5, 0.025, 0.975))[1])
  country_prob1_list_actual <- append(country_prob1_list_actual, 
                                      hv121_stan_data222[2*i, School] / hv121_stan_data222[2*i, Total])
  country_prob2_list_actual <- append(country_prob2_list_actual, 
                                      hv121_stan_data222[(2*i)-1, School] / hv121_stan_data222[(2*i)-1, Total])
}

# Put in a data table for plotting
check_model_data <- data.table(prob1_est = country_prob1_list,
                               prob1_act = country_prob1_list_actual,
                               prob2_est = country_prob2_list,
                               prob2_act = country_prob2_list_actual,
                               group = hv121_stan_data22$Orphan)
check_model_plot <- ggplot() +
  geom_abline(intercept = 0, slope = 1, colour = 'grey50') +
  geom_point(data = check_model_data, aes(x = prob1_est, y = prob1_act, colour = factor(group))) +
  geom_point(data = check_model_data, aes(x = prob2_est, y = prob2_act, colour = factor(group))) +
  labs(
    y = 'Prob attend school, actual', 
    x = 'prob attend school, estimated',
    title = 'Model checking for hv121'
  ) +
  scale_color_manual(
    name = 'Orphan Status', # Set the legend title here
    values = c('1' = 'red', '0' = 'green'),
    labels = c('1' = 'Orphan', '0' = 'Non Orphan')
  )

ggsave(file = file.path(out.dir,'hv121_check_plot.png'), check_model_plot, w = 6, h = 5)
knitr::include_graphics(file.path(out.dir,'hv121_check_plot.png'))

########

# Create a model checking plot focusing on 2 age categories
# Stratify into ages 5-9 and 10-18

#5-9
data_filtered59 <- data_filtered[data_filtered$hv105 >= 5]
data_filtered59 <- data_filtered59[data_filtered59$hv105 <= 9]
data_filtered59[Orphan==0, Orphan:=2]
data_filtered59[Orphan==1, Orphan:=0]
data_filtered59[Orphan==2, Orphan:=1]
data_filtered59[hv121==2, hv121:=1]
orphan_hv121_59 <- sum(data_filtered59[Orphan=='1']$hv121) / length(data_filtered59[Orphan=='1']$hv121) # red
nonorphan_hv121_59 <- sum(data_filtered59[Orphan=='0']$hv121) / length(data_filtered59[Orphan=='0']$hv121) # blue

#10-18
data_filtered1018 <- data_filtered[data_filtered$hv105 >= 10]
data_filtered1018 <- data_filtered1018[data_filtered1018$hv105 <= 18]
data_filtered1018[Orphan==0, Orphan:=2]
data_filtered1018[Orphan==1, Orphan:=0]
data_filtered1018[Orphan==2, Orphan:=1]
data_filtered1018[hv121==2, hv121:=1]
orphan_hv121_1018 <- sum(data_filtered1018[Orphan=='1']$hv121) / length(data_filtered1018[Orphan=='1']$hv121) # green
nonorphan_hv121_1018 <- sum(data_filtered1018[Orphan=='0']$hv121) / length(data_filtered1018[Orphan=='0']$hv121) # pink

# Do this for all countries (empirical on x axis)
hv121_split_data59 <- split(data_filtered59, by = "SurveyId")
hv121_split_data1018 <- split(data_filtered1018, by = "SurveyId")

# FIX: Need to sort list of data tables by alphabetical order
hv121_split_data59 <- hv121_split_data59[order(names(hv121_split_data59))]
hv121_split_data1018 <- hv121_split_data1018[order(names(hv121_split_data1018))]

orphan_hv121_59_list <- c() # red
nonorphan_hv121_59_list <- c() # blue
orphan_hv121_1018_list <- c()
nonorphan_hv121_1018_list <- c()
for(i in 1:length(hv121_split_data59)){
  orphan_hv121_59_list <- append(orphan_hv121_59_list, sum(hv121_split_data59[[i]][Orphan=='1']$hv121)
                                 / length(hv121_split_data59[[i]][Orphan=='1']$hv121))
  nonorphan_hv121_59_list <- append(nonorphan_hv121_59_list, sum(hv121_split_data59[[i]][Orphan=='0']$hv121)
                                    / length(hv121_split_data59[[i]][Orphan=='0']$hv121))
  orphan_hv121_1018_list <- append(orphan_hv121_1018_list, sum(hv121_split_data1018[[i]][Orphan=='1']$hv121)
                                   / length(hv121_split_data1018[[i]][Orphan=='1']$hv121))
  nonorphan_hv121_1018_list <- append(nonorphan_hv121_1018_list, sum(hv121_split_data1018[[i]][Orphan=='0']$hv121)
                                      / length(hv121_split_data1018[[i]][Orphan=='0']$hv121))
}

# Create table counting no. children in each category
orphan_59_count <- c()
nonorphan_59_count <- c()
orphan_1018_count <- c()
nonorphan_1018_count <- c()
for(i in 1:length(hv121_split_data59)){
  orphan_59_count <- append(orphan_59_count, length(hv121_split_data59[[i]][Orphan=='1']$hv105))
  nonorphan_59_count <- append(nonorphan_59_count, length(hv121_split_data59[[i]][Orphan=='0']$hv105))
  orphan_1018_count <- append(orphan_1018_count, length(hv121_split_data1018[[i]][Orphan=='1']$hv105))
  nonorphan_1018_count <- append(nonorphan_1018_count, length(hv121_split_data1018[[i]][Orphan=='0']$hv105))
}


# NEW: Made a mistake before, since countries did not match up in empirical and model estimates
country_names_sort <- c("Angola", "Burkina Faso", "Benin", "Burundi", "Congo Democratic Republic", "Congo",
                        "Cote d'Ivoire", "Cameroon", "Gabon", "Ghana", "Gambia", "Guinea", "Kenya",
                        "Comoros", "Liberia", "Lesotho", "Madagascar", "Mali", "Mauritania", "Malawi",
                        "Mozambique", "Nigeria", "Niger", "Namibia", "Rwanda", "Sierra Leone", "Senegal",
                        "Sao Tome and Principe", "Chad", "Togo", "Tanzania", "Uganda", "South Africa", "Zambia",
                        "Zimbabwe")

# Make count table, use sorted countries now
orphan_age_table <- data.table(Country = country_names_sort, OrphanAged5to9 = orphan_59_count,
                               OrphanAged10to18 = orphan_1018_count,
                               NonOrphanAged5to9 = nonorphan_59_count,
                               NonOrphanAged10to18 = nonorphan_1018_count)
orphan_age_table <- orphan_age_table[order(orphan_age_table$Country)]


# These are sorted by country_names_sort
model_estimates_hv121 <- data.table(Orphan59 = orphan_hv121_59_list)
model_estimates_hv121$Nonorphan59 <- nonorphan_hv121_59_list
model_estimates_hv121$Orphan1018 <- orphan_hv121_1018_list
model_estimates_hv121$Nonorphan1018 <- nonorphan_hv121_1018_list
model_estimates_hv121$Country <- country_names_sort
model_estimates_hv121 <- model_estimates_hv121[order(model_estimates_hv121$Country)]
model_estimates_hv121[,Country:=NULL]

# These are sorted by country_names
# Add column of upper and lower bounds
country_prob1_upper_list <- c()
country_prob1_lower_list <- c()
country_prob2_upper_list <- c()
country_prob2_lower_list <- c()
# NEW
country_prob1_effect_list <- c()
country_prob2_effect_list <- c()
for(i in 1:35){
  country_prob1_upper_list <- append(country_prob1_upper_list,
                                     quantile(inv.logit(po[["baseline_hyper_mean"]] + po[["units_baseline"]][,i] +
                                                          po[["trt_hyper_mean"]] + po[["units_trt"]][,i]), prob = c(0.5, 0.025, 0.975))[3])
  country_prob1_lower_list <- append(country_prob1_lower_list,
                                     quantile(inv.logit(po[["baseline_hyper_mean"]] + po[["units_baseline"]][,i] +
                                                          po[["trt_hyper_mean"]] + po[["units_trt"]][,i]), prob = c(0.5, 0.025, 0.975))[2])
  country_prob2_upper_list <- append(country_prob2_upper_list,
                                     quantile(inv.logit(po[["baseline_hyper_mean"]] + po[["units_baseline"]][,i]),
                                              prob = c(0.5, 0.025, 0.975))[3])
  country_prob2_lower_list <- append(country_prob2_lower_list,
                                     quantile(inv.logit(po[["baseline_hyper_mean"]] + po[["units_baseline"]][,i]),
                                              prob = c(0.5, 0.025, 0.975))[2])
  country_prob1_effect_list <- append(country_prob1_effect_list,
                                      quantile(inv.logit(po[["baseline_hyper_mean"]] + po[["units_baseline"]][,i] +
                                                           po[["trt_hyper_mean"]] + po[["units_trt"]][,i]), prob = c(0.5, 0.025, 0.975))[1])
  country_prob2_effect_list <- append(country_prob2_effect_list,
                                      quantile(inv.logit(po[["baseline_hyper_mean"]] + po[["units_baseline"]][,i]),
                                               prob = c(0.5, 0.025, 0.975))[1])
}

# NEW: Need to match up countries
# Add to data table
model_estimates_hv121_2 <- data.table(Orphan = country_prob1_effect_list, Nonorphan = country_prob2_effect_list)
model_estimates_hv121_2$UpperOrphan <- country_prob1_upper_list
model_estimates_hv121_2$LowerOrphan <- country_prob1_lower_list
model_estimates_hv121_2$UpperNonorphan <- country_prob2_upper_list
model_estimates_hv121_2$LowerNonorphan <- country_prob2_lower_list
model_estimates_hv121_2$Country <- country_names
model_estimates_hv121_2 <- model_estimates_hv121_2[order(model_estimates_hv121_2$Country)]
model_estimates_hv121_2[,Country:=NULL]

# Re use this variable name
model_estimates_hv121_test <- cbind(model_estimates_hv121, model_estimates_hv121_2)

# Plot graph
f <- ggplot(model_estimates_hv121_test,
            aes(x = Orphan59, y = Orphan, ymin = LowerOrphan, ymax = UpperOrphan, color = 'red')) +
  geom_errorbar(width = 0.005) +
  geom_point(size = 0.7) +
  geom_abline(intercept = 0, slope = 1, colour = 'grey50') +
  labs(
    y = 'Prob attend school estimated from model',
    x = 'Prob attend school empirical from data',
    title = 'Prob attend school by orphan status and age by country'
  )

# Add the second set of points to the existing ggplot object
f <- f + geom_errorbar(data = model_estimates_hv121_test, aes(x = Orphan1018, y = Orphan, ymin = LowerOrphan, ymax = UpperOrphan, color = 'chartreuse4'), width = 0.005) +
  geom_point(data = model_estimates_hv121_test, aes(x = Orphan1018, y = Orphan, color = 'chartreuse4'), size = 0.7)

# Add the third set of points to the existing ggplot object
f <- f + geom_errorbar(data = model_estimates_hv121_test, aes(x = Nonorphan59, y = Nonorphan, ymin = LowerNonorphan, ymax = UpperNonorphan, color = 'blue'), width = 0.005) +
  geom_point(data = model_estimates_hv121_test, aes(x = Nonorphan59, y = Nonorphan, color = 'blue'), size = 0.7)

# Add the fourth set of points to the existing ggplot object
f <- f + geom_errorbar(data = model_estimates_hv121_test, aes(x = Nonorphan1018, y = Nonorphan, ymin = LowerNonorphan, ymax = UpperNonorphan, color = 'deeppink'), width = 0.005) +
  geom_point(data = model_estimates_hv121_test, aes(x = Nonorphan1018, y = Nonorphan, color = 'deeppink'), size = 0.7)

# Change legend
f <- f + scale_color_manual(
  name = 'Groups', # Set the legend title here
  values = c('red' = 'red', 'chartreuse4' = 'chartreuse4', 'blue' = 'blue', 'deeppink' = 'deeppink'),
  labels = c('red' = 'Orphans aged 5-9', 
             'chartreuse4' = 'Orphans aged 10-18',
             'blue' = 'Non Orphans aged 5-9',
             'deeppink' = 'Non Orphans aged 10-18')
)
ggsave(file = file.path(out.dir,'orphan_age_check.png'), f, w = 6, h = 5)
knitr::include_graphics(file.path(out.dir,'orphan_age_check.png'))
print(f)

# Make a histogram of orphans and non-orphans
# Orphans is 0 here
orphan_ages <- data_filtered[data_filtered$Orphan==0]$hv105
orphan_table_value <- c(0, 0)
for(i in 1:length(orphan_ages)){
  if(5 <= orphan_ages[i] & orphan_ages[i] <= 9){
    orphan_table_value[1] = orphan_table_value[1] + 1
  }
  if(10 <= orphan_ages[i] & orphan_ages[i] <= 18){
    orphan_table_value[2] = orphan_table_value[2] + 1
  }
}
orph_label <- c('Orphan', 'Non-Orphan')
# Non orphans is 1 here
nonorphan_ages <- data_filtered[data_filtered$Orphan==1]$hv105
nonorphan_table_value <- c(0, 0)
for(i in 1:length(nonorphan_ages)){
  if(5 <= nonorphan_ages[i] & nonorphan_ages[i] <= 9){
    nonorphan_table_value[1] = nonorphan_table_value[1] + 1
  }
  if(10 <= nonorphan_ages[i] & nonorphan_ages[i] <= 18){
    nonorphan_table_value[2] = nonorphan_table_value[2] + 1
  }
}

######

# Create orphan type check plot


# Filtering columns
# Should get rid of all those not related to orphans
recode_orphan_type <- c("hv106", "hv109", "hv121", "hv105", "hv111", "hv113", "SurveyId")
orphan_type_data <- all_data[, ..recode_orphan_type]
# Get rid of NA values
orphan_type_data <- orphan_type_data[complete.cases(orphan_type_data)]
# Get rid of 8 or 9 values in hv106
orphan_type_data <- orphan_type_data[!(hv106 %in% c(8, 9))]
# Get rid of 9 value in hv121
orphan_type_data <- orphan_type_data[!(hv121 %in% c(9))]
# Get rid of 8 or 9 value in hv111
orphan_type_data <- orphan_type_data[!(hv111 %in% c(8, 9))]
# Get rid of 8 or 9 value in hv113
orphan_type_data <- orphan_type_data[!(hv113 %in% c(8, 9))]

orphan_type_data <- orphan_type_data[orphan_type_data$hv105 >= 5]
orphan_type_data <- orphan_type_data[, c("hv121", "hv111", "hv113", "SurveyId")]

# Better way to do it
# 0 means double orphan
# 1 means paternal orphan
# 2 means maternal orphan
# 3 means non orphan
orphan_type_list <- as.integer(orphan_type_data$hv111) + (as.integer(orphan_type_data$hv113) * 2)
print(table(as.integer(orphan_type_list)))

orphan_type_data_hv121 <- copy(orphan_type_data)
orphan_type_data_hv121[, hv111:=NULL]
orphan_type_data_hv121[, hv113:=NULL]
orphan_type_data_hv121$OrphanType <- orphan_type_list # add orphan type column

orphan_type_data_hv121_split <- split(orphan_type_data_hv121, by = 'SurveyId')

# Change so hv121 is binary
for(i in 1:length(orphan_type_data_hv121_split)){
  orphan_type_data_hv121_split[[i]][hv121 == 2, hv121 := 1]
  print(table(orphan_type_data_hv121_split[[i]]))
}


orphan_type_table_test <- table(orphan_type_data_hv121_split[[1]]$OrphanType)


# Make the data in long form in table
hv121_00 <- c()
hv121_10 <- c()
hv121_01 <- c()
hv121_11 <- c()
for(i in 1:length(orphan_type_data_hv121_split)){
  tl <- table(orphan_type_data_hv121_split[[i]]$OrphanType)["0"]
  tr <- table(orphan_type_data_hv121_split[[i]]$OrphanType)["1"]
  bl <- table(orphan_type_data_hv121_split[[i]]$OrphanType)["2"]
  br <- table(orphan_type_data_hv121_split[[i]]$OrphanType)["3"]
  hv121_00 <- append(hv121_00, tl)
  hv121_10 <- append(hv121_10, tr)
  hv121_01 <- append(hv121_01, bl)
  hv121_11 <- append(hv121_11, br)
}

# Summary table for orphan types counts by country
orphan_type_table <- data.table(Country = country_names, DoubleOrphan = hv121_00, MaternalOrphan = hv121_10,
                                PaternalOrphan = hv121_01, NonOrphan = hv121_11)

# Now make the orphan type graph
test <- orphan_type_data_hv121_split[[1]]

# Do this for all countries (empirical on x axis)
hv121_00_list <- c()
hv121_10_list <- c()
hv121_01_list <- c()
hv121_11_list <- c()
for(i in 1:length(orphan_type_data_hv121_split)){
  hv121_00_list <- append(hv121_00_list, sum(orphan_type_data_hv121_split[[i]][OrphanType=='0']$hv121)
                          / length(orphan_type_data_hv121_split[[i]][OrphanType=='0']$hv121))
  hv121_10_list <- append(hv121_10_list, sum(orphan_type_data_hv121_split[[i]][OrphanType=='1']$hv121)
                          / length(orphan_type_data_hv121_split[[i]][OrphanType=='1']$hv121))
  hv121_01_list <- append(hv121_01_list, sum(orphan_type_data_hv121_split[[i]][OrphanType=='2']$hv121)
                          / length(orphan_type_data_hv121_split[[i]][OrphanType=='2']$hv121))
  hv121_11_list <- append(hv121_11_list, sum(orphan_type_data_hv121_split[[i]][OrphanType=='3']$hv121)
                          / length(orphan_type_data_hv121_split[[i]][OrphanType=='3']$hv121))
}

# Create data table to plot from
orphan_type_prob_data <- data.table(DoubleOrphan = hv121_00_list, MaternalOrphan = hv121_10_list,
                                    PaternalOrphan = hv121_01_list, NonOrphan = hv121_11_list,
                                    Orphan_model = country_prob1_list, Nonorphan_model = country_prob2_list,
                                    UpperOrphan = country_prob1_upper_list, LowerOrphan = country_prob1_lower_list,
                                    UpperNonorphan = country_prob2_upper_list, LowerNonorphan = country_prob2_lower_list)

# Plot graph
g <- ggplot(orphan_type_prob_data,
            aes(x = DoubleOrphan, y = Orphan_model, ymin = LowerOrphan, ymax = UpperOrphan, color = 'blue')) +
  geom_errorbar(width = 0.005) +
  geom_point(size = 0.7) +
  geom_abline(intercept = 0, slope = 1, colour = 'grey50') +
  labs(
    y = 'Prob attend school estimated from model',
    x = 'Prob attend school empirical from data',
    title = 'Prob attend school by orphan status and orphan type by country'
  )

# Add the second set of points to the existing ggplot object
g <- g + geom_errorbar(data = orphan_type_prob_data, aes(x = MaternalOrphan, y = Orphan_model, ymin = LowerOrphan, ymax = UpperOrphan, color = 'red'), width = 0.005) +
  geom_point(data = orphan_type_prob_data, aes(x = MaternalOrphan, y = Orphan_model, color = 'red'), size = 0.7)

# Add the third set of points to the existing ggplot object
g <- g + geom_errorbar(data = orphan_type_prob_data, aes(x = PaternalOrphan, y = Orphan_model, ymin = LowerOrphan, ymax = UpperOrphan, color = 'green'), width = 0.005) +
  geom_point(data = orphan_type_prob_data, aes(x = PaternalOrphan, y = Orphan_model, color = 'green'), size = 0.7)

# Add the fourth set of points to the existing ggplot object
g <- g + geom_errorbar(data = orphan_type_prob_data, aes(x = NonOrphan, y = Nonorphan_model, ymin = LowerNonorphan, ymax = UpperNonorphan, color = 'deeppink'), width = 0.005) +
  geom_point(data = orphan_type_prob_data, aes(x = NonOrphan, y = Nonorphan_model, color = 'deeppink'), size = 0.7)

# Change legend
g <- g + scale_color_manual(
  name = 'Groups', # Set the legend title here
  values = c('blue' = 'blue', 'red' = 'red', 'green' = 'green', 'deeppink' = 'deeppink'),
  labels = c('blue' = 'Double Orphan', 
             'red' = 'Maternal Orphan',
             'green' = 'Paternal Orphan',
             'deeppink' = 'Non Orphan')
)

ggsave(file = file.path(out.dir,'orphan_type_check.png'), g, w = 6, h = 5)
knitr::include_graphics(file.path(out.dir,'orphan_type_check.png'))

#######

# Make final plot checking the impact of child gender
# i.e. how wrong are we in our model by not including child gender

# Create new function to pull data, asking for age data
# Need to ask for age as well
collect_data2 <- function(countries, terms, maxage, filetype="PR", latest_surv=TRUE){
  
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
result_with_gender <- collect_data2(countries, terms, maxage, filetype="PR", latest_surv=TRUE)

# Get rid of hb68 and hc68 in Egypt (LS2014DHS), since it was missing labels?
result_with_gender[[18]][,hb68:=NULL]
result_with_gender[[18]][,hc68:=NULL]

# Get rid of columns in Kenya (KE2022DHS), missing labels
result_with_gender[[33]][,ha68:=NULL]
result_with_gender[[33]][,hc68:=NULL]
result_with_gender[[33]][,hb68:=NULL]
result_with_gender[[33]][,shcureduc:=NULL]
result_with_gender[[33]][,shprveduc:=NULL]

# Wrong variable in BF
result_with_gender[[5]][,sh17aa:=NULL]

# Merge all datatables
all_data_with_gender <- result_with_gender[[1]]
count <- 0
for(i in 2:length(result_with_gender)){
  all_data_with_gender <- merge(result_with_gender[[i]], all_data_with_gender, all = TRUE)
  num_dim <- dim(result_with_gender[[i]])
  print(num_dim)
  count = count + num_dim[1]
}
count = count + dim(result_with_gender[[1]])[1]


# Filtering columns
# Should get rid of all those not related to orphans
recode_orphan_gender <- c("hv106", "hv109", "hv121", "hv105", "hv111", "hv113", "SurveyId", "hv104")
orphan_gender_data <- all_data_with_gender[, ..recode_orphan_gender]
# Get rid of NA values
orphan_gender_data <- orphan_gender_data[complete.cases(orphan_gender_data)]
# Get rid of 8 or 9 values in hv106
orphan_gender_data <- orphan_gender_data[!(hv106 %in% c(8, 9))]
# Get rid of 9 value in hv121
orphan_gender_data <- orphan_gender_data[!(hv121 %in% c(9))]
# Get rid of 8 or 9 value in hv111
orphan_gender_data <- orphan_gender_data[!(hv111 %in% c(8, 9))]
# Get rid of 8 or 9 value in hv113
orphan_gender_data <- orphan_gender_data[!(hv113 %in% c(8, 9))]

# NEW ONE: Get rid of 9 value in hv104
orphan_gender_data <- orphan_gender_data[!(hv104 %in% c(9))]

orphan_gender_data <- orphan_gender_data[orphan_gender_data$hv105 >= 5]

# Create an orphan column
orphan_column <- as.integer(orphan_gender_data$hv111) * as.integer(orphan_gender_data$hv113)
orphan_column <- 1 - orphan_column

# Replace mother and father columns with orphan column
orphan_gender_data[,hv111:=NULL]
orphan_gender_data[,hv113:=NULL]
orphan_gender_data[,Orphan:=orphan_column]


orphan_gender_data <- orphan_gender_data[, c("hv121", "hv104", "SurveyId", "Orphan")]

orphan_gender_data_hv121_split <- split(orphan_gender_data, by = 'SurveyId')
orphan_gender_data_hv121_split <- orphan_gender_data_hv121_split[order(names(orphan_gender_data_hv121_split))]

# Change so hv121 is binary
for(i in 1:length(orphan_gender_data_hv121_split)){
  orphan_gender_data_hv121_split[[i]][hv121 == 2, hv121 := 1]
  print(table(orphan_gender_data_hv121_split[[i]]))
}

orphan_male_count <- c()
orphan_female_count <- c()
nonorphan_male_count <- c()
nonorphan_female_count <- c()
for(i in 1:length(orphan_gender_data_hv121_split)){
  orphan_male_count <- append(orphan_male_count,
                              table(orphan_gender_data_hv121_split[[i]][Orphan=='1']$hv104)[1])
  orphan_female_count <- append(orphan_female_count,
                                table(orphan_gender_data_hv121_split[[i]][Orphan=='1']$hv104)[2])
  nonorphan_male_count <- append(nonorphan_male_count,
                                 table(orphan_gender_data_hv121_split[[i]][Orphan=='0']$hv104)[1])
  nonorphan_female_count <- append(nonorphan_female_count,
                                   table(orphan_gender_data_hv121_split[[i]][Orphan=='0']$hv104)[2])
}

orphan_gender_table <- data.table(Country = country_names_sort, OrphanMale = orphan_male_count,
                                  OrphanFemale = orphan_female_count,
                                  NonorphanMale = nonorphan_male_count,
                                  NonorphanFemale = nonorphan_female_count)

# Now make the graph

# Do this for all countries (empirical on x axis)
hv121_male_orphan_list <- c()
hv121_female_orphan_list <- c()
hv121_male_nonorphan_list <- c()
hv121_female_nonorphan_list <- c()
for(i in 1:length(orphan_gender_data_hv121_split)){
  hv121_male_orphan_list <- append(hv121_male_orphan_list, sum(orphan_gender_data_hv121_split[[i]][Orphan=='1'][hv104==1]$hv121)
                                   / length(orphan_gender_data_hv121_split[[i]][Orphan=='1'][hv104==1]$hv121))
  hv121_female_orphan_list <- append(hv121_female_orphan_list, sum(orphan_gender_data_hv121_split[[i]][Orphan=='1'][hv104==2]$hv121)
                                     / length(orphan_gender_data_hv121_split[[i]][Orphan=='1'][hv104==2]$hv121))
  hv121_male_nonorphan_list <- append(hv121_male_nonorphan_list, sum(orphan_gender_data_hv121_split[[i]][Orphan=='0'][hv104==1]$hv121)
                                      / length(orphan_gender_data_hv121_split[[i]][Orphan=='0'][hv104==1]$hv121))
  hv121_female_nonorphan_list <- append(hv121_female_nonorphan_list, sum(orphan_gender_data_hv121_split[[i]][Orphan=='0'][hv104==2]$hv121)
                                        / length(orphan_gender_data_hv121_split[[i]][Orphan=='0'][hv104==2]$hv121))
}

# These are sorted by country_names_sort
orphan_gender_prob_data <- data.table(OrphanMale = hv121_male_orphan_list, OrphanFemale = hv121_female_orphan_list,
                                      NonorphanMale = hv121_male_nonorphan_list, NonorphanFemale = hv121_female_nonorphan_list)
# These are sorted by country_names          
orphan_gender_prob_data2 <- data.table(Orphan_model = country_prob1_list, Nonorphan_model = country_prob2_list,
                                       UpperOrphan = country_prob1_upper_list, LowerOrphan = country_prob1_lower_list,
                                       UpperNonorphan = country_prob2_upper_list, LowerNonorphan = country_prob2_lower_list,
                                       Country = country_names)
orphan_gender_prob_data2 <- orphan_gender_prob_data2[order(orphan_gender_prob_data2$Country)]

# NEW
country_codes <- c("AO", "BJ", "BF", "BU", "CM", "TD", "KM", "CG", "CD", "CI", "GA", "GM",
                   "GH", "GN", "KE", "LS", "LB", "MD", "MW", "ML", "MR", "MZ", "NM", "NI",
                   "NG", "RW", "ST", "SN", "SL", "ZA", "TZ", "TG", "UG", "ZM", "ZW")
model_estimates_hv121_2_new <- copy(model_estimates_hv121_2)
model_estimates_hv121_2_new$Country <- country_codes
model_estimates_hv121_2_new <- model_estimates_hv121_2_new[order(model_estimates_hv121_2_new$Country)]
model_estimates_hv121_2_new[,Country:=NULL]

# Use this test version
orphan_gender_prob_data_test <- cbind(orphan_gender_prob_data, model_estimates_hv121_2_new)

# Plot graph
h <- ggplot(orphan_gender_prob_data_test,
            aes(x = OrphanMale, y = Orphan, ymin = LowerOrphan, ymax = UpperOrphan, color = 'red')) +
  geom_errorbar(width = 0.005) +
  geom_point(size = 0.7) +
  geom_abline(intercept = 0, slope = 1, colour = 'grey50') +
  labs(
    y = 'Prob attend school estimated from model',
    x = 'Prob attend school empirical from data',
    title = 'Prob attend school by orphan status and orphan type by country'
  )

# Add the second set of points to the existing ggplot object
h <- h + geom_errorbar(data = orphan_gender_prob_data_test, aes(x = OrphanFemale, y = Orphan, ymin = LowerOrphan, ymax = UpperOrphan, color = 'green'), width = 0.005) +
  geom_point(data = orphan_gender_prob_data_test, aes(x = OrphanFemale, y = Orphan, color = 'green'), size = 0.7)

# Add the third set of points to the existing ggplot object
h <- h + geom_errorbar(data = orphan_gender_prob_data_test, aes(x = NonorphanMale, y = Nonorphan, ymin = LowerNonorphan, ymax = UpperNonorphan, color = 'blue'), width = 0.005) +
  geom_point(data = orphan_gender_prob_data_test, aes(x = NonorphanMale, y = Nonorphan, color = 'blue'), size = 0.7)

# Add the fourth set of points to the existing ggplot object
h <- h + geom_errorbar(data = orphan_gender_prob_data_test, aes(x = NonorphanFemale, y = Nonorphan, ymin = LowerNonorphan, ymax = UpperNonorphan, color = 'deeppink'), width = 0.005) +
  geom_point(data = orphan_gender_prob_data_test, aes(x = NonorphanFemale, y = Nonorphan, color = 'deeppink'), size = 0.7)

# Change legend
h <- h + scale_color_manual(
  name = 'Groups', # Set the legend title here
  values = c('red' = 'red', 'green' = 'green', 'blue' = 'blue', 'deeppink' = 'deeppink'),
  labels = c('red' = 'Male Orphan', 
             'green' = 'Female Orphan',
             'blue' = 'Male Non Orphan',
             'deeppink' = 'Female Non Orphan')
)

ggsave(file = file.path(out.dir,'orphan_gender_check.png'), h, w = 6, h = 5)
knitr::include_graphics(file.path(out.dir,'orphan_gender_check.png'))