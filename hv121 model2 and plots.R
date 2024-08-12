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

# Need hv104, hv105, hv121, hv111, hv113

recode_orphan_gender2 <- c("hv121", "hv104", "hv105", "hv111", "hv113", "SurveyId")
hv121_data_update <- all_data_with_gender[, ..recode_orphan_gender2]

# Get rid of NA values
hv121_data_update <- hv121_data_update[complete.cases(hv121_data_update)]
# Get rid of 9 value in hv121
hv121_data_update <- hv121_data_update[!(hv121 %in% c(9))]
# Get rid of 8 or 9 value in hv111
hv121_data_update <- hv121_data_update[!(hv111 %in% c(8, 9))]
# Get rid of 8 or 9 value in hv113
hv121_data_update <- hv121_data_update[!(hv113 %in% c(8, 9))]
# NEW ONE: Get rid of 9 value in hv104
hv121_data_update <- hv121_data_update[!(hv104 %in% c(9))]

# Get rid of children below 5
hv121_data_update <- hv121_data_update[hv121_data_update$hv105>=5]

orphan_type_replace <- as.integer(hv121_data_update$hv111) + (as.integer(hv121_data_update$hv113) * 2)
hv121_data_update[, hv111:=NULL]
hv121_data_update[, hv113:=NULL]
hv121_data_update$OrphanType <- orphan_type_replace # add orphan type column

# Put the ages into categories
output_test <- ifelse(hv121_data_update$hv105 >= 5 & hv121_data_update$hv105 <= 9, 0, 1)
hv121_data_update[, hv105:=NULL]
hv121_data_update$AgeCat <- output_test

# Split the data
hv121_data_update_split <- split(hv121_data_update, by='SurveyId')
# Change so hv121 is binary
for(i in 1:length(hv121_data_update_split)){
  hv121_data_update_split[[i]][hv121 == 2, hv121 := 1]
  print(table(hv121_data_update_split[[i]]))
}
# These are sorted by country_names_sort
hv121_data_update_split <- hv121_data_update_split[order(names(hv121_data_update_split))]

# hv121: 1 is attend school, 0 did not attend
# hv105: 0 is 5-9, 1 is 10-18
# hv104: 1 is male, 2 female
# OrphanType: 3 (Double), 1 (Paternal), 2 (Maternal), 0 (Non-Orphan)

hv121_schools_list <- c()
hv121_totals_list <- c()
for(i in 1:length(hv121_data_update_split)){
  print(i)
  tables_list <- as.vector(table(hv121_data_update_split[[i]]))
  for(j in 0:7){
    hv121_schools_list <- append(hv121_schools_list, c(tables_list[4*j + 2], tables_list[4*j + 4]))
    hv121_totals_list <- append(hv121_totals_list,
                                c(tables_list[4*j + 1] + tables_list[4*j + 2], tables_list[4*j + 3] + tables_list[4*j + 4]))
  }
}

country_names_sort16 <- rep(country_names_sort, each = 16)
orphan_type16 <- rep(c(3, 3, 1, 1, 2, 2, 0, 0), 70)
age_cat16 <- rep(c(0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1), 35)
sex_16 <- rep(c(1, 2), 280)

hv121_data_update_long <- data.table(Country = country_names_sort16,
                                     OrphanType = orphan_type16,
                                     AgeCategory = age_cat16,
                                     Sex = sex_16,
                                     School = hv121_schools_list,
                                     Total = hv121_totals_list)

hv121_updated_txt <- "
data {
  int<lower=1> N; // number of observations, here is 560
  int<lower=1> K; // number of units
  int<lower=0> y[N]; 
  int<lower=0> Total[N];
  int<lower=1> A; // number of age-sex categories
  int<lower=0> AgeSex[N]; // age sex category indicator
  // multi-indexing as seen before
  vector[N] Orphan1; // paternal orphan indicator
  vector[N] Orphan2; // maternal orphan indicator
  vector[N] Orphan3; // double orphan indicator
  int<lower=1> Nk_max; // max number of observations for a unit
  int<lower=1, upper=Nk_max> units_to_obs_length[K]; // number of observations per unit
  int<lower=0, upper=N> units_to_obs[K, Nk_max]; // index of observations per unit, with the rest set to 0
}
parameters {
  // random effects on baseline
  real units_baseline[K]; // variation by country for baseline
  real<lower=0> baseline_hyper_sd;
  real baseline_hyper_mean;
  real units_baseline_agesex[A]; // variation by age-sex category for baseline
  real<lower=0> baseline_agesex_sd;
  
  // random effects on treatment effect
  real units_orp1[K]; // variation by country for paternal
  real units_orp2[K]; // variation by country for maternal
  real units_orp3[K]; // variation by country for double
  real<lower=0> orp1_hyper_sd;
  real<lower=0> orp2_hyper_sd;
  real<lower=0> orp3_hyper_sd;
  real orp1_hyper_mean;
  real orp2_hyper_mean;
  real orp3_hyper_mean;
  real units_orp1_agesex[A]; // variation by age-sex category for maternal
  real units_orp2_agesex[A]; // variation by age-sex category for paternal
  real units_orp3_agesex[A]; // variation by age-sex category for double
  real<lower=0> orp1_agesex_sd;
  real<lower=0> orp2_agesex_sd;
  real<lower=0> orp3_agesex_sd;
}
transformed parameters {
  vector[N] obs_logit_pi;
  
  // fixed effects
  obs_logit_pi = rep_vector( baseline_hyper_mean, N ) +
                 orp1_hyper_mean * Orphan1 +
                 orp2_hyper_mean * Orphan2 +
                 orp3_hyper_mean * Orphan3;
  
  // Add age-sex category effects to obs_logit_pi
  for (n in 1:N) {
    obs_logit_pi[n] += units_baseline_agesex[AgeSex[n]];
  }
  
  // Add country-specific baseline random effects
  for (k in 1:K) {
    obs_logit_pi[ units_to_obs[k, 1:units_to_obs_length[k] ] ] +=
    rep_vector( units_baseline[k], units_to_obs_length[k]);
  }
  
  // Add country-specific and age-sex-specific treatment effects
  for (k in 1:K) {
    for (n in units_to_obs[k, 1:units_to_obs_length[k]]) {
      int age_sex_category = AgeSex[n];
      
      // Paternal orphan effect
      obs_logit_pi[n] += Orphan1[n] * (
        units_orp1[k] +
        units_orp1_agesex[age_sex_category]
      );
      
      // Maternal orphan effect
      obs_logit_pi[n] += Orphan2[n] * (
        units_orp2[k] +
        units_orp2_agesex[age_sex_category]
      );
      
      // Double orphan effect
      obs_logit_pi[n] += Orphan3[n] * (
        units_orp3[k] +
        units_orp3_agesex[age_sex_category]
      );
    }
  }
}
model {
  baseline_hyper_mean ~ normal(0,5);
  baseline_agesex_sd ~ exponential(1);
  baseline_hyper_sd ~ cauchy(0,1);
  units_baseline ~ normal(0, baseline_hyper_sd);
  units_baseline_agesex ~ normal(0, baseline_agesex_sd);
  //
  orp1_hyper_mean ~ normal(0,5);
  orp2_hyper_mean ~ normal(0,5);
  orp3_hyper_mean ~ normal(0,5);
  orp1_agesex_sd ~ exponential(1);
  orp2_agesex_sd ~ exponential(1);
  orp3_agesex_sd ~ exponential(1);
  orp1_hyper_sd ~ cauchy(0,1);
  orp2_hyper_sd ~ cauchy(0,1);
  orp3_hyper_sd ~ cauchy(0,1);
  units_orp1 ~ normal(0, orp1_hyper_sd);
  units_orp2 ~ normal(0, orp2_hyper_sd);
  units_orp3 ~ normal(0, orp3_hyper_sd);
  units_orp1_agesex ~ normal(0, orp1_agesex_sd);
  units_orp2_agesex ~ normal(0, orp2_agesex_sd);
  units_orp3_agesex ~ normal(0, orp3_agesex_sd);
  //
  y ~ binomial_logit(Total, obs_logit_pi);
}
"

orphan_list1 <- rep(c(0, 0, 1, 1, 0, 0, 0, 0), 70)
orphan_list2 <- rep(c(0, 0, 0, 0, 1, 1, 0, 0), 70)
orphan_list3 <- rep(c(1, 1, 0, 0, 0, 0, 0, 0), 70)

stan_data_hv121_updated <- c()
stan_data_hv121_updated$N <- as.integer(35 * 16)
stan_data_hv121_updated$K <- as.integer(35)
stan_data_hv121_updated$y <- hv121_data_update_long$School
stan_data_hv121_updated$Total <- hv121_data_update_long$Total
stan_data_hv121_updated$A <- 4
stan_data_hv121_updated$AgeSex <- as.integer(rep(append(rep(c(1, 2), 4), rep(c(3, 4), 4)), 35))
stan_data_hv121_updated$Orphan1 <- orphan_list1
stan_data_hv121_updated$Orphan2 <- orphan_list2
stan_data_hv121_updated$Orphan3 <- orphan_list3
stan_data_hv121_updated$Nk_max <- as.integer(16)
stan_data_hv121_updated$units_to_obs_length <- rep(as.integer(16), 35)

# Create the units_to_obs matrix
units_to_obs_test <- matrix(0, nrow = 35, ncol = 16)
Study_test <- rep(1:35, each = 16)
for (k in 1:35) {
  units_to_obs_test[k, ] <- which(Study_test == k)
}
stan_data_hv121_updated$units_to_obs <- units_to_obs_test

# compile the model
hv121_updated_model_compiled <- rstan::stan_model(
  model_name = 'hv121_updated_model', 
  model_code = gsub('\t',' ', hv121_updated_txt)
)
# run Stan
hv121_updated_model_fit <- rstan::sampling(hv121_updated_model_compiled, 
                                           data = stan_data_hv121_updated, 
                                           warmup = 1e3, iter = 5e3, chains = 1
)
# remember this is sorted by country_names_sort
# extract posterior samples
po_updated <- rstan::extract(hv121_updated_model_fit)

# Units_baseline is pretty similar in new and old model
print(colMeans(po_updated$units_baseline))
print(test_dt$Effect)


# I am going to try to create a forest plot for each country
# So each will have 16 disaggs in them, start with Angola
# I think in the forest plot we focus on the 12 disaggs corresponding to different orphan types

hv121_updated_forest_effect <- c()
hv121_updated_forest_upper <- c()
hv121_updated_forest_lower <- c()
# These are the 4 disaggs with orphan status 1
for(i in 1:35){
  for(j in 1:4){
    hv121_updated_forest_effect <- append(hv121_updated_forest_effect,
                                          quantile(po_updated$orp1_hyper_mean + po_updated$units_orp1[,i] + po_updated$units_orp1_agesex[,j], probs = c(0.5, 0.025, 0.975))[1])
    hv121_updated_forest_upper <- append(hv121_updated_forest_upper,
                                         quantile(po_updated$orp1_hyper_mean + po_updated$units_orp1[,i] + po_updated$units_orp1_agesex[,j], probs = c(0.5, 0.025, 0.975))[3])
    hv121_updated_forest_lower <- append(hv121_updated_forest_lower,
                                         quantile(po_updated$orp1_hyper_mean + po_updated$units_orp1[,i] + po_updated$units_orp1_agesex[,j], probs = c(0.5, 0.025, 0.975))[2])
  }
  for(j in 1:4){
    hv121_updated_forest_effect <- append(hv121_updated_forest_effect,
                                          quantile(po_updated$orp2_hyper_mean + po_updated$units_orp2[,i] + po_updated$units_orp2_agesex[,j], probs = c(0.5, 0.025, 0.975))[1])
    hv121_updated_forest_upper <- append(hv121_updated_forest_upper,
                                         quantile(po_updated$orp2_hyper_mean + po_updated$units_orp2[,i] + po_updated$units_orp2_agesex[,j], probs = c(0.5, 0.025, 0.975))[3])
    hv121_updated_forest_lower <- append(hv121_updated_forest_lower,
                                         quantile(po_updated$orp2_hyper_mean + po_updated$units_orp2[,i] + po_updated$units_orp2_agesex[,j], probs = c(0.5, 0.025, 0.975))[2])
  }
  for(j in 1:4){
    hv121_updated_forest_effect <- append(hv121_updated_forest_effect,
                                          quantile(po_updated$orp3_hyper_mean + po_updated$units_orp3[,i] + po_updated$units_orp3_agesex[,j], probs = c(0.5, 0.025, 0.975))[1])
    hv121_updated_forest_upper <- append(hv121_updated_forest_upper,
                                         quantile(po_updated$orp3_hyper_mean + po_updated$units_orp3[,i] + po_updated$units_orp3_agesex[,j], probs = c(0.5, 0.025, 0.975))[3])
    hv121_updated_forest_lower <- append(hv121_updated_forest_lower,
                                         quantile(po_updated$orp3_hyper_mean + po_updated$units_orp3[,i] + po_updated$units_orp3_agesex[,j], probs = c(0.5, 0.025, 0.975))[2])
  }
}

country_names_sort12 <- rep(country_names_sort, each = 12)
orphan_status12 <- rep(rep(c(1, 2, 3), each = 4), 35)
agesex12 <- rep(rep(c("5-9", "10-18"), each = 2), 105)
sex12 <- rep(c("M", "F"), 210)
labels12 <- c("Paternal Orphan, 5-9, Male", "Paternal Orphan, 5-9, Female",
              "Paternal Orphan, 10-18 Male", "Paternal Orphan, 10-18, Female",
              "Maternal Orphan, 5-9, Male", "Maternal Orphan, 5-9, Female",
              "Maternal Orphan, 10-18 Male", "Maternal Orphan, 10-18, Female",
              "Double Orphan, 5-9, Male", "Double Orphan, 5-9, Female",
              "Double Orphan, 10-18 Male", "Double Orphan, 10-18, Female")
hv121_updated_forest_dt <- data.table(Country = country_names_sort12,
                                      OrphanType = orphan_status12,
                                      AgeCategory = agesex12,
                                      Sex = sex12,
                                      Effect = hv121_updated_forest_effect,
                                      Upper = hv121_updated_forest_upper,
                                      Lower = hv121_updated_forest_lower,
                                      Labels = labels12)

# TODO: Forest plot for each country
# Lets do an example with Angola
# Save them all in a folder
out.dir2 <- "/Users/hmw850/OneDrive - Queen Mary, University of London/Documents/UROP1/DHS/Plots/Updated hv121"
for(country in country_names_sort){
  hv121_updated_logodds <- ggplot(hv121_updated_forest_dt[hv121_updated_forest_dt$Country == country], aes(x = Labels, y = Effect, ymin = Lower, ymax = Upper)) +
    geom_hline(yintercept = 0, colour = 'grey50') +
    geom_point() +
    geom_errorbar() +
    theme_bw() +
    labs(title = country, y = 'log odds of school attendance', x = '') +
    coord_flip()
  ggsave(file = file.path(out.dir2, paste0(paste0('hv121_updated_', country), ".png")), hv121_updated_logodds, w = 5, h = 6)
  knitr::include_graphics(file.path(out.dir2, paste0(paste0('hv121_updated_', country), ".png")))
}

# TODO: Forest plot for each disagg
disagg_forest_effect <- c()
disagg_forest_upper <- c()
disagg_forest_lower <- c()
for(j in 1:4){
  disagg_forest_effect <- append(disagg_forest_effect,
                                 quantile(po_updated$orp1_hyper_mean + po_updated$units_orp1_agesex[,j], probs=c(0.5, 0.025, 0.975))[1])
  disagg_forest_upper <- append(disagg_forest_upper,
                                quantile(po_updated$orp1_hyper_mean + po_updated$units_orp1_agesex[,j], probs=c(0.5, 0.025, 0.975))[3])
  disagg_forest_lower <- append(disagg_forest_lower,
                                quantile(po_updated$orp1_hyper_mean + po_updated$units_orp1_agesex[,j], probs=c(0.5, 0.025, 0.975))[2])
}
for(j in 1:4){
  disagg_forest_effect <- append(disagg_forest_effect,
                                 quantile(po_updated$orp2_hyper_mean + po_updated$units_orp2_agesex[,j], probs=c(0.5, 0.025, 0.975))[1])
  disagg_forest_upper <- append(disagg_forest_upper,
                                quantile(po_updated$orp2_hyper_mean + po_updated$units_orp2_agesex[,j], probs=c(0.5, 0.025, 0.975))[3])
  disagg_forest_lower <- append(disagg_forest_lower,
                                quantile(po_updated$orp2_hyper_mean + po_updated$units_orp2_agesex[,j], probs=c(0.5, 0.025, 0.975))[2])
}
for(j in 1:4){
  disagg_forest_effect <- append(disagg_forest_effect,
                                 quantile(po_updated$orp3_hyper_mean + po_updated$units_orp3_agesex[,j], probs=c(0.5, 0.025, 0.975))[1])
  disagg_forest_upper <- append(disagg_forest_upper,
                                quantile(po_updated$orp3_hyper_mean + po_updated$units_orp3_agesex[,j], probs=c(0.5, 0.025, 0.975))[3])
  disagg_forest_lower <- append(disagg_forest_lower,
                                quantile(po_updated$orp3_hyper_mean + po_updated$units_orp3_agesex[,j], probs=c(0.5, 0.025, 0.975))[2])
}

disagg_forest_dt <- data.table(Label = labels12,
                               Effect = disagg_forest_effect,
                               Upper = disagg_forest_upper,
                               Lower = disagg_forest_lower)
hv121_updated_logodds_disagg <- ggplot(disagg_forest_dt, aes(x = Label, y = Effect, ymin = Lower, ymax = Upper)) +
  geom_hline(yintercept = 0, colour = 'grey50') +
  geom_point() +
  geom_errorbar() +
  theme_bw() +
  labs(title = 'Pooled', y = 'log odds of school attendance', x = '') +
  coord_flip()
ggsave(file = file.path(out.dir2, 'hv121_updated_logodds_disagg.png'), hv121_updated_logodds_disagg, w = 5, h = 6)
knitr::include_graphics(file.path(out.dir2, 'hv121_updated_logodds_disagg.png'))