data_filtered2 <- data_filtered[data_filtered$hv105 >= 5]
# The typical minimum school age is around 5
# Extract the hv121 data
hv106_data2 <- data_filtered2[, c("hv106", "Orphan", "SurveyId")]
# Change orphan here
hv106_data2[Orphan == 1, Orphan := 2]
hv106_data2[Orphan == 0, Orphan := 1]
hv106_data2[Orphan == 2, Orphan := 0]
# Make hv106 go from 1-4
hv106_data2[, hv106 := as.integer(hv106) + 1]
# Split the data by country
hv106_split_data2 <- split(hv106_data2, by = "SurveyId")

# Make count table for y by country
head(hv106_data2)
head(hv106_split_data2)
hv106_1_count <- c()
hv106_2_count <- c()
hv106_3_count <- c()
hv106_4_count <- c()
for(i in 1:length(hv106_split_data2)){
  hv106_1_count <- append(hv106_1_count,
                          length(hv106_split_data2[[i]][Orphan=='1'][hv106=='1']$hv106))
  hv106_1_count <- append(hv106_1_count,
                          length(hv106_split_data2[[i]][Orphan=='0'][hv106=='1']$hv106))
  hv106_2_count <- append(hv106_2_count,
                          length(hv106_split_data2[[i]][Orphan=='1'][hv106=='2']$hv106))
  hv106_2_count <- append(hv106_2_count,
                          length(hv106_split_data2[[i]][Orphan=='0'][hv106=='2']$hv106))
  hv106_3_count <- append(hv106_3_count,
                          length(hv106_split_data2[[i]][Orphan=='1'][hv106=='3']$hv106))
  hv106_3_count <- append(hv106_3_count,
                          length(hv106_split_data2[[i]][Orphan=='0'][hv106=='3']$hv106))
  hv106_4_count <- append(hv106_4_count,
                          length(hv106_split_data2[[i]][Orphan=='1'][hv106=='4']$hv106))
  hv106_4_count <- append(hv106_4_count,
                          length(hv106_split_data2[[i]][Orphan=='0'][hv106=='4']$hv106))
}

# Double up country name and add orphan column
country_names2 <- c()
for(i in 1:35){
  country_names2 <- append(country_names2, rep(country_names[i], 2))
}
orphan_col_hv106 <- 1 - orphan_col_hv121_2

hv106_count_table <- data.table(Country = country_names2,
                                OrphanStatus = orphan_col_hv106,
                                NoEducation = as.integer(hv106_1_count),
                                PrimaryEducation = as.integer(hv106_2_count),
                                SecondaryEducation = as.integer(hv106_3_count),
                                HigherEducation = as.integer(hv106_4_count))

# Make the 70 x 4 matrix for counts to be y
hv106_y_matrix <- as.matrix(hv106_count_table[, c("NoEducation", "PrimaryEducation",
                                                  "SecondaryEducation", "HigherEducation")])

# Remember here, 1 means orphan and 0 means non orphan
# MODEL1 (Old)
mult_nom_hier_txt <- "
data {
  int<lower=1> N; // number of observations (70)
  int<lower=1> K; // number of countries (35)
  int<lower=1> C; // number of categories (4)
  array[N, C] int<lower=0> y; // response counts for each category
  vector[N] Orphan; // orphan status indicator
  int<lower=1> Nk_max; // max number of observations for a country (2)
  array[K] int<lower=1, upper=Nk_max> countries_to_obs_length; // number of observations per country (2, 2,...)
  array[K, Nk_max] int<lower=0, upper=N> countries_to_obs; // index of observations per country, with the rest set to 0
}
parameters {
  matrix[K, C-1] countries_baseline; // random effects on baseline for each category (except reference) for each country
  array[C-1] real<lower=0> baseline_hyper_sd;
  array[C-1] real baseline_hyper_mean;
  
  matrix[K, C-1] countries_orphan; // random effects on orphanhood effect for each category (except reference) for each country
  array[C-1] real<lower=0> orphan_hyper_sd;
  array[C-1] real orphan_hyper_mean;
}
transformed parameters {
  matrix[N, C-1] obs_pi;
  
  // fixed effects
  for (c in 1:(C-1)) {
    obs_pi[, c] = rep_vector(baseline_hyper_mean[c], N) + orphan_hyper_mean[c] * Orphan;
  }
  
  // add random effects
  for (k in 1:K) {
    for (c in 1:(C-1)) {
      obs_pi[countries_to_obs[k, 1:countries_to_obs_length[k]], c] += rep_vector(countries_baseline[k, c], countries_to_obs_length[k]);
      obs_pi[countries_to_obs[k, 1:countries_to_obs_length[k]], c] += Orphan[countries_to_obs[k, 1:countries_to_obs_length[k]]]
                                                                      .* rep_vector(countries_orphan[k ,c], countries_to_obs_length[k]);
    }
  }
}
model {
  for (c in 1:(C-1)) {
    baseline_hyper_mean[c] ~ normal(0, 5);
    baseline_hyper_sd[c] ~ cauchy(0, 1);
    countries_baseline[, c] ~ normal(0, baseline_hyper_sd[c]);
    orphan_hyper_mean[c] ~ normal(0, 3.2);
    orphan_hyper_sd[c] ~ exponential(1);
    countries_orphan[, c] ~ normal(0, orphan_hyper_sd[c]);
  }

  for (n in 1:N) {
      vector[C] category_prob;
    for (c in 1:(C-1)) {
      category_prob[c] = exp(obs_pi[n, c]);
      }
    category_prob[C] = 1.0; // for the reference category
    
    category_prob /= sum(category_prob); // normalise to get probabilities
    
    y[n] ~ multinomial(category_prob); // use multinomial since provide probabilities
  }
}
"
#  for (n in 1:N){
#   y[n] ~ multinomial_logit(append_row(obs_pi[n], 0));
#}

# cmdstanr requires the model to be written to a file
mult_nom_hier_cmdstanr <- cmdstanr::write_stan_file(
  gsub('\t',' ',mult_nom_hier_txt),
  dir = out.dir,
  basename = NULL,
  force_overwrite = FALSE,
  hash_salt = ""
)

# prepare data for Stan
stan_data_mult_nom_hier <- list()
stan_data_mult_nom_hier$N <- as.integer(70)
stan_data_mult_nom_hier$K <- as.integer(35)
stan_data_mult_nom_hier$C <- as.integer(4)
stan_data_mult_nom_hier$y <- hv106_y_matrix # what type here?
stan_data_mult_nom_hier$Orphan <- hv106_count_table$OrphanStatus
stan_data_mult_nom_hier$Nk_max <- as.integer(2)
stan_data_mult_nom_hier$countries_to_obs_length <- as.integer(rep(2, 35))
stan_data_mult_nom_hier$countries_to_obs <- stan_data$units_to_obs

# compile Stan model
mult_nom_hier_compiled <- cmdstanr::cmdstan_model(mult_nom_hier_cmdstanr)

# sample from joint posterior of the Hello World model with cmdstanr
mult_nom_hier_fit <- mult_nom_hier_compiled$sample(
  data = stan_data_mult_nom_hier,
  seed = 123,
  chains = 1,
  parallel_chains = 1,
  refresh = 500, # print update every 500 iters,
  save_warmup = TRUE,
  iter_sampling = 5000,
  iter_warmup = 1000
)

# save output to RDS
mult_nom_hier_fit$save_object(file = file.path(out.dir, "mult_nom_hier_compiled.rds"))

# load output from RDS
mult_nom_hier_fit <- readRDS(file.path(out.dir, "mult_nom_hier_compiled.rds"))

# to manipulate the posterior Monte Carlo samples after warmup
# we typically prefer to
# extract Monte Carlo samples as a data frame
mult_nom_hier_po <- mult_nom_hier_fit$draws(
  inc_warmup = FALSE,
  format = "draws_df"
) 
mult_nom_hier_po <- as.data.table(mult_nom_hier_po)
setnames(model1_po, names(model1_po), gsub("\\.","",names(model1_po)))

# Check model fit
# R hat
mult_nom_hier_summary <- mult_nom_hier_fit$summary()
# Filter the parameters of interest
filtered_summary <- mult_nom_hier_summary[grep("hyper|units|lp", mult_nom_hier_summary$variable), ]
# Get the minimum effective sample size (ess_bulk and ess_tail) and the maximum Rhat
min_n_eff_bulk <- min(filtered_summary$ess_bulk, na.rm = TRUE)
min_n_eff_tail <- min(filtered_summary$ess_tail, na.rm = TRUE)
max_rhat <- max(filtered_summary$rhat, na.rm = TRUE)


## Extract effects
# Make countries_orphan_matrix
hv106_level1_logodds_eff <- c()
hv106_level1_logodds_upper <- c()
hv106_level1_logodds_lower <- c()
for(i in 1:35){
  countryeffect_level1 <- paste0("countries_orphan[", i, ",1]")
  hv106_level1_logodds_eff <- append(hv106_level1_logodds_eff,
                                     quantile(mult_nom_hier_po$`orphan_hyper_mean[1]` + mult_nom_hier_po[[countryeffect_level1]], prob = c(0.5, 0.025, 0.975))[1])
  hv106_level1_logodds_upper <- append(hv106_level1_logodds_upper,
                                       quantile(mult_nom_hier_po$`orphan_hyper_mean[1]` + mult_nom_hier_po[[countryeffect_level1]], prob = c(0.5, 0.025, 0.975))[3])
  hv106_level1_logodds_lower <- append(hv106_level1_logodds_lower,
                                       quantile(mult_nom_hier_po$`orphan_hyper_mean[1]` + mult_nom_hier_po[[countryeffect_level1]], prob = c(0.5, 0.025, 0.975))[2])
}
# add pooled effect
hv106_pooled_logodds_eff <- quantile(mult_nom_hier_po$`orphan_hyper_mean[1]`, prob = c(0.5, 0.025, 0.975))[1]
hv106_pooled_logodds_upper <- quantile(mult_nom_hier_po$`orphan_hyper_mean[1]`, prob = c(0.5, 0.025, 0.975))[3]
hv106_pooled_logodds_lower <- quantile(mult_nom_hier_po$`orphan_hyper_mean[1]`, prob = c(0.5, 0.025, 0.975))[2]
hv106_level1_dt <- data.table(Country = append(country_names, "Pooled"),
                              Effect = append(hv106_level1_logodds_eff, hv106_pooled_logodds_eff),
                              Upper = append(hv106_level1_logodds_upper, hv106_pooled_logodds_upper),
                              Lower = append(hv106_level1_logodds_lower, hv106_pooled_logodds_lower))
# make forest plot
j <- ggplot(hv106_level1_dt, aes(x = Country, y = Effect, ymin = Lower, ymax = Upper)) +
  geom_hline(yintercept = 0, colour = 'grey50') +
  geom_point() +
  geom_errorbar() +
  theme_bw() +
  labs(y = 'log odds of no education compared to higher education', x = '') +
  coord_flip()
ggsave(file = file.path(out.dir,'hv106_mult_nom_level1_logoddstrteffect.png'), j, w = 6.5, h = 6)
knitr::include_graphics(file.path(out.dir,'hv106_mult_nom_level1_logoddstrteffect.png'))

########

# MODEL1 (Current)
## Going to get rid of the higher education column and make secondary education the reference category
mult_nom_hier_txt2 <- "
data {
  int<lower=1> N; // number of observations (70)
  int<lower=1> K; // number of countries (35)
  int<lower=1> C; // number of categories (3) ## changed this
  array[N, C] int<lower=0> y; // response counts for each category
  vector[N] Orphan; // orphan status indicator
  int<lower=1> Nk_max; // max number of observations for a country (2)
  array[K] int<lower=1, upper=Nk_max> countries_to_obs_length; // number of observations per country (2, 2,...)
  array[K, Nk_max] int<lower=0, upper=N> countries_to_obs; // index of observations per country, with the rest set to 0
}
parameters {
  matrix[K, C-1] countries_baseline; // random effects on baseline for each category (except reference) for each country
  array[C-1] real<lower=0> baseline_hyper_sd;
  array[C-1] real baseline_hyper_mean;
  
  matrix[K, C-1] countries_orphan; // random effects on orphanhood effect for each category (except reference) for each country
  array[C-1] real<lower=0> orphan_hyper_sd;
  array[C-1] real orphan_hyper_mean;
}
transformed parameters {
  matrix[N, C-1] obs_pi;
  
  // fixed effects
  for (c in 1:(C-1)) {
    obs_pi[, c] = rep_vector(baseline_hyper_mean[c], N) + orphan_hyper_mean[c] * Orphan;
  }
  
  // add random effects
  for (k in 1:K) {
    for (c in 1:(C-1)) {
      obs_pi[countries_to_obs[k, 1:countries_to_obs_length[k]], c] += rep_vector(countries_baseline[k, c], countries_to_obs_length[k]);
      obs_pi[countries_to_obs[k, 1:countries_to_obs_length[k]], c] += Orphan[countries_to_obs[k, 1:countries_to_obs_length[k]]]
                                                                      .* rep_vector(countries_orphan[k ,c], countries_to_obs_length[k]);
    }
  }
}
model {
  for (c in 1:(C-1)) {
    baseline_hyper_mean[c] ~ normal(0, 5);
    baseline_hyper_sd[c] ~ cauchy(0, 1);
    countries_baseline[, c] ~ normal(0, baseline_hyper_sd[c]);
    orphan_hyper_mean[c] ~ normal(0, 3.2);
    orphan_hyper_sd[c] ~ exponential(1);
    countries_orphan[, c] ~ normal(0, orphan_hyper_sd[c]);
  }
  
  for (n in 1:N) {
      vector[C] category_prob;
    for (c in 1:(C-1)) {
      category_prob[c] = exp(obs_pi[n, c]);
      }
    category_prob[C] = 1.0; // for the reference category
    
    category_prob /= sum(category_prob); // normalise to get probabilities
    
    y[n] ~ multinomial(category_prob); // use multinomial since provide probabilities
  }
}
"
# cmdstanr requires the model to be written to a file
mult_nom_hier_cmdstanr2 <- cmdstanr::write_stan_file(
  gsub('\t',' ',mult_nom_hier_txt2),
  dir = out.dir,
  basename = NULL,
  force_overwrite = FALSE,
  hash_salt = ""
)

hv106_y_matrix2 <- hv106_y_matrix[, 1:3]
# prepare data for Stan
stan_data_mult_nom_hier2 <- list()
stan_data_mult_nom_hier2$N <- as.integer(70)
stan_data_mult_nom_hier2$K <- as.integer(35)
stan_data_mult_nom_hier2$C <- as.integer(3)
stan_data_mult_nom_hier2$y <- hv106_y_matrix2 # what type here?
stan_data_mult_nom_hier2$Orphan <- hv106_count_table$OrphanStatus
stan_data_mult_nom_hier2$Nk_max <- as.integer(2)
stan_data_mult_nom_hier2$countries_to_obs_length <- as.integer(rep(2, 35))
stan_data_mult_nom_hier2$countries_to_obs <- stan_data$units_to_obs

# compile Stan model
mult_nom_hier_compiled2 <- cmdstanr::cmdstan_model(mult_nom_hier_cmdstanr2)

# sample from joint posterior of the Hello World model with cmdstanr
mult_nom_hier_fit2 <- mult_nom_hier_compiled2$sample(
  data = stan_data_mult_nom_hier2,
  seed = 123,
  chains = 1,
  parallel_chains = 1,
  refresh = 500, # print update every 500 iters,
  save_warmup = TRUE,
  iter_sampling = 5000,
  iter_warmup = 1000
)

# save output to RDS
mult_nom_hier_fit2$save_object(file = file.path(out.dir, "mult_nom_hier_compiled2.rds"))

# load output from RDS
mult_nom_hier_fit2 <- readRDS(file.path(out.dir, "mult_nom_hier_compiled2.rds"))

# to manipulate the posterior Monte Carlo samples after warmup
# we typically prefer to
# extract Monte Carlo samples as a data frame
mult_nom_hier_po2 <- mult_nom_hier_fit2$draws(
  inc_warmup = FALSE,
  format = "draws_df"
) 
mult_nom_hier_po2 <- as.data.table(mult_nom_hier_po2)

## Extract effects
# Make countries_orphan_matrix
hv106_level1_logodds_eff2 <- c()
hv106_level1_logodds_upper2 <- c()
hv106_level1_logodds_lower2 <- c()
for(i in 1:35){
  countryeffect_level1 <- paste0("countries_orphan[", i, ",1]")
  hv106_level1_logodds_eff2 <- append(hv106_level1_logodds_eff2,
                                      quantile(mult_nom_hier_po2$`orphan_hyper_mean[1]` + mult_nom_hier_po2[[countryeffect_level1]], prob = c(0.5, 0.025, 0.975))[1])
  hv106_level1_logodds_upper2 <- append(hv106_level1_logodds_upper2,
                                        quantile(mult_nom_hier_po2$`orphan_hyper_mean[1]` + mult_nom_hier_po2[[countryeffect_level1]], prob = c(0.5, 0.025, 0.975))[3])
  hv106_level1_logodds_lower2 <- append(hv106_level1_logodds_lower2,
                                        quantile(mult_nom_hier_po2$`orphan_hyper_mean[1]` + mult_nom_hier_po2[[countryeffect_level1]], prob = c(0.5, 0.025, 0.975))[2])
}
# add pooled effect
hv106_pooled_logodds_eff2 <- quantile(mult_nom_hier_po2$`orphan_hyper_mean[1]`, prob = c(0.5, 0.025, 0.975))[1]
hv106_pooled_logodds_upper2 <- quantile(mult_nom_hier_po2$`orphan_hyper_mean[1]`, prob = c(0.5, 0.025, 0.975))[3]
hv106_pooled_logodds_lower2 <- quantile(mult_nom_hier_po2$`orphan_hyper_mean[1]`, prob = c(0.5, 0.025, 0.975))[2]
hv106_level1_dt2 <- data.table(Country = append(country_names, "Pooled"),
                               Effect = append(hv106_level1_logodds_eff2, hv106_pooled_logodds_eff2),
                               Upper = append(hv106_level1_logodds_upper2, hv106_pooled_logodds_upper2),
                               Lower = append(hv106_level1_logodds_lower2, hv106_pooled_logodds_lower2))
# make forest plot
j2 <- ggplot(hv106_level1_dt2, aes(x = Country, y = Effect, ymin = Lower, ymax = Upper)) +
  geom_hline(yintercept = 0, colour = 'grey50') +
  geom_point() +
  geom_errorbar() +
  theme_bw() +
  labs(y = 'log odds of no education compared to secondary education', x = '') +
  coord_flip()
ggsave(file = file.path(out.dir,'hv106_mult_nom_level1_logoddstrteffect2.png'), j2, w = 6.5, h = 6)
knitr::include_graphics(file.path(out.dir,'hv106_mult_nom_level1_logoddstrteffect2.png'))


# Now do for level 2- level 3, primary vs secondary
## Extract effects
# Make countries_orphan_matrix
hv106_level2_logodds_eff2 <- c()
hv106_level2_logodds_upper2 <- c()
hv106_level2_logodds_lower2 <- c()
for(i in 1:35){
  countryeffect_level2 <- paste0("countries_orphan[", i, ",2]")
  hv106_level2_logodds_eff2 <- append(hv106_level2_logodds_eff2,
                                      quantile(mult_nom_hier_po2$`orphan_hyper_mean[2]` + mult_nom_hier_po2[[countryeffect_level2]], prob = c(0.5, 0.025, 0.975))[1])
  hv106_level2_logodds_upper2 <- append(hv106_level2_logodds_upper2,
                                        quantile(mult_nom_hier_po2$`orphan_hyper_mean[2]` + mult_nom_hier_po2[[countryeffect_level2]], prob = c(0.5, 0.025, 0.975))[3])
  hv106_level2_logodds_lower2 <- append(hv106_level2_logodds_lower2,
                                        quantile(mult_nom_hier_po2$`orphan_hyper_mean[2]` + mult_nom_hier_po2[[countryeffect_level2]], prob = c(0.5, 0.025, 0.975))[2])
}
# add pooled effect
hv106_pooled_level2_logodds_eff2 <- quantile(mult_nom_hier_po2$`orphan_hyper_mean[2]`, prob = c(0.5, 0.025, 0.975))[1]
hv106_pooled_level2_logodds_upper2 <- quantile(mult_nom_hier_po2$`orphan_hyper_mean[2]`, prob = c(0.5, 0.025, 0.975))[3]
hv106_pooled_level2_logodds_lower2 <- quantile(mult_nom_hier_po2$`orphan_hyper_mean[2]`, prob = c(0.5, 0.025, 0.975))[2]
hv106_level2_dt2 <- data.table(Country = append(country_names, "Pooled"),
                               Effect = append(hv106_level2_logodds_eff2, hv106_pooled_level2_logodds_eff2),
                               Upper = append(hv106_level2_logodds_upper2, hv106_pooled_level2_logodds_upper2),
                               Lower = append(hv106_level2_logodds_lower2, hv106_pooled_level2_logodds_lower2))
# make forest plot
k2 <- ggplot(hv106_level2_dt2, aes(x = Country, y = Effect, ymin = Lower, ymax = Upper)) +
  geom_hline(yintercept = 0, colour = 'grey50') +
  geom_point() +
  geom_errorbar() +
  theme_bw() +
  labs(y = 'log odds of primary education compared to secondary education', x = '') +
  coord_flip()
ggsave(file = file.path(out.dir,'hv106_mult_nom_level2_logoddstrteffect2.png'), k2, w = 6.5, h = 6)
knitr::include_graphics(file.path(out.dir,'hv106_mult_nom_level2_logoddstrteffect2.png'))




## Do risk ratio instead on forest plot
secondary_education_rr_list <- c()
secondary_education_rr_list_upper <- c()
secondary_education_rr_list_lower <- c()
for(i in 1:35){
  countrybaseline_level1 <- paste0("countries_baseline[", i, ",1]")
  countrybaseline_level2 <- paste0("countries_baseline[", i, ",2]")
  countryeffect_level1 <- paste0("countries_orphan[", i, ",1]")
  countryeffect_level2 <- paste0("countries_orphan[", i, ",2]")
  
  secondary_education_rr_num <- 1 +
    exp(mult_nom_hier_po2$`baseline_hyper_mean[1]` + mult_nom_hier_po2[[countrybaseline_level1]]) +
    exp(mult_nom_hier_po2$`baseline_hyper_mean[2]` + mult_nom_hier_po2[[countrybaseline_level2]])
  secondary_education_rr_denom <- 1 +
    exp(mult_nom_hier_po2$`baseline_hyper_mean[1]` + mult_nom_hier_po2[[countrybaseline_level1]] +
          mult_nom_hier_po2$`orphan_hyper_mean[1]` + mult_nom_hier_po2[[countryeffect_level1]]) +
    exp(mult_nom_hier_po2$`baseline_hyper_mean[2]` + mult_nom_hier_po2[[countrybaseline_level2]] +
          mult_nom_hier_po2$`orphan_hyper_mean[2]` + mult_nom_hier_po2[[countryeffect_level2]])
  secondary_education_rr <- secondary_education_rr_num / secondary_education_rr_denom
  secondary_education_rr_list <- append(secondary_education_rr_list,
                                        quantile(secondary_education_rr, probs = c(0.5, 0.025, 0.975))[1])
  secondary_education_rr_list_upper <- append(secondary_education_rr_list_upper,
                                              quantile(secondary_education_rr, probs = c(0.5, 0.025, 0.975))[3])
  secondary_education_rr_list_lower <- append(secondary_education_rr_list_lower,
                                              quantile(secondary_education_rr, probs = c(0.5, 0.025, 0.975))[2])
}

secondary_education_rr_dt <- data.table(Country = country_names,
                                        Effect = secondary_education_rr_list,
                                        Upper = secondary_education_rr_list_upper,
                                        Lower = secondary_education_rr_list_lower)

# Make study list


# make forest plot for risk ratio of secondary education
l <- ggplot(secondary_education_rr_dt, aes(x = Country, y = Effect, ymin = Lower, ymax = Upper)) +
  geom_hline(yintercept = 1, colour = 'grey50') +
  geom_point() +
  geom_errorbar() +
  theme_bw() +
  labs(y = 'Risk ratio of secondary education', x = '') +
  coord_flip()
ggsave(file = file.path(out.dir,'hv106_mult_nom_level3_rr.png'), l, w = 6.5, h = 6)
knitr::include_graphics(file.path(out.dir,'hv106_mult_nom_level3_rr.png'))


## Now do for no education
no_education_rr_list <- c()
no_education_rr_list_upper <- c()
no_education_rr_list_lower <- c()
for(i in 1:35){
  countrybaseline_level1 <- paste0("countries_baseline[", i, ",1]")
  countrybaseline_level2 <- paste0("countries_baseline[", i, ",2]")
  countryeffect_level1 <- paste0("countries_orphan[", i, ",1]")
  countryeffect_level2 <- paste0("countries_orphan[", i, ",2]")
  
  no_education_rr_num1 <- 1 +
    exp(mult_nom_hier_po2$`baseline_hyper_mean[1]` + mult_nom_hier_po2[[countrybaseline_level1]]) +
    exp(mult_nom_hier_po2$`baseline_hyper_mean[2]` + mult_nom_hier_po2[[countrybaseline_level2]])
  no_education_rr_num2 <- exp(mult_nom_hier_po2$`orphan_hyper_mean[1]` + mult_nom_hier_po2[[countryeffect_level1]])
  no_education_rr_denom <- 1 +
    exp(mult_nom_hier_po2$`baseline_hyper_mean[1]` + mult_nom_hier_po2[[countrybaseline_level1]] +
          mult_nom_hier_po2$`orphan_hyper_mean[1]` + mult_nom_hier_po2[[countryeffect_level1]]) +
    exp(mult_nom_hier_po2$`baseline_hyper_mean[2]` + mult_nom_hier_po2[[countrybaseline_level2]] +
          mult_nom_hier_po2$`orphan_hyper_mean[2]` + mult_nom_hier_po2[[countryeffect_level2]])
  
  no_education_rr <- (no_education_rr_num1 * no_education_rr_num2 / no_education_rr_denom)
  no_education_rr_list <- append(no_education_rr_list,
                                 quantile(no_education_rr, probs = c(0.5, 0.025, 0.975))[1])
  no_education_rr_list_upper <- append(no_education_rr_list_upper,
                                       quantile(no_education_rr, probs = c(0.5, 0.025, 0.975))[3])
  no_education_rr_list_lower <- append(no_education_rr_list_lower,
                                       quantile(no_education_rr, probs = c(0.5, 0.025, 0.975))[2])
}

no_education_rr_dt <- data.table(Country = country_names,
                                 Effect = no_education_rr_list,
                                 Upper = no_education_rr_list_upper,
                                 Lower = no_education_rr_list_lower)

# make forest plot for risk ratio of no education
l2 <- ggplot(no_education_rr_dt, aes(x = Country, y = Effect, ymin = Lower, ymax = Upper)) +
  geom_hline(yintercept = 1, colour = 'grey50') +
  geom_point() +
  geom_errorbar() +
  theme_bw() +
  labs(y = 'Risk ratio of no education', x = '') +
  coord_flip()
ggsave(file = file.path(out.dir,'hv106_mult_nom_level1_rr.png'), l2, w = 6.5, h = 6)
knitr::include_graphics(file.path(out.dir,'hv106_mult_nom_level1_rr.png'))




# Now do for primary education
## Now do for primary education
primary_education_rr_list <- c()
primary_education_rr_list_upper <- c()
primary_education_rr_list_lower <- c()
for(i in 1:35){
  countrybaseline_level1 <- paste0("countries_baseline[", i, ",1]")
  countrybaseline_level2 <- paste0("countries_baseline[", i, ",2]")
  countryeffect_level1 <- paste0("countries_orphan[", i, ",1]")
  countryeffect_level2 <- paste0("countries_orphan[", i, ",2]")
  
  primary_education_rr_num1 <- 1 +
    exp(mult_nom_hier_po2$`baseline_hyper_mean[1]` + mult_nom_hier_po2[[countrybaseline_level1]]) +
    exp(mult_nom_hier_po2$`baseline_hyper_mean[2]` + mult_nom_hier_po2[[countrybaseline_level2]])
  primary_education_rr_num2 <- exp(mult_nom_hier_po2$`orphan_hyper_mean[2]` + mult_nom_hier_po2[[countryeffect_level2]])
  primary_education_rr_denom <- 1 +
    exp(mult_nom_hier_po2$`baseline_hyper_mean[1]` + mult_nom_hier_po2[[countrybaseline_level1]] +
          mult_nom_hier_po2$`orphan_hyper_mean[1]` + mult_nom_hier_po2[[countryeffect_level1]]) +
    exp(mult_nom_hier_po2$`baseline_hyper_mean[2]` + mult_nom_hier_po2[[countrybaseline_level2]] +
          mult_nom_hier_po2$`orphan_hyper_mean[2]` + mult_nom_hier_po2[[countryeffect_level2]])
  
  primary_education_rr <- (primary_education_rr_num1 * primary_education_rr_num2 / primary_education_rr_denom)
  primary_education_rr_list <- append(primary_education_rr_list,
                                      quantile(primary_education_rr, probs = c(0.5, 0.025, 0.975))[1])
  primary_education_rr_list_upper <- append(primary_education_rr_list_upper,
                                            quantile(primary_education_rr, probs = c(0.5, 0.025, 0.975))[3])
  primary_education_rr_list_lower <- append(primary_education_rr_list_lower,
                                            quantile(primary_education_rr, probs = c(0.5, 0.025, 0.975))[2])
}

primary_education_rr_dt <- data.table(Country = country_names,
                                      Effect = primary_education_rr_list,
                                      Upper = primary_education_rr_list_upper,
                                      Lower = primary_education_rr_list_lower)

# make forest plot for risk ratio of primary education
l3 <- ggplot(primary_education_rr_dt, aes(x = Country, y = Effect, ymin = Lower, ymax = Upper)) +
  geom_hline(yintercept = 1, colour = 'grey50') +
  geom_point() +
  geom_errorbar() +
  theme_bw() +
  labs(y = 'Risk ratio of primary education', x = '') +
  coord_flip()
ggsave(file = file.path(out.dir,'hv106_mult_nom_level2_rr.png'), l3, w = 6.5, h = 6)
knitr::include_graphics(file.path(out.dir,'hv106_mult_nom_level2_rr.png'))