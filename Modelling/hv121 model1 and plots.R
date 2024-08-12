# MODEL1 (Old)
# Now use Stan binomial GLM for hv121
## Change orphan so 1 is orphan, and 0 is not orphan
for(i in 1:length(hv121_split_data)){
  hv121_split_data[[i]][Orphan == 1, Orphan := 2]
  hv121_split_data[[i]][Orphan == 0, Orphan := 1]
  hv121_split_data[[i]][Orphan == 2, Orphan := 0]
}
table(hv121_split_data[[1]])

# Make the data in long form for Stan
study_col_hv121 <- c()
orphan_col_hv121 <- c()
school_col_hv121 <- c()
total_col_hv121 <- c()
for(i in 1:length(hv121_split_data)){
  table_count <- table(hv121_split_data[[i]])
  study_col_hv121 <- append(study_col_hv121, rep(hv121_split_data[[i]]$SurveyId[1], 2))
  orphan_col_hv121 <- append(orphan_col_hv121, c(0, 1))
  tl <- table(hv121_split_data[[i]])["0", "0", tail(study_col_hv121, n=1)]
  tr <- table(hv121_split_data[[i]])["0", "1", tail(study_col_hv121, n=1)]
  bl <- table(hv121_split_data[[i]])["1", "0", tail(study_col_hv121, n=1)]
  br <- table(hv121_split_data[[i]])["1", "1", tail(study_col_hv121, n=1)]
  school_col_hv121 <- append(school_col_hv121, bl)
  school_col_hv121 <- append(school_col_hv121, br)
  total_col_hv121 <- append(total_col_hv121, tl + bl)
  total_col_hv121 <- append(total_col_hv121, tr + br)
}

hv121_stan_data <- data.table(Study=study_col_hv121, Orphan=orphan_col_hv121,
                              School=school_col_hv121, Total=total_col_hv121)

# characterise prior range
tmp <- hv121_stan_data[Orphan == 0, list(prob_school = (School + .5)/(Total + 5)), by = 'Study']
tmp[, quantile(gtools::logit(prob_school), p = c(0.5, 0.025,0.975))]
# so a normal(0,5) prior captures the median log odds of pain relief

# set prior density sth variance of trt effect = 0 has highest density 
log_bin_hier2_txt <- "
data{
  int<lower=1> N; // number of observations
  int<lower=1> K; // number of units
  int<lower=0> y[N]; 
  int<lower=0> Total[N];
  // multi-indexing as seen before
  vector[N] Treatment; // treatment arm indicator
  int<lower=1> Nk_max; // max number of observations for a unit
  int<lower=1, upper=Nk_max> units_to_obs_length[K]; // number of observations per unit
  int<lower=0, upper=N> units_to_obs[K, Nk_max]; // index of observations per unit, with the rest set to 0
}
parameters{
  // random effects on baseline
  real units_baseline[K];
  real<lower=0> baseline_hyper_sd;
  real baseline_hyper_mean;
  // random effects on treatment effect
  real units_trt[K];
  real<lower=0> trt_hyper_sd;
  real trt_hyper_mean;
}
transformed parameters{
  vector[N] obs_logit_pi;
  
  // fixed effects
  obs_logit_pi = rep_vector( baseline_hyper_mean, N ) + trt_hyper_mean * Treatment;
  //
  // add random effects
  for(k in 1:K)
  {
    //
    // add baseline random effects
    obs_logit_pi[ units_to_obs[k, 1:units_to_obs_length[k] ] ] += 
    rep_vector( units_baseline[k], units_to_obs_length[k]);
    //  
    // add trt random effects  
    obs_logit_pi[ units_to_obs[k, 1:units_to_obs_length[k] ] ] += 
    ( 
    Treatment[ units_to_obs[k, 1:units_to_obs_length[k] ] ] .*
    rep_vector( units_trt[k], units_to_obs_length[k])
    );
  }
}
model{
  baseline_hyper_mean ~ normal(0,5);
  baseline_hyper_sd ~ cauchy(0,1);
  units_baseline ~ normal( 0, baseline_hyper_sd);
  trt_hyper_mean ~ normal(0,3.2);
  trt_hyper_sd ~ exponential(1);
  units_trt ~ normal( 0, trt_hyper_sd);
  y ~ binomial_logit( Total, obs_logit_pi);  
}
"

# compile the model
log_bin_hier2_model_vS_compiled <- rstan::stan_model(
  model_name = 'log_bin_hier2_model_vS', 
  model_code = gsub('\t',' ',log_bin_hier2_txt)
)

# Change study names to numbers
hv121_stan_data2 <- copy(hv121_stan_data)
surv_num <- c()
for(i in 1:35){
  surv_num <- append(surv_num, rep(i, 2))
}
hv121_stan_data2[, 1] <- surv_num

# define data in format needed for model specification
stan_data <- list()
stan_data$N <- nrow(hv121_stan_data2)
stan_data$K <- max(hv121_stan_data2$Study)
stan_data$y <- hv121_stan_data2$School
stan_data$Total <- hv121_stan_data2$Total
stan_data$Treatment <- hv121_stan_data2$Orphan
stan_data$Nk_max <- length(unique(hv121_stan_data2$Orphan))
# define number of observations per unit
tmp <- hv121_stan_data2[, list(LEN = length(Orphan)), by = 'Study']
tmp <- tmp[order(Study)]
stan_data$units_to_obs_length <- tmp$LEN
# define index of observations per unit, with the rest set to 0
tmp <- matrix(data = 0, nrow = stan_data$K, ncol = stan_data$Nk_max)
for (k in 1:stan_data$K){
  tmp2 <- hv121_stan_data2[,which(Study == k)]
  tmp[k, 1:length(tmp2) ] <- tmp2
  stopifnot(length(tmp2) == stan_data$units_to_obs_length[k])
}
stan_data$units_to_obs <- tmp

# run Stan
log_bin_hier2_model_vS_fit <- rstan::sampling(log_bin_hier2_model_vS_compiled, 
                                              data = stan_data, 
                                              warmup = 1e3, iter = 10e3, chains = 1
)

# save
out.dir <- "/Users/hmw850/OneDrive - Queen Mary, University of London/Documents/UROP1/DHS"
saveRDS(log_bin_hier2_model_vS_fit, file = file.path(out.dir, 'hv121_log_bin_hier2_model_vS_fit.rds'))

# Converging and mixing
# calculate Rhat and neff and summaries of marginal posterior densities
library(rstan)
log_bin_hier2_model_vS_su <- summary(log_bin_hier2_model_vS_fit)$summary
log_bin_hier2_model_vS_su <- log_bin_hier2_model_vS_su[grepl("hyper|units|lp",rownames(log_bin_hier2_model_vS_su)), ] 
min(log_bin_hier2_model_vS_su[,'n_eff'])
#> [1] 227.414
max(log_bin_hier2_model_vS_su[,'Rhat'])
#> [1] 1.000256

# Forest plot of log odds orphan effect
# calculate treatment effect for each Study (Monte Carlo samples)
po <- rstan::extract(log_bin_hier2_model_vS_fit)
pot <- as.data.table(reshape2::melt(po$units_trt))
setnames(pot, 2:3, c('study','trt_rnd_eff'))
tmp <- data.table(trt_hyper_mean = po$trt_hyper_mean, 
                  iterations = seq_along(po$trt_hyper_mean)
)
pot <- merge(pot, tmp, by = 'iterations')
pot[, trt := trt_hyper_mean + trt_rnd_eff]
pot <- subset(pot, select = c(iterations, study, trt))

# extract pooled treatment effect estimate
tmp <- data.table(trt = po$trt_hyper_mean, 
                  iterations = seq_along(po$trt_hyper_mean), 
                  study = 0L)
pot <- rbind(pot, tmp)

# summarise posterior estimates of log odds of pain relief
pots <- pot[, list(value = quantile(trt, prob = c(0.5, 0.025, 0.975)), variable = c('M','CL','CU')), by = 'study']
pots[, study_label := paste0('Study ', study)]
setkey(pots, study)
set(pots, NULL, 'study_label', pots[, gsub('Study 0','Pooled',study_label)])
set(pots, NULL, 'study_label', pots[, factor(study, levels = study, labels = study_label)])
pots <- dcast.data.table(pots, study_label~variable, value.var = 'value')

# make forest plot
library(ggplot2)
p <- ggplot(pots, aes(x = study_label, y = M, ymin = CL, ymax = CU)) +
  geom_hline(yintercept = 0, colour = 'grey50') +
  geom_point() +
  geom_errorbar() +
  theme_bw() +
  labs(y = 'log odds of school attendance', x = '') +
  coord_flip()
ggsave(file = file.path(out.dir,'hv121_log_bin_hier2_logoddstrteffect.png'), p, w = 5, h = 6)
knitr::include_graphics(file.path(out.dir,'hv121_log_bin_hier2_logoddstrteffect.png'))


# Risk reduction in school attendance
# summarise posterior estimates of difference in probability of pain relief

# get Monte Carlo samples into long format
po <- rstan::extract(log_bin_hier2_model_vS_fit)
pot <- as.data.table(reshape2::melt(po$units_trt))
setnames(pot, 2:3, c('study','trt_rnd_eff'))
tmp <- as.data.table(reshape2::melt(po$units_baseline))
setnames(tmp, 2:3, c('study','baseline_rnd_eff'))
pot <- merge(pot, tmp, by = c('iterations','study'))
tmp <- data.table(base_hyper_mean = po$baseline_hyper_mean, iterations = seq_along(po$baseline_hyper_mean))
pot <- merge(pot, tmp, by = 'iterations')
tmp <- data.table(trt_hyper_mean = po$trt_hyper_mean, iterations = seq_along(po$trt_hyper_mean))
pot <- merge(pot, tmp, by = 'iterations')

# prob no pain relief in control and treatment group
pot[, control_no_pain_relief := 1 - gtools::inv.logit(base_hyper_mean + baseline_rnd_eff)]
pot[, trt_no_pain_relief := 1 - gtools::inv.logit(base_hyper_mean + baseline_rnd_eff + trt_hyper_mean + trt_rnd_eff)]

# summarise risk reduction
pots <- pot[, 
            list(
              value = quantile(control_no_pain_relief - trt_no_pain_relief, prob = c(0.5, 0.025, 0.975)), 
              variable = c('M','CL','CU')
            ), 
            by = 'study']
pots[, study_label := paste0('Study ', study)]
setkey(pots, study)
set(pots, NULL, 'study_label', pots[, gsub('Study 0','Pooled',study_label)])
set(pots, NULL, 'study_label', pots[, factor(study, levels = study, labels = study_label)])
pots <- dcast.data.table(pots, study_label~variable, value.var = 'value')

# plot
p <- ggplot(pots, aes(x = study_label, y = M, ymin = CL, ymax = CU)) +
  geom_hline(yintercept = 0, colour = 'grey50') +
  geom_point() +
  geom_errorbar() +
  theme_bw() +
  scale_y_continuous(labels = scales::percent) +
  labs(y = 'risk reduction in school attendance', x = '') +
  coord_flip()
ggsave(file = file.path(out.dir,'hv121_log_bin_hier2_difftrteffect.png'), p, w = 5, h = 6)
knitr::include_graphics(file.path(out.dir,'hv121_log_bin_hier2_difftrteffect.png'))


# Number needed to treat
# summarise posterior estimates of number needed to treat to prevent one unfavourable outcome (pain)
pots <- pot[, 
            list(
              value = quantile(1/(control_no_pain_relief - trt_no_pain_relief), prob = c(0.5, 0.025, 0.975)), 
              variable = c('M','CL','CU')
            ), 
            by = 'study']
pots[, study_label := paste0('Study ', study)]
setkey(pots, study)
set(pots, NULL, 'study_label', pots[, gsub('Study 0','Pooled',study_label)])
set(pots, NULL, 'study_label', pots[, factor(study, levels = study, labels = study_label)])
pots <- dcast.data.table(pots, study_label~variable, value.var = 'value')

# plot
p <- ggplot(pots, aes(x = study_label, y = M, ymin = CL, ymax = CU)) +
  geom_hline(yintercept = 1, colour = 'grey50') +
  geom_point() +
  geom_errorbar() +
  theme_bw() +
  labs(y = 'number needed to treat', x = '') +
  coord_flip() +
  scale_y_log10()
ggsave(file = file.path(out.dir,'hv121_log_bin_hier2_nnttrteffect.png'), p, w = 5, h = 6)
knitr::include_graphics(file.path(out.dir,'hv121_log_bin_hier2_nnttrteffect.png'))

# L'abbe plot
# summarise heterogeneity in control and treatment group (L'Abbe plot)

# risk reduction
pot[, risk_reduction := control_no_pain_relief - trt_no_pain_relief]

# precision in estimated risk reduction (inverse variance)
tmp <- pot[, list(risk_reduction_prec = 1/var(risk_reduction) ), by = 'study']

# summarise probs no pain relief
pot <- melt(pot, id.vars = c('iterations','study'), measure.vars = c('control_no_pain_relief','trt_no_pain_relief'))
pots <- pot[, list(value = median(value)), by = c('study','variable')]
pots <- dcast.data.table(pots, study~variable, value.var = 'value')

# merge precision in estimated risk reduction (inverse variance)
pots <- merge(pots, tmp, by = 'study')

# prepare plotting
pots[, study_label := paste0('Study ', study)]
setkey(pots, study)
set(pots, NULL, 'study_label', pots[, factor(study, levels = study, labels = study_label)])

# plot
p <- ggplot(pots, aes(x = control_no_pain_relief, y = trt_no_pain_relief, size = risk_reduction_prec)) +
  geom_abline(intercept = 0, slope = 1, colour = 'grey50') +
  geom_point() +
  theme_bw() +
  scale_x_continuous(labels = scales::percent, expand = c(0,0), limits = c(0,1)) +
  scale_y_continuous(labels = scales::percent, expand = c(0,0), limits = c(0,1)) +
  labs(
    y = 'prob not attend school, orphan group', 
    x = 'prob not attend school, control group', 
    size = 'precision\nin estimated\nrisk\nreduction'
  )
ggsave(file = file.path(out.dir,'hv121_log_bin_hier2_labbeplot.png'), p, w = 6, h = 5)
knitr::include_graphics(file.path(out.dir,'hv121_log_bin_hier2_labbeplot.png'))

########

# MODEL1 (Current)
## Now, only focus of children aged 5 or older, since majority of children aged <5 do not go to school, regardless of orphan status
# Show tables
table(data_filtered[data_filtered$hv105 == 2]$hv121)
table(data_filtered[data_filtered$hv105 == 3]$hv121)
table(data_filtered[data_filtered$hv105 == 4]$hv121)
table(data_filtered[data_filtered$hv105 == 5]$hv121)

# Make a histogram of orphan ages
# Extract the hv105 data
data_filtered2 <- data_filtered[data_filtered$hv105 >= 5]
hv105_data2 <- data_filtered2[, c("hv105", "Orphan", "SurveyId")]
orphan_ages2 <- hv105_data2[hv105_data2$Orphan == 0, ]
hist(orphan_ages2$hv105)

# The typical minimum school age is around 5
# Extract the hv121 data
hv121_data2 <- data_filtered2[, c("hv121", "Orphan", "SurveyId")]
# Split the data by country
hv121_split_data2 <- split(hv121_data2, by = "SurveyId")
# To fix the issue, combine the values 1 and 2, turn 2 into 1
for(i in 1:length(hv121_split_data2)){
  hv121_split_data2[[i]][hv121 == 2, hv121 := 1]
  print(table(hv121_split_data2[[i]]))
}
# Change orphan so 1 is orphan, and 0 is not orphan
for(i in 1:length(hv121_split_data2)){
  hv121_split_data2[[i]][Orphan == 1, Orphan := 2]
  hv121_split_data2[[i]][Orphan == 0, Orphan := 1]
  hv121_split_data2[[i]][Orphan == 2, Orphan := 0]
}
table(hv121_split_data2[[2]])
# Make the data in long form for Stan
study_col_hv121_2 <- c()
orphan_col_hv121_2 <- c()
school_col_hv121_2 <- c()
total_col_hv121_2 <- c()
for(i in 1:length(hv121_split_data2)){
  table_count2 <- table(hv121_split_data2[[i]])
  study_col_hv121_2 <- append(study_col_hv121_2, rep(hv121_split_data2[[i]]$SurveyId[1], 2))
  orphan_col_hv121_2 <- append(orphan_col_hv121_2, c(0, 1))
  tl2 <- table(hv121_split_data2[[i]])["0", "0", tail(study_col_hv121_2, n=1)]
  tr2 <- table(hv121_split_data2[[i]])["0", "1", tail(study_col_hv121_2, n=1)]
  bl2 <- table(hv121_split_data2[[i]])["1", "0", tail(study_col_hv121_2, n=1)]
  br2 <- table(hv121_split_data2[[i]])["1", "1", tail(study_col_hv121_2, n=1)]
  school_col_hv121_2 <- append(school_col_hv121_2, bl2)
  school_col_hv121_2 <- append(school_col_hv121_2, br2)
  total_col_hv121_2 <- append(total_col_hv121_2, tl2 + bl2)
  total_col_hv121_2 <- append(total_col_hv121_2, tr2 + br2)
}
hv121_stan_data22 <- data.table(Study=study_col_hv121_2, Orphan=orphan_col_hv121_2,
                                School=school_col_hv121_2, Total=total_col_hv121_2)
# characterise prior range
tmp <- hv121_stan_data22[Orphan == 0, list(prob_school = (School + .5)/(Total + 5)), by = 'Study']
tmp[, quantile(gtools::logit(prob_school), p = c(0.5, 0.025,0.975))]
# so a normal(0,5) prior captures the median log odds of pain relief
# set prior density sth variance of trt effect = 0 has highest density 
log_bin_hier3_txt <- "
data {
  int<lower=1> N; // number of observations
  int<lower=1> K; // number of units
  int<lower=0> y[N]; 
  int<lower=0> Total[N];
  // multi-indexing as seen before
  vector[N] Treatment; // treatment arm indicator
  int<lower=1> Nk_max; // max number of observations for a unit
  int<lower=1, upper=Nk_max> units_to_obs_length[K]; // number of observations per unit
  int<lower=0, upper=N> units_to_obs[K, Nk_max]; // index of observations per unit, with the rest set to 0
}
parameters {
  // random effects on baseline
  real units_baseline[K];
  real<lower=0> baseline_hyper_sd;
  real baseline_hyper_mean;
  // random effects on treatment effect
  real units_trt[K];
  real<lower=0> trt_hyper_sd;
  real trt_hyper_mean;
}
transformed parameters {
  vector[N] obs_logit_pi;
  
  // fixed effects
  obs_logit_pi = rep_vector( baseline_hyper_mean, N ) + trt_hyper_mean * Treatment;
  //
  // add random effects
  for(k in 1:K)
  {
    //
    // add baseline random effects
    obs_logit_pi[ units_to_obs[k, 1:units_to_obs_length[k] ] ] += 
    rep_vector( units_baseline[k], units_to_obs_length[k]);
    //  
    // add trt random effects  
    obs_logit_pi[ units_to_obs[k, 1:units_to_obs_length[k] ] ] += 
    ( 
    Treatment[ units_to_obs[k, 1:units_to_obs_length[k] ] ] .*
    rep_vector( units_trt[k], units_to_obs_length[k])
    );
  }
}
model {
  baseline_hyper_mean ~ normal(0,5);
  baseline_hyper_sd ~ cauchy(0,1);
  units_baseline ~ normal( 0, baseline_hyper_sd);
  trt_hyper_mean ~ normal(0,3.2);
  trt_hyper_sd ~ exponential(1);
  units_trt ~ normal( 0, trt_hyper_sd);
  y ~ binomial_logit( Total, obs_logit_pi);  
}
"
# compile the model
log_bin_hier3_model_vS_compiled <- rstan::stan_model(
  model_name = 'log_bin_hier3_model_vS', 
  model_code = gsub('\t',' ',log_bin_hier3_txt)
)
# Change study names to numbers
hv121_stan_data222 <- copy(hv121_stan_data22)
surv_num <- c()
for(i in 1:35){
  surv_num <- append(surv_num, rep(i, 2))
}
hv121_stan_data222[, 1] <- surv_num
# define data in format needed for model specification
stan_data <- list()
stan_data$N <- nrow(hv121_stan_data222)
stan_data$K <- max(hv121_stan_data222$Study)
stan_data$y <- hv121_stan_data222$School
stan_data$Total <- hv121_stan_data222$Total
stan_data$Treatment <- hv121_stan_data222$Orphan
stan_data$Nk_max <- length(unique(hv121_stan_data222$Orphan))
# define number of observations per unit
tmp <- hv121_stan_data222[, list(LEN = length(Orphan)), by = 'Study']
tmp <- tmp[order(Study)]
stan_data$units_to_obs_length <- tmp$LEN
# define index of observations per unit, with the rest set to 0
tmp <- matrix(data = 0, nrow = stan_data$K, ncol = stan_data$Nk_max)
for (k in 1:stan_data$K){
  tmp2 <- hv121_stan_data222[,which(Study == k)]
  tmp[k, 1:length(tmp2) ] <- tmp2
  stopifnot(length(tmp2) == stan_data$units_to_obs_length[k])
}
stan_data$units_to_obs <- tmp
# run Stan
log_bin_hier3_model_vS_fit <- rstan::sampling(log_bin_hier3_model_vS_compiled, 
                                              data = stan_data, 
                                              warmup = 1e3, iter = 10e3, chains = 1
)
# save
out.dir <- "/Users/hmw850/OneDrive - Queen Mary, University of London/Documents/UROP1/DHS"
saveRDS(log_bin_hier3_model_vS_fit, file = file.path(out.dir, 'hv121_log_bin_hier3_model_vS_fit.rds'))
# Converging and mixing
# calculate Rhat and neff and summaries of marginal posterior densities
log_bin_hier3_model_vS_su <- summary(log_bin_hier3_model_vS_fit)$summary
log_bin_hier3_model_vS_su <- log_bin_hier3_model_vS_su[grepl("hyper|units|lp",rownames(log_bin_hier3_model_vS_su)), ] 
min(log_bin_hier3_model_vS_su[,'n_eff'])
#> [1] 463.2957
max(log_bin_hier3_model_vS_su[,'Rhat'])
#> [1] 1.000669

# Use country names instead of study 1, study 2 etc.
country_names <- c("Lesotho", "Zambia", "Burkina Faso", "Uganda", "Mauritania", "Congo Democratic Republic",
                   "Guinea", "Togo", "Chad", "Liberia", "Namibia", "Malawi", "Sierra Leone", "Mozambique",
                   "Zimbabwe", "Benin", "Congo", "Mali", "Gambia", "Comoros", "Madagascar", "Nigeria",
                   "Tanzania", "Ghana", "South Africa", "Gabon", "Rwanda", "Cote d'Ivoire", "Angola",
                   "Niger", "Cameroon", "Kenya", "Burundi", "Sao Tome and Principe", "Senegal")
country_names3 <- c()
for(i in 1:35){
  country_names3 <- append(country_names3, rep(country_names[i], 3))
}

# Forest plot of log odds orphan effect
# calculate treatment effect for each Study (Monte Carlo samples)
po <- rstan::extract(log_bin_hier3_model_vS_fit)

# NEW:
test_dt <- data.table(Effect = colMeans(po$units_baseline), Country = country_names)
test_dt <- test_dt[order(test_dt$Country)]
test_dt[,Country:=NULL]
test_dt$Country <- country_codes
test_dt <- test_dt[order(test_dt$Country)]

pot <- as.data.table(reshape2::melt(po$units_trt))
setnames(pot, 2:3, c('study','trt_rnd_eff'))
tmp <- data.table(trt_hyper_mean = po$trt_hyper_mean, 
                  iterations = seq_along(po$trt_hyper_mean)
)

pot <- merge(pot, tmp, by = 'iterations')
pot[, trt := trt_hyper_mean + trt_rnd_eff]
pot <- subset(pot, select = c(iterations, study, trt))

# extract pooled treatment effect estimate
tmp <- data.table(trt = po$trt_hyper_mean, 
                  iterations = seq_along(po$trt_hyper_mean), 
                  study = 0L)
pot <- rbind(pot, tmp)

# summarise posterior estimates of log odds of pain relief
pots <- pot[, list(value = quantile(trt, prob = c(0.5, 0.025, 0.975)), variable = c('M','CL','CU')), by = 'study']
pots[, study_label := paste0('Study ', study)]
pots$study_label <- append(country_names3, rep("Study 0", 3))
setkey(pots, study)
set(pots, NULL, 'study_label', pots[, gsub('Study 0','Pooled',study_label)])
set(pots, NULL, 'study_label', pots[, factor(study, levels = study, labels = study_label)])
pots <- dcast.data.table(pots, study_label~variable, value.var = 'value')

# make forest plot
p <- ggplot(pots, aes(x = study_label, y = M, ymin = CL, ymax = CU)) +
  geom_hline(yintercept = 0, colour = 'grey50') +
  geom_point() +
  geom_errorbar() +
  theme_bw() +
  labs(y = 'log odds of school attendance', x = '') +
  coord_flip()
ggsave(file = file.path(out.dir,'hv121_log_bin_hier3_logoddstrteffect.png'), p, w = 5, h = 6)
knitr::include_graphics(file.path(out.dir,'hv121_log_bin_hier3_logoddstrteffect.png'))
# Risk reduction in school attendance
# summarise posterior estimates of difference in probability of pain relief
# get Monte Carlo samples into long format
po <- rstan::extract(log_bin_hier3_model_vS_fit)
pot <- as.data.table(reshape2::melt(po$units_trt))
setnames(pot, 2:3, c('study','trt_rnd_eff'))
tmp <- as.data.table(reshape2::melt(po$units_baseline))
setnames(tmp, 2:3, c('study','baseline_rnd_eff'))
pot <- merge(pot, tmp, by = c('iterations','study'))
tmp <- data.table(base_hyper_mean = po$baseline_hyper_mean, iterations = seq_along(po$baseline_hyper_mean))
pot <- merge(pot, tmp, by = 'iterations')
tmp <- data.table(trt_hyper_mean = po$trt_hyper_mean, iterations = seq_along(po$trt_hyper_mean))
pot <- merge(pot, tmp, by = 'iterations')
# prob no pain relief in control and treatment group
pot[, control_no_pain_relief := 1 - gtools::inv.logit(base_hyper_mean + baseline_rnd_eff)]
pot[, trt_no_pain_relief := 1 - gtools::inv.logit(base_hyper_mean + baseline_rnd_eff + trt_hyper_mean + trt_rnd_eff)]
# summarise risk reduction
pots <- pot[, 
            list(
              value = quantile(control_no_pain_relief - trt_no_pain_relief, prob = c(0.5, 0.025, 0.975)), 
              variable = c('M','CL','CU')
            ), 
            by = 'study']
pots[, study_label := paste0('Study ', study)]
setkey(pots, study)
set(pots, NULL, 'study_label', pots[, gsub('Study 0','Pooled',study_label)])
set(pots, NULL, 'study_label', pots[, factor(study, levels = study, labels = study_label)])
pots <- dcast.data.table(pots, study_label~variable, value.var = 'value')
# plot
p <- ggplot(pots, aes(x = study_label, y = M, ymin = CL, ymax = CU)) +
  geom_hline(yintercept = 0, colour = 'grey50') +
  geom_point() +
  geom_errorbar() +
  theme_bw() +
  scale_y_continuous(labels = scales::percent) +
  labs(y = 'risk reduction in school attendance', x = '') +
  coord_flip()
ggsave(file = file.path(out.dir,'hv121_log_bin_hier3_difftrteffect.png'), p, w = 5, h = 6)
knitr::include_graphics(file.path(out.dir,'hv121_log_bin_hier3_difftrteffect.png'))
# Number needed to treat
# summarise posterior estimates of number needed to treat to prevent one unfavourable outcome (pain)
pots <- pot[, 
            list(
              value = quantile(1/(control_no_pain_relief - trt_no_pain_relief), prob = c(0.5, 0.025, 0.975)), 
              variable = c('M','CL','CU')
            ), 
            by = 'study']
pots[, study_label := paste0('Study ', study)]
setkey(pots, study)
set(pots, NULL, 'study_label', pots[, gsub('Study 0','Pooled',study_label)])
set(pots, NULL, 'study_label', pots[, factor(study, levels = study, labels = study_label)])
pots <- dcast.data.table(pots, study_label~variable, value.var = 'value')
# plot
p <- ggplot(pots, aes(x = study_label, y = M, ymin = CL, ymax = CU)) +
  geom_hline(yintercept = 1, colour = 'grey50') +
  geom_point() +
  geom_errorbar() +
  theme_bw() +
  labs(y = 'number needed to treat', x = '') +
  coord_flip() +
  scale_y_log10()
ggsave(file = file.path(out.dir,'hv121_log_bin_hier3_nnttrteffect.png'), p, w = 5, h = 6)
knitr::include_graphics(file.path(out.dir,'hv121_log_bin_hier3_nnttrteffect.png'))

# L'abbe plot
# summarise heterogeneity in control and treatment group (L'Abbe plot)
# risk reduction
pot[, risk_reduction := control_no_pain_relief - trt_no_pain_relief]
# precision in estimated risk reduction (inverse variance)
tmp <- pot[, list(risk_reduction_prec = 1/var(risk_reduction) ), by = 'study']
# summarise probs no pain relief
pot <- melt(pot, id.vars = c('iterations','study'), measure.vars = c('control_no_pain_relief','trt_no_pain_relief'))
pots <- pot[, list(value = median(value)), by = c('study','variable')]
pots <- dcast.data.table(pots, study~variable, value.var = 'value')
# merge precision in estimated risk reduction (inverse variance)
pots <- merge(pots, tmp, by = 'study')
# prepare plotting
pots[, study_label := paste0('Study ', study)]
setkey(pots, study)
set(pots, NULL, 'study_label', pots[, factor(study, levels = study, labels = study_label)])
# plot
p <- ggplot(pots, aes(x = control_no_pain_relief, y = trt_no_pain_relief, size = risk_reduction_prec)) +
  geom_abline(intercept = 0, slope = 1, colour = 'grey50') +
  geom_point() +
  theme_bw() +
  scale_x_continuous(labels = scales::percent, expand = c(0,0), limits = c(0,1)) +
  scale_y_continuous(labels = scales::percent, expand = c(0,0), limits = c(0,1)) +
  labs(
    y = 'prob not attend school, orphan group', 
    x = 'prob not attend school, control group', 
    size = 'precision\nin estimated\nrisk\nreduction'
  )
ggsave(file = file.path(out.dir,'hv121_log_bin_hier3_labbeplot.png'), p, w = 6, h = 5)
knitr::include_graphics(file.path(out.dir,'hv121_log_bin_hier3_labbeplot.png'))

########

## Create map to show log odds orphanhood effect on hv121
# Collect data required
# Forest plot of log odds orphan effect
# calculate treatment effect for each Study (Monte Carlo samples)
po <- rstan::extract(log_bin_hier3_model_vS_fit)
pot <- as.data.table(reshape2::melt(po$units_trt))
setnames(pot, 2:3, c('study','trt_rnd_eff'))
tmp <- data.table(trt_hyper_mean = po$trt_hyper_mean, 
                  iterations = seq_along(po$trt_hyper_mean)
)
pot <- merge(pot, tmp, by = 'iterations')
pot[, trt := trt_hyper_mean + trt_rnd_eff]
pot <- subset(pot, select = c(iterations, study, trt))
# extract pooled treatment effect estimate
tmp <- data.table(trt = po$trt_hyper_mean, 
                  iterations = seq_along(po$trt_hyper_mean), 
                  study = 0L)
pot <- rbind(pot, tmp)
# summarise posterior estimates of log odds of pain relief
pots <- pot[, list(value = quantile(trt, prob = c(0.5, 0.025, 0.975)), variable = c('M','CL','CU')), by = 'study']
pots[, study_label := paste0('Study ', study)]
pots$study_label <- append(country_names3, rep("Study 0", 3))
setkey(pots, study)
set(pots, NULL, 'study_label', pots[, gsub('Study 0','Pooled',study_label)])
set(pots, NULL, 'study_label', pots[, factor(study, levels = study, labels = study_label)])
pots <- dcast.data.table(pots, study_label~variable, value.var = 'value')

## Install required packages to make choropleth map
install.packages(c("sf", "ggplot2", "rnaturalearth", "dplyr"))
# Load the libraries
library(sf)
library(ggplot2)
library(rnaturalearth)
library(dplyr)

# Filter for just the median and ignore pooled
choropleth_map_data <- pots[-1, c('study_label', 'M')]

# Get Africa shapefile
africa <- ne_countries(continent = 'Africa', returnclass = 'sf')
# Merge your data with the shapefile
africa_data <- africa %>%
  left_join(choropleth_map_data, by = c("name" = "study_label"))
# Create the choropleth map
choropleth_map <- ggplot(data = africa_data) +
  geom_sf(aes(fill = M), color = "black") +
  scale_fill_gradient(low = "red", high = "green", na.value = "grey50") +
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  ) +
  labs(
    title = "Log odds of school attendence (hv121) by Country",
    fill = "Log effect"
  )
ggsave(file = file.path(out.dir,'choropleth_map.png'), choropleth_map, w = 6, h = 5)
knitr::include_graphics(file.path(out.dir,'choropleth_map.png'))