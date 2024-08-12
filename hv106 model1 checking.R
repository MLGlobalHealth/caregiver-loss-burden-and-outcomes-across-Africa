# TODO: Create model checking plots
# This is sorted by country_names
# Find empirical estimates for each of the 8 categories for each country
# Remember we dont include higher education, since so little data
hv106_count_table_prob <- copy(hv106_count_table)
hv106_count_table_prob[,HigherEducation:=NULL]
hv106_count_table_prob$Total <- hv106_count_table_prob$NoEducation + hv106_count_table_prob$PrimaryEducation +
  hv106_count_table_prob$SecondaryEducation
hv106_empirical_prob_dt <- data.table(Country = rep(country_names, each = 2),
                                      OrphanStatus = hv106_count_table_prob$OrphanStatus,
                                      NoEducationProb = hv106_count_table_prob$NoEducation / hv106_count_table_prob$Total,
                                      PrimaryEducationProb = hv106_count_table_prob$PrimaryEducation / hv106_count_table_prob$Total,
                                      SecondaryEducationProb = hv106_count_table_prob$SecondaryEducation / hv106_count_table_prob$Total)
## Find model probabilities
# Do orphan (1) first
hv106_model_prob_list <- c()
hv106_model_prob_upper_list <- c()
hv106_model_prob_lower_list <- c()
for(i in 1:35){
  countrybaseline_level1 <- paste0("countries_baseline[", i, ",1]")
  countrybaseline_level2 <- paste0("countries_baseline[", i, ",2]")
  countryeffect_level1 <- paste0("countries_orphan[", i, ",1]")
  countryeffect_level2 <- paste0("countries_orphan[", i, ",2]")
  hv106_model_prob_noedu_orphan <- (exp(mult_nom_hier_po2$`baseline_hyper_mean[1]` + mult_nom_hier_po2[[countrybaseline_level1]] +
                                          mult_nom_hier_po2$`orphan_hyper_mean[1]` + mult_nom_hier_po2[[countryeffect_level1]])) /
    (1 +
       exp(mult_nom_hier_po2$`baseline_hyper_mean[1]` + mult_nom_hier_po2[[countrybaseline_level1]] +
             mult_nom_hier_po2$`orphan_hyper_mean[1]` + mult_nom_hier_po2[[countryeffect_level1]]) +
       exp(mult_nom_hier_po2$`baseline_hyper_mean[2]` + mult_nom_hier_po2[[countrybaseline_level2]] +
             mult_nom_hier_po2$`orphan_hyper_mean[2]` + mult_nom_hier_po2[[countryeffect_level2]]))
  hv106_model_prob_priedu_orphan <- (exp(mult_nom_hier_po2$`baseline_hyper_mean[2]` + mult_nom_hier_po2[[countrybaseline_level2]] +
                                           mult_nom_hier_po2$`orphan_hyper_mean[2]` + mult_nom_hier_po2[[countryeffect_level2]])) /
    (1 +
       exp(mult_nom_hier_po2$`baseline_hyper_mean[1]` + mult_nom_hier_po2[[countrybaseline_level1]] +
             mult_nom_hier_po2$`orphan_hyper_mean[1]` + mult_nom_hier_po2[[countryeffect_level1]]) +
       exp(mult_nom_hier_po2$`baseline_hyper_mean[2]` + mult_nom_hier_po2[[countrybaseline_level2]] +
             mult_nom_hier_po2$`orphan_hyper_mean[2]` + mult_nom_hier_po2[[countryeffect_level2]]))
  hv106_model_prob_secedu_orphan <- (1) /
    (1 +
       exp(mult_nom_hier_po2$`baseline_hyper_mean[1]` + mult_nom_hier_po2[[countrybaseline_level1]] +
             mult_nom_hier_po2$`orphan_hyper_mean[1]` + mult_nom_hier_po2[[countryeffect_level1]]) +
       exp(mult_nom_hier_po2$`baseline_hyper_mean[2]` + mult_nom_hier_po2[[countrybaseline_level2]] +
             mult_nom_hier_po2$`orphan_hyper_mean[2]` + mult_nom_hier_po2[[countryeffect_level2]]))
  hv106_model_prob_noedu_nonorphan <- (exp(mult_nom_hier_po2$`baseline_hyper_mean[1]` + mult_nom_hier_po2[[countrybaseline_level1]])) /
    (1 + exp(mult_nom_hier_po2$`baseline_hyper_mean[1]` + mult_nom_hier_po2[[countrybaseline_level1]]) + exp(mult_nom_hier_po2$`baseline_hyper_mean[2]` + mult_nom_hier_po2[[countrybaseline_level2]]))
  hv106_model_prob_priedu_nonorphan <- (exp(mult_nom_hier_po2$`baseline_hyper_mean[2]` + mult_nom_hier_po2[[countrybaseline_level2]])) /
    (1 + exp(mult_nom_hier_po2$`baseline_hyper_mean[1]` + mult_nom_hier_po2[[countrybaseline_level1]]) + exp(mult_nom_hier_po2$`baseline_hyper_mean[2]` + mult_nom_hier_po2[[countrybaseline_level2]]))
  hv106_model_prob_secedu_nonorphan <- (1) /
    (1 + exp(mult_nom_hier_po2$`baseline_hyper_mean[1]` + mult_nom_hier_po2[[countrybaseline_level1]]) + exp(mult_nom_hier_po2$`baseline_hyper_mean[2]` + mult_nom_hier_po2[[countrybaseline_level2]]))
  
  # Append orphan quantile values to list
  hv106_model_prob_list <- append(hv106_model_prob_list,
                                  quantile(hv106_model_prob_noedu_orphan, probs = c(0.5, 0.025, 0.975))[1])
  hv106_model_prob_upper_list <- append(hv106_model_prob_upper_list,
                                        quantile(hv106_model_prob_noedu_orphan, probs = c(0.5, 0.025, 0.975))[3])
  hv106_model_prob_lower_list <- append(hv106_model_prob_lower_list,
                                        quantile(hv106_model_prob_noedu_orphan, probs = c(0.5, 0.025, 0.975))[2])
  hv106_model_prob_list <- append(hv106_model_prob_list,
                                  quantile(hv106_model_prob_priedu_orphan, probs = c(0.5, 0.025, 0.975))[1])
  hv106_model_prob_upper_list <- append(hv106_model_prob_upper_list,
                                        quantile(hv106_model_prob_priedu_orphan, probs = c(0.5, 0.025, 0.975))[3])
  hv106_model_prob_lower_list <- append(hv106_model_prob_lower_list,
                                        quantile(hv106_model_prob_priedu_orphan, probs = c(0.5, 0.025, 0.975))[2])
  hv106_model_prob_list <- append(hv106_model_prob_list,
                                  quantile(hv106_model_prob_secedu_orphan, probs = c(0.5, 0.025, 0.975))[1])
  hv106_model_prob_upper_list <- append(hv106_model_prob_upper_list,
                                        quantile(hv106_model_prob_secedu_orphan, probs = c(0.5, 0.025, 0.975))[3])
  hv106_model_prob_lower_list <- append(hv106_model_prob_lower_list,
                                        quantile(hv106_model_prob_secedu_orphan, probs = c(0.5, 0.025, 0.975))[2])
  # Append nonorphan quantile values to list
  hv106_model_prob_list <- append(hv106_model_prob_list,
                                  quantile(hv106_model_prob_noedu_nonorphan, probs = c(0.5, 0.025, 0.975))[1])
  hv106_model_prob_upper_list <- append(hv106_model_prob_upper_list,
                                        quantile(hv106_model_prob_noedu_nonorphan, probs = c(0.5, 0.025, 0.975))[3])
  hv106_model_prob_lower_list <- append(hv106_model_prob_lower_list,
                                        quantile(hv106_model_prob_noedu_nonorphan, probs = c(0.5, 0.025, 0.975))[2])
  hv106_model_prob_list <- append(hv106_model_prob_list,
                                  quantile(hv106_model_prob_priedu_nonorphan, probs = c(0.5, 0.025, 0.975))[1])
  hv106_model_prob_upper_list <- append(hv106_model_prob_upper_list,
                                        quantile(hv106_model_prob_priedu_nonorphan, probs = c(0.5, 0.025, 0.975))[3])
  hv106_model_prob_lower_list <- append(hv106_model_prob_lower_list,
                                        quantile(hv106_model_prob_priedu_nonorphan, probs = c(0.5, 0.025, 0.975))[2])
  hv106_model_prob_list <- append(hv106_model_prob_list,
                                  quantile(hv106_model_prob_secedu_nonorphan, probs = c(0.5, 0.025, 0.975))[1])
  hv106_model_prob_upper_list <- append(hv106_model_prob_upper_list,
                                        quantile(hv106_model_prob_secedu_nonorphan, probs = c(0.5, 0.025, 0.975))[3])
  hv106_model_prob_lower_list <- append(hv106_model_prob_lower_list,
                                        quantile(hv106_model_prob_secedu_nonorphan, probs = c(0.5, 0.025, 0.975))[2])
}
# For each country, have orphan no education, primary education, secondary education, then same for non orphan
hv106_model_prob_dt <- data.table(Country = rep(country_names, each = 6),
                                  Orphan = rep(c(1, 1, 1, 0, 0, 0), 35),
                                  Prob = hv106_model_prob_list,
                                  Upper = hv106_model_prob_upper_list,
                                  Lower = hv106_model_prob_lower_list,
                                  Label = rep(c("No Education", "Primary Education", "Secondary Education"), 70))

# Reshape the empirical data table to fit this style
hv106_empirical_prob_list <- c()
for(i in 1:nrow(hv106_empirical_prob)){
  hv106_empirical_prob_list <- append(hv106_empirical_prob_list, hv106_empirical_prob_dt[i,3][[1]])
  hv106_empirical_prob_list <- append(hv106_empirical_prob_list, hv106_empirical_prob_dt[i,4][[1]])
  hv106_empirical_prob_list <- append(hv106_empirical_prob_list, hv106_empirical_prob_dt[i,5][[1]])
}

# Collect data for plot
hv106_model_check_dt <- copy(hv106_model_prob_dt)
hv106_model_check_dt$Empirical <- hv106_empirical_prob_list

# Create the 6 data tables
ind1 <- seq(1, nrow(hv106_model_check_dt), by = 6)
hv106_model_check_dt1 <- hv106_model_check_dt[ind1, ]
ind2 <- seq(2, nrow(hv106_model_check_dt), by = 6)
hv106_model_check_dt2 <- hv106_model_check_dt[ind2, ]
ind3 <- seq(3, nrow(hv106_model_check_dt), by = 6)
hv106_model_check_dt3 <- hv106_model_check_dt[ind3, ]
ind4 <- seq(4, nrow(hv106_model_check_dt), by = 6)
hv106_model_check_dt4 <- hv106_model_check_dt[ind4, ]
ind5 <- seq(5, nrow(hv106_model_check_dt), by = 6)
hv106_model_check_dt5 <- hv106_model_check_dt[ind5, ]
ind6 <- seq(6, nrow(hv106_model_check_dt), by = 6)
hv106_model_check_dt6 <- hv106_model_check_dt[ind6, ]

# Plot graph
# Initially plot first set of points
hv106_model_check <- ggplot(hv106_model_check_dt1,
                            aes(x = Empirical, y = Prob, ymin = Lower, ymax = Upper, color = 'red')) +
  geom_errorbar(width = 0.005) +
  geom_point(size = 0.7) +
  geom_abline(intercept = 0, slope = 1, colour = 'grey50') +
  labs(
    y = 'Prob attend school estimated from model',
    x = 'Prob attend school empirical from data',
    title = 'hv106 model check'
  )
# Add the second set of points
hv106_model_check <- hv106_model_check + geom_errorbar(data = hv106_model_check_dt2, aes(x = Empirical, y = Prob, ymin = Lower, ymax = Upper, color = 'green'), width = 0.005) +
  geom_point(data = hv106_model_check_dt2, aes(x = Empirical, y = Prob, color = 'green'), size = 0.7)
# Add the third set of points
hv106_model_check <- hv106_model_check + geom_errorbar(data = hv106_model_check_dt3, aes(x = Empirical, y = Prob, ymin = Lower, ymax = Upper, color = 'blue'), width = 0.005) +
  geom_point(data = hv106_model_check_dt3, aes(x = Empirical, y = Prob, color = 'blue'), size = 0.7)
# Add the fourth set of points
hv106_model_check <- hv106_model_check + geom_errorbar(data = hv106_model_check_dt4, aes(x = Empirical, y = Prob, ymin = Lower, ymax = Upper, color = 'deeppink'), width = 0.005) +
  geom_point(data = hv106_model_check_dt4, aes(x = Empirical, y = Prob, color = 'deeppink'), size = 0.7)
# Add the fifth set of points
hv106_model_check <- hv106_model_check + geom_errorbar(data = hv106_model_check_dt5, aes(x = Empirical, y = Prob, ymin = Lower, ymax = Upper, color = 'darkorange1'), width = 0.005) +
  geom_point(data = hv106_model_check_dt5, aes(x = Empirical, y = Prob, color = 'darkorange1'), size = 0.7)
# Add the fifth set of points
hv106_model_check <- hv106_model_check + geom_errorbar(data = hv106_model_check_dt6, aes(x = Empirical, y = Prob, ymin = Lower, ymax = Upper, color = 'cadetblue4'), width = 0.005) +
  geom_point(data = hv106_model_check_dt6, aes(x = Empirical, y = Prob, color = 'cadetblue4'), size = 0.7)
# Change legend
hv106_model_check <- hv106_model_check + scale_color_manual(
  name = 'Groups', # Set the legend title here
  values = c('red' = 'red', 'green' = 'green', 'blue' = 'blue', 'deeppink' = 'deeppink', 'darkorange1' = 'darkorange1', 'cadetblue4' = 'cadetblue4'),
  labels = c('red' = 'Orphan, No Education', 
             'green' = 'Orphan, Primary Education',
             'blue' = 'Orphan, Secondary Education',
             'deeppink' = 'Non Orphan, No Education',
             'darkorange1' = 'Non Orphan, Primary Education',
             'cadetblue4' = 'Non Orphan, Secondary Education')
)
out.dir4 <- "/Users/hmw850/OneDrive - Queen Mary, University of London/Documents/UROP1/DHS/Plots/hv106 model checks"
ggsave(file = file.path(out.dir4, 'hv106_check_plot.png'), hv106_model_check, w = 9, h = 7)
knitr::include_graphics(file.path(out.dir4, 'hv106_check_plot.png'))
