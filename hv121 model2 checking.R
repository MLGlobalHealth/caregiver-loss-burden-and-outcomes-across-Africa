# Model checks
# Find empirical estimates from data table
hv121_data_update_long_prob <- copy(hv121_data_update_long)
hv121_data_update_long_prob$Prob <- hv121_data_update_long$School / hv121_data_update_long$Total
# Find model estimates
hv121_model_updated_prob_list <- c()
hv121_model_updated_upper_list <- c()
hv121_model_updated_lower_list <- c()
for(i in 1:35){
  #301
  hv121_model_updated_prob_list <- append(hv121_model_updated_prob_list,
                                          quantile(inv.logit(po_updated$baseline_hyper_mean + po_updated$units_baseline[,i] +
                                                               po_updated$units_baseline_agesex[,1] + po_updated$orp3_hyper_mean +
                                                               po_updated$units_orp3[,i] + po_updated$units_orp3_agesex[,1]), probs = c(0.5, 0.025, 0.975))[1])
  hv121_model_updated_upper_list <- append(hv121_model_updated_upper_list,
                                           quantile(inv.logit(po_updated$baseline_hyper_mean + po_updated$units_baseline[,i] +
                                                                po_updated$units_baseline_agesex[,1] + po_updated$orp3_hyper_mean +
                                                                po_updated$units_orp3[,i] + po_updated$units_orp3_agesex[,1]), probs = c(0.5, 0.025, 0.975))[3])
  hv121_model_updated_lower_list <- append(hv121_model_updated_lower_list,
                                           quantile(inv.logit(po_updated$baseline_hyper_mean + po_updated$units_baseline[,i] +
                                                                po_updated$units_baseline_agesex[,1] + po_updated$orp3_hyper_mean +
                                                                po_updated$units_orp3[,i] + po_updated$units_orp3_agesex[,1]), probs = c(0.5, 0.025, 0.975))[2])
  #302
  hv121_model_updated_prob_list <- append(hv121_model_updated_prob_list,
                                          quantile(inv.logit(po_updated$baseline_hyper_mean + po_updated$units_baseline[,i] +
                                                               po_updated$units_baseline_agesex[,2] + po_updated$orp3_hyper_mean +
                                                               po_updated$units_orp3[,i] + po_updated$units_orp3_agesex[,2]), probs = c(0.5, 0.025, 0.975))[1])
  hv121_model_updated_upper_list <- append(hv121_model_updated_upper_list,
                                           quantile(inv.logit(po_updated$baseline_hyper_mean + po_updated$units_baseline[,i] +
                                                                po_updated$units_baseline_agesex[,2] + po_updated$orp3_hyper_mean +
                                                                po_updated$units_orp3[,i] + po_updated$units_orp3_agesex[,2]), probs = c(0.5, 0.025, 0.975))[3])
  hv121_model_updated_lower_list <- append(hv121_model_updated_lower_list,
                                           quantile(inv.logit(po_updated$baseline_hyper_mean + po_updated$units_baseline[,i] +
                                                                po_updated$units_baseline_agesex[,2] + po_updated$orp3_hyper_mean +
                                                                po_updated$units_orp3[,i] + po_updated$units_orp3_agesex[,2]), probs = c(0.5, 0.025, 0.975))[2])
  #101
  hv121_model_updated_prob_list <- append(hv121_model_updated_prob_list,
                                          quantile(inv.logit(po_updated$baseline_hyper_mean + po_updated$units_baseline[,i] +
                                                               po_updated$units_baseline_agesex[,1] + po_updated$orp1_hyper_mean +
                                                               po_updated$units_orp1[,i] + po_updated$units_orp1_agesex[,1]), probs = c(0.5, 0.025, 0.975))[1])
  hv121_model_updated_upper_list <- append(hv121_model_updated_upper_list,
                                           quantile(inv.logit(po_updated$baseline_hyper_mean + po_updated$units_baseline[,i] +
                                                                po_updated$units_baseline_agesex[,1] + po_updated$orp1_hyper_mean +
                                                                po_updated$units_orp1[,i] + po_updated$units_orp1_agesex[,1]), probs = c(0.5, 0.025, 0.975))[3])
  hv121_model_updated_lower_list <- append(hv121_model_updated_lower_list,
                                           quantile(inv.logit(po_updated$baseline_hyper_mean + po_updated$units_baseline[,i] +
                                                                po_updated$units_baseline_agesex[,1] + po_updated$orp1_hyper_mean +
                                                                po_updated$units_orp1[,i] + po_updated$units_orp1_agesex[,1]), probs = c(0.5, 0.025, 0.975))[2])
  #102
  hv121_model_updated_prob_list <- append(hv121_model_updated_prob_list,
                                          quantile(inv.logit(po_updated$baseline_hyper_mean + po_updated$units_baseline[,i] +
                                                               po_updated$units_baseline_agesex[,2] + po_updated$orp1_hyper_mean +
                                                               po_updated$units_orp1[,i] + po_updated$units_orp1_agesex[,2]), probs = c(0.5, 0.025, 0.975))[1])
  hv121_model_updated_upper_list <- append(hv121_model_updated_upper_list,
                                           quantile(inv.logit(po_updated$baseline_hyper_mean + po_updated$units_baseline[,i] +
                                                                po_updated$units_baseline_agesex[,2] + po_updated$orp1_hyper_mean +
                                                                po_updated$units_orp1[,i] + po_updated$units_orp1_agesex[,2]), probs = c(0.5, 0.025, 0.975))[3])
  hv121_model_updated_lower_list <- append(hv121_model_updated_lower_list,
                                           quantile(inv.logit(po_updated$baseline_hyper_mean + po_updated$units_baseline[,i] +
                                                                po_updated$units_baseline_agesex[,2] + po_updated$orp1_hyper_mean +
                                                                po_updated$units_orp1[,i] + po_updated$units_orp1_agesex[,2]), probs = c(0.5, 0.025, 0.975))[2])
  #201
  hv121_model_updated_prob_list <- append(hv121_model_updated_prob_list,
                                          quantile(inv.logit(po_updated$baseline_hyper_mean + po_updated$units_baseline[,i] +
                                                               po_updated$units_baseline_agesex[,1] + po_updated$orp2_hyper_mean +
                                                               po_updated$units_orp2[,i] + po_updated$units_orp2_agesex[,1]), probs = c(0.5, 0.025, 0.975))[1])
  hv121_model_updated_upper_list <- append(hv121_model_updated_upper_list,
                                           quantile(inv.logit(po_updated$baseline_hyper_mean + po_updated$units_baseline[,i] +
                                                                po_updated$units_baseline_agesex[,1] + po_updated$orp2_hyper_mean +
                                                                po_updated$units_orp2[,i] + po_updated$units_orp2_agesex[,1]), probs = c(0.5, 0.025, 0.975))[3])
  hv121_model_updated_lower_list <- append(hv121_model_updated_lower_list,
                                           quantile(inv.logit(po_updated$baseline_hyper_mean + po_updated$units_baseline[,i] +
                                                                po_updated$units_baseline_agesex[,1] + po_updated$orp2_hyper_mean +
                                                                po_updated$units_orp2[,i] + po_updated$units_orp2_agesex[,1]), probs = c(0.5, 0.025, 0.975))[2])
  #202
  hv121_model_updated_prob_list <- append(hv121_model_updated_prob_list,
                                          quantile(inv.logit(po_updated$baseline_hyper_mean + po_updated$units_baseline[,i] +
                                                               po_updated$units_baseline_agesex[,2] + po_updated$orp2_hyper_mean +
                                                               po_updated$units_orp2[,i] + po_updated$units_orp2_agesex[,2]), probs = c(0.5, 0.025, 0.975))[1])
  hv121_model_updated_upper_list <- append(hv121_model_updated_upper_list,
                                           quantile(inv.logit(po_updated$baseline_hyper_mean + po_updated$units_baseline[,i] +
                                                                po_updated$units_baseline_agesex[,2] + po_updated$orp2_hyper_mean +
                                                                po_updated$units_orp2[,i] + po_updated$units_orp2_agesex[,2]), probs = c(0.5, 0.025, 0.975))[3])
  hv121_model_updated_lower_list <- append(hv121_model_updated_lower_list,
                                           quantile(inv.logit(po_updated$baseline_hyper_mean + po_updated$units_baseline[,i] +
                                                                po_updated$units_baseline_agesex[,2] + po_updated$orp2_hyper_mean +
                                                                po_updated$units_orp2[,i] + po_updated$units_orp2_agesex[,2]), probs = c(0.5, 0.025, 0.975))[2])
  #001
  hv121_model_updated_prob_list <- append(hv121_model_updated_prob_list,
                                          quantile(inv.logit(po_updated$baseline_hyper_mean + po_updated$units_baseline[,i] +
                                                               po_updated$units_baseline_agesex[,1]), probs = c(0.5, 0.025, 0.975))[1])
  hv121_model_updated_upper_list <- append(hv121_model_updated_upper_list,
                                           quantile(inv.logit(po_updated$baseline_hyper_mean + po_updated$units_baseline[,i] +
                                                                po_updated$units_baseline_agesex[,1]), probs = c(0.5, 0.025, 0.975))[3])
  hv121_model_updated_lower_list <- append(hv121_model_updated_lower_list,
                                           quantile(inv.logit(po_updated$baseline_hyper_mean + po_updated$units_baseline[,i] +
                                                                po_updated$units_baseline_agesex[,1]), probs = c(0.5, 0.025, 0.975))[2])
  #002
  hv121_model_updated_prob_list <- append(hv121_model_updated_prob_list,
                                          quantile(inv.logit(po_updated$baseline_hyper_mean + po_updated$units_baseline[,i] +
                                                               po_updated$units_baseline_agesex[,2]), probs = c(0.5, 0.025, 0.975))[1])
  hv121_model_updated_upper_list <- append(hv121_model_updated_upper_list,
                                           quantile(inv.logit(po_updated$baseline_hyper_mean + po_updated$units_baseline[,i] +
                                                                po_updated$units_baseline_agesex[,2]), probs = c(0.5, 0.025, 0.975))[3])
  hv121_model_updated_lower_list <- append(hv121_model_updated_lower_list,
                                           quantile(inv.logit(po_updated$baseline_hyper_mean + po_updated$units_baseline[,i] +
                                                                po_updated$units_baseline_agesex[,2]), probs = c(0.5, 0.025, 0.975))[2])
  #311
  hv121_model_updated_prob_list <- append(hv121_model_updated_prob_list,
                                          quantile(inv.logit(po_updated$baseline_hyper_mean + po_updated$units_baseline[,i] +
                                                               po_updated$units_baseline_agesex[,3] + po_updated$orp3_hyper_mean +
                                                               po_updated$units_orp3[,i] + po_updated$units_orp3_agesex[,3]), probs = c(0.5, 0.025, 0.975))[1])
  hv121_model_updated_upper_list <- append(hv121_model_updated_upper_list,
                                           quantile(inv.logit(po_updated$baseline_hyper_mean + po_updated$units_baseline[,i] +
                                                                po_updated$units_baseline_agesex[,3] + po_updated$orp3_hyper_mean +
                                                                po_updated$units_orp3[,i] + po_updated$units_orp3_agesex[,3]), probs = c(0.5, 0.025, 0.975))[3])
  hv121_model_updated_lower_list <- append(hv121_model_updated_lower_list,
                                           quantile(inv.logit(po_updated$baseline_hyper_mean + po_updated$units_baseline[,i] +
                                                                po_updated$units_baseline_agesex[,3] + po_updated$orp3_hyper_mean +
                                                                po_updated$units_orp3[,i] + po_updated$units_orp3_agesex[,3]), probs = c(0.5, 0.025, 0.975))[2])
  #312
  hv121_model_updated_prob_list <- append(hv121_model_updated_prob_list,
                                          quantile(inv.logit(po_updated$baseline_hyper_mean + po_updated$units_baseline[,i] +
                                                               po_updated$units_baseline_agesex[,4] + po_updated$orp3_hyper_mean +
                                                               po_updated$units_orp3[,i] + po_updated$units_orp3_agesex[,4]), probs = c(0.5, 0.025, 0.975))[1])
  hv121_model_updated_upper_list <- append(hv121_model_updated_upper_list,
                                           quantile(inv.logit(po_updated$baseline_hyper_mean + po_updated$units_baseline[,i] +
                                                                po_updated$units_baseline_agesex[,4] + po_updated$orp3_hyper_mean +
                                                                po_updated$units_orp3[,i] + po_updated$units_orp3_agesex[,4]), probs = c(0.5, 0.025, 0.975))[3])
  hv121_model_updated_lower_list <- append(hv121_model_updated_lower_list,
                                           quantile(inv.logit(po_updated$baseline_hyper_mean + po_updated$units_baseline[,i] +
                                                                po_updated$units_baseline_agesex[,4] + po_updated$orp3_hyper_mean +
                                                                po_updated$units_orp3[,i] + po_updated$units_orp3_agesex[,4]), probs = c(0.5, 0.025, 0.975))[2])
  #111
  hv121_model_updated_prob_list <- append(hv121_model_updated_prob_list,
                                          quantile(inv.logit(po_updated$baseline_hyper_mean + po_updated$units_baseline[,i] +
                                                               po_updated$units_baseline_agesex[,3] + po_updated$orp1_hyper_mean +
                                                               po_updated$units_orp1[,i] + po_updated$units_orp1_agesex[,3]), probs = c(0.5, 0.025, 0.975))[1])
  hv121_model_updated_upper_list <- append(hv121_model_updated_upper_list,
                                           quantile(inv.logit(po_updated$baseline_hyper_mean + po_updated$units_baseline[,i] +
                                                                po_updated$units_baseline_agesex[,3] + po_updated$orp1_hyper_mean +
                                                                po_updated$units_orp1[,i] + po_updated$units_orp1_agesex[,3]), probs = c(0.5, 0.025, 0.975))[3])
  hv121_model_updated_lower_list <- append(hv121_model_updated_lower_list,
                                           quantile(inv.logit(po_updated$baseline_hyper_mean + po_updated$units_baseline[,i] +
                                                                po_updated$units_baseline_agesex[,3] + po_updated$orp1_hyper_mean +
                                                                po_updated$units_orp1[,i] + po_updated$units_orp1_agesex[,3]), probs = c(0.5, 0.025, 0.975))[2])
  #112
  hv121_model_updated_prob_list <- append(hv121_model_updated_prob_list,
                                          quantile(inv.logit(po_updated$baseline_hyper_mean + po_updated$units_baseline[,i] +
                                                               po_updated$units_baseline_agesex[,4] + po_updated$orp1_hyper_mean +
                                                               po_updated$units_orp1[,i] + po_updated$units_orp1_agesex[,4]), probs = c(0.5, 0.025, 0.975))[1])
  hv121_model_updated_upper_list <- append(hv121_model_updated_upper_list,
                                           quantile(inv.logit(po_updated$baseline_hyper_mean + po_updated$units_baseline[,i] +
                                                                po_updated$units_baseline_agesex[,4] + po_updated$orp1_hyper_mean +
                                                                po_updated$units_orp1[,i] + po_updated$units_orp1_agesex[,4]), probs = c(0.5, 0.025, 0.975))[3])
  hv121_model_updated_lower_list <- append(hv121_model_updated_lower_list,
                                           quantile(inv.logit(po_updated$baseline_hyper_mean + po_updated$units_baseline[,i] +
                                                                po_updated$units_baseline_agesex[,4] + po_updated$orp1_hyper_mean +
                                                                po_updated$units_orp1[,i] + po_updated$units_orp1_agesex[,4]), probs = c(0.5, 0.025, 0.975))[2])
  #211
  hv121_model_updated_prob_list <- append(hv121_model_updated_prob_list,
                                          quantile(inv.logit(po_updated$baseline_hyper_mean + po_updated$units_baseline[,i] +
                                                               po_updated$units_baseline_agesex[,3] + po_updated$orp2_hyper_mean +
                                                               po_updated$units_orp2[,i] + po_updated$units_orp2_agesex[,3]), probs = c(0.5, 0.025, 0.975))[1])
  hv121_model_updated_upper_list <- append(hv121_model_updated_upper_list,
                                           quantile(inv.logit(po_updated$baseline_hyper_mean + po_updated$units_baseline[,i] +
                                                                po_updated$units_baseline_agesex[,3] + po_updated$orp2_hyper_mean +
                                                                po_updated$units_orp2[,i] + po_updated$units_orp2_agesex[,3]), probs = c(0.5, 0.025, 0.975))[3])
  hv121_model_updated_lower_list <- append(hv121_model_updated_lower_list,
                                           quantile(inv.logit(po_updated$baseline_hyper_mean + po_updated$units_baseline[,i] +
                                                                po_updated$units_baseline_agesex[,3] + po_updated$orp2_hyper_mean +
                                                                po_updated$units_orp2[,i] + po_updated$units_orp2_agesex[,3]), probs = c(0.5, 0.025, 0.975))[2])
  #212
  hv121_model_updated_prob_list <- append(hv121_model_updated_prob_list,
                                          quantile(inv.logit(po_updated$baseline_hyper_mean + po_updated$units_baseline[,i] +
                                                               po_updated$units_baseline_agesex[,4] + po_updated$orp2_hyper_mean +
                                                               po_updated$units_orp2[,i] + po_updated$units_orp2_agesex[,4]), probs = c(0.5, 0.025, 0.975))[1])
  hv121_model_updated_upper_list <- append(hv121_model_updated_upper_list,
                                           quantile(inv.logit(po_updated$baseline_hyper_mean + po_updated$units_baseline[,i] +
                                                                po_updated$units_baseline_agesex[,4] + po_updated$orp2_hyper_mean +
                                                                po_updated$units_orp2[,i] + po_updated$units_orp2_agesex[,4]), probs = c(0.5, 0.025, 0.975))[3])
  hv121_model_updated_lower_list <- append(hv121_model_updated_lower_list,
                                           quantile(inv.logit(po_updated$baseline_hyper_mean + po_updated$units_baseline[,i] +
                                                                po_updated$units_baseline_agesex[,4] + po_updated$orp2_hyper_mean +
                                                                po_updated$units_orp2[,i] + po_updated$units_orp2_agesex[,4]), probs = c(0.5, 0.025, 0.975))[2])
  #011
  hv121_model_updated_prob_list <- append(hv121_model_updated_prob_list,
                                          quantile(inv.logit(po_updated$baseline_hyper_mean + po_updated$units_baseline[,i] +
                                                               po_updated$units_baseline_agesex[,3]), probs = c(0.5, 0.025, 0.975))[1])
  hv121_model_updated_upper_list <- append(hv121_model_updated_upper_list,
                                           quantile(inv.logit(po_updated$baseline_hyper_mean + po_updated$units_baseline[,i] +
                                                                po_updated$units_baseline_agesex[,3]), probs = c(0.5, 0.025, 0.975))[3])
  hv121_model_updated_lower_list <- append(hv121_model_updated_lower_list,
                                           quantile(inv.logit(po_updated$baseline_hyper_mean + po_updated$units_baseline[,i] +
                                                                po_updated$units_baseline_agesex[,3]), probs = c(0.5, 0.025, 0.975))[2])
  #012
  hv121_model_updated_prob_list <- append(hv121_model_updated_prob_list,
                                          quantile(inv.logit(po_updated$baseline_hyper_mean + po_updated$units_baseline[,i] +
                                                               po_updated$units_baseline_agesex[,4]), probs = c(0.5, 0.025, 0.975))[1])
  hv121_model_updated_upper_list <- append(hv121_model_updated_upper_list,
                                           quantile(inv.logit(po_updated$baseline_hyper_mean + po_updated$units_baseline[,i] +
                                                                po_updated$units_baseline_agesex[,4]), probs = c(0.5, 0.025, 0.975))[3])
  hv121_model_updated_lower_list <- append(hv121_model_updated_lower_list,
                                           quantile(inv.logit(po_updated$baseline_hyper_mean + po_updated$units_baseline[,i] +
                                                                po_updated$units_baseline_agesex[,4]), probs = c(0.5, 0.025, 0.975))[2])
}

# Create model checking plot to check updated hv121 model
hv121_update_check_dt <- data.table(Country = country_names_sort16,
                                    EmpiricalEffectEstimate = hv121_data_update_long_prob$Prob,
                                    ModelEffectEstimate = hv121_model_updated_prob_list,
                                    ModelUpperEstimate = hv121_model_updated_upper_list,
                                    ModelLowerEstimate = hv121_model_updated_lower_list,
                                    Label = rep(c("301", "302", "101", "102", "201", "202", "001", "002",
                                                  "311", "312", "111", "112", "211", "212", "011", "012"), 35))
out.dir3 <- "/Users/hmw850/OneDrive - Queen Mary, University of London/Documents/UROP1/DHS/Plots/hv121 model2 checks"
# Plot graph
hv121_update_check_plot <- ggplot(hv121_update_check_dt,
                                  aes(x = EmpiricalEffectEstimate, y = ModelEffectEstimate, ymin = ModelLowerEstimate, ymax = ModelUpperEstimate, color = Label)) +
  geom_errorbar(width = 0.005) +
  geom_point(size = 0.7) +
  geom_abline(intercept = 0, slope = 1, colour = 'grey50') +
  labs(
    y = 'Prob attend school estimated from model',
    x = 'Prob attend school empirical from data',
    title = 'hv121 updated model check'
  )
ggsave(file = file.path(out.dir3, 'hv121_update_check_plot.png'), hv121_update_check_plot, w = 9, h = 7)
knitr::include_graphics(file.path(out.dir3, 'hv121_update_check_plot.png'))

## Split by group to make plot clearer

# test for 301 (Double, 5-9, Male)
indices301 <- seq(1, nrow(hv121_update_check_dt), by = 16)
hv121_update_check_301_dt <- hv121_update_check_dt[indices301, ]
# Plot graph
hv121_update_check_plot_301 <- ggplot(hv121_update_check_301_dt,
                                      aes(x = EmpiricalEffectEstimate, y = ModelEffectEstimate, ymin = ModelLowerEstimate, ymax = ModelUpperEstimate, label = Country)) +
  geom_errorbar(width = 0.005) +
  geom_point(size = 0.7) +
  geom_text(hjust=0, vjust=1, size = 2) +
  geom_abline(intercept = 0, slope = 1, colour = 'grey50') +
  labs(
    y = 'Prob attend school estimated from model',
    x = 'Prob attend school empirical from data',
    title = 'hv121 updated model check: (301) Double Orphan, Age 5-9, Male'
  )
ggsave(file = file.path(out.dir3, 'hv121_update_check_plot_301.png'), hv121_update_check_plot_301, w = 9, h = 7)
knitr::include_graphics(file.path(out.dir3, 'hv121_update_check_plot_301.png'))

# Plot all the graphs, need a mapping between e.g. 301 and Double Orphan, Age 5-9, Male
subgroup_name_list <- c("Double Orphan, Age 5-9, Male", "Double Orphan, Age 5-9, Female",
                        "Paternal Orphan, Age 5-9, Male", "Paternal Orphan, Age 5-9, Female",
                        "Maternal Orphan, Age 5-9, Male", "Maternal Orphan, Age 5-9, Female",
                        "Non Orphan, Age 5-9, Male", "Non Orphan, Age 5-9, Female",
                        "Double Orphan, Age 10-18, Male", "Double Orphan, Age 10-18, Female",
                        "Paternal Orphan, Age 10-18, Male", "Paternal Orphan, Age 10-18, Female",
                        "Maternal Orphan, Age 10-18, Male", "Maternal Orphan, Age 10-18, Female",
                        "Non Orphan, Age 10-18, Male", "Non Orphan, Age 10-18, Female")
for(i in 1:16){
  hv121_update_check_dt2 <- copy(hv121_update_check_dt)
  hv121_update_check_dt2$Name <- rep(subgroup_name_list, 35)
  indices <- seq(i, nrow(hv121_update_check_dt2), by = 16)
  hv121_update_check_subgroup_dt <- hv121_update_check_dt2[indices, ]
  # plot graph
  hv121_update_check_plot_subgroup <- ggplot(hv121_update_check_subgroup_dt,
                                             aes(x = EmpiricalEffectEstimate, y = ModelEffectEstimate, ymin = ModelLowerEstimate, ymax = ModelUpperEstimate, label = Country)) +
    geom_errorbar(width = 0.005) +
    geom_point(size = 0.7) +
    geom_text(hjust=0, vjust=1, size = 2) +
    geom_abline(intercept = 0, slope = 1, colour = 'grey50') +
    labs(
      y = 'Prob attend school estimated from model',
      x = 'Prob attend school empirical from data',
      title = paste(paste0(paste0('hv121 updated model check: (', hv121_update_check_subgroup_dt$Label[1]), ')'), hv121_update_check_subgroup_dt$Name[1])
    )
  # save graph
  ggsave(file = file.path(out.dir3, paste0(paste0('hv121_update_check_plot_', hv121_update_check_subgroup_dt$Label[1]), '.png')), hv121_update_check_plot_subgroup, w = 9, h = 7)
  knitr::include_graphics(file.path(out.dir3, paste0(paste0('hv121_update_check_plot_', hv121_update_check_subgroup_dt$Label[1]), '.png')))
}