# figure 2
# show overlapped density plots of mlsn vs gss
# I think go ahead and omit y axis

# perhaps do actual data density first, then modeled

# that is, figure 2 = gss density vs model
# then figure 3 = gss model vs mlsn model

source('r/libraries.R')
source('r/functions.R')

# read the data, also make vectors of MLSN data, fix below detection
source('r/read_data_private_stage1.R')

# make charts
source('r/plots/exact_ph_plot.R')
source('r/plots/exact_om_plot.R')
source('r/plots/exact_k_plot.R')
source('r/plots/exact_p_plot.R')
source('r/plots/exact_p_bray_plot.R')
source('r/plots/exact_p_olsen_plot.R')
source('r/plots/exact_ca_plot.R')
source('r/plots/exact_mg_plot.R')
source('r/plots/exact_s_plot.R')
source('r/plots/exact_fe_plot.R')
source('r/plots/exact_mn_plot.R')
source('r/plots/exact_cu_plot.R')
source('r/plots/exact_zn_plot.R')
source('r/plots/exact_b_plot.R')
source('r/plots/exact_cl_plot.R')
source('r/plots/exact_no3_plot.R')
source('r/plots/exact_nh4_plot.R')
source('r/plots/exact_ec_plot.R')

fig2_survey <- ph_plot_survey + om_plot_survey + k_plot_survey + p_plot_survey + bray_plot_survey + olsen_plot_survey +
  ca_plot_survey + mg_plot_survey + s_plot_survey + fe_plot_survey + mn_plot_survey + cu_plot_survey + 
  zn_plot_survey + b_plot_survey + cl_plot_survey + no3_plot_survey + nh4_plot_survey + ec_plot_survey +
  plot_layout(nrow = 3)

save_plot('~/Desktop/fig2_survey_exact.png', fig2_survey, base_width = 15, base_height = 7.5)

fig3_compare <- ph_plot_norm + om_plot_sim + k_plot_sim + p_plot_sim + bray_plot_sim + olsen_plot_sim +
  ca_plot_sim + mg_plot_sim + s_plot_sim + fe_plot_sim + mn_plot_sim + cu_plot_sim +
  zn_plot_sim + b_plot_sim + cl_plot_sim + no3_plot_sim + nh4_plot_sim + ec_plot_sim +
  plot_layout(nrow = 3)


save_plot('~/Desktop/fig3_compare_exact.png', fig3_compare, base_width = 15, base_height = 7.5)

