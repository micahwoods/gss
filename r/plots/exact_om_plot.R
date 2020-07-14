### ORGANIC MATTER

msim <- m_dist(om_mlsn * 10)
gsim <- g_dist(d$OM360 * 10)

m_values <- msim[[1]]
max_mlsn <- msim[[2]]

g_values <- gsim[[1]]
max_gss <- gsim[[2]]

# choose max and a pretty max that divides for scale
real.max <- max(c(max_mlsn, max_gss))
pretty.max <- max(pretty(c(0, max(c(max_mlsn, max_gss)))))

# om plot with sim data
p <- ggplot(data = m_values, aes(x = x.values, y = y.values))
om_plot_sim <- p + theme_cowplot() +
  background_grid(major = 'x') +
  geom_area(fill = "#d95f02", colour = '#d95f02', alpha = 0.2, 
               linetype = 'dashed') +
  geom_area(data = g_values, aes(x = x.values, y = y.values), 
               colour = "#3f7300", fill = '#3f7300', alpha = 0.3) +
  scale_x_continuous(limits = c(0, 80),
                     #                 #   expand = c(0, 0),
                     breaks = seq(0, 80, 20)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme(axis.line.y = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  # labs(x = expression(paste('mg Zn ', kg^{-1}))) 
  labs(x = expression(paste('soil organic matter, g ', kg^{-1})))

om_plot_sim

# om plot with sim vs gss actual
p <- ggplot(data = d, aes(x = OM360 * 10))
om_plot_survey <- p + theme_cowplot() +
  background_grid(major = 'x') +
  geom_density(fill = '#7570b3', colour = '#7570b3', alpha = 0.2, 
               linetype = 'dashed') +
  geom_area(data = g_values, aes(x = x.values, y = y.values), 
               colour = "#3f7300", fill = '#3f7300', alpha = 0.3) +
  scale_x_continuous(limits = c(0, 80),
                     #                 #   expand = c(0, 0),
                     breaks = seq(0, 80, 20)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme(axis.line.y = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  # labs(x = expression(paste('mg Zn ', kg^{-1}))) 
  labs(x = expression(paste('soil organic matter, g ', kg^{-1})))

om_plot_survey
