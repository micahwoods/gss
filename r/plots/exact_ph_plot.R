# for pH, however, do this custom
# pH can be normal because in practice it doesn't have upper and lower bound

m_ph_fit <- fitdist(ph_mlsn, distr = 'norm')
x.values <- seq(4, 9, 5/500)
y.values <- dnorm(x.values, m_ph_fit[["estimate"]][1], m_ph_fit[["estimate"]][2])
m_ph_sim <- cbind.data.frame(x.values, y.values)

g_ph_fit <- fitdist(d$pH, distr = 'norm')
x.values <- seq(4, 9, 5/500)
y.values <- dnorm(x.values, g_ph_fit[["estimate"]][1], g_ph_fit[["estimate"]][2])
g_ph_sim <- cbind.data.frame(x.values, y.values)

# ph plot with sim data from a normal
p <- ggplot(data = m_ph_sim, aes(x = x.values, y = y.values))
ph_plot_norm <- p + theme_cowplot() +
  background_grid(major = 'x') +
  geom_area(fill = "#d95f02", colour = '#d95f02', alpha = 0.2, 
               linetype = 'dashed') +
  geom_area(data = g_ph_sim, aes(x = x.values, y = y.values), 
               colour = "#3f7300", fill = '#3f7300', alpha = 0.3) +
  #  scale_x_continuous(limits = c(0, max.x),
  #                 #   expand = c(0, 0),
  #                 breaks = seq(0, max.x, max.x/5)) +
  scale_x_continuous(limits = c(4, 9),
                     breaks = 4:9) +
  scale_y_continuous(expand = c(0, 0)) +
  theme(axis.line.y = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  labs(x = expression(paste('soil pH in 1:1 ', H[2], 'O'))) +
  annotate("segment", x = 4.5, xend = 5.48, y = 0.37, 
           yend = 0.27, colour = "#3f7300",  size = 0.5, 
           arrow=arrow(type = 'closed',
                       length = unit(0.3, 'cm'))) +
  annotate("label", x = 4, hjust = 0, y = 0.42,
           colour = "#3f7300",
           label = 'GSS\nfit',
           family = 'Ubuntu Condensed',
           label.padding = unit(0.2, "lines"),
           size = 3.5) +
  annotate("segment", x = 8.8, xend = 7.75, y = 0.41, 
           yend = 0.3, colour = "#d95f02",  size = 0.5, linetype = 'dashed',
           arrow=arrow(type = 'closed',
                       length = unit(0.3, 'cm'))) +
  annotate("label", x = 9, hjust = 1, y = 0.43,
           colour = "#d95f02",
           family = 'Ubuntu Condensed',
           label = 'MLSN\nfit',
           label.padding = unit(0.2, "lines"),
           size = 3.5) 

ph_plot_norm

## ph plot with gss model vs original gss data density
p <- ggplot(data = d, aes(x = pH))
ph_plot_survey <- p + theme_cowplot() +
  background_grid(major = 'x') +
  geom_density(fill = '#7570b3', colour = '#7570b3', alpha = 0.2, 
               linetype = 'dashed') +
  geom_area(data = g_ph_sim, aes(x = x.values, y = y.values), 
               colour = "#3f7300", fill = '#3f7300', alpha = 0.3) +
  #  scale_x_continuous(limits = c(0, max.x),
  #                 #   expand = c(0, 0),
  #                 breaks = seq(0, max.x, max.x/5)) +
  scale_x_continuous(limits = c(4, 9),
                     breaks = 4:9) +
  scale_y_continuous(expand = c(0, 0)) +
  theme(axis.line.y = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  labs(x = expression(paste('soil pH in 1:1 ', H[2], 'O'))) +
  annotate("label", x = 4, hjust = 0, y = 0.41,
           colour = '#7570b3',
           label = 'GSS\nsurvey\ndata',
           family = 'Ubuntu Condensed',
           label.padding = unit(0.2, "lines"),
           size = 3.5) +
  annotate("segment", x = 4.4, xend = 5.32, y = 0.31, 
           yend = 0.2, colour = '#7570b3',  size = 0.5, linetype = 'dashed',
           arrow=arrow(type = 'closed',
                       length = unit(0.3, 'cm'))) +
  annotate("label", x = 9, hjust = 1, y = 0.44,
           colour = "#3f7300",
           family = 'Ubuntu Condensed',
           label = 'fitted\ndistribution',
           label.padding = unit(0.2, "lines"),
           size = 3.5) +
  annotate("segment", x = 8.6, xend = 7.33, y = 0.38, 
           yend = 0.3, colour = "#3f7300",  size = 0.5, 
           arrow=arrow(type = 'closed',
                      length = unit(0.3, 'cm')))

ph_plot_survey
