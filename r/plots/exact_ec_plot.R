### EC

msim <- m_dist(ec_mlsn * 1e3)
gsim <- g_dist(d$EC12 * 1e3)

m_values <- msim[[1]]
max_mlsn <- msim[[2]]

g_values <- gsim[[1]]
max_gss <- gsim[[2]]

# choose max and a pretty max that divides for scale
real.max <- max(c(max_mlsn, max_gss))
pretty.max <- max(pretty(c(0, max(c(max_mlsn, max_gss))))) / 1e3

# plot with sim data
p <- ggplot(data = m_values, aes(x = x.values / 1e3, y = y.values * 1e3))
ec_plot_sim <- p + theme_cowplot() +
  background_grid(major = 'x') +
  geom_area(fill = "#d95f02", colour = '#d95f02', alpha = 0.2, 
               linetype = 'dashed') +
  geom_area(data = g_values, aes(x = x.values / 1e3, y = y.values * 1e3), 
               colour = "#3f7300", fill = '#3f7300', alpha = 0.3) +
  scale_x_continuous(limits = c(0, pretty.max),
                     breaks = seq(0, pretty.max, pretty.max/5)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme(axis.line.y = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  labs(x = expression(paste('1:2 EC, dS ', m^{-1}))) +
  annotate("label", x = 0.55, hjust = 1, y = 8.2,
           colour = "#3f7300",
           label = 'GSS\nfit',
           family = 'Ubuntu Condensed',
           label.padding = unit(0.2, "lines"),
           size = 3.5) +
  annotate("segment", x = 0.46, xend = 0.06, y = 8, 
           yend = 7.5, colour = "#3f7300",  size = 0.5, 
           arrow=arrow(type = 'closed',
                       length = unit(0.3, 'cm'))) +
  annotate("label", x = .56, hjust = 1, y = 4.5,
           colour = "#d95f02",
           family = 'Ubuntu Condensed',
           label = 'MLSN\nfit',
           label.padding = unit(0.2, "lines"),
           size = 3.5) +
  annotate("segment", x = 0.43, xend = 0.21, y = 3.9, 
           yend = 2.3, colour = "#d95f02",  size = 0.5, linetype = 'dashed',
           arrow=arrow(type = 'closed',
                       length = unit(0.3, 'cm')))
 
ec_plot_sim

# plot with sim vs gss actual
p <- ggplot(data = d, aes(x = EC12))
ec_plot_survey <- p + theme_cowplot() +
  background_grid(major = 'x') +
  geom_density(fill = '#7570b3', colour = '#7570b3', alpha = 0.2, 
               linetype = 'dashed') +
  geom_area(data = g_values, aes(x = x.values / 1e3, y = y.values * 1e3), 
               colour = "#3f7300", fill = '#3f7300', alpha = 0.3) +
  scale_x_continuous(limits = c(0, pretty.max),
                     breaks = seq(0, pretty.max, pretty.max/5)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme(axis.line.y = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  labs(x = expression(paste('1:2 EC, dS ', m^{-1}))) +
  annotate("label", x = 0.58, hjust = 1, y = 8.8,
           colour = '#7570b3',
           label = 'GSS\nsurvey\ndata',
           family = 'Ubuntu Condensed',
           label.padding = unit(0.2, "lines"),
           size = 3.5) +
  annotate("segment", x = 0.44, xend = 0.0825, y = 8, 
           yend = 7.5, colour = '#7570b3',  size = 0.5, linetype = 'dashed',
           arrow=arrow(type = 'closed',
                       length = unit(0.3, 'cm'))) +
  annotate("label", x = .65, hjust = 1, y = 4.5,
           colour = "#3f7300",
           family = 'Ubuntu Condensed',
           label = 'fitted\ndistribution',
           label.padding = unit(0.2, "lines"),
           size = 3.5) +
  annotate("segment", x = 0.41, xend = 0.26, y = 3.3, 
           yend = 0.7, colour = "#3f7300",  size = 0.5, 
           arrow=arrow(type = 'closed',
                       length = unit(0.3, 'cm')))
   

ec_plot_survey

