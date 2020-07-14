# map, North America, Europe, Asia, tryptych type for 42 locs

library("ggmap")
library("tmap")
library("patchwork")
library("sf")
library("ggplot2")
library("rnaturalearth")
library("rnaturalearthdata")
library("cowplot")
library("rgdal")
library("rgeos")
library("lubridate")

# register the key
register_google(key = "PUT YOUR GOOGLE API KEY HERE")

# we've omitted the site names from the public GSS data
# we made the chart by taking the sites from the GSS samples and geocoding them
# as below, then plotting those points on the map
# read in a datafile that does contain locs

# the d$loc column is some company name, or address, or city/state/country combination 
locVector <- unique(d$loc)

#  finds lat and lon for a location 
latlong <- geocode(locVector)

latlong$course <- locVector

world <- ne_countries(scale = "medium", returnclass = "sf")

# map of North American sites
na <- ggplot(data = world) +
  geom_sf(colour = "lightslategrey",
          size = 0.3) + 
  coord_sf(xlim = c(-130, -55), ylim = c(20, 60), expand = FALSE) +
    geom_point(data = latlong, 
                aes(x = lon, y = lat), 
                colour = "#3f7300",
                size = 2,
                alpha = 0.8) +
  theme(panel.background = element_rect(fill = 'aliceblue'),
     #   plot.title = element_text(margin = margin(b = -17),
      #                            hjust = .5),
        axis.title.y = element_blank()) +
          labs(# title = 'North America',
               x = 'Longitude',
               y = 'Latitude') +
  annotate("label", x = -95, y = 58, 
           fill = "snow", size = 4,
           label = "North America")

na

asia <- ggplot(data = world) +
    geom_sf(colour = 'lightslategrey',
            size = 0.2) + 
    coord_sf(xlim = c(90, 150), ylim = c(0, 50), expand = TRUE) +
    geom_point(data = latlong, 
                aes(x = lon, y = lat), 
                colour = "#3f7300",
                size = 2,
                alpha = 0.8) +
  theme(panel.background = element_rect(fill = 'aliceblue'),
 #       plot.title = element_text(margin = margin(b = -17),
  #                                hjust = .02),
        axis.title.x = element_blank()
        ) +
  labs(# title = 'Asia',
       x = 'Longitude',
       y = 'Latitude') +
  annotate("label", x = 88, y = 50, hjust = 0, 
           fill = "snow", size = 4,
           label = "Asia")

asia

eur <- ggplot(data = world) +
  geom_sf(colour = 'lightslategrey',
          size = 0.2) + 
  coord_sf(xlim = c(-30, 30), ylim = c(45, 70), expand = TRUE) +
  geom_jitter(data = latlong, 
              aes(x = lon, y = lat), 
              colour = "#3f7300",
              size = 2,
              alpha = 0.8) +
  theme(panel.background = element_rect(fill = 'aliceblue'),
      #  plot.title = element_text(margin = margin(b = -17),
       #                           hjust = .97),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  labs(# title = 'Europe',
       x = 'Longitude',
       y = 'Latitude') +
  annotate("label", x = 31, y = 70, hjust = 1, 
           fill = "snow", size = 4,
           label = "Europe")

eur

yo <- asia + na + eur

save_plot('~/Desktop/gss_locs_exact.pdf', yo, base_width = 11)
