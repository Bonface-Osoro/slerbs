library(ggplot2)
library(dplyr)
library(tidyr)
library(sf)
library(ggspatial)

# Get relative path of fold containing this code
# Import data
suppressMessages(library(tidyverse))
folder <- dirname(rstudioapi::getSourceEditorContext()$path)

filename <- 'literature_coordinates.csv'
data <- read.csv(file.path(folder, "..", "results", filename), stringsAsFactors = FALSE)

df = data %>%
  group_by(country) %>%
  summarize(total_count = sum(number)) 

df1 = data %>%
  group_by(author_city, latitude, longitude) %>%
  summarize(total_city = sum(number)) 

# Get the world map data and filter out longitudes > 180
world_map <- map_data("world") %>%
  filter(! long > 180)

world_data <- left_join(world_map, df, by = c("region" = "country"))

# Replace NA with 0 in the count column
world_data$total_count[is.na(world_data$total_count)] <- 0

# # Define new custom breaks and labels
breaks <- c(0, 1, 2, 3, 4, 5, 7, 10, 21, 25, Inf)
labels <- c('0', '1', '2', '3', '4', '5', '7', '10', '21', '25')

world_data$total_count <- cut(world_data$total_count, breaks = breaks,
    labels = labels, include.lowest = TRUE, right = FALSE)

custom_palette <- c("beige", viridis(n = length(labels) - 1, direction = -1,
    begin = 0.1, end = 0.9))
names(custom_palette) <- labels

ggplot(df1, aes(x = longitude, y = latitude)) +
  geom_point(color = "blue", size = 3)

map_plot <-
  ggplot(world_data, aes(x = long, y = lat, group = group, fill = total_count)) +
  geom_polygon(color = "gray80", linewidth = 0.2) +  
  expand_limits(x = world_data$long, y = world_data$lat) +
  scale_fill_manual(values = custom_palette, name = "Papers per Country", drop = FALSE) +
  labs(colour = NULL, 
       title = "(a) Quantity of First-Author Broadband Sustainability Papers.",
       subtitle = "Spatial distribution of papers by country.",
       x = "Longitude", y = "Latitude") + 
  theme(
    legend.position = 'bottom',
    axis.text.x = element_text(size = 10),
    panel.spacing = unit(0.6, "lines"),
    plot.title = element_text(size = 11, face = "bold"),
    plot.subtitle = element_text(size = 9),
    axis.text.y = element_text(size = 10),
    legend.title = element_text(size = 8),
    legend.text = element_text(size = 8),
    axis.title.x = element_text(size = 10)
  ) +
  guides(fill = guide_legend(title = "Count", reverse = FALSE, nrow = 1)) +
  annotation_scale(location = "bl", width_hint = 0.5) + 
  ggspatial::annotation_north_arrow(
    location = "tr", which_north = "true",
    pad_x = unit(0.1, "in"), pad_y = unit(0.1, "in"),
    style = ggspatial::north_arrow_nautical(
      fill = c("grey40", "white"),
      line_col = "grey20",
      text_family = "ArcherPro Book"
    )
  ) 






