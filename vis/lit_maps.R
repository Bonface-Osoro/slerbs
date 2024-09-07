library(ggplot2)
library(dplyr)
library(tidyr)
library(sf)
library(ggspatial)

# Get relative path of fold containing this code
# Import data
suppressMessages(library(tidyverse))
folder <- dirname(rstudioapi::getSourceEditorContext()$path)

#########################
## WORLD AUTHOR PLOT  ###
#########################
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

map_plot <-
  ggplot(world_data, aes(x = long, y = lat, group = group, fill = total_count)) +
  geom_polygon(color = "gray80", linewidth = 0.2) +  
  expand_limits(x = world_data$long, y = world_data$lat) +
  scale_fill_brewer(palette = "Paired") +
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
  guides(fill = guide_legend(title = "Papers per Country", reverse = FALSE, nrow = 1)) +
  ggspatial::annotation_north_arrow(
    location = "tr", which_north = "true",
    pad_x = unit(0.1, "in"), pad_y = unit(0.1, "in"),
    style = ggspatial::north_arrow_nautical(
      fill = c("grey40", "white"),
      line_col = "grey20",
      text_family = "ArcherPro Book"
    )
  ) 

################
## USA PLOT  ###
################
usa_data <- data %>%
  filter(country == "USA") 

continental_usa_data <- usa_data %>%
  filter(longitude >= -125, longitude <= -67,
         latitude >= 24, latitude <= 50)

df2 = continental_usa_data %>%
  group_by(author_city, latitude, longitude) %>%
  summarize(total_city = sum(number)) 

usa_map <- map_data("world", region = "USA")
continental_usa_map <- usa_map %>%
  filter(long >= -125, long <= -67, lat >= 24, lat <= 50)

usa_cities_to_label <- df2 %>% filter(total_city >= 1)

usa_plot <- 
  ggplot() +
  geom_polygon(data = continental_usa_map, aes(x = long, y = lat, group = group), 
               fill = "grey", colour = "white") +
  geom_point(data = df2, aes(x = longitude, y = latitude, size = total_city, 
            show.legend = FALSE), alpha = 0.7) +
  scale_size(range = c(3, 10), name = "Number of Publications") +
  labs(title = "(b) Number of authors per major cities.",
       subtitle = "USA",
       x = "Longitude", y = "Latitude") +
  theme(axis.text.x = element_text(size = 10),
        panel.spacing = unit(0.6, "lines"),
        plot.title = element_text(size = 11, face = "bold"),
        plot.subtitle = element_text(size = 11),
        axis.text.y = element_text(size = 10),
        legend.title = element_text(size = 8),
        legend.text = element_text(size = 8),
        legend.position = "right",
        legend.box = "vertical",
        legend.key.size = unit(1, "cm"),
        legend.box.margin = margin(l = 10),
        axis.title.x = element_text(size = 10)) +
  geom_label_repel(data = usa_cities_to_label, aes(x = longitude, y = latitude,
        label = author_city, fill = author_city), color = "black",
        fontface = "bold", size = 3, box.padding = 1.0, point.padding = 0.3,
        min.segment.length = 0, max.overlaps = Inf, force = 25, max.time = 2,
        segment.color = "black", segment.size = 0.3,
        nudge_x = ifelse(usa_cities_to_label$longitude > mean(continental_usa_map$long), 1, -1),
        nudge_y = ifelse(usa_cities_to_label$latitude > mean(continental_usa_map$lat), 1, -1),
        direction = "both", label.size = NA, show.legend = FALSE) +
  scale_fill_manual(values = c("#FAEBD7", "#CDC0B0", "azure3", "#EEC591", 
        "#8B7356", "#CDC8B1", "#CDC1C5", "#8B8386", "#CDC9A5", "#CDAF95", 
        "#EED8AE", "#CDC5BF", "#FFF5EE", "#CDB5CD", "#FFDAB9", "#CD8500",
        "#6B8E23", "#CD8162", "#68838B")) 

##################
## CHINA PLOT  ###
##################
china_data <- data %>%
  filter(country == "China")

df3 = china_data %>%
  group_by(author_city, latitude, longitude) %>%
  summarize(total_city = sum(number)) 

china_map <- map_data("world", region = "China")

cities_to_label <- df3 %>% filter(total_city >= 1)

china_plot <- 
  ggplot() +
  geom_polygon(data = china_map, aes(x = long, y = lat, group = group), 
               fill = "grey", colour = "white") +
  geom_point(data = df3, aes(x = longitude, y = latitude, size = total_city, 
                             show.legend = FALSE), alpha = 0.7) +
  scale_size(range = c(3, 10), name = "Number of Publications") +
  labs(title = " ", subtitle = "China", x = "Longitude", y = "Latitude") +
  theme(axis.text.x = element_text(size = 10),
    panel.spacing = unit(0.6, "lines"),
    plot.title = element_text(size = 11, face = "bold"),
    plot.subtitle = element_text(size = 11),
    axis.text.y = element_text(size = 10),
    legend.title = element_text(size = 8),
    legend.text = element_text(size = 8),
    legend.position = "right",
    legend.box = "vertical",
    legend.key.size = unit(1, "cm"),
    legend.box.margin = margin(l = 10),
    axis.title.x = element_text(size = 10)) +
  geom_label_repel(data = cities_to_label, aes(x = longitude, y = latitude,
       label = author_city, fill = author_city), color = "black",
       fontface = "bold", size = 3, box.padding = 1.0, point.padding = 0.3,
       min.segment.length = 0, max.overlaps = Inf, force = 25, max.time = 2,
       segment.color = "black", segment.size = 0.3,
       nudge_x = ifelse(cities_to_label$longitude > mean(china_map$long), 1, -1),
       nudge_y = ifelse(cities_to_label$latitude > mean(china_map$lat), 1, -1),
       direction = "both", label.size = NA, show.legend = FALSE) +
  scale_fill_manual(values = c("#FAEBD7", "#CDC0B0", "azure3", "#EEC591", 
      "#8B7356", "#CDC8B1", "#CDC1C5", "#8B8386", "#CDC9A5", "#CDAF95", 
      "#EED8AE", "#CDC5BF", "#FFF5EE", "#CDB5CD", "#FFDAB9", "#CD8500",
      "#6B8E23", "#CD8162", "#68838B")) +
  ggspatial::annotation_north_arrow(
    location = "tr", which_north = "true",
    pad_x = unit(0.1, "in"), pad_y = unit(0.1, "in"),
    style = ggspatial::north_arrow_nautical(
      fill = c("grey40", "white"),
      line_col = "grey20",
      text_family = "ArcherPro Book"
    )
  ) 

##################
## INDIA PLOT  ###
##################
india_data <- data %>%
  filter(country == "India") 

df4 = india_data %>%
  group_by(author_city, latitude, longitude) %>%
  summarize(total_city = sum(number)) 

india_map <- map_data("world", region = "India")

india_cities_to_label <- df4 %>% filter(total_city >= 1)

india_plot <- 
  ggplot() +
  geom_polygon(data = india_map, aes(x = long, y = lat, group = group), 
               fill = "beige", colour = "white") +
  geom_point(data = df4, aes(x = longitude, y = latitude, size = total_city, 
                             show.legend = FALSE), alpha = 0.7) +
  scale_size(range = c(3, 10), name = "Number of Publications") +
  scale_fill_brewer(palette = "Spectral") +
  labs(title = " ", subtitle = "(d) India",  x = "Longitude", y = "Latitude") +
  theme(axis.text.x = element_text(size = 10),
        panel.spacing = unit(0.6, "lines"),
        plot.title = element_text(size = 11, face = "bold"),
        plot.subtitle = element_text(size = 9, face = "bold"),
        axis.text.y = element_text(size = 10),
        legend.title = element_text(size = 8),
        legend.text = element_text(size = 8),
        legend.position = "right",
        legend.box = "vertical",
        legend.key.size = unit(1, "cm"),
        legend.box.margin = margin(l = 10),
        axis.title.x = element_text(size = 10)) +
  geom_label_repel(data = india_cities_to_label, aes(x = longitude, y = latitude,
      label = author_city, fill = author_city), color = "black",
      fontface = "bold", size = 3, box.padding = 1.0, point.padding = 0.3,
      min.segment.length = 0, max.overlaps = Inf, force = 25, max.time = 2,
      segment.color = "black", segment.size = 0.3,
      nudge_x = ifelse(india_cities_to_label$longitude > mean(india_map$long), 1, -1),
      nudge_y = ifelse(india_cities_to_label$latitude > mean(india_map$lat), 1, -1),
      direction = "both", label.size = NA, show.legend = FALSE)

top_3 <- ggarrange(usa_plot, china_plot, ncol = 2, 
                   common.legend = TRUE, legend='bottom') 

combined <- ggarrange(map_plot, top_3, nrow = 2, 
          common.legend = FALSE, legend='bottom') 

path = file.path(folder, 'figures', 'article_maps.png')
png(path, units="in", width=12, height=11, res=300)
print(combined)
dev.off()




