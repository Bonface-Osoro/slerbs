library(ggplot2)
library(dplyr)
library(tidyr)
library(viridis)
#library(patchwork)
library(stringr)
library(ggpubr)
library(maps)
library(scales)
library(ggrepel)
library(patchwork)
library(tools)
library(cowplot)

################################################
#######review_of_the_literature_plots###########
################################################
##############gallagher_oughton#################
################################################

# Get relative path of fold containing this code
# Import data
suppressMessages(library(tidyverse))
folder <- dirname(rstudioapi::getSourceEditorContext()$path)

filename <- 'fixed_countryplot2.csv'
df <- read.csv(file.path(folder, "..", "data", filename), stringsAsFactors = FALSE)

# Aggregate data by country to count publications or other measures
country_data <- df %>%
  group_by(standard_country) %>%
  summarize(count = n(), .groups = 'drop')

# Get the world map data and filter out longitudes > 180
world_map <- map_data("world") %>%
  filter(! long > 180)

# Create a mapping between standard_country and region
country_mapping <- c(
  "United States of America" = "USA",
  "United Kingdom" = "UK",
  "Korea, Republic of" = "South Korea",
  "Taiwan, Province of China" = "Taiwan",
  "Türkiye" = "Turkey",
  "Colombia" = "Colombia",
  "Algeria" = "Algeria",
  "Lithuania" = "Lithuania",
  "Iceland" = "Iceland"
)

# Replace standard_country values with the mapped region names
country_data$standard_country <- ifelse(country_data$standard_country %in% names(country_mapping),
    country_mapping[country_data$standard_country],
     country_data$standard_country)

# Merge publication data with world map data
world_data <- left_join(world_map, country_data, by = c("region" = "standard_country"))

# Replace NA with 0 in the count column
world_data$count[is.na(world_data$count)] <- 0

# Define new custom breaks and labels
breaks <- c(0, 1, 6, 11, 16, 21, 26, 31, 36, Inf)
labels <- c("0", "1-5", "6-10", "11-15", "16-20", "21-25", "26-30", "31-35", "36+")

# Create a factor for fill
world_data$count_factor <- cut(world_data$count,
                               breaks = breaks,
                               labels = labels,
                               include.lowest = TRUE,
                               right = FALSE)

custom_palette <- c("grey", viridis(n = length(labels) - 1, direction = -1, begin = 0.1, end = 0.9))
names(custom_palette) <- labels

# Plot the world map
map_plot <-
  ggplot(world_data, aes(x = long, y = lat, group = group, fill = count_factor)) +
  geom_polygon(color = "gray80", linewidth = 0.2) +  # Changed border color to light gray
  expand_limits(x = world_data$long, y = world_data$lat) +
  coord_map("moll", xlim = c(-180, 180), ylim = c(-50, 90)) +
  scale_fill_manual(values = custom_palette,
                    name = "Count",
                    drop = FALSE) +
  labs(title = "(A) Publication Count by Country",
       subtitle = "Reported for multispectral object detection publications nationally.") +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.background = element_rect(fill = "gray95", color = NA),
    plot.background = element_rect(fill = "gray95", color = NA),
    legend.position = "right",
    legend.box = "vertical",
    legend.key.width = unit(1, "cm"),
    legend.key.height = unit(0.5, "cm"),
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 12),
    plot.title = element_text(hjust = 0, vjust = 0, size = 20, margin = margin(b = 5)),
    plot.subtitle = element_text(hjust = 0, vjust = 0, size = 16, margin = margin(b = 10)),
    plot.margin = margin(t = 5, r = 5, b = 5, l = 5, unit = "pt")
  ) +
  guides(fill = guide_legend(title = "Count", reverse = TRUE, ncol = 1))

#############################
#######plot_2_china_map######
#############################

top_5_yolo_groups <- df %>%
  filter(country == "China", grepl("YOLO", model, ignore.case = TRUE)) %>%
  mutate(model_group = case_when(
    grepl("YOLO-modified", model, ignore.case = TRUE) ~ "YOLO-modified",
    grepl("YOLOv5", model, ignore.case = TRUE) ~ "YOLOv5",
    grepl("YOLOv3", model, ignore.case = TRUE) ~ "YOLOv3",
    grepl("YOLOv4", model, ignore.case = TRUE) ~ "YOLOv4",
    grepl("GMD-YOLO", model, ignore.case = TRUE) ~ "GMD-YOLO",
    TRUE ~ "Other YOLO"
  )) %>%
  count(model_group) %>%
  top_n(5, n) %>%
  arrange(desc(n)) %>%
  pull(model_group)

# Filter china_cities to include only the top 5 YOLO model groups
china_cities <- df %>%
  filter(country == "China", !is.na(latitude), !is.na(longitude), !is.na(author_city)) %>%
  mutate(model_group = case_when(
    grepl("YOLO-modified", model, ignore.case = TRUE) ~ "YOLO-modified",
    grepl("YOLOv5", model, ignore.case = TRUE) ~ "YOLOv5",
    grepl("YOLOv3", model, ignore.case = TRUE) ~ "YOLOv3",
    grepl("YOLOv4", model, ignore.case = TRUE) ~ "YOLOv4",
    grepl("GMD-YOLO", model, ignore.case = TRUE) ~ "GMD-YOLO",
    TRUE ~ "Other YOLO"
  )) %>%
  group_by(author_city, latitude, longitude) %>%
  summarize(count = n(),
            most_common_model = model_group[which.max(table(model_group))],
            .groups = 'drop') %>%
  mutate(most_common_model = if_else(most_common_model %in% top_5_yolo_groups, most_common_model, "Other YOLO")) %>%
  arrange(desc(count))

# Filter cities for labeling (2 or more publications)
cities_to_label <- china_cities %>% filter(count >= 2)

# Get the map of China
china_map <- map_data("world", region = "China")

# Create the China plot
china_plot <- 
  ggplot() +
  geom_polygon(data = china_map, aes(x = long, y = lat, group = group), fill = "gray70", colour = "white") +
  geom_point(data = china_cities, aes(x = longitude, y = latitude, size = count, color = most_common_model), alpha = 0.7) +
  scale_size(range = c(3, 20), name = "Number of Publications") +
  scale_color_viridis_d(name = NULL,
                        direction = -1,
                        begin = 0.1,
                        end = 0.9,
                        alpha = 1,
                        option = "viridis") +
  labs(title = "(B) Publication Count for Chinese Cities",
       subtitle = "Reported with the top five most used YOLO models.") +
  theme_void() +
  theme(
    plot.title = element_text(hjust = 0, vjust = 2, size = 20),
    plot.subtitle = element_text(hjust = 0, vjust = 2, size = 16),
    panel.background = element_rect(fill = "gray95", color = NA),
    plot.background = element_rect(fill = "gray95", color = NA),
    legend.position = "right",
    legend.box = "vertical",
    legend.key.size = unit(1, "cm"),
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 12),
    legend.box.margin = margin(l = 10)
  ) +
  geom_label_repel(data = cities_to_label,
                   aes(x = longitude, y = latitude,
                       label = author_city,
                       fill = most_common_model),
                   color = "black",
                   fontface = "bold",
                   size = 3,
                   box.padding = 1.0,
                   point.padding = 0.3,
                   min.segment.length = 0,
                   max.overlaps = Inf,
                   force = 25,
                   max.time = 2,
                   segment.color = "black",
                   segment.size = 0.5,
                   nudge_x = ifelse(cities_to_label$longitude > mean(china_map$long), 1, -1),
                   nudge_y = ifelse(cities_to_label$latitude > mean(china_map$lat), 1, -1),
                   direction = "both",
                   label.size = NA) +
  scale_fill_viridis_d(direction = -1, begin = 0.1, end = 0.9, alpha = 0.5, option = "viridis") +
  coord_fixed(ratio = 1.3) +
  guides(
    color = guide_legend(override.aes = list(size = 8, shape = 15), ncol = 1, order = 1,reverse = TRUE),
    size = guide_legend(order = 2, ncol = 1),
    fill = "none"
  )

# Combine the plots using patchwork with adjusted heights
combined_plot <- map_plot / china_plot +
  plot_layout(heights = c(1, 1.5)) +
  plot_annotation(theme = theme(plot.margin = margin(10, 10, 10, 10)))

# Save the combined plot with adjusted dimensions
folder <- dirname(rstudioapi::getSourceEditorContext()$path)
ggsave(
  file.path(folder, 'data', 'plots', 'map_plots.png'),
  plot = combined_plot,
  width = 8,
  height = 9,
  units = "in",
  dpi = 300,
  bg = "white"
)


##########################
####### keywords_plot ####
##########################

filename <- 'clean_countryplot.csv'
df <- read.csv(file.path(folder, "..", "data", filename), stringsAsFactors = FALSE)

# Process the data
df <- df %>%
  mutate(sensor = as.character(sensor)) %>%
  separate_rows(sensor, sep = ",\\s*") %>%
  mutate(words = strsplit(as.character(cleaned_ngrams), ",\\s*")) %>%
  unnest(words) %>%
  mutate(words = gsub(",+$", "", words)) %>%
  group_by(sensor, words) %>%
  summarize(count = n(), .groups = 'drop')

# Get top sensors
top_sensors <- df %>%
  group_by(sensor) %>%
  summarize(total_count = sum(count)) %>%
  arrange(desc(total_count)) %>%
  slice_head(n = 10) %>%
  pull(sensor)

# Get top 10 words
top_words <- df %>%
  group_by(words) %>%
  summarize(total_count = sum(count), .groups = 'drop') %>%
  arrange(desc(total_count)) %>%
  slice_head(n = 10) %>%
  pull(words)

# Filter data for top sensors and words
filtered_data <- df %>%
  filter(words %in% top_words, sensor %in% top_sensors) %>%
  ungroup() %>%
  mutate(words = factor(toTitleCase(words), levels = toTitleCase(top_words), ordered = TRUE))

# Calculate the maximum total count for any word
max_word_count <- filtered_data %>%
  group_by(words) %>%
  summarize(total = sum(count)) %>%
  pull(total) %>%
  max()

# Round up to the nearest 20
max_word_count_rounded <- ceiling(max_word_count / 20) * 20

# Calculate totals for each word
word_totals <- filtered_data %>%
  group_by(words) %>%
  summarize(total = sum(count), .groups = 'drop')

# Create plot
plot2 <- ggplot(filtered_data, aes(x = words, y = count, fill = sensor)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(data = word_totals, 
            aes(y = total, x = words, label = total), 
            hjust = -0.2, vjust = 0.5, size = 3,
            inherit.aes = FALSE) +  # Add this line to prevent inheriting aesthetics
  scale_fill_viridis_d(direction = -1) +
  labs(
    title = "(B) Top 10 Words by Sensor",
    subtitle = "Top 10 words used across the YOLO multispectral literature.",
    x = NULL,
    y = "Count",
    fill = "Sensor"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    axis.text.y = element_text(size = 10),
    axis.title = element_text(size = 14),
    plot.title = element_text(size = 16),
    plot.subtitle = element_text(size = 10),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12),
    legend.position = "right"
  ) +
  scale_x_discrete(limits = rev) +  # Reverse the order of x-axis
  scale_y_continuous(
    expand = c(0, 0.15),  # Increased upper expansion for labels
    limits = c(0, max_word_count_rounded * 1.15),  # Increased upper limit for labels
    breaks = seq(0, max_word_count_rounded, by = 20)
  ) +
  coord_flip()
