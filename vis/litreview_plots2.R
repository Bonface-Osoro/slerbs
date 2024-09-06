################################################
#######review_of_the_literature_plots###########
################################################
##############gallagher_oughton#################
################################################

############################
#######stack_plot_1#########
############################

library(ggplot2)
library(dplyr)
library(tidyr)
library(viridis)
#library(patchwork)
library(stringr)
library(ggpubr)

# Get relative path of fold containing this code
# Import data

folder <- dirname(dirname(rstudioapi::getSourceEditorContext()$path))
filename <- 'countryplot.csv'
df <- read.csv(file.path(folder, "documents", "data", filename), stringsAsFactors = FALSE)

############################################
#######plot_1_country_platform_plot#########
############################################

library(dplyr)
library(ggplot2)
library(tidyr)
library(stringr)

# Read the initial data
folder <- dirname(dirname(rstudioapi::getSourceEditorContext()$path))
filename <- 'countryplot.csv'
df <- read.csv(file.path(folder, "documents", "data", filename), stringsAsFactors = FALSE)

# Process the data
df1 <- df %>%
  separate_rows(platform, sep = ",") %>%
  mutate(
    platform = trimws(platform),
    country = tools::toTitleCase(tolower(country))
  ) %>%
  filter(platform != "", platform != "-") %>%
  mutate(platform = tools::toTitleCase(platform)) %>%
  group_by(country, platform) %>%
  summarize(count = n(), .groups = 'drop') %>%
  ungroup()

# Identify the top 10 countries
top_10_countries <- df1 %>%
  group_by(country) %>%
  summarize(total = sum(count), .groups = 'drop') %>%
  arrange(desc(total)) %>%
  head(10) %>%
  pull(country)

# Group other countries into "Other"
df1 <- df1 %>%
  mutate(country = ifelse(country %in% top_10_countries, country, "Other"))

# Recalculate the country order, including "Other"
country_order <- df1 %>%
  group_by(country) %>%
  summarize(total = sum(count), .groups = 'drop') %>%
  arrange(desc(total)) %>%
  pull(country)

df1$country <- factor(df1$country, levels = country_order)

# Define platform order
df1$platform <- factor(df1$platform, levels = c("Ground", "Satellite", "Multirotor", "Aircraft"))

df_errorbar <- 
  df1 |>
  group_by(country) |>
  summarize(
    count = sum(count)
  ) |>
  group_by(country) |>
  summarize(
    platform = 'Satellite', 
    count = sum(count)
  )

plot1 <-
  ggplot(df1, aes(x = country, y = count, fill = platform)) +
  geom_bar(aes(y = count), stat = "identity", position = "stack") +
  geom_text(data = df_errorbar, aes(label = paste(round(count, 2), "")), 
            size = 2.5, vjust = -0.5, hjust = 0.3, angle = 0) +
  scale_fill_viridis_d(option = "D", direction = -1) +  # Inverted color palette
  labs(title = "(A) Paper Count for the Top 10 Countries",
       subtitle = "Reported by platform type.",
       x = "Country",
       y = "Paper Count",
       fill = "Platform\nType") +
  theme_minimal() +
  guides(fill = guide_legend(nrow = 2)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text = element_text(size = 8),
        axis.title = element_text(size = 8),
        plot.title = element_text(size = 12),
        plot.subtitle = element_text(size = 10),
        legend.title = element_text(size = 8),
        legend.text = element_text(size = 8),
        legend.position = "bottom") + 
  scale_y_continuous(expand = c(0, 0), limits = c(0, 135))


############################################
#######plot_2_cnn_model_sensor_plot#########
############################################
library(dplyr)
library(ggplot2)
library(tidyr)

df2 <- df %>%
  separate_rows(sensor, sep = ",") %>%
  separate_rows(model, sep = ",") %>%
  mutate(sensor = trimws(sensor), model = trimws(model)) %>%
  filter(sensor != "", model != "") %>%
  mutate(model = toupper(model)) %>%
  group_by(sensor, model) %>%
  summarize(count = n(), .groups = 'drop') %>%
  ungroup()

top_10_models <- df2 %>%
  group_by(model) %>%
  summarize(total = sum(count), .groups = 'drop') %>%
  arrange(desc(total)) %>%
  head(10) %>%
  pull(model)

# Ensure the "other" is between "YOLO-MODIFIED" and "YOLOV5"
top_10_models <- c(top_10_models[1:which(top_10_models == "YOLO-MODIFIED")],
                   "other",  # Changed to lowercase
                   top_10_models[(which(top_10_models == "YOLO-MODIFIED") + 1):length(top_10_models)])

top_10_sensors <- df2 %>%
  group_by(sensor) %>%
  summarize(total = sum(count), .groups = 'drop') %>%
  arrange(desc(total)) %>%
  head(10) %>%
  pull(sensor)

df2_filtered <- df2 %>%
  mutate(model = ifelse(model %in% top_10_models, model, "other"),  # Changed to lowercase
         sensor = ifelse(sensor %in% top_10_sensors, sensor, "OTHER")) %>%
  filter(sensor != "OTHER")

df2_filtered$model <- factor(df2_filtered$model, levels = top_10_models)
df2_filtered$sensor <- factor(df2_filtered$sensor, levels = c(top_10_sensors, "other"))  # Changed to lowercase

df_errorbar <- df2_filtered %>%
  group_by(model) %>%
  summarize(count = sum(count), .groups = 'drop') %>%
  mutate(sensor = 'Coastal')

plot2 <- ggplot(df2_filtered, aes(x = model, y = count, fill = sensor)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(data = df_errorbar, aes(label = paste(round(count, 2), "")),
            size = 2.5, vjust = -0.5, hjust = 0.3, angle = 0) +
  scale_fill_viridis_d(option = "D", direction = -1) +  # Inverted color palette
  labs(title = "(B) Paper Count for the Top 10 CNN Models",
       subtitle = "Reported by top 10 sensor type.",
       x = "Model",
       y = "Paper Count",
       fill = NULL) +
  guides(fill = guide_legend(nrow = 2)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text = element_text(size = 8),
        axis.title = element_text(size = 8),
        plot.title = element_text(size = 12),
        plot.subtitle = element_text(size = 10),
        legend.title = element_text(size = 8),
        legend.text = element_text(size = 8),
        legend.position = "bottom") + 
  scale_y_continuous(expand = c(0, 0), limits = c(0, 135))


#########################################
#######plot_3_publication_year###########
#########################################

library(dplyr)
library(ggplot2)

# Read the initial data
folder <- dirname(dirname(rstudioapi::getSourceEditorContext()$path))
filename <- 'countryplot.csv'
df <- read.csv(file.path(folder, "documents", "data", filename), stringsAsFactors = FALSE)

# Select the relevant columns
data <- select(df, year, publisher)

# Group and summarize the data
data <- data %>%
  group_by(year, publisher) %>%
  summarize(count = n(), .groups = 'drop')

# Identify the top 5 publishers
top_5_publishers <- data %>%
  group_by(publisher) %>%
  summarize(total = sum(count), .groups = 'drop') %>%
  arrange(desc(total)) %>%
  head(5) %>%
  pull(publisher)

# Group the publishers
data <- data %>%
  mutate(publisher = ifelse(publisher %in% top_5_publishers, publisher, "OTHER"))

# Set factor levels for publishers
data$publisher <- factor(data$publisher, levels = c(top_5_publishers, "OTHER"))

# Prepare data for error bars
df_errorbar <- data %>%
  group_by(year) %>%
  summarize(count = sum(count), .groups = 'drop') %>%
  mutate(publisher = 'IEEE')

# Plot the data
plot3 <- ggplot(data, aes(x = year, y = count, fill = publisher)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(data = df_errorbar, aes(label = paste(round(count, 2), "")),
            size = 2.5, vjust = -0.5, hjust = 0.3, angle = 0) +
  scale_fill_viridis_d(option = "D", direction = -1) +  # Inverted color palette
  labs(title = "(C) Paper Count by Year",
       subtitle = "Reported by top 5 publishers.",
       x = "Year",
       y = "Paper Count",
       fill = NULL) +
  guides(fill = guide_legend(nrow = 2)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text = element_text(size = 8),
        axis.title = element_text(size = 8),
        plot.title = element_text(size = 12),
        plot.subtitle = element_text(size = 10),
        legend.title = element_text(size = 8),
        legend.text = element_text(size = 8),
        legend.position = "bottom") + 
  scale_y_continuous(expand = c(0, 0), limits = c(0, 75))


#########################################
#######plot_4_publisher_access###########
#########################################
library(dplyr)
library(ggplot2)
library(stringr)

# Read the initial data
folder <- dirname(dirname(rstudioapi::getSourceEditorContext()$path))
filename <- 'countryplot.csv'
df <- read.csv(file.path(folder, "documents", "data", filename), stringsAsFactors = FALSE)

df <- df %>%
  filter(!is.na(publisher), !is.na(type), !is.na(access), publisher != "") %>%
  mutate(
    type_access = interaction(type, access, sep = " - ")
  )

# Insert line breaks in the type_access labels
df$type_access <- str_replace_all(df$type_access, " - ", "\n")

# Determine the order of type_access
type_access_order <- df %>%
  count(type_access) %>%
  arrange(desc(n)) %>%
  pull(type_access)

df$type_access <- factor(df$type_access, levels = type_access_order)

# Identify the top 10 publishers
top_10_publishers <- df %>%
  count(publisher) %>%
  arrange(desc(n)) %>%
  head(10) %>%
  pull(publisher)

# Group other publishers into "other"
df <- df %>%
  mutate(publisher = ifelse(publisher %in% top_10_publishers, publisher, "other"))

# Recalculate the publisher order, including "other"
publisher_order <- df %>%
  count(publisher) %>%
  arrange(desc(n)) %>%
  pull(publisher)

df$publisher <- factor(df$publisher, levels = publisher_order)

# Calculate the total count for each publisher
total_counts <- df %>%
  count(publisher) %>%
  arrange(match(publisher, publisher_order))

# Create the plot
plot4 <- ggplot(df, aes(x = publisher, fill = type_access)) +
  geom_bar(stat = "count", position = "stack") +
  scale_fill_viridis_d(option = "D", direction = -1, guide = guide_legend(nrow = 2)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +  # Add space at the top of the y-axis
  geom_text(data = total_counts, aes(x = publisher, y = n, label = n), 
            vjust = -0.5, size = 3, inherit.aes = FALSE) +
  labs(title = "(D) Access by Top 10 Publishers",
       subtitle = "Publications separated by type and access.",
       x = "Publisher",
       y = "Count of Publications",
       fill = "Type and Access") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text = element_text(size = 8),
        axis.title = element_text(size = 8),
        plot.title = element_text(size = 12),
        plot.subtitle = element_text(size = 10),
        legend.title = element_text(size = 8),
        legend.text = element_text(size = 8),
        legend.position = "bottom")

#####################################
#######plot_5_yolo_type##############
#####################################
library(dplyr)
library(ggplot2)
library(tidyr)
library(stringr)

# Read the initial data
folder <- dirname(dirname(rstudioapi::getSourceEditorContext()$path))
filename <- 'countryplot.csv'
df <- read.csv(file.path(folder, "documents", "data", filename), stringsAsFactors = FALSE)

df_split <- df %>%
  separate_rows(technologies, sep = ", ") %>%
  separate_rows(sensor, sep = ", ") %>%
  mutate(technologies = str_trim(technologies),
         sensor = str_trim(sensor)) %>%
  filter(str_detect(technologies, "YOLO")) %>%
  group_by(technologies, sensor) %>%
  summarise(n = n(), .groups = "drop") %>%
  ungroup()

# Identify the top 10 YOLO variants
top_10_yolo <- df_split %>%
  group_by(technologies) %>%
  summarise(total = sum(n), .groups = "drop") %>%
  arrange(desc(total)) %>%
  head(10) %>%
  pull(technologies)

# Group other YOLO variants into "Other"
df_split <- df_split %>%
  mutate(technologies = ifelse(technologies %in% top_10_yolo, technologies, "Other"))

# Filter out "Other" if it makes the list more than 10
df_split <- df_split %>%
  group_by(technologies) %>%
  filter(!(technologies == "Other" & n() > 10)) %>%
  ungroup()

# Recalculate the order of YOLO variants including "Other"
yolo_order <- df_split %>%
  group_by(technologies) %>%
  summarise(total = sum(n), .groups = "drop") %>%
  arrange(desc(total)) %>%
  pull(technologies)

# Identify the top 10 sensors
top_10_sensors <- df_split %>%
  group_by(sensor) %>%
  summarise(total = sum(n), .groups = "drop") %>%
  arrange(desc(total)) %>%
  head(10) %>%
  pull(sensor)

# Group other sensors into "Other"
df_split <- df_split %>%
  mutate(sensor = ifelse(sensor %in% top_10_sensors, sensor, "Other"))

# Recalculate the order of sensors including "Other"
sensor_order <- df_split %>%
  group_by(sensor) %>%
  summarise(total = sum(n), .groups = "drop") %>%
  arrange(desc(total)) %>%
  pull(sensor)

# Calculate the total count for each technology (YOLO variant)
total_counts <- df_split %>%
  group_by(technologies) %>%
  summarise(total = sum(n), .groups = "drop")

# Create the plot with data labels on top of the bars
plot5 <- ggplot(df_split, aes(x = factor(technologies, levels = yolo_order), y = n, fill = factor(sensor, levels = sensor_order))) +
  geom_bar(stat = "identity") +
  geom_text(data = total_counts, aes(x = technologies, y = total, label = total), 
            vjust = -0.5, size = 3, inherit.aes = FALSE) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +  # Add space at the top of the y-axis
  scale_fill_viridis_d(option = "D", direction = -1) +
  labs(title = "(E) Top 10 YOLO Variants by Sensor Type",
       subtitle = "YOLO versions broken down by modifications and sensor applications.",
       x = "YOLO Variants",
       y = "Count",
       fill = "Sensor Type") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text = element_text(size = 8),
        axis.title = element_text(size = 8),
        plot.title = element_text(size = 12),
        plot.subtitle = element_text(size = 10),
        legend.title = element_text(size = 8),
        legend.text = element_text(size = 8),
        legend.position = "bottom")


###################################################
#######plot_6_top10_classes_top5_sensors###########
###################################################
library(dplyr)
library(ggplot2)
library(tidyr)
library(stringr)

# Read the initial data
folder <- dirname(dirname(rstudioapi::getSourceEditorContext()$path))
filename <- 'countryplot.csv'
df <- read.csv(file.path(folder, "documents", "data", filename), stringsAsFactors = FALSE)

df <- df %>%
  filter(!is.na(object_class) & object_class != "" & !is.na(sensor) & sensor != "")

df_summary <- df %>%
  separate_rows(object_class, sep = ", ") %>%
  separate_rows(sensor, sep = ", ") %>%
  mutate(object_class = str_replace_all(object_class, "-", "Other"),  # Change "OTHER" to "Other"
         sensor = str_trim(sensor)) %>%
  group_by(object_class, sensor) %>%
  summarize(count = n(), .groups = 'drop')

df_summary <- df_summary %>%
  group_by(object_class) %>%
  mutate(total_count_object = sum(count)) %>%
  group_by(sensor) %>%
  mutate(total_count_sensor = sum(count)) %>%
  ungroup()

top_object_classes <- df_summary %>%
  distinct(object_class, .keep_all = TRUE) %>%
  filter(object_class != "COCO") %>%
  arrange(desc(total_count_object)) %>%
  head(10) %>%
  pull(object_class)

top_sensors <- df_summary %>%
  distinct(sensor, .keep_all = TRUE) %>%
  arrange(desc(total_count_sensor)) %>%
  head(5) %>%
  pull(sensor)

df_summary_filtered <- df_summary %>%
  filter(object_class %in% top_object_classes & sensor %in% top_sensors)

object_class_total <- df_summary_filtered %>%
  group_by(object_class) %>%
  summarize(total_count = sum(count), .groups = 'drop') %>%
  arrange(desc(total_count))

df_summary_filtered$object_class <- factor(df_summary_filtered$object_class, levels = object_class_total$object_class)
df_summary_filtered$sensor <- factor(df_summary_filtered$sensor, levels = rev(sort(top_sensors)))

# Calculate the total count for each object class
total_counts <- df_summary_filtered %>%
  group_by(object_class) %>%
  summarize(total = sum(count), .groups = 'drop')

# Create the plot with data labels on top of the bars and a two-row legend
plot6 <- ggplot(df_summary_filtered, aes(x = object_class, y = count, fill = sensor)) +
  geom_bar(stat = "identity") +
  geom_text(data = total_counts, aes(x = object_class, y = total, label = total), 
            vjust = -0.5, size = 3, inherit.aes = FALSE) +  # Slight adjustment to vjust
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +  # Add space at the top of the y-axis
  scale_fill_viridis_d(option = "D", direction = -1) +
  labs(title = "(F) Top 10 Object Classes and Top 5 Sensors",
       subtitle = "Top 5 sensors used for detecting the top 10 object classes.",
       x = "Object Class",
       y = "Count",
       fill = "Sensor") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text = element_text(size = 8),
        axis.title = element_text(size = 8),
        plot.title = element_text(size = 12),
        plot.subtitle = element_text(size = 10),
        legend.title = element_text(size = 8),
        legend.text = element_text(size = 8),
        legend.position = "bottom") +
  guides(fill = guide_legend(nrow = 2, reverse = TRUE))  # Adjusted to display legend in two rows



# Combine all plots into a 2x3 grid using ggarrange
panel = ggarrange(
  plot1, plot2,
  plot3, plot4,
  plot5, plot6,
  ncol = 2, nrow = 3,
  common.legend = FALSE,
  legend = 'bottom'
)

# Save the combined plot
folder <- dirname(rstudioapi::getSourceEditorContext()$path)
ggsave(
  file.path(folder, 'data', 'plots', 'combined_plots.png'),
  plot = panel,
  width = 10,
  height = 12,
  units = "in",
  bg = "white"
)


# ################################################################
# #######################stack_plots_2_maps#######################
# ################################################################

# #############################
# #######plot_1_world_map######
# #############################

library(ggplot2)
library(dplyr)
library(tidyr)
library(maps)
library(scales)
library(viridis)

# Load the data
folder <- dirname(dirname(rstudioapi::getSourceEditorContext()$path))
filename <- 'fixed_countryplot2.csv'
df <- read.csv(file.path(folder, "documents", "data", filename), stringsAsFactors = FALSE)

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

library(ggplot2)
library(dplyr)
library(tidyr)
library(maps)
library(scales)
library(ggrepel)
library(patchwork)

# Group similar YOLO models and get top 5
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

##########################################
########## platform_n-gram_panel_plot ####
##########################################

library(dplyr)
library(ggplot2)
library(tidyr)
library(viridis)
library(tools)
library(cowplot)
library(patchwork)

folder <- dirname(dirname(rstudioapi::getSourceEditorContext()$path))
filename <- 'countryplot.csv'
df <- read.csv(file.path(folder, "documents", "data", filename), stringsAsFactors = FALSE)

df1 <- df %>%
  separate_rows(sensor, sep = ",") %>%
  separate_rows(platform, sep = ",") %>%
  mutate(
    sensor = trimws(sensor),
    platform = trimws(platform)
  ) %>%
  filter(sensor != "", sensor != "-", platform != "", platform != "-") %>%
  mutate(
    sensor = tools::toTitleCase(sensor),
    platform = tools::toTitleCase(platform)
  ) %>%
  group_by(sensor, platform) %>%
  summarize(count = n(), .groups = 'drop') %>%
  ungroup()

top_8_sensors <- df1 %>%
  group_by(sensor) %>%
  summarize(total = sum(count), .groups = 'drop') %>%
  arrange(desc(total)) %>%
  head(8) %>%
  pull(sensor)

df1_filtered <- df1 %>%
  filter(sensor %in% top_8_sensors)

# Reorder the sensors based on total count
sensor_order <- df1_filtered %>%
  group_by(sensor) %>%
  summarize(total = sum(count)) %>%
  arrange(desc(total)) %>%
  pull(sensor)

df1_filtered$sensor <- factor(df1_filtered$sensor, levels = sensor_order)

# Reorder the platforms based on total count
platform_order <- df1_filtered %>%
  group_by(platform) %>%
  summarize(total = sum(count), .groups = 'drop') %>%
  arrange(desc(total)) %>%
  pull(platform)

df1_filtered$platform <- factor(df1_filtered$platform, levels = platform_order)

# Calculate the maximum total count for any sensor
max_count <- df1_filtered %>%
  group_by(sensor) %>%
  summarize(total = sum(count)) %>%
  pull(total) %>%
  max()

# Round up to the nearest 20
max_count_rounded <- ceiling(max_count / 20) * 20

# Calculate totals for each sensor
sensor_totals <- df1_filtered %>%
  group_by(sensor) %>%
  summarize(total = sum(count), .groups = 'drop')

plot1 <- ggplot(df1_filtered, aes(y = sensor, x = count, fill = platform)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(data = sensor_totals, 
            aes(x = total, y = sensor, label = total), 
            hjust = -0.2, vjust = 0.5, size = 3,
            inherit.aes = FALSE) +  # Add this line to prevent inheriting aesthetics
  scale_fill_viridis_d(option = "D", direction = -1) +
  labs(
    title = "(A) Top 8 Sensors by Platform Type",
    subtitle = "Top 8 sensors broken down by the platforms they are mounted to.",
    x = "Count",
    y = "Sensor",
    fill = "Platform"
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 10),
    axis.text.x = element_text(size = 10),
    axis.title = element_text(size = 14),
    plot.title = element_text(size = 16),
    plot.subtitle = element_text(size = 10),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12),
    legend.position = "right",
    panel.grid.major.y = element_line(color = "gray90"),
    panel.grid.minor.y = element_blank()
  ) +
  scale_y_discrete(limits = rev(levels(df1_filtered$sensor))) +
  scale_x_continuous(
    expand = c(0, 0.15),
    limits = c(0, max_count_rounded * 1.15),
    breaks = seq(0, max_count_rounded, by = 20)
  ) +
  guides(fill = guide_legend(reverse = TRUE))

##########################
####### keywords_plot ####
##########################

filename <- 'clean_countryplot.csv'
df <- read.csv(file.path(folder, "documents", "data", filename), stringsAsFactors = FALSE)

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

# Combine the plots using patchwork
combined_plot <- wrap_plots(plot1, plot2, ncol = 2)

# Save the combined plot
folder <- dirname(rstudioapi::getSourceEditorContext()$path)
ggsave(
  file.path(folder, 'data', 'plots', 'panel2_plots_side_by_side_with_labels.png'),
  plot = combined_plot,
  width = 14,  # Slightly increased width to accommodate labels
  height = 4,  # Adjusted height
  units = "in",
  bg = "white"
)


##############################
#######dataset_plot###########
##############################

library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)
library(viridis)
library(tools)

folder <- dirname(dirname(rstudioapi::getSourceEditorContext()$path))
filename <- 'clean_countryplot.csv'
df <- read.csv(file.path(folder, "documents", "data", filename), stringsAsFactors = FALSE)

# Process the data
df <- df %>%
  filter(!is.na(dataset_name) & dataset_name != "" & dataset_name != "-") %>%
  mutate(model = as.character(model)) %>%
  separate_rows(model, sep = ",\\s*") %>%
  mutate(model = str_trim(model)) %>%
  # Combine all KAIST datasets
  mutate(dataset_name = ifelse(str_detect(dataset_name, "KAIST"), "KAIST", dataset_name)) %>%
  # Uppercase the first letter of "custom"
  mutate(dataset_name = ifelse(tolower(dataset_name) == "custom", "Custom", dataset_name))

# Get top 3 YOLO models
top_yolo_models <- df %>%
  filter(str_detect(model, "YOLO")) %>%
  group_by(model) %>%
  summarize(count = n()) %>%
  arrange(desc(count)) %>%
  slice_head(n = 3) %>%
  pull(model)

# Get top 3 datasets
top_datasets <- df %>%
  group_by(dataset_name) %>%
  summarize(count = n()) %>%
  arrange(desc(count)) %>%
  slice_head(n = 3) %>%
  pull(dataset_name)

# Filter data for top datasets and top YOLO models
filtered_data <- df %>%
  filter(dataset_name %in% top_datasets, model %in% top_yolo_models)

# Calculate total count for each dataset
dataset_totals <- filtered_data %>%
  group_by(dataset_name) %>%
  summarize(total = n()) %>%
  arrange(desc(total))

# Create plot
plot <- ggplot(filtered_data, aes(x = factor(dataset_name, levels = dataset_totals$dataset_name), fill = model)) +
  geom_bar(position = "stack") +
  scale_fill_viridis_d(direction = -1) +
  geom_text(data = dataset_totals, aes(x = dataset_name, y = total, label = total), 
            inherit.aes = FALSE, hjust = -0.1, size = 5) +  # Add aggregate data labels
  labs(
    title = "Top 3 Datasets by Top 3 YOLO Models",
    subtitle = "Breakdown of YOLO models used for each dataset.",
    x = "Dataset",
    y = "Count",
    fill = "YOLO Model"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
    axis.text.y = element_text(size = 12),
    axis.title = element_text(size = 16),
    plot.title = element_text(size = 20),
    plot.subtitle = element_text(size = 14),
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 14),
    legend.position = "bottom",  # Move legend to bottom
    legend.box = "horizontal"  # Arrange legend items horizontally
  ) +
  coord_flip()

# Save the plot
ggsave(
  file.path(folder, 'documents', 'data', 'plots', 'top3datasets_top3yolo_breakdown.png'),
  plot = plot,
  width = 7,
  height = 4,
  units = "in",
  bg = "white",
  dpi = 300
)



####################################
#########YOLO_multispectral#########
####################################

library(ggplot2)
library(dplyr)
library(tidyr)
library(viridis)

# Load the data
folder <- dirname(dirname(rstudioapi::getSourceEditorContext()$path))
filename <- 'yolo_multispectral.csv'
df <- read.csv(file.path(folder, "documents", "data", filename), stringsAsFactors = FALSE)

# Data preprocessing
df$mAP <- as.numeric(sub("%", "", df$mAP))  # Remove % and convert to numeric
df$group <- factor(df$group, levels = unique(df$group))  # Ensure 'group' is a factor to maintain order
df$YOLO.Model <- factor(df$YOLO.Model, levels = unique(df$YOLO.Model))  # Maintain YOLO Model order

# Create the plot with tight bars and correct labels
p <- ggplot(df, aes(x = YOLO.Model, y = mAP, fill = Dataset)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), width = 0.9) +  # Bars touch each other
  geom_text(aes(label = paste0(mAP, "%")), 
            position = position_dodge(width = 0.9), 
            vjust = -0.5, color = "black", size = 5) +  # Increased font size for labels
  scale_fill_viridis_d(option = "D", direction = -1) +
  facet_grid(. ~ group, scales = "free_x", space = "free_x") +
  labs(title = "Multispectral YOLO Model Performance",
       subtitle = "mAP of multispectral YOLO models compared to default-YOLO model performance",
       x = "YOLO Model",
       y = "mAP (%)",
       fill = "Dataset") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 12),
    axis.text.y = element_text(size = 14),
    axis.title = element_text(size = 14),
    plot.title = element_text(size = 18),
    plot.subtitle = element_text(size = 16),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 14),
    legend.position = "bottom",
    strip.text = element_text(size = 16, face = "bold"),
    panel.spacing = unit(0.5, "lines")  # Reduce spacing between facets
  ) +
  coord_cartesian(clip = "off", expand = TRUE) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, max(df$mAP) * 1.15))

# Save the plot
ggsave(
  file.path(folder, 'documents', 'data', 'plots', 'yolomulti_final_larger_labels.png'),
  plot = p,
  width = 12,
  height = 8,
  dpi = 300
)

# Display the plot
print(p)




##############################
#######sensor_year_plot######
##############################
# 
# library(dplyr)
# library(ggplot2)
# library(viridis)
# library(tools)  # For toTitleCase function
# 
# folder <- dirname(dirname(rstudioapi::getSourceEditorContext()$path))
# filename <- 'countryplot.csv'
# df <- read.csv(file.path(folder, "documents", "data", filename), stringsAsFactors = FALSE)
# 
# data <- data %>%
#   filter(!is.na(year)) %>%
#   mutate(year = as.integer(year)) %>%
#   separate_rows(platform, sep = ",\\s*") %>%
#   mutate(platform = trimws(platform)) %>%
#   filter(platform != "", platform != "-") %>%
#   mutate(platform = tools::toTitleCase(platform))  # Capitalize the first letter of each word
# 
# platform_order <- data %>%
#   count(platform) %>%
#   arrange(desc(n)) %>%
#   pull(platform)
# 
# data$platform <- factor(data$platform, levels = platform_order)
# 
# plot3 <- ggplot(data, aes(x = year, fill = platform)) +
#   geom_bar(stat = "count", position = "stack") +
#   scale_fill_viridis_d() +
#   labs(title = "(C) Platforms Used by Year",
#        subtitle = "Platform usage broken down by year",
#        x = "Year",
#        y = "Count of Platforms",
#        fill = "Platform") +
#   theme_minimal() +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1),
#         axis.text = element_text(size = 12),
#         axis.title = element_text(size = 14),
#         plot.title = element_text(size = 18),
#         plot.subtitle = element_text(size = 14),
#         legend.title = element_text(size = 14),
#         legend.text = element_text(size = 12),
#         legend.position = "right")
# 
# # Save the plot to the specified output file path
# ggsave("C:\\Users\\Jim\\OneDrive\\Documents\\data\\plots\\platforms_by_year_plot.png", plot3, width = 12, height = 8, dpi = 300)

# ###########################################################
# ##################individual_plots#########################
# ###########################################################
# 
# ###########################
# #######sankey_plot#########
# ###########################
# 
# #use the below link to restructure csv data for sankey plot 
# #https://colab.research.google.com/drive/10q8LX6gC2V2DOv-ZKy8gZWX9LWh6J4x8?usp=sharing 
# 
# library(ggplot2)
# library(ggsankey)
# library(dplyr)
# library(tidyr)
# library(stringr)
# 
# # Adjust code so file works on anyone's machine
# df <- read.csv("C:/Users/Jim/OneDrive/Documents/data/test611.csv", stringsAsFactors = FALSE)
# 
# # Filter out rows with any blank or NA values in key columns before sampling
# df <- df %>%
#   filter(!(is.na(country) | country == "" | is.na(model) | model == "" | is.na(sensor) | sensor == ""))
# 
# set.seed(111)
# 
# # Correctly sample the entire rows to maintain the relationship between columns
# df_sampled <- df[sample(nrow(df), size = 100, replace = TRUE), ]
# 
# # Reshape the sampled data for the Sankey diagram
# df_long <- df_sampled %>%
#   make_long(country, model, sensor) %>%
#   mutate(x = str_to_title(x)) # Capitalize the first letter of each category name
# 
# sankey_plot <- ggplot(df_long, aes(x = x,
#                                    next_x = next_x,
#                                    node = node,
#                                    next_node = next_node,
#                                    fill = factor(node),
#                                    label = node))
# 
# sankey_plot <- sankey_plot +
#   geom_sankey(flow.alpha = 0.5,
#               node.color = "black",
#               show.legend = FALSE) +
#   geom_sankey_label(size = 3,
#                     color = "black",
#                     fill = "white",
#                     hjust = 0) +
#   scale_fill_viridis_d() +
#   theme(
#     plot.title = element_text(hjust = 0.5, size = rel(1.2)),
#     plot.subtitle = element_text(hjust = 0.5),
#     axis.title = element_blank(),
#     axis.text.y = element_blank(),
#     axis.ticks = element_blank(),
#     panel.grid = element_blank(),
#     panel.background = element_rect(fill = "white"),
#     panel.border = element_blank(),
#     plot.background = element_rect(fill = "white"),
#     legend.position = "none"
#   ) +
#   labs(title = "Country, Model, and Sensor Type", # Set main title
#        subtitle = "CNN Model and Sensors used by Author's Country of Origin", # Set subtitle
#        x = NULL, # Remove x-axis label
#        y = NULL) # Remove y-axis label
# 
# print(sankey_plot)
# 
# # Save the plot
# ggsave("C:/Users/Jim/OneDrive/Documents/data/plots/sankey_plot.png", sankey_plot, width = 8, height = 10, units = "in")