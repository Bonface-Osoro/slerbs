library(ggpubr)
library(ggplot2)
library(tidyverse)

suppressMessages(library(tidyverse))
folder <- dirname(rstudioapi::getSourceEditorContext()$path)

data <- read.csv(file.path(folder, '..', 'data',
                            'Sustainability Literature review.csv'))

############################
## ARTICLES BY PUBLISHER ###
############################
df = data %>%
  group_by(publisher, access) %>%
  summarize(total_arts = sum(number)) 

label_totals <- df %>%
  group_by(publisher) %>%
  summarize(total_value = sum(total_arts))

publisher_tots <-
  ggplot(df, aes(x = publisher, y = total_arts)) +
  geom_bar(stat = "identity", aes(fill = access)) + 
  geom_text(data = label_totals, aes(x = publisher, y = total_value, 
      label = sprintf("%.0f", total_value)), size = 3,
      position = position_dodge(0.9), vjust = 0.05, hjust = -0.1) +
  labs(colour = NULL, title = "(a) Papers by Publisher.",
       subtitle = "Results broken down by access.",
       x = NULL, y = " ") + 
  theme(
    legend.position = 'bottom',
    axis.text.x = element_text(size = 10),
    panel.spacing = unit(0.6, "lines"),
    plot.title = element_text(size = 11, face = "bold"),
    plot.subtitle = element_text(size = 9),
    axis.text.y = element_text(size = 10),
    axis.title.y = element_markdown(size = 10),
    legend.title = element_text(size = 8),
    legend.text = element_text(size = 8),
    axis.title.x = element_text(size = 7)
  ) + expand_limits(y = 0) +
  guides(fill = guide_legend(ncol = 6, title = 'Access')) +
  scale_x_discrete(expand = c(0, 0.15)) + scale_y_continuous(expand = c(0, 0),
  labels = function(y)format(y, scientific = FALSE), limit = c(0, 60))

######################
## ARTICLES BY SDG ###
######################
df = data %>%
  group_by(SDG, income) %>%
  summarize(total_sdgs = sum(number)) 

df$SDG <- factor(df$SDG,
  levels = c('SDG 2', 'SDG 3', 'SDG 4', 'SDG 8', 'SDG 9', 'SDG 10', 'SDG 13',
             'All', 'Not Mentioned'),
  labels = c('SDG 2', 'SDG 3', 'SDG 4', 'SDG 8', 'SDG 9', 'SDG 10', 'SDG 13',
             'All', 'Not \nMentioned'))

df$income <- factor(df$income,
   levels = c('LMC', 'UMC', 'HIC', 'Global', 'None'),
   labels = c('LMC', 'UMC', 'HIC', 'Global', 'None'))

label_totals <- df %>%
  group_by(SDG) %>%
  summarize(total_value = sum(total_sdgs))

sdg_tots <-
  ggplot(df, aes(x = SDG, y = total_sdgs)) +
  geom_bar(stat = "identity", aes(fill = income)) + coord_flip() + 
  geom_text(data = label_totals, aes(x = SDG, y = total_value, 
      label = sprintf("%.0f", total_value)), size = 3,
      position = position_dodge(0.9), vjust = 0.05, hjust = -0.1) +
  labs(colour = NULL, title = "(b) Papers by SDG Addressed.",
       subtitle = "Results broken down by case-study country's income group.",
       x = NULL, y = bquote("Number of Reviewed Papers")) + 
  theme(
    legend.position = 'bottom',
    axis.text.x = element_text(size = 10),
    panel.spacing = unit(0.6, "lines"),
    plot.title = element_text(size = 11, face = "bold"),
    plot.subtitle = element_text(size = 9),
    axis.text.y = element_text(size = 10),
    axis.title.y = element_markdown(size = 7),
    legend.title = element_text(size = 8),
    legend.text = element_text(size = 8),
    axis.title.x = element_text(size = 7)
  ) + expand_limits(y = 0) +
  guides(fill = guide_legend(ncol = 6, title = 'Income Group')) +
  scale_x_discrete(expand = c(0, 0.15)) + scale_y_continuous(expand = c(0, 0),
  labels = function(y)format(y, scientific = FALSE), limit = c(0, 45))


###################################
## ARTICLES BY APPLICATION AREA ###
###################################
df = data %>%
  group_by(Application, income) %>%
  summarize(total_apps = sum(number)) 

df$Application <- factor(df$Application,
   levels = c('Agriculture', 'Healthcare', 'Education', 'Technology', 
              'General Information', 'Equality', 'Energy', 'Emissions', 
              'Business & Economy'),
   labels = c('Agriculture', 'Healthcare', 'Education', 'Technology', 
              'General \nInformation', 'Equality', 'Energy', 'Emissions', 
              'Business \n& Economy'))

df$income <- factor(df$income,
              levels = c('LMC', 'UMC', 'HIC', 'Global', 'None'),
              labels = c('LMC', 'UMC', 'HIC', 'Global', 'None'))

label_totals <- df %>%
  group_by(Application) %>%
  summarize(total_value = sum(total_apps))

apps_tots <-
  ggplot(df, aes(x = Application, y = total_apps)) +
  geom_bar(stat = "identity", aes(fill = income)) + coord_flip() + 
  geom_text(data = label_totals, aes(x = Application, y = total_value, 
                                     label = sprintf("%.0f", total_value)), size = 3,
            position = position_dodge(0.9), vjust = 0.05, hjust = -0.1) +
  #scale_fill_brewer(palette = "Paired") +
  labs(colour = NULL, title = "(b) Papers by Application Areas.",
       subtitle = "Results broken down by case-study country's income group.",
       x = NULL, y = bquote("Number of Reviewed Papers")) + 
  theme(
    legend.position = 'bottom',
    axis.text.x = element_text(size = 10),
    panel.spacing = unit(0.6, "lines"),
    plot.title = element_text(size = 11, face = "bold"),
    plot.subtitle = element_text(size = 9),
    axis.text.y = element_text(size = 10),
    axis.title.y = element_markdown(size = 7),
    legend.title = element_text(size = 8),
    legend.text = element_text(size = 8),
    axis.title.x = element_text(size = 7)
  ) + expand_limits(y = 0) +
  guides(fill = guide_legend(ncol = 6, title = 'Income Group')) +
  scale_x_discrete(expand = c(0, 0.15)) + scale_y_continuous(expand = c(0, 0),
  labels = function(y)format(y, scientific = FALSE), limit = c(0, 45))

##############################
## ARTICLES BY METHODOLOGY ###
##############################
df = data %>%
  group_by(Methodology, open_code) %>%
  summarize(total_methods = sum(number)) 

df$Methodology <- factor(df$Methodology,
   levels = c('Statistical', 'Optimization & AI', 'Experiment & Survey', 
              'Literature Review', 'Empirical'),
   labels = c('Statistical', 'Optimization \n& AI', 'Experiment \n& Survey', 
              'Literature \nReview', 'Empirical'))

df$open_code <- factor(df$open_code,
   levels = c('Yes', 'No', 'Not Applicable'),
   labels = c('Yes', 'No', 'Not Applicable'))

label_totals <- df %>%
  group_by(Methodology) %>%
  summarize(total_value = sum(total_methods))

method_tots <-
  ggplot(df, aes(x = Methodology, y = total_methods)) +
  geom_bar(stat = "identity", aes(fill = open_code)) + coord_flip() + 
  geom_text(data = label_totals, aes(x = Methodology, y = total_value, 
       label = sprintf("%.0f", total_value)), size = 3,
       position = position_dodge(0.9), vjust = 0.05, hjust = -0.1) +
  labs(colour = NULL, title = "(d) Methodology used in the Papers.",
       subtitle = "Results broken down by availability of open source code.",
       x = NULL, y = bquote("Number of Reviewed Papers")) + 
  theme(
    legend.position = 'bottom',
    axis.text.x = element_text(size = 10),
    panel.spacing = unit(0.6, "lines"),
    plot.title = element_text(size = 11, face = "bold"),
    plot.subtitle = element_text(size = 9),
    axis.text.y = element_text(size = 10),
    axis.title.y = element_markdown(size = 7),
    legend.title = element_text(size = 8),
    legend.text = element_text(size = 8),
    axis.title.x = element_text(size = 7)
  ) + expand_limits(y = 0) +
  guides(fill = guide_legend(ncol = 6, title = 'Open-Source Code Available?')) +
  scale_x_discrete(expand = c(0, 0.15)) + scale_y_continuous(expand = c(0, 0),
  labels = function(y)format(y, scientific = FALSE), limit = c(0, 49))


################################
## ARTICLES BY SPATIAL FOCUS ###
################################
df = data %>%
  group_by(spatial_focus, income) %>%
  summarize(total_spat = sum(number)) 

df$spatial_focus <- factor(df$spatial_focus,
   levels = c('Remote', 'Rural', 'Urban', 'Not Applicable', 'All'),
   labels = c('Remote', 'Rural', 'Urban', 'Not \nApplicable', 'All'))

df$income <- factor(df$income,
   levels = c('LMC', 'UMC', 'HIC', 'Global', 'None'),
   labels = c('LMC', 'UMC', 'HIC', 'Global', 'None'))

label_totals <- df %>%
  group_by(spatial_focus) %>%
  summarize(total_value = sum(total_spat))

spatial_tots <-
  ggplot(df, aes(x = spatial_focus, y = total_spat)) +
  geom_bar(stat = "identity", aes(fill = income)) + coord_flip() + 
  geom_text(data = label_totals, aes(x = spatial_focus, y = total_value, 
     label = sprintf("%.0f", total_value)), size = 3,
     position = position_dodge(0.9), vjust = 0.05, hjust = -0.1) +
  labs(colour = NULL, title = "(e) Spatial Focus of Papers.",
       subtitle = "Results broken by case-study country's income group.",
       x = NULL, y = bquote("Number of Reviewed Papers")) + 
  theme(
    legend.position = 'bottom',
    axis.text.x = element_text(size = 10),
    panel.spacing = unit(0.6, "lines"),
    plot.title = element_text(size = 11, face = "bold"),
    plot.subtitle = element_text(size = 9),
    axis.text.y = element_text(size = 10),
    axis.title.y = element_markdown(size = 7),
    legend.title = element_text(size = 8),
    legend.text = element_text(size = 8),
    axis.title.x = element_text(size = 7)
  ) + expand_limits(y = 0) +
  guides(fill = guide_legend(ncol = 6, title = 'Income Group')) +
  scale_x_discrete(expand = c(0, 0.15)) + scale_y_continuous(expand = c(0, 0),
  labels = function(y)format(y, scientific = FALSE), limit = c(0, 74))


################################
## ARTICLES BY SPATIAL FOCUS ###
################################
df = data %>%
  group_by(technology, spatial_focus) %>%
  summarize(total_tech = sum(number)) 

df$technology <- factor(df$technology,
   levels = c('Fixed Wireless', 'Satellite','Wired', 'Mobile', 'All'),
   labels = c('Fixed \nWireless', 'Satellite','Wired', 'Mobile', 'All'))

df$spatial_focus <- factor(df$spatial_focus,
   levels = c('Remote', 'Rural', 'Urban', 'Not Applicable', 'All'),
   labels = c('Remote', 'Rural', 'Urban', 'Not \nApplicable', 'All'))

label_totals <- df %>%
  group_by(technology) %>%
  summarize(total_value = sum(total_tech))

technology_tots <-
  ggplot(df, aes(x = technology, y = total_tech)) +
  geom_bar(stat = "identity", aes(fill = spatial_focus)) + coord_flip() + 
  geom_text(data = label_totals, aes(x = technology, y = total_value, 
     label = sprintf("%.0f", total_value)), size = 3,
     position = position_dodge(0.9), vjust = 0.05, hjust = -0.1) +
  labs(colour = NULL, title = "(f) Key Technology Focus of Papers.",
       subtitle = "Results broken by taraget users.",
       x = NULL, y = bquote("Number of Reviewed Papers")) + 
  theme(
    legend.position = 'bottom',
    axis.text.x = element_text(size = 10),
    panel.spacing = unit(0.6, "lines"),
    plot.title = element_text(size = 11, face = "bold"),
    plot.subtitle = element_text(size = 9),
    axis.text.y = element_text(size = 10),
    axis.title.y = element_markdown(size = 7),
    legend.title = element_text(size = 8),
    legend.text = element_text(size = 8),
    axis.title.x = element_text(size = 7)
  ) + expand_limits(y = 0) +
  guides(fill = guide_legend(ncol = 6, title = 'Target Users')) +
  scale_x_discrete(expand = c(0, 0.15)) + scale_y_continuous(expand = c(0, 0),
  labels = function(y)format(y, scientific = FALSE), limit = c(0, 45))

###################
## PANEL ARTICLE ##
###################
aggregate_paper <- ggarrange(publisher_tots, sdg_tots, 
                             apps_tots, method_tots,
                             spatial_tots, technology_tots,
  ncol = 2, nrow = 3, align = c('hv'),
  common.legend = FALSE, legend='bottom') 

path = file.path(folder, 'figures', 'aggregate_article.png')
png(path, units="in", width=10, height=13, res=300)
print(aggregate_paper)
dev.off()





