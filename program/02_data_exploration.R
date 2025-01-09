###----------------------###
###   Data Exploration   ###
###----------------------###


# Prepare environment ----

rm(list = ls())
graphics.off()
setwd(file_path <- dirname(rstudioapi::getSourceEditorContext()$path))

output_dir <- normalizePath("../output", winslash = "/")
if (!dir.exists(output_dir)) {
  dir.create(output_dir)
 } 
  
  
# Packages ----

library(ggplot2)
library(dplyr)
library(GGally)


# Load data ----

load("../data/df.RData")
load("../data/df_f.RData")
load("../data/df_m.RData")


# Exploratory Analysis ----

## Scatterplot matrix ----

subset_df <- df %>%
  select(birth_year, age, educat, marcat, kidscat, pgs_l, pgs_k)

scatter_matrix_plot <- ggpairs(subset_df, title = "Scatterplot Matrix") +
  theme(panel.background = element_rect(fill = "white", color = NA), 
        plot.background = element_rect(fill = "white", color = NA)) 

scatter_matrix_file <- file.path(output_dir, "scatterplot_matrix.png")
ggsave(scatter_matrix_file, plot = scatter_matrix_plot, width = 12, height = 10, dpi = 300)


## PRS longevity vs PRS fertility ----

### Correlation coefficients by sex ----

correlation_males <- cor(df$pgs_k[df$sex == "Male"], df$pgs_l[df$sex == "Male"])
p_value_males <- cor.test(df$pgs_k[df$sex == "Male"], df$pgs_l[df$sex == "Male"])$p.value

correlation_females <- cor(df$pgs_k[df$sex == "Female"], df$pgs_l[df$sex == "Female"])
p_value_females <- cor.test(df$pgs_k[df$sex == "Female"], df$pgs_l[df$sex == "Female"])$p.value

### Scatter plot ----

colors <- c("Male" = "blue", "Female" = "#FF7F0E")

prs_plot <- ggplot(df, aes(x = pgs_k, y = pgs_l, color = sex)) +
  geom_point(alpha = 0.3) +
  labs(x = "PRS for Number of Children", y = "PRS for Longevity",
       title = "PRS for Longevity vs. PRS for Number of Children",
       subtitle = "by Sex") +
  scale_color_manual(values = colors, name = "Sex") +
  theme_minimal() +
  theme(legend.position = "right",
        panel.background = element_rect(fill = "white", color = NA), 
        plot.background = element_rect(fill = "white", color = NA)) + 
  geom_smooth(method = "lm", se = FALSE, aes(group = sex, color = sex)) +
  annotate("text", x = 3.2, y = 4.7,
           label = paste("Correlation (Males):", 
                         round(correlation_males, 2), 
                         ifelse(p_value_males < 0.001, "(p < 0.001)", paste("(p =", round(p_value_males, 3), ")"))),
           hjust = 1, vjust = 0, size = 3, color = "black") +
  annotate("text", x = 3.2, y = 4.4,
           label = paste("Correlation (Females):", 
                         round(correlation_females, 2), 
                         ifelse(p_value_females < 0.001, "(p < 0.001)", paste("(p =", round(p_value_females, 3), ")"))),
           hjust = 1, vjust = 0, size = 3, color = "black")

prs_plot_file <- file.path(output_dir, "prs_plot.png")
ggsave(prs_plot_file, plot = prs_plot, width = 8, height = 6, dpi = 300)


## Age by sex and delta ----

df$delta <- factor(df$delta)

### Density plot ---- 

age_density_plot <- ggplot(df, aes(x = age, color = sex, fill = sex)) +
  geom_density(alpha = 0.1, linewidth = 1, position = "identity") +
  facet_wrap(~ delta, nrow = 1, labeller = labeller(delta = c("0" = "Alive", "1" = "Deceased"))) +
  labs(
    x = "Age",
    y = "Density",
    title = "Probability Distribution of Age of Deceased and Survivors",
    subtitle = "by Sex"
  ) +
  scale_color_manual(values = c("Male" = "blue", "Female" = "#FF7F0E")) +
  scale_fill_manual(values = c("Male" = "blue", "Female" = "#FF7F0E")) +
  theme_minimal() +
  theme(legend.position = "top", legend.title = element_blank(),
        panel.background = element_rect(fill = "white", color = NA), 
        plot.background = element_rect(fill = "white", color = NA)) 

age_density_file <- file.path(output_dir, "age_density_plot.png")
ggsave(age_density_file, plot = age_density_plot, width = 8, height = 6, dpi = 300)


### Bar plot ----

age_distribution_plot <- ggplot(df, aes(x = age, color = sex, fill = sex)) +
  geom_bar(alpha = 0.4, position = "identity", width = 0.9, show.legend = TRUE) +
  facet_wrap(~ delta, nrow = 1, labeller = labeller(delta = c("0" = "Alive", "1" = "Deceased"))) +
  labs(
    x = "Age",
    y = "Count",
    title = "Age Distribution of Deceased and Alive",
    subtitle = "by Sex"
  ) +
  scale_color_manual(values = c("Female" = "#FF7F0E", "Male" = "blue")) +
  scale_fill_manual(values = c("Female" = "#FF7F0E", "Male" = "blue")) +
  theme_minimal() +
  theme(legend.position = "top", legend.title = element_blank(),
        panel.background = element_rect(fill = "white", color = NA), 
        plot.background = element_rect(fill = "white", color = NA)) 

age_distribution_file <- file.path(output_dir, "age_distribution_plot.png")
ggsave(age_distribution_file, plot = age_distribution_plot, width = 8, height = 6, dpi = 300)


## Save median ages ----

median_age_alive_females <- median(df$age[df$sex == "Female" & df$delta == 0])
median_age_deceased_females <- median(df$age[df$sex == "Female" & df$delta == 1])
median_age_alive_males <- median(df$age[df$sex == "Male" & df$delta == 0])
median_age_deceased_males <- median(df$age[df$sex == "Male" & df$delta == 1])

median_age_data <- data.frame(
  Group = c("Alive Females", "Deceased Females", "Alive Males", "Deceased Males"),
  Median_Age = c(median_age_alive_females, median_age_deceased_females, median_age_alive_males, median_age_deceased_males)
)

median_age_data
