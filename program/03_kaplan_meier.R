###------------------###
###   Kaplan-Meier   ###
###------------------###


# Prepare environment ----

rm(list = ls())
graphics.off()
setwd(file_path <- dirname(rstudioapi::getSourceEditorContext()$path))

output_dir <- normalizePath("../output", winslash = "/")
if (!dir.exists(output_dir)) {
  dir.create(output_dir)
}


# Packages ----

library(survival)
library(ranger)
library(ggplot2)
library(dplyr)
library(ggfortify)
library(survminer)


# Load data ----

load("../data/df.RData")
load("../data/df_f.RData")
load("../data/df_m.RData")


# Kaplan Meier estimates ----

## Sex ----

km_sex <- survfit(Surv(age, delta) ~ sex, data = df)

### Survival curve ----

colors <- c("Male" = "blue", "Female" = "#FF7F0E")

km_sex_plot <- ggsurvplot(km_sex, data = df,
                          conf.int = FALSE,
                          pval = TRUE,
                          risk.table = TRUE,
                          risk.table.height = 0.3,
                          legend.labs = c("Female", "Male"),
                          legend.title = "Sex",
                          xlab = "Age",
                          break.time.by = 10,
                          palette = colors,
                          title = "Kaplan-Meier Survival Curve by Sex",
                          xlim = c(70, 105),
                          ggtheme = theme_light() +
                            theme(panel.background = element_rect(fill = "white", color = NA),
                                  plot.background = element_rect(fill = "white", color = NA),
                                  text = element_text(size = 14)),
                          surv.median.line = "hv")
km_sex_file <- file.path(output_dir, "kaplan_meier_sex.png")
ggsave(km_sex_file, plot = km_sex_plot$plot, width = 8, height = 6, dpi = 300)

km_sex_plot


### Median ages ----

median_ages <- summary(km_sex)$table
category_labels <- c("Female", "Male")
rownames(median_ages) <- category_labels
median_values <- median_ages[, "median"]
median_table <- data.frame(Age = median_values)
print(median_table)

### Log-rank test ----

survdiff(Surv(age, delta) ~ sex, data = df)


## Number of Children ----

km_kidscat_f <- survfit(Surv(age, delta) ~ factor(kidscat, levels = c("No children", "1 child", "2 children", "3 children", "More than 3 children")), data = df_f)
km_kidscat_m <- survfit(Surv(age, delta) ~ factor(kidscat, levels = c("No children", "1 child", "2 children", "3 children", "More than 3 children")), data = df_m)

### Survival curve ----

my_palette <- viridis::viridis(length(unique(df_f$kidscat)))
colors <- setNames(my_palette, unique(df_f$kidscat))

km_kidscat_f_plot <- ggsurvplot(km_kidscat_f, data = df_f,
                                conf.int = FALSE,
                                pval = TRUE,
                                risk.table = F,
                                legend.labs = unique(df_f$kidscat),
                                legend.title = "",
                                xlab = "Age",
                                break.time.by = 10,
                                palette = colors,
                                title = "Kaplan-Meier Survival Curve by Number of Children (Females)",
                                xlim = c(70, 100),
                                surv.median.line = "hv",
                                ggtheme = theme_light() +
                                  theme(panel.background = element_rect(fill = "white", color = NA),
                                        plot.background = element_rect(fill = "white", color = NA),
                                        text = element_text(size = 12)),
                                legend = "top")

km_kidscat_f_file <- file.path(output_dir, "kaplan_meier_kidscat_females.png")
ggsave(km_kidscat_f_file, plot = km_kidscat_f_plot$plot, width = 8, height = 6, dpi = 300)

km_kidscat_m_plot <- ggsurvplot(km_kidscat_m, data = df_m,
                                conf.int = FALSE,
                                pval = TRUE,
                                risk.table = F,
                                legend.labs = unique(df_m$kidscat),
                                legend.title = "",
                                xlab = "Age",
                                break.time.by = 10,
                                palette = colors,
                                title = "Kaplan-Meier Survival Curve by Number of Children (Males)",
                                xlim = c(70, 100),
                                surv.median.line = "hv",
                                ggtheme = theme_light() +
                                  theme(panel.background = element_rect(fill = "white", color = NA),
                                        plot.background = element_rect(fill = "white", color = NA),
                                        text = element_text(size = 12)),
                                legend = "top")

km_kidscat_m_file <- file.path(output_dir, "kaplan_meier_kidscat_males.png")
ggsave(km_kidscat_m_file, plot = km_kidscat_m_plot$plot, width = 8, height = 6, dpi = 300)

arrange_ggsurvplots(x = list(km_kidscat_f_plot, km_kidscat_m_plot), ncol = 2)

### Median ages ----

median_ages_f <- summary(km_kidscat_f)$table
median_ages_m <- summary(km_kidscat_m)$table

category_labels <- c("No children", "1 child", "2 children", "3 children", "More than 3 children")

rownames(median_ages_f) <- category_labels
rownames(median_ages_m) <- category_labels

median_values_f <- median_ages_f[, "median"]
median_values_m <- median_ages_m[, "median"]

median_table <- data.frame(Females = median_values_f)
median_table$Males <- median_values_m

print(median_table)

### Log-rank test ----

survdiff(Surv(age, delta) ~ kidscat, data = df_f)
survdiff(Surv(age, delta) ~ kidscat, data = df_m)


## Education level ----

km_educat_f <- survfit(Surv(age, delta) ~ factor(educat, levels = c("No degree", "High school diploma", "College degree", "Advanced degree")), data = df_f)
km_educat_m <- survfit(Surv(age, delta) ~ factor(educat, levels = c("No degree", "High school diploma", "College degree", "Advanced degree")), data = df_m)

### Survival curve ----

my_palette <- viridis::viridis(length(unique(df_f$educat)))
colors <- setNames(my_palette, unique(df_f$educat))

km_educat_f_plot <- ggsurvplot(km_educat_f, data = df_f,
                               conf.int = FALSE,
                               pval = TRUE,
                               risk.table = F,
                               legend.labs = unique(df_f$educat),
                               legend.title = "",
                               xlab = "Age",
                               break.time.by = 10,
                               palette = colors,
                               title = "Kaplan-Meier Survival Curve by Education Level (Females)",
                               xlim = c(70, 100),
                               surv.median.line = "hv",
                               ggtheme = theme_light() +
                                 theme(panel.background = element_rect(fill = "white", color = NA),
                                       plot.background = element_rect(fill = "white", color = NA),
                                       text = element_text(size = 12)),
                               legend = "top")

km_educat_f_file <- file.path(output_dir, "kaplan_meier_educat_females.png")
ggsave(km_educat_f_file, plot = km_educat_f_plot$plot, width = 8, height = 6, dpi = 300)

km_educat_m_plot <- ggsurvplot(km_educat_m, data = df_m,
                               conf.int = FALSE,
                               pval = TRUE,
                               risk.table = F,
                               legend.labs = unique(df_m$educat),
                               legend.title = "",
                               xlab = "Age",
                               break.time.by = 10,
                               palette = colors,
                               title = "Kaplan-Meier Survival Curve by Education Level (Males)",
                               xlim = c(70, 100),
                               surv.median.line = "hv",
                               ggtheme = theme_light() +
                                 theme(panel.background = element_rect(fill = "white", color = NA),
                                       plot.background = element_rect(fill = "white", color = NA),
                                       text = element_text(size = 12)),
                               legend = "top")

km_educat_m_file <- file.path(output_dir, "kaplan_meier_educat_males.png")
ggsave(km_educat_m_file, plot = km_educat_m_plot$plot, width = 8, height = 6, dpi = 300)

arrange_ggsurvplots(x = list(km_educat_f_plot, km_educat_m_plot), ncol = 2)

### Median ages ----

median_ages_f <- summary(km_educat_f)$table
median_ages_m <- summary(km_educat_m)$table

category_labels <- c("No degree", "High school diploma", "College degree", "Advanced degree")

rownames(median_ages_f) <- category_labels
rownames(median_ages_m) <- category_labels

median_values_f <- median_ages_f[, "median"]
median_values_m <- median_ages_m[, "median"]

median_table <- data.frame(Females = median_values_f)
median_table$Males <- median_values_m

print(median_table)

### Log-rank test ----

survdiff(Surv(age, delta) ~ educat, data = df_f)
survdiff(Surv(age, delta) ~ educat, data = df_m)


## Marital Status ----

km_marcat_f <- survfit(Surv(age, delta) ~ factor(marcat, levels = c("Married", "Never married", "Separated/Divorced", "Widowed")), data = df_f)
km_marcat_m <- survfit(Surv(age, delta) ~ factor(marcat, levels = c("Married", "Never married", "Separated/Divorced", "Widowed")), data = df_m)

### Survival curve ----

my_palette <- viridis::viridis(length(unique(df_f$marcat)))
colors <- setNames(my_palette, unique(df_f$marcat))

km_marcat_f_plot <- ggsurvplot(km_marcat_f, data = df_f,
                               conf.int = FALSE,
                               pval = TRUE,
                               risk.table = F,
                               legend.labs = unique(df_f$marcat),
                               legend.title = "",
                               xlab = "Age",
                               break.time.by = 10,
                               palette = colors,
                               title = "Kaplan-Meier Survival Curve by Marital Status (Females)",
                               xlim = c(70, 100),
                               surv.median.line = "hv",
                               ggtheme = theme_light() +
                                 theme(panel.background = element_rect(fill = "white", color = NA),
                                       plot.background = element_rect(fill = "white", color = NA),
                                       text = element_text(size = 12)),
                               legend = "top")

km_marcat_f_file <- file.path(output_dir, "kaplan_meier_marcat_females.png")
ggsave(km_marcat_f_file, plot = km_marcat_f_plot$plot, width = 8, height = 6, dpi = 300)

km_marcat_m_plot <- ggsurvplot(km_marcat_m, data = df_m,
                               conf.int = FALSE,
                               pval = TRUE,
                               risk.table = F,
                               legend.labs = unique(df_m$marcat),
                               legend.title = "",
                               xlab = "Age",
                               break.time.by = 10,
                               palette = colors,
                               title = "Kaplan-Meier Survival Curve by Marital Status (Males)",
                               xlim = c(70, 100),
                               surv.median.line = "hv",
                               ggtheme = theme_light() +
                                 theme(panel.background = element_rect(fill = "white", color = NA),
                                       plot.background = element_rect(fill = "white", color = NA),
                                       text = element_text(size = 12)),
                               legend = "top")

km_marcat_m_file <- file.path(output_dir, "kaplan_meier_marcat_males.png")
ggsave(km_marcat_m_file, plot = km_marcat_m_plot$plot, width = 8, height = 6, dpi = 300)

arrange_ggsurvplots(x = list(km_marcat_f_plot, km_marcat_m_plot), ncol = 2)

### Median ages ----

median_ages_f <- summary(km_marcat_f)$table
median_ages_m <- summary(km_marcat_m)$table

category_labels <- c("Married", "Never married", "Separated/Divorced", "Widowed")

rownames(median_ages_f) <- category_labels
rownames(median_ages_m) <- category_labels

median_values_f <- median_ages_f[, "median"]
median_values_m <- median_ages_m[, "median"]

median_table <- data.frame(Females = median_values_f)
median_table$Males <- median_values_m

print(median_table)

### Log-rank test ----

survdiff(Surv(age, delta) ~ marcat, data = df_f)
survdiff(Surv(age, delta) ~ marcat, data = df_m)


## PRS Longevity ----

# Compute highest and lowest deciles
df_f$pgs_lf_decile <- cut(df_f$pgs_l, breaks = quantile(df_f$pgs_l, probs = 0:10/10), include.lowest = TRUE, labels = FALSE)
df_f$pgs_lf_decile <- ifelse(df_f$pgs_lf_decile %in% c(1, 10), as.character(df_f$pgs_lf_decile), NA)

df_m$pgs_lm_decile <- cut(df_m$pgs_l, breaks = quantile(df_m$pgs_l, probs = 0:10/10), include.lowest = TRUE, labels = FALSE)
df_m$pgs_lm_decile <- ifelse(df_m$pgs_lm_decile %in% c(1, 10), as.character(df_m$pgs_lm_decile), NA)

df_combined <- rbind(
  df_f %>% select(age, delta, decile = pgs_lf_decile) %>% mutate(Sex = "Female"),
  df_m %>% select(age, delta, decile = pgs_lm_decile) %>% mutate(Sex = "Male")
)

km_decile <- survfit(Surv(age, delta) ~ decile + Sex, data = df_combined)

### Survival curve ----

km_prs_l_plot <- ggsurvplot(km_decile, data = df_combined,
                         conf.int = FALSE,
                         pval = TRUE,
                         risk.table = F,
                         risk.table.height = 0.3,
                         legend.labs = c("Female, Lowest decile", "Male, Lowest decile",
                                         "Female, Highest decile", "Male, Highest decile"), 
                         legend.title = "",
                         xlab = "Age",
                         break.time.by = 10, 
                         palette = c("#FFB6C1", "#ADD8E6", "#C71585", "#0000CD"), 
                         title = "Kaplan-Meier Survival Curve by Sex and PRS for Longevity", 
                         xlim = c(70, 100),
                         ggtheme = theme_light() +
                          theme(text = element_text(size = 14)),
                         surv.median.line = "hv",
                         legend = "top")
km_prs_l_file <- file.path(output_dir, "kaplan_meier_prs_l.png")
ggsave(km_prs_l_file, plot = km_prs_l_plot$plot, width = 8, height = 6, dpi = 300)

km_prs_l_plot

### Median ages ----

median_ages <- summary(km_decile)$table
category_labels <- c("Female, Lowest decile", "Male, Lowest decile", "Female, Highest decile", "Male, Highest decile")
rownames(median_ages) <- category_labels
median_values <- median_ages[, "median"]
median_table <- data.frame(PRS_longevity = median_values)
print(median_table)


## PRS Fertlity ----

# Compute highest and lowest deciles
df_f$pgs_kf_decile <- cut(df_f$pgs_kf, breaks = quantile(df_f$pgs_kf, probs = 0:10/10), include.lowest = TRUE, labels = FALSE)
df_f$pgs_kf_decile <- ifelse(df_f$pgs_kf_decile %in% c(1, 10), as.character(df_f$pgs_kf_decile), NA)

df_m$pgs_km_decile <- cut(df_m$pgs_km, breaks = quantile(df_m$pgs_km, probs = 0:10/10), include.lowest = TRUE, labels = FALSE)
df_m$pgs_km_decile <- ifelse(df_m$pgs_km_decile %in% c(1, 10), as.character(df_m$pgs_km_decile), NA)

df_combined <- rbind(
  df_f %>% select(age, delta, decile = pgs_kf_decile) %>% mutate(Sex = "Female"),
  df_m %>% select(age, delta, decile = pgs_km_decile) %>% mutate(Sex = "Male")
)

km_decile <- survfit(Surv(age, delta) ~ decile + Sex, data = df_combined)

### Survival curve ----

km_prs_f_plot <- ggsurvplot(km_decile, data = df_combined,
                         conf.int = FALSE,
                         pval = TRUE,
                         risk.table = F,
                         risk.table.height = 0.3,
                         legend.labs = c("Female, Lowest decile", "Male, Lowest decile",
                                         "Female, Highest decile", "Male, Highest decile"), 
                         legend.title = "",
                         xlab = "Age",
                         break.time.by = 10, 
                         palette = c("#FFB6C1", "#ADD8E6", "#C71585",  "#0000CD"), 
                         title = "Kaplan-Meier Survival Curve by Sex and PRS for Fertility", 
                         xlim = c(70, 100),
                         ggtheme = theme_light() +
                           theme(text = element_text(size = 14)),
                         surv.median.line = "hv",
                         legend = "top")

km_prs_f_file <- file.path(output_dir, "kaplan_meier_prs_f.png")
ggsave(km_prs_f_file, plot = km_prs_f_plot$plot, width = 8, height = 6, dpi = 300)

km_prs_f_plot

### Median ages ----

median_ages <- summary(km_decile)$table
category_labels <- c("Female, Lowest decile", "Male, Lowest decile", "Female, Highest decile", "Male, Highest decile")
rownames(median_ages) <- category_labels
median_values <- median_ages[, "median"]
median_table <- data.frame(PRS_fertility = median_values)
print(median_table)

## Survival curves PRS ----

arrange_ggsurvplots(x = list(km_prs_l_plot, km_prs_f_plot), ncol = 2)

