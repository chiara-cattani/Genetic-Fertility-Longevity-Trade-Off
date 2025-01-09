###------------------------###
###   Summary Statistics   ###
###------------------------###


# Prepare environment ----

rm(list = ls())
graphics.off()
setwd(file_path <- dirname(rstudioapi::getSourceEditorContext()$path))

output_path <- normalizePath("../output", winslash = "/")
if (!dir.exists(output_path)) {
  dir.create(output_path)
}


# Packages ----

library(ggplot2)
library(dplyr)
library(tidyr)
library(viridis)
library(GGally)
library(knitr)
library(kableExtra)
library(webshot)


# Load data ----

load("../data/df.RData")
load("../data/df_f.RData")
load("../data/df_m.RData")


# Summary statistics ----

# Labels
numeric_variable_labels <- list(
  birth_year = "Year of birth",
  age = "Age at death/last follow-up",
  pgs_l = "PRS Longevity",
  pgs_k = "PRS Fertility"
)

categorical_variable_labels <- list(
  delta = "Mortality",
  sex = "Sex",
  kidscat = "Number of children",
  educat = "Education level",
  marcat = "Marital status",
  pgs_sign = "PRS for longevity and fertility"
)

# Order
categorical_order <- c("delta", "sex", "kidscat", "educat", "marcat", "pgs_sign")
category_levels <- list(
  delta = c("Deceased", "Alive"),
  sex = c("Male", "Female"),
  kidscat = c("No children", "1 child", "2 children", "3 children", "More than 3 children"),
  educat = c("No degree", "High school diploma", "College degree", "Advanced degree"),
  marcat = c("Married", "Never married", "Separated/Divorced", "Widowed"),
  pgs_sign = c("Both high", "High longevity, low fertility", "Low longevity, high fertility", "Both low")
)


## Numeric variables  ----

compute_numeric_stats <- function(data, variable_name) {
  min_val <- min(data[[variable_name]], na.rm = TRUE)
  max_val <- max(data[[variable_name]], na.rm = TRUE)
  
  if (variable_name %in% c("birth_year", "age")) {
    min_val <- round(min_val, 0)
    max_val <- round(max_val, 0)
  } else {
    min_val <- round(min_val, 2)
    max_val <- round(max_val, 2)
  }
  
  mean_val <- round(mean(data[[variable_name]], na.rm = TRUE), 2)
  sd_val <- round(sd(data[[variable_name]], na.rm = TRUE), 2)
  
  data.frame(
    Min = min_val,
    Max = max_val,
    `Mean (SD)` = paste0(mean_val, " (", sd_val, ")")
  )
}

numeric_variables <- c("birth_year", "age", "pgs_l", "pgs_k")

numeric_summary <- do.call(rbind, lapply(numeric_variables, function(var) {
  stats <- compute_numeric_stats(df, var)
  stats <- cbind(Variable = numeric_variable_labels[[var]], stats)
  stats
}))

numeric_summary <- numeric_summary %>%
  mutate(
    Min = ifelse(Variable %in% c("Year of birth", "Age at death/last follow-up"),
                 sprintf("%.0f", Min), sprintf("%.2f", Min)),
    Max = ifelse(Variable %in% c("Year of birth", "Age at death/last follow-up"),
                 sprintf("%.0f", Max), sprintf("%.2f", Max))
  )

numeric_table <- numeric_summary %>%
  kbl(caption = "Characteristics of the study sample.",
      col.names = c("Variable", "Min", "Max", "Mean (SD)"),
      booktabs = TRUE, format = "html") %>%
  kable_styling(full_width = FALSE, position = "center")

numeric_file <- file.path(output_path, "numeric_variables.png")
save_kable(numeric_table, file = numeric_file, zoom = 2)


## Categorical variables ----

compute_categorical_stats <- function(data, variable_name) {

  if (variable_name == "delta") {
    data[[variable_name]] <- factor(data[[variable_name]], levels = c(1, 0), labels = c("Deceased", "Alive"))
  }
  
  if (variable_name == "pgs_sign") {
    data[[variable_name]] <- factor(data[[variable_name]], levels = c(
      "High PRS for longevity and fertility",
      "High PRS for longevity and low PRS for fertility",
      "Low PRS for longevity and high PRS for fertility",
      "Low PRS for longevity and fertility"
    ), labels = c("Both high", "High longevity, low fertility", "Low longevity, high fertility", "Both low"))
  }
  
  if (variable_name %in% names(category_levels)) {
    data[[variable_name]] <- factor(data[[variable_name]], levels = category_levels[[variable_name]])
  }
  
  table_data <- table(data[[variable_name]], useNA = "ifany")
  prop_table_data <- prop.table(table_data) * 100
  
  data.frame(
    Status = names(table_data),
    `N (frequency %)` = paste0(table_data, " (", round(prop_table_data, 2), "%)")
  )
}

categorical_summaries <- lapply(categorical_order, function(var) {
  stats <- compute_categorical_stats(df, var)
  stats <- cbind(Variable = "", stats)  
  list(
    Variable = categorical_variable_labels[[var]],  
    Data = stats
  )
})

kable_table <- NULL
for (cat_summary in categorical_summaries) {
  variable_header <- data.frame(
    Variable = cat_summary$Variable,
    Status = "",
    `N (frequency %)` = "",
    stringsAsFactors = FALSE
  )
  
  cat_data <- as.data.frame(cat_summary$Data, stringsAsFactors = FALSE)
  
  colnames(variable_header) <- colnames(cat_data) <- c("Variable", "Status", "N (frequency %)")
  
  kable_table <- rbind(kable_table, variable_header, cat_data)
}

categorical_table <- kable_table %>%
  kbl(caption = "Characteristics of the study sample.",
      col.names = c("Variable", "Status", "N (frequency %)"),
      booktabs = TRUE, format = "html") %>%
  kable_styling(full_width = FALSE, position = "center")

categorical_file <- file.path(output_path, "categorical_variables.png")
save_kable(categorical_table, file = categorical_file, zoom = 2)

