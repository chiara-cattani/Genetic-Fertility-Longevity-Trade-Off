# Description


## Folders structure:

raw/: Contains raw datasets (pgenscore4e_r.dta, randhrs1992_2020v1.dta,
 	trk2020tr_r.dta).

data/: Contains pre-processed datasets (df.RData, df_f.RData, df_m.RData).

doc/: Contains documentation regarding raw datasets.

output/: Contains all the plots and tables that are produced.

program/: Contains the script in R to be run in the following order:

- 00_data_manipulation.R (Optional): Script for generating pre-processed datasets (It
 	can be skipped as the following scripts are optimized for pre-processed
 	datasets; loading raw data takes approximately 15 minutes).

- 01_summary_statistics.R: Generates tables for numeric and categorical summary
 	statistics.

- 02_data_exploration.R: Produces scatterplots, density plots, and bar charts for
 	exploratory analysis.

- 03_kaplan_meier.R: Creates Kaplan-Meier survival curves and computes median survival
 	times.

- 04_cox_models.R: Fits Cox proportional hazards models and visualizes results.
