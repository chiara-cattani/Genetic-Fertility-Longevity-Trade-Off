###----------------###
###   Cox Models   ###
###----------------###


# Prepare environment ----

rm(list = ls())
graphics.off()
setwd(file_path <- dirname(rstudioapi::getSourceEditorContext()$path))


# Packages ----

library(survival)
library(survminer)


# Load data ----

load("../data/df.RData")
load("../data/df_f.RData")
load("../data/df_m.RData")


# Multivariate Cox Proportional Hazards models ----

## Set the reference categories ----

df_f$kidscat <- relevel(df_f$kidscat, ref = "No children")
df_m$kidscat <- relevel(df_m$kidscat, ref = "No children")

df_f$educat <- relevel(df_f$educat, ref = "High school diploma")
df_m$educat <- relevel(df_m$educat, ref = "High school diploma")

df_f$marcat <- relevel(df_f$marcat, ref = "Married")
df_m$marcat <- relevel(df_m$marcat, ref = "Married")


## Model 1) ----

# h(t)=h_0(t)∙exp⁡(β_1∙Children)

cox_1f <- coxph(Surv(age, delta) ~ kidscat, data = df_f)
summary(cox_1f)

cox_1m <- coxph(Surv(age, delta) ~ kidscat, data = df_m)
summary(cox_1m)

### Proportional hazards assumption ----

test.ph <- cox.zph(cox_1f)
test.ph
ggcoxzph(test.ph)

test.ph <- cox.zph(cox_1m)
test.ph
ggcoxzph(test.ph)

### Outliers ----

ggcoxdiagnostics(cox_1f, type = "dfbeta", linear.predictions = FALSE, ggtheme = theme_bw())
ggcoxdiagnostics(cox_1f, type = "deviance", linear.predictions = FALSE, ggtheme = theme_bw())

ggcoxdiagnostics(cox_1m, type = "dfbeta", linear.predictions = FALSE, ggtheme = theme_bw())
ggcoxdiagnostics(cox_1m, type = "deviance", linear.predictions = FALSE, ggtheme = theme_bw())


## Model 2 ----

# h(t)=h_0(t)∙exp⁡(β_1∙Children+β_2∙Birth Year+β_3∙Education+β_4∙Marital Status)

cox_2f <- coxph(Surv(age, delta) ~ kidscat + birth_year + educat + marcat, data = df_f)
summary(cox_2f)

cox_2m <- coxph(Surv(age, delta) ~ kidscat + birth_year + educat + marcat, data = df_m)
summary(cox_2m)

### Proportional hazards assumption ----

test.ph <- cox.zph(cox_2f)
test.ph
ggcoxzph(test.ph)

test.ph <- cox.zph(cox_2m)
test.ph
ggcoxzph(test.ph)

### Outliers ----

ggcoxdiagnostics(cox_2f, type = "dfbeta", linear.predictions = FALSE, ggtheme = theme_bw())
ggcoxdiagnostics(cox_2f, type = "deviance", linear.predictions = FALSE, ggtheme = theme_bw())

ggcoxdiagnostics(cox_2m, type = "dfbeta", linear.predictions = FALSE, ggtheme = theme_bw())
ggcoxdiagnostics(cox_2m, type = "deviance", linear.predictions = FALSE, ggtheme = theme_bw())


## Model 3 ----

# h(t)=h_0(t)∙exp⁡(β_1∙Children+β_2∙Birth Year+β_3∙Education+β_4∙Marital Status+β_5∙PRS Longevity+β_6∙PRS Fertility +∑(β_j∙PCA_j) 

cox_3f <- coxph(Surv(age, delta) ~ kidscat + birth_year + educat + marcat + pgs_l + pgs_k +
                  pc1 + pc2 + pc3 + pc4 + pc5 + pc6 + pc7 + pc8 + pc9 + pc10, data = df_f)
summary(cox_3f)

cox_3m <- coxph(Surv(age, delta) ~ kidscat + birth_year + educat + marcat + pgs_l + pgs_k +
                  pc1 + pc2 + pc3 + pc4 + pc5 + pc6 + pc7 + pc8 + pc9 + pc10, data = df_m)
summary(cox_3m)

### Proportional hazards assumption ----

test.ph <- cox.zph(cox_3f)
test.ph
ggcoxzph(test.ph)

test.ph <- cox.zph(cox_3m)
test.ph
ggcoxzph(test.ph)

### Outliers ----

ggcoxdiagnostics(cox_3f, type = "dfbeta", linear.predictions = FALSE, ggtheme = theme_bw())
ggcoxdiagnostics(cox_3f, type = "deviance", linear.predictions = FALSE, ggtheme = theme_bw())

ggcoxdiagnostics(cox_3m, type = "dfbeta", linear.predictions = FALSE, ggtheme = theme_bw())
ggcoxdiagnostics(cox_3m, type = "deviance", linear.predictions = FALSE, ggtheme = theme_bw())

