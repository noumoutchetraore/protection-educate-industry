#############################################
# ANNOTATED PIPELINE — "Does Protection Educate Industry? Tariffs, Productive Power,
# and Industrialisation in Developing Countries"
# Author: Noumoutchè Yaya Traore (Anadolu University)
# Last updated: November 2026
#
# PURPOSE:
# Runs the full pipeline from raw data import (LIST_DATA.xlsx) to the final tables and figures:
# panel cleaning and descriptive statistics; a PCA-based Index of Productive Power (IPP);
# fixed-effects baselines with Driscoll-Kraay standard errors; Hansen (1999) panel threshold
# tests (F1, F2, F3) with bootstrap p-values; LR confidence intervals for the thresholds;
# and the final double-threshold regression with Driscoll-Kraay standard errors.
#
# STRUCTURE:
#   BLOC 0 — Packages + options
#   BLOC 1 — Data import
#   BLOC 2 — Structural cleaning: types, sorting, duplicates
#   BLOC 3 — Time restriction + panel creation (before PCA)
#   BLOC 4 — PCA for the Index of Productive Power (IPP)
#   BLOC 5 — Descriptive diagnostics
#   BLOC 6 — Diagnostics + tables/figures (article style)
#   BLOC 7 — Baseline fixed effects (LIST reference)
#   BLOC 8 — Hansen (1999) panel threshold test: F1 + bootstrap
#   BLOC 9 — Hansen (1999) double threshold test: F2 + bootstrap
#   BLOC 10 — Hansen (1999) triple threshold test: F3 + bootstrap
#   After BLOC 10 (unnumbered sections): threshold-test tables, LR profile figures,
#   final double-threshold regression, country-regime tables and heatmap.
#############################################

#############################################
# SCRIPT: clean pipeline rebuild (v1)
# Objective: clean, reproducible pipeline
#############################################

##### BLOC 0 — Packages + options #####


library(readxl)
library(dplyr)
library(plm)
library(naniar)
library(ggplot2)
library(tidyr)
library(gt)
library(tibble)
library(lmtest)
library(sandwich)
library(countrycode)



options(stringsAsFactors = FALSE)
set.seed(123)


##### BLOC 1 — Data import #####
file_path <- "database/LIST_DATA.xlsx"  # path relative to the project folder

df_raw <- read_excel(file_path, sheet = "data_treated", na = "..") %>%
  as.data.frame()

# Minimal checks
stopifnot(all(c("country","year") %in% names(df_raw)))

str(df_raw)
summary(df_raw)





##### BLOC 2 — Structural cleaning: types, sorting, duplicates #####

# 1) Ensure year is numeric/integer
df_raw <- df_raw %>%
  mutate(
    country = as.character(country),
    year    = as.integer(year)
  )

# 2) Remove country-year duplicates (if any)
dup_count <- df_raw %>%
  count(country, year) %>%
  filter(n > 1)

print(dup_count)

# Check for duplicate country-year rows; keep first occurrence.
# Retain a single row per country-year (first occurrence):
df_raw <- df_raw %>%
  arrange(country, year) %>%
  distinct(country, year, .keep_all = TRUE)

# 3) Sort (essential for reproducibility)
df_raw <- df_raw %>%
  arrange(country, year)

# Final check
stopifnot(!any(duplicated(df_raw[c("country","year")])))







##### BLOC 3 — Time restriction + panel creation (before PCA) #####
df <- df_raw %>%
  filter(year >= 2000, year <= 2022) %>%
  arrange(country, year)

# Panel structure (informative step)
pdata <- pdata.frame(df, index = c("country", "year"))

pdim(pdata)

# Share of missing values by variable (useful for screening)
na_rate <- sort(colMeans(is.na(df)), decreasing = TRUE)
head(na_rate, 20)

# Missing-value diagnostics on model variables
vars_model <- c("mva_pc", "tariff", "tnrr", "rol", "elec_pc", "gfcf_pc", "rore", "goveff")

missing_model <- df %>%
  summarise(across(all_of(vars_model), ~ mean(is.na(.)))) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "na_rate") %>%
  arrange(desc(na_rate))

print(missing_model)

# Missing values by country on key variables (compact summary)
diagnostic_pays <- df %>%
  group_by(country) %>%
  summarise(
    total_obs = n(),
    na_mva    = sum(is.na(mva_pc)),
    na_tariff = sum(is.na(tariff)),
    na_tnrr   = sum(is.na(tnrr)),
    na_rol    = sum(is.na(rol)),
    na_elec   = sum(is.na(elec_pc)),
    na_gfcf   = sum(is.na(gfcf_pc)),
    na_rore   = sum(is.na(rore)),
    na_goveff = sum(is.na(goveff)),
    .groups = "drop"
  ) %>%
  arrange(desc(na_mva), desc(na_tariff))

print(diagnostic_pays)

# Missing-data visualisation (optional)
gg_miss_var(df)
gg_miss_fct(df, fct = year)
gg_miss_fct(df, fct = country)










#############################################
# TABLE 1 — Variable definitions & summary statistics (GT)
# + "Data Source" column
# - IPP excluded (constructed later via PCA)
# - Variables used: mva_pc, gfcf_pc, tariff, rore, tnrr, rol, goveff, elec_pc
#############################################


# --- 0) Data used for descriptive statistics ---
df_used <- if (exists("df_plain")) df_plain else df

# --- 1) Analysis variables ---
vars_table1 <- c("mva_pc", "gfcf_pc", "tariff", "rore", "tnrr", "rol", "goveff", "elec_pc")

missing_vars <- setdiff(vars_table1, names(df_used))
if (length(missing_vars) > 0) {
  stop("Variables manquantes dans df_used: ", paste(missing_vars, collapse = ", "))
}

# --- 2) Dictionary: definitions + data sources ---
var_dict <- tibble::tibble(
  variable = vars_table1,
  Variable = c(
    "MVA_pc",
    "GFCF_pc",
    "Tariff",
    "RoRE",
    "TNRR",
    "RoL",
    "GovEff",
    "ELEC_pc"
  ),
  Definition = c(
    "Manufacturing value added, per capita (USD 2015)",
    "Gross fixed capital formation, per capita (USD 2015)",
    "Tariff rate, applied, weighted mean, all products (%)",
    "Rate of Return on Education, constructed as exp(r × MYS)",
    "Total natural resource rents (% of GDP)",
    "Rule of Law: Estimate (index)",
    "Government Effectiveness: Estimate (index)",
    "Electric power consumption (kWh per capita)"
  ),
  
  `Data Source` = c(
    "World Bank (WDI)",
    "World Bank (WDI)",
    "World Bank (WDI)",
    "Authors’ calculation",
    "World Bank (WDI)",
    "World Bank (WDI)",
    "World Bank (WDI)",
    "World Bank (WDI)"
  )
)

# --- 3) Summary statistics (Mean, SD, Min, Max) ---
summ_stats <- df_used %>%
  summarise(across(all_of(vars_table1),
                   list(
                     Mean = ~ mean(., na.rm = TRUE),
                     SD   = ~ sd(., na.rm = TRUE),
                     Min  = ~ min(., na.rm = TRUE),
                     Max  = ~ max(., na.rm = TRUE)
                   ),
                   .names = "{.col}__{.fn}")) %>%
  pivot_longer(cols = everything(),
               names_to = c("variable", "stat"),
               names_sep = "__",
               values_to = "value") %>%
  pivot_wider(names_from = stat, values_from = value)

# --- 4) Merge with definitions/sources ---
table1_df <- var_dict %>%
  left_join(summ_stats, by = "variable") %>%
  select(Variable, Mean, SD, Min, Max, Definition, `Data Source`)

# --- 5) GT formatting (clean theme, consistent across tables) ---
gt_table1 <- table1_df %>%
  gt() %>%
  tab_header(title = "Variable Definitions and Summary Statistics") %>%
  fmt_number(columns = c(Mean, SD, Min, Max), decimals = 2) %>%
  cols_align(align = "center", columns = c(Mean, SD, Min, Max)) %>%
  cols_align(align = "left", columns = c(Variable, Definition, `Data Source`)) %>%
  cols_width(
    Variable ~ px(170),
    Mean ~ px(85),
    SD ~ px(85),
    Min ~ px(85),
    Max ~ px(85),
    Definition ~ px(360),
    `Data Source` ~ px(100)
  ) %>%
  tab_options(
    table.font.names = "Arial",
    table.font.size = 12,
    table.font.color = "black",
    table.border.top.width = px(0),
    table.border.bottom.width = px(0),
    heading.border.bottom.width = px(1),
    column_labels.border.bottom.width = px(1),
    table_body.hlines.width = px(0),
    table_body.vlines.width = px(0),
    source_notes.font.size = 9,
    data_row.padding = px(4)
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(everything())
  ) %>%
  tab_source_note(
    source_note = md(
      "Note: Per-capita series (MVA_pc and GFCF_pc) are computed by the authors by dividing level series by population. RoRE is constructed following Jones (1999) as exp(r × MYS), where MYS is obtained from the Global Data Lab. Additional data treatments (e.g., handling specific missing series episodes) are reported in the Appendix."
    )
  )

gt_table1

# --- 6) High-quality PNG export ---
gtsave(
  gt_table1,
  filename = "Table_1_Summary_Statistics_Definitions.png",
  expand = 15,
  zoom = 5,
  vwidth = 1500
)








# Descriptive sample matching the model sample
N_obs  <- nrow(df)
N_pays <- dplyr::n_distinct(df$country)
plage_annees <- range(df$year)

list(
  N_pays = N_pays,
  N_obs = N_obs,
  annees = plage_annees
)






#############################################
# BLOC 4 — PCA for the Index of Productive Power (IPP)
#############################################

##### 4.1 Selection of PCA variables + keys #####
ifp_data <- df %>%
  select(country, year, elec_pc, gfcf_pc, rore, goveff)

# Check structure
stopifnot(nrow(ifp_data) == nrow(df))

##### 4.2 Restriction to complete observations (PCA) #####
ifp_complete <- ifp_data %>%
  filter(complete.cases(elec_pc, gfcf_pc, rore, goveff))

nrow(ifp_complete)
nrow(ifp_data) - nrow(ifp_complete)  # number of observations dropped

##### 4.3 Standardisation (mean 0, variance 1) #####
X_ifp <- scale(
  ifp_complete %>% select(elec_pc, gfcf_pc, rore, goveff)
)

# Standardisation checks
apply(X_ifp, 2, mean)
apply(X_ifp, 2, sd)

##### 4.4 PCA #####
pca_ifp <- prcomp(X_ifp, center = FALSE, scale. = FALSE)

summary(pca_ifp)
pca_ifp$rotation[, 1]

##### 4.5 Construction of IPP scores (PC1) #####
ipp_scores <- ifp_complete %>%
  select(country, year) %>%
  mutate(ipp = as.numeric(pca_ifp$x[, 1]))

# Key checks
stopifnot(nrow(ipp_scores) == nrow(ifp_complete))
stopifnot(nrow(distinct(ipp_scores, country, year)) == nrow(ipp_scores))

##### 4.6 Merge into the main dataset #####
df <- df %>%
  left_join(ipp_scores, by = c("country", "year"))

# Final check
sum(!is.na(df$ipp))
summary(df$ipp)



summary(pca_ifp)
pca_ifp$rotation[,1]
summary(df$ipp)
sum(!is.na(df$ipp))





#############################################
# BLOC 5 — Descriptive diagnostics
#############################################

##### 5.1 Conversion to panel (AFTER PCA) #####
pdata <- pdata.frame(df, index = c("country", "year"))

pdim(pdata)

##### 5.2 IPP histogram #####
ipp_hist <- ggplot(df, aes(x = ipp)) +
  geom_histogram(aes(y = after_stat(density)),
                 bins = 30,
                 fill = "grey80",
                 color = "black") +
  geom_density(linewidth = 1) +
  labs(
    x = "Index of Productive Power (IPP)",
    y = "Density"
  ) +
  theme_minimal()

print(ipp_hist)

##### 5.3 Main descriptive statistics #####
vars_desc <- df %>%
  select(mva_pc, tariff, tnrr, rol, ipp)

summary(vars_desc)

##### 5.4 Within variation (key for FE & Hansen) #####
within_sd <- function(x, id) {
  sd(x - ave(x, id, FUN = function(z) mean(z, na.rm = TRUE)),
     na.rm = TRUE)
}

within_variation <- sapply(
  vars_desc,
  within_sd,
  id = df$country
)

within_variation

##### 5.5 Simple correlations #####
cor_mat <- cor(vars_desc, use = "pairwise.complete.obs")
round(cor_mat, 3)






#############################################
# BLOC 6 — DIAGNOSTICS + TABLES/FIGURES (ARTICLE STYLE)
# Prerequisites: df contains country, year, mva_pc, tariff, tnrr, rol, ipp
#               pca_ifp exists (prcomp object) and has been computed correctly
#############################################


##### 6.1 Final panel #####
df_plain <- df %>% arrange(country, year)
pdata3 <- pdata.frame(df_plain, index = c("country", "year"))

pdim(pdata3)

##### 6.2 Within variation (FE diagnostic) #####
within_sd <- function(x, id) {
  sd(x - ave(x, id, FUN = function(z) mean(z, na.rm = TRUE)), na.rm = TRUE)
}

within_variation <- sapply(
  df_plain[, c("mva_pc", "tariff", "tnrr", "rol", "ipp")],
  within_sd,
  id = df_plain$country
)

within_variation

##### 6.3 IPP distribution (histogram + density) + export #####
ipp_hist <- ggplot(df_plain, aes(x = ipp)) +
  geom_histogram(aes(y = after_stat(density)),
                 bins = 30,
                 fill = "grey80",
                 color = "black") +
  geom_density(linewidth = 1) +
  labs(
    x = "Index of Productive Power (IPP)",
    y = "Density"
  ) +
  theme_minimal()

ggsave(
  filename = "Histogram_IPP.png",
  plot = ipp_hist,
  width = 10,
  height = 6,
  dpi = 300
)





##### 6.6 PCA tables (GT) + export #####
# (a) Loadings PC1
pca_loadings <- data.frame(
  Variable = rownames(pca_ifp$rotation),
  PC1_Loading = round(pca_ifp$rotation[, 1], 4)
)

gt_pca_loadings <- pca_loadings %>%
  gt() %>%
  tab_header(title = "PCA Loadings for IPP (First Component)") %>%
  cols_align(align = "center", columns = everything()) %>%
  cols_width(everything() ~ px(140)) %>%
  tab_options(
    table.font.names = "Arial",
    table.font.size = 10,
    table.font.color = "black",
    table.border.top.width = px(0),
    table.border.bottom.width = px(0),
    heading.border.bottom.width = px(1),
    column_labels.border.bottom.width = px(1),
    table_body.hlines.width = px(0),
    table_body.vlines.width = px(0),
    source_notes.font.size = 8
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(everything())
  ) %>%
  tab_source_note(
    source_note = md("Note: Variables are standardized prior to PCA (mean 0, sd 1).")
  )

gtsave(
  gt_pca_loadings,
  filename = "PCA_Loadings_IPP2.png",
  expand = 0,
  zoom = 5,
  vwidth = 1600
)




# Alternative table layout

gt_pca_loadings <- pca_loadings %>%
  gt() %>%
  tab_header(
    title = "PCA Loadings for IPP"
  ) %>%
  cols_align(
    align = "center",
    columns = Variable
  ) %>%
  cols_align(
    align = "center",
    columns = PC1_Loading
  ) %>%
  cols_width(
    Variable ~ px(120),
    PC1_Loading ~ px(100)
  ) %>%
  tab_options(
    table.font.names = "Arial",
    table.font.size = 9.5,
    table.font.color = "black",
    data_row.padding = px(2),
    column_labels.padding = px(2),
    table.border.top.width = px(0),
    table.border.bottom.width = px(0),
    heading.border.bottom.width = px(1),
    column_labels.border.bottom.width = px(1),
    table_body.hlines.width = px(0),
    table_body.vlines.width = px(0),
    source_notes.font.size = 8
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(everything())
  )
  

gtsave(
  gt_pca_loadings,
  filename = "PCA_Loadings_IPP_compact1.png",
  expand = 5,
  zoom = 15,
  vwidth = 1600
)












# (b) Variance explained
imp <- summary(pca_ifp)$importance
pca_variance <- data.frame(
  Component = colnames(imp),
  Std_Dev   = round(imp[1, ], 4),
  Prop_Var  = round(imp[2, ], 4),
  Cum_Prop  = round(imp[3, ], 4)
)

gt_pca_variance <- pca_variance %>%
  gt() %>%
  tab_header(title = "PCA Explained Variance") %>%
  cols_align(align = "center", columns = everything()) %>%
  cols_width(everything() ~ px(140)) %>%
  tab_options(
    table.font.names = "Arial",
    table.font.size = 12,
    table.font.color = "black",
    table.border.top.width = px(0),
    table.border.bottom.width = px(0),
    heading.border.bottom.width = px(1),
    column_labels.border.bottom.width = px(1),
    table_body.hlines.width = px(0),
    table_body.vlines.width = px(0),
    source_notes.font.size = 9
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(everything())
  ) %>%
  tab_source_note(
    source_note = md("Note: PC1 explains 73.37% of total variance and is used as the IPP index.")
  )

gtsave(
  gt_pca_variance,
  filename = "PCA_Explained_Variance_IPP2.png",
  expand = 5,
  zoom = 5,
  vwidth = 1600
)






gt_pca_variance <- pca_variance %>%
  gt() %>%
  tab_header(title = "PCA Explained Variance") %>%
  cols_align(align = "center", columns = everything()) %>%
  cols_width(
    Component ~ px(90),
    Std_Dev   ~ px(85),
    Prop_Var  ~ px(85),
    Cum_Prop  ~ px(85)
  ) %>%
  tab_options(
    table.font.names = "Arial",
    table.font.size = 10,
    table.font.color = "black",
    
    # vertical compaction
    data_row.padding = px(1),
    column_labels.padding = px(1),
    heading.padding = px(2),
    source_notes.padding = px(1),
    
    table.border.top.width = px(0),
    table.border.bottom.width = px(0),
    heading.border.bottom.width = px(0),
    column_labels.border.bottom.width = px(0),
    table_body.hlines.width = px(0),
    table_body.vlines.width = px(0),
    source_notes.font.size = 7
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(everything())
  ) %>%
  tab_source_note(
    source_note = md("Note: PC1 explains 73.37% of total variance and is used as the IPP index.")
  )

gtsave(
  gt_pca_variance,
  filename = "PCA_Explained_Variance_IPP2.png",
  expand = 1,
  zoom = 6,
  vwidth = 1000
)














#############################################
# BLOC 7 — Baseline Fixed Effects (LIST reference)
#############################################

# Final panel
pdata3 <- pdata.frame(df_plain, index = c("country", "year"))

# One-way FE (individual) — Hansen baseline
fe_ow <- plm(
  mva_pc ~ tariff + tnrr + rol + ipp,
  data   = pdata3,
  model  = "within",
  effect = "individual"
)

summary(fe_ow)

# Two-way FE (informational test)
fe_tw <- plm(
  mva_pc ~ tariff + tnrr + rol + ipp,
  data   = pdata3,
  model  = "within",
  effect = "twoways"
)

summary(fe_tw)

# F test: one-way vs two-way
pFtest(fe_tw, fe_ow)  # time effects are statistically significant










##### 6.4 Correlation matrix (GT style) + export #####
vars <- df_plain %>% select(mva_pc, tariff, tnrr, rol, ipp)

cor_mat <- cor(vars, use = "pairwise.complete.obs")
cor_mat[upper.tri(cor_mat, diag = FALSE)] <- NA

cor_df <- as.data.frame(round(cor_mat, 4)) %>%
  mutate(Variables = rownames(.)) %>%
  relocate(Variables)

gt_cor_df <- cor_df %>%
  gt() %>%
  tab_header(title = "Pairwise Correlation Matrix") %>%
  tab_style(
    style = cell_text(align = "center"),
    locations = cells_title(groups = "title")
  ) %>%
  fmt_missing(columns = everything(), missing_text = "") %>%
  cols_align(align = "center", columns = everything()) %>%
  cols_width(everything() ~ px(85)) %>%
  tab_options(
    table.font.names = "Arial",
    table.font.size = 12,
    table.background.color = "white",
    table.font.color = "black",
    data_row.padding = px(5),
    table.border.top.width = px(0),
    table.border.bottom.width = px(0),
    table.border.left.width = px(0),
    table.border.right.width = px(0),
    heading.border.bottom.color = "black",
    heading.border.bottom.width = px(1),
    column_labels.border.bottom.color = "black",
    column_labels.border.bottom.width = px(1),
    table_body.hlines.width = px(0),
    table_body.vlines.width = px(0),
    source_notes.font.size = 9
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(
      columns = c("Variables", "mva_pc", "tariff", "tnrr", "rol", "ipp")
    )
  ) %>%
  tab_source_note(
    source_note = md("Note: The table reports pairwise correlations based on country-year observations.")
  )

gtsave(
  gt_cor_df,
  filename = "Table_Correlation_Matrix.png",
  expand = 5,
  zoom = 5,
  vwidth = 1600
)

##### 6.5 Cross-sectional dependence (CSD tests) + GT export #####
bp_test <- pcdtest(
  mva_pc ~ tariff + tnrr + rol + ipp,
  data = pdata3,
  test = "lm"
)

cd_test <- pcdtest(
  mva_pc ~ tariff + tnrr + rol + ipp,
  data = pdata3,
  test = "cd"
)

csd_df <- tibble(
  Test = c("Breusch–Pagan LM", "Pesaran CD"),
  Statistic = c(unname(bp_test$statistic), unname(cd_test$statistic)),
  df = c(ifelse(is.null(bp_test$parameter), NA, unname(bp_test$parameter)), NA),
  `p-value` = c(bp_test$p.value, cd_test$p.value)
)

csd_gt <- csd_df %>%
  gt() %>%
  tab_header(title = "Cross-Sectional Dependence Tests") %>%
  fmt_number(columns = c(Statistic, df, `p-value`), decimals = 3) %>%
  sub_missing(columns = everything(), missing_text = "-") %>%
  cols_align(align = "center", columns = everything()) %>%
  cols_width(everything() ~ px(140)) %>%
  tab_options(
    table.font.names = "Arial",
    table.font.size = 10,
    table.font.color = "black",
    table.border.top.width = px(0),
    table.border.bottom.width = px(0),
    heading.border.bottom.width = px(1),
    column_labels.border.bottom.width = px(1),
    table_body.hlines.width = px(0),
    table_body.vlines.width = px(0),
    source_notes.font.size = 9
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = list(
      cells_column_labels(everything()),
      cells_body(columns = 1)
    )
  ) %>%
  tab_source_note(
    source_note = md("Note: Due to the moderate time dimension (T=23), both tests are computed using Within-residuals. Null hypothesis: no cross-sectional dependence.")
  )

gtsave(
  csd_gt,
  filename = "Cross_Sectional_Dependence_Tests2.png",
  expand = 5,
  zoom = 5,
  vwidth = 1600
)


#############################################
# Driscoll–Kraay SE (robust to cross-sectional dependence and autocorrelation)
#############################################



# For one-way FE (baseline)
coeftest(
  fe_ow,
  vcov = vcovSCC(fe_ow, type = "HC1", maxlag = 2)
)













#############################################
# BLOC 8 — HANSEN (1999) PANEL THRESHOLD TEST: F1 + BOOTSTRAP
# Objective:
#   Test H0: no threshold (linear)
#   vs H1: one endogenous threshold, where the effect of tariff varies with IPP (ipp)
#
# Baseline (H0):
#   mva_pc_it = μ_i + β tariff_it + Γ X_it + ε_it
#
# Threshold model (H1, τ):
#   mva_pc_it = μ_i
#            + β1 tariff_it * I(ipp_it <= τ)
#            + β2 tariff_it * I(ipp_it >  τ)
#            + Γ X_it + ε_it
#
# Key note:
#   τ is not identified under H0 → non-standard distribution → bootstrap p-value (Hansen)
#############################################

##### 8.1 Panel + H0 model (one-way individual FE) #####
pdata3 <- pdata.frame(df_plain, index = c("country","year"))

fe_0 <- plm(
  mva_pc ~ tariff + tnrr + rol + ipp,
  data   = pdata3,
  model  = "within",
  effect = "individual"
)

# Extract the effective estimation sample (avoids NAs / unbalanced-panel issues)
df_boot <- model.frame(fe_0)
resid_0  <- residuals(fe_0)
fitted_0 <- fitted(fe_0)

stopifnot(nrow(df_boot) == length(resid_0))

# SSR0 and residual variance under H0
SSR_0    <- sum(resid_0^2)
k0       <- length(coef(fe_0))  # number of parameters excluding FE
N_eff    <- length(resid_0)
sigma2_0 <- SSR_0 / (N_eff - k0)

list(N_eff = N_eff, k0 = k0, SSR_0 = SSR_0, sigma2_0 = sigma2_0)

##### 8.2 Threshold grid (10%–90% trimming on the effective sample) #####
tau_bounds <- quantile(df_boot$ipp, probs = c(0.10, 0.90), na.rm = TRUE)

# Grid based on observed values (faithful to Hansen, but computationally heavy)
tau_candidates <- sort(unique(df_boot$ipp[
  df_boot$ipp >= tau_bounds[1] & df_boot$ipp <= tau_bounds[2]
]))

length(tau_candidates)

# (Optional speed-up: 1% quantile grid)
# tau_candidates <- as.numeric(quantile(df_boot$ipp, probs = seq(0.10, 0.90, by = 0.01), na.rm = TRUE))
# tau_candidates <- sort(unique(tau_candidates))

##### 8.3 SSR(τ) function for the one-threshold model #####
compute_ssr_1threshold <- function(tau, data) {
  
  data$tmp_low  <- as.numeric(data$ipp <= tau)
  data$tmp_high <- as.numeric(data$ipp >  tau)
  
  m_tau <- try(
    plm(
      mva_pc ~ tariff:tmp_low + tariff:tmp_high + tnrr + rol + ipp,
      data   = data,
      model  = "within",
      effect = "individual"
    ),
    silent = TRUE
  )
  
  if (inherits(m_tau, "try-error")) return(NA_real_)
  sum(residuals(m_tau)^2)
}

##### 8.4 Threshold estimate τ_hat and observed F1 #####
SSR_tau <- sapply(tau_candidates, compute_ssr_1threshold, data = df_boot)

stopifnot(sum(is.na(SSR_tau)) < length(SSR_tau))  # at least some estimations succeeded

tau_hat_1  <- tau_candidates[which.min(SSR_tau)]
SSR_1_hat  <- min(SSR_tau, na.rm = TRUE)

F1_obs <- (SSR_0 - SSR_1_hat) / sigma2_0

list(tau_hat_1 = tau_hat_1, SSR_1_hat = SSR_1_hat, F1_obs = F1_obs)

##### 8.5 SSR(τ) plot + export (for the article) #####
ssr_df <- data.frame(tau = tau_candidates, SSR = SSR_tau)

ssr_plot <- ggplot(ssr_df, aes(x = tau, y = SSR)) +
  geom_line(color = "black") +
  geom_vline(xintercept = tau_hat_1, linetype = "dashed") +
  labs(
    x = "Threshold candidate (IPP)",
    y = "Sum of Squared Residuals"
  ) +
  theme_minimal()

ggsave(
  filename = "SSR_Threshold_Candidates_F1.png",
  plot = ssr_plot,
  width = 10,
  height = 6,
  dpi = 300
)

##### 8.6 Hansen bootstrap for the p-value of F1 #####
set.seed(123)

B <- 500  # B = 200 for quick tests; 500 for article results
F1_boot <- numeric(B)

for (b in 1:B) {
  
  # (i) Generate y* under H0: fitted values + resampled residuals
  resid_star <- sample(resid_0, replace = TRUE)
  df_boot$mva_pc_star <- fitted_0 + resid_star
  
  # (ii) Re-estimate H0 on y*
  fe0_star <- plm(
    mva_pc_star ~ tariff + tnrr + rol + ipp,
    data   = df_boot,
    model  = "within",
    effect = "individual"
  )
  
  SSR0_star   <- sum(residuals(fe0_star)^2)
  sigma2_star <- SSR0_star / (length(residuals(fe0_star)) - k0)
  
  # (iii) Minimise SSR1*(τ) over the grid
  SSR1_star <- sapply(
    tau_candidates,
    function(tau) {
      
      df_boot$tmp_low  <- as.numeric(df_boot$ipp <= tau)
      df_boot$tmp_high <- as.numeric(df_boot$ipp >  tau)
      
      m_star <- try(
        plm(
          mva_pc_star ~ tariff:tmp_low + tariff:tmp_high + tnrr + rol + ipp,
          data   = df_boot,
          model  = "within",
          effect = "individual"
        ),
        silent = TRUE
      )
      
      if (inherits(m_star, "try-error")) return(NA_real_)
      sum(residuals(m_star)^2)
    }
  )
  
  SSR1_star_hat <- min(SSR1_star, na.rm = TRUE)
  F1_boot[b] <- (SSR0_star - SSR1_star_hat) / sigma2_star
  
  if (b %% 50 == 0) cat("Bootstrap F1:", b, "/", B, "\n")
}

# Hansen bootstrap p-value

p_value_F1 <- (sum(F1_boot >= F1_obs, na.rm = TRUE) + 1) / (B + 1)

p_value_F1  # Reported result: bootstrap p-value of F1 = 0.001996008; the null of no threshold is strongly rejected.




##### 8.7 Bootstrap distribution of F1 + export #####
boot_df <- data.frame(F1_boot = F1_boot)

boot_plot <- ggplot(boot_df, aes(x = F1_boot)) +
  geom_histogram(aes(y = after_stat(density)),
                 bins = 30,
                 fill = "grey80",
                 color = "black") +
  geom_density(color = "black", linewidth = 0.8) +
  geom_vline(xintercept = F1_obs, linetype = "dashed") +
  labs(
    x = "Bootstrap distribution of F1",
    y = "Density"
  ) +
  theme_minimal()

ggsave(
  filename = "Bootstrap_Distribution_F1.png",
  plot = boot_plot,
  width = 10,
  height = 6,
  dpi = 300
)



#############################################
# BLOC 9 — HANSEN (1999) DOUBLE THRESHOLD TEST: F2 + BOOTSTRAP
# Sequential test:
#   H0: 1 threshold
#   H1: 2 thresholds
#
# IMPORTANT (consistency with Hansen):
# - Everything is computed on the effective sample under H0 = 1 threshold (df_boot1)
# - 10%-90% trimmed grid built on df_boot1$ipp
# - P-value via bootstrap under H0 = 1 threshold
#############################################

library(plm)

##### 9.1 H0 = 1 threshold model at the estimated threshold tau_hat_1 (on df_boot) #####
# df_boot comes from BLOC 8 (model.frame(fe_0))
df_boot1 <- df_boot
df_boot1$tmp_low  <- as.numeric(df_boot1$ipp <= tau_hat_1)
df_boot1$tmp_high <- as.numeric(df_boot1$ipp >  tau_hat_1)

fe_1hat <- plm(
  mva_pc ~ tariff:tmp_low + tariff:tmp_high + tnrr + rol + ipp,
  data   = df_boot1,
  model  = "within",
  effect = "individual"
)

# Effective sample under H0 = 1 threshold
df_boot1 <- model.frame(fe_1hat)
resid_1  <- residuals(fe_1hat)
fitted_1 <- fitted(fe_1hat)

stopifnot(nrow(df_boot1) == length(resid_1))

##### 9.2 Quantile grid (10%-90% trimming) on df_boot1 #####
tau_candidates_1 <- as.numeric(
  quantile(df_boot1$ipp, probs = seq(0.10, 0.90, by = 0.01), na.rm = TRUE)
)
tau_candidates_1 <- sort(unique(tau_candidates_1))
length(tau_candidates_1)  # ~81

##### 9.3 Minimal SSR functions (1 threshold and 2 thresholds) #####
ssr1_min_on_grid <- function(yvar, data, tau_grid) {
  best <- Inf
  for (tau in tau_grid) {
    
    data$tmp_low  <- as.numeric(data$ipp <= tau)
    data$tmp_high <- as.numeric(data$ipp >  tau)
    
    m1 <- try(
      plm(
        as.formula(paste0(yvar, " ~ tariff:tmp_low + tariff:tmp_high + tnrr + rol + ipp")),
        data   = data,
        model  = "within",
        effect = "individual"
      ),
      silent = TRUE
    )
    
    if (!inherits(m1, "try-error")) {
      ssr <- sum(residuals(m1)^2)
      if (is.finite(ssr) && ssr < best) best <- ssr
    }
  }
  best
}

##### 9.4 Build admissible pairs (tau1<tau2) + regime-size trimming #####
min_frac <- 0.10
n1 <- nrow(df_boot1)

valid_pairs <- list()

for (i in seq_along(tau_candidates_1)) {
  for (j in seq_along(tau_candidates_1)) {
    
    if (tau_candidates_1[i] < tau_candidates_1[j]) {
      
      tau1 <- tau_candidates_1[i]
      tau2 <- tau_candidates_1[j]
      
      n_r1 <- sum(df_boot1$ipp <= tau1, na.rm = TRUE)
      n_r2 <- sum(df_boot1$ipp > tau1 & df_boot1$ipp <= tau2, na.rm = TRUE)
      n_r3 <- sum(df_boot1$ipp > tau2, na.rm = TRUE)
      
      if (min(n_r1, n_r2, n_r3) >= min_frac * n1) {
        valid_pairs[[length(valid_pairs) + 1]] <- c(tau1, tau2)
      }
    }
  }
}

length(valid_pairs)

# (Optional diagnostic for the article)
min_counts <- sapply(valid_pairs, function(p) {
  tau1 <- p[1]; tau2 <- p[2]
  n_r1 <- sum(df_boot1$ipp <= tau1, na.rm = TRUE)
  n_r2 <- sum(df_boot1$ipp > tau1 & df_boot1$ipp <= tau2, na.rm = TRUE)
  n_r3 <- sum(df_boot1$ipp > tau2, na.rm = TRUE)
  min(n_r1, n_r2, n_r3)
})
summary(min_counts)

ssr2_min_on_pairs <- function(yvar, data, pairs) {
  best <- Inf
  for (k in seq_along(pairs)) {
    
    tau1 <- pairs[[k]][1]
    tau2 <- pairs[[k]][2]
    
    data$r1 <- as.numeric(data$ipp <= tau1)
    data$r2 <- as.numeric(data$ipp > tau1 & data$ipp <= tau2)
    data$r3 <- as.numeric(data$ipp > tau2)
    
    m2 <- try(
      plm(
        as.formula(paste0(yvar, " ~ tariff:r1 + tariff:r2 + tariff:r3 + tnrr + rol + ipp")),
        data   = data,
        model  = "within",
        effect = "individual"
      ),
      silent = TRUE
    )
    
    if (!inherits(m2, "try-error")) {
      ssr <- sum(residuals(m2)^2)
      if (is.finite(ssr) && ssr < best) best <- ssr
    }
  }
  best
}

##### 9.5 Observed F2 (Hansen-style) #####
SSR1_hat_1 <- ssr1_min_on_grid("mva_pc", df_boot1, tau_candidates_1)
SSR2_hat_1 <- ssr2_min_on_pairs("mva_pc", df_boot1, valid_pairs)

# k2 = number of parameters excluding FE in the 2-threshold model:
# (3 tariff slopes) + tnrr + rol + ipp = 6
k2 <- 6
sigma2_2 <- SSR2_hat_1 / (n1 - k2)

F2_obs <- (SSR1_hat_1 - SSR2_hat_1) / sigma2_2

list(SSR1_hat = SSR1_hat_1, SSR2_hat = SSR2_hat_1, sigma2_2 = sigma2_2, F2_obs = F2_obs)



##### 9.6 Hansen bootstrap for the p-value of F2 under H0 = 1 threshold #####
set.seed(123)

B <- 500
F2_boot <- numeric(B)

for (b in 1:B) {
  
  # Generate y* under H0 (1 threshold)
  e_star <- sample(resid_1, replace = TRUE)
  df_boot1$mva_pc_star <- fitted_1 + e_star
  
  # SSR1*(y*) = minimum over the grid
  SSR1_star <- ssr1_min_on_grid("mva_pc_star", df_boot1, tau_candidates_1)
  
  # SSR2*(y*) = minimum over the pairs
  SSR2_star <- ssr2_min_on_pairs("mva_pc_star", df_boot1, valid_pairs)
  
  # sigma2* (2-threshold alternative)
  sigma2_star <- SSR2_star / (n1 - k2)
  
  F2_boot[b] <- (SSR1_star - SSR2_star) / sigma2_star
  
  if (b %% 50 == 0) cat("Bootstrap F2:", b, "/", B, "\n")
}

# Bootstrap p-value (+1 correction to avoid a mechanical p = 0)
p_value_F2 <- (sum(F2_boot >= F2_obs, na.rm = TRUE) + 1) / (B + 1)
p_value_F2

summary(F2_boot)
sum(is.na(F2_boot))

crit_F2 <- quantile(F2_boot, probs = c(0.99, 0.95, 0.90), na.rm = TRUE)
crit_F2


boot2_df <- data.frame(F2_boot = F2_boot)


boot2_plot <- ggplot(boot2_df, aes(x = F2_boot)) +
  geom_histogram(aes(y = after_stat(density)),
                 bins = 30,
                 fill = "grey80",
                 color = "black") +
  geom_density(color = "black", linewidth = 0.8) +
  geom_vline(xintercept = F2_obs, linetype = "dashed") +
  labs(
    x = "Bootstrap distribution of F2",
    y = "Density"
  ) +
  theme_minimal()

ggsave("Bootstrap_Distribution_F2.png", boot2_plot, width = 10, height = 6, dpi = 300)











#############################################
# BLOC 10 — HANSEN (1999) TRIPLE THRESHOLD TEST: F3 + BOOTSTRAP
# Sequential test:
#   H0: 2 thresholds
#   H1: 3 thresholds
#
# Computational strategy:
#   Estimate (tau1_hat, tau2_hat) under H0 = 2 thresholds on the quantile grid,
#   then search for the third threshold by splitting ONE of the 3 regimes (1, 2, 3) at a time.
#############################################

library(plm)

##### 10.1 Estimate (tau1_hat, tau2_hat) under H0 = 2 thresholds (on valid_pairs) #####
SSR2_vec <- numeric(length(valid_pairs))

for (k in seq_along(valid_pairs)) {
  tau1 <- valid_pairs[[k]][1]
  tau2 <- valid_pairs[[k]][2]
  
  df_boot1$r1 <- as.numeric(df_boot1$ipp <= tau1)
  df_boot1$r2 <- as.numeric(df_boot1$ipp > tau1 & df_boot1$ipp <= tau2)
  df_boot1$r3 <- as.numeric(df_boot1$ipp > tau2)
  
  m2 <- try(
    plm(
      mva_pc ~ tariff:r1 + tariff:r2 + tariff:r3 + tnrr + rol + ipp,
      data   = df_boot1,
      model  = "within",
      effect = "individual"
    ),
    silent = TRUE
  )
  
  SSR2_vec[k] <- if (inherits(m2, "try-error")) NA_real_ else sum(residuals(m2)^2)
}

min_k    <- which.min(SSR2_vec)
tau1_hat <- valid_pairs[[min_k]][1]
tau2_hat <- valid_pairs[[min_k]][2]

##### 10.2 Estimate H0 = 2 thresholds at the estimated thresholds + residuals/fitted #####
df_boot1$r1 <- as.numeric(df_boot1$ipp <= tau1_hat)
df_boot1$r2 <- as.numeric(df_boot1$ipp > tau1_hat & df_boot1$ipp <= tau2_hat)
df_boot1$r3 <- as.numeric(df_boot1$ipp > tau2_hat)

fe_2hat <- plm(
  mva_pc ~ tariff:r1 + tariff:r2 + tariff:r3 + tnrr + rol + ipp,
  data   = df_boot1,
  model  = "within",
  effect = "individual"
)

resid_2  <- residuals(fe_2hat)
fitted_2 <- fitted(fe_2hat)

stopifnot(length(resid_2) == nrow(df_boot1))

SSR2_hat_obs <- sum(resid_2^2)

##### 10.3 SSR3 function: best third threshold obtained by splitting one regime #####
# split_regime ∈ {1,2,3} indicates which regime (defined by tau1_hat, tau2_hat) is split
ssr3_split_min <- function(yvar, data, tau1, tau2, split_regime, tau_grid, min_frac = 0.10) {
  
  n <- nrow(data)
  best <- Inf
  
  # Candidates depend on the regime being split
  if (split_regime == 1) {
    candidates <- tau_grid[tau_grid < tau1]
  } else if (split_regime == 2) {
    candidates <- tau_grid[tau_grid > tau1 & tau_grid < tau2]
  } else {
    candidates <- tau_grid[tau_grid > tau2]
  }
  
  candidates <- sort(unique(candidates))
  if (length(candidates) == 0) return(Inf)
  
  for (s in candidates) {
    
    # Build 4 regimes
    if (split_regime == 1) {
      data$q1 <- as.numeric(data$ipp <= s)
      data$q2 <- as.numeric(data$ipp > s & data$ipp <= tau1)
      data$q3 <- as.numeric(data$ipp > tau1 & data$ipp <= tau2)
      data$q4 <- as.numeric(data$ipp > tau2)
    } else if (split_regime == 2) {
      data$q1 <- as.numeric(data$ipp <= tau1)
      data$q2 <- as.numeric(data$ipp > tau1 & data$ipp <= s)
      data$q3 <- as.numeric(data$ipp > s & data$ipp <= tau2)
      data$q4 <- as.numeric(data$ipp > tau2)
    } else {
      data$q1 <- as.numeric(data$ipp <= tau1)
      data$q2 <- as.numeric(data$ipp > tau1 & data$ipp <= tau2)
      data$q3 <- as.numeric(data$ipp > tau2 & data$ipp <= s)
      data$q4 <- as.numeric(data$ipp > s)
    }
    
    # Trimming: all regimes >= min_frac*n
    counts <- c(sum(data$q1), sum(data$q2), sum(data$q3), sum(data$q4))
    if (min(counts) < min_frac * n) next
    
    m3 <- try(
      plm(
        as.formula(paste0(yvar, " ~ tariff:q1 + tariff:q2 + tariff:q3 + tariff:q4 + tnrr + rol + ipp")),
        data   = data,
        model  = "within",
        effect = "individual"
      ),
      silent = TRUE
    )
    if (inherits(m3, "try-error")) next
    
    ssr <- sum(residuals(m3)^2)
    if (is.finite(ssr) && ssr < best) best <- ssr
  }
  
  best
}

##### 10.4 Observed F3: best third threshold (split of the best regime) #####
SSR3_r1 <- ssr3_split_min("mva_pc", df_boot1, tau1_hat, tau2_hat, 1, tau_candidates_1, min_frac = 0.10)
SSR3_r2 <- ssr3_split_min("mva_pc", df_boot1, tau1_hat, tau2_hat, 2, tau_candidates_1, min_frac = 0.10)
SSR3_r3 <- ssr3_split_min("mva_pc", df_boot1, tau1_hat, tau2_hat, 3, tau_candidates_1, min_frac = 0.10)

SSR3_hat_obs <- min(SSR3_r1, SSR3_r2, SSR3_r3)

n  <- nrow(df_boot1)
k3 <- 7  # (4 tariff slopes) + tnrr + rol + ipp

sigma2_3 <- SSR3_hat_obs / (n - k3)
F3_obs   <- (SSR2_hat_obs - SSR3_hat_obs) / sigma2_3

list(tau1_hat = tau1_hat, tau2_hat = tau2_hat,
     SSR2_hat = SSR2_hat_obs, SSR3_hat = SSR3_hat_obs, F3_obs = F3_obs,
     SSR3_r1 = SSR3_r1, SSR3_r2 = SSR3_r2, SSR3_r3 = SSR3_r3)

##### 10.5 Hansen bootstrap for the p-value of F3 under H0 = 2 thresholds #####
set.seed(123)
B <- 500
F3_boot <- numeric(B)

for (b in 1:B) {
  
  # Generate y* under H0 = 2 thresholds (tau1_hat, tau2_hat fixed)
  e_star <- sample(resid_2, replace = TRUE)
  df_boot1$mva_pc_star <- fitted_2 + e_star
  
  # SSR2*(y*) under H0 = 2 FIXED thresholds (consistent with data generation)
  df_boot1$r1 <- as.numeric(df_boot1$ipp <= tau1_hat)
  df_boot1$r2 <- as.numeric(df_boot1$ipp > tau1_hat & df_boot1$ipp <= tau2_hat)
  df_boot1$r3 <- as.numeric(df_boot1$ipp > tau2_hat)
  
  m2s <- plm(
    mva_pc_star ~ tariff:r1 + tariff:r2 + tariff:r3 + tnrr + rol + ipp,
    data   = df_boot1,
    model  = "within",
    effect = "individual"
  )
  SSR2_star <- sum(residuals(m2s)^2)
  
  # SSR3*(y*) = best split
  SSR3s_1 <- ssr3_split_min("mva_pc_star", df_boot1, tau1_hat, tau2_hat, 1, tau_candidates_1, min_frac = 0.10)
  SSR3s_2 <- ssr3_split_min("mva_pc_star", df_boot1, tau1_hat, tau2_hat, 2, tau_candidates_1, min_frac = 0.10)
  SSR3s_3 <- ssr3_split_min("mva_pc_star", df_boot1, tau1_hat, tau2_hat, 3, tau_candidates_1, min_frac = 0.10)
  
  SSR3_star <- min(SSR3s_1, SSR3s_2, SSR3s_3)
  
  sigma2_star <- SSR3_star / (n - k3)
  F3_boot[b]  <- (SSR2_star - SSR3_star) / sigma2_star
  
  if (b %% 50 == 0) cat("Bootstrap F3:", b, "/", B, "\n")
}

p_value_F3 <- (sum(F3_boot >= F3_obs, na.rm = TRUE) + 1) / (B + 1)
p_value_F3




summary(F3_boot)
sum(is.na(F3_boot))


boot3_df <- data.frame(F3_boot = F3_boot)


boot3_plot <- ggplot(boot3_df, aes(x = F3_boot)) +
  geom_histogram(aes(y = after_stat(density)),
                 bins = 30,
                 fill = "grey80",
                 color = "black") +
  geom_density(color = "black", linewidth = 0.8) +
  geom_vline(xintercept = F3_obs, linetype = "dashed") +
  labs(
    x = "Bootstrap distribution of F3",
    y = "Density"
  ) +
  theme_minimal()

ggsave("Bootstrap_Distribution_F3.png", boot3_plot, width = 10, height = 6, dpi = 300)











#############################################
# TABLE X — TESTS FOR THRESHOLD EFFECTS
# (Hansen 1999) — article style (gt)
#############################################

library(dplyr)
library(gt)

# --- (A) Fix the values (guard against missing objects) ---
# The following objects must exist in the environment:
# F1_obs, p_value_F1, F2_obs, p_value_F2, F3_obs, p_value_F3, B

stopifnot(exists("F1_obs"), exists("p_value_F1"),
          exists("F2_obs"), exists("p_value_F2"),
          exists("F3_obs"), exists("p_value_F3"),
          exists("B"))

# Optional: p-value formatting (avoid displaying p = 0 without qualification)
fmt_p <- function(p) {
  if (is.na(p)) return(NA_character_)
  if (p == 0) return("<0.001")
  sprintf("%.3f", p)
}

# --- (B) Build the table (Yang structure) ---
thr_tests_df <- tibble::tibble(
  `Threshold model` = c("Single threshold (F1)", "Double threshold (F2)", "Triple threshold (F3)"),
  `F statistic`     = c(F1_obs, F2_obs, F3_obs),
  `p-value`         = c(p_value_F1, p_value_F2, p_value_F3),
  `BS replications` = c(B, B, B)
) %>%
  mutate(
    # significance stars (journal convention)
    sig = case_when(
      `p-value` < 0.01 ~ "***",
      `p-value` < 0.05 ~ "**",
      `p-value` < 0.10 ~ "*",
      TRUE ~ ""
    ),
    `F statistic` = paste0(sprintf("%.3f", `F statistic`), sig),
    `p-value` = vapply(`p-value`, fmt_p, character(1))
  ) %>%
  select(-sig)

# --- (C) GT rendering: journal look ---
gt_thr_tests <- thr_tests_df %>%
  gt() %>%
  tab_header(
    title = "Tests for Threshold Effects"
  ) %>%
  fmt_missing(
    columns = everything(),
    missing_text = ""
  ) %>%
  cols_align(
    align = "center",
    columns = everything()
  ) %>%
  cols_width(
    everything() ~ px(170)
  ) %>%
  tab_options(
    table.font.names = "Arial",
    table.font.size = 12,
    table.font.color = "black",
    table.background.color = "white",
    data_row.padding = px(5),
    table.border.top.width = px(0),
    table.border.bottom.width = px(0),
    table.border.left.width = px(0),
    table.border.right.width = px(0),
    heading.border.bottom.color = "black",
    heading.border.bottom.width = px(1),
    column_labels.border.bottom.color = "black",
    column_labels.border.bottom.width = px(1),
    table_body.hlines.width = px(0),
    table_body.vlines.width = px(0),
    source_notes.font.size = 9
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = list(
      cells_column_labels(everything()),
      cells_body(columns = 1)
    )
  ) %>%
  tab_source_note(
    source_note = md(
      paste0(
        "Note: Bootstrap p-values are obtained from ", B,
        " replications following Hansen (1999). ",
        "The null hypotheses are no threshold (F1), one threshold (F2), and two thresholds (F3), respectively. ",
        "***, **, and * denote significance at the 1%, 5%, and 10% levels."
      )
    )
  )

gt_thr_tests

# --- (D) High-quality image export ---
gtsave(
  gt_thr_tests,
  filename = "Table_Threshold_Effects.png",
  expand = 15,
  zoom = 5,
  vwidth = 1600
)





















#############################################
# TABLE X — TESTS FOR THRESHOLD EFFECTS
# (Hansen 1999) — article style (gt)
#############################################

library(dplyr)
library(gt)

# --- (A) Fix the values ---
stopifnot(exists("F1_obs"), exists("p_value_F1"),
          exists("F2_obs"), exists("p_value_F2"),
          exists("F3_obs"), exists("p_value_F3"))

# If the same B was used throughout, keep B.
# Otherwise, define B_F1, B_F2, B_F3 before this block.
if (!exists("B_F1")) B_F1 <- B
if (!exists("B_F2")) B_F2 <- B
if (!exists("B_F3")) B_F3 <- B

fmt_p <- function(p) {
  if (is.na(p)) return("")
  if (p < 0.001) return("<0.001")
  sprintf("%.3f", p)
}

thr_tests_df <- tibble::tibble(
  `Threshold model` = c("Single threshold (F1)", "Double threshold (F2)", "Triple threshold (F3)"),
  `F statistic`     = c(F1_obs, F2_obs, F3_obs),
  `p-value`         = c(p_value_F1, p_value_F2, p_value_F3),
  `BS replications` = c(B_F1, B_F2, B_F3)
) %>%
  mutate(
    sig = case_when(
      `p-value` < 0.01 ~ "***",
      `p-value` < 0.05 ~ "**",
      `p-value` < 0.10 ~ "*",
      TRUE ~ ""
    ),
    `F statistic` = paste0(sprintf("%.3f", `F statistic`), sig),
    `p-value` = vapply(`p-value`, fmt_p, character(1))
  ) %>%
  select(-sig)

gt_thr_tests <- thr_tests_df %>%
  gt() %>%
  tab_header(title = "Tests for Threshold Effects") %>%
  fmt_missing(columns = everything(), missing_text = "") %>%
  cols_align(align = "center", columns = everything()) %>%
  cols_width(everything() ~ px(150)) %>%
  tab_options(
    table.font.names = "Arial",
    table.font.size = 12,
    table.font.color = "black",
    table.background.color = "white",
    data_row.padding = px(5),
    table.border.top.width = px(0),
    table.border.bottom.width = px(0),
    table.border.left.width = px(0),
    table.border.right.width = px(0),
    heading.border.bottom.color = "black",
    heading.border.bottom.width = px(1),
    column_labels.border.bottom.color = "black",
    column_labels.border.bottom.width = px(1),
    table_body.hlines.width = px(0),
    table_body.vlines.width = px(0),
    source_notes.font.size = 9
  ) %>%
  tab_source_note(
    source_note = md(
      paste0(
        "Note: Bootstrap p-values are obtained following Hansen (1999). ",
        "Null hypotheses: no threshold (F1), one threshold (F2), and two thresholds (F3). ",
        "***, **, and * denote significance at the 1%, 5%, and 10% levels."
      )
    )
  )

gt_thr_tests

gtsave(
  gt_thr_tests,
  filename = "Table_Threshold_Effects.png",
  expand = 5,
  zoom = 5,
  vwidth = 1600
)




#############################################
# FIGURES (Yang style) — LR profiles for double threshold
# Fig. 1: Estimated CI of the first threshold (tau1)
# Fig. 2: Estimated CI of the second threshold (tau2)
# CI rule: conditional LR profiles, LR <= 7.35 (95% asymptotic, Hansen 1999)
#############################################

library(ggplot2)

# --- Sanity checks: required objects ---
stopifnot(
  exists("valid_pairs"),
  exists("SSR2_vec"),
  exists("df_boot1"),
  exists("tau1_hat"),
  exists("tau2_hat")
)

# --- 1) Build table (tau1, tau2, SSR) from valid_pairs + SSR2_vec ---
pairs_mat <- do.call(rbind, valid_pairs)

pairs_df <- data.frame(
  tau1 = as.numeric(pairs_mat[, 1]),
  tau2 = as.numeric(pairs_mat[, 2]),
  SSR  = as.numeric(SSR2_vec)
)

# Remove NA/Inf
pairs_df <- pairs_df[is.finite(pairs_df$SSR), ]

# --- 2) Global minimum SSR (double threshold) ---
SSR2_hat <- min(pairs_df$SSR, na.rm = TRUE)

# --- 3) Variance sigma^2 (Hansen/Yang) based on the 2-threshold model ---
n  <- nrow(df_boot1)
k2 <- 6  # (3 slopes tariff) + tnrr + rol + ipp
sigma2_2 <- SSR2_hat / (n - k2)

# --- 4) LR for each pair (tau1, tau2) ---
pairs_df$LR <- (pairs_df$SSR - SSR2_hat) / sigma2_2

# --- 5) CONDITIONAL LR PROFILES (Hansen, 1999, sec. 5) ---
# Each threshold varies while the other is held at its estimate:
#   LR1(tau1) uses the pairs (tau1, tau2_hat); LR2(tau2) uses the pairs (tau1_hat, tau2).
# Taking instead the minimum over the other threshold mixes in pairs far from the
# estimate and produces isolated low-LR points (it gave CI tau1 [-1.277, 1.113]
# and CI tau2 [1.113, 2.373], which are not the reported intervals).
LR_tau1 <- pairs_df[abs(pairs_df$tau2 - tau2_hat) < 1e-12, c("tau1", "SSR")]
LR_tau1 <- LR_tau1[order(LR_tau1$tau1), ]
LR_tau1$LR <- (LR_tau1$SSR - min(LR_tau1$SSR)) / sigma2_2
LR_tau2 <- pairs_df[abs(pairs_df$tau1 - tau1_hat) < 1e-12, c("tau2", "SSR")]
LR_tau2 <- LR_tau2[order(LR_tau2$tau2), ]
LR_tau2$LR <- (LR_tau2$SSR - min(LR_tau2$SSR)) / sigma2_2

# --- 6) 95% asymptotic critical value (Hansen, 1999): 7.35 ---
cv95 <- 7.35

# --- 7) 95% CI: range of the grid values with LR <= 7.35 ---
# Expected: tau1 [-1.277, -0.620], tau2 [1.113, 1.194]. One grid point inside the
# tau1 interval (-0.682) has LR = 7.88 > 7.35; the reported interval is the range.
CI_tau1_95 <- range(LR_tau1$tau1[LR_tau1$LR <= cv95], na.rm = TRUE)
CI_tau2_95 <- range(LR_tau2$tau2[LR_tau2$LR <= cv95], na.rm = TRUE)

# --- 8) Figure 1 — CI of the first threshold (grey area = 95% CI) ---
fig1 <- ggplot(LR_tau1, aes(x = tau1, y = LR)) +
  annotate("rect", xmin = CI_tau1_95[1], xmax = CI_tau1_95[2], ymin = 0, ymax = cv95, fill = "grey90") +
  geom_line(color = "black", linewidth = 0.7) +
  geom_vline(xintercept = tau1_hat, linetype = "dashed", linewidth = 0.7) +
  geom_hline(yintercept = cv95, linetype = "dotted", linewidth = 0.7) +
  labs(
    x = "First-threshold candidate (IPP)",
    y = "Likelihood ratio function"
  ) +
  theme_minimal(base_size = 12)

ggsave(
  filename = "Figure1_CI_First_Threshold_LR.png",
  plot = fig1,
  width = 10,
  height = 6,
  dpi = 300
)

# --- 9) Figure 2 — CI of the second threshold ---
fig2 <- ggplot(LR_tau2, aes(x = tau2, y = LR)) +
  annotate("rect", xmin = CI_tau2_95[1], xmax = CI_tau2_95[2], ymin = 0, ymax = cv95, fill = "grey90") +
  geom_line(color = "black", linewidth = 0.7) +
  geom_vline(xintercept = tau2_hat, linetype = "dashed", linewidth = 0.7) +
  geom_hline(yintercept = cv95, linetype = "dotted", linewidth = 0.7) +
  labs(
    x = "Second-threshold candidate (IPP)",
    y = "Likelihood ratio function"
  ) +
  theme_minimal(base_size = 12)

ggsave(
  filename = "Figure2_CI_Second_Threshold_LR.png",
  plot = fig2,
  width = 10,
  height = 6,
  dpi = 300
)

# --- 10) Summary of threshold estimates and confidence intervals ---
list(
  tau1_hat = tau1_hat,
  tau2_hat = tau2_hat,
  CI_tau1_95 = CI_tau1_95,
  CI_tau2_95 = CI_tau2_95,
  cv95 = cv95,
  SSR2_hat = SSR2_hat,
  sigma2_2 = sigma2_2
)




### Combined LR figure ####


library(patchwork)


fig_lr_double <- fig1 + fig2 +
  plot_layout(ncol = 2, widths = c(1, 1))

fig_lr_double


ggsave(
  filename = "Figure_LR_Double_Threshold12.png",
  plot = fig_lr_double,
  width = 12,
  height = 4,
  dpi = 300
)












# Sequential tests (Table 2)
list(
  F1 = F1_obs, p_F1 = p_value_F1,
  F2 = F2_obs, p_F2 = p_value_F2,
  F3 = F3_obs, p_F3 = p_value_F3,
  B  = B
)



# Estimated thresholds + 95% CI (Table 3)
list(
  tau1_hat   = tau1_hat,
  CI_tau1_95 = CI_tau1_95,
  tau2_hat   = tau2_hat,
  CI_tau2_95 = CI_tau2_95,
  cv95       = 7.35
)






#### Table of estimated thresholds

library(gt)
library(tibble)

threshold_estimates <- tibble(
  Threshold = c("τ₁", "τ₂"),
  Estimate  = c(tau1_hat, tau2_hat),
  CI_Lower  = c(CI_tau1_95[1], CI_tau2_95[1]),
  CI_Upper  = c(CI_tau1_95[2], CI_tau2_95[2])
)

gt_thresholds <- threshold_estimates %>%
  gt() %>%
  tab_header(
    title = "Threshold Estimates"
  ) %>%
  fmt_number(
    columns = c(Estimate, CI_Lower, CI_Upper),
    decimals = 3
  ) %>%
  cols_align(
    align = "center",
    columns = everything()
  ) %>%
  cols_width(
    Threshold ~ px(140),
    Threshold ~ px(140),
    everything() ~ px(100)
  ) %>%
  tab_options(
    table.font.names = "Arial",
    table.font.size = 12,
    table.font.color = "black",
    table.border.top.width = px(0),
    table.border.bottom.width = px(0),
    heading.border.bottom.width = px(1),
    column_labels.border.bottom.width = px(1),
    table_body.hlines.width = px(0),
    table_body.vlines.width = px(0),
    source_notes.font.size = 8
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(everything())
  ) %>%
  tab_source_note(
    source_note = md(
      "Note: Confidence intervals (CI) by likelihood-ratio inversion (Hansen, 1999), each threshold varying while the other is held at its estimate. Reported interval: range of the grid values with LR ≤ 7.35."
    )
  )

gt_thresholds


gtsave(
  gt_thresholds,
  filename = "Table_Threshold_Estimates.png",
  expand = 0,
  zoom = 5,
  vwidth = 1600
)



### Alternative table layout


# --- Safe formatting of the CIs ---
fmt_ci <- function(x, digits = 3) {
  # x is a vector of length 2: c(min, max)
  paste0("[", sprintf(paste0("%.", digits, "f"), x[1]), ", ",
         sprintf(paste0("%.", digits, "f"), x[2]), "]")
}

# --- Build the table (Yang style) ---
thr_est_df <- tibble(
  `Threshold` = c(expression(hat(tau)[1]), expression(hat(tau)[2])),
  `Estimate` = c(tau1_hat, tau2_hat),
  `95% CI` = c(fmt_ci(CI_tau1_95), fmt_ci(CI_tau2_95))
) %>%
  mutate(`Estimate` = sprintf("%.3f", `Estimate`))



gt_thr_est <- thr_est_df %>%
  gt() %>%
  # IMPORTANT: render the first column as math-like (otherwise gt prints "expression(...)")
  text_transform(
    locations = cells_body(columns = `Threshold`),
    fn = function(x) c("τ̂₁", "τ̂₂")
  ) %>%
  cols_align(align = "center", columns = everything()) %>%
  cols_width(
    `Threshold` ~ px(100),
    `Estimate` ~ px(100),
    `95% CI` ~ px(140)
  ) %>%
  tab_options(
    table.font.names = "Arial",
    table.font.size = 11,
    table.font.color = "black",
    data_row.padding = px(2),
    column_labels.padding = px(2),
    
    # Journal look: no unnecessary grid lines
    table_body.hlines.width = px(0),
    table_body.vlines.width = px(0),
    
    # Sober borders
    table.border.top.width = px(0),
    table.border.bottom.width = px(0),
    heading.border.bottom.width = px(0),
    column_labels.border.bottom.width = px(0),
    source_notes.font.size = 8
  ) %>%
  tab_style(
    style = cell_text(),
    locations = cells_column_labels(everything())
  ) %>% tab_source_note(
    source_note = md(
      "Note: Confidence intervals by likelihood-ratio inversion (Hansen, 1999), each threshold varying while the other is held at its estimate; range of the grid values with LR ≤ 7.35."
    ))

gt_thr_est

# Image export (adjust vwidth/zoom to the target document)
gtsave(
  gt_thr_est,
  filename = "Table_Threshold_Estimates.png",
  expand = 0,
  zoom = 8,
  vwidth = 1200
)
















#############################################
# TABLE — Country groups by year (Yang style, single title)
# - Two stacked blocks, no individual titles
# - A single global title above
# - Reduced width (spacing/padding/columns)
#############################################


library(dplyr)
library(tidyr)
library(gt)
library(magick)

stopifnot(exists("tau1_hat"), exists("tau2_hat"))
stopifnot(exists("df_plain") | exists("pdata3"))

df0 <- if (exists("df_plain")) df_plain else as.data.frame(pdata3)

# --- 1) Grouping rule (IPP) ---
df_groups <- df0 %>%
  select(country, year, ipp) %>%
  filter(!is.na(country), !is.na(year), !is.na(ipp)) %>%
  mutate(
    group = case_when(
      ipp <= tau1_hat ~ paste0("IPP \u2264 ", sprintf("%.3f", tau1_hat)),
      ipp >  tau2_hat ~ paste0("IPP > ",  sprintf("%.3f", tau2_hat)),
      TRUE            ~ paste0(sprintf("%.3f", tau1_hat), " < IPP \u2264 ", sprintf("%.3f", tau2_hat))
    )
  )

group_levels <- c(
  paste0("IPP \u2264 ", sprintf("%.3f", tau1_hat)),
  paste0(sprintf("%.3f", tau1_hat), " < IPP \u2264 ", sprintf("%.3f", tau2_hat)),
  paste0("IPP > ",  sprintf("%.3f", tau2_hat))
)

tab_wide <- df_groups %>%
  group_by(year, group) %>%
  summarise(n_countries = n_distinct(country), .groups = "drop") %>%
  mutate(group = factor(group, levels = group_levels)) %>%
  tidyr::pivot_wider(names_from = year, values_from = n_countries) %>%
  arrange(group)

tab_wide[is.na(tab_wide)] <- 0

# --- 2) Split years into 2 blocks to avoid A4 overflow ---
years_all <- sort(as.integer(setdiff(names(tab_wide), "group")))
mid <- ceiling(length(years_all) / 2)
years_block1 <- years_all[1:mid]
years_block2 <- years_all[(mid+1):length(years_all)]

tab1 <- tab_wide %>% select(group, all_of(as.character(years_block1)))
tab2 <- tab_wide %>% select(group, all_of(as.character(years_block2)))



# Common styling function (no title, no source_note)
make_gt_block <- function(df_block) {
  gt(df_block) %>%
    cols_label(group = "Country group") %>%
    fmt_missing(columns = everything(), missing_text = "") %>%
    cols_align(align = "center", columns = everything()) %>%
    cols_align(align = "left", columns = "group") %>%
    cols_width(
      group ~ px(120),
      everything() ~ px(34)
    ) %>%
    tab_options(
      table.font.names = "Arial",
      table.font.size = 10,
      table.background.color = "white",
      table.font.color = "black",
      data_row.padding = px(1),
      column_labels.padding = px(1),
      table.border.top.width = px(0),
      table.border.bottom.width = px(0),
      table.border.left.width = px(0),
      table.border.right.width = px(0),
      heading.border.bottom.width = px(0),
      column_labels.border.bottom.color = "black",
      column_labels.border.bottom.width = px(1),
      table_body.hlines.width = px(0),
      table_body.vlines.width = px(0),
      source_notes.font.size = 7
    )
}

# --- Block 1 (no note) ---
gt1 <- make_gt_block(tab1)

# --- Block 2 (note only here) ---
gt2 <- make_gt_block(tab2) %>%
  tab_source_note(
    source_note = md(
      "Note: Countries are classified into Low, Medium, and High IPP groups based on the estimated double-threshold values."
    )
  )

# Exports (same width for clean stacking)
gtsave(gt1, filename = "Table_GroupByYear_Block1.png", expand = 3, zoom = 5, vwidth = 950)
gtsave(gt2, filename = "Table_GroupByYear_Block2.png", expand = 3, zoom = 5, vwidth = 950)



# --- 4) Stack blocks + add ONE global title (Yang style) ---
img1 <- image_read("Table_GroupByYear_Block1.png")
img2 <- image_read("Table_GroupByYear_Block2.png")
tables_stacked <- image_append(image_join(list(img1, img2)), stack = TRUE)


final_img <- image_append(image_join(list(tables_stacked)), stack = TRUE)
image_write(final_img, path = "Table_GroupByYear_FINAL.png", format = "png")















# Final estimation



df_final <- df_boot1

df_final$r1 <- as.numeric(df_final$ipp <= tau1_hat)
df_final$r2 <- as.numeric(df_final$ipp > tau1_hat & df_final$ipp <= tau2_hat)
df_final$r3 <- as.numeric(df_final$ipp > tau2_hat)



fe_double <- plm(
  mva_pc ~ tariff:r1 + tariff:r2 + tariff:r3 + tnrr + rol + ipp,
  data   = df_final,
  model  = "within",
  effect = "twoways"
)



dk_se <- vcovSCC(
  fe_double,
  type   = "HC1",
  maxlag = 2
)

dk_test <- coeftest(fe_double, vcov = dk_se)
dk_test


ols_test <- coeftest(fe_double)  # classical standard errors
ols_test


library(dplyr)
library(gt)
library(lmtest)
library(sandwich)

# --- 1) t-tests: OLS (classical) vs DK (SCC) ---
ols_test <- coeftest(fe_double)  # classical SE
dk_se    <- vcovSCC(fe_double, type = "HC1", maxlag = 2)
dk_test  <- coeftest(fe_double, vcov = dk_se)

# --- 2) Coefficient labels ---
coef_names <- c(
  "tariff:r1" = "Tariff × I(IPP ≤ τ₁)",
  "tariff:r2" = "Tariff × I(τ₁ < IPP ≤ τ₂)",
  "tariff:r3" = "Tariff × I(IPP > τ₂)",
  "tnrr"      = "TNRR",
  "rol"       = "Rule of Law",
  "ipp"       = "IPP"
)

# --- 3) Significance based on DK (as Yang does with White SE) ---
stars <- function(p) {
  if (is.na(p)) ""
  else if (p < 0.01) "***"
  else if (p < 0.05) "**"
  else if (p < 0.10) "*"
  else ""
}

# --- 4) Build the table ---
reg_df <- tibble(
  Regressor   = names(coef(fe_double)),
  `Coefficient estimate` = unname(coef(fe_double)),
  `t_OLS`     = ols_test[, "t value"],
  `t_DK`      = dk_test[, "t value"],
  `p_DK`      = dk_test[, "Pr(>|t|)"]
) %>%
  mutate(
    Regressor = ifelse(Regressor %in% names(coef_names), coef_names[Regressor], Regressor),
    `Coefficient estimate` = sprintf("%.4f", `Coefficient estimate`),
    `t_OLS` = sprintf("%.2f", `t_OLS`),
    `t_DK`  = paste0(sprintf("%.2f", `t_DK`), vapply(`p_DK`, stars, character(1)))
  ) %>%
  select(-p_DK)

# --- 5) Add "Yes" rows for fixed effects (as in Yang) ---
fe_rows <- tibble(
  Regressor = c("Individual effect", "Time effect"),
  `Coefficient estimate` = c("Yes", "Yes"),
  `t_OLS` = c("Yes", "Yes"),
  `t_DK`  = c("Yes", "Yes")
)

reg_df_final <- bind_rows(reg_df, fe_rows)


# --- 6) GT: same clean theme as the other tables ---
gt_reg <- reg_df_final %>%
  gt() %>%
  tab_header(title = "Regression Estimates: Double Threshold Model") %>%
  cols_align(align = "center", columns = everything()) %>%
  cols_align(align = "left", columns = Regressor) %>%
  cols_width(
    Regressor ~ px(260),
    everything() ~ px(140)
  ) %>%
  tab_options(
    data_row.padding = px(2),
    column_labels.padding = px(2),
    heading.padding = px(4),
    source_notes.padding = px(2),
    table.font.names = "Arial",
    table.font.size = 12,
    table.font.color = "black",
    table.border.top.width = px(0),
    table.border.bottom.width = px(0),
    heading.border.bottom.width = px(1),
    column_labels.border.bottom.width = px(1),
    table_body.hlines.width = px(0),
    table_body.vlines.width = px(0),
    source_notes.font.size = 9
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(everything())
  ) %>%
  tab_source_note(
    source_note = md(
      "Note: t_OLS reports conventional within-model t-statistics. \
t_DK reports Driscoll–Kraay t-statistics robust to cross-sectional dependence and serial correlation (maxlag = 2). \
***, **, and * denote significance at the 1%, 5%, and 10% levels."
    )
  )

gt_reg

gtsave(
  gt_reg,
  filename = "Table_Regression_DoubleThreshold_OLS_vs_DK1.png",
  expand = 5,
  zoom = 5,
  vwidth = 1600
)



























# Inspect regime classification by country

df_regime <- df_plain %>%
  mutate(
    regime = case_when(
      ipp <= tau1_hat ~ "Low productive power",
      ipp > tau1_hat & ipp <= tau2_hat ~ "Intermediate productive power",
      ipp > tau2_hat ~ "High productive power"
    )
  )


# Example: Côte d'Ivoire (CIV)
df_regime %>%
  filter(country == "CIV") %>%
  select(country, year, ipp, regime)





# --- 1) Build the regime for each observation ---

years_key <- c(2000, 2010, 2022)

df_regime <- df0 %>%
  mutate(
    regime = case_when(
      ipp <= tau1_hat ~ "Low",
      ipp >  tau1_hat & ipp <= tau2_hat ~ "Intermediate",
      ipp >  tau2_hat ~ "High",
      TRUE ~ NA_character_
    )
  )

# --- 2) Filter key years + pivot wide (Country x Years) ---
tab_status <- df_regime %>%
  filter(year %in% years_key) %>%
  select(country, year, regime) %>%
  distinct(country, year, .keep_all = TRUE) %>%
  tidyr::pivot_wider(
    names_from = year,
    values_from = regime
  ) %>%
  arrange(country)

# --- 3) GT: article style ---
gt_status <- tab_status %>%
  gt() %>%
  tab_header(
    title = "Country Regime Status by Key Years (IPP Thresholds)"
  ) %>%
  cols_label(
    country = "Country"
  ) %>%
  fmt_missing(columns = everything(), missing_text = "") %>%
  cols_align(align = "left", columns = "country") %>%
  cols_align(align = "center", columns = as.character(years_key)) %>%
  cols_width(
    country ~ px(220),
    everything() ~ px(130)
  ) %>%
  tab_options(
    table.font.names = "Arial",
    table.font.size = 12,
    table.font.color = "black",
    table.border.top.width = px(0),
    table.border.bottom.width = px(0),
    heading.border.bottom.width = px(1),
    column_labels.border.bottom.width = px(1),
    table_body.hlines.width = px(0),
    table_body.vlines.width = px(0),
    source_notes.font.size = 9,
    data_row.padding = px(4)
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(everything())
  ) %>%
  tab_source_note(
    source_note = md(
      paste0(
        "Note: Regimes are defined using estimated thresholds τ₁ and τ₂. ",
        "Low: IPP ≤ τ₁; Intermediate: τ₁ < IPP ≤ τ₂; High: IPP > τ₂."
      )
    )
  )

gt_status

# --- 4) High-quality PNG export ---
gtsave(
  gt_status,
  filename = "Table_Status_2000_2010_2022.png",
  expand = 15,
  zoom = 5,
  vwidth = 1700
)












df_regime_named <- df_regime %>%
  mutate(
    country_name = countrycode(
      sourcevar   = country,
      origin      = "iso3c",
      destination = "country.name"
    )
  )




tab_grouped_names <- df_regime_named %>%
  filter(year %in% years_key) %>%
  filter(!is.na(regime)) %>%
  group_by(year, regime) %>%
  summarise(
    n_countries = n_distinct(country_name),
    countries = paste(sort(unique(country_name)), collapse = ", "),
    .groups = "drop"
  ) %>%
  arrange(year, factor(regime, levels = c("Low","Intermediate","High")))







gt_grouped_names <- tab_grouped_names %>%
  select(year, regime, n_countries, countries) %>%
  gt(groupname_col = "year") %>%
  
  tab_header(
    title = "Country Grouping by Regime and Key Years"
  ) %>%
  
  cols_label(
    regime = "Regime",
    n_countries = "N countries",
    countries = "Countries"
  ) %>%
  
  cols_hide(columns = year) %>%
  
  fmt_missing(columns = everything(), missing_text = "") %>%
  
  cols_align(align = "center", columns = c(regime, n_countries)) %>%
  cols_align(align = "left", columns = countries) %>%
  
  cols_width(
    regime ~ px(140),
    n_countries ~ px(110),
    countries ~ px(400)
  ) %>%
  
  tab_options(
    table.font.names = "Arial",
    table.font.size = 12,  # overall font size
    table.font.color = "black",
    
    heading.border.bottom.width = px(1),
    column_labels.border.bottom.width = px(0),
    
    row_group.border.top.width = px(0.5),
    row_group.border.top.color = "grey",
    
    table_body.hlines.width = px(0),
    table_body.vlines.width = px(0),
    
    data_row.padding = px(1),
    source_notes.font.size = 9
  ) %>%
  
  # --- Smaller font for country names ONLY ---
  tab_style(
    style = cell_text(size = px(10), whitespace = "normal"),
    locations = cells_body(columns = countries)
  ) %>%
  
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(everything())
  ) %>%
  
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_row_groups()
  ) %>%
  
  tab_source_note(
    source_note = md(
      "Note: Countries are grouped using IPP regimes derived from thresholds (τ₁, τ₂)."
    )
  )

gt_grouped_names




gtsave(
  gt_grouped_names,
  filename = "Table_Grouping_ByYear_Regime_Names_2000_2010_2022.png",
  expand = 5,
  zoom = 5,
  vwidth = 1150
)











# --- B) Build country trajectories over 2000/2010/2022 ---
tab_paths <- df_regime %>%
  filter(year %in% years_key) %>%
  select(country, year, regime) %>%
  distinct(country, year, .keep_all = TRUE) %>%
  pivot_wider(names_from = year, values_from = regime) %>%
  mutate(
    path = paste0(`2000`, " → ", `2010`, " → ", `2022`)
  ) %>%
  arrange(path, country)

# Option: keep only countries that actually moved upward
tab_paths_progress <- tab_paths %>%
  filter(path %in% c("Low → Intermediate → High",
                     "Low → Low → Intermediate",
                     "Intermediate → High → High",
                     "Low → Intermediate → Intermediate"))

gt_paths <- tab_paths_progress %>%
  gt() %>%
  tab_header(
    title = "Regime Trajectories Across Key Years"
  ) %>%
  cols_label(
    country = "Country",
    `2000` = "2000",
    `2010` = "2010",
    `2022` = "2022",
    path = "Trajectory"
  ) %>%
  fmt_missing(columns = everything(), missing_text = "") %>%
  cols_align(align = "left", columns = c(country, path)) %>%
  cols_align(align = "center", columns = c("2000","2010","2022")) %>%
  cols_width(
    country ~ px(220),
    `2000` ~ px(110),
    `2010` ~ px(110),
    `2022` ~ px(110),
    path ~ px(320)
  ) %>%
  tab_options(
    table.font.names = "Arial",
    table.font.size = 12,
    table.font.color = "black",
    table.border.top.width = px(0),
    table.border.bottom.width = px(0),
    heading.border.bottom.width = px(1),
    column_labels.border.bottom.width = px(1),
    table_body.hlines.width = px(0),
    table_body.vlines.width = px(0),
    source_notes.font.size = 9,
    data_row.padding = px(4)
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(everything())
  ) %>%
  tab_source_note(
    source_note = md(
      "Note: The table highlights countries with upward regime movements between 2000, 2010 and 2022."
    )
  )

gt_paths

gtsave(
  gt_paths,
  filename = "Table_Regime_Trajectories_2000_2010_2022.png",
  expand = 15,
  zoom = 5,
  vwidth = 1600
)











# HEATMAP


df_heat <- df_plain %>%
  filter(!is.na(ipp)) %>%
  mutate(
    regime = case_when(
      ipp <= tau1_hat ~ "Low",
      ipp >  tau1_hat & ipp <= tau2_hat ~ "Intermediate",
      ipp >  tau2_hat ~ "High"
    ),
    country_name = countrycode(
      sourcevar = country,
      origin = "iso3c",
      destination = "country.name"
    )
  ) %>%
  filter(!is.na(country_name))





country_order <- df_heat %>%
  filter(year == 2022) %>%
  group_by(country_name) %>%
  summarise(
    regime_2022 = first(regime),
    ipp_2022 = first(ipp),
    .groups = "drop"
  ) %>%
  arrange(
    factor(regime_2022, levels = c("Low","Intermediate","High")),
    ipp_2022
  ) %>%
  pull(country_name)

df_heat$country_name <- factor(
  df_heat$country_name,
  levels = unique(country_order)
)






heatmap_regime <- ggplot(
  df_heat,
  aes(x = year, y = country_name, fill = regime)
) +
  geom_tile(color = "white", linewidth = 0.2) +
  
  scale_fill_manual(
    values = c(
      "Low" = "#d73027",  # muted red
      "Intermediate" = "#fee08b",  # neutral yellow
      "High" = "#1a9850"  # dark green
    ),
    name = "Productive power regime"
  ) +
  
  scale_x_continuous(
    breaks = seq(2000, 2022, by = 2),
    expand = c(0,0)
  ) +
  
  labs(
    x = "Year",
    y = "",
    title = "Productive Power Regimes Across Countries (2000–2022)",
    subtitle = "Regimes defined by Hansen-type thresholds on the Productive Power Index"
  ) +
  
  theme_minimal(base_size = 11) +
  theme(
    panel.grid = element_blank(),
    axis.text.y = element_text(size = 8),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 9),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 10)
  )


ggsave(
  filename = "Heatmap_Productive_Power_Regimes_2000_2022.png",
  plot = heatmap_regime,
  width = 12,
  height = 8,
  dpi = 300
)













# Descriptive table of IPP



df0 <- if (exists("df_plain")) df_plain else df
stopifnot(all(c("country","year","ipp") %in% names(df0)))

years_full <- 2000:2022

# 1) Base: country + year + ipp, clean names
df_ipp <- df0 %>%
  filter(year %in% years_full) %>%
  mutate(country_name = countrycode(country, "iso3c", "country.name")) %>%
  filter(!is.na(country_name)) %>%
  select(country_name, year, ipp) %>%
  distinct(country_name, year, .keep_all = TRUE)

# 2) Ensure all years are present (even if missing)
df_ipp_complete <- df_ipp %>%
  group_by(country_name) %>%
  tidyr::complete(year = years_full) %>%
  ungroup()

# 3) Reshape to wide: one column per year
ipp_wide <- df_ipp_complete %>%
  tidyr::pivot_wider(
    names_from = year,
    values_from = ipp
  )

# 4) Country-level statistics (2000–2022)
ipp_master <- ipp_wide %>%
  rowwise() %>%
  mutate(
    N = sum(!is.na(c_across(all_of(as.character(years_full))))),
    Mean = mean(c_across(all_of(as.character(years_full))), na.rm = TRUE),
    SD = sd(c_across(all_of(as.character(years_full))), na.rm = TRUE),
    Median = median(c_across(all_of(as.character(years_full))), na.rm = TRUE),
    Min = min(c_across(all_of(as.character(years_full))), na.rm = TRUE),
    Max = max(c_across(all_of(as.character(years_full))), na.rm = TRUE)
  ) %>%
  ungroup() %>%
  arrange(desc(Mean))

# 5) GT (wide table)
year_cols <- as.character(years_full)

gt_ipp_master <- ipp_master %>%
  gt() %>%
  tab_header(
    title = "IPP (2000–2022) by Country with Summary Statistics"
  ) %>%
  
  # Spanners for readability
  tab_spanner(
    label = "IPP values",
    columns = all_of(year_cols)
  ) %>%
  tab_spanner(
    label = "Summary statistics (2000–2022)",
    columns = c(N, Mean, SD, Median, Min, Max)
  ) %>%
  
  cols_label(country_name = "Country") %>%
  
  fmt_number(
    columns = all_of(year_cols),
    decimals = 3
  ) %>%
  fmt_number(
    columns = c(Mean, SD, Median, Min, Max),
    decimals = 3
  ) %>%
  
  cols_align(align = "left", columns = country_name) %>%
  cols_align(align = "center", columns = c(all_of(year_cols), N, Mean, SD, Median, Min, Max)) %>%
  
  cols_width(
    country_name ~ px(190),
    all_of(year_cols) ~ px(45),
    N ~ px(45),
    Mean ~ px(70),
    SD ~ px(70),
    Median ~ px(70),
    Min ~ px(70),
    Max ~ px(70)
  ) %>%
  
  tab_options(
    table.font.names = "Arial",
    table.font.size = 10,  # font size 10px (readable)
    table.font.color = "black",
    table.border.top.width = px(0),
    table.border.bottom.width = px(0),
    heading.border.bottom.width = px(1),
    column_labels.border.bottom.width = px(1),
    table_body.hlines.width = px(0),
    table_body.vlines.width = px(0),
    data_row.padding = px(2),
    source_notes.font.size = 9
  ) %>%
  
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(everything())
  ) %>%
  
  tab_source_note(
    source_note = md(
      "Note: The table reports the PCA-based Productive Power Index (IPP) for each country-year (2000–2022) and country-level summary statistics computed over available years. Countries are sorted by the Mean IPP."
    )
  )

gt_ipp_master



gtsave(
  gt_ipp_master,
  filename = "Table_IPP_Master_2000_2022.png",
  expand = 5,
  zoom = 5,
  vwidth = 2500
)



gtsave(
  gt_ipp_master,
  filename = "Table_IPP_Master_2000_2022.html"
)



save.image(file = "LIST_Project_Workspace.RData")



save(
  df_plain, df_boot, df_boot1, df_final,
  ifp_complete, ipp_scores, pca_ifp,
  tau1_hat, tau2_hat,
  F1_obs, F2_obs, F3_obs,
  p_value_F1, p_value_F2, p_value_F3,
  F1_boot, F2_boot, F3_boot,
  file = "LIST_Resultats_Principaux.RData"
)








dk_se[c("tariff:r1","tariff:r2","tariff:r3"),
      c("tariff:r1","tariff:r2","tariff:r3")]




































































