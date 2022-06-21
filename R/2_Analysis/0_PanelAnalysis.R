################################################################################
########################        Panel Measures        ##########################
################################################################################

# Panel resources:
# - https://rpubs.com/phle/r_tutorial_panel_data_analysis

library(tidyverse) # 1.3.1
library(countrycode) # 1.3.1
library(fixest) # 0.10.4 Great package for fixed effects estimations :)
library(plm) # 2.6-1 More established package for panel regressions
library(lme4) # 1.1-29
library(lfe) # 2.8-8
library(lmtest) # 0.9.40
library(car) # 3.0.13
library(corrplot) # 0.92 Correlation plots
library(Hmisc) # 4.7.0 Utilities for data science
library(pander) # 0.6.5 For regression tables

# Utilities
data_path <- "data/CleanData/"
eps_path <- "data/CleanData/EPS/"
innovation_path <- "data/CleanData/Innovation/"
controls_path <- "data/CleanData/Controls/"
mergeCols <- c("Year" = "Year", "COU" = "COU")
regressiontablepath <- "Thesis_Final/figures/RegressionTables/"

# Countries:
oecd <- read.csv("data_raw/oecd.csv") %>%
  dplyr::mutate(WB_code = countrycode::countrycode(Code,
                                                   "iso3c",
                                                   "wb"),
                iso2c = countrycode::countrycode(Code,
                                                 "iso3c",
                                                 "iso2c"))


############################ Import Data #######################################

##############################
# Cooperation Network Measures
##############################

######
# FDSM
######

# Strength FDSM:
Strength_FDSM <- readRDS(paste0(data_path, "Centrality/OECD_Panel_Strength_fdsm.rds"))
# Eigenvector FDSM:
Eigenvector_FDSM <- readRDS(paste0(data_path, "Centrality/OECD_Panel_Eigenvector_fdsm.rds"))
# Transitivity FDSM:
Transitivity_FDSM <- readRDS(paste0(data_path, "Centrality/OECD_Panel_Transitivity_fdsm.rds")) %>%
  dplyr::mutate(Year = as.character(Year))

FDSM_joined <- dplyr::inner_join(Strength_FDSM, Eigenvector_FDSM,
                                 by = c("Country" = "Country", "Year" = "Year")) %>%
  dplyr::mutate(COU = Country) %>%
  dplyr::select(-Country) %>%
  dplyr::rename(Strength_FDSM = value.x,
                Eigenvector_FDSM = value.y)

FDSM_joined <- dplyr::inner_join(FDSM_joined, Transitivity_FDSM,
                                 by = c("COU" = "Country", "Year" = "Year")) %>%
  dplyr::mutate(Year = as.character(Year)) %>%
  dplyr::rename(Transitivity_FDSM = value) %>%
  dplyr::relocate(COU, Year, Strength_FDSM, Eigenvector_FDSM, Transitivity_FDSM)

######
# SDSM
######

# Strength SDSM:
Strength_SDSM <- readRDS(paste0(data_path, "Centrality/OECD_Panel_Strength_SDSM.rds"))
# Eigenvector SDSM:
Eigenvector_SDSM <- readRDS(paste0(data_path, "Centrality/OECD_Panel_Eigenvector_SDSM.rds"))
# Transitivity SDSM:
Transitivity_SDSM <- readRDS(paste0(data_path, "Centrality/OECD_Panel_Transitivity_SDSM.rds")) %>%
  dplyr::mutate(Year = as.character(Year))

SDSM_joined <- dplyr::inner_join(Strength_SDSM, Eigenvector_SDSM,
                                 by = c("Country" = "Country", "Year" = "Year")) %>%
  dplyr::mutate(COU = Country) %>%
  dplyr::select(-Country) %>%
  dplyr::rename(Strength_SDSM = value.x,
                Eigenvector_SDSM = value.y)

SDSM_joined <- dplyr::inner_join(SDSM_joined, Transitivity_SDSM,
                                 by = c("COU" = "Country", "Year" = "Year")) %>%
  dplyr::mutate(Year = as.character(Year)) %>%
  dplyr::rename(Transitivity_SDSM = value) %>%
  dplyr::relocate(COU, Year, Strength_SDSM, Eigenvector_SDSM, Transitivity_SDSM)

# Join Centrality measures trhoughout methods

Centrality_joined <- dplyr::inner_join(FDSM_joined, SDSM_joined,
                                 by = c("COU" = "COU", "Year" = "Year")) %>%
  dplyr::relocate(COU, Year, dplyr::ends_with("FDSM"), dplyr::ends_with("SDSM")) %>%
  dplyr::mutate(Year = as.integer(Year)) %>%
  dplyr::filter(Year <= 2015 & Year >= 1985 & COU %in% oecd$Code)

##############################
# Innovation Measures
##############################

# Business expenditures on aggregate R&D 2015 PPP USD
berd <- dplyr::tibble(readRDS(paste0(innovation_path, "BERD_PPP.rds"))) %>%
  dplyr::rename(berd = Value) %>%
  dplyr::select(Year, COU, berd) %>%
  dplyr::filter(Year <= 2015 & Year >= 1985 & COU %in% oecd$Code) %>%
  dplyr::mutate(Year = as.integer(Year))

# Government expenditures on aggregate R&D 2015 PPP USD
goverd <- dplyr::tibble(readRDS(paste0(innovation_path, "GOVERD_controls.rds"))) %>%
  dplyr::select(-COU) %>%
  dplyr::rename(goverd = Value,
                COU = iso3c) %>%
  dplyr::select(Year, COU, goverd) %>%
  dplyr::filter(Year <= 2015 & Year >= 1985 & COU %in% oecd$Code) %>%
  dplyr::mutate(Year = as.integer(Year))

# Total Factor Productivity dataset 2015 PPP USD
tfp <- readRDS(paste0(innovation_path, "tfp012_join.rds")) %>%
  dplyr::filter(nace_r2 == "TOT") %>%
  dplyr::mutate(COU = countrycode::countrycode(COU, "iso2c", "iso3c",
                                               custom_match = c("EL" = "GRC"))) %>%
  dplyr::select(Year, COU, TFP0, TFP1, TFP2) %>%
  dplyr::filter(Year <= 2015 & Year >= 1985 & COU %in% oecd$Code) %>%
  dplyr::mutate(Year = as.integer(Year))

#  patents OECD dataset
# environmental_patents <- readRDS(paste0(innovation_path, "EnvPat.rds")) %>%
#   dplyr::rename(EnvPat = Value,
#                 Country = Inventor.country) %>%
#   dplyr::select(Year, COU, EnvPat) %>%
#   dplyr::filter(Year <= 2015 & Year >= 1985 & COU %in% oecd$Code) %>%
#   dplyr::mutate(Year = as.integer(Year))

environmental_patents <- readRDS(paste0(innovation_path, "EnvPatShare.rds")) %>%
  dplyr::filter(COU %in% oecd$Code) %>%
  dplyr::mutate(Year = as.integer(Year))

# Triadic patent counts
triadic_patent <- readRDS(paste0(innovation_path, "tpf_inventor_country.rds")) %>%
  dplyr::mutate(COU = countrycode::countrycode(COU, "iso2c", "iso3c")) %>%
  dplyr::rename(Year = First_Prio_Year,
                triadic_patent = n) %>%
  dplyr::select(Year, COU, triadic_patent) %>%
  dplyr::filter(Year <= 2015 & Year >= 1985 & COU %in% oecd$Code) %>%
  dplyr::mutate(Year = as.integer(Year))

# Innovation measures joined
innovation <- purrr::reduce(list(berd, goverd, tfp, environmental_patents,
                                 triadic_patent),
                            dplyr::left_join, by = mergeCols)

##############################
# Control Measures
##############################

# World Bank Controls
WB <- dplyr::tibble(readRDS(paste0(controls_path, "WB_controls.rds"))) %>%
  dplyr::mutate(iso2c = countrycode::countrycode(iso2c,
                                                 "iso2c",
                                                 "iso3c")) %>%
  dplyr::rename(Year = year,
                COU = iso2c) %>%
  dplyr::mutate(Year = as.integer(Year)) %>%
  dplyr::select(-country) %>%
  dplyr::filter(Year <= 2015 & Year >= 1985 & COU %in% oecd$Code)

#############
# EPS Measure
#############

EPS <- dplyr::tibble(readRDS(paste0(eps_path, "eps.rds"))) %>%
  dplyr::select(-COU) %>%
  dplyr::rename(EPS = Value,
                COU = iso3c) %>%
  dplyr::select(Year, COU, EPS) %>%
  dplyr::mutate(Year = as.integer(Year)) %>%
  dplyr::filter(Year <= 2015 & Year >= 1985 & COU %in% oecd$Code)

########################
# Everything else joined
########################

all_vars <- innovation %>%
  dplyr::full_join(Centrality_joined, by = mergeCols) %>%
  dplyr::full_join(EPS, by = mergeCols) %>%
  dplyr::full_join(WB, by = mergeCols) %>%
  dplyr::select(-iso2c.x, -iso2c.y)

# Creating lags for policy variables. See theoretical explanation below.

all_vars <- all_vars %>%
  dplyr::mutate(
    # No lags just logs
    BERD_log = log(berd),
    Triadic_Patents_log = log(triadic_patent),
    EPS_log = log(EPS),
    Strength_FDSM_log = log(Strength_FDSM),
    Transitivity_FDSM_log = log(Transitivity_FDSM + 1),
    Strength_SDSM_log = log(Strength_SDSM),
    Transitivity_SDSM_log = log(Transitivity_FDSM + 1),
    GDPCapita_log = log(GDPCapita),
    ExportIntensity_log = log(ExportIntensity),
    ImportIntensity_log = log(ImportIntensity),
    # 1 Year lags
    GDPCapita_log_lag1 = log(Hmisc::Lag(GDPCapita, 1)),
    ExportIntensity_log_lag1 = log(Hmisc::Lag(ExportIntensity, 1)),
    ImportIntensity_log_lag1 = log(Hmisc::Lag(ImportIntensity, 1)),
    EPS_log_lag1 = log(Hmisc::Lag(EPS, 1)),
    Strength_FDSM_log_lag1 = log(Hmisc::Lag(Strength_FDSM, 1)),
    Transitivity_FDSM_log_lag1 = log(Hmisc::Lag(Transitivity_FDSM + 1, 1)),
    Strength_SDSM_log_lag1 = log(Hmisc::Lag(Strength_SDSM, 1)),
    Transitivity_SDSM_log_lag1 = log(Hmisc::Lag(Transitivity_SDSM + 1, 1)),
    # 5 Year lags
    EPS_log_lag5 = log(Hmisc::Lag(EPS, 5)),
    Strength_FDSM_log_lag5 = log(Hmisc::Lag(Strength_FDSM, 5)),
    Transitivity_FDSM_log_lag5 = log(Hmisc::Lag(Transitivity_FDSM + 1, 5)),
    Strength_SDSM_log_lag5 = log(Hmisc::Lag(Strength_SDSM, 5)),
    Transitivity_SDSM_log_lag5 = log(Hmisc::Lag(Transitivity_SDSM + 1, 5))) %>%
  dplyr::mutate(dplyr::across(where(is.numeric), ~na_if(., NaN))) %>%
  dplyr::mutate(dplyr::across(where(is.numeric), ~na_if(., -Inf))) %>%
  dplyr::filter(Year <= 2015 & Year >= 1990) %>%
  dplyr::rename(Country = COU)

# Save full panel

saveRDS(all_vars, file = paste0(data_path, "Panel.rds"))

############################ Panel Analysis ####################################

# Set panel dimensions for {fixest} package and other fixest parameters.
# !!!!!! RUN BEFORE ESTIMATONS !!!!!!!!
fixest::setFixest_estimation(panel.id = ~Country + Year)
fixest::setFixest_dict(c(BERD_log = "R&D",
                         EnvPatShare = "Environmental Patents",
                         Triadic_Patents_log = "Patents",
                 EPS_log_lag1 = "ln EPS (t-1)",
                 EPS_log_lag5 = "ln EPS (t-5)",
                 Strength_FDSM_log_lag1 = "ln Strength (t-1)",
                 Strength_FDSM_log_lag5 = "ln Strength (t-5)",
                 Transitivity_FDSM_log_lag1 = "ln Transitivity (t-1)",
                 Transitivity_FDSM_log_lag5 = "ln Transitivity (t-5)",
                 Strength_SDSM_log_lag1 = "ln Strength (t-1)",
                 Strength_SDSM_log_lag5 = "ln Strength (t-5)",
                 Transitivity_SDSM_log_lag1 = "ln Transitivity (t-1)",
                 Transitivity_SDSM_log_lag5 = "ln Transitivity (t-5)",
                 GDPCapita_log = "GDP per cap.",
                 ExportIntensity_log = "Export Intensity",
                 ImportIntensity_log = "Import Intensity",
                 GDPCapita_log_lag1 = "GDP per cap. (t-1)",
                 ExportIntensity_log_lag1 = "Export Intensity (t-1)",
                 ImportIntensity_log_lag1 = "Import Intensity (t-1)"))
my_style = fixest::style.df(depvar.title = "", fixef.title = "",
)
setFixest_etable(style.df = my_style,
                 postprocess.df = pander::pandoc.table.return,
                 style.tex = style.tex(main = "aer"))

# Step 1: Is our panel balanced ? Yes. There are missing values though.

# all_vars %>%
#   select(Year, Country) %>%
#   table()
# plm::is.pbalanced(all_vars) # Actually False for FE models

# Step 2: Correlations
correlations <- all_vars %>%
  dplyr::select(
    BERD_log, TFP0,
    EnvPatShare, EnvPatShare,
    EPS_log_lag1, EPS_log_lag5,
    Transitivity_FDSM_log_lag1, Transitivity_FDSM_log_lag5,
    Transitivity_SDSM_log_lag1, Transitivity_SDSM_log_lag5,
    Strength_FDSM_log_lag1, Strength_FDSM_log_lag5,
    Strength_SDSM_log_lag1, Strength_SDSM_log_lag5) %>%
  cor(use = "complete.obs")

correlations %>%
  corrplot::corrplot(type = "lower", order = "original",
                     tl.col = "black", tl.srt = 45)


# A quick note on lags: we will use respectively 1 and 5 year lags of policy
# stringency measures and the international environmental indexes. This follows
# Lankoski, 2010, Martinez-Zarzoso et al. 2015 and is mentionned in the
# literature review by Lanoie et al. 2020
# https://www.journals.uchicago.edu/doi/full/10.1093/reep/res016
# This is mainly due to the fact that innovation takes time and that it arguably
# does not make theoretical sense to compare contemporaneous policy stringency
# changes and innovation.

# A quick note on Standard Errors, we follow Martinez Zarosso et al. 2019 by
# applying the Newey-West SE corrections.

# Weak Porter Hypothesis:
# Increased Environmental Regulations are linked with increased rates of
# innovation due to the factors below (or a combination of them).

# Proxies for innovation:

#   - Business expenditures on R&D: BERD_log

######
# POLS
######

# LAG 1: #######################################################################

fml_FDSM_BERD_lag1 <- formula(BERD_log ~ EPS_log_lag1 +
                                Strength_FDSM_log_lag1 + Transitivity_FDSM_log_lag1 +
                                log(GDPCapita) +
                                log(ExportIntensity) +
                                log(ImportIntensity))
pols_berd_fdsm_lm_lag1 <- lm(fml_FDSM_BERD_lag1,
                        data = all_vars)
summary(pols_berd_fdsm_lm_lag1)
pols_berd_fdsm_plm_lag1 <- plm::plm(fml_FDSM_BERD_lag1,
                               data = all_vars,
                               index = c("Country", "Year"),
                               model = "pooling")
summary(pols_berd_fdsm_plm_lag1)
# SDSM
fml_SDSM_BERD_lag1 <- formula(BERD_log ~ EPS_log_lag1 +
                                Strength_SDSM_log_lag1 + Transitivity_SDSM_log_lag1 +
                                log(GDPCapita) +
                                log(ExportIntensity) +
                                log(ImportIntensity))
pols_berd_SDSM_lm_lag1 <- lm(fml_SDSM_BERD_lag1,
                        data = all_vars)
summary(pols_berd_SDSM_lm_lag1)
pols_berd_SDSM_plm_lag1 <- plm::plm(fml_SDSM_BERD_lag1,
                               data = all_vars,
                               index = c("Country", "Year"),
                               model = "pooling")
summary(pols_berd_SDSM_plm_lag1)

# Main observations:
#  POLS regression of first lags is highly significant for lagged EPS in
#  the first case. (1.14923, 1.039674). Marginally significant (9%) for Strength_FDSM_log_lag1
#  with a negative coefficient (-0.14814). Confirms Weak Porter Hypothesis.

# Pooling Model for FDSM model
#
# Call:
#   plm::plm(formula = fml_FDSM_BERD_POLS, data = all_vars, model = "pooling",
#            index = c("Country", "Year"))
#
# Unbalanced Panel: n = 29, T = 1-25, N = 551
#
# Residuals:
#   Min.  1st Qu.   Median  3rd Qu.     Max.
# -3.88416 -0.69253  0.13739  0.84370  2.44921
#
# Coefficients:
#   Estimate Std. Error t-value  Pr(>|t|)
#   (Intercept)                 8.27437    1.25537  6.5912 1.033e-10 ***
#   EPS_log_lag1                1.14923    0.13854  8.2955 8.550e-16 ***
#   Strength_FDSM_log_lag1     -0.14814    0.08879 -1.6685   0.09579 .
#   Transitivity_FDSM_log_lag1  0.54822    0.46097  1.1893   0.23484
#   log(GDPCapita)              1.21558    0.10681 11.3810 < 2.2e-16 ***
#   log(ExportIntensity)       -1.36432    0.33916 -4.0226 6.570e-05 ***
#   log(ImportIntensity)       -0.18038    0.37924 -0.4756   0.63453
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# Total Sum of Squares:    1694.4
# Residual Sum of Squares: 758.78
# R-Squared:      0.55219
# Adj. R-Squared: 0.54725
# F-statistic: 111.801 on 6 and 544 DF, p-value: < 2.22e-16

#  Pooling Model for SDSM model

# Call:
#   plm::plm(formula = fml_SDSM_BERD_lag1, data = all_vars, model = "pooling",
#            index = c("Country", "Year"))
#
# Unbalanced Panel: n = 28, T = 1-23, N = 466
#
# Residuals:
#   Min.  1st Qu.   Median  3rd Qu.     Max.
# -3.73506 -0.71698  0.11078  0.78302  2.44106
#
# Coefficients:
#   Estimate Std. Error t-value  Pr(>|t|)
# (Intercept)                10.068664   1.310464  7.6833 9.471e-14 ***
#   EPS_log_lag1                1.041457   0.151900  6.8562 2.291e-11 ***
#   Strength_SDSM_log_lag1      0.031629   0.086070  0.3675   0.71343
# Transitivity_SDSM_log_lag1  0.565495   0.431192  1.3115   0.19036
# log(GDPCapita)              0.908485   0.119933  7.5750 1.996e-13 ***
#   log(ExportIntensity)       -0.709302   0.380975 -1.8618   0.06327 .
# log(ImportIntensity)       -0.885168   0.423552 -2.0899   0.03718 *
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# Total Sum of Squares:    1175.5
# Residual Sum of Squares: 601.95
# R-Squared:      0.48794
# Adj. R-Squared: 0.48124
# F-statistic: 72.8959 on 6 and 459 DF, p-value: < 2.22e-16

# LAG 5: #######################################################################

# FDSM
fml_FDSM_BERD_lag5 <- formula(BERD_log ~ EPS_log_lag5 +
                                Strength_FDSM_log_lag5 + Transitivity_FDSM_log_lag5 +
                                log(GDPCapita) +
                                log(ExportIntensity) +
                                log(ImportIntensity))
pols_berd_fdsm_lm_lag5_lag1 <- lm(fml_FDSM_BERD_lag5,
                         data = all_vars)
summary(pols_berd_fdsm_lm_lag5_lag1)
pols_berd_fdsm_plm_lag5 <- plm::plm(fml_FDSM_BERD_lag5,
                        data = all_vars,
                        index = c("Country", "Year"),
                        model = "pooling")
summary(pols_berd_fdsm_plm_lag5)
# SDSM
fml_SDSM_BERD_lag5 <- formula(BERD_log ~ EPS_log_lag5 +
                           Strength_SDSM_log_lag5 + Transitivity_SDSM_log_lag5 +
                           log(GDPCapita) +
                           log(ExportIntensity) +
                           log(ImportIntensity))
pols_berd_SDSM_lm_lag5 <- lm(fml_SDSM_BERD_lag5,
                        data = all_vars,
                        na.action = na.exclude)
summary(pols_berd_SDSM_lm_lag5)
pols_berd_SDSM_plm_lag5 <- plm::plm(fml_SDSM_BERD_lag5,
                               data = all_vars,
                               index = c("Country", "Year"),
                               model = "pooling")
summary(pols_berd_SDSM_plm_lag5)

# Main observations:
#  POLS regression of fifth lags is highly significant for both lagged EPS
#  albeit a little noisier for SDSM which leads to a sig level at 1%.
#  (0.508861, 0.499037). Nothing is significant for all other policy coefficients
#  that are also lagged to the fifth.

# Pooling Model for FDSM model
#
# Call:
#   plm::plm(formula = fml_FDSM_BERD_POLS, data = all_vars, model = "pooling",
#            index = c("Country", "Year"))
#
# Unbalanced Panel: n = 31, T = 1-25, N = 513
#
# Residuals:
#   Min.  1st Qu.   Median  3rd Qu.     Max.
# -4.63482 -0.70777  0.20073  0.86175  2.44905
#
# Coefficients:
#   Estimate Std. Error t-value  Pr(>|t|)
#   (Intercept)                 5.726856   1.219527  4.6960 3.422e-06 ***
#   EPS_log_lag5                0.508861   0.139865  3.6382 0.0003027 ***
#   Strength_FDSM_log_lag5     -0.062882   0.090060 -0.6982 0.4853594
#   Transitivity_FDSM_log_lag5  0.057575   0.484986  0.1187 0.9055489
#   log(GDPCapita)              1.488104   0.101198 14.7049 < 2.2e-16 ***
#   log(ExportIntensity)       -1.315555   0.394660 -3.3334 0.0009208 ***
#   log(ImportIntensity)       -0.099043   0.440433 -0.2249 0.8221670
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# Total Sum of Squares:    1587.6
# Residual Sum of Squares: 799.47
# R-Squared:      0.49644
# Adj. R-Squared: 0.49047
# F-statistic: 83.1403 on 6 and 506 DF, p-value: < 2.22e-16

#  Pooling Model for SDSM model
#
# Call:
#   plm::plm(formula = fml_SDSM_BERD_POLS, data = all_vars, model = "pooling",
#            index = c("Country", "Year"))
#
# Unbalanced Panel: n = 29, T = 1-21, N = 422
#
# Residuals:
#   Min.  1st Qu.   Median  3rd Qu.     Max.
# -4.85037 -0.68360  0.16476  0.81805  2.30542
#
# Coefficients:
#   Estimate Std. Error t-value  Pr(>|t|)
#   (Intercept)                 8.580568   1.421925  6.0345 3.537e-09 ***
#   EPS_log_lag5                0.499037   0.164305  3.0373  0.002538 **
#   Strength_SDSM_log_lag5     -0.027599   0.094213 -0.2929  0.769714
#   Transitivity_SDSM_log_lag5  0.122748   0.481107  0.2551  0.798745
#   log(GDPCapita)              1.182531   0.121506  9.7323 < 2.2e-16 ***
#   log(ExportIntensity)       -0.263814   0.481033 -0.5484  0.583690
#   log(ImportIntensity)       -1.110363   0.530188 -2.0943  0.036841 *
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# Total Sum of Squares:    1117.4
# Residual Sum of Squares: 655.75
# R-Squared:      0.41315
# Adj. R-Squared: 0.40467
# F-statistic: 48.6947 on 6 and 415 DF, p-value: < 2.22e-16


########################
# Fixed effects: Country
########################

# LAG 1: #######################################################################

# FDSM
fe_berd_fdsm_lag1_Country <- fixest::feols(fml = BERD_log ~ EPS_log_lag1 +
                                            Strength_FDSM_log_lag1 +
                                             Transitivity_FDSM_log_lag1 +
                                            log(GDPCapita) +
                                            log(ExportIntensity) +
                                            log(ImportIntensity) | Country,
                                          data = all_vars,
                                          vcov = "NW")
fe_berd_fdsm_lag1_Country

# SDSM
fe_berd_sdsm_lag1_Country <- fixest::feols(fml = BERD_log ~ EPS_log_lag1 +
                                            Strength_SDSM_log_lag1 +
                                            Transitivity_SDSM_log_lag1 +
                                            log(GDPCapita) +
                                            log(ExportIntensity) +
                                            log(ImportIntensity) | Country,
                                          data = all_vars,
                                          vcov = "NW")
fe_berd_sdsm_lag1_Country

# LAG 5: #######################################################################

# FDSM
fe_berd_fdsm_lag5_Country <- fixest::feols(fml = BERD_log ~ EPS_log_lag5 +
                                     Strength_FDSM_log_lag5 + Transitivity_FDSM_log_lag5 +
                                     log(GDPCapita) +
                                     log(ExportIntensity) +
                                     log(ImportIntensity) | Country,
                                   data = all_vars,
                                   vcov = "NW")
fe_berd_fdsm_lag5_Country
# SDSM
fe_berd_sdsm_lag5_Country <- fixest::feols(fml = BERD_log ~ EPS_log_lag5 +
                                     Strength_SDSM_log_lag5 +
                                     Transitivity_SDSM_log_lag5 +
                                     log(GDPCapita) +
                                     log(ExportIntensity) +
                                     log(ImportIntensity) | Country,
                                   data = all_vars,
                                   vcov = "NW")
fe_berd_sdsm_lag5_Country


#################################
# Fixed effects: Country and Year
#################################

# LAG 1: #######################################################################

# FDSM
fe_berd_fdsm_lag1_Country_Time <- fixest::feols(fml = BERD_log ~ EPS_log_lag1 +
                                Strength_FDSM_log_lag1 + Transitivity_FDSM_log_lag1 +
                                log(GDPCapita) +
                                log(ExportIntensity) +
                                log(ImportIntensity) | Year + Country,
                              data = all_vars,
                              vcov = "NW")
fe_berd_fdsm_lag1_Country_Time
# fe_berd_fdsm_plm <- plm::plm(formula = BERD_log ~ EPS_log_lag1 +
#                                 Strength_FDSM_log_lag1 + Transitivity_FDSM_log_lag1 +
#                                 log(GDPCapita) +
#                                 log(ExportIntensity) +
#                                 log(ImportIntensity),
#                              data = all_vars,
#                              index = c("Country", "Year"),
#                              effect = "twoway",
#                              model = "within")
# summary(fe_berd_fdsm_plm) --> Equivalency check passed
# SDSM
fe_berd_sdsm_lag1_Country_Time <- fixest::feols(fml = BERD_log ~ EPS_log_lag1 +
                                Strength_SDSM_log_lag1 + Transitivity_SDSM_log_lag1 +
                                log(GDPCapita) +
                                log(ExportIntensity) +
                                log(ImportIntensity) | Year + Country,
                              data = all_vars,
                              vcov = "NW")
fe_berd_sdsm_lag1_Country_Time
# fe_berd_sdsm_plm <- fixest::feols(fml = BERD_log ~ EPS_log_lag1 +
#                                 Strength_SDSM_log_lag1 + Transitivity_SDSM_log_lag1 +
#                                 log(GDPCapita) +
#                                 log(ExportIntensity) +
#                                 log(ImportIntensity) | Year + Country,
#                               data = all_vars)
# summary(fe_berd_sdsm_plm) --> Equivalency check passed

# LAG 5: #######################################################################

# FDSM
fe_berd_fdsm_lag5_Country_Time <- fixest::feols(fml = BERD_log ~ EPS_log_lag5 +
                                Strength_FDSM_log_lag5 + Transitivity_FDSM_log_lag5 +
                                log(GDPCapita) +
                                log(ExportIntensity) +
                                log(ImportIntensity) | Year + Country,
                         data = all_vars,
                         vcov = "NW")
fe_berd_fdsm_lag5_Country_Time
# SDSM
fe_berd_sdsm_lag5_Country_Time <- fixest::feols(fml = BERD_log ~ EPS_log_lag5 +
                                Strength_SDSM_log_lag5 +
                                Transitivity_SDSM_log_lag5 +
                                log(GDPCapita) +
                                log(ExportIntensity) +
                                log(ImportIntensity) | Year + Country,
                      data = all_vars,
                      vcov = "NW")
fe_berd_sdsm_lag5_Country_Time

# Robustness Checks: Are time dummies required? -> Joint F-test. Yes if rejected.

# fixest::fitstat(fe_berd_sdsm_lag5_Country_Time, type = "wald")

# Exporting regression tables:
# Set labels for estimated varibles
regression_table_berd_sdsm <- fixest::etable(fe_berd_sdsm_lag1_Country_Time,
                                        fe_berd_sdsm_lag5_Country_Time,
                                        digits = 2,
                                        digits.stats = 4,
                                        signif.code = c("***" = 0.01,
                                                        "**" = 0.05,
                                                        "*" = 0.10),
                                        order = c("EPS",
                                                  "Strength",
                                                  "Transitivity"),
                                        tex = TRUE)

regression_table_berd_fdsm <- fixest::etable(fe_berd_fdsm_lag1_Country_Time,
                                             fe_berd_fdsm_lag5_Country_Time,
                                             digits = 2,
                                             digits.stats = 4,
                                             signif.code = c("***" = 0.01,
                                                             "**" = 0.05,
                                                             "*" = 0.10),
                                             order = c("EPS",
                                                       "Strength",
                                                       "Transitivity"),
                                             tex = TRUE)

saveRDS(regression_table_berd_sdsm, file = paste0(regressiontablepath, "BerdSDSM.rds"))
saveRDS(regression_table_berd_fdsm, file = paste0(regressiontablepath, "BerdFDSM.rds"))

#   - Triadic Patent counts: triadic_patent ----

# A few notes before I begin. Martinez Zaroso et al 2019 use a normal panel
# and log the dependent variable of triadic patent counts since it is
# right tailed (Jap US and GER have a lot of patents while smaller countries
# do have little). I considered using a Poisson model here since it is right
# tailed but we don't have a "true zero inflation" since all countries have
# at least *some* innovations they patented in all three countries.
#
# Second note is related to the fact that patent counts, especially triadic
# patents counts have the drawback of being contingent on patent law changes,
# accountancy criteria of patents etc. See Matrinez and Zaroso as well as
# the literature review by Popp (2019).

######
# POLS
######

# LAG 1: #######################################################################

fml_FDSM_TPF_lag1 <- formula(Triadic_Patents_log ~ EPS_log_lag1 +
                                Strength_FDSM_log_lag1 + Transitivity_FDSM_log_lag1 +
                                log(GDPCapita) +
                                log(ExportIntensity) +
                                log(ImportIntensity))
pols_TPF_fdsm_lm_lag1 <- lm(fml_FDSM_TPF_lag1,
                             data = all_vars)
summary(pols_TPF_fdsm_lm_lag1)
pols_TPF_fdsm_plm_lag1 <- plm::plm(fml_FDSM_TPF_lag1,
                                    data = all_vars,
                                    index = c("Country", "Year"),
                                    model = "pooling")
summary(pols_TPF_fdsm_plm_lag1)
# SDSM
fml_SDSM_TPF_lag1 <- formula(Triadic_Patents_log ~ EPS_log_lag1 +
                                Strength_SDSM_log_lag1 + Transitivity_SDSM_log_lag1 +
                                log(GDPCapita) +
                                log(ExportIntensity) +
                                log(ImportIntensity))
pols_TPF_SDSM_lm_lag1 <- lm(fml_SDSM_TPF_lag1,
                             data = all_vars)
summary(pols_TPF_SDSM_lm_lag1)
pols_TPF_SDSM_plm_lag1 <- plm::plm(fml_SDSM_TPF_lag1,
                                    data = all_vars,
                                    index = c("Country", "Year"),
                                    model = "pooling")
summary(pols_TPF_SDSM_plm_lag1)

# LAG 5: #######################################################################

# FDSM
fml_FDSM_TPF_lag5 <- formula(Triadic_Patents_log ~ EPS_log_lag5 +
                                Strength_FDSM_log_lag5 + Transitivity_FDSM_log_lag5 +
                                log(GDPCapita) +
                                log(ExportIntensity) +
                                log(ImportIntensity))
pols_TPF_fdsm_lm_lag5_lag1 <- lm(fml_FDSM_TPF_lag5,
                                  data = all_vars)
summary(pols_TPF_fdsm_lm_lag5_lag1)
pols_TPF_fdsm_plm_lag5 <- plm::plm(fml_FDSM_TPF_lag5,
                                    data = all_vars,
                                    index = c("Country", "Year"),
                                    model = "pooling")
summary(pols_TPF_fdsm_plm_lag5)
# SDSM
fml_SDSM_TPF_lag5 <- formula(Triadic_Patents_log ~ EPS_log_lag5 +
                                Strength_SDSM_log_lag5 + Transitivity_SDSM_log_lag5 +
                                log(GDPCapita) +
                                log(ExportIntensity) +
                                log(ImportIntensity))
pols_TPF_SDSM_lm_lag5 <- lm(fml_SDSM_TPF_lag5,
                             data = all_vars,
                             na.action = na.exclude)
summary(pols_TPF_SDSM_lm_lag5)
pols_TPF_SDSM_plm_lag5 <- plm::plm(fml_SDSM_TPF_lag5,
                                    data = all_vars,
                                    index = c("Country", "Year"),
                                    model = "pooling")
summary(pols_TPF_SDSM_plm_lag5)

########################
# Fixed effects: Country
########################

# LAG 1: #######################################################################

# FDSM
fe_TPF_fdsm_lag1_Country <- fixest::feols(fml = Triadic_Patents_log ~ EPS_log_lag1 +
                                             Strength_FDSM_log_lag1 +
                                             Transitivity_FDSM_log_lag1 +
                                             log(GDPCapita) +
                                             log(ExportIntensity) +
                                             log(ImportIntensity) | Country,
                                           data = all_vars,
                                           vcov = "NW")
fe_TPF_fdsm_lag1_Country

# SDSM
fe_TPF_sdsm_lag1_Country <- fixest::feols(fml = Triadic_Patents_log ~ EPS_log_lag1 +
                                             Strength_SDSM_log_lag1 +
                                             Transitivity_SDSM_log_lag1 +
                                             log(GDPCapita) +
                                             log(ExportIntensity) +
                                             log(ImportIntensity) | Country,
                                           data = all_vars,
                                           vcov = "NW")
fe_TPF_sdsm_lag1_Country

# LAG 5: #######################################################################

# FDSM
fe_TPF_fdsm_lag5_Country <- fixest::feols(fml = Triadic_Patents_log ~ EPS_log_lag5 +
                                             Strength_FDSM_log_lag5 + Transitivity_FDSM_log_lag5 +
                                             log(GDPCapita) +
                                             log(ExportIntensity) +
                                             log(ImportIntensity) | Country,
                                           data = all_vars,
                                           vcov = "NW")
fe_TPF_fdsm_lag5_Country
# SDSM
fe_TPF_sdsm_lag5_Country <- fixest::feols(fml = Triadic_Patents_log ~ EPS_log_lag5 +
                                             Strength_SDSM_log_lag5 +
                                             Transitivity_SDSM_log_lag5 +
                                             log(GDPCapita) +
                                             log(ExportIntensity) +
                                             log(ImportIntensity) | Country,
                                           data = all_vars,
                                           vcov = "NW")
fe_TPF_sdsm_lag5_Country


#################################
# Fixed effects: Country and Year
#################################

# LAG 1: #######################################################################

# FDSM
fe_TPF_fdsm_lag1_Country_Time <- fixest::feols(fml = Triadic_Patents_log ~ EPS_log_lag1 +
                                                  Strength_FDSM_log_lag1 +
                                                  Transitivity_FDSM_log_lag1 +
                                                  log(GDPCapita) +
                                                  log(ExportIntensity) +
                                                  log(ImportIntensity) | Year + Country,
                                                data = all_vars,
                                                vcov = "NW")
fe_TPF_fdsm_lag1_Country_Time
# fe_TPF_fdsm_plm <- plm::plm(formula = EnvPatShare ~ EPS_log_lag1 +
#                                 Strength_FDSM_log_lag1 + Transitivity_FDSM_log_lag1 +
#                                 log(GDPCapita) +
#                                 log(ExportIntensity) +
#                                 log(ImportIntensity),
#                              data = all_vars,
#                              index = c("Country", "Year"),
#                              effect = "twoway",
#                              model = "within")
# summary(fe_TPF_fdsm_plm) --> Equivalency check passed
# SDSM
fe_TPF_sdsm_lag1_Country_Time <- fixest::feols(fml = Triadic_Patents_log ~ EPS_log_lag1 +
                                                  Strength_SDSM_log_lag1 +
                                                  Transitivity_SDSM_log_lag1 +
                                                  log(GDPCapita) +
                                                  log(ExportIntensity) +
                                                  log(ImportIntensity) | Year + Country,
                                                data = all_vars,
                                                vcov = "NW")
fe_TPF_sdsm_lag1_Country_Time
# fe_TPF_sdsm_plm <- fixest::feols(fml = EnvPatShare ~ EPS_log_lag1 +
#                                 Strength_SDSM_log_lag1 + Transitivity_SDSM_log_lag1 +
#                                 log(GDPCapita) +
#                                 log(ExportIntensity) +
#                                 log(ImportIntensity) | Year + Country,
#                               data = all_vars)
# summary(fe_TPF_sdsm_plm) --> Equivalency check passed

# LAG 5: #######################################################################

# FDSM
# MZ estimate the relationship on a restricted sample between 1996-2011
# We find no effect of all three policy variables when restricting our observations.
# fe_TPF_fdsm_lag5_Country_Time <- fixest::feols(fml = Triadic_Patents_log ~ EPS_log_lag5 +
#                                                   Strength_FDSM_log_lag5 +
#                                                   Transitivity_FDSM_log_lag5 +
#                                                   log(GDPCapita) +
#                                                   log(ExportIntensity) +
#                                                   log(ImportIntensity) | Year + Country,
#                                                 panel.id = ~Country+Year,
#                                                 data = all_vars[all_vars$Year < 2011 & all_vars$Year > 1996 ,],
#                                                 vcov = "NW")
fe_TPF_fdsm_lag5_Country_Time <- fixest::feols(fml = Triadic_Patents_log ~ EPS_log_lag5 +
                                                 Strength_FDSM_log_lag5 +
                                                 Transitivity_FDSM_log_lag5 +
                                                 log(GDPCapita) +
                                                 log(ExportIntensity) +
                                                 log(ImportIntensity) | Year + Country,
                                               panel.id = ~Country+Year,
                                               data = all_vars,
                                               vcov = "NW")
fe_TPF_fdsm_lag5_Country_Time
# SDSM
# # MZ estimate the relationship on a restricted sample between 1996-2011
# We find no effect of all three policy variables when restricting our observations.
# fe_TPF_sdsm_lag5_Country_Time <- fixest::feols(fml = Triadic_Patents_log ~ EPS_log_lag5 +
#                                                   Strength_SDSM_log_lag5 +
#                                                   Transitivity_SDSM_log_lag5 +
#                                                   log(GDPCapita) +
#                                                   log(ExportIntensity) +
#                                                   log(ImportIntensity) | Year + Country,
#                                                 data = all_vars[all_vars$Year < 2011 & all_vars$Year > 1996 ,],
#                                                 panel.id = ~Country+Year,
#                                                 vcov = "NW")
fe_TPF_sdsm_lag5_Country_Time <- fixest::feols(fml = Triadic_Patents_log ~ EPS_log_lag5 +
                                                 Strength_SDSM_log_lag5 +
                                                 Transitivity_SDSM_log_lag5 +
                                                 log(GDPCapita) +
                                                 log(ExportIntensity) +
                                                 log(ImportIntensity) | Year + Country,
                                               data = all_vars,
                                               panel.id = ~Country+Year,
                                               vcov = "NW")
fe_TPF_sdsm_lag5_Country_Time
# Exporting regression tables:
regression_table_TPF_sdsm <- fixest::etable(fe_TPF_sdsm_lag1_Country_Time,
                                             fe_TPF_sdsm_lag5_Country_Time,
                                             digits = 2,
                                             digits.stats = 4,
                                             signif.code = c("***" = 0.01,
                                                             "**" = 0.05,
                                                             "*" = 0.10),
                                             order = c("EPS",
                                                       "Strength",
                                                       "Transitivity"),
                                             tex = TRUE)

regression_table_TPF_fdsm <- fixest::etable(fe_TPF_fdsm_lag1_Country_Time,
                                             fe_TPF_fdsm_lag5_Country_Time,
                                             digits = 2,
                                             digits.stats = 4,
                                             signif.code = c("***" = 0.01,
                                                             "**" = 0.05,
                                                             "*" = 0.10),
                                             order = c("EPS",
                                                       "Strength",
                                                       "Transitivity"),
                                             tex = TRUE)

saveRDS(regression_table_TPF_sdsm, file = paste0(regressiontablepath, "TPF_SDSM.rds"))
saveRDS(regression_table_TPF_fdsm, file = paste0(regressiontablepath, "TPF_FDSM.rds"))


#   - Environmental Patent counts: EnvPatShare ----

# library(pglm) # 0.2-3

########################
# Fixed effects: Country
########################

# LAG 1: #######################################################################

# FDSM
fe_EnvPat_fdsm_lag1_Country <- fixest::feglm(fml = EnvPatShare ~ EPS_log_lag1 +
                                            Strength_FDSM_log_lag1 +
                                            Transitivity_FDSM_log_lag1 +
                                            log(GDPCapita) +
                                            log(ExportIntensity) +
                                            log(ImportIntensity) | Country,
                                            family = "quasibinomial",
                                          data = all_vars,
                                          vcov = "NW")
fe_EnvPat_fdsm_lag1_Country

# SDSM
fe_EnvPat_sdsm_lag1_Country <- fixest::feglm(fml = EnvPatShare ~ EPS_log_lag1 +
                                            Strength_SDSM_log_lag1 +
                                            Transitivity_SDSM_log_lag1 +
                                            log(GDPCapita) +
                                            log(ExportIntensity) +
                                            log(ImportIntensity) | Country,
                                          data = all_vars,
                                          family = "quasibinomial",
                                          vcov = "NW")
fe_EnvPat_sdsm_lag1_Country

# LAG 5: #######################################################################

# FDSM
fe_EnvPat_fdsm_lag5_Country <- fixest::feglm(fml = EnvPatShare ~ EPS_log_lag5 +
                                            Strength_FDSM_log_lag5 +
                                            Transitivity_FDSM_log_lag5 +
                                            log(GDPCapita) +
                                            log(ExportIntensity) +
                                            log(ImportIntensity) | Country,
                                          data = all_vars,
                                          family = "quasibinomial",
                                          vcov = "NW")
fe_EnvPat_fdsm_lag5_Country
# SDSM
fe_EnvPat_sdsm_lag5_Country <- fixest::feglm(fml = EnvPatShare ~ EPS_log_lag5 +
                                            Strength_SDSM_log_lag5 +
                                            Transitivity_SDSM_log_lag5 +
                                            log(GDPCapita) +
                                            log(ExportIntensity) +
                                            log(ImportIntensity) | Country,
                                          data = all_vars,
                                          family = "quasibinomial",
                                          vcov = "NW")
fe_EnvPat_sdsm_lag5_Country


#################################
# Fixed effects: Country and Year
#################################

# LAG 1: #######################################################################

# FDSM
fe_EnvPat_fdsm_lag1_Country_Time <- fixest::feglm(fml = EnvPatShare ~ EPS_log_lag1 +
                                                 Strength_FDSM_log_lag1 +
                                                 Transitivity_FDSM_log_lag1 +
                                                log(GDPCapita) +
                                                log(ExportIntensity) +
                                                log(ImportIntensity),
                                               data = all_vars,
                                               family = "quasibinomial",
                                               vcov = "NW")
fe_EnvPat_fdsm_lag1_Country_Time
# SDSM
fe_EnvPat_sdsm_lag1_Country_Time <- fixest::feglm(fml = EnvPatShare ~ EPS_log_lag1 +
                                                 Strength_SDSM_log_lag1 +
                                                 Transitivity_SDSM_log_lag1 +
                                                 log(GDPCapita) +
                                                 log(ExportIntensity) +
                                                 log(ImportIntensity) | Year + Country,
                                               data = all_vars,
                                               family = "quasibinomial",
                                               vcov = "NW")
fe_EnvPat_sdsm_lag1_Country_Time

# LAG 5: #######################################################################

# FDSM
fe_EnvPat_fdsm_lag5_Country_Time <- fixest::feglm(fml = EnvPatShare ~ EPS_log_lag5 +
                                                 Strength_FDSM_log_lag5 +
                                                 Transitivity_FDSM_log_lag5 +
                                                   log(GDPCapita) +
                                                   log(ExportIntensity) +
                                                   log(ImportIntensity) | Year + Country,
                                               data = all_vars,
                                               family = "quasibinomial",
                                               vcov = "NW")
fe_EnvPat_fdsm_lag5_Country_Time
# SDSM
fe_EnvPat_sdsm_lag5_Country_Time <- fixest::feglm(fml = EnvPatShare ~ EPS_log_lag5 +
                                                    Strength_SDSM_log_lag5 +
                                                    Transitivity_SDSM_log_lag5 +
                                                    log(GDPCapita) +
                                                    log(ExportIntensity) +
                                                    log(ImportIntensity) | Year +
                                                    Country,
                                               data = all_vars,
                                               family = "quasibinomial",
                                               vcov = "NW")
fe_EnvPat_sdsm_lag5_Country_Time
# Exporting regression tables:
# Set labels for estimated varibles
regression_table_EnvPat_sdsm <- fixest::etable(fe_EnvPat_sdsm_lag1_Country_Time,
                                            fe_EnvPat_sdsm_lag5_Country_Time,
                                            digits = 2,
                                            digits.stats = 4,
                                            signif.code = c("***" = 0.01,
                                                            "**" = 0.05,
                                                            "*" = 0.10),
                                            order = c("EPS",
                                                      "Strength",
                                                      "Transitivity"),
                                            tex = TRUE)

regression_table_EnvPat_fdsm <- fixest::etable(fe_EnvPat_fdsm_lag1_Country_Time,
                                            fe_EnvPat_fdsm_lag5_Country_Time,
                                            digits = 2,
                                            digits.stats = 4,
                                            signif.code = c("***" = 0.01,
                                                            "**" = 0.05,
                                                            "*" = 0.10),
                                            order = c("EPS",
                                                      "Strength",
                                                      "Transitivity"),
                                            tex = TRUE)

saveRDS(regression_table_EnvPat_sdsm, file = paste0(regressiontablepath, "EnvPat_SDSM.rds"))
saveRDS(regression_table_EnvPat_fdsm, file = paste0(regressiontablepath, "EnvPat_FDSM.rds"))


# Save final full tables for SDSM and FDSM  structured results section:

regression_table_lag1 <- fixest::etable(fe_berd_fdsm_lag1_Country_Time,
                                              fe_TPF_fdsm_lag1_Country_Time,
                                              fe_EnvPat_fdsm_lag1_Country_Time,
                                               fe_berd_sdsm_lag1_Country_Time,
                                               fe_TPF_sdsm_lag1_Country_Time,
                                               fe_EnvPat_sdsm_lag1_Country_Time,
                                               digits = 2,
                                               digits.stats = 4,
                                               signif.code = c("***" = 0.01,
                                                               "**" = 0.05,
                                                               "*" = 0.10),
                                               order = c("EPS",
                                                         "Strength",
                                                         "Transitivity"),
                                               tex = TRUE,
                                            headers = list(Model = c("FDSM"=3, "SDSM"=3)),
                                             fitstat=c('n', 'wr2', "f"),
                                             notes = "Newey West corrected standard
                                             errors.* significant 10%, ** significant 5%, *** significant 1%.")

regression_table_lag5 <- fixest::etable(fe_berd_fdsm_lag5_Country_Time,
                                             fe_TPF_fdsm_lag5_Country_Time,
                                             fe_EnvPat_fdsm_lag5_Country_Time,
                                             fe_berd_sdsm_lag5_Country_Time,
                                             fe_TPF_sdsm_lag5_Country_Time,
                                             fe_EnvPat_sdsm_lag5_Country_Time,
                                             digits = 2,
                                             digits.stats = 4,
                                             signif.code = c("***" = 0.01,
                                                             "**" = 0.05,
                                                             "*" = 0.10),
                                             order = c("EPS",
                                                       "Strength",
                                                       "Transitivity"),
                                             tex = TRUE,
                                             headers = list(Model = c("FDSM"=3, "SDSM"=3)),
                                             fitstat=c('n', 'wr2', "f"),
                                             notes = "Newey West corrected standard
                                             errors.* significant 10%, ** significant 5%, *** significant 1%.")

saveRDS(regression_table_lag1, file = paste0(regressiontablepath, "Lag1.rds"))
saveRDS(regression_table_lag5, file = paste0(regressiontablepath, "lag5.rds"))

############################## Robustness checks ###############################
# Done only on the displayed models

# 1) Multicollinearity

# Checking that the minimum of the Cholesky decomposition is not equal to 0
# None.
c(fe_berd_fdsm_lag1_Country_Time$collin.min_norm,
      fe_TPF_fdsm_lag1_Country_Time$collin.min_norm,
      fe_EnvPat_fdsm_lag1_Country_Time$collin.min_norm,
      fe_berd_sdsm_lag1_Country_Time$collin.min_norm,
      fe_TPF_sdsm_lag1_Country_Time$collin.min_norm,
      fe_EnvPat_sdsm_lag1_Country_Time$collin.min_norm)
# Checking for perfect multicollinearity -> None
all(c(fe_berd_fdsm_lag1_Country_Time$multicol,
      fe_TPF_fdsm_lag1_Country_Time$multicol,
      fe_EnvPat_fdsm_lag1_Country_Time$multicol,
      fe_berd_sdsm_lag1_Country_Time$multicol,
      fe_TPF_sdsm_lag1_Country_Time$multicol,
      fe_EnvPat_sdsm_lag1_Country_Time$multicol))
# Checking that the minimum of the Cholesky decomposition is not equal or very close to 0
# None.
c(fe_berd_fdsm_lag5_Country_Time$collin.min_norm,
      fe_TPF_fdsm_lag5_Country_Time$collin.min_norm,
      fe_EnvPat_fdsm_lag5_Country_Time$collin.min_norm,
      fe_berd_sdsm_lag5_Country_Time$collin.min_norm,
      fe_TPF_sdsm_lag5_Country_Time$collin.min_norm,
      fe_EnvPat_sdsm_lag5_Country_Time$collin.min_norm)
# Checking for perfect multicollinearity -> None
all(c(fe_berd_fdsm_lag5_Country_Time$multicol,
      fe_TPF_fdsm_lag5_Country_Time$multicol,
      fe_EnvPat_fdsm_lag5_Country_Time$multicol,
      fe_berd_sdsm_lag5_Country_Time$multicol,
      fe_TPF_sdsm_lag5_Country_Time$multicol,
      fe_EnvPat_sdsm_lag5_Country_Time$multicol))

# 2) Checking out the regression residuals for normality
par(mfrow = c(3, 2))
plot(density(fe_berd_fdsm_lag1_Country_Time$residuals))
plot(density(fe_TPF_fdsm_lag1_Country_Time$residuals))
plot(density(fe_EnvPat_fdsm_lag1_Country_Time$residuals))
plot(density(fe_berd_sdsm_lag1_Country_Time$residuals))
plot(density(fe_TPF_sdsm_lag1_Country_Time$residuals))
plot(density(fe_EnvPat_sdsm_lag1_Country_Time$residuals))
dev.off()
# Looks normal ;)
par(mfrow = c(3, 2))
plot(density(fe_berd_fdsm_lag5_Country_Time$residuals))
plot(density(fe_TPF_fdsm_lag5_Country_Time$residuals))
plot(density(fe_EnvPat_fdsm_lag5_Country_Time$residuals))
plot(density(fe_berd_sdsm_lag5_Country_Time$residuals))
plot(density(fe_TPF_sdsm_lag5_Country_Time$residuals))
plot(density(fe_EnvPat_sdsm_lag5_Country_Time$residuals))
dev.off()
# Looks normal too :)
#
# 3) Joint Ftests on time dummies are always rejected. --> We need time dummies
# in all models

# SDSM lag 1
pFtest(plm::plm(BERD_log ~
                  EPS_log_lag1 +
                  Strength_SDSM_log_lag1 +
                  Transitivity_SDSM_log_lag1 +
                  log(GDPCapita) +
                  log(ExportIntensity) +
                  log(ImportIntensity) +
                  factor(Year),
                data = all_vars,
                index = c("Country", "Year"),
                model = "within"),
       plm::plm(BERD_log ~
                  EPS_log_lag1 +
                  Strength_SDSM_log_lag1 +
                  Transitivity_SDSM_log_lag1 +
                  log(GDPCapita) +
                  log(ExportIntensity) +
                  log(ImportIntensity),
                data = all_vars,
                index = c("Country", "Year"),
                model = "within")
)
# Weakest test but still rejected at 5%
pFtest(plm::plm(Triadic_Patents_log ~
                  EPS_log_lag1 +
                  Strength_SDSM_log_lag1 +
                  Transitivity_SDSM_log_lag1 +
                  log(GDPCapita) +
                  log(ExportIntensity) +
                  log(ImportIntensity) +
                  factor(Year),
                data = all_vars,
                index = c("Country", "Year"),
                model = "within"),
       plm::plm(Triadic_Patents_log ~
                  EPS_log_lag1 +
                  Strength_SDSM_log_lag1 +
                  Transitivity_SDSM_log_lag1 +
                  log(GDPCapita) +
                  log(ExportIntensity) +
                  log(ImportIntensity),
                data = all_vars,
                index = c("Country", "Year"),
                model = "within")
)
# FDSM lag 1
pFtest(plm::plm(Triadic_Patents_log ~
                  EPS_log_lag1 +
                  Strength_FDSM_log_lag1 +
                  Transitivity_FDSM_log_lag1 +
                  log(GDPCapita) +
                  log(ExportIntensity) +
                  log(ImportIntensity) +
                  factor(Year),
                data = all_vars,
                index = c("Country", "Year"),
                model = "within"),
       plm::plm(Triadic_Patents_log ~
                  EPS_log_lag1 +
                  Strength_FDSM_log_lag1 +
                  Transitivity_FDSM_log_lag1 +
                  log(GDPCapita) +
                  log(ExportIntensity) +
                  log(ImportIntensity),
                data = all_vars,
                index = c("Country", "Year"),
                model = "within")
)
pFtest(plm::plm(BERD_log ~
                  EPS_log_lag1 +
                  Strength_FDSM_log_lag1 +
                  Transitivity_FDSM_log_lag1 +
                  log(GDPCapita) +
                  log(ExportIntensity) +
                  log(ImportIntensity) +
                  factor(Year),
                data = all_vars,
                index = c("Country", "Year"),
                model = "within"),
       plm::plm(BERD_log ~
                  EPS_log_lag1 +
                  Strength_FDSM_log_lag1 +
                  Transitivity_FDSM_log_lag1 +
                  log(GDPCapita) +
                  log(ExportIntensity) +
                  log(ImportIntensity),
                data = all_vars,
                index = c("Country", "Year"),
                model = "within")
)
# SDSM lag 5
pFtest(plm::plm(BERD_log ~
                  EPS_log_lag5 +
                  Strength_SDSM_log_lag5 +
                  Transitivity_SDSM_log_lag5 +
                  log(GDPCapita) +
                  log(ExportIntensity) +
                  log(ImportIntensity) +
                  factor(Year),
                data = all_vars,
                index = c("Country", "Year"),
                model = "within"),
       plm::plm(BERD_log ~
                  EPS_log_lag5 +
                  Strength_SDSM_log_lag5 +
                  Transitivity_SDSM_log_lag5 +
                  log(GDPCapita) +
                  log(ExportIntensity) +
                  log(ImportIntensity),
                data = all_vars,
                index = c("Country", "Year"),
                model = "within")
)
pFtest(plm::plm(Triadic_Patents_log ~
                  EPS_log_lag5 +
                  Strength_SDSM_log_lag5 +
                  Transitivity_SDSM_log_lag5 +
                  log(GDPCapita) +
                  log(ExportIntensity) +
                  log(ImportIntensity) +
                  factor(Year),
                data = all_vars,
                index = c("Country", "Year"),
                model = "within"),
       plm::plm(Triadic_Patents_log ~
                  EPS_log_lag5 +
                  Strength_SDSM_log_lag5 +
                  Transitivity_SDSM_log_lag5 +
                  log(GDPCapita) +
                  log(ExportIntensity) +
                  log(ImportIntensity),
                data = all_vars,
                index = c("Country", "Year"),
                model = "within")
)
# FDSM lag 5
pFtest(plm::plm(Triadic_Patents_log ~
                  EPS_log_lag5 +
                  Strength_FDSM_log_lag5 +
                  Transitivity_FDSM_log_lag5 +
                  log(GDPCapita) +
                  log(ExportIntensity) +
                  log(ImportIntensity) +
                  factor(Year),
                data = all_vars,
                index = c("Country", "Year"),
                model = "within"),
       plm::plm(Triadic_Patents_log ~
                  EPS_log_lag5 +
                  Strength_FDSM_log_lag5 +
                  Transitivity_FDSM_log_lag5 +
                  log(GDPCapita) +
                  log(ExportIntensity) +
                  log(ImportIntensity),
                data = all_vars,
                index = c("Country", "Year"),
                model = "within")
)
pFtest(plm::plm(BERD_log ~
                  EPS_log_lag5 +
                  Strength_FDSM_log_lag5 +
                  Transitivity_FDSM_log_lag5 +
                  log(GDPCapita) +
                  log(ExportIntensity) +
                  log(ImportIntensity) +
                  factor(Year),
                data = all_vars,
                index = c("Country", "Year"),
                model = "within"),
       plm::plm(BERD_log ~
                  EPS_log_lag5 +
                  Strength_FDSM_log_lag5 +
                  Transitivity_FDSM_log_lag5 +
                  log(GDPCapita) +
                  log(ExportIntensity) +
                  log(ImportIntensity),
                data = all_vars,
                index = c("Country", "Year"),
                model = "within")
)




################################################################################
####################### Strong Porter Hypothesis ###############################
################################################################################

#   - Total Factor Productivity ----

# Negatively correlated so probably not satisfied

######
# POLS
######

# LAG 1: #######################################################################

fml_FDSM_TotalFactorProd_lag1 <- formula(log(TFP0) ~ EPS_log_lag1 +
                               Strength_FDSM_log_lag1 + Transitivity_FDSM_log_lag1 +
                               log(GDPCapita) +
                               log(ExportIntensity) +
                               log(ImportIntensity))
pols_TotalFactorProd_fdsm_lm_lag1 <- lm(fml_FDSM_TotalFactorProd_lag1,
                            data = all_vars)
summary(pols_TotalFactorProd_fdsm_lm_lag1)
pols_TotalFactorProd_fdsm_plm_lag1 <- plm::plm(fml_FDSM_TotalFactorProd_lag1,
                                   data = all_vars,
                                   index = c("Country", "Year"),
                                   model = "pooling")
summary(pols_TotalFactorProd_fdsm_plm_lag1)
# SDSM
fml_SDSM_TotalFactorProd_lag1 <- formula(log(TFP0) ~ EPS_log_lag1 +
                               Strength_SDSM_log_lag1 + Transitivity_SDSM_log_lag1 +
                               log(GDPCapita) +
                               log(ExportIntensity) +
                               log(ImportIntensity))
pols_TotalFactorProd_SDSM_lm_lag1 <- lm(fml_SDSM_TotalFactorProd_lag1,
                            data = all_vars)
summary(pols_TotalFactorProd_SDSM_lm_lag1)
pols_TotalFactorProd_SDSM_plm_lag1 <- plm::plm(fml_SDSM_TotalFactorProd_lag1,
                                   data = all_vars,
                                   index = c("Country", "Year"),
                                   model = "pooling")
summary(pols_TotalFactorProd_SDSM_plm_lag1)

# LAG 5: #######################################################################

# FDSM
fml_FDSM_TotalFactorProd_lag5 <- formula(log(TFP0) ~ EPS_log_lag5 +
                               Strength_FDSM_log_lag5 + Transitivity_FDSM_log_lag5 +
                               log(GDPCapita) +
                               log(ExportIntensity) +
                               log(ImportIntensity))
pols_TotalFactorProd_fdsm_lm_lag5_lag1 <- lm(fml_FDSM_TotalFactorProd_lag5,
                                 data = all_vars)
summary(pols_TotalFactorProd_fdsm_lm_lag5_lag1)
pols_TotalFactorProd_fdsm_plm_lag5 <- plm::plm(fml_FDSM_TotalFactorProd_lag5,
                                   data = all_vars,
                                   index = c("Country", "Year"),
                                   model = "pooling")
summary(pols_TotalFactorProd_fdsm_plm_lag5)
# SDSM
fml_SDSM_TotalFactorProd_lag5 <- formula(log(TFP0) ~ EPS_log_lag5 +
                               Strength_SDSM_log_lag5 + Transitivity_SDSM_log_lag5 +
                               log(GDPCapita) +
                               log(ExportIntensity) +
                               log(ImportIntensity))
pols_TotalFactorProd_SDSM_lm_lag5 <- lm(fml_SDSM_TotalFactorProd_lag5,
                            data = all_vars,
                            na.action = na.exclude)
summary(pols_TotalFactorProd_SDSM_lm_lag5)
pols_TotalFactorProd_SDSM_plm_lag5 <- plm::plm(fml_SDSM_TotalFactorProd_lag5,
                                   data = all_vars,
                                   index = c("Country", "Year"),
                                   model = "pooling")
summary(pols_TotalFactorProd_SDSM_plm_lag5)

########################
# Fixed effects: Country
########################

# LAG 1: #######################################################################

# FDSM
fe_TotalFactorProd_fdsm_lag1_Country <- fixest::feols(fml = log(TFP0) ~ EPS_log_lag1 +
                                            Strength_FDSM_log_lag1 +
                                            Transitivity_FDSM_log_lag1 +
                                            log(GDPCapita) +
                                            log(ExportIntensity) +
                                            log(ImportIntensity) | Country,
                                          data = all_vars,
                                          vcov = "NW")
fe_TotalFactorProd_fdsm_lag1_Country

# SDSM
fe_TotalFactorProd_sdsm_lag1_Country <- fixest::feols(fml = log(TFP0) ~ EPS_log_lag1 +
                                            Strength_SDSM_log_lag1 +
                                            Transitivity_SDSM_log_lag1 +
                                            log(GDPCapita) +
                                            log(ExportIntensity) +
                                            log(ImportIntensity) | Country,
                                          data = all_vars,
                                          vcov = "NW")
fe_TotalFactorProd_sdsm_lag1_Country

# LAG 5: #######################################################################

# FDSM
fe_TotalFactorProd_fdsm_lag5_Country <- fixest::feols(fml = log(TFP0) ~ EPS_log_lag5 +
                                            Strength_FDSM_log_lag5 +
                                              Transitivity_FDSM_log_lag5 +
                                            log(GDPCapita) +
                                            log(ExportIntensity) +
                                            log(ImportIntensity) | Country,
                                          data = all_vars,
                                          vcov = "NW")
fe_TotalFactorProd_fdsm_lag5_Country
# SDSM
fe_TotalFactorProd_sdsm_lag5_Country <- fixest::feols(fml = log(TFP0) ~ EPS_log_lag5 +
                                            Strength_SDSM_log_lag5 +
                                            Transitivity_SDSM_log_lag5 +
                                            log(GDPCapita) +
                                            log(ExportIntensity) +
                                            log(ImportIntensity) | Country,
                                          data = all_vars,
                                          vcov = "NW")
fe_TotalFactorProd_sdsm_lag5_Country


#################################
# Fixed effects: Country and Year
#################################

# LAG 1: #######################################################################

# FDSM
fe_TotalFactorProd_fdsm_lag1_Country_Time <- fixest::feols(fml = TFP0 ~ EPS_log_lag1 +
                                                 Strength_FDSM_log_lag1 +
                                                 Transitivity_FDSM_log_lag1 +
                                                 log(GDPCapita) +
                                                 log(ExportIntensity) +
                                                 log(ImportIntensity) | Year + Country,
                                               data = all_vars,
                                               vcov = "NW")
fe_TotalFactorProd_fdsm_lag1_Country_Time
# fe_TotalFactorProd_fdsm_plm <- plm::plm(formula = log(TFP0) ~ EPS_log_lag1 +
#                                 Strength_FDSM_log_lag1 + Transitivity_FDSM_log_lag1 +
#                                 log(GDPCapita) +
#                                 log(ExportIntensity) +
#                                 log(ImportIntensity),
#                              data = all_vars,
#                              index = c("Country", "Year"),
#                              effect = "twoway",
#                              model = "within")
# summary(fe_TotalFactorProd_fdsm_plm) --> Equivalency check passed
# SDSM
fe_TotalFactorProd_sdsm_lag1_Country_Time <- fixest::feols(fml = TFP0 ~ EPS_log_lag1 +
                                                 Strength_SDSM_log_lag1 +
                                                 Transitivity_SDSM_log_lag1 +
                                                 log(GDPCapita) +
                                                 log(ExportIntensity) +
                                                 log(ImportIntensity) | Year + Country,
                                               data = all_vars,
                                               vcov = "NW")
fe_TotalFactorProd_sdsm_lag1_Country_Time
# fe_TotalFactorProd_sdsm_plm <- fixest::feols(fml = log(TFP0) ~ EPS_log_lag1 +
#                                 Strength_SDSM_log_lag1 + Transitivity_SDSM_log_lag1 +
#                                 log(GDPCapita) +
#                                 log(ExportIntensity) +
#                                 log(ImportIntensity) | Year + Country,
#                               data = all_vars)
# summary(fe_TotalFactorProd_sdsm_plm) --> Equivalency check passed

# LAG 5: #######################################################################

# FDSM
fe_TotalFactorProd_fdsm_lag5_Country_Time <- fixest::feols(fml = TFP0 ~ EPS_log_lag5 +
                                                 Strength_FDSM_log_lag5 +
                                                 Transitivity_FDSM_log_lag5 +
                                                 log(GDPCapita) +
                                                 log(ExportIntensity) +
                                                 log(ImportIntensity) | Year + Country,
                                               data = all_vars,
                                               vcov = "NW")
fe_TotalFactorProd_fdsm_lag5_Country_Time
# SDSM
fe_TotalFactorProd_sdsm_lag5_Country_Time <- fixest::feols(fml = TFP0 ~ EPS_log_lag5 +
                                                 Strength_SDSM_log_lag5 +
                                                 Transitivity_SDSM_log_lag5 +
                                                 log(GDPCapita) +
                                                 log(ExportIntensity) +
                                                 log(ImportIntensity) | Year + Country,
                                               data = all_vars,
                                               vcov = "NW")
fe_TotalFactorProd_sdsm_lag5_Country_Time
# Exporting regression tables:
# Set labels for estimated varibles
regression_table_strong <- fixest::etable(fe_TotalFactorProd_sdsm_lag1_Country_Time,
                                          fe_TotalFactorProd_fdsm_lag1_Country_Time,
                                          fe_TotalFactorProd_sdsm_lag5_Country_Time,
                                          fe_TotalFactorProd_fdsm_lag5_Country_Time,
                                        digits = 2,
                                        digits.stats = 4,
                                        signif.code = c("***" = 0.01,
                                                        "**" = 0.05,
                                                        "*" = 0.10),
                                        order = c("EPS",
                                                  "Strength",
                                                  "Transitivity"),
                                        tex = TRUE,
                                        headers = list(Lag = c("k = 1" = 2, "k = 5" = 2),
                                                       Model = c("FDSM" = 1, "SDSM" = 1,"FDSM" = 1, "SDSM" = 1)),
                                        fitstat = c('n', 'wr2', "f"),
                                        notes = "Newey West corrected standard
                                             errors.* significant 10%, ** significant 5%, *** significant 1%.")

saveRDS(regression_table_lag1_strong, file = paste0(regressiontablepath, "StrongRegressionTable.rds"))


############################## Robustness checks ###############################
# Done only on the displayed models

# 1) Multicollinearity

# Checking that the minimum of the Cholesky decomposition is not equal to 0
# None.
c(fe_TotalFactorProd_sdsm_lag1_Country_Time$collin.min_norm,
  fe_TotalFactorProd_fdsm_lag1_Country_Time$collin.min_norm)
# Checking for perfect multicollinearity -> None
all(c(fe_TotalFactorProd_sdsm_lag1_Country_Time$multicol,
      fe_TotalFactorProd_fdsm_lag1_Country_Time$multicol))
# Checking that the minimum of the Cholesky decomposition is not equal or very close to 0
# None.
c(fe_TotalFactorProd_sdsm_lag5_Country_Time$collin.min_norm,
  fe_TotalFactorProd_fdsm_lag5_Country_Time$collin.min_norm)
# Checking for perfect multicollinearity -> None
all(c(fe_TotalFactorProd_sdsm_lag5_Country_Time$multicol,
      fe_TotalFactorProd_fdsm_lag5_Country_Time$multicol))

# 2) Checking out the regression residuals for normality
par(mfrow = c(2, 2))
plot(density(fe_TotalFactorProd_sdsm_lag1_Country_Time$residuals))
plot(density(fe_TotalFactorProd_fdsm_lag1_Country_Time$residuals))
plot(density(fe_TotalFactorProd_sdsm_lag5_Country_Time$residuals))
plot(density(fe_TotalFactorProd_fdsm_lag5_Country_Time$residuals))
dev.off()
# Looks normal :)
#
# 3) Joint Ftests on time dummies are always rejected. --> We need time dummies
# in all models

# SDSM lag 1
pFtest(plm::plm(TFP0 ~
                  EPS_log_lag1 +
                  Strength_SDSM_log_lag1 +
                  Transitivity_SDSM_log_lag1 +
                  log(GDPCapita) +
                  log(ExportIntensity) +
                  log(ImportIntensity) +
                  factor(Year),
                data = all_vars,
                index = c("Country", "Year"),
                model = "within"),
       plm::plm(TFP0 ~
                  EPS_log_lag1 +
                  Strength_SDSM_log_lag1 +
                  Transitivity_SDSM_log_lag1 +
                  log(GDPCapita) +
                  log(ExportIntensity) +
                  log(ImportIntensity),
                data = all_vars,
                index = c("Country", "Year"),
                model = "within")
)
# FDSM lag 1
pFtest(plm::plm(TFP0 ~
                  EPS_log_lag1 +
                  Strength_FDSM_log_lag1 +
                  Transitivity_FDSM_log_lag1 +
                  log(GDPCapita) +
                  log(ExportIntensity) +
                  log(ImportIntensity) +
                  factor(Year),
                data = all_vars,
                index = c("Country", "Year"),
                model = "within"),
       plm::plm(TFP0 ~
                  EPS_log_lag1 +
                  Strength_FDSM_log_lag1 +
                  Transitivity_FDSM_log_lag1 +
                  log(GDPCapita) +
                  log(ExportIntensity) +
                  log(ImportIntensity),
                data = all_vars,
                index = c("Country", "Year"),
                model = "within")
)
# SDSM lag 5
pFtest(plm::plm(TFP0 ~
                  EPS_log_lag5 +
                  Strength_SDSM_log_lag5 +
                  Transitivity_SDSM_log_lag5 +
                  log(GDPCapita) +
                  log(ExportIntensity) +
                  log(ImportIntensity) +
                  factor(Year),
                data = all_vars,
                index = c("Country", "Year"),
                model = "within"),
       plm::plm(TFP0 ~
                  EPS_log_lag5 +
                  Strength_SDSM_log_lag5 +
                  Transitivity_SDSM_log_lag5 +
                  log(GDPCapita) +
                  log(ExportIntensity) +
                  log(ImportIntensity),
                data = all_vars,
                index = c("Country", "Year"),
                model = "within")
)
)
# FDSM lag 5
pFtest(plm::plm(TFP0 ~
                  EPS_log_lag5 +
                  Strength_FDSM_log_lag5 +
                  Transitivity_FDSM_log_lag5 +
                  log(GDPCapita) +
                  log(ExportIntensity) +
                  log(ImportIntensity) +
                  factor(Year),
                data = all_vars,
                index = c("Country", "Year"),
                model = "within"),
       plm::plm(TFP0 ~
                  EPS_log_lag5 +
                  Strength_FDSM_log_lag5 +
                  Transitivity_FDSM_log_lag5 +
                  log(GDPCapita) +
                  log(ExportIntensity) +
                  log(ImportIntensity),
                data = all_vars,
                index = c("Country", "Year"),
                model = "within")
)
