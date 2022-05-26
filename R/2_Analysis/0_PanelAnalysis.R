################################################################################
########################        Panel Measures        ##########################
################################################################################

# Panel resources:
# - https://rpubs.com/phle/r_tutorial_panel_data_analysis

library(tidyverse) # 1.3.1
library(countrycode) # 1.3.1
library(fixest) # Great package for multiple fixed effects estimations
library(plm) # 2.6-1
library(lme4) # 1.1-29
library(lfe) # 2.8-8

data_path <- "data/CleanData/"
eps_path <- "data/CleanData/EPS/"
innovation_path <- "data/CleanData/Innovation/"
controls_path <- "data/CleanData/Controls/"


############################ Import Data #######################################

##############################
# Cooperation Network Measures
##############################

# Strength FROW:
load(paste0(data_path, "OECD_Panel_Strength_frow.rds"))
Strength_frow <- saved
rm(saved)
# Eigenvector FROW:
load(paste0(data_path, "OECD_Panel_Eigenvector_frow.rds"))
Eigenvector_frow <- saved
rm(saved)

frow_joined <- dplyr::inner_join(Strength_frow, Eigenvector_frow,
                                 by = c("Country" = "Country", "Year" = "Year")) %>%
  dplyr::mutate(COU = countrycode::countrycode(Country, "iso3c", "iso2c"),
                Year = as.numeric(Year))

# # Strength FDSM:
# load(paste0(data_path, "OECD_Panel_Strength_fdsm.rds"))
# Strength_frow <- saved
# rm(saved)
# # Eigenvector FDSM:
# load(paste0(data_path, "OECD_Panel_Eigenvector_fdsm.rds"))
# Eigenvector_frow <- saved
# rm(saved)
#
# frow_joined <- dplyr::inner_join(Strength_frow, Eigenvector_frow,
#                                  by = c("Country" = "Country", "Year" = "Year")) %>%
#   dplyr::mutate(COU = countrycode::countrycode(Country, "iso3c", "iso2c"),
#                 Year = as.numeric(Year))

# Strength SDSM:
load(paste0(data_path, "OECD_Panel_Strength_frow.rds"))
Strength_frow <- saved
rm(saved)
# Eigenvector SDSM:
load(paste0(data_path, "OECD_Panel_Eigenvector_frow.rds"))
Eigenvector_frow <- saved
rm(saved)

frow_joined <- dplyr::inner_join(Strength_frow, Eigenvector_frow,
                                 by = c("Country" = "Country", "Year" = "Year")) %>%
  dplyr::mutate(COU = countrycode::countrycode(Country, "iso3c", "iso2c"),
                Year = as.numeric(Year))

##############################
# Innovation Measures
##############################

# Business expenditures on aggregate R&D 2015 PPP USD
berd <- readRDS(paste0(innovation_path, "BERD_PPP.rds")) %>%
  dplyr::rename(berd = Value) %>%
  dplyr::select(Year, COU, Country, berd)

# Government expenditures on aggregate R&D 2015 PPP USD
goverd <- readRDS(paste0(innovation_path, "GOVERD_controls.rds")) %>%
  dplyr::select(-COU) %>%
  dplyr::rename(goverd = Value,
                COU = iso3c) %>%
  dplyr::select(Year, COU, Country, goverd)

# Total Factor Productivity dataset 2015 PPP USD
tfp <- readRDS(paste0(innovation_path, "tfp012_join.rds")) %>%
  dplyr::select(-COU) %>%
  dplyr::rename(goverd = Value,
                COU = iso3c) %>%
  dplyr::select(Year, COU, Country, goverd)

# Environmental patents OECD dataset
environmental_patents <- readRDS(paste0(innovation_path, "EnvPat.rds")) %>%
  dplyr::rename(EnvPat = Value,
                Country = Inventor.country) %>%
  dplyr::select(Year, COU, Country, EnvPat)

# Triadic patent counts
triadic_patent <- readRDS(paste0(innovation_path, "tpf_inventor_country.rds")) %>%
  dplyr::mutate(COU = countrycode::countrycode(COU, "iso2c", "iso3c")) %>%
  dplyr::rename(Year = First_Prio_Year,
                triadic_patent = n) %>%
  dplyr::select(Year, COU, Country, triadic_patent)

##############################
# Control Measures
##############################

# World Bank Controls
WB <- readRDS(paste0(controls_path, "WB_controls.rds")) %>%
  dplyr::mutate(iso2c = countrycode::countrycode(iso2c, "iso2c", "iso3c")) %>%
  dplyr::rename(Year = year,
                COU = iso2c,
                Country = country)

#############
# EPS Measure
#############

EPS <- readRDS(paste0(eps_path, "eps.rds"))

############################ Join Different datasets ###########################




#################### Subset Datasets to Considered Period ######################


############################ Panel Analysis ####################################

pols_tfp0 <- fixest::feols(fml = Value ~ value.x + value.y, data = panel1)


############################ Generate tables ###################################


# Balancedness of the panel
panel %>%
  select(Year, COUSectorID) %>%
  table()
panel %>%
  plm::is.pbalanced(Index = Year, COUSectorID)

# Graphing panel time series:

ggplot(data = panel) +
  geom_line(aes(x = Year, y = EigenCent,
                group = COU)) +
  labs(x = "Year",  y = "Eigenvector Centrality") +
  theme(legend.position = "none")

ggplot(data = panel) +
  geom_line(aes(x = Year, y = GDPCapita,
                group = COU)) +
  labs(x = "Year",  y = "GDP per Capita") +
  theme(legend.position = "none")

ggplot(data = panel) +
  geom_line(aes(x = Year, y = BERD_PPP,
                group = COU)) +
  labs(x = "Year",  y = "BERD_PPP") +
  theme(legend.position = "none")

# Pooled OLS: ----
# Note: feols from the {fixest} package allows us to estimate any number of
# fixed effects.
#
# feols(fml = TFP0 ~ EigenCent + log(GDPCapita), data = panel)
# is equivalent to:
# lm(formula = TFP0 ~ EigenCent + log(GDPCapita), data = panel)

# Replication Martinez-Zarzoso et al
# POLS
pols_tfp0 <- fixest::feols(fml = TFP0 ~ EPS + log(GDPCapita), data = panel)
pols_tfp1 <- fixest::feols(fml = TFP1 ~ EPS + log(GDPCapita), data = panel)
pols_tfp2 <- fixest::feols(fml = TFP2 ~ EPS + log(GDPCapita), data = panel)
pols_tpf <- fixest::feols(fml = tpf ~ EPS + log(GDPCapita), data = panel)
pols_berd <- fixest::feols(fml = BERD_PPP ~ EPS + log(GDPCapita), data = panel)

# Fixed Effects:
fe_tfp0 <- fixest::feols(fml = TFP0 ~ EPS + log(GDPCapita) | Year + COU + Sector, data = panel)
fe_tfp1 <- fixest::feols(fml = TFP1 ~ EPS + log(GDPCapita) | Year + COU + Sector, data = panel)
fe_tfp2 <- fixest::feols(fml = TFP2 ~ EPS + log(GDPCapita) | Year + COU + Sector, data = panel)
fe_tpf <- fixest::feols(fml = tpf ~ EPS + log(GDPCapita) | Year + COU + Sector, data = panel)
fe_berd <- fixest::feols(fml = BERD_PPP ~ EPS + log(GDPCapita) | Year + COU + Sector, data = panel) # data issues

# Eigenvector centrality:

pols_eps <- fixest::feols(fml = EPS ~ EigenCent + log(GDPCapita), data = panel)
pols_tfp0 <- fixest::feols(fml = TFP0 ~ EigenCent + log(GDPCapita), data = panel)
pols_tfp1 <- fixest::feols(fml = TFP1 ~ EigenCent + log(GDPCapita), data = panel)
pols_tfp2 <- fixest::feols(fml = TFP2 ~ EigenCent + log(GDPCapita), data = panel)
pols_tpf <- fixest::feols(fml = tpf ~ EigenCent + log(GDPCapita), data = panel)
pols_berd <- fixest::feols(fml = BERD_PPP ~ EigenCent + log(GDPCapita), data = panel) # data issues

# Fixed Effects Estimation:

fe_eps <- fixest::feols(fml = EPS ~ EigenCent + log(GDPCapita) | Year + COU + Sector, data = panel)
fe_tfp0 <- fixest::feols(fml = TFP0 ~ EigenCent + log(GDPCapita) | Year + COU + Sector, data = panel)
fe_tfp1 <- fixest::feols(fml = TFP1 ~ EigenCent + log(GDPCapita) | Year + COU + Sector, data = panel)
fe_tfp2 <- fixest::feols(fml = TFP2 ~ EigenCent + log(GDPCapita) | Year + COU + Sector, data = panel)
fe_tpf <- fixest::feols(fml = tpf ~ EigenCent + log(GDPCapita) | Year + COU + Sector, data = panel)
fe_berd <- fixest::feols(fml = BERD_PPP ~ EigenCent + log(GDPCapita) | Year + COU + Sector, data = panel) # data issues


# FE or POLS? pFtest(fe_model_plm, pooled_ols_plm)
# Jointly testing the significance of "LSDV" dummies
plm::pFtest(fe_eps, pols_eps)


# Perform a Hausmann test: -> phtest() RE or FE ?

# Breush Pagan Test: -> plmtest() POLS or RE ?

# Heteroskedasticity test Breush-Pagan lmtest::bptest() Should we use robust SE?

# Serial Correlation test Breusch-Godfrey/Wooldridge: Check whether residuals are serially correlated.

