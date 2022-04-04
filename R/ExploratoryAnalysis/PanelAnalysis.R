################################################################################
########################        Panel Measures        ##########################
################################################################################

# Panel resources:
# - https://rpubs.com/phle/r_tutorial_panel_data_analysis

library(tidyverse)
library(fixest) # Great package for multiple fixed effects estimations
library(plm)
library(lme4)
library(lfe)


# Import clean panel data:
data_path <- data_path <- "data/ExploratoryAnalysis/"
panel <- read_rds(paste0(data_path, "CleanPanel.rds"))

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

