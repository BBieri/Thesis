################################################################################
################## Import Innovation Proxies ###################################
################################################################################

library(tidyverse)

# Note: Everything is normalized to USD 2010 PPP

# Countries:
oecd <- read.csv("data_raw/oecd.csv") %>%
  dplyr::mutate(WB_code = countrycode::countrycode(Code,
                                                   "iso3c",
                                                   "wb"),
                iso2c = countrycode::countrycode(Code,
                                                 "iso3c",
                                                 "iso2c"))

# R&D Expenditures: OECD ----
GERD <- read.csv("data_raw/OECD/RDExpenditures/MSTI_PUB_24032022.csv")
names(GERD)[1] <- "MSTI_VAR"
vars_GERD <- dplyr::tibble(Code = unique(GERD$MSTI_VAR),
                           Name = unique(GERD$MSTI.Variables))
BERD_PPP <- GERD %>%
  dplyr::filter(MSTI_VAR == "B_PPP") %>%
  dplyr::mutate(Value = Value * 10^PowerCode.Code)
# B_PPP is already in constant 2015 USD PPP
# TFP: EU KLEMS ----
tfp0 <- readRDS("data_raw/TFP_EU_KLEMS/wiiw-KLEMS_TFP0.rds")
tfp1 <- readRDS("data_raw/TFP_EU_KLEMS/wiiw-KLEMS_TFP1.rds")
tfp2 <- readRDS("data_raw/TFP_EU_KLEMS/wiiw-KLEMS_TFP2.rds")
tfpshares <- readRDS("data_raw/TFP_EU_KLEMS/wiiw-KLEMS_shares.rds")


# MFP: OECD aggregate level Multifactor productivity:
# See: https://www.oecd.org/sdd/productivity-stats/
# Retrieved on 2022-03-25
MFP_OECD <- read.csv("data_raw/OECD/MFP/PDB_GR_25032022.csv")
names(MFP_OECD)[1] <- "COU"
vars_MFP_OECD <- dplyr::tibble(Code = unique(MFP_OECD$SUBJECT),
                                Name = unique(MFP_OECD$Subject))
# Get MFP Index. 2015 base year
MFP_OECD <- MFP_OECD %>%
  dplyr::filter(SUBJECT == "T_MFP")
# This index is an aggregate one. The paper by Martinez et al. use the EU Klems
# database to differentiate between sectors.

# PATSTAT: OECD ----
# https://forms.office.com/pages/responsepage.aspx?id=1MdBrGEfDUaw9PySWitHHKuxmuqpz_9KusL7-G1D6wFUOEU0OVBYVk5QTzROVlBTSUtBUUREWVhHTiQlQCN0PWcu
