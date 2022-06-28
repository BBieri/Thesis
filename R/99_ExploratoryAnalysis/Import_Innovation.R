################################################################################
################## Import Innovation Proxies ###################################
################################################################################

library(tidyverse)

save_path <- "data/ExploratoryAnalysis/"

# Note: Everything is normalized to USD 2015 PPP

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

saveRDS(BERD_PPP, file = paste0(save_path,
                                   "BERD_PPP.rds"))

# TFP: EU KLEMS ----
#
# Note: See data documentation in data_raw folder for more information about the
# methodology. All TFP data is in growth rates and not in levels.
# Source: https://euklems.eu/
# Retrieved on: 2022-03-25

tfp0 <- dplyr::tibble(readRDS("data_raw/TFP_EU_KLEMS/wiiw-KLEMS_TFP0.rds"))
tfp1 <- dplyr::tibble(readRDS("data_raw/TFP_EU_KLEMS/wiiw-KLEMS_TFP1.rds"))
tfp2 <- dplyr::tibble(readRDS("data_raw/TFP_EU_KLEMS/wiiw-KLEMS_TFP2.rds"))
tfpshares <- dplyr::tibble(readRDS("data_raw/TFP_EU_KLEMS/wiiw-KLEMS_shares.rds"))
# Remove duplicate columns:
tfp0 <- tfp0[!duplicated(as.list(tfp0))]
tfp1 <- tfp1[!duplicated(as.list(tfp1))]
tfp2 <- tfp2[!duplicated(as.list(tfp2))]
tfpshares <- tfpshares[!duplicated(as.list(tfpshares))]
# Label things correctly:
tfp0 <- tfp0 %>%
  dplyr::rename(LabourProductivity = LPH_0,
                CapitalDeepening = LPH_1,
                TFP0 = LPH_2,
                ValueAdded = VA_0,
                PersonsEmployed = VA_1,
                AvgHoursWorked = VA_2,
                CapitalStock = VA_3,
                Year = year,
                Country = country)
tfp1 <- tfp1 %>%
  dplyr::rename(LabourProductivity = LPH_0,
                CapitalDeepening = LPH_1,
                CapitalComposition = LPH_2,
                TFP1 = LPH_3,
                ValueAdded = VA_0,
                PersonsEmployed = VA_1,
                AvgHoursWorked = VA_2,
                CapitalStock = VA_3,
                Year = year,
                Country = country)

tfp2 <- tfp2 %>%
  dplyr::rename(LabourProductivity = LPH_0,
                LabourComposition = LPH_1,
                CapitalDeepening = LPH_2,
                CapitalComposition = LPH_3,
                TFP2 = LPH_4,
                ValueAdded = VA_0,
                PersonsEmployed = VA_1,
                AvgHoursWorked = VA_2,
                CapitalStock = VA_4,
                Year = year,
                Country = country)
# Join TFP data
tfp012_join <- dplyr::left_join(tfp0, tfp1, by = c("Country" = "Country",
                                            "Year" = "Year",
                                            "nace_r2" = "nace_r2"),
                         suffix = c(".tfp0", ".tfp1")) %>%
  dplyr::left_join(tfp2, by = c("Country" = "Country",
                                     "Year" = "Year",
                                     "nace_r2" = "nace_r2")) %>%
  dplyr::rename(COU = Country) %>%
  dplyr::mutate(Country = countrycode::countrycode(COU, "iso2c",
                                               "country.name",
                                               custom_match = c("EL" = "Greece"))) %>%
  dplyr::select(Year, COU, Country, nace_r2, Sort_ID.tfp0, TFP0, TFP1, TFP2) %>%
  dplyr::rename(Sort_ID = Sort_ID.tfp0)

saveRDS(tfp012_join, file = paste0(save_path, "tfp012_join.rds"))

# Note: nace_r2 is the sector: see https://ec.europa.eu/eurostat/documents/3859598/5902521/KS-RA-07-015-EN.PDF

# skimr::skim(tfp012_join)

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
#
# Triadic patent counts by inventor country with fractional counting in case
# the patent was invented by inventors from different countries. See Pp. 18-20
# of the manual in data_raw folder for more details.

tpf_core <- read.table("data_raw/PATSTAT/202107_TPF_Core/202107_TPF_Core.txt",
                       header = T,
                       sep = "|",
                       na.strings = "")
tpf_inventor <- read.table("data_raw/PATSTAT/202107_TPF_Inventors/202107_TPF_Inventors.txt",
                           header = T,
                           sep = "|",
                           na.strings = "")
tpf_tot <- dplyr::left_join(tpf_inventor, tpf_core,
                            by = c("Family_id" = "Family_id")) %>%
  dplyr::mutate(First_Prio_Year = substr(as.character(First_Prio), 1, 4))

tpf_inventor_country <- dplyr::tibble(tpf_tot) %>%
  dplyr::count(Country, First_Prio_Year, wt = Inventor_Share) %>%
  dplyr::filter(Country %in% oecd$iso2c) %>%
  dplyr::mutate(COU = Country,
                Country = countrycode::countrycode(COU,
                                                   "iso2c",
                                                   "country.name"))

saveRDS(tpf_inventor_country, file = paste0(save_path,
                                            "tpf_inventor_country.rds"))

# skimr::skim(tpf_inventor_country)
#
# ggplot2::ggplot(tpf_inventor_country, aes(x = First_Prio_Year, y = n,
#                                           group = Country,
#                                           color = Country)) +
#   geom_line() +
#   ggplot2::labs(title = "Weighted number of patent application by country over time",
#                 caption = "Source: OECD Triadic Patent Family database")
#   hrbrthemes::theme_modern_rc()
