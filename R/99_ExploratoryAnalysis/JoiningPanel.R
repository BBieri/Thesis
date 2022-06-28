################################################################################
########################### Joining panel data  ################################
################################################################################

library(tidyverse)
# Set path to clean data
data_path <- "data/ExploratoryAnalysis/"
# OECD Country code helpers
oecd <- read.csv("data_raw/oecd.csv") %>%
  dplyr::mutate(WB_code = countrycode::countrycode(Code,
                                                   "iso3c",
                                                   "wb"),
                iso2c = countrycode::countrycode(Code,
                                                 "iso3c",
                                                 "iso2c"))

# Note: everything is in USD PPP 2015; COU corresponds to ISO3c code

# Import clean data: ----

# Governance data:
ieadb_eigen_OECD <- readRDS(paste0(data_path, "ieadb_eigen_OECD.rds"))
# Innovation data -> dependent variable
BERD_PPP <- readRDS(paste0(data_path, "BERD_PPP.rds"))
TotalFactorProd <- readRDS(paste0(data_path, "tfp012_join.rds"))
tpf_inventor_country <- readRDS(paste0(data_path, "tpf_inventor_country.rds"))
# Controls:
WB_controls <- readRDS(paste0(data_path, "WB_controls.rds"))
labour_controls <- readRDS(paste0(data_path, "labour_controls.rds"))
GOVERD_controls <- readRDS(paste0(data_path, "GOVERD_controls.rds"))
eps <- readRDS(paste0(data_path, "eps.rds"))

# Minor corrections to individual dataframes & filter:----

ieadb_eigen_OECD <- ieadb_eigen_OECD %>%
  dplyr::mutate(Country = countrycode::countrycode(Country,
                                                  "iso3c",
                                                  "country.name")) %>%
  dplyr::rename(EigenCent = value,
                COU = iso2c) %>%
  dplyr::filter(Year %in% 1990:2015) %>%
  dplyr::filter(COU %in% oecd$iso2c)
# Innovation data -> dependent variable
BERD_PPP <- BERD_PPP %>%
  dplyr::select(COU, Year, Country, Value, Flag.Codes, Flags) %>%
  dplyr::rename(FlagCodes = Flag.Codes,
                BERD_PPP = Value) %>%
  dplyr::filter(Year %in% 1990:2015) %>%
  dplyr::filter(COU %in% oecd$iso2c)
TotalFactorProd <- TotalFactorProd %>%
  dplyr::filter(Year %in% 1990:2015) %>%
  dplyr::filter(COU %in% oecd$iso2c)
tpf_inventor_country <- tpf_inventor_country %>%
  dplyr::rename(tpf = n,
                Year = First_Prio_Year) %>%
  dplyr::mutate(Year = as.numeric(Year)) %>%
  dplyr::filter(Year %in% 1990:2015) %>%
  dplyr::filter(COU %in% oecd$iso2c)
# Controls:
WB_controls <- WB_controls %>%
  dplyr::rename(COU = iso2c,
                Year = year,
                Country = country) %>%
  dplyr::filter(COU %in% oecd$iso2c)
labour_controls <- labour_controls %>%
  dplyr::rename(Year = TIME) %>%
  dplyr::select(-Time) %>%
  dplyr::select(COU, Year, Country, Sector, Value, lnValue,
                laggedlnValue, DifflnValue)
  dplyr::relocate(COU, Year, Country) %>%
  dplyr::filter(Year %in% 1990:2015) %>%
    dplyr::filter(COU %in% oecd$iso2c)
GOVERD_controls <- GOVERD_controls %>%
  dplyr::select(COU, Year, Country, Value) %>%
  dplyr::relocate(COU, Year, Country) %>%
  dplyr::filter(Year %in% 1990:2015) %>%
  dplyr::filter(COU %in% oecd$iso2c)
eps <- eps %>%
  dplyr::rename(EPS = Value) %>%
  dplyr::select(COU, Year, Country, EPS) %>%
  dplyr::relocate(COU, Year, Country) %>%
  dplyr::filter(Year %in% 1990:2015) %>%
  dplyr::filter(COU %in% oecd$iso2c)

# Join clean data: ----

mergeCols <- c("Year" = "Year",
               "COU" = "COU")
# Governance Data --> Independent variable of interest

Governance <- ieadb_eigen_OECD

# Innovation data -> dependent variable

innovation <- BERD_PPP %>%
  dplyr::full_join(TotalFactorProd, by = mergeCols,
                   suffix = c(".TFP", ".TFP")) %>%
  dplyr::full_join(tpf_inventor_country, by = mergeCols,
                   suffix = c(".PATENT", ".PATENT"))

# Controls:
controls <- WB_controls %>%
  dplyr::full_join(labour_controls, by = mergeCols,
                   suffix = c(".labour", ".labour")) %>%
  dplyr::full_join(GOVERD_controls, by = mergeCols,
                   suffix = c(".GOVERD", ".GOVERD")) %>%
  dplyr::full_join(eps, by = mergeCols,
                   suffix = c(".eps", ".eps"))

panel <- Governance %>%
  dplyr::full_join(innovation, by = mergeCols,
                          suffix = c(".innovation", ".innovation")) %>%
  dplyr::full_join(controls, by = mergeCols,
                   suffix = c(".controls", ".controls"))
# Create a country/sector id
panel <- panel %>%
  dplyr::group_by(COU, Sector) %>%
  dplyr::mutate(COUSectorID = dplyr::cur_group_id()) %>%
  # dplyr::ungroup() %>%
  dplyr::rename(Country = Country.innovation) %>%
  dplyr::relocate(COUSectorID, Year, COU, Country)
# Export panel:
saveRDS(panel, paste0(data_path, "CleanPanel.rds"))
