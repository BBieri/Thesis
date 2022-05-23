################################################################################
######################### Import Controls ######################################
################################################################################

library(tidyverse) # 1.3.1
library(WDI) # 2.7.6 World Bank data from API
library(fixest) # Panel tools package
# library(OECD) # OECD data
# OECD_dataset_list <- OECD::get_datasets() # Import available datasets for browsing
save_path <- "data/CleanData/Controls/"

# Note: Everything is normalized to USD 2015 PPP
# Retrieved on:
retrieved <- paste0("Retrieved on ", Sys.Date(), ".")

# Countries:
oecd <- read.csv("data_raw/oecd.csv") %>%
  dplyr::mutate(WB_code = countrycode::countrycode(Code,
                                                   "iso3c",
                                                   "wb"),
                iso2c = countrycode::countrycode(Code,
                                                 "iso3c",
                                                 "iso2c"))
################################
# World Bank GDP, Import, Export
################################

# GDP and GDP per capita: WB ----
# WDIsearch('gdp.*capita.*constant')
# WDIsearch('gdp.*constant')
GDP <- WDI::WDI(indicator = c("GDPCapita" = 'NY.GDP.PCAP.KD',
                              "GDP" = "NY.GDP.MKTP.KD"),
                country = oecd$iso2c,
                start = 1990, end = 2015)
# 2015 USD by default
# Imports/GDP: WB ----
# WDIsearch('import.*constant')
imports <- WDI::WDI(indicator = c("TotalImports" = 'NE.IMP.GNFS.KD'),
                    country = oecd$iso2c,
                    start = 1990, end = 2015)
# 2015 USD by default
# Exports/GDP: WB ----
# WDIsearch('export.*constant')
exports <- WDI::WDI(indicator = c("TotalExports" = 'NE.EXP.GNFS.KD'),
                    country = oecd$iso2c,
                    start = 1990, end = 2015)
# 2015 USD by default
# Combine WB data: ----
WB <- dplyr::inner_join(GDP, imports, by = c("iso2c" = "iso2c",
                                             "year" = "year")) %>%
  dplyr::inner_join(exports, by = c("iso2c" = "iso2c",
                                         "year" = "year")) %>%
  dplyr::select(-country.x, -country.y) %>%
  dplyr::arrange(country, year) %>%
  dplyr::relocate(iso2c, year, country) %>%
  dplyr::mutate(ExportIntensity = TotalExports/GDP,
                ImportIntensity = TotalImports/GDP)

saveRDS(WB, file = paste0(save_path, "WB_controls.rds"))

#############################
# Labour variation: OECD ----
#############################

# This dataframe contains data on the number of persons employed in sector
# i in country j and in year t. The panel is unbalanced.
# Source: https://stats.oecd.org/viewhtml.aspx?datasetcode=STLABOUR&lang=en#
# OECD::search_dataset("labour", data = OECD_dataset_list)
labour <- read.csv("data_raw/OECD/Labour/STLABOUR_240320222.csv")
names(labour)[1] <- "COU"
vars_labour <- dplyr::tibble(Code = unique(labour$SUBJECT),
                         Name = unique(labour$Subject))
labour <- labour %>%
  dplyr::mutate(
    Value = Value * 10^PowerCode.Code, # Set value to actual number
    Sector = ifelse(SUBJECT == "LFEAAGTT", "AGRI",
      ifelse(SUBJECT == "LFEACNTT", "CONS",
        ifelse(SUBJECT == "LFEAINTT", "INDU",
          ifelse(SUBJECT == "LFEAMNTT", "MANU",
            ifelse(SUBJECT == "LFEASETT", "SERV",
              ifelse(SUBJECT == "LFEAICTT", "INDUCONS", NA))
          )
        )
      )
    ),
    lnValue = log(Value)
  ) %>%
  dplyr::filter(Sector != "INDUCONS") %>%
  dplyr::rename(iso3c = COU) %>%
  dplyr::mutate(COU = countrycode::countrycode(iso3c, "iso3c", "iso2c",
                                               custom_match = c("EA19" = "EA19",
                                                                "EU27_2020" = "EU27_2020",
                                                                "G-7" = "G7",
                                                                "OECD" = "OECD")))

labour <- labour %>%
  dplyr::group_by(COU, Sector) %>%
  dplyr::mutate(laggedlnValue = dplyr::lag(lnValue)) %>%
  dplyr::ungroup() %>%
  dplyr::filter(Time != 1989) %>%
  dplyr::mutate(DifflnValue = lnValue - laggedlnValue)

saveRDS(labour, file = paste0(save_path, "labour_controls.rds"))

########################
# Value added: OECD ----
########################

# Source: https://stats.oecd.org/Index.aspx?DataSetCode=SNA_TABLE6A#
# OECD::search_dataset("value added", data = OECD_dataset_list)
VA <- read.csv("data_raw/OECD/ValueAdded/SNA_TABLE6A_24032022.csv")
names(VA)[1] <- "COU"
vars_VA <- dplyr::tibble(Code = unique(VA$TRANSACT),
                           Name = unique(VA$Transaction),
                         MeasureCode = unique(VA$MEASURE),
                         MeasureName = unique(VA$Measure))
GVA_BY_ACTVITY <- VA %>%
  dplyr::filter(MEASURE == "VOB") %>% # 2015 LCU
  dplyr::mutate(iso2c = countrycode::countrycode(COU,
                                                 "iso3c",
                                                 "iso2c",
                                                 custom_match =
                                                   c("EU27_2020" = "EU27_2020")))
# TODO: this needs to be transformed into USD from 2015 constant LCU
# paper did it with WB correction factor


activities_VA <- dplyr::tibble(Code = unique(VA$ACTIVITY),
                               Name = unique(VA$Activity))



# R&D Capital stock and Patent Stock: OECD ----
# Not sure if this is available straight up: the authors seem to compute it
# with the R&D expenditure data and the patent data by summing the patents/the expenditures
# over the last 10 years. --> See innovation mesures

# Government Expenditure in R&D per country: OECD ----
# GOVERD
# Retrieved on 2022-03-24.
# Sources:
# https://stats.oecd.org/viewhtml.aspx?datasetcode=MSTI_PUB&lang=en#
# https://rdmetadata.oecd.org/
# See data manual in data_raw
GERD <- read.csv("data_raw/OECD/RDExpenditures/MSTI_PUB_24032022.csv")
names(GERD)[1] <- "MSTI_VAR"
vars_GERD <- dplyr::tibble(Code = unique(GERD$MSTI_VAR),
                           Name = unique(GERD$MSTI.Variables))
# Reference period is 2015 PPP USD
GOVERD_PPP <- GERD %>%
  dplyr::filter(MSTI_VAR == "GV_PPPCT") %>% # 2015 USD
  dplyr::select(-YEAR) %>%
  dplyr::mutate(iso2c = countrycode::countrycode(COU,
                                                 "iso3c",
                                                 "iso2c",
                                                 custom_match = (
                                                   c("EU27_2020" = "EU27_2020",
                                                     "OECD" = "OECD")
                                                 ))) %>%
  dplyr::rename(iso3c = COU,
                COU = iso2c)

saveRDS(GOVERD_PPP, file = paste0(save_path, "GOVERD_controls.rds"))
