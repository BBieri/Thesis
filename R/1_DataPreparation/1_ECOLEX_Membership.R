# ECOLEX_MEM Preparation Script

# load("data_raw/ECOLEX/Membership/ecomembs.RData")
# Actually let's load the processed data since it's the same
# Extracted on the 22/04/2022
# ECOLEX_mem <- manyenviron::memberships$ECOLEX_MEM
# save(ECOLEX_mem, file = "data_raw/ECOLEX/Membership/ECOLEX_membership.rds")

# Load Libraries
library(tidyverse)

# Import Agreements from ECOLEX and join both:

load("data/ECOLEX/Agreements/ECOLEX_Agreements.rds")
load("data_raw/ECOLEX/Membership/ECOLEX_membership.rds")

ECOLEX_mems <- dplyr::left_join(ECOLEX_mem, ECOLEX,
                                by = c("ecolexID" = "ecolexID"),
                                suffix = c("", ".annoying_duplicate_column")) %>%
  dplyr::select(-dplyr::ends_with(".annoying_duplicate_column"))

# Corrected a wrong date
ECOLEX_mems[[1, "Beg"]] <- ECOLEX_mems[[1, "SignatureCountry"]] <-
  messydates::as_messydate("1992-06-12")
# Testing if uncertain messydates and converting everything back to dates
any(!messydates::is_precise(ECOLEX_mems$Beg), na.rm = T) # FALSE
any(!messydates::is_precise(ECOLEX_mems$Force), na.rm = T) # FALSE
any(!messydates::is_precise(ECOLEX_mems$SignatureCountry), na.rm = T) # FALSE
any(!messydates::is_precise(ECOLEX_mems$End), na.rm = T) # FALSE
any(!messydates::is_precise(ECOLEX_mems$Rat), na.rm = T) # FALSE
# Checking whether the NA values in Country ID are the agreements that
# were passed with non state actors. (load eco_membs on line 3 to check)
# sort(eco_membs[which(is.na(eco_membs$StatID)), "EcolexID"]) == sort(pull(ECOLEX_mems[which(is.na(ECOLEX_mems$CountryID)), "ecolexID"]))
# These are removed below in the filtering stage.


ECOLEX_mems <- ECOLEX_mems %>%
  dplyr::mutate(Beg = as.Date(Beg, mean),
                End = as.Date(End, mean),
                SignatureCountry = as.Date(SignatureCountry, mean),
                Force = as.Date(Force, mean),
                Rat = as.Date(Rat, mean))
# Filtered out agreements that were signed pre-war, or have not dates of
# ratification or entry into force following Carattini et al.
ECOLEX_mems <- ECOLEX_mems %>%
  dplyr::filter(Beg >= as.Date("1948-01-01")) %>%
  # Removes pre-war Agreements
  # 21270 membership actions remain
  # 4566 manyID agreements (more ID's due to different signature dates)
  # 608 ecolexID agreements
  dplyr::filter(!(is.na(Rat) & is.na(Force))) %>%
  # Removes agreements which we are not sure were ratified or are in force
  # 19823 membership actions remain
  # 4493 manyID agreements (more ID's due to different signature dates)
  # 585 ecolexID agreements
  dplyr::filter(!is.na(CountryID))
  # Removes agreements which have been signed by a non-state entity.
  # 19801 membership actions remain
  # 4490 manyID agreements (more ID's due to different signature dates)
  # 585 ecolexID agreements

# NB: length(unique(ECOLEX_mems$manyID)) --> Unique manyID's
# NB: length(unique(ECOLEX_mems$ecolexID)) --> Unique ecolexID's

# Additional cleaning:

country_ranking <- dplyr::count(ECOLEX_mems, CountryID, sort = TRUE) %>% dplyr::mutate(Name = countrycode::countrycode(CountryID, "iso3c", "country.name", custom_match = c('BAV' = 'Bavaria', 'BAD' = 'BadenWutemberg', 'KOS' = 'Kosovo', 'EUE' = 'European Union')))

# Remove Subnational Entities, SAR, etc.
# De Jure sovereign states remain.

ECOLEX_mems <- ECOLEX_mems %>%
  dplyr::filter(!(CountryID %in% c("BAD", "BAV", "GGY",
                                   "JEY", "MAC", "MSR",
                                   "VGB", "HKG", "TKL")))
# 19788 membership actions remain
# 4489 manyID agreements (more ID's due to different signature dates) remain
# 585 ecolexID agreements remain

# Highlight agreements that ended at a given point
ended <- ECOLEX_mems[which(!is.na(ECOLEX_mems$End)),]

# Create subject by subject dataframes
subjects <- unique(ECOLEX_mems$Subject_1)
lst <- list()
for (i in seq(length(subjects))) {
  lst[[i]] <- dplyr::filter(ECOLEX_mems, grepl(subjects[[i]], Subject))
}
names(lst) <- paste0("ECOLEX_", subjects)
names(lst)[[15]] <- "ECOLEX_Envrionment_gen"
names(lst) <- gsub("&", "", names(lst))
names(lst) <- gsub("\\s+", " ", names(lst))
names(lst) <- chartr(" ", "_", names(lst))

# Save everything ----
path <- "data/ECOLEX/Membership/"
save(ECOLEX_mems, file = paste0(path, "ECOLEX_Membership.rds")) # Full
for (i in seq(length(subjects))) {
  saved <- lst[[i]]
  save(saved, file = paste0(path, names(lst)[[i]], ".rds")) # Subject
}
