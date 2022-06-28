################################################################################
######################## Import EPS data #######################################
################################################################################
save_path <- "data/ExploratoryAnalysis/"
eps <- read.csv("data_raw/EPS/EPS_OECD_18032022.csv")

# Get unique countries
eps_countries <- unique(eps$Country)
eps_countrycodes <- manystates::code_states(eps_countries, abbrev = T)
eps_countrycodes[[1]] <- "GRC" # temp fix for greek regex oddity

# Filter out the aggregate index and plot it in time:
eps_aggregate <- eps %>%
  dplyr::filter(VAR == "EPS")

names(eps_aggregate)[1] <- "COU"

eps_aggregate <- eps_aggregate %>%
  dplyr::mutate(iso2c = countrycode::countrycode(COU,
                                                 "iso3c",
                                                 "iso2c",
                                                 custom_match = (
                                                   c("EU27_2020" = "EU27_2020",
                                                     "OECD" = "OECD")
                                                 ))) %>%
  dplyr::rename(iso3c = COU,
                COU = iso2c)

saveRDS(eps_aggregate, file = paste0(save_path, "eps.rds"))
# # Vizualizing EPS:
# ggplot2::ggplot(eps_aggregate, aes(x = Year, y = Value,
#                                    group = Country, color = Country)) +
#   ggplot2::geom_line(size = 1.05) +
#   hrbrthemes::theme_ipsum() +
#   ggplot2::theme(legend.key.size = unit(0.1, "cm"),
#         legend.text = element_text(size = 8)) +
#   ggplot2::labs(title = "Environmental Policy Stringency Index 1990-2015")
