################################################################################
############## Create Yearly One Mode Centrality Scores ########################
################################################################################

library(manyenviron)
library(tidyverse)
library(migraph)

# Import IEADB and put into a list ----

# How many years? 26 1990-2015
# EPS range is 1990-2015 --> Extract observations for the years prior to it
# before adding the additional observations year by year.
ieadb_lst <- list()
for (i in seq(26)) {
  ieadb_lst[[i]] <- manyenviron::memberships$IEADB_MEM %>%
    dplyr::mutate(year = as.numeric(stringr::str_extract(manyID, "[:digit:]{4}"))) %>%
    dplyr::filter(year <= 1990 + i) %>%
    dplyr::arrange(year)
}
names(ieadb_lst) <- as.character(1990:2015)

# Convert to a list of networks
ieadb_net_lst <- lapply(ieadb_lst, migraph::as_tidygraph, twomode = TRUE)
# Check if all subsets are bipartite --> Needs to be true
all(unlist(lapply(ieadb_net_lst, migraph::is_bipartite)))
# Perform a one mode projection to simplify things
ieadb_net_lst_onemode <- lapply(ieadb_net_lst, migraph::project_cols)
# Check if everything is unimodal -> Needs to be false
all(unlist(lapply(ieadb_net_lst_onemode, migraph::is_bipartite)))

####################### Compute Centrality Scores ##############################

# Degree (strength, since its a weighted network)
ieadb_net_lst_onemode_degree_centrality <- lapply(ieadb_net_lst_onemode,
                                                  igraph::strength)

# Closeness --> 22 of the 26 years have disconnected components
ieadb_net_lst_onemode_closeness_centrality <- lapply(ieadb_net_lst_onemode,
                                                  igraph::closeness)

# Eigenvector
ieadb_net_lst_onemode_eigenvector_centrality <- lapply(ieadb_net_lst_onemode,
                                                  igraph::eigen_centrality,
                                                  directed = TRUE)
# Betweenness
ieadb_net_lst_onemode_betweeness_centrality <- lapply(ieadb_net_lst_onemode,
                                                  igraph::betweenness,
                                                  directed = TRUE)
# Note that for theoretical reasons, we're more interested in the eigenvector
# centrality measure since the higher the score of a node, the more integrated
# it is in the core of the network.

################ Transform centrality vectors into a dataframe #################

ieadb_eigen_panel <- data.frame(Country = seq(196))
for (i in seq(26)) {
  ieadb_eigen_panel[ , ncol(ieadb_eigen_panel) + 1] <- ieadb_net_lst_onemode_eigenvector_centrality[[i]]$vector
  colnames(ieadb_eigen_panel)[ncol(ieadb_eigen_panel)] <- paste0(seq(1990, 2015)[[i]])
}
# Check if names are equal across projections
all(names(ieadb_net_lst_onemode_eigenvector_centrality[[1]]$vector) ==
  names(ieadb_net_lst_onemode_eigenvector_centrality[[2]]$vector))
all(names(ieadb_net_lst_onemode_eigenvector_centrality[[2]]$vector) ==
      names(ieadb_net_lst_onemode_eigenvector_centrality[[3]]$vector))
all(names(ieadb_net_lst_onemode_eigenvector_centrality[[15]]$vector) ==
      names(ieadb_net_lst_onemode_eigenvector_centrality[[16]]$vector))
# They are ! Manually checked --> Append the names to the df
ieadb_eigen_panel$Country <-
  names(ieadb_net_lst_onemode_eigenvector_centrality[[1]]$vector)
# Some names need fixing:
# manyenviron::memberships$IEADB_MEM[manyenviron::memberships$IEADB_MEM$CountryID == "GRC_EC", ]
ieadb_eigen_panel$Country[[32]] <- "GRC" # Greece

# manyenviron::memberships$IEADB_MEM[manyenviron::memberships$IEADB_MEM$CountryID == "KOR_PRK", ]
ieadb_eigen_panel$Country[[91]] <- "PRK" # Peoples Republic of Korea

# manyenviron::memberships$IEADB_MEM[manyenviron::memberships$IEADB_MEM$CountryID == "SWA_SWZ", ]
ieadb_eigen_panel$Country[[134]] <- "SWZ" # Eswatini

# manyenviron::memberships$IEADB_MEM[manyenviron::memberships$IEADB_MEM$CountryID == "GIN_GNQ", ]
ieadb_eigen_panel$Country[[154]] <- "GNQ" # Equatorial Guinea

# manyenviron::memberships$IEADB_MEM[manyenviron::memberships$IEADB_MEM$CountryID == "GIN_GNQ", ]
ieadb_eigen_panel$Country[[154]] <- "SSN" # South Sudan

# Now let's subset to get only OECD countries
oecd <- read.csv("data_raw/oecd.csv")

ieadb_eigen_OECD <- ieadb_eigen_panel[ieadb_eigen_panel$Country %in% oecd$Code, ]

# Order
ieadb_eigen_OECD <- ieadb_eigen_OECD[order(ieadb_eigen_OECD$Country), ]

# Pivot longer
ieadb_eigen_OECD <- tidyr::pivot_longer(ieadb_eigen_OECD, where(is.numeric),
                                        names_to = "Year")
ieadb_eigen_OECD$Year <- as.integer(ieadb_eigen_OECD$Year)
# Add iso2c codes for joins
ieadb_eigen_OECD$iso2c <- countrycode::countrycode(ieadb_eigen_OECD$Country,
                                                   "iso3c",
                                                   "iso2c")

# Plot Eigenvector centrality:

ggplot2::ggplot(ieadb_eigen_OECD, aes(x = Year, y = value,
                                      group = Country, color = Country)) +
  ggplot2::geom_line() +
  labs(title = "Eigenvector Centrality Score of One-Mode Network over time")

ggsave(filename = "graphs/ExploratoryAnalysis/EigenvectorCentrality.jpeg")

######################################################################
### Relationship between Centrality indicators and EPS ###############
######################################################################
# Note: this section requires you to run the EPS import script
names(eps_aggregate)[[1]] <- "ccode"
eps_IEADB <- dplyr::inner_join(eps_aggregate, ieadb_eigen_OECD,
                               by = c("ccode" = "Country", "Year" = "Year")) # Joins on country/year
eps_IEADB <- dplyr::rename(eps_IEADB, EPS = Value, Eigen = value)

cor.test(eps_IEADB$EPS, eps_IEADB$Eigen, method = "spearman")


plm::plm(EPS ~ Eigen, data = eps_IEADB,
         index = c("ccode", "Year"))
