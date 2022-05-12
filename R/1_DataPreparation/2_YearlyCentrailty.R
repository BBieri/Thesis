################################################################################
############## Create Yearly One Mode Centrality Scores ########################
################################################################################

library(tidyverse)
library(igraph)
library(migraph)
library(backbone)
library(countrycode)
library(plotly)
loadpath <- "data/ECOLEX/Membership/"
save_path <- "data/CleanData/"

# Import ECOLEX and put into a list ----

# Full Network:

load(paste0(loadpath, "ECOLEX_Membership.rds")) # ECOLEX_mems

# There is a little bit more cleaning to do it turns out:
# Some countries are not standard


# How many years? 71 1948-2018
# We'll follow Caratini et al. by extracting the network of the modern period
# only aka post 1948.
ecolex_lst <- list()
for (i in seq(length(unique(as.numeric(stringr::str_extract(ECOLEX_mems$Beg, "[:digit:]{4}")))))) {
  ecolex_lst[[i]] <- ECOLEX_mems %>%
    dplyr::mutate(year_beg = as.numeric(stringr::str_extract(Beg, "[:digit:]{4}")),
                  year_end = as.numeric(stringr::str_extract(End, "[:digit:]{4}"))) %>%
    dplyr::filter(year_beg <= 1947 + i & year_end >= 1947 + i | # Future end date
                    year_beg <= 1947 + i & is.na(year_end)) %>% # No end date
    dplyr::arrange(year_beg) %>%
    dplyr::select(ecolexID, CountryID)
}
names(ecolex_lst) <- as.character(1948:2018)

# Convert to igraph object ----
ecolex_lst_igraph <- lapply(ecolex_lst, migraph::as_igraph,
                            twomode = TRUE) |>
  lapply(igraph::as.undirected)
# Add a network argument
# Check if all subsets are bipartite --> Needs to be true
all(unlist(lapply(ecolex_lst_igraph,
                  migraph::is_bipartite)))
# Naive projection will yield an overly dense network
ecolex_lst_igraph_onemode <- lapply(ecolex_lst_igraph,
                                    migraph::to_mode2)
# Check if everything is unimodal -> Needs to be false
all(unlist(lapply(ecolex_lst_igraph_onemode, migraph::is_bipartite)))
# Plot the naive projection of the bipartite network for 1948
plot(ecolex_lst_igraph_onemode$`2010`)
# Extract the bipartite incidence matrix ----
# Cols are countries, rows are agreements
ecolex_lst_incidence <- lapply(ecolex_lst_igraph,
                               igraph::get.incidence) |>
  lapply(t)
# This needs to be reversed to run SDSM, lets transpose things to get
# Counties (agents) on rows and treaties (artifacts) on the edges.

# Now let us compute the adjacency matrix of all yearly one-mode snapshots
# to correct them after computing the backbone.
ecolex_lst_onemode_adj <- lapply(ecolex_lst_igraph_onemode,
                               igraph::get.adjacency,
                               attr = "weight",
                               sparse = F)

# Use {backbone} to avoid overly dense networks due to the projection
# Let's use the FDSM algorithm
# ecolex_lst_fdsm <- lapply(ecolex_lst_incidence, backbone::fdsm, alpha = 0.05,
#                           signed = FALSE, narrative = TRUE)
# Let's use the SDSM algorithm (BiCM) with \alpha = 0.13 which
# is equivalent to FDFM @ 0.05 percent (see Neal 2021)
# This is mainly due to the fact that the fatter tails of
# the underlying probability distribution lead to an
# algorithm with less power.
# E.g. it over-rejects the presence of an edge
ecolex_lst_sdsm <- lapply(ecolex_lst_incidence, backbone::sdsm, alpha = 0.13,
                          method = "DivideFFT",
                          signed = FALSE, narrative = TRUE)
# Test plot
plot(as.undirected(graph_from_adjacency_matrix(ecolex_lst_sdsm[[71]], weighted = T)))

# Keep only significant edges:
ecolex_lst_onemode_sdsm <- ecolex_lst_onemode_adj
for (i in seq(length(ecolex_lst_sdsm))) { # 71 years
  ecolex_lst_onemode_sdsm[[i]][ecolex_lst_sdsm[[i]] == 0] <- 0
}
all(unlist(lapply(ecolex_lst_onemode_sdsm, isSymmetric))) # Matrices are all symmetric
ecolex_lst_onemode_sdsm_igraph <- list()
for (i in seq(length(ecolex_lst_sdsm))) { # 71 years
  # Convert corrected network projection back to igraph object
  ecolex_lst_onemode_sdsm_igraph[[i]] <- igraph::graph_from_adjacency_matrix(ecolex_lst_onemode_sdsm[[i]], weighted = T, mode = "undirected")
}
is.directed(ecolex_lst_onemode_sdsm_igraph[[70]]) # False as expected
names(ecolex_lst_onemode_sdsm_igraph) <- as.character(1948:2018)

# Some intermediary plots to highlight the backbone extraction process:
l <- layout.fruchterman.reingold(as.undirected(ecolex_lst_onemode_sdsm_igraph$`2010`))
par(mfrow = c(1, 3))
plot(ecolex_lst_igraph_onemode$`2010`,
     vertex.size = 1,
     edge.width = 0.5,
     vertex.label = NA,
     layout = l,
     main = "2010 Cooperation Network")
plot(as.undirected(igraph::graph_from_adjacency_matrix(ecolex_lst_sdsm$`2010`)),
     vertex.size = 1,
     edge.width = 0.5,
     vertex.label = NA,
     layout = l,
     main = "2010 Cooperation Network Backbone")
plot(as.undirected(ecolex_lst_onemode_sdsm_igraph$`2010`),
     vertex.size = 1,
     edge.width = 0.5,
     vertex.label = NA,
     layout = l,
     main = "2010 Cooperation Network\n(Backbone Corrected)")
dev.off()


####################### Compute Centrality Scores ##############################

library(netrankr) # Version 1.1.1

# Comparing centrality measures in a graph
# cent_scores <- data.frame(
#   degree = degree(ecolex_lst_onemode_sdsm_igraph$`2010`),
#   betweenness = round(betweenness(ecolex_lst_onemode_sdsm_igraph$`2010`),4),
#   closeness = round(closeness(ecolex_lst_onemode_sdsm_igraph$`2010`),4),
#   eigenvector = round(eigen_centrality(ecolex_lst_onemode_sdsm_igraph$`2010`)$vector,4),
#   subgraph = round(subgraph_centrality(ecolex_lst_onemode_sdsm_igraph$`2010`),4))
# apply(cent_scores,2,which.max)
# P <- neighborhood_inclusion(ecolex_lst_onemode_sdsm_igraph$`2010`)
# plot(rank_intervals(P),cent_scores = cent_scores,ties.method="average")
#
# netrankr::rank_intervals(P)


# Degree (strength, since its a weighted network)
ecolex_net_lst_onemode_strength_centrality <- lapply(ecolex_lst_onemode_sdsm_igraph,
                                                  igraph::strength)

# Closeness --> 22 of the 26 years have disconnected components
ecolex_net_lst_onemode_closeness_centrality <- lapply(ecolex_lst_onemode_sdsm_igraph,
                                                     igraph::closeness)

# Eigenvector
ecolex_net_lst_onemode_eigenvector_centrality <- lapply(ecolex_lst_onemode_sdsm_igraph,
                                                       igraph::eigen_centrality,
                                                       directed = FALSE,
                                                       scale = TRUE, # Max eigenvect = 1
                                                       weights = NULL) # weight attribute of igraph object is used
# Betweenness
ecolex_net_lst_onemode_betweeness_centrality <- lapply(ecolex_lst_onemode_sdsm_igraph,
                                                      igraph::betweenness,
                                                      directed = FALSE,
                                                      weight = NULL) # weight attribute of igraph object is used
# Subgraph
ecolex_net_lst_onemode_subgraph_centrality <- lapply(ecolex_lst_onemode_sdsm_igraph,
                                                      igraph::subgraph.centrality)
# Note that for theoretical reasons, we're more interested in the eigenvector
# centrality measure since we would like to characterize its embeddedness in
# the international governance network i.e. the propensity to be connected
# to other "popular" nodes.

################ Transform centrality vectors into a dataframe #################

# Initialize wide dataframes
ecolex_strength_panel <- ecolex_closeness_panel <- ecolex_eigen_panel <- ecolex_betweeness_panel <- ecolex_subgraph_panel <- data.frame(Country = unique(ECOLEX_mems$CountryID))

# Eigenvector centrality (normalized to [0;1] score)
e <- list()
for (i in seq(length(ecolex_lst))) { # 71 years
  # Extract sorted vector by country name
  e[[i]] <- ecolex_net_lst_onemode_eigenvector_centrality[[i]]$vector[order(names(ecolex_net_lst_onemode_eigenvector_centrality[[i]]$vector))]
  # Extract Country Names
  ecolex_eigen_panel$i <- e[[i]][match(ecolex_eigen_panel$Country, names(e[[i]]))]
  # Name columns
  colnames(ecolex_eigen_panel)[ncol(ecolex_eigen_panel)] <- paste0(seq(1948, 2018)[[i]])
}
rm(e)
# Strength centrality (non-normalized)
e <- list()
for (i in seq(length(ecolex_lst))) { # 71 years
  # Extract sorted vector by country name
  e[[i]] <- ecolex_net_lst_onemode_strength_centrality[[i]][order(names(ecolex_net_lst_onemode_strength_centrality[[i]]))]
  # Extract Country Names
  ecolex_strength_panel$i <- e[[i]][match(ecolex_strength_panel$Country, names(e[[i]]))]
  # Name columns
  colnames(ecolex_strength_panel)[ncol(ecolex_strength_panel)] <- paste0(seq(1948, 2018)[[i]])
}
rm(e)
# Closeness centrality (normalized)
e <- list()
for (i in seq(length(ecolex_lst))) { # 71 years
  # Extract sorted vector by country name
  e[[i]] <- ecolex_net_lst_onemode_closeness_centrality[[i]][order(names(ecolex_net_lst_onemode_closeness_centrality[[i]]))]
  # Extract Country Names
  ecolex_closeness_panel$i <- e[[i]][match(ecolex_closeness_panel$Country, names(e[[i]]))]
  # Name columns
  colnames(ecolex_closeness_panel)[ncol(ecolex_closeness_panel)] <- paste0(seq(1948, 2018)[[i]])
}
rm(e)
# Betweeness centrality (Not normalized)----
e <- list()
for (i in seq(length(ecolex_lst))) { # 71 years
  # Extract sorted vector by country name
  e[[i]] <- ecolex_net_lst_onemode_betweeness_centrality[[i]][order(names(ecolex_net_lst_onemode_betweeness_centrality[[i]]))]
  # Extract Country Names
  ecolex_betweeness_panel$i <- e[[i]][match(ecolex_betweeness_panel$Country, names(e[[i]]))]
  # Name columns
  colnames(ecolex_betweeness_panel)[ncol(ecolex_betweeness_panel)] <- paste0(seq(1948, 2018)[[i]])
}
rm(e)
# Subgraph centrality (not normalized) ----
e <- list()
for (i in seq(length(ecolex_lst))) { # 71 years
  # Extract sorted vector by country name
  e[[i]] <- ecolex_net_lst_onemode_subgraph_centrality[[i]][order(names(ecolex_net_lst_onemode_subgraph_centrality[[i]]))]
  # Extract Country Names
  ecolex_subgraph_panel$i <- e[[i]][match(ecolex_subgraph_panel$Country, names(e[[i]]))]
  # Name columns
  colnames(ecolex_subgraph_panel)[ncol(ecolex_subgraph_panel)] <- paste0(seq(1948, 2018)[[i]])
}
rm(e)

############################ Subset OECD Countries #############################

# Now let's subset to get only OECD countries for every centrality panel
oecd <- read.csv("data_raw/oecd.csv")

ecolex_eigen_OECD <- ecolex_eigen_panel[ecolex_eigen_panel$Country %in% oecd$Code, ]
ecolex_strength_OECD <- ecolex_strength_panel[ecolex_strength_panel$Country %in% oecd$Code, ]
ecolex_closeness_OECD <- ecolex_closeness_panel[ecolex_closeness_panel$Country %in% oecd$Code, ]
ecolex_betweeness_OECD <- ecolex_betweeness_panel[ecolex_betweeness_panel$Country %in% oecd$Code, ]
ecolex_subgraph_OECD <- ecolex_subgraph_panel[ecolex_subgraph_panel$Country %in% oecd$Code, ]

# Check that we have all OECD countries in our panel:
all(c(nrow(ecolex_eigen_OECD) == 38, nrow(ecolex_strength_OECD) == 38,
  nrow(ecolex_closeness_OECD) == 38, nrow(ecolex_betweeness_OECD) == 38,
  nrow(ecolex_subgraph_OECD) == 38)) # TRUE

# Order countries alphabetically
ecolex_eigen_OECD <- ecolex_eigen_OECD[order(ecolex_eigen_OECD$Country), ]
ecolex_strength_OECD <- ecolex_strength_OECD[order(ecolex_strength_OECD$Country), ]
ecolex_closeness_OECD <- ecolex_closeness_OECD[order(ecolex_closeness_OECD$Country), ]
ecolex_betweeness_OECD <- ecolex_betweeness_OECD[order(ecolex_betweeness_OECD$Country), ]
ecolex_subgraph_OECD <- ecolex_subgraph_OECD[order(ecolex_subgraph_OECD$Country), ]

# Pivot longer
OECD_centrality <- list(ecolex_eigen_OECD, ecolex_strength_OECD,
                        ecolex_closeness_OECD, ecolex_betweeness_OECD,
                        ecolex_subgraph_OECD)

OECD_centrality <- lapply(OECD_centrality,
                          tidyr::pivot_longer,
                          where(is.numeric),
                          names_to = "Year")
for (i in length(OECD_centrality)) {
  OECD_centrality[[i]]$Year <- as.integer(OECD_centrality[[i]]$Year)
  # Add iso2c codes for joins
  OECD_centrality[[i]]$iso2c <-
    countrycode::countrycode(OECD_centrality[[i]]$Country, "iso3c", "iso2c")
}
names(OECD_centrality) <- c("Eigenvector",
                            "Strength",
                            "Closeness",
                            "Betweeness",
                            "Subgraph")

############################## Save OECD Countries #############################
for (i in seq(length(names(OECD_centrality)))) {
  saved <- OECD_centrality[[i]]
  save(saved, file = paste0(save_path, "OECD_Panel_",
                            names(OECD_centrality)[[i]], ".rds"))
}

######################### Descriptive plots centrality #########################

# Plot Eigenvector centrality:
eigen_plot <- ggplot2::ggplot(OECD_centrality$Eigenvector, aes(x = Year, y = value,
                                      group = Country, color = Country)) +
  ggplot2::geom_line() +
  labs(title = "Eigenvector Centrality Score of One-Mode Network over time")

eigen_plotly <- plotly::ggplotly(eigen_plot)

ggsave(filename = "graphs/Descriptive/EigenvectorCentrality.jpeg")


# Plot Strength centrality:
strength_plot <- ggplot2::ggplot(OECD_centrality$Strength, aes(x = Year, y = value,
                                                               group = Country, color = Country)) +
  ggplot2::geom_line() +
  labs(title = "Strength Centrality Score of One-Mode Network over time")

strength_plotly <- plotly::ggplotly(strength_plot)

ggsave(filename = "graphs/Descriptive/StrengthCentrality.jpeg")


# Plot Closeness centrality:
close_plot <- ggplot2::ggplot(OECD_centrality$Closeness, aes(x = Year, y = value,
                                                               group = Country, color = Country)) +
  ggplot2::geom_line() +
  labs(title = "Closeness Centrality Score of One-Mode Network over time")

close_plotly <- plotly::ggplotly(close_plot)

ggsave(filename = "graphs/Descriptive/ClosenessCentrality.jpeg")


# Plot Betweeness centrality:
between_plot <- ggplot2::ggplot(OECD_centrality$Betweeness, aes(x = Year, y = value,
                                                               group = Country, color = Country)) +
  ggplot2::geom_line() +
  labs(title = "Betweenness Centrality Score of One-Mode Network over time")

between_plotly <- plotly::ggplotly(between_plot)

ggsave(filename = "graphs/Descriptive/BetweennessCentrality.jpeg")


# Plot Subgraph centrality:
subgraph_plot <- ggplot2::ggplot(OECD_centrality$Subgraph, aes(x = Year, y = value,
                                                               group = Country, color = Country)) +
  ggplot2::geom_line() +
  labs(title = "Subgraph Centrality Score of One-Mode Network over time")

subgraph_plotly <- plotly::ggplotly(subgraph_plot)

ggsave(filename = "graphs/Descriptive/SubgraphCentrality.jpeg")

######################################################################
### Relationship between Centrality indicators and EPS ###############
######################################################################
# Note: this section requires you to run the EPS import script
# names(eps_aggregate)[[1]] <- "ccode"
# eps_IEADB <- dplyr::inner_join(eps_aggregate, ieadb_eigen_OECD,
#                                by = c("ccode" = "Country", "Year" = "Year")) # Joins on country/year
# eps_IEADB <- dplyr::rename(eps_IEADB, EPS = Value, Eigen = value)
#
# cor.test(eps_IEADB$EPS, eps_IEADB$Eigen, method = "spearman")
#
#
# plm::plm(EPS ~ Eigen, data = eps_IEADB,
#          index = c("ccode", "Year"))
