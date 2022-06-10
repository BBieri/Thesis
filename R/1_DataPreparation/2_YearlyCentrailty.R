################################################################################
############## Create Yearly One Mode Centrality Scores ########################
################################################################################

library(tidyverse) # 1.3.1
library(igraph) # 1.3.1
library(migraph) # 0.9.3
library(backbone) # 2.0.3
library(countrycode) # 1.3.1
library(plotly) # 4.10.0
loadpath <- "data/ECOLEX/Membership/"
save_path <- "data/CleanData/"

# Import ECOLEX and put into a list ----

# Full Network:

load(paste0(loadpath, "ECOLEX_Membership.rds")) # ECOLEX_mems

# How many years? 71 1948-2018
# We'll follow Caratini et al. by extracting the network of the modern period
# only aka post 1948.
ecolex_lst <- list()
for (i in seq(length(unique(as.numeric(stringr::str_extract(ECOLEX_mems$Beg, "[:digit:]{4}")))))) {
  ecolex_lst[[i]] <- ECOLEX_mems %>%
    dplyr::filter(!is.na(Force)) %>% # Remove treaties which are not into force (910 membership actions)
    dplyr::mutate(year_beg = as.numeric(stringr::str_extract(Force, "[:digit:]{4}")),
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
# Compute bipartite degree metric ----
ecolex_lst_bipartite_degree <- lapply(ecolex_lst_igraph,
                                      igraph::degree)
# Compute bipartite eigenvector metric ----
# ecolex_lst_bipartite_eigenvector<- lapply(ecolex_lst_igraph)

# Naive projection will yield an overly dense network ----
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

#############################################################################
# Use {backbone} to avoid overly dense networks due to the projection #######
#############################################################################

# #############################################################
# Let's use the Fixed Row algorithm (Fixed country degrees)----
# #############################################################

# Note: The following code runs the Fixed Row algorithm that fixed the degree
# of each agent (country) exactly. It is a more sophisticated BiPCM strategy
# which sets the restrictions exactly instead of on average. The agents'
# degrees are considered instead of the artifacts since they have agency
# over how many agreements to sign.

ecolex_lst_frow <- lapply(ecolex_lst_incidence, backbone::fixedrow,
                          alpha = 0.05, signed = FALSE, narrative = TRUE)
# Keep only significant edges:
ecolex_lst_onemode_frow <- ecolex_lst_onemode_adj
for (i in seq(length(ecolex_lst_frow))) { # 71 years
  ecolex_lst_onemode_frow[[i]][ecolex_lst_frow[[i]] == 0] <- 0
}
# Convert corrected network projection back to igraph object
ecolex_lst_onemode_frow_igraph <- list()
for (i in seq(length(ecolex_lst_frow))) { # 71 years
  ecolex_lst_onemode_frow_igraph[[i]] <- igraph::graph_from_adjacency_matrix(ecolex_lst_onemode_frow[[i]],
                                                                             weighted = T,
                                                                             mode = "undirected")
}
is.directed(ecolex_lst_onemode_frow_igraph[[70]]) # False as expected
names(ecolex_lst_onemode_frow_igraph) <- as.character(1948:2018)

# #################################
# Let's use the FDSM algorithm ----
# #################################

# Note: The following code runs rather slowly due to the nature of the
# the constraints that are imposed on the simulated network. This technique,
# simulates about 80000 bipartite networks by keeping the sum of rows and the
# sum of each individual column (e.g. the country and the treaty) degrees
# constant.

# ecolex_lst_fdsm <- lapply(ecolex_lst_incidence, backbone::fdsm, alpha = 0.05,
#                           signed = FALSE, narrative = TRUE)
# Keep only significant edges:
# ecolex_lst_onemode_fdsm <- ecolex_lst_onemode_adj
# for (i in seq(length(ecolex_lst_fdsm))) { # 71 years
#   ecolex_lst_onemode_fdsm[[i]][ecolex_lst_fdsm[[i]] == 0] <- 0
# }
# all(unlist(lapply(ecolex_lst_onemode_fdsm, isSymmetric))) # Matrices are all symmetric
# ecolex_lst_onemode_fdsm_igraph <- list()
# for (i in seq(length(ecolex_lst_fdsm))) { # 71 years
#   # Convert corrected network projection back to igraph object
#   ecolex_lst_onemode_fdsm_igraph[[i]] <- igraph::graph_from_adjacency_matrix(ecolex_lst_onemode_fdsm[[i]], weighted = T, mode = "undirected")
# }
# is.directed(ecolex_lst_onemode_fdsm_igraph[[70]]) # False as expected
# names(ecolex_lst_onemode_fdsm_igraph) <- as.character(1948:2018)


# #################################
# Let's use the SDSM algorithm ----
# #################################

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

# Mini {netrankr} test drive

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


####################################
# For Fixed Row corrected projection
####################################

# Degree (strength, since its a weighted network)
ecolex_net_lst_onemode_strength_centrality_frow <- lapply(ecolex_lst_onemode_frow_igraph,
                                                     igraph::strength)

# Closeness --> 22 of the 26 years have disconnected components
ecolex_net_lst_onemode_closeness_centrality_frow <- lapply(ecolex_lst_onemode_frow_igraph,
                                                      igraph::closeness)

# Eigenvector
ecolex_net_lst_onemode_eigenvector_centrality_frow <- lapply(ecolex_lst_onemode_frow_igraph,
                                                        igraph::eigen_centrality,
                                                        directed = FALSE,
                                                        scale = TRUE, # Max eigenvect = 1
                                                        weights = NULL) # weight attribute of igraph object is used
# Betweenness
ecolex_net_lst_onemode_betweeness_centrality_frow <- lapply(ecolex_lst_onemode_frow_igraph,
                                                       igraph::betweenness,
                                                       directed = FALSE,
                                                       weight = NULL) # weight attribute of igraph object is used
# Subgraph
ecolex_net_lst_onemode_subgraph_centrality_frow <- lapply(ecolex_lst_onemode_frow_igraph,
                                                     igraph::subgraph.centrality)
# Note that for theoretical reasons, we're interested in both the degree and the
# eigenvector centrality measure since we would like to characterize its
# embeddedness in the international governance network i.e. the propensity to be
# connected to other "popular" nodes when corrected for own connections.

# ###############################
# # For FDSM corrected projection
# ###############################
#
# # Degree (strength, since its a weighted network)
# ecolex_net_lst_onemode_strength_centrality_fdsm <- lapply(ecolex_lst_onemode_fdsm_igraph,
#                                                      igraph::strength)
#
# # Closeness --> 22 of the 26 years have disconnected components
# ecolex_net_lst_onemode_closeness_centrality_fdsm <- lapply(ecolex_lst_onemode_fdsm_igraph,
#                                                       igraph::closeness)
#
# # Eigenvector
# ecolex_net_lst_onemode_eigenvector_centrality_fdsm <- lapply(ecolex_lst_onemode_fdsm_igraph,
#                                                         igraph::eigen_centrality,
#                                                         directed = FALSE,
#                                                         scale = TRUE, # Max eigenvect = 1
#                                                         weights = NULL) # weight attribute of igraph object is used
# # Betweenness
# ecolex_net_lst_onemode_betweeness_centrality_fdsm <- lapply(ecolex_lst_onemode_fdsm_igraph,
#                                                        igraph::betweenness,
#                                                        directed = FALSE,
#                                                        weight = NULL) # weight attribute of igraph object is used
# # Subgraph
# ecolex_net_lst_onemode_subgraph_centrality_fdsm <- lapply(ecolex_lst_onemode_fdsm_igraph,
#                                                    igraph::subgraph.centrality)

# Note that for theoretical reasons, we're interested in both the degree and the
# eigenvector centrality measure since we would like to characterize its
# embeddedness in the international governance network i.e. the propensity to be
# connected to other "popular" nodes when corrected for own connections.

###############################
# For SDSM corrected projection
###############################

# Degree (strength, since its a weighted network)
ecolex_net_lst_onemode_strength_centrality_sdsm <- lapply(ecolex_lst_onemode_sdsm_igraph,
                                                  igraph::strength)

# Closeness --> 22 of the 26 years have disconnected components
ecolex_net_lst_onemode_closeness_centrality_sdsm <- lapply(ecolex_lst_onemode_sdsm_igraph,
                                                     igraph::closeness)

# Eigenvector
ecolex_net_lst_onemode_eigenvector_centrality_sdsm <- lapply(ecolex_lst_onemode_sdsm_igraph,
                                                       igraph::eigen_centrality,
                                                       directed = FALSE,
                                                       scale = TRUE, # Max eigenvect = 1
                                                       weights = NULL) # weight attribute of igraph object is used
# Betweenness
ecolex_net_lst_onemode_betweeness_centrality_sdsm <- lapply(ecolex_lst_onemode_sdsm_igraph,
                                                      igraph::betweenness,
                                                      directed = FALSE,
                                                      weight = NULL) # weight attribute of igraph object is used
# Subgraph
ecolex_net_lst_onemode_subgraph_centrality_sdsm <- lapply(ecolex_lst_onemode_sdsm_igraph,
                                                      igraph::subgraph.centrality)
# Note that for theoretical reasons, we're interested in both the degree and the
# eigenvector centrality measure since we would like to characterize its
# embeddedness in the international governance network i.e. the propensity to be
# connected to other "popular" nodes when corrected for own connections.

################ Transform centrality vectors into a dataframe #################

# Create a quick function for it:
# Requires ECOLEX_mems df to be loaded in the environment

centralitylst_centralitypanel <- function(lst){
  # DF init
  df <- data.frame(Country = unique(ECOLEX_mems$CountryID))
  # Eigenvector centrality requires a different conversion process
  if (grepl("eigen", deparse(substitute(lst)))) {
    e <- list()
    for (i in seq(length(lst))) { # 71 years
      # Extract sorted vector by country name
      e[[i]] <- lst[[i]]$vector[order(names(lst[[i]]$vector))]
      # Extract Country Names
      df$i <- e[[i]][match(df$Country, names(e[[i]]))]
      # Name columns
      colnames(df)[ncol(df)] <- paste0(seq(1948, 2018)[[i]])
    }
  } else {
    e <- list()
    for (i in seq(length(lst))) { # 71 years
      # Extract sorted vector by country name
      e[[i]] <- lst[[i]][order(names(lst[[i]]))]
      # Extract Country Names
      df$i <- e[[i]][match(df$Country, names(e[[i]]))]
      # Name columns
      colnames(df)[ncol(df)] <- paste0(seq(1948, 2018)[[i]])
    }
  }
  return(df)
}

# Apply the function to every panel:

# FROW
ecolex_eigenvector_panel_frow <-
  centralitylst_centralitypanel(ecolex_net_lst_onemode_eigenvector_centrality_frow)
ecolex_strength_panel_frow <-
  centralitylst_centralitypanel(ecolex_net_lst_onemode_strength_centrality_frow)
ecolex_betweenness_panel_frow <-
  centralitylst_centralitypanel(ecolex_net_lst_onemode_betweeness_centrality_frow)
ecolex_closeness_panel_frow <-
  centralitylst_centralitypanel(ecolex_net_lst_onemode_closeness_centrality_frow)
ecolex_subgraph_panel_frow <-
  centralitylst_centralitypanel(ecolex_net_lst_onemode_subgraph_centrality_frow)

# # FDSM
# ecolex_eigenvector_panel_fdsm <-
#   centralitylst_centralitypanel(ecolex_net_lst_onemode_eigenvector_centrality_fdsm)
# ecolex_strength_panel_fdsm <-
#   centralitylst_centralitypanel(ecolex_net_lst_onemode_strength_centrality_fdsm)
# ecolex_betweenness_panel_fdsm <-
#   centralitylst_centralitypanel(ecolex_net_lst_onemode_betweenness_centrality_fdsm)
# ecolex_closeness_panel_fdsm <-
#   centralitylst_centralitypanel(ecolex_net_lst_onemode_closeness_centrality_fdsm)
# ecolex_subgraph_panel_fdsm <-
#   centralitylst_centralitypanel(ecolex_net_lst_onemode_subgraph_centrality_fdsm)

# SDSM
ecolex_eigenvector_panel_sdsm <-
  centralitylst_centralitypanel(ecolex_net_lst_onemode_eigenvector_centrality_sdsm)
ecolex_strength_panel_sdsm <-
  centralitylst_centralitypanel(ecolex_net_lst_onemode_strength_centrality_sdsm)
ecolex_betweenness_panel_sdsm <-
  centralitylst_centralitypanel(ecolex_net_lst_onemode_betweeness_centrality_sdsm)
ecolex_closeness_panel_sdsm <-
  centralitylst_centralitypanel(ecolex_net_lst_onemode_closeness_centrality_sdsm)
ecolex_subgraph_panel_sdsm <-
  centralitylst_centralitypanel(ecolex_net_lst_onemode_subgraph_centrality_sdsm)


############################ Subset OECD Countries #############################

# Now let's subset to get only OECD countries for every centrality panel
oecd <- read.csv("data_raw/oecd.csv")

# FROW

ecolex_eigen_OECD_frow <- ecolex_eigenvector_panel_frow[ecolex_eigenvector_panel_frow$Country %in% oecd$Code, ]
ecolex_strength_OECD_frow <- ecolex_strength_panel_frow[ecolex_strength_panel_frow$Country %in% oecd$Code, ]
ecolex_closeness_OECD_frow <- ecolex_closeness_panel_frow[ecolex_closeness_panel_frow$Country %in% oecd$Code, ]
ecolex_betweenness_OECD_frow <- ecolex_betweenness_panel_frow[ecolex_betweenness_panel_frow$Country %in% oecd$Code, ]
ecolex_subgraph_OECD_frow <- ecolex_subgraph_panel_frow[ecolex_subgraph_panel_frow$Country %in% oecd$Code, ]

# FDSM
# ecolex_eigen_OECD_fdsm <- ecolex_eigenvector_panel_fdsm[ecolex_eigenvector_panel_fdsm$Country %in% oecd$Code, ]
# ecolex_strength_OECD_fdsm <- ecolex_strength_panel_fdsm[ecolex_strength_panel_fdsm$Country %in% oecd$Code, ]
# ecolex_closeness_OECD_fdsm <- ecolex_closeness_panel_fdsm[ecolex_closeness_panel_fdsm$Country %in% oecd$Code, ]
# ecolex_betweenness_OECD_fdsm <- ecolex_betweenness_panel_fdsm[ecolex_betweenness_panel_fdsm$Country %in% oecd$Code, ]
# ecolex_subgraph_OECD_fdsm <- ecolex_subgraph_panel_fdsm[ecolex_subgraph_panel_fdsm$Country %in% oecd$Code, ]

# SDSM
ecolex_eigen_OECD_sdsm <- ecolex_eigenvector_panel_sdsm[ecolex_eigenvector_panel_sdsm$Country %in% oecd$Code, ]
ecolex_strength_OECD_sdsm <- ecolex_strength_panel_sdsm[ecolex_strength_panel_sdsm$Country %in% oecd$Code, ]
ecolex_closeness_OECD_sdsm <- ecolex_closeness_panel_sdsm[ecolex_closeness_panel_sdsm$Country %in% oecd$Code, ]
ecolex_betweenness_OECD_sdsm <- ecolex_betweenness_panel_sdsm[ecolex_betweenness_panel_sdsm$Country %in% oecd$Code, ]
ecolex_subgraph_OECD_sdsm <- ecolex_subgraph_panel_sdsm[ecolex_subgraph_panel_sdsm$Country %in% oecd$Code, ]

# Check that we have all OECD countries in our panel:
all(c(nrow(ecolex_eigen_OECD_frow) == 38, nrow(ecolex_strength_OECD_frow) == 38,
  nrow(ecolex_closeness_OECD_frow) == 38, nrow(ecolex_betweenness_OECD_frow) == 38,
  nrow(ecolex_subgraph_OECD_frow) == 38)) # TRUE
# all(c(nrow(ecolex_eigen_OECD_fdsm) == 38, nrow(ecolex_strength_OECD_fdsm) == 38,
#       nrow(ecolex_closeness_OECD_fdsm) == 38, nrow(ecolex_betweenness_OECD_fdsm) == 38,
#       nrow(ecolex_subgraph_OECD_fdsm) == 38)) # TRUE
all(c(nrow(ecolex_eigen_OECD_sdsm) == 38, nrow(ecolex_strength_OECD_sdsm) == 38,
      nrow(ecolex_closeness_OECD_sdsm) == 38, nrow(ecolex_betweenness_OECD_sdsm) == 38,
      nrow(ecolex_subgraph_OECD_sdsm) == 38)) # TRUE

# Order countries alphabetically

# FROW
ecolex_eigen_OECD_frow <- ecolex_eigen_OECD_frow[order(ecolex_eigen_OECD_frow$Country), ]
ecolex_strength_OECD_frow <- ecolex_strength_OECD_frow[order(ecolex_strength_OECD_frow$Country), ]
ecolex_closeness_OECD_frow <- ecolex_closeness_OECD_frow[order(ecolex_closeness_OECD_frow$Country), ]
ecolex_betweenness_OECD_frow <- ecolex_betweenness_OECD_frow[order(ecolex_betweenness_OECD_frow$Country), ]
ecolex_subgraph_OECD_frow <- ecolex_subgraph_OECD_frow[order(ecolex_subgraph_OECD_frow$Country), ]

# FDSM
# ecolex_eigen_OECD_fdsm <- ecolex_eigen_OECD_fdsm[order(ecolex_eigen_OECD_fdsm$Country), ]
# ecolex_strength_OECD_fdsm <- ecolex_strength_OECD_fdsm[order(ecolex_strength_OECD_fdsm$Country), ]
# ecolex_closeness_OECD_fdsm <- ecolex_closeness_OECD_fdsm[order(ecolex_closeness_OECD_fdsm$Country), ]
# ecolex_betweenness_OECD_fdsm <- ecolex_betweenness_OECD_fdsm[order(ecolex_betweenness_OECD_fdsm$Country), ]
# ecolex_subgraph_OECD <- ecolex_subgraph_OECD[order(ecolex_subgraph_OECD$Country), ]

# SDSM
ecolex_eigen_OECD_sdsm <- ecolex_eigen_OECD_sdsm[order(ecolex_eigen_OECD_sdsm$Country), ]
ecolex_strength_OECD_sdsm <- ecolex_strength_OECD_sdsm[order(ecolex_strength_OECD_sdsm$Country), ]
ecolex_closeness_OECD_sdsm <- ecolex_closeness_OECD_sdsm[order(ecolex_closeness_OECD_sdsm$Country), ]
ecolex_betweenness_OECD_sdsm <- ecolex_betweenness_OECD_sdsm[order(ecolex_betweenness_OECD_sdsm$Country), ]
ecolex_subgraph_OECD_sdsm <- ecolex_subgraph_OECD_sdsm[order(ecolex_subgraph_OECD_sdsm$Country), ]


# Pivot longer
OECD_centrality_frow <- list(ecolex_eigen_OECD_frow, ecolex_strength_OECD_frow,
                        ecolex_closeness_OECD_frow, ecolex_betweenness_OECD_frow,
                        ecolex_subgraph_OECD_frow)
# OECD_centrality_fdsm <- list(ecolex_eigen_OECD_fdsm, ecolex_strength_OECD_fdsm,
#                              ecolex_closeness_OECD_fdsm, ecolex_betweenness_OECD_fdsm,
#                              ecolex_subgraph_OECD_fdsm)
OECD_centrality_sdsm <- list(ecolex_eigen_OECD_sdsm, ecolex_strength_OECD_sdsm,
                             ecolex_closeness_OECD_sdsm, ecolex_betweenness_OECD_sdsm,
                             ecolex_subgraph_OECD_sdsm)

OECD_centrality_frow <- lapply(OECD_centrality_frow,
                          tidyr::pivot_longer,
                          where(is.numeric),
                          names_to = "Year")
# OECD_centrality_fdsm <- lapply(OECD_centrality_fdsm,
#                                tidyr::pivot_longer,
#                                where(is.numeric),
#                                names_to = "Year")
OECD_centrality_sdsm <- lapply(OECD_centrality_sdsm,
                               tidyr::pivot_longer,
                               where(is.numeric),
                               names_to = "Year")
# Minor changes to df
panel_manipulations <- function(panel) {
  for (i in length(panel)) {
    panel[[i]]$Year <- as.integer(panel[[i]]$Year)
    # Add iso2c codes for joins
    panel[[i]]$iso2c <-
      countrycode::countrycode(panel[[i]]$Country, "iso3c", "iso2c")
  }
  # Add names to each df in the list
  names(panel) <- c("Eigenvector",
                    "Strength",
                    "Closeness",
                    "Betweeness",
                    "Subgraph")
  panel
}
# Apply the function
OECD_centrality_frow <- panel_manipulations(panel = OECD_centrality_frow)
# OECD_centrality_fdsm <- panel_manipulations(panel = OECD_centrality_fdsm)
OECD_centrality_sdsm <- panel_manipulations(panel = OECD_centrality_sdsm)

####################### Save each panel separately #############################

saving_lists <- function(panel){
  for (i in seq(length(names(panel)))) {
    saved <- panel[[i]]
    type <- sub(".*_", "", deparse(substitute(panel)))
    save(saved, file = paste0(save_path, "OECD_Panel_",
                              names(panel)[[i]], "_", type, ".rds"))
  }
}
# Apply the function
saving_lists(OECD_centrality_frow)
# saving_lists(OECD_centrality_fdsm)
saving_lists(OECD_centrality_sdsm)

######################### Descriptive plots centrality #########################

# #############################################################
# Let's use the Fixed Row algorithm (Fixed country degrees)----
# #############################################################

# Plot Eigenvector centrality:
eigen_plot_frow <- ggplot2::ggplot(OECD_centrality_frow$Eigenvector, aes(x = Year, y = value,
                                      group = Country, color = Country)) +
  ggplot2::geom_line() +
  labs(title = "Eigenvector Centrality Score of One-Mode Network over time",
       caption = "FROW backbone extration process")

eigen_plotly_frow <- plotly::ggplotly(eigen_plot_frow)

ggsave(filename = "graphs/Descriptive/EigenvectorCentrality_frow.jpeg")


# Plot Strength centrality:
strength_plot_frow <- ggplot2::ggplot(OECD_centrality_frow$Strength, aes(x = Year, y = value,
                                                               group = Country, color = Country)) +
  ggplot2::geom_line() +
  labs(title = "Strength Centrality Score of One-Mode Network over time")

strength_plot_frow <- plotly::ggplotly(strength_plot_frow)

ggsave(filename = "graphs/Descriptive/StrengthCentrality_frow.jpeg")


# Plot Closeness centrality:
close_plot_frow <- ggplot2::ggplot(OECD_centrality$Closeness, aes(x = Year, y = value,
                                                               group = Country, color = Country)) +
  ggplot2::geom_line() +
  labs(title = "Closeness Centrality Score of One-Mode Network over time")

close_plotly_frow <- plotly::ggplotly(close_plot_frow)

ggsave(filename = "graphs/Descriptive/ClosenessCentrality_frow.jpeg")


# Plot Betweeness centrality:
between_plot_frow <- ggplot2::ggplot(OECD_centrality_frow$Betweeness, aes(x = Year, y = value,
                                                               group = Country, color = Country)) +
  ggplot2::geom_line() +
  labs(title = "Betweenness Centrality Score of One-Mode Network over time")

between_plotly_frow <- plotly::ggplotly(between_plot_frow)

ggsave(filename = "graphs/Descriptive/BetweennessCentrality_frow.jpeg")


# Plot Subgraph centrality:
subgraph_plot_frow <- ggplot2::ggplot(OECD_centrality_frow$Subgraph, aes(x = Year, y = value,
                                                               group = Country, color = Country)) +
  ggplot2::geom_line() +
  labs(title = "Subgraph Centrality Score of One-Mode Network over time")

subgraph_plotly_frow <- plotly::ggplotly(subgraph_plot_frow)

ggsave(filename = "graphs/Descriptive/SubgraphCentrality_frow.jpeg")


# ############################
# Let's use the FDSM algorithm
# ############################

# Plot Eigenvector centrality:
# eigen_plot <- ggplot2::ggplot(OECD_centrality_fdsm$Eigenvector, aes(x = Year, y = value,
#                                                                     group = Country, color = Country)) +
#   ggplot2::geom_line() +
#   labs(title = "Eigenvector Centrality Score of One-Mode Network over time")
#
# eigen_plotly <- plotly::ggplotly(eigen_plot)
#
# ggsave(filename = "graphs/Descriptive/EigenvectorCentrality_fdsm.jpeg")
#
#
# # Plot Strength centrality:
# strength_plot <- ggplot2::ggplot(OECD_centrality_fdsm$Strength, aes(x = Year, y = value,
#                                                                     group = Country, color = Country)) +
#   ggplot2::geom_line() +
#   labs(title = "Strength Centrality Score of One-Mode Network over time")
#
# strength_plotly <- plotly::ggplotly(strength_plot)
#
# ggsave(filename = "graphs/Descriptive/StrengthCentrality_fdsm.jpeg")
#
#
# # Plot Closeness centrality:
# close_plot <- ggplot2::ggplot(OECD_centrality$Closeness, aes(x = Year, y = value,
#                                                              group = Country, color = Country)) +
#   ggplot2::geom_line() +
#   labs(title = "Closeness Centrality Score of One-Mode Network over time")
#
# close_plotly <- plotly::ggplotly(close_plot)
#
# ggsave(filename = "graphs/Descriptive/ClosenessCentrality_fdsm.jpeg")
#
#
# # Plot Betweeness centrality:
# between_plot <- ggplot2::ggplot(OECD_centrality_fdsm$Betweeness, aes(x = Year, y = value,
#                                                                      group = Country, color = Country)) +
#   ggplot2::geom_line() +
#   labs(title = "Betweenness Centrality Score of One-Mode Network over time")
#
# between_plotly <- plotly::ggplotly(between_plot)
#
# ggsave(filename = "graphs/Descriptive/BetweennessCentrality_fdsm.jpeg")
#
#
# # Plot Subgraph centrality:
# subgraph_plot <- ggplot2::ggplot(OECD_centrality_fdsm$Subgraph, aes(x = Year, y = value,
#                                                                     group = Country, color = Country)) +
#   ggplot2::geom_line() +
#   labs(title = "Subgraph Centrality Score of One-Mode Network over time")
#
# subgraph_plotly <- plotly::ggplotly(subgraph_plot)
#
# ggsave(filename = "graphs/Descriptive/SubgraphCentrality_fdsm.jpeg")

# ############################
# Let's use the SDSM algorithm
# ############################

# Plot Eigenvector centrality:
eigen_plot_sdsm <- ggplot2::ggplot(OECD_centrality_sdsm$Eigenvector, aes(x = Year, y = value,
                                                                    group = Country, color = Country)) +
  ggplot2::geom_line() +
  labs(title = "Eigenvector Centrality Score of One-Mode Network over time")

eigen_plotly_sdsm <- plotly::ggplotly(eigen_plot_sdsm)

ggsave(filename = "graphs/Descriptive/EigenvectorCentrality_sdsm.jpeg")


# Plot Strength centrality:
strength_plot_sdsm <- ggplot2::ggplot(OECD_centrality_sdsm$Strength, aes(x = Year, y = value,
                                                                    group = Country, color = Country)) +
  ggplot2::geom_line() +
  labs(title = "Strength Centrality Score of One-Mode Network over time")

strength_plotly_sdsm <- plotly::ggplotly(strength_plot_sdsm)

ggsave(filename = "graphs/Descriptive/StrengthCentrality_sdsm.jpeg")


# Plot Closeness centrality:
close_plot_sdsm <- ggplot2::ggplot(OECD_centrality_sdsm$Closeness, aes(x = Year, y = value,
                                                             group = Country, color = Country)) +
  ggplot2::geom_line() +
  labs(title = "Closeness Centrality Score of One-Mode Network over time")

close_plotly_sdsm <- plotly::ggplotly(close_plot_sdsm)

ggsave(filename = "graphs/Descriptive/ClosenessCentrality_sdsm.jpeg")


# Plot Betweeness centrality:
between_plot_sdsm <- ggplot2::ggplot(OECD_centrality_sdsm$Betweeness, aes(x = Year, y = value,
                                                                     group = Country, color = Country)) +
  ggplot2::geom_line() +
  labs(title = "Betweenness Centrality Score of One-Mode Network over time")

between_plotly_sdsm <- plotly::ggplotly(between_plot_sdsm)

ggsave(filename = "graphs/Descriptive/BetweennessCentrality_sdsm.jpeg")


# Plot Subgraph centrality:
subgraph_plot_sdsm <- ggplot2::ggplot(OECD_centrality_sdsm$Subgraph, aes(x = Year, y = value,
                                                                    group = Country, color = Country)) +
  ggplot2::geom_line() +
  labs(title = "Subgraph Centrality Score of One-Mode Network over time")

subgraph_plotly_sdsm <- plotly::ggplotly(subgraph_plot_sdsm)

ggsave(filename = "graphs/Descriptive/SubgraphCentrality_sdsm.jpeg")
