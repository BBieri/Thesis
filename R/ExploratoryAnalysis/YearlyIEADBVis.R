################################################################################
############## Create Yearly One Mode Centrality Scores ########################
################################################################################

library(manyenviron)
library(tidyverse)
library(migraph)

# Import relevant data ----

# How many years?
# EPS range is 1990-2015 --> Extract observations for these years

ieadb <- manyenviron::memberships$IEADB_MEM %>%
  dplyr::mutate(year = as.numeric(stringr::str_extract(many_ID, "[:digit:]{4}"))) %>%
  dplyr::filter(year >= 1990 & year <= 2015) %>%
  dplyr::arrange(year)

skimr::skim(ieadb)

initial_year <- 1990

# Create a list of yearly datasets
ieadb <- ieadb %>%
  dplyr::group_by(year)
ieadb_lst <- group_split(ieadb)
# Convert to a list of networks
ieadb_net_lst <- lapply(ieadb_lst, migraph::as_tidygraph, twomode = TRUE)
# Check if all subsets are bipartite
all(unlist(lapply(ieadb_net_lst, migraph::is_bipartite)))
# Perform a one mode projection to simplify things
ieadb_net_lst_onemode <- lapply(ieadb_net_lst, migraph::project_cols)

years <- seq(1990, 2015)
date <- paste0(years, "-01-01")
# gsub("-01-01", "", date)
# Plotting Wrapper Function:

make_combined_plots <- function(network, date, theme){
  year <- gsub("-01-01", "", date)
  migraph::network_map(network, date = date, theme = theme) +
    ggplot2::labs(title = year)
}
#
# # Plot all the yearly graphs into a plot list
# mapplotsieadb <- mapply(make_combined_plots,
#                         ieadb_net_lst_onemode,
#                         date = date,
#                         theme = "dark")
mapplotsieadb <- list()
year <- gsub("-01-01", "", date)
for (i in seq(length(date))) {
  mapplotsieadb[[i]] <- migraph::network_map(ieadb_net_lst_onemode[[i]],
                                             date = date[[i]],
                                             theme = "dark") +
    ggplot2::labs(title = year[[i]])
}

# Maptheme function from migraph
maptheme <- function(palette = c("#FFFAFA", "#596673")) {
  landcolor <- palette[1]
  oceancolor <- palette[2]
  titlecolor <- ifelse(is_dark(palette[2]), "white", "black")
  # Create map theme
  maptheme <- ggplot2::theme(panel.grid = ggplot2::element_blank()) +
    ggplot2::theme(axis.text = ggplot2::element_blank()) +
    ggplot2::theme(axis.ticks = ggplot2::element_blank()) +
    ggplot2::theme(axis.title = ggplot2::element_blank()) +
    ggplot2::theme(legend.position = "bottom") +
    ggplot2::theme(panel.grid = ggplot2::element_blank()) +
    ggplot2::theme(panel.background = ggplot2::element_blank()) +
    ggplot2::theme(panel.border = element_blank()) +
    ggplot2::theme(plot.background = ggplot2::element_rect(fill = oceancolor,
                                                           color = oceancolor)) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(
        color = titlecolor,
        hjust = 0.1,
        vjust = 0.1
      ),
      plot.subtitle = ggplot2::element_text(
        color = titlecolor,
        hjust = 0.065,
        vjust = 0.1
      ),
      plot.caption = ggplot2::element_text(
        color = titlecolor,
        hjust = 0.96
      )
    ) +
    ggplot2::theme(plot.margin = ggplot2::unit(c(0, 0, 0.5, 0), "cm"))
  # This function returns a map theme for ggplot
  maptheme
}

is_dark <- function(hex) {
  # Google luma formula for details.
  luma <- 0.33 * grDevices::col2rgb(hex)[[1]] +
    0.5 * grDevices::col2rgb(hex)[[2]] +
    0.16 * grDevices::col2rgb(hex)[[3]]
  isdark <- ifelse(luma < 186, TRUE, FALSE)
  isdark
}

# Plot things
plotIEADB <- patchwork::wrap_plots(mapplotsieadb, ncol = 6) +
  patchwork::plot_annotation(
    title = "Evolution of the Environmental Governance Network"
    ) &
  maptheme()

ggsave("mapIEADB.jpeg", plotIEADB)
