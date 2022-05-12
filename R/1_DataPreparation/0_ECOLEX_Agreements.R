# ECOLEX agreements Preparation Script

# Source: Slightly modified import script from manyenviron

# Installing Required Packages
# remotes::install_github("globalgov/manypkgs")
# install.packages("tidyverse")

# Stage one: Collecting data
load("data_raw/ECOLEX/Agreements/ecoagree.RData")
ECOLEX <- eco_agree
manypkgs::retain("ECOLEX")

# Function to split the topics into a column each.
split_into_multiple <- function(column, pattern = ", ", into_prefix){
  cols <- str_split_fixed(column, pattern, n = Inf)
  # Sub out the ""'s returned by filling the matrix to the right, with NAs which are useful
  cols[which(cols == "")] <- NA
  cols <- as.tibble(cols)
  # name the 'cols' tibble as 'into_prefix_1', 'into_prefix_2', ..., 'into_prefix_m'
  # where m = # columns of 'cols'
  m <- dim(cols)[2]

  names(cols) <- paste(into_prefix, 1:m, sep = "_")
  return(cols)
}

# Stage two: Correcting data

ECOLEX <- dplyr::as_tibble(ECOLEX) %>%
  dplyr::rename(ecolexID = EcolexID) %>%
  dplyr::select(ecolexID, Subject)

# Separate subject cols.
ECOLEX <- cbind(ECOLEX,
                split_into_multiple(ECOLEX$Subject, ", ", into_prefix = "Subject"))

# Connect back with original agreement database from manyenviron:
ECOLEX <- dplyr::inner_join(manyenviron::agreements$ECOLEX, ECOLEX,
                            c("ecolexID" = "ecolexID"))

# Extract subject specific datasets:

subjects <- unique(ECOLEX$Subject_1)
lst <- list()
for (i in seq(length(subjects))) {
  lst[[i]] <- dplyr::filter(ECOLEX, grepl(subjects[[i]], Subject))
}
names(lst) <- paste0("ECOLEX_", subjects)
names(lst)[[15]] <- "ECOLEX_Envrionment_gen"
names(lst) <- gsub("&", "", names(lst))
names(lst) <- gsub("\\s+", " ", names(lst))
names(lst) <- chartr(" ", "_", names(lst))

# Save stuff for further processing.
path <- "data/ECOLEX/Agreements/"
save(ECOLEX, file = paste0(path, "ECOLEX_Agreements.rds"))
for (i in seq(length(subjects))) {
  saved <- lst[[i]]
  save(saved, file = paste0(path, names(lst)[[i]], ".rds"))
}
