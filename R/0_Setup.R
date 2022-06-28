# Setup file for running the replication code.

# R 4+ is required.

# Install required packages. Version number in comments.

# Data preparation:
install.packages("remotes")
remotes::install_github("globalgov/manypkgs") # latest
install.packages("tidyverse") # 1.3.1
install.packages("igraph") # 1.3.1
install.packages("migraph") # 0.9.3
install.packages("backbone") # 2.0.3
install.packages("countrycode") # 1.3.1
install.packages("plotly") # 4.10.0
install.packages("WDI") # 2.7.6 World Bank data from API
remotes::install_github("manystates") # 0.1.0

# Data analysis:
install.packages("fixest") # 0.10.4 Great package for fixed effects estimations :)
install.packages("plm") # 2.6-1 More established package for panel regressions
install.packages("lme4") # 1.1-29
install.packages("lfe") # 2.8-8
install.packages("lmtest") # 0.9.40
install.packages("car") # 3.0.13
install.packages("corrplot") # 0.92 Correlation plots
install.packages("Hmisc") # 4.7.0 Utilities for data science
install.packages("pander") # 0.6.5 For regression tables
install.packages("margins") # 0.3.26 Computing marginal effects of fractional logit models
