################################################################################
######################## Import EPU data #######################################
################################################################################

# See https://www.policyuncertainty.com/climate_uncertainty.html
# Gavriilidis 2021
#
# Monthly coverage US Newspapers from Jan 2000 to March 2021

epu <- read.csv("data_raw/EnvironmentalPolicyUncertainty/CPU_index.csv",
                skip = 2)

