# The goal of this script is to clean all the output folders in one easy move

rm(list = ls())
gc()

# Set up directories
## CHANGE THE DIRECTORIES if needed *****************************************************###
getwd()
wdir <- "/0A_Developing_a_reference_set"
###*************************************************************************************###

# Remove all the files in the output folders
output.dirs <- dir(paste0(getwd(), wdir), pattern = "output", full.names = T)
output.files <- list.files(output.dirs, full.names = T, recursive = T)
file.remove(output.files)
