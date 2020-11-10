# Input:    - clean .csv sheets with counts of national and international reviewers from Step 0
#           - all_lins_step0.csv from Step 0

# Output:   - step1_Intra-reviewer_correlations.png plot
#           - step1_good_lins.csv with the stations that passed the Step 1 (intra-reviewer performance)

## There are some sections you need to change depending upon your file names: ##
## search for the word CHANGE ##


# Setup----
rm(list = ls())
gc()

# Set up FU.
## CHANGE fu name and Lin's CCC treshold ************************************************###
fu.name <- "2021"
threshold <- 0.5
###**************************************************************************************###

# Set up directories

## CHANGE THE DIRECTORIES if needed *****************************************************###
getwd()
wdir <- "/0A_Developing_a_reference_set"
func.dir <- paste0(getwd(), wdir, "/functions/")
input.dir <- paste0(getwd(), wdir, "/output_step0/")
output.dir <- paste0(getwd(), wdir,"/output_step1/")
###**************************************************************************************###
# Create folders if they don't exist
if(!dir.exists(input.dir)) { dir.create(input.dir) }
if(!dir.exists(output.dir)) { dir.create(output.dir) }

# Libraries
library(ggplot2)
# # Load functions
source(paste0(func.dir, "/ccc.r"))
source(paste0(func.dir, "/required.funcs.r"))
source(paste0(func.dir, "/lin.ccc.compare.r"))
source(paste0(func.dir, "/template.format.converter.R"))


# Read from Step 0
all <- read.csv(paste0(input.dir, "all_lins_step0.csv"))

# Exploratory intra-reviewer plot ----
ggplot(all, aes(x=Stations, y=Lin)) +
  facet_grid(~Counter_ID) +
  geom_hline(yintercept=0.5, linetype=2, col="red", size=1) +
  geom_hline(yintercept=0.6, linetype=2, col="orange", size=1) +
  geom_hline(yintercept=0.7, linetype=2, col="darkgreen", size=1) +
  geom_point() +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  scale_y_continuous(limits=c(0,1),breaks=c(0,0.5,0.6,0.7,1)) +
  labs(y="Intra-reviewer Lin's CCC", x="Station number")
ggsave(paste0(output.dir, "step1_Intra-reviewer_correlations.png"), heigh=5.5, width=10)

# Now we have the intra-reviewer lins ----
# Step 1 of the decision tree states that:
# "If a reviewer's counts do not pass the threshold, then we dismiss this reviewer's data for this station.
all$Lin <- NA.to.0(all$Lin)
# Stations to remove:
all[all$Lin<threshold,]
# We select the "good" stations
good <- all[all$Lin>=threshold, c("Station", "Counter_ID", "Lin")]
good$st.id <- with(good, paste(Station, Counter_ID, sep=":"))
write.csv(good, paste0(output.dir, "step1_good_lins.csv"), row.names=F)


