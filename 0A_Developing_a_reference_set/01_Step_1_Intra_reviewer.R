# Input:    - raw excel datasheets with counts of national and international reviewers
# Output:   - clean csv sheets with counts of national and international reviewers
#           - step1_good_lins.csv with the stations that passed the Step 1 (intra-reviewer performance)

## There are some sections you need to change depending upon your file names
## search for the word CHANGE

# Setup----

# Set up FU.
## CHANGE fu name and Lin's CCC treshold
fu.name <- "2021"
threshold <- 0.5
# Set up directories
## CHANGE THE DIRECTORY
getwd()
wdir <- "/0A_Developing_a_reference_set"
func.dir <- paste0(getwd(), wdir, "/functions/")
input.dir <- paste0(getwd(), wdir, "/input/")
output.dir <- paste0(getwd(), wdir,"/output_step1/")

# Libraries
library(lattice)
library(openxlsx)
library(ggplot2)
library(plotrix)
# Load functions
source(paste0(func.dir, "/ccc.r"))
source(paste0(func.dir, "/required.funcs.r"))
source(paste0(func.dir, "/lin.ccc.compare.r"))
source(paste0(func.dir,  "/template.format.converter.R"))

# Input files need to be in the input folder
input_sheets <- list.files(input.dir, pattern = "ReferenceCount_Datasheet")
input_sheets
# Based on the input files, CHANGE the following line:
counters_names <- c("Int_Rev_1", "Nat_Rev_1", "Nat_Rev_2", "Nat_Rev_3", "Nat_Rev_4", "Nat_Rev_5")


# Read data and run Intra-reviewer LIN's CCC ----
if (length(input_sheets) != length(counters_names)) {print("'input_sheets' and 'counters_names' must have the same length")} else {
  all <- NULL
  # Step through each spreadsheet
  # Convert template to format used by Lin CCC function  
  for (i in 1:length(input_sheets)) {
    template.name <- input_sheets[i]
    converted.output.name <- counters_names[i]
    # Create .csv file
    template.format.converter(input.path=paste0(input.dir, template.name), output.dir=paste0(output.dir, "clean_inputs/"), output.name=converted.output.name)
    # Run Lin CCC and save results in object
    out <- lin.ccc.compare(wk.dir = output.dir,
                           counts = paste0(paste0(output.dir, "clean_inputs/"), converted.output.name, ".csv"),
                           # FU = paste0(converted.output.name, "_", fu.name))
    FU = fu.name)
    assign(converted.output.name, out)
    all <- rbind(all, out)
    rm(out)
    setwd("../..")
  }
}

all

# Edit output
all$newcounterid <- gsub("_1", "", all$Counter_ID)
all$newcounterid <- gsub("_2", "", all$newcounterid)
all$Counter_ID <- all$newcounterid
all$Station <- all$Stations ##name change required

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
ggsave(paste0(output.dir, "step1_Intra-reviewer correlations.png"), heigh=5.5, width=10)

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


