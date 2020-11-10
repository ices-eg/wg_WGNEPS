# Input:    - raw .xlsx datasheets with counts of national and international reviewers

# Output:   - clean .csv sheets with counts of national and international reviewers
#           - "all_lins_step0.csv"
#           - individual .pdf with ccc plots for each reviewer
#           - "Intracounter_variability.png"
#           - "Mean_counts.png"
#           - "Mean_counts_excluding_Int_rev.png"

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
input.dir <- paste0(getwd(), wdir, "/input/")
output.dir <- paste0(getwd(), wdir,"/output_step0/")
###**************************************************************************************###
# Create folders if they don't exist
if(!dir.exists(input.dir)) { dir.create(input.dir) }
if(!dir.exists(output.dir)) { dir.create(output.dir) }
if(!dir.exists(paste0(output.dir, "clean_inputs/"))) { dir.create(paste0(output.dir, "clean_inputs/")) }

# Libraries
library(ggplot2)
# # # Load functions
source(paste0(func.dir, "/ccc.r"))
source(paste0(func.dir, "/lin.ccc.compare.r"))
source(paste0(func.dir, "/template.format.converter.R"))





# Input files need to be in the input folder
input_sheets <- list.files(input.dir, pattern = "ReferenceCount_Datasheet")
input_sheets
# Based on the input files, CHANGE the following line: **********************************###
counters_names <- c("Int_Rev_1", "Nat_Rev_1", "Nat_Rev_2", "Nat_Rev_3", "Nat_Rev_4", "Nat_Rev_5")
###**************************************************************************************###

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

# Edit output
all$newcounterid <- gsub("_1", "", all$Counter_ID)
all$newcounterid <- gsub("_2", "", all$newcounterid)
all$Counter_ID <- all$newcounterid
all$Station <- all$Stations # name edit required

write.csv(all, paste0(output.dir, "all_lins_step0.csv"), row.names = F) # Save for Step 1


## Read the clean datasheets
# Input files need to be in the input folder
input_sheets <- list.files(paste0(output.dir, "clean_inputs/"), pattern = ".csv", full.name = T)
dat <- NULL
for (i in 1:length(input_sheets)) {
  dat <- rbind(dat, read.table(input_sheets[i], sep=",", header=T))
}


dat$recount.no <- gsub(".*([0-9]+)$", "\\1", dat$Counter_ID)
dat$ID <- gsub("_$", "", gsub("\\d+$", "", dat$Counter_ID))
dat <- dat[!is.na(dat$Count),]
 
## INTRA COUNTER VARIABILITY
intra <- dat
intra$recount.no <- as.factor(intra$recount.no)
intra$ID <- as.factor(intra$ID)

ggplot(intra, aes(x=Min, y=Count, col=recount.no))+
  geom_line()+
  facet_grid(ID~Station, scales="free")+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

ggsave(paste0(output.dir, "Intracounter_variability.png"), width=12, height=6)
#ggsave("Intracounter_variability_Without_AW1.png", width=10, height=6)


## MEAN COUNT FOR EVERYONE

ggplot(intra, aes(x=Min, y=Count, col=ID))+
  stat_summary(fun=mean, geom="line")+
  facet_grid(~Station, scales="free")+
  # geom_line(aes(y=reference), col="black", linetype=2, size=1)+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "bottom") +
  guides(colour = guide_legend(nrow = 1))
ggsave(paste0(output.dir, "Mean_counts.png"), width=10, height=3)

## Excluding Int_rev for FU2021. CHANGE this in other cases *****************************###
ggplot(subset(intra, ID != "Int_Rev"), aes(x=Min, y=Count, col=ID))+
###**************************************************************************************###
  stat_summary(fun=mean, geom="line")+
  facet_grid(~Station, scales="free")+
  # geom_line(aes(y=reference), col="black", linetype=2, size=1)+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "bottom") +
  guides(colour = guide_legend(nrow = 1))
ggsave(paste0(output.dir, "Mean_counts_excluding_Int_rev.png"), width=10, height=3)

