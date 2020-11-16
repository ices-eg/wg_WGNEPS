# Input:    - fu2021_tv_final_2020.csv with all the historical station densities

# Output:   - violin density plot

## There are some sections you need to change depending upon your file names: ##
## search for the word CHANGE ##


# Setup----
rm(list = ls())
gc()

# Set up FU.
## CHANGE fu name ***********************************************************************###
fu.name <- "fu2021"
###**************************************************************************************###

# Set up directories

## CHANGE THE DIRECTORIES if needed *****************************************************###
getwd()
wdir <- "/D_Survey_plots/Marine_Institute_Ireland"
input.dir <- paste0(getwd(), wdir, "/input_1/")
output.dir <- paste0(getwd(), wdir,"/output/")
###**************************************************************************************###
# Create folders if they don't exist
if(!dir.exists(input.dir)) { dir.create(input.dir) }
if(!dir.exists(output.dir)) { dir.create(output.dir) }

# Libraries
library(ggplot2)


# Read data. CHANGE input name if needed
surv <- read.csv(paste0(input.dir, fu.name, "_tv_final_2020.csv"))

# Violin plot
ggplot(surv,aes(x=as.factor(Year), y=Density_Adjusted))+ 
  geom_violin(aes(group=Year, colour=Year, fill=Year),alpha=0.5, 
              kernel="rectangular")+           # passes to stat_density, makes violin rectangular 
  geom_boxplot(aes(group=Year), width=.2)+    
  stat_summary(fun=mean, geom="line", colour="blue", aes(group=1)) +
  xlab("Year")+                                # label one axis
  ylab("Density (burrow/m^2)")+                       # label the other
  theme_bw()+                                  # make white background on plot
  theme(legend.position = "none")              # suppress legend

ggsave(paste0(output.dir, "Violin_plot_", fu.name, ".png"), width = 10, height = 6)
