# Authors: Carlos Mesquita, Ewen Bell, Mikel Aristegui

# Input:    - mean_counts_dat3_step2.csv from step2
#           - ccc_results_after_step2.csv from step2
#           - stacked_all_reviewers_lins.csv from step2

# Output:   - step3_matrix_counters.png with Lin's CCC values for all the pairings before Step 3
#           - mean_counts_dat3_step3.csv
#           - FUXX_reference_minute.txt with the rinal reference counts after Step 3

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
wdir <- "/A_Developing_a_reference_set"
func.dir <- paste0(getwd(), wdir, "/functions/")
input.dir <- paste0(getwd(), wdir, "/output_step2/")
output.dir <- paste0(getwd(), wdir,"/output_step3/")
###*************************************************************************************###
# Create folders if they don't exist
if(!dir.exists(input.dir)) { dir.create(input.dir) }
if(!dir.exists(output.dir)) { dir.create(output.dir) }


# Libraries
# library(ggplot2)
library(lattice)
library(plotrix)
# Load functions
source(paste0(func.dir, "/ccc.r"))
source(paste0(func.dir, "/required.funcs.r"))

# Read outputs fromstep2
dat3 <- read.csv(paste0(input.dir, "mean_counts_dat3_step2.csv"))
ccc.results.step2 <- read.csv(paste0(input.dir, "ccc_results_after_step2.csv"))
stacked <- read.csv(paste0(input.dir, "stacked_all_reviewers_lins.csv"))
                                             
# Plot the matrixes with intercounter consistency:
origin.matrix <- with(ccc.results.step2, tapply(ccc, list(U1, U2, stn), sum)) # we exclude the international counter for the matrix plot
n.counters <- length(unique(c(ccc.results.step2$U1, ccc.results.step2$U2)))

legend_in_matrix <- T # CHANGE if you want or don't want legend in the matrix

png(paste0(output.dir, "step3_matrix_counters.png"), res=400, width=3500, height=3000)

  if (legend_in_matrix == T) {
    # par with legend:
    par(mfrow=c(3,3), mar=c(2,2,2,2), oma=c(1,1,1,4), xpd=TRUE)
    } else if (legend_in_matrix == F) {
      # par without legend for the report:
      par(mfrow=c(3,3), mar=c(2,2,2,1), oma=c(1,1,1,1), xpd=TRUE)
    }
  
  for (i in unique(dat3$Station)) {
    
    # extract each station separately
    x <- as.data.frame(origin.matrix)[(i*n.counters-(n.counters-1)) : (i*n.counters)]
    
    # change structure of the matrix for visual proposes
    for (row in seq(nrow(x)-1)) {
      for (column in seq(row+1, ncol(x))) {
        if (is.na(x[row, column])) {
          x[row, column] <- x[column, row]
          x[column, row] <- NA
        }
      }
    }
    
    # colours for the plot
    cellcol <- as.matrix(x)
    cellcol[cellcol<0.5] <- "tomato"
    cellcol[cellcol!="tomato"] <- "palegreen"
    cellcol[lower.tri(cellcol, diag=T)] <- "grey85"
    # plot the matrix
    color2D.matplot(x, show.legend=F,
                    show.values=2, cellcolors=cellcol, axes=F,
                    main=paste("Station", substr(colnames(x)[1], nchar(colnames(x)), nchar(colnames(x)))),
                    xlab=NA, ylab=NA)
    # axises for the plot
    axis(1, at=seq(ncol(x))-0.5, labels=sub("_", "\n", substr(colnames(x), 1, nchar(colnames(x))-2)), tick = F)
    axis(2, at=seq(nrow(x))-0.5, hadj=0.7, labels=sub("_", "\n", rev(rownames(x))), las = 1, tick = F)
  }
  
  if (legend_in_matrix == T) {
    # legend for the plot
    par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 1, 0), new = TRUE)
    plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
    color.legend(1.05,-0.95,0.92,-0.8, legend=c("< 0.5","0.5 =<"),
                 rect.col=c("tomato", "palegreen"), gradient="y")
    }

dev.off()

# back to normal par
par(mfrow=c(1,1), oma=c(1,1,1,1), mar=c(2,2,2,2))

########


####################################################################################################
#####################################################################################################
##create the reference set - average the individual averages
stacked$Station <- stacked$stn
stacked$UserID <- as.factor(stacked$user)
stacked.passed <- stacked[stacked$ccc>threshold,]
stacked.passed$ttt <- with(stacked.passed, paste(stn, user))

dat3$ttt <- with(dat3, paste(Station, Counter_ID))


##now we have the passed counts in stacked.passed, put that back with the counts so that we can subset
dat4 <- dat3[dat3$ttt %in% stacked.passed$ttt,]
##dat4 <- merge(dat3, stacked.passed, all.y=T)
summary(dat4)

ref.set <- tapply.ID(dat4, "Burrows", c("Station", "Block", "DVDNo", "TVID", "ObsID"), "mean", "Burrows")
ref.set$UserID <- "reference"
ref.set$Counter_ID <- ref.set$UserID
ref.set$RunID <- with(ref.set, paste(Counter_ID, Station, sep=":"))

ref.set
reference <- cbind(ref.set[, c("Burrows")], fu.name, ref.set[, c("Station", "Block")])
names(reference) <- c("reference","FU","Station","Minute")
write.table(reference, paste0(output.dir, "FU", fu.name, "_reference_minute.txt"), row.names=F, quote=F, sep="\t")


dat3 <- rbind(dat4[,names(ref.set)], ref.set)
write.csv(dat3, paste0(output.dir, "mean_counts_dat3_step3.csv")) # Save dat3 for 04_check_reference_set_robustness

