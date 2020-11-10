# Input:    - mean_counts_dat3_step3.csv from step3
#           - ccc_results_after_step3.csv from step3
#           - stacked_all_reviewers_lins.csv from step2

# Output:   - all.ccc.plots.pdf with all the Lin's CCC for all the pairingsafter Step 3
#           - mean_counts_vs_reference_plot.png with the reference counts and the counts of the reviewers used to generate each station's reference set
#           - counters_vs_reference_matrix.png with Lin's CCC values for all the pairings after Step 3 and vs Reference
#           - step4_Inter-reviewer_performance_50perc_After_step4.png

## There are some sections you need to change depending upon your file names: ##
## search for the word CHANGE ##


# Setup----
rm(list = ls())
gc()

# Set up directories
## CHANGE THE DIRECTORIES if needed *****************************************************###
getwd()
wdir <- "/0A_Developing_a_reference_set"
func.dir <- paste0(getwd(), wdir, "/functions/")
input.dir <- paste0(getwd(), wdir, "/output_step3/")
output.dir <- paste0(getwd(), wdir,"/output_step4/")
###*************************************************************************************###
# Create folders if they don't exist
if(!dir.exists(input.dir)) { dir.create(input.dir) }
if(!dir.exists(output.dir)) { dir.create(output.dir) }


# Libraries
library(ggplot2)
library(lattice)
library(plotrix)
# # Load functions
source(paste0(func.dir, "/ccc.r"))
source(paste0(func.dir, "/required.funcs.r"))



dat3 <- read.csv(paste0(input.dir, "mean_counts_dat3_step3.csv"))

# Plot up again - this time put the reference line as black dashed
# and only the reviewers used to generate each station's reference set
png(paste0(output.dir, "mean_counts_vs_reference_plot.png"), res=400, width=3500, height=3000)
t <- trellis.par.get("superpose.line")
t$lwd <- 2
t$col <- c("red", "green", "pink", "blue", "orange", "black")
t$lty <- c(1,1,1,1,1,2)
trellis.par.set("superpose.line",t)
with(dat3, xyplot(Burrows~Block | as.factor(Station), groups=Counter_ID, xlab="minute", type="l", layout=c(3,3),auto.key=list(space="bottom", lines=T, points=F,columns=4),
                  panel=function(x,y,...)
                  {
                    panel.xyplot(x,y,...)
                    #   panel.abline(h=0.5, lty=2, col="grey")
                    
                  }))
dev.off()

##now put this into the lemon hunter!
source(paste(func.dir, "f_lemon.hunter.2018.R", sep="")) ##you don't need to change anything here
ccc.results.step4 <- ccc.results

##re-work the matrix of passes
##get a single column with station user and ccc - stack together U1 and U2 results
t1 <- ccc.results.step4[,c("stn", "U1", "ccc")]
t1$user <- t1$U1
t2 <- ccc.results.step4[,c("stn", "U2", "ccc")]
t2$user <- t2$U2

stacked.step4 <- rbind(t1[,c("stn", "ccc", "user")],t2[,c("stn", "ccc", "user")])
with(stacked.step4, bwplot(ccc ~ as.factor(user),panel=function(x,y,...){
  panel.bwplot(x,y,...)
  panel.abline(h=0.5, lty=2, col="red")
}))
summary(stacked.step4)

ggplot(stacked.step4, aes(user, ccc)) +
  geom_boxplot() +
  ylim(c(0,1)) +
  geom_hline(yintercept=0.5, col="red", linetype=2) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(y="Inter-reviewer-reference Lin's CCC", x="Reviewer")
ggsave(paste0(output.dir, "step4_Inter-reviewer_performance_50perc_After_step4.png"), width=8, height=6)



# Plot the matrixes with intercounter consistency and vs reference counts:
ref.matrix <- with(ccc.results.step4, tapply(ccc, list(U1, U2, stn), sum)) # we exclude the international counter for the matrix plot
colnames(ref.matrix)[colnames(ref.matrix) == "reference"] <- "Refe-_rence" # formatting for matrix
row.names(ref.matrix)[row.names(ref.matrix) == "reference"] <- "Refe-_rence" # formatting for matrix
n.counters <- length(unique(c(ccc.results.step4$U1, ccc.results.step4$U2)))

legend_in_matrix <- T # CHANGE if you want or don't want legend in the matrix

png(paste0(output.dir, "counters_vs_reference_matrix.png"), res=400, width=3500, height=3000)

  if (legend_in_matrix == T) {
    # par with legend:
    par(mfrow=c(3,3), mar=c(3,2,2,2), oma=c(1,1,1,4), xpd=TRUE)
  } else if (legend_in_matrix == F) {
    # par without legend for the report:
    par(mfrow=c(3,3), mar=c(3,2,2,1), oma=c(1,1,1,1), xpd=TRUE)
  }
  
  for (i in unique(dat3$Station)) {
    
    # extract each station separately
    x <- as.data.frame(ref.matrix)[(i*n.counters-(n.counters-1)) : (i*n.counters)]
    
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
    axis(1, at=seq(ncol(x))-0.5, hadj=0.7, labels=sub("_", "\n", substr(colnames(x), 1, nchar(colnames(x))-2)), tick = F, las=2)
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

