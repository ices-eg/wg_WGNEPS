# Set up directories
## CHANGE THE DIRECTORY
getwd()
wdir <- "/0A_Developing_a_reference_set"
func.dir <- paste0(getwd(), wdir, "/functions/")
input.dir <- paste0(getwd(), wdir, "/output_step1/")
output.dir <- paste0(getwd(), wdir,"/output_step2/")

# Libraries
library(ggplot2)
library(lattice)
library(plotrix)
# Load functions
source(paste0(func.dir, "/ccc.r"))
source(paste0(func.dir, "/required.funcs.r"))

# These are the stations that passed the step 1
good <- read.csv(paste0(input.dir, "step1_good_lins.csv"))


## Read the clean datasheets
# Input files need to be in the input folder
input_sheets <- list.files(paste0(input.dir, "clean_inputs/"), pattern = ".csv", full.name = T)
dat <- NULL
for (i in 1:length(input_sheets)) {
  dat <- rbind(dat, read.table(input_sheets[i], sep=",", header=T))
}

summary(dat)

dat <- dat[!is.na(dat$Count),]
dat$newcounterid <- gsub("_1", "", dat$Counter_ID)
dat$newcounterid <- gsub("_2", "", dat$newcounterid)
dat$Counter_ID <- dat$newcounterid
dat$st.id <- with(dat, paste(Station, Counter_ID, sep=":"))

# Select only the ones that passed step1
dat2 <- dat[dat$st.id %in% good$st.id,]
summary(dat2)

##now create the mean per st.id
dat3 <- tapply.ID(dat2, "Count", c("Station", "Counter_ID", "Functional_Unit", "Min"), "mean", "Burrows")
dat3$RunID <- with(dat3, paste(Counter_ID, Station, sep=":"))
dat3$ObsID <- dat3$Station
dat3$UserID <- dat3$Counter_ID
dat3$Block <- dat3$Min
dat3$DVDNo <- 1
dat3$TVID <- dat3$Station
summary(dat3)


t <- trellis.par.get("superpose.line")
t$lwd <- 2
t$col <- c("red", "grey", "black", "blue", "green", "orange")
t$lty <- c(2,1,1,1,1,1)
trellis.par.set("superpose.line",t)
with(dat3, xyplot(Burrows~Min | as.factor(Station), groups=Counter_ID, xlab="minute", type="l", auto.key=list(space="bottom", lines=T, points=F,columns=4),
                  panel=function(x,y,...)
                  {
                    panel.xyplot(x,y,...)
                    #   panel.abline(h=0.5, lty=2, col="grey")
                    
                  }))

with(dat3, bwplot(Burrows~Counter_ID))
source(paste0(func.dir, "f_lemon.hunter.2018.R")) ##you don't need to change anything here

summary(ccc.results)
with(ccc.results, bwplot(ccc ~ as.factor(stn)))

# get a single column with station user and ccc - stack together U1 and U2 results
t1 <- ccc.results[,c("stn", "U1", "ccc")]
t1$user <- t1$U1
t2 <- ccc.results[,c("stn", "U2", "ccc")]
t2$user <- t2$U2

stacked <- rbind(t1[,c("stn", "ccc", "user")],t2[,c("stn", "ccc", "user")])
with(stacked, bwplot(ccc ~ as.factor(user),panel=function(x,y,...){
  panel.bwplot(x,y,...)
  panel.abline(h=0.5, lty=2, col="red")
}))
summary(stacked)

#To plot Lin's boxplots for each reviewer.

step2_plot <- ggplot(stacked, aes(user, ccc)) +
  geom_boxplot() +
  geom_hline(yintercept=0.5, col="red", linetype=2) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(y="Inter-reviewer Lin's CCC", x="Reviewer")
step2_plot
ggsave(paste0(output.dir, "step2_Inter-reviewer_performance_50perc_Before_step2.png"), width=8, height=6)

# Step 2 of the decision tree states that: ----
# "If a reviewer fails more than 50% of its pairings, then we dismiss this reviewer's all stations."
# If a reviewer's median is below the threshold in the above boxplot, this means that the reviewer is failling step2.
# Now, if any reviewer fails, we will remove the worst reviewer,
# then run again the threshold, and keep removing reviewers if they keep failing this step2.

# First time to see which reviewers fail more than 50%
step2 <- rbind(setNames(ccc.results[,c("U1", "stn", "ccc")], c("id", "stn", "ccc")),
               setNames(ccc.results[,c("U2", "stn", "ccc")], c("id", "stn", "ccc")))
step2$pass <- step2$ccc >= 0.5
step2_1 <- setNames(merge(aggregate(pass ~ id, data = step2, FUN = sum),
                          aggregate(stn ~ id, data = step2, FUN = length)),
                    c("id", "pass", "pairings"))
step2_1$good <- with(step2_1, pass/pairings)
step2_1 <- step2_1[order(step2_1$good),]
step2_fail <- step2_1[step2_1$good < 0.5, "id"]

# Now inside a loop to remove all the reviewers that fail, but ONE BY ONE. This is important!
for (i in 1:length(step2_fail)) {
  if (step2_1[step2_1$id == step2_fail[i], "good"] < 0.5) { # if true, then we dismiss this counter's all stations
    ccc.results.step2 <-subset(ccc.results, U1 != step2_fail[i] & U2 != step2_fail[i])
    
    step2 <- rbind(setNames(ccc.results.step2[,c("U1", "stn", "ccc")], c("id", "stn", "ccc")),
                   setNames(ccc.results.step2[,c("U2", "stn", "ccc")], c("id", "stn", "ccc")))
    step2$pass <- step2$ccc >= 0.5
    step2_1 <- setNames(merge(aggregate(pass ~ id, data = step2, FUN = sum),
                              aggregate(stn ~ id, data = step2, FUN = length)),
                        c("id", "pass", "pairings"))
    step2_1$good <- with(step2_1, pass/pairings)
  }
}


t1.step2 <- ccc.results.step2 [,c("stn", "U1", "ccc")]
t1.step2$user <- t1.step2$U1
t2.step2 <- ccc.results.step2 [,c("stn", "U2", "ccc")]
t2.step2$user <- t2.step2$U2

stacked.step2 <- rbind(t1.step2[,c("stn", "ccc", "user")],
                       t2.step2[,c("stn", "ccc", "user")])

ggplot(stacked.step2, aes(user, ccc)) +
  geom_boxplot() +
  ylim(layer_scales(step2_plot)$y$range$range) +
  geom_hline(yintercept=0.5, col="red", linetype=2) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(y="Inter-reviewer Lin's CCC", x="Reviewer")

ggsave(paste0(output.dir, "step2_Inter-reviewer_performance_50perc_After_step2.png"), width=8, height=6)


## Now to plot both previous plots together for the report (mikel)
stacked$step <- as.factor("All reviewers")
stacked.step2$step <- as.factor("Reviewers after step 2")
stacked.all <- rbind(stacked, stacked.step2)

ggplot(stacked.all, aes(user, ccc)) +
  geom_boxplot() +
  geom_hline(yintercept=0.5, col="red", linetype=2) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  facet_wrap(~step, scales = "free_x") +
  labs(y="Inter-reviewer Lin's CCC", x="Reviewer")

ggsave(paste0(output.dir, "step2_Inter-reviewer_performance_50perc_Before_and_after_step2.png"), width=10, height=5)




##### To plot the matrixes with intercounter consistency (mikel)

#origin.matrix <- with(ccc.results, tapply(ccc, list(U1, U2, stn), sum))
#n.counters <- length(unique(stacked$user))

origin.matrix <- with(ccc.results.ExcInter, tapply(ccc, list(U1, U2, stn), sum)) # we exclude the international counter for the matrix plot
n.counters <- length(unique(stacked.ExcInter$user))



png("step3_matrix.png", res=400, width=3500, height=3000)

# par with legend:
# par(mfrow=c(3,3), mar=c(2,5,2,1), oma=c(1,1,4,0), xpd=TRUE)

# par without legend for the report:
par(mfrow=c(3,3), mar=c(2,3,2,1), xpd=TRUE)


for (i in unique(dat$Station)) {
  
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


# legend for the plot
#par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 1, 0), new = TRUE)
#plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
#color.legend(-0.35,0.95,-0.25,1.1,legend=c("< 0.5","0.5 =<"),
#             rect.col=c("tomato", "palegreen"), gradient="y")

dev.off()

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
reference <- cbind(ref.set[, c("Burrows")], "2021", ref.set[, c("Station", "Block")])
names(reference) <- c("reference","FU","Station","Minute")
write.table(reference, "FU2021reference_minute.txt", row.names=F, quote=F, sep="\t")


dat3 <- rbind(dat4[,names(ref.set)], ref.set)

##plot up again - this time put the reference line as black dashed
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



##now put this into the lemon hunter!
source(paste(wdir,"functions/f_lemon.hunter.2018.R", sep="")) ##you don't need to change anything here
##re-work the matrix of passes
##get a single column with station user and ccc - stack together U1 and U2 results
t1 <- ccc.results[,c("stn", "U1", "ccc")]
t1$user <- t1$U1
t2 <- ccc.results[,c("stn", "U2", "ccc")]
t2$user <- t2$U2

stacked <- rbind(t1[,c("stn", "ccc", "user")],t2[,c("stn", "ccc", "user")])
with(stacked, bwplot(ccc ~ as.factor(user),panel=function(x,y,...){
  panel.bwplot(x,y,...)
  panel.abline(h=0.5, lty=2, col="red")
}))
summary(stacked)

ref.matrix <- with(ccc.results, tapply(ccc, list(U1, U2, stn), sum))


