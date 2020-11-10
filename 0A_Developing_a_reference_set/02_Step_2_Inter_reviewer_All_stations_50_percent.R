# Input:    - clean csv sheets with counts of national and international reviewers from Step 1

# Output:   - mean_counts_plot.png exploratory plot
#           - all.ccc.plots.pdf with all the Lin's CCC for all the pairings before Step 2
#           - Boxplots with Lin's CCC values for each reviewer before & after Step 2 (inter-reviewer performance 50%)
#           - mean_counts_dat3_step2.csv
#           - ccc_results_after_step2.csv
#           - stacked_all_reviewers_lins.csv

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
input.dir0 <- paste0(getwd(), wdir, "/output_step0/")
input.dir1 <- paste0(getwd(), wdir, "/output_step1/")
output.dir <- paste0(getwd(), wdir,"/output_step2/")
###*************************************************************************************###
# Create folders if they don't exist
if(!dir.exists(input.dir0)) { dir.create(input.dir0) }
if(!dir.exists(input.dir1)) { dir.create(input.dir1) }
if(!dir.exists(output.dir)) { dir.create(output.dir) }

# Libraries
library(ggplot2)
library(lattice)
# Load functions
source(paste0(func.dir, "/ccc.r"))
source(paste0(func.dir, "/required.funcs.r"))

# These are the stations that passed the step 1
good <- read.csv(paste0(input.dir1, "step1_good_lins.csv"))


## Read the clean datasheets
# Input files need to be in the input folder
input_sheets <- list.files(paste0(input.dir0, "clean_inputs/"), pattern = ".csv", full.name = T)
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
write.csv(dat3, paste0(output.dir, "mean_counts_dat3_step2.csv")) # Save dat3 for step3

png(paste0(output.dir, "mean_counts_plot.png"), res=400, width=3500, height=3000)
t <- trellis.par.get("superpose.line")
t$lwd <- 2
t$col <- c("grey", "red", "green", "pink", "blue", "orange")
t$lty <- c(1,1,1,1,1,1)
trellis.par.set("superpose.line",t)
with(dat3, xyplot(Burrows~Min | as.factor(Station), groups=Counter_ID, xlab="minute", type="l", auto.key=list(space="bottom", lines=T, points=F,columns=4),
                  panel=function(x,y,...)
                  {
                    panel.xyplot(x,y,...)
                    #   panel.abline(h=0.5, lty=2, col="grey")
                  }))
dev.off()


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
write.csv(ccc.results.step2, paste0(output.dir, "ccc_results_after_step2.csv")) # Save ccc.results.step2 for step3

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
write.csv(stacked, paste0(output.dir, "stacked_all_reviewers_lins.csv"), row.names = F)# Save stacked for step3
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

