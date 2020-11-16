# Input:    - burrow.school.session_example.xlsx

# Output:   - ccc.plots.pdf with all the Lin's pairing plots
#           - ccc_by_density.png & ccc_by_reviewer.png where we can see the performance of individual reviewers vs. the reference counts

## There are some sections you need to change depending upon your file names: ##
## search for the word CHANGE ##


# Setup----
rm(list = ls())
gc()

## CHANGE Lin's CCC treshold ************************************************************###
threshold <- 0.5
###**************************************************************************************###


# Set up directories

## CHANGE THE DIRECTORIES if needed *****************************************************###
getwd()
wdir <- "/B_Lins_training_pre_survey/Marine_Institute_Ireland/Method_before_2020"
func.dir <- paste0(getwd(), wdir, "/functions/")
input.dir <- paste0(getwd(), wdir, "/input/")
output.dir <- paste0(getwd(), wdir,"/output/")
###**************************************************************************************###
# Create folders if they don't exist
if(!dir.exists(input.dir)) { dir.create(input.dir) }
if(!dir.exists(output.dir)) { dir.create(output.dir) }


# Libraries
library(lattice)
library(openxlsx)
## Load functions
source(paste0(func.dir, "ccc.r"))
source(paste0(func.dir, "required.funcs.r"))

## CHANGE your file name if needed ******************************************************###
refs <- read.xlsx(paste0(input.dir, "burrow.school.session_example.xlsx"))
###**************************************************************************************###

# refs2 <- read.table(paste0(input.dir, "session1.txt"), sep="\t", header=T)

summary(refs)

real.refs <- read.table(paste0(input.dir, "FU2021reference_minute_WKNEPS_2018.txt"), sep="\t", header=T)

refs <- merge(refs, real.refs)

id <- data.frame(old.ID=unique(refs$ID), ID=c(1:length(unique(refs$ID))))
refs$old.ID <- refs$ID
refs <- data.frame.drop(refs, "ID")
refs <- merge(refs, id)

refs <- sort.data.frame(~FU+Station+ID+recount.no+Count, dat=refs)

vis <- sort(unique(refs$Visual.Clarity))
dens <- sort(unique(refs$Density))

pdf(paste0(output.dir, "ccc.plots.pdf"))
#holder for all the results
cc <-  vector(mode="list", length=0)

#loop through the FUs seperately
fus <- sort(unique(refs$FU))
for(f in c(1:length(fus)))
{
  refs1 <- refs[refs$FU==fus[f],]
  
  refs1$run.ID <- with(refs1, paste(ID, recount.no, sep=":"))
  
  #work through all possible pairings of run.ID
  run.ids <- sort(unique(refs1$run.ID))
  for(i in c(1:length(run.ids)))
  {
    #all pooled
    print(paste("FU",fus[f],"   ", run.ids[i], sep=":"))
    
    u1 <- sort.data.frame(~reference, dat=refs1[refs1$run.ID==run.ids[i],])
    if(length(u1[,1])>0)
    {
      ccc.res <- ccc(u1$Count, u1$reference)
      
      #plot the original data, jittering a bit because very often low integer exact matches
      if(sum(c(u1$Count))>0)
      {
        plot(jitter(u1$reference, factor=0.25),jitter(u1$Count, factor=0.25), xlab="Reference", ylab=run.ids[i], main=paste("FU", fus[f], "All pooled",sep=" "), sub=paste("CCC= ",round(ccc.res$ccc,3), ": Pearson= ", round(ccc.res$pearson,3)), ylim=range(c(u1$Count, u1$reference)))           
        #get a linear fit between the two
        m <- lm(u1$Count~u1$reference)
        #                d <- data.frame(Count=range(u1$Count))
        #               mm <- data.frame(a=range(u1$Count), b=predict.lm(m, d))
        mm <- data.frame(a=(u1$Count), b=m$fit)
        #              mm <- sort.data.frame(~a, dat=mm)
        lines(range(c(u1$Count,u1$reference)), range(c(u1$Count,u1$reference)), lty=3)
        #lines(mm$b, mm$a, lty=2, col=2)
        abline(m, lty=2, col=2)
      }
      else(ccc.res <- list(ccc=1, pearson=1, u=1, v=1))
      #        cc <- c(cc, list( ccc.res=list(  FU=fus[f],type="all", ccc=ccc.res, U1=run.ids[i], U2=run.ids[j])))
      cc <- c(cc, list( ccc.res=list(  FU=fus[f],type="all", ccc=ccc.res, U1=unique(u1$ID))))
    }
    
    #  print(paste(i,j, sep=":"))          
    
    
    ##############################################################
    #Vis groups
    
    for(v in c(1:length(vis)))
    {            
      u1 <- refs1[refs1$run.ID==run.ids[i] & refs1$Visual.Clarity==vis[v],]
      if(length(u1[,1])>0)
      {
        
        ccc.res <- ccc(u1$Count, u1$reference)
        
        #plot the original data, jittering a bit because very often low integer exact matches
        if(sum(c(u1$Count))>0)
        {
          plot(jitter(u1$reference, factor=0.25),jitter(u1$Count, factor=0.25), xlab="Reference", ylab=run.ids[i], main=paste("FU", fus[f] ,"Visibility",vis[v],sep=":"), sub=paste("CCC= ",round(ccc.res$ccc,3), ": Pearson= ", round(ccc.res$pearson,3)), ylim=range(c(u1$Count, u1$reference)))           
          #get a linear fit between the two
          m <- lm(u1$Count~u1$reference)
          mm <- data.frame(a=(u1$Count), b=m$fit)
          mm <- sort.data.frame(~a, dat=mm)
          lines(range(c(u1$Count,u1$reference)), range(c(u1$Count,u1$reference)), lty=3)
          #lines(mm$a, mm$b, lty=2, col=2)
          abline(m, lty=2, col=2)                    
        }
        else(ccc.res <- list(ccc=1, pearson=1, u=1, v=1))
        #        cc <- c(cc, list( ccc.res=list(  FU=fus[f],type="all", ccc=ccc.res, U1=run.ids[i], U2=run.ids[j])))
        cc <- c(cc, list( ccc.res=list(  FU=fus[f],type=paste("Visibility", vis[v], sep=" : "), ccc=ccc.res, U1=unique(u1$ID))))
      }
    }
    
    
    ##############################################################
    #Density groups
    
    for(d in c(1:length(dens)))
    {
      u1 <- refs1[refs1$run.ID==run.ids[i] & refs1$Density==dens[d],]
      if(length(u1[,1])>0)
      {
        
        ccc.res <- ccc(u1$Count, u1$reference)                    
        if(sum(c(u1$Count))>0)
        {
          plot(jitter(u1$reference, factor=0.25),jitter(u1$Count, factor=0.25), xlab="Reference", ylab=run.ids[i], main=paste("FU", fus[f] ,"Density",dens[d],sep=":"), sub=paste("CCC= ",round(ccc.res$ccc,3), ": Pearson= ", round(ccc.res$pearson,3)), ylim=range(c(u1$Count, u1$reference)))           
          #get a linear fit between the two
          m <- lm(u1$Count~u1$reference)
          mm <- data.frame(a=(u1$Count), b=m$fit)
          mm <- sort.data.frame(~a, dat=mm)
          lines(range(c(u1$Count,u1$reference)), range(c(u1$Count,u1$reference)), lty=3)
          abline(m, lty=2, col=2)
        }
        else(ccc.res <- list(ccc=1, pearson=1, u=1, v=1))
        #        cc <- c(cc, list( ccc.res=list(  FU=fus[f],type="all", ccc=ccc.res, U1=run.ids[i], U2=run.ids[j])))
        cc <- c(cc, list( ccc.res=list(  FU=fus[f],type=paste("Density", dens[d], sep=" : "), ccc=ccc.res, U1=unique(u1$ID))))
      }                    
    }
  }
}
#  print(paste(i,j, sep=":"))          
dev.off()

# Plots----
# now unstitch it all
res <- data.frame(FU=NA, U1=NA,  type=NA, ccc=NA)

for( l in c(1:length(cc)))
{
  res <- rbind(res, data.frame(FU=cc[l]$ccc.res$FU, U1=cc[l]$ccc.res$U1,  type=cc[l]$ccc.res$type, ccc=cc[l]$ccc.res$ccc$ccc))
}
res2 <- res[!is.na(res$FU),]

t <- res2[res2$FU==fus[f],]


png(paste0(output.dir, "ccc_by_density.png"), width = 22, height = 14, res = 300, units = "cm")
  print(with(t, bwplot(ccc ~ as.factor(U1) | type,layout=c(length(unique(res2$type)), 1),
                       scales=list(x=list(rot=90,cex=0.5)),
                       panel=function(x,y,...) {
                         panel.bwplot(x,y,...)
                         panel.abline(h=threshold, lty=2, col=1)
                         })))
dev.off()


png(paste0(output.dir, "ccc_by_reviewer.png"), width = 22, height = 18, res = 300, units = "cm")
  print(with(t, bwplot(ccc ~ type | as.factor(U1),layout=c(length(unique(res2$U1)), 1),
                       scales=list(x=list(rot=90,cex=0.9)),
                       panel=function(x,y,...) {
                         panel.bwplot(x,y,...)
                         panel.abline(h=threshold, lty=2, col=1)
                         })))
dev.off()

# Reviewers id in plot, as they are anonymised
id
