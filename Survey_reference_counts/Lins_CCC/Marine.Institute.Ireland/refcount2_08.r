


setwd("N:/Surveys/UWTV SURVEYS FU2022 CELTIC SEA/2017 Labadie/FU2021 Training/ReferenceCountsTraining/R Code")
source('./ccc.r')
source('./required.funcs.r')
require(lattice) 

#read in data
refs <- read.table("session1.txt",sep="\t", header=T)
#summary of data:
#check counter IDS and number of data should be 100 minutes per counter

summary(refs)

#read in reference counts
real.refs <- read.table("fu2021.reference.counts.txt", sep="\t", header=T)

#now trickery to merge for analysis
refs <- merge(refs, real.refs)

id <- data.frame(old.ID=unique(refs$ID), ID=c(1:length(unique(refs$ID))))
refs$old.ID <- refs$ID
refs <- data.frame.drop(refs, "ID")
refs <- merge(refs, id)

refs <- sort.data.frame(~FU+Station+ID+recount.no+Count, dat=refs)

vis <- sort(unique(refs$Visual.Clarity))
dens <- sort(unique(refs$Density))

#set up pdf output
pdf("ccc.plots.pdf")
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


################run code to here################

#then to check




windows()
#now unstitch it all
res <- data.frame(FU=NA, U1=NA,  type=NA, ccc=NA)

for( l in c(1:length(cc)))
  {
    res <- rbind(res, data.frame(FU=cc[l]$ccc.res$FU, U1=cc[l]$ccc.res$U1,  type=cc[l]$ccc.res$type, ccc=cc[l]$ccc.res$ccc$ccc))
  }
res2 <- res[!is.na(res$FU),]

with(res2, bwplot(ccc ~ U1 | type*as.factor(FU), scales=list(x=list(rot=90,cex=0.5))))

windows(record=T)
for(f in c(1:length(fus)))
  {
    t <- res2[res2$FU==fus[f],]
    print(with(t, bwplot(ccc ~ as.factor(U1) | type,layout=c(6,1), scales=list(x=list(rot=90,cex=0.5)),
                         panel=function(x,y,...)
                         {
panel.bwplot(x,y,...)
panel.abline(h=0.5, lty=2, col=1)
                         }
                         )))
    print(with(t, bwplot(ccc ~ type | as.factor(U1),layout=c(6,1), scales=list(x=list(rot=90,cex=0.9)),  panel=function(x,y,...)
                         {
panel.bwplot(x,y,...)
panel.abline(h=0.5, lty=2, col=1)
                         }
                         )))
  }


############################################################
############################################################
#run code to here
#it will output ccc plot
#to check who has passed

id

############################################################
###########################################################





 #this makes plotting easier.
refs$ID.recount <- with(refs,(paste(ID, recount.no, sep="@")))

##### NEED sort.data.frame function first!!

source("C:/Program Files/R/lib/sortdf.r")

### reference <- sort.data.frame(~Station+ID.recount+Minute, dat=reference)
# THIS ORIGINAL LINE DOES NOT WORK!!!!!!
refs <- sort.data.frame(refs, ~Station+ID.recount+Minute)




 
#make a line plot by Station and ID with different lines for each ID and recount
file.name<-paste("N:\\Surveys\\UWTV SURVEYS ARAN\\2013 Aran Grounds\\FU 17 Training\\ReferenceCountsTraining\\R Code","LinePlots", ".jpg", sep="")
jpeg(filename=file.name,width=800,height=580,pointsize=12,bg="white",res=NA,restoreConsole=TRUE)
with(refs, xyplot(Count ~ Minute | as.factor(Station) + as.factor(ID), 
groups=ID.recount,type="a", 
auto.key=list(space="bottom", columns=3, lines=T, points=F), 
main="counts by individuals, lines represent different recounts"))
dev.off()