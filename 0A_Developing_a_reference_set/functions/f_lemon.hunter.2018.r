#this loads the statistical function to analyse pairwise sets of data


#make a local copy of the recount data
lemon.recounts <- sort.data.frame(~RunID + ObsID+UserID+Block, dat=dat3)

#set the threshold for rejection of particular counts
threshold <- 0.5

cc <-  vector(mode="list", length=0)
pdf(paste0(output.dir, "all.ccc.plots.pdf"), width=7, height=10.5,paper="a4")
#windows(record=T)
#################################################################################################################################
#at each obsID, (don't forget that stations could be repeated!), plot all possible pariwise combinations of RunIDs

Oid <- sort(unique(lemon.recounts$ObsID))
print("Oid")
print(Oid)
print("####")
par(mfrow=c(3,2))
for (o in c(1:length(Oid)))
  {
    temp <- sort.data.frame(~Burrows, dat=lemon.recounts[lemon.recounts$ObsID==Oid[o],])
    temp$Counter_ID <- as.factor(as.character(temp$Counter_ID))
    stn <- unique(temp$ObsID)
    TVID=unique(temp$TVID)
    DVDNo=unique(temp$DVDNo)
    print(paste(Oid[o], length(Oid),stn,unique(temp$RunID),unique(temp$TVID), sep=":"))

    Rs <- unique(temp$RunID)
    l <- length(Rs)
    if(l>1)
      {

        for( i in c(1:(l-1)))
          {
            for(j in c((i+1):l))
              {

                                          print(paste(i,j,Rs[i], Rs[j],TVID=unique(temp$TVID), sep=":"))
                n1 <- paste(Rs[i], unique(temp$Counter_ID[temp$RunID==Rs[i]]), sep=":")
                n2 <- paste(Rs[j], unique(temp$Counter_ID[temp$RunID==Rs[j]]), sep=":")

                temp2 <- temp[temp$RunID %in% c(Rs[i],Rs[j]),]
                                          tempTVID <- unique(temp2$TVID) # just gets the TVID
                                        #get the Blocks which are common to both observers
                n <- tapply.ID(temp2, "Burrows", c("Block", "RunID"), "sum", "t", pad=T)
                nn <-  with(n,tapply(t, Block, sum))
                nnn <- as.numeric(unlist(dimnames(nn)))[!is.na(nn)]
                                        #get the data into 2 columns
                t <- with(temp2[temp2$Block%in% nnn,], tapply(Burrows, list(Block, RunID), sum))
           #          t <- with(temp2[temp2$Block%in% nnn,], tapply(Burrows.nosecs, list(Block, RunID), sum))

                                        #run the analysis
                cc.res <- ccc(t[,1], t[,2])
            #    print(cc.res)
           #     if(!is.na(cc.res$ccc) & cc.res$ccc<threshold)
                  {
                                        #plot the original data, jittering a bit because very often low integer exact matches
                    if(sum(temp2$Burrows)>0)
                      {
                                        #                           plot(jitter(temp2$Burrows[temp2$RunID==Rs[i]& temp2$Block%in% nnn], factor=0.25),jitter(temp2$Burrows[temp2$RunID==Rs[j]& temp$Block%in% nnn], factor=0.25), xlab=n1, ylab=n2, main=Oid[o], sub=paste("CCC= ",round(cc.res$ccc,3), ": Pearson= ", round(cc.res$pearson,3)), xlim=range(t), ylim=range(t) )
                        plot(jitter(t[,1], factor=0.25),jitter(t[,2], factor=0.25), xlab=n1, ylab=n2, main=paste("Station= ",stn, " : ", tempTVID,sep=""), sub=paste("CCC= ",round(cc.res$ccc,3), ": Pearson= ", round(cc.res$pearson,3)), xlim=range(t), ylim=range(t) )
                                        #get a linear fit between the two
                        m <- lm(t[,2]~t[,1])
                        mm <- data.frame(a=t[,1], b=m$fit)
                        mm <- sort.data.frame(~a, dat=mm)
                        lines(range(t), range(t), lty=3)
                        lines(mm$a, mm$b, lty=2, col=2)

                        plot(nnn, t[,1], type="b", col=1, pch=0, lty=1,xlab="Block",xlim=range(nnn), ylim=range(t))
                        points(nnn, t[,2],  col=2, pch=3)
                        lines(nnn, t[,2],  col=2, lty=2)


                      }
                    else(cc.res <- list(ccc=1, pearson=1, u=1, v=1))
                  }

             #  print(paste(i,j, sep=":"))
                cc <- c(cc, list( ccc.res=list(name=paste(Oid[o], n1, n2, sep=":"),stn=stn, TVID=TVID, DVDNo=DVDNo,R1=Rs[i],R2=Rs[j], RunID=Oid[o], ccc=cc.res, U1=unique(temp$Counter_ID[temp$RunID==Rs[i]]), U2=unique(temp$Counter_ID[temp$RunID==Rs[j]]), density=mean(c(t[,1], t[,2])))))
              }
          }

      }
  }
dev.off()

#now work through the results and put them into a dataframe for merging back into the original data

#ccc.results <- data.frame(RunID=NA, RunID=NA, ccc=NA, pearson=NA )
ccc.results <- data.frame(RunID=NA,stn=NA,TVID=NA, DVDNo=NA, ccc=NA, pearson=NA , U1=NA, U2=NA, R1=NA, R2=NA,density=NA)
for(i in c(1:length(cc)))
     {
     #  if(cc[i]$ccc.res$ccc$ccc<0.4)
         {
#           print(i)
           print(paste(i,cc[i]$ccc.res$TVID,cc[i]$ccc.res$DVDNo,cc[i]$ccc.res$stn,cc[i]$ccc.res$ccc$ccc, sep="  "))
           ccc.results <- rbind(ccc.results, data.frame(RunID=cc[i]$ccc.res$RunID,  stn=cc[i]$ccc.res$stn,TVID=cc[i]$ccc.res$TVID,DVDNo=cc[i]$ccc.res$DVDNo, ccc=cc[i]$ccc.res$ccc$ccc, pearson=cc[i]$ccc.res$ccc$pearson, U1=cc[i]$ccc.res$U1[1], U2=cc[i]$ccc.res$U2[1],R1=cc[i]$ccc.res$R1,R2=cc[i]$ccc.res$R2, density=cc[i]$ccc.res$density))
         }
     }#
ccc.results <- ccc.results[!is.na(ccc.results$stn),]

ccc.results$ID1 <- with(ccc.results, paste(RunID,R1, U1))
ccc.results$ID2 <- with(ccc.results, paste(RunID,R2, U2))

#get a list of all the combinations of RunID and user
all.IDs <- c(ccc.results$ID1,ccc.results$ID2)

goodcounts <- ccc.results[ccc.results$ccc>=threshold | ccc.results$density<1.5,]
goodobs <- goodcounts$RunID
badcounts <- ccc.results[ccc.results$ccc<threshold | ccc.results$density<1.5 ,]
badcounts <- badcounts[ !badcounts$RunID%in% goodobs,]
good.IDs <- unique(c(goodcounts$ID1,goodcounts$ID2)) # this is the list of "good" RunID user combinations

##badcounts
goodcounts
