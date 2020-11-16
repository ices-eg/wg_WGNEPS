# Input:    - reference counts CSV files from Annotation app
#           - reviewers counts CSV files from Annotation app

# Output:   - PDF with all the Lin's pairing plots
#           - CSV file with all the Lin's CCC values for all the pairings

## There are some sections you need to change depending upon your file names: ##
## search for the word CHANGE ##


# Setup----
rm(list = ls())
gc()


# Set up FU.
## CHANGE fu name and Lin's CCC treshold ************************************************###
fu.name <- "FU22"
# threshold <- 0.6 # no need of setting
###**************************************************************************************###

# Set up directories

## CHANGE THE DIRECTORIES if needed *****************************************************###
getwd()
wdir <- "/C_Lins_during_survey/Marine_Institute_Ireland"
func.dir <- paste0(getwd(), wdir, "/functions/")
input.dir <- paste0(getwd(), wdir, "/input/")
output.dir <- paste0(getwd(), wdir,"/output/")
###**************************************************************************************###
# Create folders if they don't exist
if(!dir.exists(input.dir)) { dir.create(input.dir) }
if(!dir.exists(output.dir)) { dir.create(output.dir) }


# Libraries
library("epiR")
library("reshape2")
library("mapplots")
library("shapefiles")
library("gridExtra")
library("grid")
library("lme4")
library("dplyr")


# The following lines extract the recount data from the MARINE INSTITUTE'S DATABASE.
# As we are not uploading our full database to GitHub, I include an alternative extraction code from some example CSV files after this (line)

# Extract recounts from Database ----
  # ##you should be able to connect to the access database and extract the recounts table
  # ##This only works on 32bit windows
  # ##below is a example, but I reverted to reading in an extrcation in .csv
  # library("RODBC")
  # 
  # ### NOTES
  # #Ensure that the 3rd reviewer enters and validates his/her counts
  # #delete additional minutes else will give a false positive result
  # #run queries 1-5 in this Rscript
  # #then run the Lins verification code
  # #if no agreement after 3rd count - check the station with all 3 counters for counting skills but we will use the average of 3 counters
  # 
  # # The database must be CLOSED to run the queries
  # setwd("N:/Surveys/UWTV SURVEYS FU2022 CELTIC SEA/2020 Smalls/Final Data")
  # setwd("C:/Users/maristegui/Desktop/CV20017/Nep_surv_FU22")
  # #channel <- odbcConnectAccess("C:/Users/maristegui/Desktop/UWTV_CelticSea_Leg2/UWTV SURVEYS FU2022 CELTIC SEA/2018 Smalls/Final Data/NEPHROPS_MULTILOG_CV18022")
  # channel <- odbcConnectAccess("./SMALLS_NEPHROPS_MULTILOG_CV20017.mdb")
  # #channel <- odbcConnectAccess("N:/Surveys/UWTV SURVEYS FU2022 CELTIC SEA/2018 Smalls/Final Data/NEPHROPS_MULTILOG_CV18022")
  # 
  # # Query 1
  # sqlQuery(channel,"
  # SELECT Recounts.SurveyID, Recounts.Video_Line_Name, Recounts.VideoOperatorID, Recounts.Minute, Recounts.StopTime, Recounts.StartTime
  # FROM Recounts
  # GROUP BY Recounts.SurveyID, Recounts.Video_Line_Name, Recounts.VideoOperatorID, Recounts.Minute, Recounts.StopTime, Recounts.StartTime
  # HAVING (((Recounts.StopTime)>=#12/30/1899# And (Recounts.StopTime)<=#12/30/1899 0:15:0#) AND ((Recounts.StartTime)>=#12/30/1899# And (Recounts.StartTime)<=#12/30/1899 0:15:0#));
  #          ")
  # # Query 2
  # sqlDrop(channel, "RecountNotUsuable")
  # sqlQuery(channel,"
  # SELECT Recounts.SurveyID, Recounts.Video_Line_Name, Recounts.VideoOperatorID, Recounts.Minute, Recounts.StopTime, Recounts.StartTime, Recounts!StartTime-Recounts!StopTime AS DifferenceNumber, Recounts!StartTime-Recounts!StopTime AS DifferenceTime INTO RecountNotUsuable
  # FROM Recounts
  # GROUP BY Recounts.SurveyID, Recounts.Video_Line_Name, Recounts.VideoOperatorID, Recounts.Minute, Recounts.StopTime, Recounts.StartTime, Recounts!StartTime-Recounts!StopTime, Recounts!StartTime-Recounts!StopTime
  # HAVING (((Recounts.StopTime)>=#12/30/1899# And (Recounts.StopTime)<=#12/30/1899 0:15:0#) AND ((Recounts.StartTime)>=#12/30/1899# And (Recounts.StartTime)<=#12/30/1899 0:15:0#));
  #          ")
  # # Query 3
  # sqlQuery(channel,"
  # SELECT RecountNotUsuable.SurveyID, RecountNotUsuable.Video_Line_Name, RecountNotUsuable.VideoOperatorID, RecountNotUsuable.Minute, RecountNotUsuable.StopTime, RecountNotUsuable.StartTime, RecountNotUsuable.DifferenceTime, RecountNotUsuable.DifferenceNumber, IIf([DifferenceNumber]>=0.000347222222222222,1000) AS Unusuable
  # FROM RecountNotUsuable;
  #          ")
  # 
  # # Query 4
  # sqlDrop(channel, "UnusuableRecountMinsTable")
  # sqlQuery(channel,"
  # SELECT [Qry3-IdentifyUnusuablesForRecounts].SurveyID, [Qry3-IdentifyUnusuablesForRecounts].Video_Line_Name, [Qry3-IdentifyUnusuablesForRecounts].Minute, [Qry3-IdentifyUnusuablesForRecounts].Unusuable INTO UnusuableRecountMinsTable
  # FROM [Qry3-IdentifyUnusuablesForRecounts]
  # WHERE ((([Qry3-IdentifyUnusuablesForRecounts].Unusuable) Like 1000))
  # GROUP BY [Qry3-IdentifyUnusuablesForRecounts].SurveyID, [Qry3-IdentifyUnusuablesForRecounts].Video_Line_Name, [Qry3-IdentifyUnusuablesForRecounts].Minute, [Qry3-IdentifyUnusuablesForRecounts].Unusuable;
  # ")
  # 
  # # Query 5
  # sqlDrop(channel, "Recounts-Clean")
  # sqlQuery(channel,"
  # SELECT Recounts.RecountID, Recounts.Video_Line_Name, Recounts.SurveyID, Recounts.VideoOperatorID, Recounts.Minute, Recounts.BurrowCount, Recounts.NephropsIn, Recounts.NephropsOut, Recounts.StopTime, Recounts.StartTime, UnusuableRecountMinsTable.Unusuable INTO [Recounts-Clean]
  # FROM Recounts LEFT JOIN UnusuableRecountMinsTable ON (Recounts.Minute = UnusuableRecountMinsTable.Minute) AND (Recounts.SurveyID = UnusuableRecountMinsTable.SurveyID) AND (Recounts.Video_Line_Name = UnusuableRecountMinsTable.Video_Line_Name)
  # GROUP BY Recounts.RecountID, Recounts.Video_Line_Name, Recounts.SurveyID, Recounts.VideoOperatorID, Recounts.Minute, Recounts.BurrowCount, Recounts.NephropsIn, Recounts.NephropsOut, Recounts.StopTime, Recounts.StartTime, UnusuableRecountMinsTable.Unusuable
  # HAVING (((UnusuableRecountMinsTable.Unusuable) Is Null));
  # ")
  # 
  # 
  # Recounts <- sqlFetch(channel, "Recounts-Clean")
  # Ops <- sqlFetch(channel, "Video_Operator")
  # Locations <- sqlFetch(channel, "Video_Table")
  # 
  # close(channel)


# Alternative for GitHub: Extract recounts data from CSV files ----
Recounts <- read.csv(paste0(input.dir, "Recounts.csv"))
Ops <- read.csv(paste0(input.dir, "Ops.csv"))
Locations <- read.csv(paste0(input.dir, "Locations.csv"))




#checks number of recount stations ----
length(unique(Recounts$Video_Line_Name))
Recounts <- Recounts[order(Recounts$Video_Line_Name, Recounts$SurveyID,Recounts$VideoOperatorID,Recounts$Minute),] 

selst <- Recounts %>% group_by(Video_Line_Name) %>% 
          summarise(hatn =sum(BurrowCount)/length(BurrowCount))  %>% 
          filter(hatn> 1.5)
selst <- as.list(selst[1])

SummedBurrow<-aggregate(BurrowCount~Video_Line_Name, sum, data = Recounts)
ZeroBurrow<-subset(SummedBurrow, BurrowCount > 0)
RecountsP<-subset(Recounts, Video_Line_Name %in% selst$Video_Line_Name)
length(unique(Recounts$Video_Line_Name))


#this code sets up pdf and runs Lins statistical test on input data ----
#set up working directory where pdf will be outputted

pdf(paste0(output.dir, fu.name, ".2020.pdf"), width=7, height=10.5,paper="a4")

Lins<- NULL
Oid <- sort(unique(RecountsP$Video_Line_Name))
par(mfrow=c(3,2))
for (o in c(1:length(Oid)))
{
  temp<-RecountsP[RecountsP$Video_Line_Name==Oid[o],]
  temp <- temp[order(temp$Minute),]
  print(paste(Oid[o]))
  
  Rs <- unique(temp$VideoOperatorID)
  l <- length(Rs)
  if(l>1)
  {
    for( i in c(1:(l-1)))
    {
      for(j in c((i+1):l))
      {
        temp2 <- temp[temp$VideoOperatorID %in% c(Rs[i],Rs[j]),]
        temp2<-dcast(temp2,Minute~VideoOperatorID, value.var = "BurrowCount", sum)
        c1<-as.numeric(names(temp2[2]))
        c2<-as.numeric(names(temp2[3]))
        c1<-Ops$Initials[match(c1,Ops$VideoOperatorID)]
        c2<-Ops$Initials[match(c2,Ops$VideoOperatorID)]
      
        tmp.ccc <- epi.ccc(temp2[,2], temp2[,3], ci = "z-transform",conf.level = 0.95)
        z <- lm(temp2[,3] ~ temp2[,2])
        par(pty = "s")
        plot(temp2[,2],temp2[,3], xlab = c1,ylab = c2, pch = 16,
             main = paste("Video Line =", unique(temp$Video_Line_Name),"; Lin's CCC =",round(tmp.ccc$rho.c[1],2), sep = " "))
        abline(a = 0, b = 1, lty = 2)
        abline(z, lty = 1)
        text(temp2[,2]+.1,(temp2[,3]),temp2[,1], cex = 0.6)

        plot(temp2$Minute, temp2[,2], type="b", col=1, pch=0, lty=1,xlab="Minute", ylab = "Burrows counted", xlim=range(temp2$Minute), ylim=range(temp2[,2:3]))
        points(temp2$Minute, temp2[,3],  col=2, pch=3)
        lines(temp2$Minute, temp2[,3],  col=2, lty=2) 
        
        LinsVL<-as.data.frame(c(unique(temp$Video_Line_Name),Rs[i],Rs[j],round(tmp.ccc$rho.c[1],2)))
        names(LinsVL)<-c("VideoLine", "Counter1","Counter2","LinsCCC")
        Lins<-as.data.frame(rbind(LinsVL,Lins))
      }}}} 

dev.off()

output <- Lins

output$stn <- paste0("stn_", output$VideoLine)

Ops1 <- Ops[,c("VideoOperatorID", "Initials")]
names(Ops1) <- c("Counter1", "Initials1")
Ops2 <- Ops1
names(Ops2) <- c("Counter2", "Initials2")
output <- merge(Ops1, output)
output <- merge(Ops2, output)

write.csv(output[-nrow(Lins), c("stn", "VideoLine", "Initials1", "Initials2", "LinsCCC")],
          paste0(output.dir, fu.name, "_2020_lins.csv"), row.names=F)



