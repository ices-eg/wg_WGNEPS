#analysis of counts to generate reference set for FU20-21
#5 counters during 2015 survey generated counts
#decided to use all counters given the variability.
setwd("N://Surveys/UWTV SURVEYS FU2022 CELTIC SEA/2015 Labadie/Ref counts/")
library(plyr)
library(dplyr)
library(ggplot2)
library("magrittr")

ref <- read.csv("N:\\Surveys\\UWTV SURVEYS FU2022 CELTIC SEA\\2015 Labadie\\Ref counts\\ref.csv")
summary(ref)
p <- ggplot(aes(Minute,Count), data=ref, group=recount.no) +geom_line(aes(colour=ID))  + theme_bw()
windows()
p + facet_grid(recount.no ~ Station, as.table = T) + geom_smooth()

bur<-ref %>% group_by(FU,Station,Minute)%>%summarise(reference=mean(Count))
summary(bur)

#write it out and store the results somewhere safe!
write.table(bur, file="fu2021.reference.counts.txt", sep="\t", row.names=F, col.names=T)


library("magrittr")
names(ref) %<>% tolower
ref <- ref %>% arrange(station,survey,id,minute)
Oid <- sort(unique(ref$station))

pdf("test.pdf", width=7, height=10.5,paper="a4")
par(mfrow=c(3,2))

Oid <- sort(unique(ref$station)
for (o in c(1:length(Oid)))
{
  temp<-ref[ref$station==Oid[o],]
  temp <- temp[order(temp$minute),]
  print(paste(Oid[o]))
  
  Rs <- unique(temp$id)
  l <- length(Rs)
  if(l>1)
    
  {
    for( i in c(1:(l-1)))
    {
      for(j in c((i+1):l))
      {
        
        temp2 <- temp[temp$id %in% c(Rs[i],Rs[j]),]
        temp2<-dcast(temp2,minute~id, value.var = "count", sum)
       
        
        plot(temp2$minute, temp2[,2], type="b", col=1, pch=0,lty=1,xlab="Minute", ylab = "Burrows counted", xlim=range(temp2$Minute), ylim=range(temp2[,2:3]),main = paste("Video Line =", unique(temp$Video_Line_Name)))
        points(temp2$minute, temp2[,3],  col=2, pch=3)
        lines(temp2$minute, temp2[,3],  col=2, lty=2) 
        
      }}}}