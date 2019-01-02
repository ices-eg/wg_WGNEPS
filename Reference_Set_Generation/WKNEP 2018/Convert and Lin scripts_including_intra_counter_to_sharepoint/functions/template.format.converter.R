template.format.converter<- function(input.path, output.dir, output.name)
{
  library(openxlsx)
  temp<- read.xlsx(xlsxFile = input.path, sheet = 1, skipEmptyRows = FALSE, skipEmptyCols = FALSE, colNames=F)
  
  #File structure
  n.boxes<- list(page1=5,page2=4)
  n.min<- 10
  line.separation<- 13
  line.seq<- lapply(n.boxes, function(x) { line.separation*(1:x-1) } )
  page1.firstline<- 4
  page2.firstline<- 73
  
  #FU
  FU<- c(temp[page1.firstline-3,"X5"],temp[page2.firstline-3,"X5"])
  if(FU[1] != FU[2] | any(is.na(FU))) { warning("Check FU names in file") }
  
  #Stations
  page1.stations<- unlist(lapply(as.list(temp[page1.firstline+1+line.seq$page1,"X2"]), function(x) {rep(x,n.min)} ))
  page2.stations<- unlist(lapply(as.list(temp[page2.firstline+1+line.seq$page2,"X2"]), function(x) {rep(x,n.min)} ))
  
  #ID
  page1.ID<- rep(temp[page1.firstline-3,"X8"],length(page1.stations)); page1.ID<- c(paste0(page1.ID,"_1"), paste0(page1.ID,"_2"))
  page2.ID<- rep(temp[page2.firstline-3,"X8"],length(page2.stations)); page2.ID<- c(paste0(page2.ID,"_1"), paste0(page2.ID,"_2"))
  
  #Minutes
  page1.mins<- temp[rep(page1.firstline+(1+(1:n.min)),n.boxes$page1) + unlist(lapply(as.list(line.seq$page1), function(x) {rep(x,n.min)} )), "X3"]
  page2.mins<- temp[rep(page2.firstline+(1+(1:n.min)),n.boxes$page2) + unlist(lapply(as.list(line.seq$page2), function(x) {rep(x,n.min)} )), "X3"]
  
  #Counts
  page1.counts.1<- temp[rep(page1.firstline+(1+(1:n.min)),n.boxes$page1) + unlist(lapply(as.list(line.seq$page1), function(x) {rep(x,n.min)} )), "X4"]
  page1.counts.2<- temp[rep(page1.firstline+(1+(1:n.min)),n.boxes$page1) + unlist(lapply(as.list(line.seq$page1), function(x) {rep(x,n.min)} )), "X9"]
  page2.counts.1<- temp[rep(page2.firstline+(1+(1:n.min)),n.boxes$page2) + unlist(lapply(as.list(line.seq$page2), function(x) {rep(x,n.min)} )), "X4"]
  page2.counts.2<- temp[rep(page2.firstline+(1+(1:n.min)),n.boxes$page2) + unlist(lapply(as.list(line.seq$page2), function(x) {rep(x,n.min)} )), "X9"]
  
  #output
  out<- data.frame(Functional_Unit=FU, Station=c(rep(page1.stations,2), rep(page2.stations,2)), Min=as.numeric(c(page1.mins,page1.mins,page2.mins,page2.mins)), Counter_ID=c(page1.ID,page2.ID), Count=as.numeric(c(page1.counts.1,page1.counts.2,page2.counts.1,page2.counts.2)))
  out<- out[order(out$Station,out$Counter_ID,out$Min),]
  write.csv(out, file=paste0(output.dir,output.name,".csv"),row.names=F)
}