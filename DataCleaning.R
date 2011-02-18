#data cleaning
read.xls("list.xls",sheet=1)->lst1
cert.sales <- lst1$T
as.character(cert.sales)->cert.sales
unlist(strsplit(cert.sales,"\n")) -> cert.sales
cert.sales[grep("^\\*",cert.sales)] -> cert.sales
cert.sales <- as.numeric(unlist(strsplit(cert.sales," "))[seq(5,42,by=6)])*1000000    #A problem here is that the split may change the obs's order!

lst1$Claimed.sales->clm.sales
as.character(clm.sales)->clm.sales
substr(clm.sales,24,nchar(clm.sales))->clm.sales
substr(unlist(strsplit(clm.sales,"\n"))[seq(2,14,by=2)],1,3)->clm.sales
as.numeric(clm.sales)*1000000->clm.sales
options(scipen=12)    #the max num of digits.
lst1$Claimed.sales<-clm.sales
write.table(lst1,"lst1.txt")

library(gdata)
options(scipen=12)
for(i in 2:5){
    lst <- read.xls("list.xls",sheet=i)
    clm.sales <- as.character(lst$Claimed.sales)
    clm.sales <- as.numeric(substr(clm.sales,1,3))*1000000
    lst$Claimed.sales <- clm.sales
    eval(parse(text=paste("lst",i," <- lst",sep="")))
}
lst<-rbind(lst2,lst3,lst4,lst5)
write.table(lst,"lst2-5.txt")

names(lst)<-names(lst1)[1:5]
lst<-rbind(lst1[,1:5],lst)
lst.sorted<-with(lst,lst[order(Claimed.sales,decreasing=T),])
write.table(lst.sorted,"SortList.txt")


