#Jan. 29 text graphs trail one.
old.par <- par()

lst<-read.table("SortList.txt")
#pdf("graffiti2.pdf")
png("graf.png",height=1500,width=1500)
#svg("graffiti.svg",8,8)
par(bg="black")
plot(0:144,0:144,type="n",bty="n",xaxt="n",yaxt="n",xlab="",ylab="")
cor.x <- sample(144)
cor.y <- sample(144)
#cor.x <- active.since
#cor.y <- active.yrs
sales <- sqrt(lst$Claimed.sales)/200
#text(sample(100,144,replace=T),sample(100,144,replace=T),lst$Artist,
#     cex=lst$Claimed.sales/100000000,col=rainbow(144,alpha=.5)[sample(144)])
symbols(cor.x,cor.y,circles=sales,inches=F,fg="#6495ED80",bg="#6495ED80",add=T)
text(cor.x,cor.y,lst$Artist,cex=sales/19,col="#FFD700AA")
dev.off()

#help(chartr)
genre <- lapply(strsplit(toupper(as.character(lst$Genre)),"/|,"),function(x)gsub("(^ +)|( +$)","",x))
table(as.factor(unlist(genre)))
#get the active periods
active <- gsub("[pP]resent","2011",as.character(lst$Period.active))
active.since <- as.numeric(substr(active,1,4))
active.to <- as.numeric(substr(active,nchar(active)-3,nchar(active)))
active.yrs <- as.numeric(substr(active,nchar(active)-3,nchar(active)))-as.numeric(substr(active,1,4)) + 1

active.mid <- apply(rbind(active.since,active.to),2,mean)
active.summit <- (active.yrs/2)^2

act.yrs <- active.yrs[order(active.yrs)]
act.since <- active.since[order(active.yrs)]
act.to <- active.to[order(active.yrs)]
act.summit <- active.summit[order(active.yrs)]

pdf("g.pdf")
par(bg="black")
plot(c(1930,2012),c(-65,95),type="n",bty="n",xaxt="n",yaxt="n",xlab="",ylab="")
abline(h = 0,col="white")
id <- 1:144
id.pop <- grep("POP",genre)
id.rock <- grep("ROCK",genre)
id.hh <- grep("HOP",genre)
id.country <- grep("COUNTRY",genre)
id.metal <- grep("METAL",genre)
id.rb <- grep("R&B",genre)
id.soul <- grep("SOUL",genre)
id.danc <- grep("DANCE",genre)
id.disco <- grep("DISCO",genre)
id.other <- id[-unique(c(id.pop,id.rock,id.hh,id.country,id.metal,id.rb,id.soul,id.danc,id.disco))]
#0x8DD3C7; 0xFFFFB3; 0xBEBADA; 0xFB8072; 0x80B1D3; 0xFDB462; 0xB3DE69; 0xFCCDE5; 0xD9D9D9; 0xBC80BD; 
for(i in 1:144){
    if(any(id.pop == i))
        curve(0.5*i*(-1)^i/act.summit[i]*(x-act.since[i])*(x-act.to[i]),add=T,xlim=c(act.since[i],act.to[i]),col="#00FF00AA")
    if(any(id.rock == i))
        curve(0.5*i*(-1)^i/act.summit[i]*(x-act.since[i])*(x-act.to[i]),add=T,xlim=c(act.since[i],act.to[i]),col="#FF0000AA")
    if(any(id.country == i))
        curve(0.5*i*(-1)^i/act.summit[i]*(x-act.since[i])*(x-act.to[i]),add=T,xlim=c(act.since[i],act.to[i]),col="#B23AEEAA")
    if(any(id.metal == i))
        curve(0.5*i*(-1)^i/act.summit[i]*(x-act.since[i])*(x-act.to[i]),add=T,xlim=c(act.since[i],act.to[i]),col="#BEBEBEAA")
    if(any(id.soul == i))
        curve(0.5*i*(-1)^i/act.summit[i]*(x-act.since[i])*(x-act.to[i]),add=T,xlim=c(act.since[i],act.to[i]),col="#00CED1AA")
    if(any(id.danc == i))
        curve(0.5*i*(-1)^i/act.summit[i]*(x-act.since[i])*(x-act.to[i]),add=T,xlim=c(act.since[i],act.to[i]),col="#FFD700AA")
    if(any(id.hh == i))
        curve(0.5*i*(-1)^i/act.summit[i]*(x-act.since[i])*(x-act.to[i]),add=T,xlim=c(act.since[i],act.to[i]),col="#FFA0A0AA")
    if(any(id.rb == i))
        curve(0.5*i*(-1)^i/act.summit[i]*(x-act.since[i])*(x-act.to[i]),add=T,xlim=c(act.since[i],act.to[i]),col="#EE8262AA")
    if(any(id.disco == i))
        curve(0.5*i*(-1)^i/act.summit[i]*(x-act.since[i])*(x-act.to[i]),add=T,xlim=c(act.since[i],act.to[i]),col="#4682B4AA")
    if(any(id.other == i))
        curve(0.5*i*(-1)^i/act.summit[i]*(x-act.since[i])*(x-act.to[i]),add=T,xlim=c(act.since[i],act.to[i]),col="#FFAAFFAA")
}
legend("topleft",legend=c("POP","ROCK","COUNTRY","METAL","SOUL","DANCE","HIP-HOP","R&B","DISCO","OTHER"),
       col=c("#00FF00AA","#FF0000AA","#B23AEEAA","#BEBEBEAA","#00CED1AA","#FFD700AA","#FFA0A0AA","#EE8262AA","#4682B4AA","#FFAAFFAA"),
       lty=1,text.col="white",cex=.7)
dev.off()
#add the genre and artist name to the above plot
