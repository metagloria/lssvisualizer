# three functions for extracting times & run IDs
gametime <- function(x){
  ttx <- as.numeric(unlist(strsplit(unlist(strsplit(strsplit(x,":")[[1]],"<")),">")))[3:5]
  return(3600*ttx[1] + 60*ttx[2] + ttx[3])
}

attemptid <- function(x){ 
  as.numeric(unlist(strsplit(unlist(strsplit(x,"="))[2]," "))[1])
}

timeid <- function(x){        
  ttx<-as.numeric(unlist(strsplit(unlist(strsplit(x,"="))[2],">"))[1])
  if(is.na(ttx)) ttx <- -9
  if(ttx<0) ttx <- ttx+9999
  return(ttx)
}

# read data in as a single column of character strings
lss <- read.csv("Tom 4B.lss")
lss[,1] <- as.character(lss[,1])

# extract data
segtime <- rep(NA,nrow(lss))
segcpnum <- rep(0,nrow(lss))
segdata <- rep("",nrow(lss))
cpnum <- 0
for(i in 1:length(segtime)){
  if(length(grep("<Name>",lss[i,]))>0) cpnum <- cpnum+1
  segcpnum[i] <- cpnum
  if(length(grep("GameTime",lss[i,]))>0){ 
    segtime[i] <- gametime(lss[i,])
    segdata[i] <- "gt"
  }
  if(length(grep("Time id",lss[i,]))>0){ 
    segtime[i] <- timeid(lss[i,])
    segdata[i] <- "id"
  }
  if(length(grep("Attempt id",lss[i,]))>0){
    segtime[i] <- attemptid(lss[i,])
    segdata[i] <- "id"
  }
}
#cbind(segdata,segcpnum,segtime)[segdata=="id" & is.na(segtime),]

# reshape data into a matrix of numbered runs
ilruns <- matrix(NA,nrow=max(segtime[segdata=="id"]),ncol=cpnum+1)
index <- 0
for(i in 1:length(segdata)){
  if(segdata[i]=="id" & segtime[i]>0) index <- segtime[i]
  if(segdata[i]=="gt"){
    ilruns[index,segcpnum[i]+1] <- segtime[i]
  }
}
ilruns <- ilruns[-which(rowSums(ilruns,na.rm=T)==0),]

ttgolds<-apply(ilruns,2,min,na.rm=T)

# select a subset of data relevant to plot, and plot it
# manual tuning begins here:
# YOU have to pick the column to define ilruns2, the colors, the main title, etc.
ilruns2 <- subset(ilruns,!is.na(ilruns[,2]))
#ilruns2 <- subset(ilruns2,ilruns2[,1]<120)
#ilruns2 <- ilruns2[-(1:169),]
dim(ilruns2)
#ilruns2[,1] <- rowSums(ilruns2[,2:4])
#ilruns2 <- subset(ilruns2,ilruns2[,2]<30)
colSums(is.na(ilruns2))
#cpcols <- c("gray",rep("blue",3),rep("magenta",3),rep("red",4),rep("violet",4),rep("darkorange",6),rep("forestgreen",7),rep("tan",9))
#cpcols <- rep("cornflowerblue",4)
#cpcols <- hsv(runif(50,min=0,max=8/9),runif(50,min=0.6,max=1),1)
cpcols1 <- hsv(seq(0.02,0.48,length.out=10),0.8,1)
cpcols2 <- hsv(seq(0.5,0.98,length.out=10),0.8,1)
cpcols <- rep(0,20)
cpcols[2*(1:10) - 1] <- cpcols1
cpcols[2*(1:10)] <- cpcols2
par(mar=c(5.1,4.1,2.1,2.1))
plot(0,0,type="n",xlab="Attempt (out of first screen)",ylab="Time (seconds)",xlim=c(1,nrow(ilruns2)),ylim=c(0,max(ilruns2[,1],na.rm=T)),main="Tom 4B")
abline(h=seq(60,6000,60),col="gray",lty=2)
for(i in 1:nrow(ilruns2)){
  rect(i-0.45,0,i+0.45,ilruns2[i,2],density=-1,col=cpcols[1],border=1)
  if(ilruns2[i,2]==ttgolds[2]) rect(i-0.45,0,i+0.45,ilruns2[i,2],density=-1,col="gold",border=1)
  for(j in 3:ncol(ilruns2)){ 
    if(!is.na(ilruns2[i,j])){ 
      rect(i-0.45,sum(ilruns2[i,2:j]),i+0.45,sum(ilruns2[i,2:(j-1)]),density=-1,col=cpcols[j-1],border=1)
      if(ilruns2[i,j]==ttgolds[j]) rect(i-0.45,sum(ilruns2[i,2:j]),i+0.45,sum(ilruns2[i,2:(j-1)]),density=-1,col="gold",border=1)
    }
  }
  if(!is.na(ilruns2[i,1])) if(ilruns2[i,1]==ttgolds[1]) rect(i-0.45,0,i+0.45,ilruns2[i,1],density=0,border="gold",lwd=2)
}


# spaghetti plot
ttavg <- colMeans(ilruns2,na.rm=T)
paceavg <- cumsum(ttavg[2:length(ttavg)])
plot(0,0,type="n",xlab="Attempt (completed)",ylab="Seconds above average",xlim=c(1,ncol(ilruns2)),ylim=c(-25,30),main="Tom 4B")
for(i in 1:nrow(ilruns2)){
  ttrun <- cumsum(ilruns2[i,2:ncol(ilruns2)])-paceavg
  lines(1:length(ttrun),ttrun,col=hsv((i-1)/nrow(ilruns2),0.8,1))
}