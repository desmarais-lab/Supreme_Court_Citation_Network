# code assumes working directory is ./Supreme_Court_Citation_Network/R-Code

nodes_per_year <- 3
years <- 3

greyseq <- paste("grey",round(seq(30,90,length=years)),sep="")

cases <- years*nodes_per_year
pdf("../Tex/images/daggish.pdf",height=5,width=5)
par(las=1)
plot(1:(cases-1),1:(cases-1),type="n",xlim=c(0,cases),ylim=c(0,cases),xaxs="i",yaxs="i",xaxt="n",yaxt="n",ylab="sending case",xlab="receiving case")

yrstart=0
for(yr in 1:years){
	rect(xleft=0,xright=yrstart+nodes_per_year,ybottom=yrstart,ytop=yrstart+nodes_per_year,col=greyseq[yr])
	rect(xleft=yrstart+nodes_per_year,xright=yrstart+nodes_per_year,ybottom=yrstart,ytop=yrstart+nodes_per_year,col=greyseq[yr])
	yrstart=yrstart+nodes_per_year
	
}

abline(h=0:cases,v=0:cases)


set.seed(1234)
nzeros <- round(0.7*cases^2)
zeros.sender <- c(1:cases,sample(1:cases,nzeros,rep=T))
zeros.receiver <- c(1:cases,sample(1:cases,nzeros,rep=T))

for(i in 1:nzeros){
	rect(xleft = zeros.receiver[i]-1,xright=zeros.receiver[i],ybottom=zeros.sender[i]-1,zeros.sender[i],col="white")
}


yrstart=0
for(yr in 1:years){
	rect(xleft=yrstart+nodes_per_year,xright=cases,ybottom=yrstart,ytop=yrstart+nodes_per_year,col="white")
	yrstart=yrstart+nodes_per_year
	
}

axis(1,at=(1:cases-0.5),lab=as.character(1:cases))
axis(2,at=(1:cases-0.5),lab=as.character(1:cases))

dev.off()
