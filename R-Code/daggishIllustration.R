# code assumes working directory is ./Supreme_Court_Citation_Network/R-Code

CurlyBraces <- function(x0, x1, y0, y1, pos = 1, direction = 1, depth = 1) {

    a=c(1,2,3,48,50)    # set flexion point for spline
    b=c(0,.2,.28,.7,.8) # set depth for spline flexion point

    curve = spline(a, b, n = 50, method = "natural")$y * depth

    curve = c(curve,rev(curve))

    if (pos == 1){
        a_sequence = seq(x0,x1,length=100)
        b_sequence = seq(y0,y1,length=100)  
    }
    if (pos == 2){
        b_sequence = seq(x0,x1,length=100)
        a_sequence = seq(y0,y1,length=100)      
    }

    # direction
    if(direction==1)
        a_sequence = a_sequence+curve
    if(direction==2)
        a_sequence = a_sequence-curve

    # pos
    if(pos==1)
        lines(a_sequence,b_sequence, lwd=1.5,   xpd=NA) # vertical
    if(pos==2)
        lines(b_sequence,a_sequence, lwd=1.5, xpd=NA) # horizontal

}

nodes_per_year <- 3
years <- 3

greyseq <- paste("grey",round(seq(30,80,length=years)),sep="")

cases <- years*nodes_per_year
pdf("../Tex/images/daggish.pdf",height=5,width=6)
par(las=1,mar=c(4,8,1,1))
plot(1:(cases-1),1:(cases-1),type="n",xlim=c(0,cases),ylim=c(0,cases),xaxs="i",yaxs="i",xaxt="n",yaxt="n",ylab="",xlab="")

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


CurlyBraces(x0=-.75, x1=-.75, y0=cases-nodes_per_year, y1=cases, pos = 1, direction = 2, depth = .5) 

CurlyBraces(x0=-.75, x1=-.75, y0=0, y1=cases-nodes_per_year, pos = 1, direction = 2, depth = .5) 

CurlyBraces(x0=-1.95, x1=-1.95, y0=0, y1=cases, pos = 1, direction = 2, depth = .5) 

library(latex2exp)

Ct <- TeX('$C_t$')
Clt <- TeX('$C_{<t}$')
Clet <- TeX('$C_{ \\leq t}$')

text(-1.55,(cases-nodes_per_year+cases)/2,Ct,xpd=T,cex=1.5)
text(-1.55,(cases-nodes_per_year)/2,Clt,xpd=T,cex=1.5)
text(-2.75,(cases)/2,Clet,xpd=T,cex=1.5)

title(xlab="receiving case",line=2.25)

dev.off()
