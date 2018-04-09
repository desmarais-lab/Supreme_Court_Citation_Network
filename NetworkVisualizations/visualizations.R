#Preparation
rm(list=ls())
#setwd("C:/Users/TC/Dropbox/28_Citation Networks/R")

library(statnet)

load("supreme.RData")
rm(model20.mle)

#Distinguishing between different courts
#https://www.supremecourt.gov/about/members_text.aspx
cj<-list(
  hughes=seq(which(node_attributes$usid=="280US390"),which(node_attributes$usid=="313US508")), #February 24, 1930	
  stone=seq(which(node_attributes$usid=="314US1"),which(node_attributes$usid=="328US781")), #July 3, 1941	
  vinson=seq(which(node_attributes$usid=="329US1"),which(node_attributes$usid=="346US324")), #June 24, 1946	
  warren=seq(which(node_attributes$usid=="346US325"),which(node_attributes$usid=="395US710")), #October 5, 1953	
  burger=seq(which(node_attributes$usid=="395US711"),which(node_attributes$usid=="478US1311")), #June 23, 1969	
  rehnquist=seq(which(node_attributes$usid=="479US1"),which(node_attributes$usid=="534US533")) #September 26, 1986
  #Roberts: September 29, 2005
)

court<-rep(0,nrow(node_attributes))
for(i in 1:length(cj)){
  court[cj[[i]]]<-i
}

#Some nodal attributes
scnetwork%v%"caseid"<-node_attributes$caseid
scnetwork%v%"court"<-court
scnetwork%v%"indeg"<-node_attributes$indeg
scnetwork%v%"auth"<-node_attributes$auth
scnetwork%v%"iso"<-ifelse(degree(scnetwork)==0,1,0)

#Subsetting the network objects for different figures
#subset to 1937-2002 (Figure 2)
net<-scnetwork%s%which(scnetwork%v%"year">1936)
el.net<-edgelist[which((edgelist[,1]%in%(net%v%"caseid"))&(edgelist[,2]%in%(net%v%"caseid"))),]
first1937<-(net%v%"caseid")[which((net%v%"year")==1937)][1]

#subset to 1985 to 1989
five<-scnetwork%s%which((scnetwork%v%"year")%in%seq(1985,1989))
el.five<-edgelist[which((edgelist[,1]%in%(five%v%"caseid"))&(edgelist[,2]%in%(five%v%"caseid"))),]
first1985<-(net%v%"caseid")[which((net%v%"year")==1985)][1]

# Plotting
pink.c<-rgb(1,0.42,0.71)
blue.c<-rgb(0.42,0.71,1)
yell.c<-rgb(1,1,0.42)
par(mar=c(0,0,0,0))

# 1937-2002, by Chief Justice
set.seed(7697225)
net.layout<-network.layout.fruchtermanreingold(net,list(repulse.rad=network.size(net)^2*log(network.size(net))/5))

# Need to play around with these values to get a good looking plot.
adj.layout<-apply(net.layout,2,function(x){x<-x-mean(x);x<-400*x/max(abs(x));return(x)})

lsize<-410
vsize<-300
adj.curv<-c(750,250,-50,50,400,900)

# Just for visual inspection.
plot.grid<-persp(seq(-lsize,lsize,lsize),seq(-lsize,lsize,lsize),matrix(lsize*6,ncol=3,nrow=3),zlim=c(lsize,lsize*6),border=rgb(0,0,0,1),box=F,axes=F,
                 phi=20,theta=-30,d=1.2,r=4,scale=F)
for(i in 1:6){
  poly.3d<-trans3d(x=c(-lsize,-lsize,lsize,lsize)-adj.curv[i],y=c(-lsize,lsize,lsize,-lsize),z=(i*vsize),plot.grid)
  polygon(poly.3d$x,poly.3d$y,col=rgb(0.8,0.5,0.9,0.2),border=NA)
}

node.adj<-as.numeric(as.character(cut(net%v%"court",breaks=c(0,1,2,3,4,5,6)+0.5,labels=adj.curv)))
adj.layout[,1]<-adj.layout[,1]-node.adj
pts.3d<-trans3d(adj.layout[,1],
                adj.layout[,2],
                net%v%"court"*vsize,
                plot.grid)
pts.3d<-matrix(unlist(pts.3d),ncol=2,byrow=F)

# Plotting
layers<-cj
pts.cex<-0.01*(net%v%"indeg"+20)
v.cex<-4
v.col<-adjustcolor(ifelse(net%v%"salience",yell.c,pink.c),alpha.f=0.9)
v.lwd<-0.1
v.lcol<-rgb(0,0,0,0.3)
e.cex<-0.1
e.col<-rgb(0,0,0,0.008)
l.col<-adjustcolor(blue.c,alpha.f=0.2)
l.lcol<-rgb(0,0,0,0.6)
l<-6
l.lab<-toupper(attributes(layers)$names)
lab.cex<-1.5

pdf("citations1.pdf",height=20,width=20)
par(mar=c(0,0,0,0))
plot.new()
plot.window(xlim=c(-0.25,0.1),ylim=c(-0.27,0.08))

# Drawing nodes and edges by layer
for(i in 1:l){
  poly.3d<-trans3d(x=c(-lsize,-lsize,lsize,lsize)-adj.curv[i],y=c(-lsize,lsize,lsize,-lsize),z=(i*vsize),plot.grid)
  polygon(poly.3d$x,poly.3d$y,col=l.col,border=l.lcol)
  l.slope<-atan((poly.3d$y[4]-poly.3d$y[3])/(poly.3d$x[4]-poly.3d$x[3]))*180/pi
  text(weighted.mean(poly.3d$x[3:4],(c(1,3)/4)),
       weighted.mean(poly.3d$y[3:4],(c(1,3)/4))
       ,labels=l.lab[i],srt=l.slope,pos=3,offset=1,cex=lab.cex)
  
  # Within-layer edges
  # Find edges where both nodes are in this layer
  el.sub<-which((as.numeric(substr(el.net[,1],2,nchar(el.net[,1])))%in%layers[[i]])&
                  (as.numeric(substr(el.net[,2],2,nchar(el.net[,2])))%in%layers[[i]]))
  
  el.sub1<-merge(data.frame(cid=el.net[el.sub,1]),data.frame(cid=net%v%"caseid",id=1:network.size(net)))[,2]
  el.sub2<-merge(data.frame(cid=el.net[el.sub,2]),data.frame(cid=net%v%"caseid",id=1:network.size(net)))[,2]
  
  segments(x0=pts.3d[el.sub1,1],y0=pts.3d[el.sub1,2],
           x1=pts.3d[el.sub2,1],y1=pts.3d[el.sub2,2],
           lwd=e.cex,
           col=e.col)
  
  # Nodes
  pts<-which(net%v%"court"==i)
  points(x=pts.3d[pts,1],y=pts.3d[pts,2],cex=pts.cex[pts]*v.cex,
         pch=21,col=v.lcol,bg=v.col[pts],lwd=v.lwd)
  
  # Inter-layer Edges
  if(i==l){break}else{
    for(j in l:(i+1)){
      el.sub<-which((as.numeric(substr(el.net[,1],2,nchar(el.net[,1])))%in%layers[[j]])&
                      (as.numeric(substr(el.net[,2],2,nchar(el.net[,2])))%in%layers[[i]]))
      el.sub1<-merge(data.frame(cid=el.net[el.sub,1]),data.frame(cid=net%v%"caseid",id=1:network.size(net)))[,2]
      el.sub2<-merge(data.frame(cid=el.net[el.sub,2]),data.frame(cid=net%v%"caseid",id=1:network.size(net)))[,2]
      
      segments(x0=pts.3d[el.sub1,1],y0=pts.3d[el.sub1,2],
               x1=pts.3d[el.sub2,1],y1=pts.3d[el.sub2,2],
               lwd=e.cex,
               col=e.col)
    }
  }
}
dev.off()

###

# 1937-2002, by year
set.seed(7507224)
net.layout<-network.layout.fruchtermanreingold(net,list(repulse.rad=network.size(net)^2*log(network.size(net))/5))

# Need to play around with these values to get a good looking plot.
adj.layout<-apply(net.layout,2,function(x){x<-x-mean(x);x<-400*x/max(abs(x));return(x)})

lsize<-410
vsize<-40
adj.curv<-rep(0,66)
plot.grid<-persp(seq(-lsize,lsize,lsize),seq(-lsize,lsize,lsize),matrix(lsize*6,ncol=3,nrow=3),zlim=c(lsize,lsize*6),border=rgb(0,0,0,1),box=F,axes=F,
                 phi=40,theta=-30,d=1.2,r=4,scale=F)
for(i in 1:66){
  poly.3d<-trans3d(x=c(-lsize,-lsize,lsize,lsize)-adj.curv[i],y=c(-lsize,lsize,lsize,-lsize),z=(i*vsize),plot.grid)
  polygon(poly.3d$x,poly.3d$y,col=rgb(0.8,0.5,0.9,0.2),border=NA)
}

pts.3d<-trans3d(adj.layout[,1],
                adj.layout[,2],
                ((net%v%"year")-1936)*vsize,
                plot.grid)
pts.3d<-matrix(unlist(pts.3d),ncol=2,byrow=F)

layers<-lapply(1937:2002,function(x){which((net%v%"year")%in%x)+as.numeric(substr(first1937,2,nchar(first1937)))-1})

##
pdf("citations2.pdf",height=20,width=10)
par(mar=c(0,0,0,0))
pts.cex<-0.01*(net%v%"indeg"+20)
v.cex<-5
v.col<-adjustcolor(ifelse(net%v%"salience",yell.c,pink.c),alpha.f=0.8)
v.lwd<-0.1
v.lcol<-rgb(0,0,0,0.1)
e.cex<-0.1
e.col<-rgb(0,0,0,0.05)
l.col<-adjustcolor(blue.c,alpha.f=0.02)
l.lcol<-rgb(0,0,0,0.05)
l<-66
l.lab<-seq(1937,2002)
lab.cex<-1.5
plot.new()
plot.window(xlim=c(-0.13,0.12),ylim=c(-.25,.25))

# First layer
for(i in 1:l){
  poly.3d<-trans3d(x=c(-lsize,-lsize,lsize,lsize)-adj.curv[i],y=c(-lsize,lsize,lsize,-lsize),z=(i*vsize),plot.grid)
  polygon(poly.3d$x,poly.3d$y,col=l.col,border=l.lcol)

  # Within-layer edges
  # Find edges where both nodes are in this layer
  el.sub<-which((as.numeric(substr(el.net[,1],2,nchar(el.net[,1])))%in%layers[[i]])&
                  (as.numeric(substr(el.net[,2],2,nchar(el.net[,2])))%in%layers[[i]]))
  
  el.sub1<-merge(data.frame(cid=el.net[el.sub,1]),data.frame(cid=net%v%"caseid",id=1:network.size(net)))[,2]
  el.sub2<-merge(data.frame(cid=el.net[el.sub,2]),data.frame(cid=net%v%"caseid",id=1:network.size(net)))[,2]
  
  segments(x0=pts.3d[el.sub1,1],y0=pts.3d[el.sub1,2],
           x1=pts.3d[el.sub2,1],y1=pts.3d[el.sub2,2],
           lwd=e.cex,
           col=e.col)
  
  # Nodes
  pts<-which(net%v%"year"==i+1936)
  points(x=pts.3d[pts,1],y=pts.3d[pts,2],cex=pts.cex[pts]*v.cex,
         pch=21,col=v.lcol,bg=v.col[pts],lwd=v.lwd)
  
  # Inter-layer Edges
  if(i==l){break}else{
    for(j in l:(i+1)){
      el.sub<-which((as.numeric(substr(el.net[,1],2,nchar(el.net[,1])))%in%layers[[j]])&
                      (as.numeric(substr(el.net[,2],2,nchar(el.net[,2])))%in%layers[[i]]))
      el.sub1<-merge(data.frame(cid=el.net[el.sub,1]),data.frame(cid=net%v%"caseid",id=1:network.size(net)))[,2]
      el.sub2<-merge(data.frame(cid=el.net[el.sub,2]),data.frame(cid=net%v%"caseid",id=1:network.size(net)))[,2]
      
      segments(x0=pts.3d[el.sub1,1],y0=pts.3d[el.sub1,2],
               x1=pts.3d[el.sub2,1],y1=pts.3d[el.sub2,2],
               lwd=e.cex,
               col=e.col)
    }
  }
}
dev.off()

###

# 1998-2002, by year
set.seed(507411)
net.layout<-network.layout.fruchtermanreingold(five,list(repulse.rad=network.size(five)^2*log(network.size(five))/5))

# Manual adjustment
adj.layout<-apply(net.layout,2,function(x){x<-x-mean(x);x<-395*x/max(abs(x));return(x)})

lsize<-410
vsize<-400
adj.curv<-c(500,150,0,200,600)

plot.grid<-persp(seq(-lsize,lsize,lsize),seq(-lsize,lsize,lsize),matrix(lsize*5,ncol=3,nrow=3),zlim=c(lsize,lsize*5),border=rgb(0,0,0,1),box=F,axes=F,
                 phi=30,theta=-30,d=0.9,r=2,scale=F)
for(i in 1:5){
  poly.3d<-trans3d(x=c(-lsize,-lsize,lsize,lsize)-adj.curv[i],y=c(-lsize,lsize,lsize,-lsize),z=(i*vsize),plot.grid)
  polygon(poly.3d$x,poly.3d$y,col=rgb(0.8,0.5,0.9,0.2),border=NA)
}

node.adj<-as.numeric(as.character(cut(five%v%"year",breaks=seq(1984,1989)+0.5,labels=adj.curv)))
adj.layout[,1]<-adj.layout[,1]-node.adj
pts.3d<-trans3d(adj.layout[,1],
                adj.layout[,2],
                ((five%v%"year")-1984)*vsize,
                plot.grid)
pts.3d<-matrix(unlist(pts.3d),ncol=2,byrow=F)

layers<-lapply(1985:1989,function(x){which((five%v%"year")%in%x)+as.numeric(substr(first1985,2,nchar(first1985)))-1})


indeg<-merge(data.frame(caseid=five%v%"caseid"),data.frame(caseid=names(table(el.five[,2])),indeg=c(table(el.five[,2]))),all.x=T)
indeg<-indeg[sort(indeg$caseid),]
indeg$indeg[is.na(indeg$indeg)]<-0

##
pdf("citations3.pdf",height=10,width=8)
par(mar=c(0,0,0,0))
pts.cex<-0.01*(indeg$indeg+4)
v.cex<-20
v.col<-adjustcolor(ifelse(five%v%"salience",yell.c,pink.c),alpha.f=0.9)
v.lwd<-0.1
v.lcol<-rgb(0,0,0,0.4)
e.cex<-0.2
e.col<-rgb(0,0,0,0.05)
l.col<-adjustcolor(blue.c,alpha.f=0.2)
l.lcol<-rgb(0,0,0,0.6)
l<-5
l.lab<-seq(1985,1989)
lab.cex<-1.5
plot.new()
plot.window(xlim=c(-0.365,0.155),ylim=c(-0.39,0.26))

# First layer
for(i in 1:l){
  poly.3d<-trans3d(x=c(-lsize,-lsize,lsize,lsize)-adj.curv[i],y=c(-lsize,lsize,lsize,-lsize),z=(i*vsize),plot.grid)
  polygon(poly.3d$x,poly.3d$y,col=l.col,border=l.lcol)
  l.slope<-atan((poly.3d$y[4]-poly.3d$y[3])/(poly.3d$x[4]-poly.3d$x[3]))*180/pi
  text(weighted.mean(poly.3d$x[3:4],(c(1,3)/4)),
       weighted.mean(poly.3d$y[3:4],(c(1,3)/4))
       ,labels=l.lab[i],srt=l.slope,pos=3,offset=0.25,cex=lab.cex)
  
  # Within-layer edges
  # Find edges where both nodes are in this layer
  el.sub<-which((as.numeric(substr(el.five[,1],2,nchar(el.five[,1])))%in%layers[[i]])&
                  (as.numeric(substr(el.five[,2],2,nchar(el.five[,2])))%in%layers[[i]]))
  el.sub1<-merge(data.frame(cid=el.five[el.sub,1]),data.frame(cid=five%v%"caseid",id=1:network.size(five)))[,2]
  el.sub2<-merge(data.frame(cid=el.five[el.sub,2]),data.frame(cid=five%v%"caseid",id=1:network.size(five)))[,2]
  
  segments(x0=pts.3d[el.sub1,1],y0=pts.3d[el.sub1,2],
           x1=pts.3d[el.sub2,1],y1=pts.3d[el.sub2,2],
           lwd=e.cex,
           col=e.col)

  # Nodes
  pts<-which(five%v%"year"==i+1984)
  points(x=pts.3d[pts,1],y=pts.3d[pts,2],cex=pts.cex[pts]*v.cex,
         pch=21,col=v.lcol,bg=v.col[pts],lwd=v.lwd)
  
  # Inter-layer Edges
  if(i==l){break}else{
    for(j in l:(i+1)){
      el.sub<-which((as.numeric(substr(el.five[,1],2,nchar(el.five[,1])))%in%layers[[j]])&
                      (as.numeric(substr(el.five[,2],2,nchar(el.five[,2])))%in%layers[[i]]))
      el.sub1<-merge(data.frame(cid=el.five[el.sub,1]),data.frame(cid=five%v%"caseid",id=1:network.size(five)))[,2]
      el.sub2<-merge(data.frame(cid=el.five[el.sub,2]),data.frame(cid=five%v%"caseid",id=1:network.size(five)))[,2]
      
      segments(x0=pts.3d[el.sub1,1],y0=pts.3d[el.sub1,2],
               x1=pts.3d[el.sub2,1],y1=pts.3d[el.sub2,2],
               lwd=e.cex,
               col=e.col)
    }
  }
}
dev.off()

# Same network, different plot.
adj.layout<-apply(net.layout,2,function(x){x<-x-mean(x);x<-395*x/max(abs(x));return(x)})

pdf("citations4.pdf",height=10,width=10)
par(mar=c(0,0,0,0))
plot.new()
plot.window(xlim=c(-lsize,lsize),ylim=c(-lsize,lsize))
el.sub1<-which((five%v%"caseid")%in%el.five[,1])
el.sub2<-which((five%v%"caseid")%in%el.five[,2])
segments(x0=adj.layout[el.sub1,1],y0=adj.layout[el.sub1,2],
         x1=adj.layout[el.sub2,1],y1=adj.layout[el.sub2,2],
         lwd=0.2,
         col=rgb(0,0,0,0.2))
points(adj.layout[,1],adj.layout[,2],cex=pts.cex*30,
       pch=21,bg=adjustcolor(v.col,alpha.f=0.9))
dev.off()
###

## local subgraphs
pink.c<-rgb(1,0.42,0.71)
blue.c<-rgb(0.42,0.71,1)
yell.c<-rgb(1,1,0.42)
par(mar=c(0,0,0,0))

plot.local<-function(cases,seed,mode="both",label.pos=1){
  cid<-node_attributes$caseid[which(node_attributes$usid%in%uscases)]
  if(mode=="in"){
    tel<-edgelist[which(edgelist[,2]%in%cid),]
  }
  if(mode=="out"){
    tel<-edgelist[which(edgelist[,1]%in%cid),]
  }
  if(mode=="both"){
    tel<-edgelist[which(edgelist[,1]%in%cid|edgelist[,2]%in%cid),]
  }
  temp.net<-network(tel)
  
  e.col<-rep(0,nrow(tel))
  e.col[which(tel[,1]%in%cid&tel[,2]%in%cid)]<-1
  
  set.seed(seed)
  par(mar=c(0,0,0,0))
  plot(temp.net,
       vertex.col=ifelse((temp.net%v%"vertex.names")%in%cid,yell.c,adjustcolor(blue.c,alpha.f=0.4)),
       vertex.border=ifelse((temp.net%v%"vertex.names")%in%cid,rgb(0,0,0),adjustcolor(blue.c,alpha.f=0.4)),
       vertex.cex=ifelse((temp.net%v%"vertex.names")%in%cid,2,1),
       edge.col=ifelse(e.col,adjustcolor(pink.c,alpha.f=0.9),rgb(0,0,0,0.1)),
       label=sub("US"," U.S. ",node_attributes$names[match(temp.net%v%"vertex.names",node_attributes$caseid)]),
       label.border=rgb(0,0,0,0),label.bg=ifelse((temp.net%v%"vertex.names")%in%cid,rgb(1,1,1,0.5),rgb(0,0,0,0)),boxed.labels=T,label.pad=1,label.cex=1.2,
       displaylabels=T,label.col=ifelse((temp.net%v%"vertex.names")%in%cid,rgb(0,0,0),rgb(0,0,0,0)),label.pos=label.pos,
       arrowhead.cex=ifelse(e.col,1.5,1))
}

#transitivity
uscases<-c("517US44","491US1","377US184")
labs<-c("Seminole Tribe of Fla. v. Florida","Pennsylvania v. Union Gas Co.","Parden v. Terminal R. Co.")
node_attributes$names<-NA
for(i in 1:3){
  node_attributes$names[which(node_attributes$usid==uscases[i])]<-labs[i]
}

pdf("citations.trans.pdf",height=10,width=10)
plot.local(uscases,14156819,label.pos=3)
dev.off()

#reciprocity
uscases<-c("472US400","472US353")
labs<-c("Western Air Lines v. Criswell","Johnson v. Mayor of Baltimore")
for(i in 1:2){
  node_attributes$names[which(node_attributes$usid==uscases[i])]<-labs[i]
}

pdf("citations.recip.pdf",height=10,width=10)
plot.local(uscases,1206627,label.pos=2)
dev.off()

#popularity
uscases<-node_attributes$usid[which(substr(node_attributes$usid,1,3)%in%c("369"))]
node_attributes$names[which(node_attributes$usid=="369US186")]<-"Baker v. Carr"
cid<-node_attributes$caseid[which(node_attributes$usid%in%uscases)]
tel<-edgelist[which(edgelist[,2]%in%cid),]
temp.net<-network(tel)

set.seed(905826)
pdf("citations.pop.pdf",height=10,width=10)
par(mar=c(0,0,0,0))
plot(temp.net,
     vertex.col=ifelse(temp.net%v%"vertex.names"==node_attributes$caseid[which(node_attributes$usid=="369US186")],adjustcolor(pink.c,alpha=0.8),
                       ifelse((temp.net%v%"vertex.names")%in%cid,yell.c,adjustcolor(blue.c,alpha.f=0.4))),
     vertex.border=ifelse((temp.net%v%"vertex.names")%in%cid,rgb(0,0,0),adjustcolor(blue.c,alpha.f=0.4)),
     vertex.cex=0.02*degree(temp.net,cmode="indegree")+0.5,
     edge.col=rgb(0,0,0,0.1),
     label=sub("US"," U.S. ",node_attributes$names[match(temp.net%v%"vertex.names",node_attributes$caseid)]),
     label.border=rgb(0,0,0,0),label.bg=ifelse(temp.net%v%"vertex.names"==node_attributes$caseid[which(node_attributes$usid=="369US186")]
                                               ,rgb(1,1,1,0.8),rgb(0,0,0,0)),boxed.labels=T,label.pad=1,label.cex=1.2,
     displaylabels=T,label.col=ifelse(temp.net%v%"vertex.names"==node_attributes$caseid[which(node_attributes$usid=="369US186")],rgb(0,0,0),rgb(0,0,0,0)),
     arrowhead.cex=0.5)
dev.off()

#sociality
uscases<-node_attributes$usid[which(substr(node_attributes$usid,1,3)%in%c("466"))]
node_attributes$names[which(node_attributes$usid=="466US789")]<-"City Council v. Taxpayers for Vincent"
cid<-node_attributes$caseid[which(node_attributes$usid%in%uscases)]
tel<-edgelist[which(edgelist[,1]%in%cid),]
temp.net<-network(tel)

set.seed(905323)
pdf("citations.soci.pdf",height=10,width=10)
par(mar=c(0,0,0,0))
plot(temp.net,
     vertex.col=ifelse(temp.net%v%"vertex.names"==node_attributes$caseid[which(node_attributes$usid=="466US789")],adjustcolor(pink.c,alpha=0.8),
                       ifelse((temp.net%v%"vertex.names")%in%cid,yell.c,adjustcolor(blue.c,alpha.f=0.4))),
     vertex.border=ifelse((temp.net%v%"vertex.names")%in%cid,rgb(0,0,0),adjustcolor(blue.c,alpha.f=0.4)),
     vertex.cex=0.02*degree(temp.net,cmode="outdegree")+0.5,
     edge.col=rgb(0,0,0,0.1),
     label=sub("US"," U.S. ",node_attributes$names[match(temp.net%v%"vertex.names",node_attributes$caseid)]),
     label.border=rgb(0,0,0,0),label.bg=ifelse(temp.net%v%"vertex.names"==node_attributes$caseid[which(node_attributes$usid=="466US789")]
                                               ,rgb(1,1,1,0.8),rgb(0,0,0,0)),boxed.labels=T,label.pad=1,label.cex=1.2,
     displaylabels=T,label.col=ifelse(temp.net%v%"vertex.names"==node_attributes$caseid[which(node_attributes$usid=="466US789")],rgb(0,0,0),rgb(0,0,0,0)),
     arrowhead.cex=0.5)
dev.off()


uscases<-node_attributes$usid[which(substr(node_attributes$usid,1,3)%in%c("460"))]
node_attributes$names[which(node_attributes$usid=="460US719")]<-"Kush v. Rutledge"
cid<-node_attributes$caseid[which(node_attributes$usid%in%uscases)]
tel<-edgelist[which(edgelist[,1]%in%cid),]
temp.net<-network(tel)

set.seed(160675)
pdf("citations.soci2.pdf",height=10,width=10)
par(mar=c(0,0,0,0))
plot(temp.net,
     vertex.col=ifelse(temp.net%v%"vertex.names"==node_attributes$caseid[which(node_attributes$usid=="460US719")],adjustcolor(pink.c,alpha=0.8),
                       ifelse((temp.net%v%"vertex.names")%in%cid,yell.c,adjustcolor(blue.c,alpha.f=0.4))),
     vertex.border=ifelse((temp.net%v%"vertex.names")%in%cid,rgb(0,0,0),adjustcolor(blue.c,alpha.f=0.4)),
     vertex.cex=0.02*degree(temp.net,cmode="outdegree")+0.5,
     edge.col=rgb(0,0,0,0.1),
     label=sub("US"," U.S. ",node_attributes$names[match(temp.net%v%"vertex.names",node_attributes$caseid)]),
     label.border=rgb(0,0,0,0),label.bg=ifelse(temp.net%v%"vertex.names"==node_attributes$caseid[which(node_attributes$usid=="460US719")]
                                               ,rgb(1,1,1,0.8),rgb(0,0,0,0)),boxed.labels=T,label.pad=1,label.cex=1.2,
     displaylabels=T,label.col=ifelse(temp.net%v%"vertex.names"==node_attributes$caseid[which(node_attributes$usid=="460US719")],rgb(0,0,0),rgb(0,0,0,0)),
     arrowhead.cex=0.5)
dev.off()



