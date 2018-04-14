#----Code to produce citation network plot----#
# The 'network' package is required for the force-directed algorithm on line 57

#----Preparation----#
# Set Directory
rm(list=ls())
setwd("C:/Users/TC/Dropbox/28_Citation Networks/R")

# Loading Data
load("SCOTUS.RData")
rm(mq.matrix,Overruled.matrix,same.issue.area,SCDB_justice,year.diff.matrix)

# Defining Colour
blue.c<-rgb(0.42,0.71,1)

# Preparing Network
# Remove Isolates (there aren't many (348), and it helps with the rest of the visualization)
isolates<-which(rowSums(adjacency.matrix)+colSums(adjacency.matrix)==0)
mat<-adjacency.matrix[-isolates,-isolates]
rm(adjacency.matrix)

# Defining layers
justices<-unique(scc1$chief[-isolates])
n.layer<-length(justices)
justice.id<-as.numeric(factor(scc1$chief[-isolates],levels=justices))
rm(scc1)

# Partitioning matrix into multilayer blocks
mat.block<-mat
for(i in 1:n.layer){
  #id the intralayer edges
  mat.block[which(justice.id==i),which(justice.id==i)]<-mat[which(justice.id==i),which(justice.id==i)]*i
  for(j in 1:n.layer){
    #id the interlayer edges
    mat.block[which(justice.id!=i),which(justice.id==i)]<-mat[which(justice.id!=i),which(justice.id==i)]*(i+0.5)
  }
}

# Manual adjustments for xy to xyz coordinate system
lsize<-410 # Size of layer
vsize<-450 # Vertical height between layers
adj.curv<-c(500,0,-500,-750,-500,-100,400) # Horizontal shifting of layers
plot.grid<-persp(seq(-lsize,lsize,lsize),seq(-lsize,lsize,lsize),matrix(lsize*2,ncol=3,nrow=3),zlim=c(vsize,vsize*n.layer),
                 border=rgb(0,0,0,1),box=F,axes=F,scale=F,
                 # Values below control viewing angle (phi and theta), perspective (d), and distance (r). See ?persp for details.
                 phi=30,
                 theta=20,
                 d=1.5,
                 r=2)
for(i in 1:n.layer){ # Visualizing the placement of layers
  poly.3d<-trans3d(x=c(-lsize,-lsize,lsize,lsize)-adj.curv[i],y=c(-lsize,lsize,lsize,-lsize),z=(i*vsize),plot.grid)
  polygon(poly.3d$x,poly.3d$y,col=rgb(0.8,0.5,0.9,0.2),border=NA)
}

# Force-directed Algorithm for Node Placement
set.seed(1903614)
net.layout<-network::network.layout.fruchtermanreingold(network::network(mat,directed=T),NULL)
for(i in 1:n.layer){ # Spreading nodes within layers
  net.layout[which(justice.id==i),]<-apply(net.layout[which(justice.id==i),],2,
                                           function(x){x<-x-mean(x);x<-(0.9*lsize)*x/max(abs(x));return(x)})
}
plot(net.layout) # Checking placements

# Determining node placements
# Matching nodes to horizontal shifting of layers
node.adj<-factor(justice.id)
levels(node.adj)<-adj.curv
net.layout[,1]<-net.layout[,1]-as.numeric(as.character(node.adj))

# Center of the nodes (for drawing edges)
pts.3d<-trans3d(net.layout[,1],
                net.layout[,2],
                justice.id*vsize,
                plot.grid)
pts.3d<-matrix(unlist(pts.3d),ncol=2,byrow=F)
plot(pts.3d) # Checking placement

# Node size
v.cex<-colSums(mat)/20+1

# Transforming nodes from circles or ellipses when plotting
node.att.plot<-cbind(net.layout,v.cex,justice.id)

# Function to draw ellipse instead of using points()
to.ellipse<-function(x){
  temp.pos<-plotrix::draw.circle(x[1],x[2],radius=x[3]*10,nv=100)
  temp.pos$z<-x[4]
  return(temp.pos)
}
ellipse.layout<-apply(node.att.plot,1,to.ellipse)

# Ellipse to proper xyz
ellipse.3d<-lapply(ellipse.layout,function(x){trans3d(x$x,x$y,x$z*vsize,plot.grid)})


# Plotting parameters
v.col<-adjustcolor(blue.c,alpha.f=0.5) #Vertex colour
v.lwd<-0.01 # Vertex border weight
v.lcol<-rgb(0.3,0.3,0.3,0.5) # Vertex border colour
e.cex<-0.1 # Edge weight
e.col<-rgb(0,0,0,0.008) # Edge colour
l.col<-grey(1,alpha=0.6) # Layer colour
l.lcol<-grey(0.7,alpha=1) # Layer border colour
l<-n.layer # First l layers to plot
l.lab<-justices # Layer labels
lab.cex<-1.2 # Layer label size
apply(pts.3d,2,range) # Checking x and y lim of plot

#----Plotting----#
#pdf("courts.layer.pdf",height=10,width=8)
#png("courts.layer.png",height=10,width=8,units="in",res=300)
par(mar=c(0,0,0,0))
plot.new()
plot.window(xlim=c(-0.22,0.26),ylim=c(-0.265,0.335))

for(i in 1:l){
  poly.3d<-trans3d(x=c(-lsize,-lsize,lsize,lsize)-adj.curv[i],y=c(-lsize,lsize,lsize,-lsize),z=(i*vsize),plot.grid)
  polygon(poly.3d$x,poly.3d$y,col=l.col,border=l.lcol)
  l.slope<-atan((poly.3d$y[4]-poly.3d$y[3])/(poly.3d$x[4]-poly.3d$x[3]))*180/pi
  text(weighted.mean(poly.3d$x[3:4],(c(2,3)/5)),
       weighted.mean(poly.3d$y[3:4],(c(2,3)/5))
       ,labels=l.lab[i],srt=l.slope,pos=1,offset=0.6,cex=lab.cex)

  # Within-layer edges
  # Find edges where both nodes are in this layer
  el.sub<-unique(t(apply(which(mat.block==(1:l)[i],arr.ind=T),1,sort)))
  
  segments(x0=pts.3d[el.sub[,1],1],y0=pts.3d[el.sub[,1],2],
           x1=pts.3d[el.sub[,2],1],y1=pts.3d[el.sub[,2],2],
           lwd=e.cex,
           col=e.col)
  
  # Nodes
  pts<-which(justice.id==i)
  sapply(pts,function(x){polygon(ellipse.3d[[x]],col=v.col,border=v.lcol)})
  
  # Inter-layer Edges
  if(i==l){break}else{
    el.sub<-unique(t(apply(which(mat.block==((1:l)[i]+0.5),arr.ind=T),1,sort)))
    
    segments(x0=pts.3d[el.sub[,1],1],y0=pts.3d[el.sub[,1],2],
             x1=pts.3d[el.sub[,2],1],y1=pts.3d[el.sub[,2],2],
             lwd=e.cex,
             col=e.col)
  }
}
#dev.off()
