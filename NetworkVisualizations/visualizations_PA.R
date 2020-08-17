#----Code to produce citation network plot----#
# The 'network' package is required for the force-directed algorithm on line 57

#----Preparation----#
# Set Directory
rm(list=ls())
setwd("")

# Loading Data
#load("SCOTUS.RData")
#rm(mq.matrix,Overruled.matrix,same.issue.area,SCDB_justice,year.diff.matrix, cluster.mat, MQ.scores, overruled, overruled_cases_supremecourt, same.opinion.writer, scc)

# Defining Colour
orng.c<-"#fcae91FF"
purp.c<-"#7828a0FF"
gren.c<-"#46aa96FF"
lgry.c<-"#dcdcdcFF"
mgry.c<-"#8c8c8cFF"
dgry.c<-"#333333FF"

mkrzywinski = c('#006E82FF', 
                '#8214A0FF',
                '#005AC8FF',
                '#00A0FAFF', 
                '#FA78FAFF',
                '#14D2DCFF',
                '#AA0A3CFF',
                '#FA7850FF',
                '#0AB45AFF',
                '#F0F032FF',
                '#A0FA82FF',
                '#FAE6BEFF')

# Preparing Network
# Remove error citations
adjacency.matrix[8822, 9409] <- 0
adjacency.matrix[8822, 9540] <- 0

# Remove Isolates (there aren't many (352), and it helps with the rest of the visualization)
isolates <- which((rowSums(adjacency.matrix) + colSums(adjacency.matrix)) == 0)
mat <- adjacency.matrix[-isolates, -isolates]
rm(adjacency.matrix)

# Defining layers
justices <- unique(scc1$chief[-isolates])
n.layer <- length(justices)
justice.id <- as.numeric(factor(scc1$chief[-isolates], levels = justices))
rm(scc1)

# Partitioning matrix into multilayer blocks
mat.block <- mat
for(i in 1:n.layer){
  #id the intralayer edges
  mat.block[which(justice.id == i), which(justice.id == i)] <- mat[which(justice.id == i), which(justice.id == i)] * i
  
  if(i == n.layer){break} else {
    for(j in (i + 1):n.layer){
      #id the interlayer edges
      mat.block[which(justice.id == j), which(justice.id == i)] <- mat[which(justice.id == j), which(justice.id == i)] * (i + j/10)
    }
  }
}

# Manual adjustments for xy to xyz coordinate system
lsize <- 410 # Size of layer
vsize <- 300 # Vertical height between layers
adj.curv <- c(500,0,-500,-750,-500,-100,400) # Horizontal shifting of layers
#adj.curv <- rep(0, 7)
#adj.curv <- c(-1800, -1200, -600, 0, 600, 1200, 1800)
plot.grid <- persp(seq(-lsize, lsize, lsize), seq(-lsize, lsize, lsize), matrix(lsize * 2, ncol = 3, nrow = 3), zlim = c(vsize, vsize * n.layer),
                   border = rgb(0, 0, 0, 1), box = F, axes = F, scale = F,
                   # Values below control viewing angle (phi and theta), perspective (d), and distance (r). See ?persp for details.
                   phi = 15,
                   theta = 35,
                   d = 10,
                   r = 200)
for(i in 1:n.layer){# Visualizing the placement of layers
  poly.3d<-trans3d(x = c(-lsize,-lsize,lsize,lsize) - adj.curv[i],
                   y = c(-lsize,lsize,lsize,-lsize),
                   z = (i*vsize),
                   plot.grid)
  polygon(poly.3d$x,poly.3d$y,col=rgb(0.8,0.5,0.9,0.2),border=NA)
}

# Force-directed Algorithm for Node Placement
set.seed(566894)
# net.layout <- matrix(NA, ncol = 2, nrow = nrow(mat))
# for(j in 1:n.layer){
#   tmat <- mat[which(justice.id == j), which(justice.id == j)]
#   net.layout[which(justice.id == j),] <- network::network.layout.fruchtermanreingold(network::network(tmat, directed = T), NULL)
#   net.layout[which(justice.id == j),] <- apply(net.layout[which(justice.id == j),],2,
#                                                function(x){x<-x-mean(x);x<-(0.9*lsize)*x/max(abs(x));return(x)})
# }


net.layout<-network::network.layout.fruchtermanreingold(network::network(mat,directed=T),NULL)
for(i in 1:n.layer){ # Spreading nodes within layers
  net.layout[which(justice.id==i),]<-apply(net.layout[which(justice.id==i),],2,
                                           function(x){x<-x-mean(x);x<-(0.9*lsize)*x/max(abs(x));return(x)})
}
plot(net.layout) # Checking placements

# Determining node placements
# Matching nodes to horizontal shifting of layers
node.adj <- factor(justice.id)
levels(node.adj) <- adj.curv
net.layout[, 1] <- net.layout[, 1] - as.numeric(as.character(node.adj))

# Center of the nodes (for drawing edges)
pts.3d<-trans3d(net.layout[,1],
                net.layout[,2],
                justice.id*vsize,
                plot.grid)
pts.3d<-matrix(unlist(pts.3d),ncol=2,byrow=F)
plot(pts.3d) # Checking placement

# Node size
v.cex<-colSums(mat)/200+0.3
plot(density(v.cex),main=range(v.cex))

# Transforming nodes from circles or ellipses when plotting
node.att.plot<-cbind(net.layout,v.cex,justice.id)

# Function to draw ellipse instead of using points()
to.ellipse<-function(x){
  plot.window(xlim=c(0,1),ylim=c(0,1))
  temp.pos<-plotrix::draw.circle(x[1],x[2],radius=x[3]*10,nv=100)
  temp.pos$z<-x[4]
  return(temp.pos)
}
ellipse.layout<-apply(node.att.plot,1,to.ellipse)

# Ellipse to proper xyz
ellipse.3d<-lapply(ellipse.layout,function(x){trans3d(x$x,x$y,x$z*vsize,plot.grid)})

# gradient edge functions
seg_gradient <- function(x0 , y0, x1, y1, start_col, end_col, n_cols = 3, col_alpha = 1, ...){
  ramp <- colorRampPalette(c(start_col, end_col))
  x_pts <- seq(x0, x1, length.out = n_cols)
  y_pts <- seq(y0, y1, length.out = n_cols)
  segments(x0 = head(x_pts, -1), y0 = head(y_pts, -1), x1 = tail(x_pts, -1), y1 = tail(y_pts, -1), col = adjustcolor(ramp(n_cols), alpha.f = col_alpha), ...)
}

# Plotting parameters
v.col<-adjustcolor(orng.c,alpha.f=0.5) #Vertex colour
v.col <- mkrzywinski[c(1, 8, 9, 5, 3, 7, 2)]
v.lwd<-0.002 # Vertex border weight
v.lcol<-rgb(0.1,0.1,0.1,0.5) # Vertex border colour
e.cex<-0.001 # Edge weight
e.col<-rgb(0.2,0.2,0.2,0.01) # Edge colour
l.col<-grey(.99, alpha = 0.9) # Layer colour
l.lcol<-grey(0.7,alpha = 1) # Layer border colour
l <- n.layer # First l layers to plot
l.lab<-justices # Layer labels
lab.cex<-10 # Layer label size
apply(pts.3d,2,range) # Checking x and y lim of plot

h.id<-order(v.cex,decreasing=T)[1:10] # Highlighting certain nodes (here 10 most cited)
h.col<-adjustcolor(purp.c,alpha.f=0.5)
h.lcol<-"black"

#----Plotting----#
pdf("courts_layer.pdf",height=30,width=30, pointsize = 6)
#png("courts_layer_updated.png",height=20,width=20,units="in",res=300)
par(mar=c(0,0,0,0))
plot.new()
plot.window(xlim=c(-0.05,0.06),ylim=c(-0.055,0.055))

for(i in 1:l){
  poly.3d<-trans3d(x=c(-lsize,-lsize,lsize,lsize)-adj.curv[i],y=c(-lsize,lsize,lsize,-lsize),z=(i*vsize),plot.grid)
  polygon(poly.3d$x,poly.3d$y,col=l.col,border=l.lcol)
  l.slope<-atan((poly.3d$y[4]-poly.3d$y[3])/(poly.3d$x[4]-poly.3d$x[3]))*180/pi
  text(weighted.mean(poly.3d$x[3:4],(c(3.5,1.5)/5)),
       weighted.mean(poly.3d$y[3:4],(c(3.5,1.5)/5))
       ,labels=l.lab[i],srt=l.slope,pos=1,offset=2,cex=lab.cex)

  # Within-layer edges
  # Find edges where both nodes are in this layer
  el.sub<-unique(t(apply(which(mat.block==(1:l)[i],arr.ind=T),1,sort)))
  
  segments(x0=pts.3d[el.sub[,1],1],y0=pts.3d[el.sub[,1],2],
           x1=pts.3d[el.sub[,2],1],y1=pts.3d[el.sub[,2],2],
           lwd=e.cex,
           col=adjustcolor(v.col[i], alpha.f = 0.1))
  
  # Nodes
  pts<-which(justice.id==i)
  sapply(pts,function(x){polygon(ellipse.3d[[x]],col=v.col[i],border=v.col[i])})
  
  # Highlighted nodes
  # h.pts<-h.id[h.id%in%pts]
  # sapply(h.pts,function(x){polygon(ellipse.3d[[x]],col=h.col,border=v.lcol)})
  
  # Inter-layer Edges
  if(i == l){break} else {
    for(j in (i + 1):l){
      el.sub <- unique(t(apply(which(mat.block == (i + j/10), arr.ind = T), 1, sort)))
      for(k in 1:nrow(el.sub)){
        seg_gradient(x0 = pts.3d[el.sub[k,1],1], y0 = pts.3d[el.sub[k,1],2],
                     x1 = pts.3d[el.sub[k,2],1], y1 = pts.3d[el.sub[k,2],2],
                     start_col = v.col[i],
                     end_col = v.col[j],
                     col_alpha = 0.03, n_cols = 20,
                     lwd = e.cex)
      }
    }
  }
}
dev.off()

# Data Matrix Plot
ncases <- nrow(mat)
#pdf('data_mat.pdf', width = 5, height = 5)
#png('data_mat.png', width = 30, height = 30, units = 'in', res = 300)
plot.new()
par(mar = c(0,0,0,0))
plot.window(xlim = c(0.5, ncases+0.5), ylim = c(0.5, ncases+0.5), xaxs = "i", yaxs = "i")
axis(1)
axis(2)
box()


for(j in ncases:1){
  for(i in ncases:j){
    if(mat[i,j] == 1){
      polygon(y = c(i-0.5, i+0.5, i+0.5, i-0.5), x = c(j-0.5, j-0.5, j+0.5, j+0.5), col = colorRampPalette(c(v.col[justice.id[i]], v.col[justice.id[j]]))(3)[2], border = NA)
      #points(i, j, pch = 15, cex = 0.4, col = colorRampPalette(c(v.col[justice.id[i]], v.col[justice.id[j]]))(3)[2])
    }
  }
}

for(i in 1:7){
  linepos <- sum(table(justice.id)[1:i])
  abline(h = linepos, col = grey(0.3))
  abline(v = linepos, col = grey(0.3))
}

#dev.off()



