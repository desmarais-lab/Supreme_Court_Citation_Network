# Prep
library(statnet)
load("supreme.RData"); rm(model20.mle, scnetwork, edge_col, edge_row)
par(mar=c(0,0,0,0))

orng.c<-"#fcae91FF"
purp.c<-"#7828a0FF"
gren.c<-"#46aa96FF"
lgry.c<-"#dcdcdcFF"
mgry.c<-"#8c8c8cFF"
dgry.c<-"#333333FF"

# Plotting function
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
       vertex.col=ifelse((temp.net%v%"vertex.names")%in%cid,orng.c,adjustcolor(mgry.c,alpha.f=0.4)),
       vertex.border=ifelse((temp.net%v%"vertex.names")%in%cid,rgb(0,0,0),adjustcolor(mgry.c,alpha.f=0.4)),
       vertex.cex=ifelse((temp.net%v%"vertex.names")%in%cid,2,1),
       edge.col=ifelse(e.col,adjustcolor(purp.c,alpha.f=0.9),rgb(0,0,0,0.1)),
       label=sub("US"," U.S. ",node_attributes$names[match(temp.net%v%"vertex.names",node_attributes$caseid)]),
       label.border=rgb(0,0,0,0),label.bg=ifelse((temp.net%v%"vertex.names")%in%cid,rgb(1,1,1,0.5),rgb(0,0,0,0)),boxed.labels=T,label.pad=1,label.cex=1.2,
       displaylabels=T,label.col=ifelse((temp.net%v%"vertex.names")%in%cid,rgb(0,0,0),rgb(0,0,0,0)),label.pos=label.pos,
       arrowhead.cex=ifelse(e.col,1.5,1))
}

# transitivity (Fig 1)
uscases<-c("517US44","491US1","377US184")
labs<-c("Seminole Tribe of Fla. v. Florida","Pennsylvania v. Union Gas Co.","Parden v. Terminal R. Co.")
node_attributes$names<-NA
for(i in 1:3){
  node_attributes$names[which(node_attributes$usid==uscases[i])]<-labs[i]
}

pdf("citations.trans.pdf",height=10,width=10)
plot.local(uscases,14156819,label.pos=3)
dev.off()

# reciprocity (Fig 1)
uscases<-c("472US400","472US353")
labs<-c("Western Air Lines v. Criswell","Johnson v. Mayor of Baltimore")
for(i in 1:2){
  node_attributes$names[which(node_attributes$usid==uscases[i])]<-labs[i]
}

pdf("citations.recip.pdf",height=10,width=10)
plot.local(uscases,1206627,label.pos=2)
dev.off()

# popularity (Fig 2)
uscases<-node_attributes$usid[which(substr(node_attributes$usid,1,3)%in%c("369"))]
node_attributes$names[which(node_attributes$usid=="369US186")]<-"Baker v. Carr"
cid<-node_attributes$caseid[which(node_attributes$usid%in%uscases)]
tel<-edgelist[which(edgelist[,2]%in%cid),]
temp.net<-network(tel)

set.seed(905826)
pdf("citations.pop.pdf",height=10,width=10)
par(mar=c(0,0,0,0))
plot(temp.net,
     vertex.col=ifelse(temp.net%v%"vertex.names"==node_attributes$caseid[which(node_attributes$usid=="369US186")],adjustcolor(purp.c,alpha=0.9),
                       ifelse((temp.net%v%"vertex.names")%in%cid,orng.c,adjustcolor(mgry.c,alpha.f=0.4))),
     vertex.border=ifelse((temp.net%v%"vertex.names")%in%cid,rgb(0,0,0),adjustcolor(mgry.c,alpha.f=0.4)),
     vertex.cex=0.02*degree(temp.net,cmode="indegree")+0.5,
     edge.col=rgb(0,0,0,0.1),
     label=sub("US"," U.S. ",node_attributes$names[match(temp.net%v%"vertex.names",node_attributes$caseid)]),
     label.border=rgb(0,0,0,0),label.bg=ifelse(temp.net%v%"vertex.names"==node_attributes$caseid[which(node_attributes$usid=="369US186")]
                                               ,rgb(1,1,1,0.8),rgb(0,0,0,0)),boxed.labels=T,label.pad=1,label.cex=1.2,
     displaylabels=T,label.col=ifelse(temp.net%v%"vertex.names"==node_attributes$caseid[which(node_attributes$usid=="369US186")],rgb(0,0,0),rgb(0,0,0,0)),
     arrowhead.cex=0.5)
dev.off()