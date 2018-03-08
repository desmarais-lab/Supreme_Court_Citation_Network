
#### code to create polynomial plots

#####################################################
### degree 3 polynomials

# code assumes that working directory is set
# to top-level project
bootstrapResults <- read.csv("bootresults_3.csv",stringsAsFactors=F,row.names=1)

d<- c("edges","istar2","ostar2","mutual","triangle", "edgecov.mq.t", "edgecov.same.issue.area.t", "edgecov.year.diff.t" ,"edgecov.year.diff.square.t" ,  
      "nodeicov.AbsDiffMQscores", "nodeicov.NumberJusticesPro", "edgecov.year" , "nodeifactor.SameIssueArea.2", "nodeifactor.SameIssueArea.3",
      "nodeifactor.SameIssueArea.4","nodeifactor.SameIssueArea.5", "nodeifactor.SameIssueArea.6", "nodeifactor.SameIssueArea.7", 
      "nodeifactor.SameIssueArea.8","nodeifactor.SameIssueArea.9",  "nodeifactor.SameIssueArea.10",
      "nodeifactor.SameIssueArea.11", "nodeifactor.SameIssueArea.12", "nodeifactor.SameIssueArea.13","nodeifactor.SameIssueArea.14", 
      "nodeofactor.SameIssueArea.2", "nodeofactor.SameIssueArea.3", "nodeofactor.SameIssueArea.4","nodeofactor.SameIssueArea.5", 
      "nodeofactor.SameIssueArea.6","nodeofactor.SameIssueArea.7",
      "nodeofactor.SameIssueArea.8","nodeofactor.SameIssueArea.9","nodeofactor.SameIssueArea.10", "nodeofactor.SameIssueArea.11", 
      "nodeofactor.SameIssueArea.12", "nodeofactor.SameIssueArea.13","nodeofactor.SameIssueArea.14", "nodeicov.Overruled",
      "instar2_sendertime","outstar2_sendertime", "mutual_sendertime", "triangle_sendertime","mq_sendertime" ,"sameissuearea_sendertime",
      "yeardiff_sendertime" , "yeardiffsquare_sendertime", "AbsDiffMQscores_sendertime" , "NumberJusticesPro_sendertime", "Overruled_sendertime",
      "instar2_sendertime2","outstar2_sendertime2", "mutual_sendertime2", "triangle_sendertime2","mq_sendertime2" ,"sameissuearea_sendertime2",
      "yeardiff_sendertime2" , "yeardiffsquare_sendertime2", "AbsDiffMQscores_sendertime2" , "NumberJusticesPro_sendertime2", "Overruled_sendertime2",
      "instar2_sendertime3","outstar2_sendertime3", "mutual_sendertime3", "triangle_sendertime3","mq_sendertime3" ,"sameissuearea_sendertime3",
      "yeardiff_sendertime3" , "yeardiffsquare_sendertime3", "AbsDiffMQscores_sendertime3" , "NumberJusticesPro_sendertime3", "Overruled_sendertime3",
      "edgecov.year2", "edgecov.year3")
colnames(bootstrapResults)<- d




# sender times
sender_times <- 1:79
sender_years <- 1937:2015

density_coefficients <- c("edges","edgecov.year","edgecov.year2","edgecov.year3")
i2star_coefficients <- c("istar2","instar2_sendertime","instar2_sendertime2","instar2_sendertime3")
o2star_coefficients <- c("ostar2","outstar2_sendertime","outstar2_sendertime2","outstar2_sendertime3")
mutual_coefficients <- c("mutual","mutual_sendertime","mutual_sendertime2","mutual_sendertime3")
triangle_coefficients <- c("triangle","triangle_sendertime","triangle_sendertime2","triangle_sendertime3")
mq_coefficients <- c("edgecov.mq.t","mq_sendertime","mq_sendertime2","mq_sendertime3")
sameissue_coefficients <- c("edgecov.same.issue.area.t","sameissuearea_sendertime","sameissuearea_sendertime2","sameissuearea_sendertime3")
absdiffmq_coefficients <- c("nodeicov.AbsDiffMQscores","AbsDiffMQscores_sendertime","AbsDiffMQscores_sendertime2","AbsDiffMQscores_sendertime3")
numberjusticespro_coefficients <- c("nodeicov.NumberJusticesPro","NumberJusticesPro_sendertime","NumberJusticesPro_sendertime2","NumberJusticesPro_sendertime3")
overruled_coefficients <- c("nodeicov.Overruled", "Overruled_sendertime", "Overruled_sendertime2", "Overruled_sendertime3")
year.diff_coefficients <- c("edgecov.year.diff.t", "yeardiff_sendertime", "yeardiff_sendertime2", "yeardiff_sendertime3")
year.diff.square_coefficients <- c("edgecov.year.diff.square.t", "yeardiffsquare_sendertime", "yeardiffsquare_sendertime2", "yeardiffsquare_sendertime3" )


#### MQ is not significant
# example trend CI plot for MQ coef
mq_coef_trend <- matrix(NA,length(sender_times),3)
colnames(mq_coef_trend) <- c("mean","lower","upper")
for(t in sender_times){
  effect.t <- bootstrapResults[,mq_coefficients[1]]+t*bootstrapResults[,mq_coefficients[2]]+t^2*bootstrapResults[,mq_coefficients[3]]+t^3*bootstrapResults[,mq_coefficients[4]]
  mq_coef_trend[t,] <- c(mean(effect.t),quantile(effect.t,prob=c(0.025,.975)))
}

pdf("./images/mq_coef_trend_cubic.pdf",width=6,height=3.5)
par(las=1,mar=c(4,4,1,1))
plot(sender_years,mq_coef_trend[,1],type="n",ylab="Effect",xlab="Year", main="Martin-Quinn Score" ,ylim=c(min(c(mq_coef_trend)),max(c(mq_coef_trend))))
polygon(c(sender_years,rev(sender_years)),c(mq_coef_trend[,2],rev(mq_coef_trend[,3])),col="lightblue",border=NA)
lines(sender_years,mq_coef_trend[,1],lwd=2,col="grey50")
abline(h=0,lty=2,lwd=2)
dev.off()

# forgot add terms for cubic
# trend CI plot for edges coef
edges_coef_trend <- matrix(NA,length(sender_times),3)
colnames(edges_coef_trend) <- c("mean","lower","upper")
for(t in sender_times){
  effect.t <- bootstrapResults[,density_coefficients[1]]+t*bootstrapResults[,density_coefficients[2]]+t^2*bootstrapResults[,density_coefficients[3]]+t^3*bootstrapResults[,density_coefficients[4]]
  edges_coef_trend[t,] <- c(mean(effect.t),quantile(effect.t,prob=c(0.025,.975)))
}

pdf("./images/edges_coef_trend_cubic.pdf",width=6,height=3.5)
par(las=1,mar=c(4,4,1,1))
plot(sender_years,edges_coef_trend[,1],type="n",ylab="Effect",xlab="Year", main="Edges" ,ylim=c(min(c(edges_coef_trend)),max(c(edges_coef_trend))))
polygon(c(sender_years,rev(sender_years)),c(edges_coef_trend[,2],rev(edges_coef_trend[,3])),col="lightblue",border=NA)
lines(sender_years,edges_coef_trend[,1],lwd=2,col="grey50")
abline(h=0,lty=2,lwd=2)
dev.off()


# trend CI plot for i2star coef
i2star_coef_trend <- matrix(NA,length(sender_times),3)
colnames(i2star_coef_trend) <- c("mean","lower","upper")
for(t in sender_times){
  effect.t <- bootstrapResults[,i2star_coefficients[1]]+t*bootstrapResults[,i2star_coefficients[2]]+(t^2)*bootstrapResults[,i2star_coefficients[3]]+(t^3)*bootstrapResults[,i2star_coefficients[4]]
  i2star_coef_trend[t,] <- c(mean(effect.t),quantile(effect.t,prob=c(0.025,.975)))
}

pdf("./images/i2star_coef_trend_cubic.pdf",width=6,height=3.5)
par(las=1,mar=c(4,4,1,1))
plot(sender_years,i2star_coef_trend[,1],type="n",ylab="Effect",xlab="Year", main="Instar(2)" ,ylim=c(min(c(i2star_coef_trend)),max(c(i2star_coef_trend))))
polygon(c(sender_years,rev(sender_years)),c(i2star_coef_trend[,2],rev(i2star_coef_trend[,3])),col="lightblue",border=NA)
lines(sender_years,i2star_coef_trend[,1],lwd=2,col="grey50")
abline(h=0,lty=2,lwd=2)
dev.off()


# trend CI plot for o2star coef
o2star_coef_trend <- matrix(NA,length(sender_times),3)
colnames(o2star_coef_trend) <- c("mean","lower","upper")
for(t in sender_times){
  effect.t <- bootstrapResults[,o2star_coefficients[1]]+t*bootstrapResults[,o2star_coefficients[2]]+t^2*bootstrapResults[,o2star_coefficients[3]]+t^3*bootstrapResults[,o2star_coefficients[4]]
  o2star_coef_trend[t,] <- c(mean(effect.t),quantile(effect.t,prob=c(0.025,.975)))
}

pdf("./images/o2star_coef_trend_cubic.pdf",width=6,height=3.5)
par(las=1,mar=c(4,4,1,1))
plot(sender_years,o2star_coef_trend[,1],type="n",ylab="Effect",xlab="Year", main="Outstar(2)" ,ylim=c(min(c(o2star_coef_trend)),max(c(o2star_coef_trend))))
polygon(c(sender_years,rev(sender_years)),c(o2star_coef_trend[,2],rev(o2star_coef_trend[,3])),col="lightblue",border=NA)
lines(sender_years,o2star_coef_trend[,1],lwd=2,col="grey50")
abline(h=0,lty=2,lwd=2)
dev.off()

# mutual not sig
# trend CI plot for mutual coef
mutual_coef_trend <- matrix(NA,length(sender_times),3)
colnames(mutual_coef_trend) <- c("mean","lower","upper")
for(t in sender_times){
  effect.t <- bootstrapResults[,mutual_coefficients[1]]+t*bootstrapResults[,mutual_coefficients[2]]+t^2*bootstrapResults[,mutual_coefficients[3]]+t^3*bootstrapResults[,mutual_coefficients[4]]
  mutual_coef_trend[t,] <- c(quantile(effect.t,prob=c(0.5, 0.025,.975)))
}

pdf("./images/mutual_coef_trend_cubic.pdf",width=6,height=3.5)
par(las=1,mar=c(4,4,1,1))
plot(sender_years,mutual_coef_trend[,1],type="n",ylab="Effect",xlab="Year", main="Mutual" ,ylim=c(-50, 50))
polygon(c(sender_years,rev(sender_years)),c(mutual_coef_trend[,2],rev(mutual_coef_trend[,3])),col="lightblue",border=NA)
lines(sender_years,mutual_coef_trend[,1],lwd=2,col="grey50")
abline(h=0,lty=2,lwd=2)
dev.off()


# trend CI plot for triangle coef
triangle_coef_trend <- matrix(NA,length(sender_times),3)
colnames(triangle_coef_trend) <- c("mean","lower","upper")
for(t in sender_times){
  effect.t <- bootstrapResults[,triangle_coefficients[1]]+t*bootstrapResults[,triangle_coefficients[2]]+t^2*bootstrapResults[,triangle_coefficients[3]]+t^3*bootstrapResults[,triangle_coefficients[4]]
  triangle_coef_trend[t,] <- c(mean(effect.t),quantile(effect.t,prob=c(0.025,.975)))
}

pdf("./images/triangle_coef_trend_cubic.pdf",width=6,height=3.5)
par(las=1,mar=c(4,4,1,1))
plot(sender_years,triangle_coef_trend[,1],type="n",ylab="Effect",xlab="Year", main="Triangle" ,ylim=c(min(c(triangle_coef_trend)),max(c(triangle_coef_trend))))
polygon(c(sender_years,rev(sender_years)),c(triangle_coef_trend[,2],rev(triangle_coef_trend[,3])),col="lightblue",border=NA)
lines(sender_years,triangle_coef_trend[,1],lwd=2,col="grey50")
abline(h=0,lty=2,lwd=2)
dev.off()


# trend CI plot for sameissuearea coef
sameissue_coef_trend <- matrix(NA,length(sender_times),3)
colnames(sameissue_coef_trend) <- c("mean","lower","upper")
for(t in sender_times){
  effect.t <- bootstrapResults[,sameissue_coefficients[1]]+t*bootstrapResults[,sameissue_coefficients[2]]+t^2*bootstrapResults[,sameissue_coefficients[3]]+t^3*bootstrapResults[,sameissue_coefficients[4]]
  sameissue_coef_trend[t,] <- c(mean(effect.t),quantile(effect.t,prob=c(0.025,.975)))
}

pdf("./images/sameissue_coef_trend_cubic.pdf",width=6,height=3.5)
par(las=1,mar=c(4,4,1,1))
plot(sender_years,sameissue_coef_trend[,1],type="n",ylab="Effect",xlab="Year", main="Same Issue Area" ,ylim=c(min(c(sameissue_coef_trend)),max(c(sameissue_coef_trend))))
polygon(c(sender_years,rev(sender_years)),c(sameissue_coef_trend[,2],rev(sameissue_coef_trend[,3])),col="lightblue",border=NA)
lines(sender_years,sameissue_coef_trend[,1],lwd=2,col="grey50")
abline(h=0,lty=2,lwd=2)
dev.off()


# trend CI plot for absdiffmq coef
absdiffmq_coef_trend <- matrix(NA,length(sender_times),3)
colnames(absdiffmq_coef_trend) <- c("mean","lower","upper")
for(t in sender_times){
  effect.t <- bootstrapResults[,absdiffmq_coefficients[1]]+t*bootstrapResults[,absdiffmq_coefficients[2]]+t^2*bootstrapResults[,absdiffmq_coefficients[3]]+t^3*bootstrapResults[,absdiffmq_coefficients[4]]
  absdiffmq_coef_trend[t,] <- c(mean(effect.t),quantile(effect.t,prob=c(0.025,.975)))
}

pdf("./images/absdiffmq_coef_trend_cubic.pdf",width=6,height=3.5)
par(las=1,mar=c(4,4,1,1))
plot(sender_years,absdiffmq_coef_trend[,1],type="n",ylab="Effect",xlab="Year", main="Absolute Difference in Martin Quinn Scores" ,ylim=c(min(c(absdiffmq_coef_trend)),max(c(absdiffmq_coef_trend))))
polygon(c(sender_years,rev(sender_years)),c(absdiffmq_coef_trend[,2],rev(absdiffmq_coef_trend[,3])),col="lightblue",border=NA)
lines(sender_years,absdiffmq_coef_trend[,1],lwd=2,col="grey50")
abline(h=0,lty=2,lwd=2)
dev.off()


# trend CI plot for justices that voted pro coef
numberjusticespro_coef_trend <- matrix(NA,length(sender_times),3)
colnames(numberjusticespro_coef_trend) <- c("mean","lower","upper")
for(t in sender_times){
  effect.t <- bootstrapResults[,numberjusticespro_coefficients[1]]+t*bootstrapResults[,numberjusticespro_coefficients[2]]+t^2*bootstrapResults[,numberjusticespro_coefficients[3]]+t^3*bootstrapResults[,numberjusticespro_coefficients[4]]
  numberjusticespro_coef_trend[t,] <- c(mean(effect.t),quantile(effect.t,prob=c(0.025,.975)))
}

pdf("./images/numberjusticespro_coef_trend_cubic.pdf",width=6,height=3.5)
par(las=1,mar=c(4,4,1,1))
plot(sender_years,numberjusticespro_coef_trend[,1],type="n",ylab="Effect",xlab="Year", main="Number of Justices in Majority" ,ylim=c(min(c(numberjusticespro_coef_trend)),max(c(numberjusticespro_coef_trend))))
polygon(c(sender_years,rev(sender_years)),c(numberjusticespro_coef_trend[,2],rev(numberjusticespro_coef_trend[,3])),col="lightblue",border=NA)
lines(sender_years,numberjusticespro_coef_trend[,1],lwd=2,col="grey50")
abline(h=0,lty=2,lwd=2)
dev.off()

# trend CI plot for overruled cases coef
overruled_coef_trend <- matrix(NA,length(sender_times),3)
colnames(overruled_coef_trend) <- c("mean","lower","upper")
for(t in sender_times){
  effect.t <- bootstrapResults[,overruled_coefficients[1]]+t*bootstrapResults[,overruled_coefficients[2]]+t^2*bootstrapResults[,overruled_coefficients[3]]+t^3*bootstrapResults[,overruled_coefficients[4]]
  overruled_coef_trend[t,] <- c(mean(effect.t),quantile(effect.t,prob=c(0.025,.975)))
}

pdf("./images/overruled_coef_trend_cubic.pdf",width=6,height=3.5)
par(las=1,mar=c(4,4,1,1))
plot(sender_years,overruled_coef_trend[,1],type="n",ylab="Effect",xlab="Year", main="Overruled Cases" ,ylim=c(min(c(overruled_coef_trend)),max(c(overruled_coef_trend))))
polygon(c(sender_years,rev(sender_years)),c(overruled_coef_trend[,2],rev(overruled_coef_trend[,3])),col="lightblue",border=NA)
lines(sender_years,overruled_coef_trend[,1],lwd=2,col="grey50")
abline(h=0,lty=2,lwd=2)
dev.off()


# trend CI plot for year diff coef
year.diff_coef_trend <- matrix(NA,length(sender_times),3)
colnames(year.diff_coef_trend) <- c("mean","lower","upper")
for(t in sender_times){
  effect.t <- bootstrapResults[,year.diff_coefficients[1]]+t*bootstrapResults[,year.diff_coefficients[2]]+t^2*bootstrapResults[,year.diff_coefficients[3]]+t^3*bootstrapResults[,year.diff_coefficients[4]]
  year.diff_coef_trend[t,] <- c(mean(effect.t),quantile(effect.t,prob=c(0.025,.975)))
}

pdf("./images/yeardiff_coef_trend_cubic.pdf",width=6,height=3.5)
par(las=1,mar=c(4,4,1,1))
plot(sender_years,year.diff_coef_trend[,1],type="n",ylab="Effect",xlab="Year", main="Absolute Difference in Years between Cases" ,ylim=c(min(c(year.diff_coef_trend)),max(c(year.diff_coef_trend))))
polygon(c(sender_years,rev(sender_years)),c(year.diff_coef_trend[,2],rev(year.diff_coef_trend[,3])),col="lightblue",border=NA)
lines(sender_years,year.diff_coef_trend[,1],lwd=2,col="grey50")
abline(h=0,lty=2,lwd=2)
dev.off()

# trend CI plot for year diff square coef
year.diff.square_coef_trend <- matrix(NA,length(sender_times),3)
colnames(year.diff.square_coef_trend) <- c("mean","lower","upper")
for(t in sender_times){
  effect.t <- bootstrapResults[,year.diff.square_coefficients[1]]+t*bootstrapResults[,year.diff.square_coefficients[2]]+t^2*bootstrapResults[,year.diff.square_coefficients[3]]+t^3*bootstrapResults[,year.diff.square_coefficients[4]]
  year.diff.square_coef_trend[t,] <- c(mean(effect.t),quantile(effect.t,prob=c(0.025,.975)))
}

pdf("./images/yeardiffsquare_coef_trend_cubic.pdf",width=6,height=3.5)
par(las=1,mar=c(4,4,1,1))
plot(sender_years,year.diff.square_coef_trend[,1],type="n",ylab="Effect",xlab="Year", main="Years Difference Square between Cases" ,ylim=c(min(c(year.diff.square_coef_trend)),max(c(year.diff.square_coef_trend))))
polygon(c(sender_years,rev(sender_years)),c(year.diff.square_coef_trend[,2],rev(year.diff.square_coef_trend[,3])),col="lightblue",border=NA)
lines(sender_years,year.diff.square_coef_trend[,1],lwd=2,col="grey50")
abline(h=0,lty=2,lwd=2)
dev.off()

###################################################################################
### degree 1 polynomials


bootstrapResults <- read.csv("bootresults_1.csv",stringsAsFactors=F,row.names=1)

d<- c("edges","istar2","ostar2","mutual","triangle", "edgecov.mq.t", "edgecov.same.issue.area.t", "edgecov.year.diff.t" ,"edgecov.year.diff.square.t" ,  
      "nodeicov.AbsDiffMQscores", "nodeicov.NumberJusticesPro", "edgecov.year" , "nodeifactor.SameIssueArea.2", "nodeifactor.SameIssueArea.3",
      "nodeifactor.SameIssueArea.4","nodeifactor.SameIssueArea.5", "nodeifactor.SameIssueArea.6", "nodeifactor.SameIssueArea.7", 
      "nodeifactor.SameIssueArea.8","nodeifactor.SameIssueArea.9",  "nodeifactor.SameIssueArea.10",
      "nodeifactor.SameIssueArea.11", "nodeifactor.SameIssueArea.12", "nodeifactor.SameIssueArea.13","nodeifactor.SameIssueArea.14", 
      "nodeofactor.SameIssueArea.2", "nodeofactor.SameIssueArea.3", "nodeofactor.SameIssueArea.4","nodeofactor.SameIssueArea.5", 
      "nodeofactor.SameIssueArea.6","nodeofactor.SameIssueArea.7",
      "nodeofactor.SameIssueArea.8","nodeofactor.SameIssueArea.9","nodeofactor.SameIssueArea.10", "nodeofactor.SameIssueArea.11", 
      "nodeofactor.SameIssueArea.12", "nodeofactor.SameIssueArea.13","nodeofactor.SameIssueArea.14", "nodeicov.Overruled",
      "instar2_sendertime","outstar2_sendertime", "mutual_sendertime", "triangle_sendertime","mq_sendertime" ,"sameissuearea_sendertime",
      "yeardiff_sendertime" , "yeardiffsquare_sendertime", "AbsDiffMQscores_sendertime" , "NumberJusticesPro_sendertime", "Overruled_sendertime")
colnames(bootstrapResults)<- d




# sender times
sender_times <- 1:79
sender_years <- 1937:2015

density_coefficients <- c("edges","edgecov.year")
i2star_coefficients <- c("istar2","instar2_sendertime")
o2star_coefficients <- c("ostar2","outstar2_sendertime")
mutual_coefficients <- c("mutual","mutual_sendertime")
triangle_coefficients <- c("triangle","triangle_sendertime")
mq_coefficients <- c("edgecov.mq.t","mq_sendertime")
sameissue_coefficients <- c("edgecov.same.issue.area.t","sameissuearea_sendertime")
absdiffmq_coefficients <- c("nodeicov.AbsDiffMQscores","AbsDiffMQscores_sendertime")
numberjusticespro_coefficients <- c("nodeicov.NumberJusticesPro","NumberJusticesPro_sendertime")
overruled_coefficients <- c("nodeicov.Overruled", "Overruled_sendertime")
year.diff_coefficients <- c("edgecov.year.diff.t", "yeardiff_sendertime", "yeardiff_sendertime2", "yeardiff_sendertime3")
year.diff.square_coefficients <- c("edgecov.year.diff.square.t", "yeardiffsquare_sendertime" )


#### MQ is not significant
# example trend CI plot for MQ coef
mq_coef_trend <- matrix(NA,length(sender_times),3)
colnames(mq_coef_trend) <- c("mean","lower","upper")
for(t in sender_times){
  effect.t <- bootstrapResults[,mq_coefficients[1]]+t*bootstrapResults[,mq_coefficients[2]]
  mq_coef_trend[t,] <- c(mean(effect.t),quantile(effect.t,prob=c(0.025,.975)))
}

pdf("./images/mq_coef_trend_linear.pdf",width=6,height=3.5)
par(las=1,mar=c(4,4,1,1))
plot(sender_years,mq_coef_trend[,1],type="n",ylab="Effect",xlab="Year", main="Martin-Quinn Score" ,ylim=c(min(c(mq_coef_trend)),max(c(mq_coef_trend))))
polygon(c(sender_years,rev(sender_years)),c(mq_coef_trend[,2],rev(mq_coef_trend[,3])),col="lightblue",border=NA)
lines(sender_years,mq_coef_trend[,1],lwd=2,col="grey50")
abline(h=0,lty=2,lwd=2)
dev.off()

# forgot add terms for cubic
# trend CI plot for edges coef
edges_coef_trend <- matrix(NA,length(sender_times),3)
colnames(edges_coef_trend) <- c("mean","lower","upper")
for(t in sender_times){
  effect.t <- bootstrapResults[,density_coefficients[1]]+t*bootstrapResults[,density_coefficients[2]]
  edges_coef_trend[t,] <- c(mean(effect.t),quantile(effect.t,prob=c(0.025,.975)))
}

pdf("./images/edges_coef_trend_linear.pdf",width=6,height=3.5)
par(las=1,mar=c(4,4,1,1))
plot(sender_years,edges_coef_trend[,1],type="n",ylab="Effect",xlab="Year", main="Edges" ,ylim=c(min(c(edges_coef_trend)),max(c(edges_coef_trend))))
polygon(c(sender_years,rev(sender_years)),c(edges_coef_trend[,2],rev(edges_coef_trend[,3])),col="lightblue",border=NA)
lines(sender_years,edges_coef_trend[,1],lwd=2,col="grey50")
abline(h=0,lty=2,lwd=2)
dev.off()


# trend CI plot for i2star coef
i2star_coef_trend <- matrix(NA,length(sender_times),3)
colnames(i2star_coef_trend) <- c("mean","lower","upper")
for(t in sender_times){
  effect.t <- bootstrapResults[,i2star_coefficients[1]]+t*bootstrapResults[,i2star_coefficients[2]]
  i2star_coef_trend[t,] <- c(mean(effect.t),quantile(effect.t,prob=c(0.025,.975)))
}

pdf("./images/i2star_coef_trend_linear.pdf",width=6,height=3.5)
par(las=1,mar=c(4,4,1,1))
plot(sender_years,i2star_coef_trend[,1],type="n",ylab="Effect",xlab="Year", main="Instar(2)" ,ylim=c(min(c(i2star_coef_trend)),max(c(i2star_coef_trend))))
polygon(c(sender_years,rev(sender_years)),c(i2star_coef_trend[,2],rev(i2star_coef_trend[,3])),col="lightblue",border=NA)
lines(sender_years,i2star_coef_trend[,1],lwd=2,col="grey50")
abline(h=0,lty=2,lwd=2)
dev.off()


# trend CI plot for o2star coef
o2star_coef_trend <- matrix(NA,length(sender_times),3)
colnames(o2star_coef_trend) <- c("mean","lower","upper")
for(t in sender_times){
  effect.t <- bootstrapResults[,o2star_coefficients[1]]+t*bootstrapResults[,o2star_coefficients[2]]
  o2star_coef_trend[t,] <- c(mean(effect.t),quantile(effect.t,prob=c(0.025,.975)))
}

pdf("./images/o2star_coef_trend_linear.pdf",width=6,height=3.5)
par(las=1,mar=c(4,4,1,1))
plot(sender_years,o2star_coef_trend[,1],type="n",ylab="Effect",xlab="Year", main="Outstar(2)" ,ylim=c(min(c(o2star_coef_trend)),max(c(o2star_coef_trend))))
polygon(c(sender_years,rev(sender_years)),c(o2star_coef_trend[,2],rev(o2star_coef_trend[,3])),col="lightblue",border=NA)
lines(sender_years,o2star_coef_trend[,1],lwd=2,col="grey50")
abline(h=0,lty=2,lwd=2)
dev.off()

# mutual not sig
# trend CI plot for mutual coef
mutual_coef_trend <- matrix(NA,length(sender_times),3)
colnames(mutual_coef_trend) <- c("mean","lower","upper")
for(t in sender_times){
  effect.t <- bootstrapResults[,mutual_coefficients[1]]+t*bootstrapResults[,mutual_coefficients[2]]
  mutual_coef_trend[t,] <- c(quantile(effect.t,prob=c(0.5, 0.025,.975)))
}

pdf("./images/mutual_coef_trend_linear.pdf",width=6,height=3.5)
par(las=1,mar=c(4,4,1,1))
plot(sender_years,mutual_coef_trend[,1],type="n",ylab="Effect",xlab="Year", main="Mutual" ,ylim=c(-50, 50))
polygon(c(sender_years,rev(sender_years)),c(mutual_coef_trend[,2],rev(mutual_coef_trend[,3])),col="lightblue",border=NA)
lines(sender_years,mutual_coef_trend[,1],lwd=2,col="grey50")
abline(h=0,lty=2,lwd=2)
dev.off()


# trend CI plot for triangle coef
triangle_coef_trend <- matrix(NA,length(sender_times),3)
colnames(triangle_coef_trend) <- c("mean","lower","upper")
for(t in sender_times){
  effect.t <- bootstrapResults[,triangle_coefficients[1]]+t*bootstrapResults[,triangle_coefficients[2]]
  triangle_coef_trend[t,] <- c(mean(effect.t),quantile(effect.t,prob=c(0.025,.975)))
}

pdf("./images/triangle_coef_trend_linear.pdf",width=6,height=3.5)
par(las=1,mar=c(4,4,1,1))
plot(sender_years,triangle_coef_trend[,1],type="n",ylab="Effect",xlab="Year", main="Triangle" ,ylim=c(min(c(triangle_coef_trend)),max(c(triangle_coef_trend))))
polygon(c(sender_years,rev(sender_years)),c(triangle_coef_trend[,2],rev(triangle_coef_trend[,3])),col="lightblue",border=NA)
lines(sender_years,triangle_coef_trend[,1],lwd=2,col="grey50")
abline(h=0,lty=2,lwd=2)
dev.off()


# trend CI plot for sameissuearea coef
sameissue_coef_trend <- matrix(NA,length(sender_times),3)
colnames(sameissue_coef_trend) <- c("mean","lower","upper")
for(t in sender_times){
  effect.t <- bootstrapResults[,sameissue_coefficients[1]]+t*bootstrapResults[,sameissue_coefficients[2]]
  sameissue_coef_trend[t,] <- c(mean(effect.t),quantile(effect.t,prob=c(0.025,.975)))
}

pdf("./images/sameissue_coef_trend_linear.pdf",width=6,height=3.5)
par(las=1,mar=c(4,4,1,1))
plot(sender_years,sameissue_coef_trend[,1],type="n",ylab="Effect",xlab="Year", main="Same Issue Area" ,ylim=c(min(c(sameissue_coef_trend)),max(c(sameissue_coef_trend))))
polygon(c(sender_years,rev(sender_years)),c(sameissue_coef_trend[,2],rev(sameissue_coef_trend[,3])),col="lightblue",border=NA)
lines(sender_years,sameissue_coef_trend[,1],lwd=2,col="grey50")
abline(h=0,lty=2,lwd=2)
dev.off()


# trend CI plot for absdiffmq coef
absdiffmq_coef_trend <- matrix(NA,length(sender_times),3)
colnames(absdiffmq_coef_trend) <- c("mean","lower","upper")
for(t in sender_times){
  effect.t <- bootstrapResults[,absdiffmq_coefficients[1]]+t*bootstrapResults[,absdiffmq_coefficients[2]]
  absdiffmq_coef_trend[t,] <- c(mean(effect.t),quantile(effect.t,prob=c(0.025,.975)))
}

pdf("./images/absdiffmq_coef_trend_linear.pdf",width=6,height=3.5)
par(las=1,mar=c(4,4,1,1))
plot(sender_years,absdiffmq_coef_trend[,1],type="n",ylab="Effect",xlab="Year", main="Absolute Difference in Martin Quinn Scores" ,ylim=c(min(c(absdiffmq_coef_trend)),max(c(absdiffmq_coef_trend))))
polygon(c(sender_years,rev(sender_years)),c(absdiffmq_coef_trend[,2],rev(absdiffmq_coef_trend[,3])),col="lightblue",border=NA)
lines(sender_years,absdiffmq_coef_trend[,1],lwd=2,col="grey50")
abline(h=0,lty=2,lwd=2)
dev.off()


# trend CI plot for justices that voted pro coef
numberjusticespro_coef_trend <- matrix(NA,length(sender_times),3)
colnames(numberjusticespro_coef_trend) <- c("mean","lower","upper")
for(t in sender_times){
  effect.t <- bootstrapResults[,numberjusticespro_coefficients[1]]+t*bootstrapResults[,numberjusticespro_coefficients[2]]
  numberjusticespro_coef_trend[t,] <- c(mean(effect.t),quantile(effect.t,prob=c(0.025,.975)))
}

pdf("./images/numberjusticespro_coef_trend_linear.pdf",width=6,height=3.5)
par(las=1,mar=c(4,4,1,1))
plot(sender_years,numberjusticespro_coef_trend[,1],type="n",ylab="Effect",xlab="Year", main="Number of Justices in Majority" ,ylim=c(min(c(numberjusticespro_coef_trend)),max(c(numberjusticespro_coef_trend))))
polygon(c(sender_years,rev(sender_years)),c(numberjusticespro_coef_trend[,2],rev(numberjusticespro_coef_trend[,3])),col="lightblue",border=NA)
lines(sender_years,numberjusticespro_coef_trend[,1],lwd=2,col="grey50")
abline(h=0,lty=2,lwd=2)
dev.off()

# trend CI plot for overruled cases coef
overruled_coef_trend <- matrix(NA,length(sender_times),3)
colnames(overruled_coef_trend) <- c("mean","lower","upper")
for(t in sender_times){
  effect.t <- bootstrapResults[,overruled_coefficients[1]]+t*bootstrapResults[,overruled_coefficients[2]]
  overruled_coef_trend[t,] <- c(mean(effect.t),quantile(effect.t,prob=c(0.025,.975)))
}

pdf("./images/overruled_coef_trend_linear.pdf",width=6,height=3.5)
par(las=1,mar=c(4,4,1,1))
plot(sender_years,overruled_coef_trend[,1],type="n",ylab="Effect",xlab="Year", main="Overruled Cases" ,ylim=c(min(c(overruled_coef_trend)),max(c(overruled_coef_trend))))
polygon(c(sender_years,rev(sender_years)),c(overruled_coef_trend[,2],rev(overruled_coef_trend[,3])),col="lightblue",border=NA)
lines(sender_years,overruled_coef_trend[,1],lwd=2,col="grey50")
abline(h=0,lty=2,lwd=2)
dev.off()

# trend CI plot for year diff coef
year.diff_coef_trend <- matrix(NA,length(sender_times),3)
colnames(year.diff_coef_trend) <- c("mean","lower","upper")
for(t in sender_times){
  effect.t <- bootstrapResults[,year.diff_coefficients[1]]+t*bootstrapResults[,year.diff_coefficients[2]]
  year.diff_coef_trend[t,] <- c(mean(effect.t),quantile(effect.t,prob=c(0.025,.975)))
}

pdf("./images/yeardiff_coef_trend_linear.pdf",width=6,height=3.5)
par(las=1,mar=c(4,4,1,1))
plot(sender_years,year.diff_coef_trend[,1],type="n",ylab="Effect",xlab="Year", main="Absolute Difference in Years between Cases" ,ylim=c(min(c(year.diff_coef_trend)),max(c(year.diff_coef_trend))))
polygon(c(sender_years,rev(sender_years)),c(year.diff_coef_trend[,2],rev(year.diff_coef_trend[,3])),col="lightblue",border=NA)
lines(sender_years,year.diff_coef_trend[,1],lwd=2,col="grey50")
abline(h=0,lty=2,lwd=2)
dev.off()

# trend CI plot for year diff square coef
year.diff.square_coef_trend <- matrix(NA,length(sender_times),3)
colnames(year.diff.square_coef_trend) <- c("mean","lower","upper")
for(t in sender_times){
  effect.t <- bootstrapResults[,year.diff.square_coefficients[1]]+t*bootstrapResults[,year.diff.square_coefficients[2]]
  year.diff.square_coef_trend[t,] <- c(mean(effect.t),quantile(effect.t,prob=c(0.025,.975)))
}

pdf("./images/yeardiffsquare_coef_trend_linear.pdf",width=6,height=3.5)
par(las=1,mar=c(4,4,1,1))
plot(sender_years,year.diff.square_coef_trend[,1],type="n",ylab="Effect",xlab="Year", main="Years Difference Square between Cases" ,ylim=c(min(c(year.diff.square_coef_trend)),max(c(year.diff.square_coef_trend))))
polygon(c(sender_years,rev(sender_years)),c(year.diff.square_coef_trend[,2],rev(year.diff.square_coef_trend[,3])),col="lightblue",border=NA)
lines(sender_years,year.diff.square_coef_trend[,1],lwd=2,col="grey50")
abline(h=0,lty=2,lwd=2)
dev.off()

#####################################################
### degree 2 polynomials

# code assumes that working directory is set
# to top-level project
bootstrapResults <- read.csv("bootresults_2.csv",stringsAsFactors=F,row.names=1)

d<- c("edges","istar2","ostar2","mutual","triangle", "edgecov.mq.t", "edgecov.same.issue.area.t", "edgecov.year.diff.t" ,"edgecov.year.diff.square.t" ,  
      "nodeicov.AbsDiffMQscores", "nodeicov.NumberJusticesPro", "edgecov.year" , "nodeifactor.SameIssueArea.2", "nodeifactor.SameIssueArea.3",
      "nodeifactor.SameIssueArea.4","nodeifactor.SameIssueArea.5", "nodeifactor.SameIssueArea.6", "nodeifactor.SameIssueArea.7", 
      "nodeifactor.SameIssueArea.8","nodeifactor.SameIssueArea.9",  "nodeifactor.SameIssueArea.10",
      "nodeifactor.SameIssueArea.11", "nodeifactor.SameIssueArea.12", "nodeifactor.SameIssueArea.13","nodeifactor.SameIssueArea.14", 
      "nodeofactor.SameIssueArea.2", "nodeofactor.SameIssueArea.3", "nodeofactor.SameIssueArea.4","nodeofactor.SameIssueArea.5", 
      "nodeofactor.SameIssueArea.6","nodeofactor.SameIssueArea.7",
      "nodeofactor.SameIssueArea.8","nodeofactor.SameIssueArea.9","nodeofactor.SameIssueArea.10", "nodeofactor.SameIssueArea.11", 
      "nodeofactor.SameIssueArea.12", "nodeofactor.SameIssueArea.13","nodeofactor.SameIssueArea.14", "nodeicov.Overruled",
      "instar2_sendertime","outstar2_sendertime", "mutual_sendertime", "triangle_sendertime","mq_sendertime" ,"sameissuearea_sendertime",
      "yeardiff_sendertime" , "yeardiffsquare_sendertime", "AbsDiffMQscores_sendertime" , "NumberJusticesPro_sendertime", "Overruled_sendertime",
      "instar2_sendertime2","outstar2_sendertime2", "mutual_sendertime2", "triangle_sendertime2","mq_sendertime2" ,"sameissuearea_sendertime2",
      "yeardiff_sendertime2" , "yeardiffsquare_sendertime2", "AbsDiffMQscores_sendertime2" , "NumberJusticesPro_sendertime2", "Overruled_sendertime2","edgecov.year2")
colnames(bootstrapResults)<- d




# sender times
sender_times <- 1:79
sender_years <- 1937:2015

density_coefficients <- c("edges","edgecov.year","edgecov.year2")
i2star_coefficients <- c("istar2","instar2_sendertime","instar2_sendertime2")
o2star_coefficients <- c("ostar2","outstar2_sendertime","outstar2_sendertime2")
mutual_coefficients <- c("mutual","mutual_sendertime","mutual_sendertime2")
triangle_coefficients <- c("triangle","triangle_sendertime","triangle_sendertime2")
mq_coefficients <- c("edgecov.mq.t","mq_sendertime","mq_sendertime2")
sameissue_coefficients <- c("edgecov.same.issue.area.t","sameissuearea_sendertime","sameissuearea_sendertime2")
absdiffmq_coefficients <- c("nodeicov.AbsDiffMQscores","AbsDiffMQscores_sendertime","AbsDiffMQscores_sendertime2")
numberjusticespro_coefficients <- c("nodeicov.NumberJusticesPro","NumberJusticesPro_sendertime","NumberJusticesPro_sendertime2")
overruled_coefficients <- c("nodeicov.Overruled", "Overruled_sendertime", "Overruled_sendertime2")
year.diff_coefficients <- c("edgecov.year.diff.t", "yeardiff_sendertime", "yeardiff_sendertime2")
year.diff.square_coefficients <- c("edgecov.year.diff.square.t", "yeardiffsquare_sendertime", "yeardiffsquare_sendertime2" )


#### MQ is not significant
# example trend CI plot for MQ coef
mq_coef_trend <- matrix(NA,length(sender_times),3)
colnames(mq_coef_trend) <- c("mean","lower","upper")
for(t in sender_times){
  effect.t <- bootstrapResults[,mq_coefficients[1]]+t*bootstrapResults[,mq_coefficients[2]]+t^2*bootstrapResults[,mq_coefficients[3]]
  mq_coef_trend[t,] <- c(mean(effect.t),quantile(effect.t,prob=c(0.025,.975)))
}

pdf("./images/mq_coef_trend_quadratic.pdf",width=6,height=3.5)
par(las=1,mar=c(4,4,1,1))
plot(sender_years,mq_coef_trend[,1],type="n",ylab="Effect",xlab="Year", main="Martin-Quinn Score" ,ylim=c(min(c(mq_coef_trend)),max(c(mq_coef_trend))))
polygon(c(sender_years,rev(sender_years)),c(mq_coef_trend[,2],rev(mq_coef_trend[,3])),col="lightblue",border=NA)
lines(sender_years,mq_coef_trend[,1],lwd=2,col="grey50")
abline(h=0,lty=2,lwd=2)
dev.off()

# forgot add terms for cubic
# trend CI plot for edges coef
edges_coef_trend <- matrix(NA,length(sender_times),3)
colnames(edges_coef_trend) <- c("mean","lower","upper")
for(t in sender_times){
  effect.t <- bootstrapResults[,density_coefficients[1]]+t*bootstrapResults[,density_coefficients[2]]+t^2*bootstrapResults[,density_coefficients[3]]
  edges_coef_trend[t,] <- c(mean(effect.t),quantile(effect.t,prob=c(0.025,.975)))
}

pdf("./images/edges_coef_trend_quadratic.pdf",width=6,height=3.5)
par(las=1,mar=c(4,4,1,1))
plot(sender_years,edges_coef_trend[,1],type="n",ylab="Effect",xlab="Year", main="Edges" ,ylim=c(min(c(edges_coef_trend)),max(c(edges_coef_trend))))
polygon(c(sender_years,rev(sender_years)),c(edges_coef_trend[,2],rev(edges_coef_trend[,3])),col="lightblue",border=NA)
lines(sender_years,edges_coef_trend[,1],lwd=2,col="grey50")
abline(h=0,lty=2,lwd=2)
dev.off()


# trend CI plot for i2star coef
i2star_coef_trend <- matrix(NA,length(sender_times),3)
colnames(i2star_coef_trend) <- c("mean","lower","upper")
for(t in sender_times){
  effect.t <- bootstrapResults[,i2star_coefficients[1]]+t*bootstrapResults[,i2star_coefficients[2]]+(t^2)*bootstrapResults[,i2star_coefficients[3]]
  i2star_coef_trend[t,] <- c(mean(effect.t),quantile(effect.t,prob=c(0.025,.975)))
}

pdf("./images/i2star_coef_trend_quadratic.pdf",width=6,height=3.5)
par(las=1,mar=c(4,4,1,1))
plot(sender_years,i2star_coef_trend[,1],type="n",ylab="Effect",xlab="Year", main="Instar(2)" ,ylim=c(min(c(i2star_coef_trend)),max(c(i2star_coef_trend))))
polygon(c(sender_years,rev(sender_years)),c(i2star_coef_trend[,2],rev(i2star_coef_trend[,3])),col="lightblue",border=NA)
lines(sender_years,i2star_coef_trend[,1],lwd=2,col="grey50")
abline(h=0,lty=2,lwd=2)
dev.off()


# trend CI plot for o2star coef
o2star_coef_trend <- matrix(NA,length(sender_times),3)
colnames(o2star_coef_trend) <- c("mean","lower","upper")
for(t in sender_times){
  effect.t <- bootstrapResults[,o2star_coefficients[1]]+t*bootstrapResults[,o2star_coefficients[2]]+t^2*bootstrapResults[,o2star_coefficients[3]]
  o2star_coef_trend[t,] <- c(mean(effect.t),quantile(effect.t,prob=c(0.025,.975)))
}

pdf("./images/o2star_coef_trend_quadratic.pdf",width=6,height=3.5)
par(las=1,mar=c(4,4,1,1))
plot(sender_years,o2star_coef_trend[,1],type="n",ylab="Effect",xlab="Year", main="Outstar(2)" ,ylim=c(min(c(o2star_coef_trend)),max(c(o2star_coef_trend))))
polygon(c(sender_years,rev(sender_years)),c(o2star_coef_trend[,2],rev(o2star_coef_trend[,3])),col="lightblue",border=NA)
lines(sender_years,o2star_coef_trend[,1],lwd=2,col="grey50")
abline(h=0,lty=2,lwd=2)
dev.off()

# mutual not sig
# trend CI plot for mutual coef
mutual_coef_trend <- matrix(NA,length(sender_times),3)
colnames(mutual_coef_trend) <- c("mean","lower","upper")
for(t in sender_times){
  effect.t <- bootstrapResults[,mutual_coefficients[1]]+t*bootstrapResults[,mutual_coefficients[2]]+t^2*bootstrapResults[,mutual_coefficients[3]]
  mutual_coef_trend[t,] <- c(quantile(effect.t,prob=c(0.5, 0.025,.975)))
}

pdf("./images/mutual_coef_trend_quadratic.pdf",width=6,height=3.5)
par(las=1,mar=c(4,4,1,1))
plot(sender_years,mutual_coef_trend[,1],type="n",ylab="Effect",xlab="Year", main="Mutual" ,ylim=c(-50, 50))
polygon(c(sender_years,rev(sender_years)),c(mutual_coef_trend[,2],rev(mutual_coef_trend[,3])),col="lightblue",border=NA)
lines(sender_years,mutual_coef_trend[,1],lwd=2,col="grey50")
abline(h=0,lty=2,lwd=2)
dev.off()


# trend CI plot for triangle coef
triangle_coef_trend <- matrix(NA,length(sender_times),3)
colnames(triangle_coef_trend) <- c("mean","lower","upper")
for(t in sender_times){
  effect.t <- bootstrapResults[,triangle_coefficients[1]]+t*bootstrapResults[,triangle_coefficients[2]]+t^2*bootstrapResults[,triangle_coefficients[3]]
  triangle_coef_trend[t,] <- c(mean(effect.t),quantile(effect.t,prob=c(0.025,.975)))
}

pdf("./images/triangle_coef_trend_quadratic.pdf",width=6,height=3.5)
par(las=1,mar=c(4,4,1,1))
plot(sender_years,triangle_coef_trend[,1],type="n",ylab="Effect",xlab="Year", main="Triangle" ,ylim=c(min(c(triangle_coef_trend)),max(c(triangle_coef_trend))))
polygon(c(sender_years,rev(sender_years)),c(triangle_coef_trend[,2],rev(triangle_coef_trend[,3])),col="lightblue",border=NA)
lines(sender_years,triangle_coef_trend[,1],lwd=2,col="grey50")
abline(h=0,lty=2,lwd=2)
dev.off()


# trend CI plot for sameissuearea coef
sameissue_coef_trend <- matrix(NA,length(sender_times),3)
colnames(sameissue_coef_trend) <- c("mean","lower","upper")
for(t in sender_times){
  effect.t <- bootstrapResults[,sameissue_coefficients[1]]+t*bootstrapResults[,sameissue_coefficients[2]]+t^2*bootstrapResults[,sameissue_coefficients[3]]
  sameissue_coef_trend[t,] <- c(mean(effect.t),quantile(effect.t,prob=c(0.025,.975)))
}

pdf("./images/sameissue_coef_trend_quadratic.pdf",width=6,height=3.5)
par(las=1,mar=c(4,4,1,1))
plot(sender_years,sameissue_coef_trend[,1],type="n",ylab="Effect",xlab="Year", main="Same Issue Area" ,ylim=c(min(c(sameissue_coef_trend)),max(c(sameissue_coef_trend))))
polygon(c(sender_years,rev(sender_years)),c(sameissue_coef_trend[,2],rev(sameissue_coef_trend[,3])),col="lightblue",border=NA)
lines(sender_years,sameissue_coef_trend[,1],lwd=2,col="grey50")
abline(h=0,lty=2,lwd=2)
dev.off()


# trend CI plot for absdiffmq coef
absdiffmq_coef_trend <- matrix(NA,length(sender_times),3)
colnames(absdiffmq_coef_trend) <- c("mean","lower","upper")
for(t in sender_times){
  effect.t <- bootstrapResults[,absdiffmq_coefficients[1]]+t*bootstrapResults[,absdiffmq_coefficients[2]]+t^2*bootstrapResults[,absdiffmq_coefficients[3]]
  absdiffmq_coef_trend[t,] <- c(mean(effect.t),quantile(effect.t,prob=c(0.025,.975)))
}

pdf("./images/absdiffmq_coef_trend_quadratic.pdf",width=6,height=3.5)
par(las=1,mar=c(4,4,1,1))
plot(sender_years,absdiffmq_coef_trend[,1],type="n",ylab="Effect",xlab="Year", main="Absolute Difference in Martin Quinn Scores" ,ylim=c(min(c(absdiffmq_coef_trend)),max(c(absdiffmq_coef_trend))))
polygon(c(sender_years,rev(sender_years)),c(absdiffmq_coef_trend[,2],rev(absdiffmq_coef_trend[,3])),col="lightblue",border=NA)
lines(sender_years,absdiffmq_coef_trend[,1],lwd=2,col="grey50")
abline(h=0,lty=2,lwd=2)
dev.off()


# trend CI plot for justices that voted pro coef
numberjusticespro_coef_trend <- matrix(NA,length(sender_times),3)
colnames(numberjusticespro_coef_trend) <- c("mean","lower","upper")
for(t in sender_times){
  effect.t <- bootstrapResults[,numberjusticespro_coefficients[1]]+t*bootstrapResults[,numberjusticespro_coefficients[2]]+t^2*bootstrapResults[,numberjusticespro_coefficients[3]]
  numberjusticespro_coef_trend[t,] <- c(mean(effect.t),quantile(effect.t,prob=c(0.025,.975)))
}

pdf("./images/numberjusticespro_coef_trend_quadratic.pdf",width=6,height=3.5)
par(las=1,mar=c(4,4,1,1))
plot(sender_years,numberjusticespro_coef_trend[,1],type="n",ylab="Effect",xlab="Year", main="Number of Justices in Majority" ,ylim=c(min(c(numberjusticespro_coef_trend)),max(c(numberjusticespro_coef_trend))))
polygon(c(sender_years,rev(sender_years)),c(numberjusticespro_coef_trend[,2],rev(numberjusticespro_coef_trend[,3])),col="lightblue",border=NA)
lines(sender_years,numberjusticespro_coef_trend[,1],lwd=2,col="grey50")
abline(h=0,lty=2,lwd=2)
dev.off()

# trend CI plot for overruled cases coef
overruled_coef_trend <- matrix(NA,length(sender_times),3)
colnames(overruled_coef_trend) <- c("mean","lower","upper")
for(t in sender_times){
  effect.t <- bootstrapResults[,overruled_coefficients[1]]+t*bootstrapResults[,overruled_coefficients[2]]+t^2*bootstrapResults[,overruled_coefficients[3]]
  overruled_coef_trend[t,] <- c(mean(effect.t),quantile(effect.t,prob=c(0.025,.975)))
}

pdf("./images/overruled_coef_trend_quadratic.pdf",width=6,height=3.5)
par(las=1,mar=c(4,4,1,1))
plot(sender_years,overruled_coef_trend[,1],type="n",ylab="Effect",xlab="Year", main="Overruled Cases" ,ylim=c(min(c(overruled_coef_trend)),max(c(overruled_coef_trend))))
polygon(c(sender_years,rev(sender_years)),c(overruled_coef_trend[,2],rev(overruled_coef_trend[,3])),col="lightblue",border=NA)
lines(sender_years,overruled_coef_trend[,1],lwd=2,col="grey50")
abline(h=0,lty=2,lwd=2)
dev.off()

# trend CI plot for year diff coef
year.diff_coef_trend <- matrix(NA,length(sender_times),3)
colnames(year.diff_coef_trend) <- c("mean","lower","upper")
for(t in sender_times){
  effect.t <- bootstrapResults[,year.diff_coefficients[1]]+t*bootstrapResults[,year.diff_coefficients[2]]+t^2*bootstrapResults[,year.diff_coefficients[3]]
  year.diff_coef_trend[t,] <- c(mean(effect.t),quantile(effect.t,prob=c(0.025,.975)))
}

pdf("./images/yeardiff_coef_trend_quadratic.pdf",width=6,height=3.5)
par(las=1,mar=c(4,4,1,1))
plot(sender_years,year.diff_coef_trend[,1],type="n",ylab="Effect",xlab="Year", main="Absolute Difference in Years between Cases" ,ylim=c(min(c(year.diff_coef_trend)),max(c(year.diff_coef_trend))))
polygon(c(sender_years,rev(sender_years)),c(year.diff_coef_trend[,2],rev(year.diff_coef_trend[,3])),col="lightblue",border=NA)
lines(sender_years,year.diff_coef_trend[,1],lwd=2,col="grey50")
abline(h=0,lty=2,lwd=2)
dev.off()

# trend CI plot for year diff square coef
year.diff.square_coef_trend <- matrix(NA,length(sender_times),3)
colnames(year.diff.square_coef_trend) <- c("mean","lower","upper")
for(t in sender_times){
  effect.t <- bootstrapResults[,year.diff.square_coefficients[1]]+t*bootstrapResults[,year.diff.square_coefficients[2]]+t^2*bootstrapResults[,year.diff.square_coefficients[3]]
  year.diff.square_coef_trend[t,] <- c(mean(effect.t),quantile(effect.t,prob=c(0.025,.975)))
}

pdf("./images/yeardiffsquare_coef_trend_quadratic.pdf",width=6,height=3.5)
par(las=1,mar=c(4,4,1,1))
plot(sender_years,year.diff.square_coef_trend[,1],type="n",ylab="Effect",xlab="Year", main="Years Difference Square between Cases" ,ylim=c(min(c(year.diff.square_coef_trend)),max(c(year.diff.square_coef_trend))))
polygon(c(sender_years,rev(sender_years)),c(year.diff.square_coef_trend[,2],rev(year.diff.square_coef_trend[,3])),col="lightblue",border=NA)
lines(sender_years,year.diff.square_coef_trend[,1],lwd=2,col="grey50")
abline(h=0,lty=2,lwd=2)
dev.off()

#####################################################
### degree 4 polynomials

# code assumes that working directory is set
# to top-level project
bootstrapResults <- read.csv("bootresults_4.csv",stringsAsFactors=F,row.names=1)

d<- c("edges","istar2","ostar2","mutual","triangle", "edgecov.mq.t", "edgecov.same.issue.area.t", "edgecov.year.diff.t" ,"edgecov.year.diff.square.t" ,  
      "nodeicov.AbsDiffMQscores", "nodeicov.NumberJusticesPro", "edgecov.year" , "nodeifactor.SameIssueArea.2", "nodeifactor.SameIssueArea.3",
      "nodeifactor.SameIssueArea.4","nodeifactor.SameIssueArea.5", "nodeifactor.SameIssueArea.6", "nodeifactor.SameIssueArea.7", 
      "nodeifactor.SameIssueArea.8","nodeifactor.SameIssueArea.9",  "nodeifactor.SameIssueArea.10",
      "nodeifactor.SameIssueArea.11", "nodeifactor.SameIssueArea.12", "nodeifactor.SameIssueArea.13","nodeifactor.SameIssueArea.14", 
      "nodeofactor.SameIssueArea.2", "nodeofactor.SameIssueArea.3", "nodeofactor.SameIssueArea.4","nodeofactor.SameIssueArea.5", 
      "nodeofactor.SameIssueArea.6","nodeofactor.SameIssueArea.7",
      "nodeofactor.SameIssueArea.8","nodeofactor.SameIssueArea.9","nodeofactor.SameIssueArea.10", "nodeofactor.SameIssueArea.11", 
      "nodeofactor.SameIssueArea.12", "nodeofactor.SameIssueArea.13","nodeofactor.SameIssueArea.14", "nodeicov.Overruled",
      "instar2_sendertime","outstar2_sendertime", "mutual_sendertime", "triangle_sendertime","mq_sendertime" ,"sameissuearea_sendertime",
      "yeardiff_sendertime" , "yeardiffsquare_sendertime", "AbsDiffMQscores_sendertime" , "NumberJusticesPro_sendertime", "Overruled_sendertime",
      "instar2_sendertime2","outstar2_sendertime2", "mutual_sendertime2", "triangle_sendertime2","mq_sendertime2" ,"sameissuearea_sendertime2",
      "yeardiff_sendertime2" , "yeardiffsquare_sendertime2", "AbsDiffMQscores_sendertime2" , "NumberJusticesPro_sendertime2", "Overruled_sendertime2",
      "instar2_sendertime3","outstar2_sendertime3", "mutual_sendertime3", "triangle_sendertime3","mq_sendertime3" ,"sameissuearea_sendertime3",
      "yeardiff_sendertime3" , "yeardiffsquare_sendertime3", "AbsDiffMQscores_sendertime3" , "NumberJusticesPro_sendertime3", "Overruled_sendertime3",
      "instar2_sendertime4","outstar2_sendertime4", "mutual_sendertime4", "triangle_sendertime4","mq_sendertime4" ,"sameissuearea_sendertime4",
      "yeardiff_sendertime4" , "yeardiffsquare_sendertime4", "AbsDiffMQscores_sendertime4" , "NumberJusticesPro_sendertime4", "Overruled_sendertime4",
      "edgecov.year2", "edgecov.year3", "edgecov.year4")
colnames(bootstrapResults)<- d




# sender times
sender_times <- 1:79
sender_years <- 1937:2015

density_coefficients <- c("edges","edgecov.year","edgecov.year2","edgecov.year3","edgecov.year4")
i2star_coefficients <- c("istar2","instar2_sendertime","instar2_sendertime2","instar2_sendertime3","instar2_sendertime4")
o2star_coefficients <- c("ostar2","outstar2_sendertime","outstar2_sendertime2","outstar2_sendertime3", "outstar2_sendertime4")
mutual_coefficients <- c("mutual","mutual_sendertime","mutual_sendertime2","mutual_sendertime3","mutual_sendertime4")
triangle_coefficients <- c("triangle","triangle_sendertime","triangle_sendertime2","triangle_sendertime3", "triangle_sendertime4")
mq_coefficients <- c("edgecov.mq.t","mq_sendertime","mq_sendertime2","mq_sendertime3", "mq_sendertime4")
sameissue_coefficients <- c("edgecov.same.issue.area.t","sameissuearea_sendertime","sameissuearea_sendertime2","sameissuearea_sendertime3","sameissuearea_sendertime4")
absdiffmq_coefficients <- c("nodeicov.AbsDiffMQscores","AbsDiffMQscores_sendertime","AbsDiffMQscores_sendertime2","AbsDiffMQscores_sendertime3","AbsDiffMQscores_sendertime4" )
numberjusticespro_coefficients <- c("nodeicov.NumberJusticesPro","NumberJusticesPro_sendertime","NumberJusticesPro_sendertime2","NumberJusticesPro_sendertime3", "NumberJusticesPro_sendertime4")
overruled_coefficients <- c("nodeicov.Overruled", "Overruled_sendertime", "Overruled_sendertime2", "Overruled_sendertime3", "Overruled_sendertime4")
year.diff_coefficients <- c("edgecov.year.diff.t", "yeardiff_sendertime", "yeardiff_sendertime2", "yeardiff_sendertime3", "yeardiff_sendertime4")
year.diff.square_coefficients <- c("edgecov.year.diff.square.t", "yeardiffsquare_sendertime", "yeardiffsquare_sendertime2", "yeardiffsquare_sendertime3", "yeardiffsquare_sendertime4" )



#### MQ is not significant
# example trend CI plot for MQ coef
mq_coef_trend <- matrix(NA,length(sender_times),3)
colnames(mq_coef_trend) <- c("mean","lower","upper")
for(t in sender_times){
  effect.t <- bootstrapResults[,mq_coefficients[1]]+t*bootstrapResults[,mq_coefficients[2]]+t^2*bootstrapResults[,mq_coefficients[3]]+t^3*bootstrapResults[,mq_coefficients[4]]+t^4*bootstrapResults[,mq_coefficients[5]]
  mq_coef_trend[t,] <- c(mean(effect.t),quantile(effect.t,prob=c(0.025,.975)))
}

pdf("./images/mq_coef_trend_quartic.pdf",width=6,height=3.5)
par(las=1,mar=c(4,4,1,1))
plot(sender_years,mq_coef_trend[,1],type="n",ylab="Effect",xlab="Year", main="Martin-Quinn Score" ,ylim=c(min(c(mq_coef_trend)),max(c(mq_coef_trend))))
polygon(c(sender_years,rev(sender_years)),c(mq_coef_trend[,2],rev(mq_coef_trend[,3])),col="lightblue",border=NA)
lines(sender_years,mq_coef_trend[,1],lwd=2,col="grey50")
abline(h=0,lty=2,lwd=2)
dev.off()

# trend CI plot for edges coef
edges_coef_trend <- matrix(NA,length(sender_times),3)
colnames(edges_coef_trend) <- c("mean","lower","upper")
for(t in sender_times){
  effect.t <- bootstrapResults[,density_coefficients[1]]+t*bootstrapResults[,density_coefficients[2]]+t^2*bootstrapResults[,density_coefficients[3]]+t^3*bootstrapResults[,density_coefficients[4]]+t^4*bootstrapResults[,density_coefficients[5]]
  edges_coef_trend[t,] <- c(mean(effect.t),quantile(effect.t,prob=c(0.025,.975)))
}

pdf("./images/edges_coef_trend_quartic.pdf",width=6,height=3.5)
par(las=1,mar=c(4,4,1,1))
plot(sender_years,edges_coef_trend[,1],type="n",ylab="Effect",xlab="Year", main="Edges" ,ylim=c(min(c(edges_coef_trend)),max(c(edges_coef_trend))))
polygon(c(sender_years,rev(sender_years)),c(edges_coef_trend[,2],rev(edges_coef_trend[,3])),col="lightblue",border=NA)
lines(sender_years,edges_coef_trend[,1],lwd=2,col="grey50")
abline(h=0,lty=2,lwd=2)
dev.off()


# trend CI plot for i2star coef
i2star_coef_trend <- matrix(NA,length(sender_times),3)
colnames(i2star_coef_trend) <- c("mean","lower","upper")
for(t in sender_times){
  effect.t <- bootstrapResults[,i2star_coefficients[1]]+t*bootstrapResults[,i2star_coefficients[2]]+(t^2)*bootstrapResults[,i2star_coefficients[3]]+(t^3)*bootstrapResults[,i2star_coefficients[4]]+(t^4)*bootstrapResults[,i2star_coefficients[5]]
  i2star_coef_trend[t,] <- c(mean(effect.t),quantile(effect.t,prob=c(0.025,.975)))
}

pdf("./images/i2star_coef_trend_quartic.pdf",width=6,height=3.5)
par(las=1,mar=c(4,4,1,1))
plot(sender_years,i2star_coef_trend[,1],type="n",ylab="Effect",xlab="Year", main="Instar(2)" ,ylim=c(min(c(i2star_coef_trend)),max(c(i2star_coef_trend))))
polygon(c(sender_years,rev(sender_years)),c(i2star_coef_trend[,2],rev(i2star_coef_trend[,3])),col="lightblue",border=NA)
lines(sender_years,i2star_coef_trend[,1],lwd=2,col="grey50")
abline(h=0,lty=2,lwd=2)
dev.off()


# trend CI plot for o2star coef
o2star_coef_trend <- matrix(NA,length(sender_times),3)
colnames(o2star_coef_trend) <- c("mean","lower","upper")
for(t in sender_times){
  effect.t <- bootstrapResults[,o2star_coefficients[1]]+t*bootstrapResults[,o2star_coefficients[2]]+t^2*bootstrapResults[,o2star_coefficients[3]]+t^3*bootstrapResults[,o2star_coefficients[4]]+t^4*bootstrapResults[,o2star_coefficients[5]]
  o2star_coef_trend[t,] <- c(mean(effect.t),quantile(effect.t,prob=c(0.025,.975)))
}

pdf("./images/o2star_coef_trend_quartic.pdf",width=6,height=3.5)
par(las=1,mar=c(4,4,1,1))
plot(sender_years,o2star_coef_trend[,1],type="n",ylab="Effect",xlab="Year", main="Outstar(2)" ,ylim=c(min(c(o2star_coef_trend)),max(c(o2star_coef_trend))))
polygon(c(sender_years,rev(sender_years)),c(o2star_coef_trend[,2],rev(o2star_coef_trend[,3])),col="lightblue",border=NA)
lines(sender_years,o2star_coef_trend[,1],lwd=2,col="grey50")
abline(h=0,lty=2,lwd=2)
dev.off()

# mutual not sig
# trend CI plot for mutual coef
mutual_coef_trend <- matrix(NA,length(sender_times),3)
colnames(mutual_coef_trend) <- c("mean","lower","upper")
for(t in sender_times){
  effect.t <- bootstrapResults[,mutual_coefficients[1]]+t*bootstrapResults[,mutual_coefficients[2]]+t^2*bootstrapResults[,mutual_coefficients[3]]+t^3*bootstrapResults[,mutual_coefficients[4]]+t^4*bootstrapResults[,mutual_coefficients[5]]
  mutual_coef_trend[t,] <- c(quantile(effect.t,prob=c(0.5, 0.025,.975)))
}

pdf("./images/mutual_coef_trend_quartic.pdf",width=6,height=3.5)
par(las=1,mar=c(4,4,1,1))
plot(sender_years,mutual_coef_trend[,1],type="n",ylab="Effect",xlab="Year", main="Mutual" ,ylim=c(min(c(mutual_coef_trend[,1])), max(c(mutual_coef_trend[,1]))))
polygon(c(sender_years,rev(sender_years)),c(mutual_coef_trend[,2],rev(mutual_coef_trend[,3])),col="lightblue",border=NA)
lines(sender_years,mutual_coef_trend[,1],lwd=2,col="grey50")
abline(h=0,lty=2,lwd=2)
dev.off()


# trend CI plot for triangle coef
triangle_coef_trend <- matrix(NA,length(sender_times),3)
colnames(triangle_coef_trend) <- c("mean","lower","upper")
for(t in sender_times){
  effect.t <- bootstrapResults[,triangle_coefficients[1]]+t*bootstrapResults[,triangle_coefficients[2]]+t^2*bootstrapResults[,triangle_coefficients[3]]+t^3*bootstrapResults[,triangle_coefficients[4]]+t^4*bootstrapResults[,triangle_coefficients[5]]
  triangle_coef_trend[t,] <- c(mean(effect.t),quantile(effect.t,prob=c(0.025,.975)))
}

pdf("./images/triangle_coef_trend_quartic.pdf",width=6,height=3.5)
par(las=1,mar=c(4,4,1,1))
plot(sender_years,triangle_coef_trend[,1],type="n",ylab="Effect",xlab="Year", main="Triangle" ,ylim=c(min(c(triangle_coef_trend)),max(c(triangle_coef_trend))))
polygon(c(sender_years,rev(sender_years)),c(triangle_coef_trend[,2],rev(triangle_coef_trend[,3])),col="lightblue",border=NA)
lines(sender_years,triangle_coef_trend[,1],lwd=2,col="grey50")
abline(h=0,lty=2,lwd=2)
dev.off()


# trend CI plot for sameissuearea coef
sameissue_coef_trend <- matrix(NA,length(sender_times),3)
colnames(sameissue_coef_trend) <- c("mean","lower","upper")
for(t in sender_times){
  effect.t <- bootstrapResults[,sameissue_coefficients[1]]+t*bootstrapResults[,sameissue_coefficients[2]]+t^2*bootstrapResults[,sameissue_coefficients[3]]+t^3*bootstrapResults[,sameissue_coefficients[4]]+t^4*bootstrapResults[,sameissue_coefficients[5]]
  sameissue_coef_trend[t,] <- c(mean(effect.t),quantile(effect.t,prob=c(0.025,.975)))
}

pdf("./images/sameissue_coef_trend_quartic.pdf",width=6,height=3.5)
par(las=1,mar=c(4,4,1,1))
plot(sender_years,sameissue_coef_trend[,1],type="n",ylab="Effect",xlab="Year", main="Same Issue Area" ,ylim=c(min(c(sameissue_coef_trend)),max(c(sameissue_coef_trend))))
polygon(c(sender_years,rev(sender_years)),c(sameissue_coef_trend[,2],rev(sameissue_coef_trend[,3])),col="lightblue",border=NA)
lines(sender_years,sameissue_coef_trend[,1],lwd=2,col="grey50")
abline(h=0,lty=2,lwd=2)
dev.off()


# trend CI plot for absdiffmq coef
absdiffmq_coef_trend <- matrix(NA,length(sender_times),3)
colnames(absdiffmq_coef_trend) <- c("mean","lower","upper")
for(t in sender_times){
  effect.t <- bootstrapResults[,absdiffmq_coefficients[1]]+t*bootstrapResults[,absdiffmq_coefficients[2]]+t^2*bootstrapResults[,absdiffmq_coefficients[3]]+t^3*bootstrapResults[,absdiffmq_coefficients[4]]+t^4*bootstrapResults[,absdiffmq_coefficients[5]]
  absdiffmq_coef_trend[t,] <- c(mean(effect.t),quantile(effect.t,prob=c(0.025,.975)))
}

pdf("./images/absdiffmq_coef_trend_quartic.pdf",width=6,height=3.5)
par(las=1,mar=c(4,4,1,1))
plot(sender_years,absdiffmq_coef_trend[,1],type="n",ylab="Effect",xlab="Year", main="Absolute Difference in Martin Quinn Scores" ,ylim=c(min(c(absdiffmq_coef_trend)),max(c(absdiffmq_coef_trend))))
polygon(c(sender_years,rev(sender_years)),c(absdiffmq_coef_trend[,2],rev(absdiffmq_coef_trend[,3])),col="lightblue",border=NA)
lines(sender_years,absdiffmq_coef_trend[,1],lwd=2,col="grey50")
abline(h=0,lty=2,lwd=2)
dev.off()


# trend CI plot for justices that voted pro coef
numberjusticespro_coef_trend <- matrix(NA,length(sender_times),3)
colnames(numberjusticespro_coef_trend) <- c("mean","lower","upper")
for(t in sender_times){
  effect.t <- bootstrapResults[,numberjusticespro_coefficients[1]]+t*bootstrapResults[,numberjusticespro_coefficients[2]]+t^2*bootstrapResults[,numberjusticespro_coefficients[3]]+t^3*bootstrapResults[,numberjusticespro_coefficients[4]]+t^4*bootstrapResults[,numberjusticespro_coefficients[5]]
  numberjusticespro_coef_trend[t,] <- c(mean(effect.t),quantile(effect.t,prob=c(0.025,.975)))
}

pdf("./images/numberjusticespro_coef_trend_quartic.pdf",width=6,height=3.5)
par(las=1,mar=c(4,4,1,1))
plot(sender_years,numberjusticespro_coef_trend[,1],type="n",ylab="Effect",xlab="Year", main="Number of Justices in Majority" ,ylim=c(min(c(numberjusticespro_coef_trend)),max(c(numberjusticespro_coef_trend))))
polygon(c(sender_years,rev(sender_years)),c(numberjusticespro_coef_trend[,2],rev(numberjusticespro_coef_trend[,3])),col="lightblue",border=NA)
lines(sender_years,numberjusticespro_coef_trend[,1],lwd=2,col="grey50")
abline(h=0,lty=2,lwd=2)
dev.off()

# trend CI plot for overruled cases coef
overruled_coef_trend <- matrix(NA,length(sender_times),3)
colnames(overruled_coef_trend) <- c("mean","lower","upper")
for(t in sender_times){
  effect.t <- bootstrapResults[,overruled_coefficients[1]]+t*bootstrapResults[,overruled_coefficients[2]]+t^2*bootstrapResults[,overruled_coefficients[3]]+t^3*bootstrapResults[,overruled_coefficients[4]]+t^4*bootstrapResults[,overruled_coefficients[5]]
  overruled_coef_trend[t,] <- c(mean(effect.t),quantile(effect.t,prob=c(0.025,.975)))
}

pdf("./images/overruled_coef_trend_quartic.pdf",width=6,height=3.5)
par(las=1,mar=c(4,4,1,1))
plot(sender_years,overruled_coef_trend[,1],type="n",ylab="Effect",xlab="Year", main="Overruled Cases" ,ylim=c(min(c(overruled_coef_trend)),max(c(overruled_coef_trend))))
polygon(c(sender_years,rev(sender_years)),c(overruled_coef_trend[,2],rev(overruled_coef_trend[,3])),col="lightblue",border=NA)
lines(sender_years,overruled_coef_trend[,1],lwd=2,col="grey50")
abline(h=0,lty=2,lwd=2)
dev.off()


# trend CI plot for year diff coef
year.diff_coef_trend <- matrix(NA,length(sender_times),3)
colnames(year.diff_coef_trend) <- c("mean","lower","upper")
for(t in sender_times){
  effect.t <- bootstrapResults[,year.diff_coefficients[1]]+t*bootstrapResults[,year.diff_coefficients[2]]+t^2*bootstrapResults[,year.diff_coefficients[3]]+t^3*bootstrapResults[,year.diff_coefficients[4]]+t^4*bootstrapResults[,year.diff_coefficients[5]]
  year.diff_coef_trend[t,] <- c(mean(effect.t),quantile(effect.t,prob=c(0.025,.975)))
}

pdf("./images/yeardiff_coef_trend_quartic.pdf",width=6,height=3.5)
par(las=1,mar=c(4,4,1,1))
plot(sender_years,year.diff_coef_trend[,1],type="n",ylab="Effect",xlab="Year", main="Absolute Difference in Years between Cases" ,ylim=c(min(c(year.diff_coef_trend)),max(c(year.diff_coef_trend))))
polygon(c(sender_years,rev(sender_years)),c(year.diff_coef_trend[,2],rev(year.diff_coef_trend[,3])),col="lightblue",border=NA)
lines(sender_years,year.diff_coef_trend[,1],lwd=2,col="grey50")
abline(h=0,lty=2,lwd=2)
dev.off()

# trend CI plot for year diff square coef
year.diff.square_coef_trend <- matrix(NA,length(sender_times),3)
colnames(year.diff.square_coef_trend) <- c("mean","lower","upper")
for(t in sender_times){
  effect.t <- bootstrapResults[,year.diff.square_coefficients[1]]+t*bootstrapResults[,year.diff.square_coefficients[2]]+t^2*bootstrapResults[,year.diff.square_coefficients[3]]+t^3*bootstrapResults[,year.diff.square_coefficients[4]]+t^4*bootstrapResults[,year.diff.square_coefficients[5]]
  year.diff.square_coef_trend[t,] <- c(mean(effect.t),quantile(effect.t,prob=c(0.025,.975)))
}

pdf("./images/yeardiffsquare_coef_trend_quartic.pdf",width=6,height=3.5)
par(las=1,mar=c(4,4,1,1))
plot(sender_years,year.diff.square_coef_trend[,1],type="n",ylab="Effect",xlab="Year", main="Years Difference Square between Cases" ,ylim=c(min(c(year.diff.square_coef_trend)),max(c(year.diff.square_coef_trend))))
polygon(c(sender_years,rev(sender_years)),c(year.diff.square_coef_trend[,2],rev(year.diff.square_coef_trend[,3])),col="lightblue",border=NA)
lines(sender_years,year.diff.square_coef_trend[,1],lwd=2,col="grey50")
abline(h=0,lty=2,lwd=2)
dev.off()

