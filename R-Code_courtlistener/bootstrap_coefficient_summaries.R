
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


bootstrapResults <- read.csv("bootresults_1_parallel10.csv",stringsAsFactors=F,row.names=1)

rownames(bootstrapResults)[1]<- "edges"

bootstrapResults<- t(bootstrapResults)

#delete strange values
bootstrapResults<- bootstrapResults[-37,]
bootstrapResults<- bootstrapResults[-49,]


# sender times
sender_times <- 1:79
sender_years <- 1937:2015

density_coefficients <- c("edges","edgecov.year.t")
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
justice.homophily <- c("edgecov.same.opinion.writer.t", "jusitcehomophily_sendertime")

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
  effect.t <- t*bootstrapResults[,sameissue_coefficients[2]]#bootstrapResults[,sameissue_coefficients[1]]+
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

# trend CI plot for justice homophily coef
justice.homophily_coef_trend <- matrix(NA,length(sender_times),3)
colnames(justice.homophily_coef_trend) <- c("mean","lower","upper")
for(t in sender_times){
  effect.t <- bootstrapResults[,justice.homophily[1]]+t*bootstrapResults[,justice.homophily[2]]
  justice.homophily_coef_trend[t,] <- c(mean(effect.t),quantile(effect.t,prob=c(0.025,.975)))
}

pdf("./images/justicehomophily_coef_trend_linear.pdf",width=6,height=3.5)
par(las=1,mar=c(4,4,1,1))
plot(sender_years,justice.homophily_coef_trend[,1],type="n",ylab="Effect",xlab="Year", main="Justice Homophily" ,ylim=c(min(c(justice.homophily_coef_trend)),max(c(justice.homophily_coef_trend))))
polygon(c(sender_years,rev(sender_years)),c(justice.homophily_coef_trend[,2],rev(justice.homophily_coef_trend[,3])),col="lightblue",border=NA)
lines(sender_years,justice.homophily_coef_trend[,1],lwd=2,col="grey50")
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


###########################################
###########################################
### nodemix Issue area heat

library("lattice")


nodemix.values<- c(37.34, 1.49, 4.18, 4.58, 4.76, 3.84, 3.46,3.4, 2.09, 3.26, 1.46, 2.91, 4.53, 1.74,
                   4, 39.5, 6.49, 6.78, 6.75, 7.03, 6.28, 6.21, 4.69, 6.39, 5.68, 5.5, 6.81, 5.53,
                   3.98, 3.8, 41.17, 6.44, 6.69, 6.26, 5.89, 5.92, 4.29,6.14, 4.83, 4.73, 7.43, 4.64,
                   4.55, 4.12, 6.61, 42.87, 6.73, 7.01, 5.96, 6.41, 4.74, 6.61, 6.52, 5.18, 7.42, 6.22,
                   -1.58, -1.41, 1.22, 1.26, 38.41, 1.86, 1.34, 1.19, -0.33, 0.87, 0.35, 0.63, 3.05, 1.02, 
                   4.12, 4.17, 6.28, 6.12, 5.15, 44.22, 6.36, 6.78, 5.17, 6.84, 1.75, 6.29, 7.79, 7.19,
                   -1.66, -1.21, 1.56, 1.03, 1.55, 2.02, 37.74, 1.21, -0.29, 1.76, -0.97, -0.13, 1.06, -0.66, 
                   0.23, 0.36, 2.71, 3.25, 3.62, 3.66, 3.11, 38.48, 1.47, 3.46, 2.73, 2.34, 3.71, 2.62, 
                   0.22,0.23, 2.53, 2.94, 3.58, 3.19, 2.88, 2.69, 36.5, 3.03, 2.43, 1.63, 3.78, 2.4,
                   3.58, 3.92, 6.53, 6.67, 7.14, 6.92, 6.97, 6.79, 4.93, 42.66, 6.79, 5.74, 5.59, 6.7,
                   1.7, 3.41, 3.55, 7.17, 6.03, 6.97, 3.6, 6.05, 4.44, 6.90, 43.88, 3.66, 1.97, 7.04,
                   3.11, 3.01, 5.36, 5.77, 6.26, 6.65, 5.37, 5.83, 3.86, 5.84, 4.64, 42.45,6, 4.9, 
                   4.74, 4.87, 7.52, 7.57, 7.16, 7.65, 7.14, 7.29, 5.81, 7.81, 7.21, 6.67, 43.16, 1.79,
                   -37.29, -37.52, -35, -33, -37, -32, -38, -34, -35, -34, -38, -35, NA, NA)

## Example data
data=matrix(nodemix.values, 14, 14)
colnames(data)=1:14
rownames(data)=1:14


## Try it out
par(mar=c(3,4,2,2))
levelplot(data[c(nrow(data):1) , ], xlab="Receiver", ylab="Sender")


###################### sender receiver


ijustice <-bootstrapResults[, 41:84] 

ijusticenode <- matrix(NA,44,3)
colnames(ijusticenode) <- c("mean","lower","upper")
for(i in 1:44){
effect.t <- ijustice[,i]
  ijusticenode[i,]<- c(mean(effect.t),quantile(effect.t,prob=c(0.025,.975)))
}
  


x <- matrix(0, 44,3)


  x[1,]<-      c(0.7872,  ijusticenode[1,2:3])  
  x[2,]<-      c(0.7171   , ijusticenode[2,2:3]  )
  x[3,]<-      c(0.2557 ,   ijusticenode[3,2:3]  )
  x[4,]<-      c(0.5905 ,   ijusticenode[4,2:3]  )
  x[5,]<-     c(0.6381  ,  ijusticenode[5,2:3]  )
  x[6,]<-      c(0.3938 ,   ijusticenode[6,2:3] )
  x[7,]<-      c(0.1392 ,  ijusticenode[7,2:3]  )
  x[8,]<-     c(0.5912  ,  ijusticenode[8,2:3]  )
  x[9,]<-      c(0.3833 ,   ijusticenode[9,2:3] )
  x[10,]<-      c(0.7761  ,  ijusticenode[10,2:3]  )
  x[11,]<-      c(0.5703  ,  ijusticenode[11,2:3]  )
  x[12,]<-      c(0.6146  ,  ijusticenode[12,2:3]  )
  x[13,]<-      c(0.5369  ,  ijusticenode[13,2:3]  )
  x[14,]<-      c(0.9113  ,  ijusticenode[14,2:3]  )
  x[15,]<-      c(0.7109  ,  ijusticenode[15,2:3]  )
  x[16,]<-      c(0.5862  ,  ijusticenode[16,2:3]  )
  x[17,]<-      c(0.6527  ,  ijusticenode[17,2:3])
  x[18,]<-      c(0.5542 ,   ijusticenode[18,2:3]  )
  x[19,]<-     c(0.4767 ,   ijusticenode[19,2:3]  )
  x[20,]<-      c(0.8694  ,  ijusticenode[20,2:3]  )
  x[21,]<-     c(0.7601  ,  ijusticenode[21,2:3] )
  x[22,]<-    c(0.6900  ,  ijusticenode[22,2:3]  )
  x[23,]<-    c(  0.5065  ,  ijusticenode[23,2:3] ) 
x[24,]<-      c(0.5693 ,   ijusticenode[24,2:3]  )
x[25,]<-     c(0.7982 ,   ijusticenode[25,2:3] )
x[26,]<-    c(  0.7177  ,  ijusticenode[26,2:3]  )
x[27,]<-    c(  0.9345   , ijusticenode[27,2:3]  )
x[28,]<-      c(0.9059   , ijusticenode[28,2:3]  )
x[29,]<-      c(0.7818  ,  ijusticenode[29,2:3]  )
x[30,]<-     c(0.7949  ,  ijusticenode[30,2:3] )
x[31,]<-     c(0.7519 ,   ijusticenode[31,2:3]  )
x[32,]<-     c(0.8137 , ijusticenode[32,2:3] )
x[33,]<-    c(0.8178   , ijusticenode[33,2:3]  )
x[34,]<-     c(0.9563 ,   ijusticenode[34,2:3])
x[35,]<-     c(0.9902 ,   ijusticenode[35,2:3] )
x[36,]<-    c(0.8558   , ijusticenode[36,2:3]  )
x[37,]<-    c(0.8818  ,  ijusticenode[37,2:3]  )
x[38,]<-   c(0.7393  ,  ijusticenode[38,2:3] )
x[39,]<-   c(0.4711 ,   ijusticenode[39,2:3] )
x[40,]<-    c(0.2139   , ijusticenode[40,2:3])  
x[41,]<-   c(-0.4469  , ijusticenode[41,2:3]  )
x[42,]<-    c(-0.1825,   ijusticenode[42,2:3]  )
x[43,]<-   c(-0.7860,   ijusticenode[43,2:3]  )
x[44,]<-    c(-1.1904 ,  ijusticenode[44,2:3]) 




### plot dotchart with confidence intervals


justices <- c("JHClarke", "PButler", "ETSandford", "CEHughes", "OJRoberts", "BNCardozo", "HLBlack", "SFReed", "FFrankfurter", "WODouglas", "FMurphy", "JFBrynes", "RHJackson", "WBRutledge", "HHBurton",
            "FMVinson", "TCClark", "SMinton", "EWarren", "JHarlan", "WJBrennan", "CEWhittaker", "PStewart", "BRWhite", "AJGoldberg", "AFortas", "TMArshall",
            "WEBurger", "HABlackmun", "LFPowell", "WHRehnquist", "JPStevens", "SDOConnor", "AScalia", "AMKennedy", "DHSouter", "CThomas", "RBGinsburg", "SGBreyer", "JGRoberts", 
            "SAAlito","SSotomayor", "EKagan", "NGorsuch")

data<-data.frame(ID=justices, mean=x[,1], lb=x[,2], ub=x[,3]) # enter judge names

# Create the customized panel function
mypanel.Dotplot <- function(x, y, ...) {
  panel.Dotplot(x,y,...)
  tips <- attr(x, "other")
  panel.arrows(x0 = tips[,1], y0 = y, 
               x1 = tips[,2], y1 = y, 
               length = 0.05, unit = "native",
               angle = 90, code = 3)
}

library(Hmisc)
Dotplot(data$ID ~ Cbind(data$mean,data$lb,data$ub), col="blue", pch=20, panel = mypanel.Dotplot,
        xlab="Coefficient",ylab="Jusitce", main="Justice Popularity")



###################### sender receiver


ojustice <-bootstrapResults[, 85:128] 

ojusticenode <- matrix(NA,44,3)
colnames(ojusticenode) <- c("mean","lower","upper")
for(i in 1:44){
  effect.t <- ojustice[,i]
  ojusticenode[i,]<- c(mean(effect.t),quantile(effect.t,prob=c(0.025,.975)))
}



x <- matrix(0, 44,3)


x[1,]<-      c(0.2494,  ojusticenode[1,2:3])   #68
x[2,]<-      c(-3.3493   , ojusticenode[2,2:3]  )#71
x[3,]<-      c( -0.3269,   ojusticenode[3,2:3]  )#72
x[4,]<-      c(0.1367 ,   ojusticenode[4,2:3]  )#74
x[5,]<-     c( -0.0625,  ojusticenode[5,2:3]  )#75
x[6,]<-      c(-0.5103 ,   ojusticenode[6,2:3] )#76
x[7,]<-      c( -3.0773  ,  ojusticenode[7,2:3]  )
x[8,]<-     c( 0.2134 ,  ojusticenode[8,2:3]  )
x[9,]<-      c(0.3234,   ojusticenode[9,2:3] )
x[10,]<-      c( 0.3050 ,  ojusticenode[10,2:3]  )
x[11,]<-      c(   0.3189,  ojusticenode[11,2:3]  )
x[12,]<-      c( -0.2662 ,  ojusticenode[12,2:3]  )
x[13,]<-      c(  -0.5120,  ojusticenode[13,2:3]  )
x[14,]<-      c(  0.1571,  ojusticenode[14,2:3]  )
x[15,]<-      c(0.1652  ,  ojusticenode[15,2:3]  )
x[16,]<-      c( 0.4715 ,  ojusticenode[16,2:3]  )
x[17,]<-      c( 0.4205 ,  ojusticenode[17,2:3])
x[18,]<-      c(0.4612 ,   ojusticenode[18,2:3]  )
x[19,]<-     c(0.2888  ,   ojusticenode[19,2:3]  )
x[20,]<-      c( 0.7837  ,  ojusticenode[20,2:3]  )
x[21,]<-     c(  0.7181,  ojusticenode[21,2:3] )
x[22,]<-    c( 0.5081 ,  ojusticenode[22,2:3]  )
x[23,]<-    c(   0.5736,  ojusticenode[23,2:3] ) 
x[24,]<-      c(  0.4816,   ojusticenode[24,2:3]  )
x[25,]<-     c(0.4374  ,   ojusticenode[25,2:3] )
x[26,]<-    c( 0.4607  ,  ojusticenode[26,2:3]  )
x[27,]<-    c(    0.5833  , ojusticenode[27,2:3]  )
x[28,]<-      c(   0.6176 , ojusticenode[28,2:3]  )
x[29,]<-      c( 0.5154 ,  ojusticenode[29,2:3]  )
x[30,]<-     c( 0.5772  ,  ojusticenode[30,2:3] )
x[31,]<-     c( 0.4530 ,   ojusticenode[31,2:3]  )
x[32,]<-     c(0.4098  , ojusticenode[32,2:3] )
x[33,]<-    c( 0.4148   , ojusticenode[33,2:3]  )
x[34,]<-     c(0.4424,   ojusticenode[34,2:3])
x[35,]<-     c(0.4877  ,   ojusticenode[35,2:3] )
x[36,]<-    c(  0.4128 , ojusticenode[36,2:3]  )
x[37,]<-    c( 0.5618  ,  ojusticenode[37,2:3]  )
x[38,]<-   c( 0.3879 ,  ojusticenode[38,2:3] )
x[39,]<-   c( 0.5155,   ojusticenode[39,2:3] )
x[40,]<-    c( 0.4481  , ojusticenode[40,2:3])  
x[41,]<-   c( 0.6139, ojusticenode[41,2:3]  )
x[42,]<-    c(0.5001,   ojusticenode[42,2:3]  )
x[43,]<-   c(0.6248,   ojusticenode[43,2:3]  )
x[44,]<-    c(0.6011 ,  ojusticenode[44,2:3]) 




### plot dotchart with confidence intervals


justices <- c("JHClarke", "PButler", "ETSandford", "CEHughes", "OJRoberts", "BNCardozo", "HLBlack", "SFReed", "FFrankfurter", "WODouglas", "FMurphy", "JFBrynes", "RHJackson", "WBRutledge", "HHBurton",
              "FMVinson", "TCClark", "SMinton", "EWarren", "JHarlan", "WJBrennan", "CEWhittaker", "PStewart", "BRWhite", "AJGoldberg", "AFortas", "TMArshall",
              "WEBurger", "HABlackmun", "LFPowell", "WHRehnquist", "JPStevens", "SDOConnor", "AScalia", "AMKennedy", "DHSouter", "CThomas", "RBGinsburg", "SGBreyer", "JGRoberts", 
              "SAAlito","SSotomayor", "EKagan", "NGorsuch")

data<-data.frame(ID=justices, mean=x[,1], lb=x[,2], ub=x[,3]) # enter judge names

# Create the customized panel function
mypanel.Dotplot <- function(x, y, ...) {
  panel.Dotplot(x,y,...)
  tips <- attr(x, "other")
  panel.arrows(x0 = tips[,1], y0 = y, 
               x1 = tips[,2], y1 = y, 
               length = 0.05, unit = "native",
               angle = 90, code = 3)
}

library(Hmisc)
Dotplot(data$ID ~ Cbind(data$mean,data$lb,data$ub), col="blue", pch=20, panel = mypanel.Dotplot,
        xlab="Coefficient",ylab="Jusitce", main="Justice Activity")


########################################## same issue area popularity




iissuearea <-bootstrapResults[, 13:25] 

iissueareanode <- matrix(NA,13,3)
colnames(iissueareanode) <- c("mean","lower","upper")
for(i in 1:44){
  effect.t <- iissuearea[,i]
  iissueareanode[i,]<- c(mean(effect.t),quantile(effect.t,prob=c(0.025,.975)))
}



x <- matrix(0, 13,3)


x[1,]<-      c(0.2699,  iissueareanode[1,2:3])  
x[2,]<-      c(-2.5785   , iissueareanode[2,2:3]  )
x[3,]<-      c(-2.3688 ,   iissueareanode[3,2:3]  )
x[4,]<-      c(-2.9296 ,   iissueareanode[4,2:3]  )
x[5,]<-     c(-2.4707  ,  iissueareanode[5,2:3]  )
x[6,]<-      c(-2.2286,   iissueareanode[6,2:3] )
x[7,]<-      c(-2.1498  ,  iissueareanode[7,2:3]  )
x[8,]<-     c(-0.3300  ,  iissueareanode[8,2:3]  )
x[9,]<-      c(-2.2660,   iissueareanode[9,2:3] )
x[10,]<-      c(-2.0837   ,  iissueareanode[10,2:3]  )
x[11,]<-      c(-1.9949  ,  iissueareanode[11,2:3]  )
x[12,]<-      c(-2.5324   ,  iissueareanode[12,2:3]  )
x[13,]<-      c(-1.6377  ,  iissueareanode[13,2:3]  )


sia <- c("Civil Rights", "First Amendment", "Due Process", "Privacy","Attorneys", "Unions",
         "Economic Activity", "Judical Power", "Federalism", "Interstate Relations", "Federal Taxation",
         "Miscellaneous", "Private Action")

data<-data.frame(ID=sia, mean=x[,1], lb=x[,2], ub=x[,3]) # enter judge names


library(Hmisc)
Dotplot(data$ID ~ Cbind(data$mean,data$lb,data$ub), col="blue", pch=20, panel = mypanel.Dotplot,
        xlab="Coefficient",ylab="Issue Area", main="Issue Area Popularity")



##################### issue area activity





oissuearea <-bootstrapResults[, 26:38] 

oissueareanode <- matrix(NA,13,3)
colnames(oissueareanode) <- c("mean","lower","upper")
for(i in 1:13){
  effect.t <- oissuearea[,i]
  oissueareanode[i,]<- c(mean(effect.t),quantile(effect.t,prob=c(0.025,.975)))
}



x <- matrix(0, 13,3)


x[1,]<-      c(-2.2147,  oissueareanode[1,2:3])  
x[2,]<-      c(-2.1978   , oissueareanode[2,2:3]  )
x[3,]<-      c(-2.2944 ,   oissueareanode[3,2:3]  )
x[4,]<-      c(3.0416 ,   oissueareanode[4,2:3]  )
x[5,]<-     c(-2.4408 ,  oissueareanode[5,2:3]  )
x[6,]<-      c(2.7597,   oissueareanode[6,2:3] )
x[7,]<-      c(1.1015  ,  oissueareanode[7,2:3]  )
x[8,]<-     c(1.5760  ,  oissueareanode[8,2:3]  )
x[9,]<-      c(-2.2898,   oissueareanode[9,2:3] )
x[10,]<-      c(-2.0555  ,  oissueareanode[10,2:3]  )
x[11,]<-      c(-2.1473  ,  oissueareanode[11,2:3]  )
x[12,]<-      c(-2.5735   ,  oissueareanode[12,2:3]  )
x[13,]<-      c(38.9973  ,  oissueareanode[13,2:3]  )


sia <- c("Civil Rights", "First Amendment", "Due Process", "Privacy","Attorneys", "Unions",
         "Economic Activity", "Judical Power", "Federalism", "Interstate Relations", "Federal Taxation",
         "Miscellaneous", "Private Action")

data<-data.frame(ID=sia, mean=x[,1], lb=x[,2], ub=x[,3]) # enter judge names


library(Hmisc)
Dotplot(data$ID ~ Cbind(data$mean,data$lb,data$ub), col="blue", pch=20, panel = mypanel.Dotplot,
        xlab="Coefficient",ylab="Issue Area", main="Issue Area Activity", xlim=c(-28,45))
x[13,]
