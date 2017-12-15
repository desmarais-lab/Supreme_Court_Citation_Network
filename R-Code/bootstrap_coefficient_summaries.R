# code assumes that working directory is set
# to top-level project
bootstrapResults <- read.csv("bootresults.csv",stringsAsFactors=F,row.names=1)

# Christian, can you correct this? The sender time values should be set to map onto the range
# of sender times in our data
sender_times <- 1:65
sender_years <- 1937:2001

density_coefficients <- c("edges","nodeocov.sender.time")
i2star_coefficients <- c("istar2","instar2_sendertime")
o2star_coefficients <- c("ostar2","outstar2_sendertime")
mutual_coefficients <- c("mutual","mutual_sendertime")
triangle_coefficients <- c("triangle","triangle_sendertime")
mq_coefficients <- c("edgecov.mq.t","mq_sendertime")
sameissue_coefficients <- c("edgecov.same.issue.area.t","sameissuearea_sendertime")
absdiffmq_coefficients <- c("nodeicov.AbsDiffMQscores","AbsDiffMQscores_sendertime")
numberjusticespro_coefficients <- c("nodeicov.NumberJusticesPro","NumberJusticesPro_sendertime")

# example trend CI plot for MQ coef
mq_coef_trend <- matrix(NA,length(sender_times),3)
colnames(mq_coef_trend) <- c("mean","lower","upper")
for(t in sender_times){
  effect.t <- bootstrapResults[,mq_coefficients[1]]+t*bootstrapResults[,mq_coefficients[2]]
  mq_coef_trend[t,] <- c(mean(effect.t),quantile(effect.t,prob=c(0.025,.975)))
}

pdf("./images/mq_coef_trend.pdf",width=6,height=3.5)
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

pdf("./images/edges_coef_trend.pdf",width=6,height=3.5)
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

pdf("./images/i2star_coef_trend.pdf",width=6,height=3.5)
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

pdf("./images/o2star_coef_trend.pdf",width=6,height=3.5)
par(las=1,mar=c(4,4,1,1))
plot(sender_years,o2star_coef_trend[,1],type="n",ylab="Effect",xlab="Year", main="Outstar(2)" ,ylim=c(min(c(o2star_coef_trend)),max(c(o2star_coef_trend))))
polygon(c(sender_years,rev(sender_years)),c(o2star_coef_trend[,2],rev(o2star_coef_trend[,3])),col="lightblue",border=NA)
lines(sender_years,o2star_coef_trend[,1],lwd=2,col="grey50")
abline(h=0,lty=2,lwd=2)
dev.off()

# trend CI plot for mutual coef
mutual_coef_trend <- matrix(NA,length(sender_times),3)
colnames(mutual_coef_trend) <- c("mean","lower","upper")
for(t in sender_times){
  effect.t <- bootstrapResults[,mutual_coefficients[1]]+t*bootstrapResults[,mutual_coefficients[2]]
  mutual_coef_trend[t,] <- c(mean(effect.t),quantile(effect.t,prob=c(0.025,.975)))
}

pdf("./images/mutual_coef_trend.pdf",width=6,height=3.5)
par(las=1,mar=c(4,4,1,1))
plot(sender_years,mutual_coef_trend[,1],type="n",ylab="Effect",xlab="Year", main="Mutual" ,ylim=c(min(c(mutual_coef_trend)),max(c(mutual_coef_trend))))
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

pdf("./images/triangle_coef_trend.pdf",width=6,height=3.5)
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

pdf("./images/sameissue_coef_trend.pdf",width=6,height=3.5)
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

pdf("./images/absdiffmq_coef_trend.pdf",width=6,height=3.5)
par(las=1,mar=c(4,4,1,1))
plot(sender_years,absdiffmq_coef_trend[,1],type="n",ylab="Effect",xlab="Year", main="Absolute Difference in Martin Quinn Scores" ,ylim=c(min(c(absdiffmq_coef_trend)),max(c(absdiffmq_coef_trend))))
polygon(c(sender_years,rev(sender_years)),c(absdiffmq_coef_trend[,2],rev(absdiffmq_coef_trend[,3])),col="lightblue",border=NA)
lines(sender_years,absdiffmq_coef_trend[,1],lwd=2,col="grey50")
abline(h=0,lty=2,lwd=2)
dev.off()


# trend CI plot for absdiffmq coef
numberjusticespro_coef_trend <- matrix(NA,length(sender_times),3)
colnames(numberjusticespro_coef_trend) <- c("mean","lower","upper")
for(t in sender_times){
  effect.t <- bootstrapResults[,numberjusticespro_coefficients[1]]+t*bootstrapResults[,numberjusticespro_coefficients[2]]
  numberjusticespro_coef_trend[t,] <- c(mean(effect.t),quantile(effect.t,prob=c(0.025,.975)))
}

pdf("./images/numberjusticespro_coef_trend.pdf",width=6,height=3.5)
par(las=1,mar=c(4,4,1,1))
plot(sender_years,numberjusticespro_coef_trend[,1],type="n",ylab="Effect",xlab="Year", main="Number of Justices in Majority" ,ylim=c(min(c(numberjusticespro_coef_trend)),max(c(numberjusticespro_coef_trend))))
polygon(c(sender_years,rev(sender_years)),c(numberjusticespro_coef_trend[,2],rev(numberjusticespro_coef_trend[,3])),col="lightblue",border=NA)
lines(sender_years,numberjusticespro_coef_trend[,1],lwd=2,col="grey50")
abline(h=0,lty=2,lwd=2)
dev.off()
