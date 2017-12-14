# code assumes that working directory is set
# to top-level project
bootstrapResults <- read.csv("./R-Code/bootresults.csv",stringsAsFactors=F,row.names=1)

# Christian, can you correct this? The sender time values should be set to map onto the range
# of sender times in our data
sender_times <- 1:60
sender_years <- 1941:2000

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

pdf("./Tex/images/mq_coef_trend.pdf",width=6,height=3.5)
par(las=1,mar=c(4,4,1,1))
plot(sender_years,mq_coef_trend[,1],type="n",ylab="effect",xlab="year",ylim=c(min(c(mq_coef_trend)),max(c(mq_coef_trend))))
polygon(c(sender_years,rev(sender_years)),c(mq_coef_trend[,2],rev(mq_coef_trend[,3])),col="lightblue",border=NA)
lines(sender_years,mq_coef_trend[,1],lwd=2,col="grey50")
abline(h=0,lty=2,lwd=2)
dev.off()




