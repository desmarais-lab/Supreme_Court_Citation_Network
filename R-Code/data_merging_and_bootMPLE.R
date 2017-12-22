# merge together csv data that was obtained from the aci.ics cluster
 
# rm(list= ls()[!(ls() %in% c('scc1','adjacency.matrix', 'mq.matrix', 'same.issue.area', 'year.diff.matrix'))])
memory.limit(30000)

library(readr)

mpledata1 <- read_csv("mpledata.csv")
mpledata2 <- read_csv("mpledata2.csv")
mpledata3 <- read_csv("mpledata3.csv")
mpledata4 <- read_csv("mpledata4.csv")
mpledata5 <- read_csv("mpledata5.csv")

mpledata1<- as.data.frame(mpledata1)
mpledata2<- as.data.frame(mpledata2)
mpledata3<- as.data.frame(mpledata3)
mpledata4<- as.data.frame(mpledata4)
mpledata5<- as.data.frame(mpledata5)

mpledata<- rbind(mpledata1, mpledata2, mpledata3, mpledata4, mpledata5)
mpledata<- mpledata[,-1]

rm(mpledata1, mpledata2, mpledata3, mpledata4, mpledata5)


# next, I create new columns to add interaction effects

mpledata$instar2_sendertime<- mpledata[,3]*mpledata[,14]
mpledata$mutual_sendertime<- mpledata[,4]*mpledata[,14]
mpledata$outstar2_sendertime<- mpledata[,5]*mpledata[,14]
mpledata$triangle_sendertime<- mpledata[,6]*mpledata[,14]
mpledata$mq_sendertime<- mpledata[,7]*mpledata[,14]
mpledata$sameissuearea_sendertime<- mpledata[,8]*mpledata[,14]
mpledata$yeardiff_sendertime<- mpledata[,9]*mpledata[,14]
mpledata$yeardiffsquare_sendertime<- mpledata[,10]*mpledata[,14]
mpledata$AbsDiffMQscores_sendertime<- mpledata[,16]*mpledata[,14]
mpledata$NumberJusticesPro_sendertime<- mpledata[,17]*mpledata[,14]




write.csv(mpledata, file = "mpledata_all.csv")

mple.est<- glm(edgeij ~ istar2+ostar2+mutual+triangle+ edgecov.mq.t+ edgecov.same.issue.area.t+ edgecov.year.diff.t + edgecov.year.diff.square.t +
      nodeicov.AbsDiffMQscores+ nodeicov.NumberJusticesPro+ nodeocov.sender.time + nodeifactor.SameIssueArea.2+ nodeifactor.SameIssueArea.3+ 
      nodeifactor.SameIssueArea.4+nodeifactor.SameIssueArea.5+ nodeifactor.SameIssueArea.6+ nodeifactor.SameIssueArea.7+ 
      nodeifactor.SameIssueArea.8+nodeifactor.SameIssueArea.9+  nodeifactor.SameIssueArea.10+
      nodeifactor.SameIssueArea.11+ nodeifactor.SameIssueArea.12+ nodeifactor.SameIssueArea.13+nodeifactor.SameIssueArea.14+ 
      nodeofactor.SameIssueArea.2+ nodeofactor.SameIssueArea.3+ nodeofactor.SameIssueArea.4+nodeofactor.SameIssueArea.5+ 
      nodeofactor.SameIssueArea.6+nodeofactor.SameIssueArea.7+
      nodeofactor.SameIssueArea.8+nodeofactor.SameIssueArea.9+nodeofactor.SameIssueArea.10+ nodeofactor.SameIssueArea.11+ 
      nodeofactor.SameIssueArea.12+ nodeofactor.SameIssueArea.13+nodeofactor.SameIssueArea.14+
      instar2_sendertime+outstar2_sendertime+ mutual_sendertime+ triangle_sendertime+mq_sendertime +sameissuearea_sendertime+
      yeardiff_sendertime + yeardiffsquare_sendertime+ AbsDiffMQscores_sendertime + NumberJusticesPro_sendertime ,family=binomial,data=mpledata)
summary(mple.est)


mpledata<- read.csv("mpledata_all.csv")



# nonparametric bootstrap resampling
nboot <- 1 # this number should be 200 or higher
ncoef <- length(coef(mple.est))
boot.results <- matrix(NA,nboot,ncoef)
# need to weight by the number of potential edges in a time period
n.per.time <- table(mpledata[,11])
# get unique sender times, over which to weight
unique.sender.times <- as.numeric(names(n.per.time))
# calculate weights for weighted resampling of times
time.weights <- n.per.time/sum(n.per.time)

# loop over bootstrap iterations (easily paralellize)
for(b in 1:nboot){
  print(b)
  # sample time periods with replacement
  time.periods.b <- sample(unique.sender.times,length(unique.sender.times),prob=time.weights,rep=T)
  # construct the data for iteration b
  data.b <- list()
  for(t in time.periods.b){
    data.b[[t]] <- subset(mpledata,mpledata[,11]==t)
  }
  # collapse into a matrix
  data.b <- do.call('rbind',data.b)
  # convert to data frame
  data.b <- data.frame(data.b)
  # run mple
  mple.est.b <- glm(edgeij ~ istar2+ostar2+mutual+triangle+ edgecov.mq.t+ edgecov.same.issue.area.t+ edgecov.year.diff.t,family=binomial,data=data.b)
  # store results
  boot.results[b,] <- coef(mple.est.b)
}

mpledatatest<- read.csv("mpledata_all.csv")

##################### 
# results from aci

bootresults <- read_csv("C:/Users/Geiler Typ/Desktop/PSU/Bruce Desmarais/Supreme Court Citation Project/R-Code/bootresults.csv")
bootresults<- as.data.frame(bootresults)

quantile(bootresults[,2], probs=c(0.975, 0.025), na.rm = TRUE)
quantile(bootresults[,3], probs=c(0.975, 0.025), na.rm = TRUE)
quantile(bootresults[,4], probs=c(0.975, 0.025), na.rm = TRUE)
quantile(bootresults[,5], probs=c(0.975, 0.025), na.rm = TRUE)
quantile(bootresults[,6], probs=c(0.975, 0.025), na.rm = TRUE)
quantile(bootresults[,7], probs=c(0.975, 0.025), na.rm = TRUE)
quantile(bootresults[,8], probs=c(0.975, 0.025), na.rm = TRUE)
quantile(bootresults[,9], probs=c(0.975, 0.025), na.rm = TRUE)




#######################################
# boot MPLE for Vinson era

data.b <- list()
for(k in 214:381){
  data.b[[k-213]] <- subset(mpledata,mpledata$edgecov.sender.time.t==k)
}

# collapse into a matrix
data.b <- do.call('rbind',data.b)
# convert to data frame
mple.vinson<-  data.frame(data.b)

mple.est <- glm(edgeij ~ istar2+ostar2+mutual+triangle+ edgecov.mq.t+ edgecov.same.issue.area.t+ edgecov.year.diff.t,family=binomial,data=mple.vinson)
summary(mple.est)


# nonparametric bootstrap resampling
nboot <- 100 # this number should be 200 or higher
ncoef <- length(coef(mple.est))
boot.results <- matrix(NA,nboot,ncoef)
# need to weight by the number of potential edges in a time period
n.per.time <- table(mple.vinson[,11])
# get unique sender times, over which to weight
unique.sender.times <- as.numeric(names(n.per.time))
# calculate weights for weighted resampling of times
time.weights <- n.per.time/sum(n.per.time)

# loop over bootstrap iterations (easily paralellize)
for(b in 1:nboot){
  print(b)
  # sample time periods with replacement
  time.periods.b <- sample(unique.sender.times,length(unique.sender.times),prob=time.weights,rep=T)
  # construct the data for iteration b
  data.b <- list()
  for(k in 1:length(time.periods.b)){
    t<- time.periods.b[k]
    data.b[[k]] <- subset(mple.vinson,mple.vinson$edgecov.sender.time.t==t)
  }
  # collapse into a matrix
  data.b <- do.call('rbind',data.b)
  # convert to data frame
  data.b <- data.frame(data.b)
  # run mple
  mple.est.b <- glm(edgeij ~ istar2+ostar2+mutual+triangle+ edgecov.mq.t+ edgecov.same.issue.area.t+ edgecov.year.diff.t,family=binomial,data=data.b)
  # store results
  boot.results[b,] <- coef(mple.est.b)
}

quantile(boot.results[,2], probs=c(0.975, 0.025), na.rm = TRUE)
quantile(boot.results[,3], probs=c(0.975, 0.025), na.rm = TRUE)
quantile(boot.results[,4], probs=c(0.975, 0.025), na.rm = TRUE)
quantile(boot.results[,5], probs=c(0.975, 0.025), na.rm = TRUE)
quantile(boot.results[,6], probs=c(0.975, 0.025), na.rm = TRUE)
quantile(boot.results[,7], probs=c(0.975, 0.025), na.rm = TRUE)
quantile(boot.results[,8], probs=c(0.975, 0.025), na.rm = TRUE)
quantile(boot.results[,1], probs=c(0.975, 0.025), na.rm = TRUE)


#######################################
# boot MPLE for Warren era

data.b <- list()
for(k in 382:818){
  data.b[[k-381]] <- subset(mpledata,mpledata$edgecov.sender.time.t==k)
}

# collapse into a matrix
data.b <- do.call('rbind',data.b)
# convert to data frame
mple.warren<-  data.frame(data.b)

mple.est <- glm(edgeij ~ istar2+ostar2+mutual+triangle+ edgecov.mq.t+ edgecov.same.issue.area.t+ edgecov.year.diff.t,family=binomial,data=mple.warren)
summary(mple.est)
