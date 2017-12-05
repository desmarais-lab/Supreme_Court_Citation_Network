# merge together csv data that was obtained from the aci.ics cluster
 
# rm(list= ls()[!(ls() %in% c('scc1','adjacency.matrix', 'mq.matrix', 'same.issue.area', 'year.diff.matrix'))])
memory.limit(30000)

library(readr)

mpledata1 <- read_csv("mpledata.csv")
mpledata2 <- read_csv("mpledata2.csv")
mpledata3 <- read_csv("mpledata3.csv")
mpledata4 <- read_csv("mpledata4.csv")
mpledata5 <- read_csv("mpledata5.csv")

mpledata1<- as.data.frame(mpledata)
mpledata2<- as.data.frame(mpledata2)
mpledata3<- as.data.frame(mpledata3)
mpledata4<- as.data.frame(mpledata4)
mpledata5<- as.data.frame(mpledata5)

mpledata<- rbind(mpledata1, mpledata2, mpledata3, mpledata4, mpledata5)

rm(mpledata1, mpledata2, mpledata3, mpledata4, mpledata5)
#write.csv(mpledata, file = "mpledata_all.csv")

mple.est <- glm(edgeij ~ istar2+ostar2+mutual+triangle+ edgecov.mq.t+ edgecov.same.issue.area.t+ edgecov.year.diff.t,family=binomial,data=mpledata)
summary(mple.est)






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
