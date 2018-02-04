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

# first degree
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

# 2nd degree
mpledata$instar2_sendertime2<- mpledata[,3]*mpledata[,14]*mpledata[,14]
mpledata$mutual_sendertime2<- mpledata[,4]*mpledata[,14]*mpledata[,14]
mpledata$outstar2_sendertime2<- mpledata[,5]*mpledata[,14]*mpledata[,14]
mpledata$triangle_sendertime2<- mpledata[,6]*mpledata[,14]*mpledata[,14]
mpledata$mq_sendertime2<- mpledata[,7]*mpledata[,14]*mpledata[,14]
mpledata$sameissuearea_sendertime2<- mpledata[,8]*mpledata[,14]*mpledata[,14]
mpledata$yeardiff_sendertime2<- mpledata[,9]*mpledata[,14]*mpledata[,14]
mpledata$yeardiffsquare_sendertime2<- mpledata[,10]*mpledata[,14]*mpledata[,14]
mpledata$AbsDiffMQscores_sendertime2<- mpledata[,16]*mpledata[,14]*mpledata[,14]
mpledata$NumberJusticesPro_sendertime2<- mpledata[,17]*mpledata[,14]*mpledata[,14]

# 3rd degree
mpledata$instar2_sendertime3<- mpledata[,3]*mpledata[,14]*mpledata[,14]*mpledata[,14]
mpledata$mutual_sendertime3<- mpledata[,4]*mpledata[,14]*mpledata[,14]*mpledata[,14]
mpledata$outstar2_sendertime3<- mpledata[,5]*mpledata[,14]*mpledata[,14]*mpledata[,14]
mpledata$triangle_sendertime3<- mpledata[,6]*mpledata[,14]*mpledata[,14]*mpledata[,14]
mpledata$mq_sendertime3<- mpledata[,7]*mpledata[,14]*mpledata[,14]*mpledata[,14]
mpledata$sameissuearea_sendertime3<- mpledata[,8]*mpledata[,14]*mpledata[,14]*mpledata[,14]
mpledata$yeardiff_sendertime3<- mpledata[,9]*mpledata[,14]*mpledata[,14]*mpledata[,14]
mpledata$yeardiffsquare_sendertime3<- mpledata[,10]*mpledata[,14]*mpledata[,14]*mpledata[,14]
mpledata$AbsDiffMQscores_sendertime3<- mpledata[,16]*mpledata[,14]*mpledata[,14]*mpledata[,14]
mpledata$NumberJusticesPro_sendertime3<- mpledata[,17]*mpledata[,14]*mpledata[,14]*mpledata[,14]


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
# delete auxiliary column
mpledata<- mpledata[,-1]

##################################################################
#### Spline Expansion 

# for exploration use smaller data subset


library(mgcv)

mple.gam<- gam(edgeij ~ s(edgecov.year.t, bs="ps") + edgecov.mq.t+ mq_sendertime, method='GCV.Cp',data= mpledata, 
               family = "binomial" )

mple.gam.interact <- gam(edgeij ~  s(eval(edgecov.year.t*edgecov.mq.t), bs="ps")+ mq_sendertime, method='GCV.Cp',data= mpledata, 
               family = "binomial" )

mple.glm.interact <- lm(edgeij ~   bs(edgecov.year.t)*edgecov.mq.t +   mq_sendertime, data= mpledata, 
                         family = "binomial" )



#bs(b,df=3))


gam.coef<- coef(mple.gam)
gam.coef.interact<- coef(mple.gam.interact)

# plot sender.time
plot(mple.gam, residuals=F, xlab="Time", xlim=c(0,18), main="Sender Time") 
plot(mple.gam.interact, residuals=F, xlab="Time", xlim=c(0,18), main="Sender Time") 





plotData <- list()
trace(mgcv:::plot.gam, at = list(c(27, 1)), 
      ## tested for mgcv_1.8-4. other versions may need different at-argument.
      quote({
        message("ooh, so dirty -- assigning into globalenv()'s plotData...")
        plotData <<- pd
      }))
mgcv::plot.gam(mple.gam, seWithMean = TRUE, pages = 1)

par(mfrow = c(1, 1))
for (i in 1:1) {
  plot(plotData[[i]]$x, coef(mple.gam)[2] + coef(mple.gam)[3]*plotData[[i]]$fit + plotData[[i]]$fit, type = "l", main="f(Sendertime)*coef(MQ Score)", xlim=c(0,66), xlab="Time", ylab="y")
      # ylim = range(coef(mple.gam)[2] + coef(mple.gam)[3]*plotData[[i]]$fit + plotData[[i]]$fit + plotData[[i]]$se, coef(mple.gam)[2] + coef(mple.gam)[3]*plotData[[i]]$fit -
                     # plotData[[i]]$se)+ plotData[[i]]$fit, xlab= "Time")
  matlines(plotData[[i]]$x, cbind(coef(mple.gam)[2] + coef(mple.gam)[3]*plotData[[i]]$fit + plotData[[i]]$fit+ plotData[[i]]$se, 
                                  coef(mple.gam)[2] + coef(mple.gam)[3]*plotData[[i]]$fit + plotData[[i]]$fit- plotData[[i]]$se), lty = 2, col = 1)
  #rug(plotData[[i]]$raw)  
}


### This works!
#########################################################################################################################







plotData <- list()
trace(mgcv:::plot.gam, at = list(c(27, 1)), 
      ## tested for mgcv_1.8-4. other versions may need different at-argument.
      quote({
        message("ooh, so dirty -- assigning into globalenv()'s plotData...")
        plotData <<- pd
      }))
mgcv::plot.gam(b, seWithMean = TRUE, pages = 1)


for (i in 1:1) {
  plot(plotData[[i]]$x, plotData[[i]]$fit, type = "l", xlim = plotData[[i]]$xlim,
       ylim = range(plotData[[i]]$fit + plotData[[i]]$se, plotData[[i]]$fit -
                      plotData[[i]]$se))
  matlines(plotData[[i]]$x, cbind(plotData[[i]]$fit + plotData[[i]]$se, 
                                  plotData[[i]]$fit - plotData[[i]]$se), lty = 2, col = 1)
  #rug(plotData[[i]]$raw)  
}



mq_coefficients <- c("edgecov.mq.t","mq_sendertime")

sender_times <- 1:4
sender_years <- 1937:1940

for(t in sender_times){
  effect.t <- bootstrapResults[,mq_coefficients[1]]+t*bootstrapResults[,mq_coefficients[2]]
  mq_coef_trend[t,] <- c(mean(effect.t),quantile(effect.t,prob=c(0.025,.975)))
}

# knots
knots.gam<- mple.gam$smooth[[1]]$knots

# coefs
intercept <- coef(mple.gam)[1] 
coefs <- rep(NA, length(coef(mple.gam))) 
coefs[1]  <- intercept 
coefs[-1] <- intercept + coef(mple.gam)[-1] 

# bspline function
bspline <- function(x,k,i,m=2){ 
  ## "draws" one B-spline basis function 
  ## order of splines is m+1, i.e. m=2 means cubic splines of order 3 
  if(m==-1){ 
    res <- as.numeric(x<k[i+1] & x>=k[i]) 
  } else { 
    z0 <- (x-k[i]) / (k[i+m+1]-k[i]) 
    z1 <- (k[i+m+2]-x)/(k[i+m+2]-k[i+1]) 
    res <- z0*bspline(x,k,i,m-1) + z1*bspline(x,k,i+1,m-1) 
  } 
  res 
} 

construct_bspline <- function(x, knots, coefs, m){ 
  ## Constructs a complete spline from a set of knots and coefficients 
  y <- rep(0, times=length(x)) 
  for(i in seq(coefs)){ 
    coef <- coefs[i] 
    y <- y + coefs[i] * bspline(x, knots, i, m) 
  } 
  y 
} 

m<-2


spline_y <- construct_bspline(mpledata$mq_sendertime, knots.gam, coefs, m) 

plot(mple.gam, lwd=2, col="red") 
lines(mpledata$mq_sendertime,spline_y - mean(spline_y))



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



library(mgcv) 
set.seed(0)
dat <- gamSim(1, n = 400, dist = "normal", scale = 2)
b <- gam(y ~ s(x0) + s(x1) + s(x2) + s(x3), data = dat)

plotData <- list()
trace(mgcv:::plot.gam, at = list(c(27, 1)), 
      ## tested for mgcv_1.8-4. other versions may need different at-argument.
      quote({
        message("ooh, so dirty -- assigning into globalenv()'s plotData...")
        plotData <<- pd
      }))
mgcv::plot.gam(b, seWithMean = TRUE, pages = 1)

par(mfrow = c(2, 2))
for (i in 1:4) {
  plot(plotData[[i]]$x, plotData[[i]]$fit, type = "l", xlim = plotData[[i]]$xlim,
       ylim = range(plotData[[i]]$fit + plotData[[i]]$se, plotData[[i]]$fit -
                      plotData[[i]]$se))
  matlines(plotData[[i]]$x, cbind(plotData[[i]]$fit + plotData[[i]]$se, 
                                  plotData[[i]]$fit - plotData[[i]]$se), lty = 2, col = 1)
  #rug(plotData[[i]]$raw)  
}


#####

sender_times <- 1:10
sender_years <- 1937:1946


library(mgcv) 
set.seed(0)
dat <- gamSim(1, n = 400, dist = "normal", scale = 2)
b <- gam(y ~ s(x0) + s(x1) + s(x2) + s(x3), data = dat)

plotData <- list()
trace(mgcv:::plot.gam, at = list(c(27, 1)), 
      ## tested for mgcv_1.8-4. other versions may need different at-argument.
      quote({
        message("ooh, so dirty -- assigning into globalenv()'s plotData...")
        plotData <<- pd
      }))
mgcv::plot.gam(b, seWithMean = TRUE, pages = 1)

mq_coef_trend <- matrix(NA,length(sender_times),1)

for(t in sender_times){
  mq_coef_trend[t,] <- t*plotData[[1]]$fit
}




###################################################################################
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
