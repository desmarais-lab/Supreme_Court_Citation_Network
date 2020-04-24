
#################################################
#################################################
###             GOF for 1950 and 2015 for OSP and OST
#################################################
#################################################



library(statnet) 

# calculate indegree, outdegree and esp of unfixed nodes of observed network

year.total <- scc1[,11]-1936 #-1936 => the first year 1937 is 1, 1938 is 2 aso, term column 

set.seed(12123)
#sampled.years <- c(14, 24, 34, 44, 54, 64, 74)

t<-  79 # 79 stands for 2015, 14 for 1950
print(t+1936)


cases <- max(which(year.total==t)) 

# simple assignment of time periods to cases
years <- year.total[1:cases] 

# going to need a sender time matrix covariate
sender.time <- matrix(years,length(years),length(years),byrow=F)
year <- matrix(years, length(years),length(years),byrow=F)

# extract the network up to time t
AM <- adjacency.matrix[which(years <= t),which(years <= t)]

# determine the unfixed nodes
unfixed<- which(years==t)

# get outdegree of these nodes
unfixed.odeg <- rowSums(AM[unfixed,])
#hist(unfixed.odeg)

# get indegree of these nodes
unfixed.ideg <- colSums(AM[unfixed,])
#hist(unfixed.ideg)

net.t <- network(AM) 

# get edgewise shared partner distribution
full.esp <- summary(net.t ~ esp(0:20))
full.esp   # this is esp dist if entire network is unfixed
#plot(full.esp)

# get esp dist of t-1
AM.red <- AM[-unfixed, -unfixed]
net.r <- network(AM.red) 

# get edgewise shared partner distribution
red.esp <- summary(net.r ~ esp(0:20))
red.esp

unfixed.esp.ost<- full.esp-red.esp
#plot(unfixed.esp)


w<- which(sender.time[,1]==t)  

esp <-rep(0,21)
for(u in w){
  
  for(j in 1:nrow(sender.time)){
    if(AM[u,j]==1){
      es <- 0
      for(k in 1:nrow(sender.time)){
        if( AM[u,j]*AM[u,k]*AM[j,k]==1){
          es <- es+1 
        } # if ==1
      } # for k
      if(es< 21){
        esp[es+1]<- esp[es+1] +1
      } # end if(es<21)
    }# end if(AM[u,j]==1)
    
  } #for j
  
  
}# for i



unfixed.esp.osp<- esp





#####################################################
### get cluster results
#####################################################



# load simulated models
#load("GOF_Deg_1950.RData")
load("2015_GOF_Deg_just_deg.RData")

odeg.matrix <- li[[1]]
ideg.matrix <- li[[2]]
esp.matrix.ost <- li[[3]]
esp.matrix.osp <- li[[4]]

# get observed vector into right form
ideg.obs <- rep(0, 21)
odeg.obs <- rep(0,21)
for(k in 0:20){
  s<- which(names(table(unfixed.odeg))==k   ) 
  r<- which(names(table(unfixed.ideg))==k   )
  
  if(length(s)!=0){
    odeg.obs[k+1]<- table(unfixed.odeg)[s]
  }
  
  if(length(r)!=0){
    ideg.obs[k+1]<- table(unfixed.ideg)[r]
  }
  
}


###################################
# create goodness-of-fit plots

par(mfrow=c(2,2), oma=c(0,0,2,0))
boxplot(odeg.matrix, use.col=TRUE, col="lightgrey", main="Outdegree", xlab="Outddegree Distribution", cex.axis=1.8, cex.lab=1.7, cex.main=2.5)
lines(odeg.obs, type="l", lwd=2)

boxplot(ideg.matrix, use.col=TRUE, col="lightgrey", main="Indegree", xlab="Inddegree Distribution", cex.axis=1.8, cex.lab=1.7, cex.main=2.5)
lines(ideg.obs, type="l", lwd=2)


boxplot(esp.matrix.ost, use.col=TRUE, col="lightgrey", main="Edgewise Shared Partners OTP", xlab="ESP Distribution",
         cex.axis=1.8, cex.lab=1.7, cex.main=2.5)
lines(unfixed.esp.ost, type="l", lwd=2)

boxplot(esp.matrix.osp, use.col=TRUE, col="lightgrey", main="Edgewise Shared Partners OSP", xlab="ESP Distribution",
         cex.axis=1.8, cex.lab=1.7, cex.main=2.5)
lines(unfixed.esp.osp, type="l", lwd=2)

title("Goodness-of-fit 1950",outer=TRUE , cex.main=2.5)




######################################
# create MCMC trace plots



sim <- li[[6]]
erg.est <- li[[7]]


par(mfrow=c(5,2), oma=c(0,0,2,0))
hist(sim[,1], main="Density of Edges", col="grey55", xlab="Edges", cex.main=1.8, cex.lab=1.5, cex.axis=1.7)
abline(v=erg.est[1],lwd=3)
plot(sim[,1], main="Edges", col="grey45", xlab="Iterations", ylab="Edges", cex.main=1.8, cex.lab=1.5, cex.axis=1.7)
abline(h=erg.est[1],lwd=2)

hist(sim[,3], main="Density of Receiver Outdegree", col="grey55", xlab="Receiver Outdegree", cex.main=1.8, cex.lab=1.5, cex.axis=1.7)
abline(v=erg.est[3],lwd=3)
plot(sim[,3], main="Receiver Outdegree", col="grey45", xlab="Iterations", ylab="Receiver Outdegree", cex.main=1.8, cex.lab=1.5, cex.axis=1.7)
abline(h=erg.est[3],lwd=2)

hist(sim[,4], main="Density of GWIDEGREE", col="grey55", xlab="GWIDEGREE", cex.main=1.8, cex.lab=1.5, cex.axis=1.7)
abline(v=erg.est[4],lwd=3)
plot(sim[,4], main="GWIDEGREE", col="grey45", xlab="Iterations", ylab="GWIDEGREE", cex.main=1.8, cex.lab=1.5, cex.axis=1.7)
abline(h=erg.est[4],lwd=2)


hist(sim[,5], main="Density of Different Term Transitivity", col="grey55", xlab="Different Term Transitivity", cex.main=1.8, cex.lab=1.5, cex.axis=1.7)
abline(v=erg.est[5],lwd=3)
plot(sim[,5], main="Different Term Transitivity", col="grey45", xlab="Iterations", ylab="Different Term Transitivity", 
     cex.main=1.8, cex.lab=1.5, cex.axis=1.7)
abline(h=erg.est[5],lwd=2)


hist(sim[,6], main="Density of GWESP", col="grey55", xlab="GWESP", cex.main=1.8, cex.lab=1.5, cex.axis=1.7)
abline(v=erg.est[6],lwd=3)
plot(sim[,6], main="GWESP", col="grey45", xlab="Iterations", ylab="GWESP", cex.main=1.8, cex.lab=1.5, cex.axis=1.7)
abline(h=erg.est[6],lwd=2)


title('Degeneracy Check 2015', outer=TRUE, cex.main=3)

