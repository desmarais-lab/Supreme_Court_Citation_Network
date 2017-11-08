# simulation study for supreme court citation project

# requires functions count.outstars and count.triangles
count.outstars.sim<- function(A, age, beta, i){
  # decay vector
  decay<-  decayFun(age, 0.025)
  # set number of triangles = 0
  outstars <-0
  
    # which cases are cited by case i
    ones<- which(A[i,]==1)
    # what are all the twostars that case i has formed
    if(length(ones)>= 2){
      twostars<- t(combn(ones,2))
      l<- dim(twostars)[1]
      # loop through twostars to count weighted outstars
      for(k in 1:l){
        #add outstars
        outstars<- outstars +sqrt(decay[twostars[k,1]]*decay[twostars[k,2]])
      } # end for k
      
    } # end if
  return(outstars)
} # end function

# triangles
count.triangles.sim<- function(A, age, beta, i){
  # decay vector
  decay<- decayFun(age, 0.025)
  # set number of triangles = 0
  triangles <-0

    # which cases are cited by case i
    ones<- which(A[i,]==1)
    # what are all the twostars that case i has formed
    if(length(ones)>= 2){
      twostars<- t(combn(ones,2))
      l<- dim(twostars)[1]
      # loop through twostars to count triangles
      for(k in 1:l){
        #add triangles
        triangles<- triangles +A[twostars[k,2],twostars[k,1]]*sqrt(decay[twostars[k,1]]*decay[twostars[k,2]])
      } # end for k
      
    } # end if
  
  return(triangles)
} # end function


#######################################

# for the optimization we require network statistics that do not only calculate the change statistic, but the entire statistic

count.citation.triangles.sim <- function(A,age,beta){
  # decay vector
  decay<- decayFun(age, 0.025)
  # set number of triangles = 0
  triangles <-0
  
  d<- dim(A)[1]
  for(i in 2:d){
    # which cases are cited by case i
    ones<- which(A[i,]==1)
    # what are all the twostars that case i has formed
    if(length(ones)>= 2){
      twostars<- t(combn(ones,2))
      l<- dim(twostars)[1]
      # loop through twostars to count triangles
      for(k in 1:l){
        #add triangles
        triangles<- triangles +A[twostars[k,2],twostars[k,1]]*sqrt(decay[twostars[k,1]]*decay[twostars[k,2]])
      } # end for k
      
    } # end if
  } # end i
  return(triangles)
}

count.citation.outstars.sim <- function(A, age, beta){
  # decay vector
  decay<-  decayFun(age, 0.025)
  # set number of triangles = 0
  outstars <-0
  
  d<- dim(A)[1]
  for(i in 2:d){
    # which cases are cited by case i
    ones<- which(A[i,]==1)
    # what are all the twostars that case i has formed
    if(length(ones)>= 2){
      twostars<- t(combn(ones,2))
      l<- dim(twostars)[1]
      # loop through twostars to count weighted outstars
      for(k in 1:l){
        #add outstars
        outstars<- outstars +sqrt(decay[twostars[k,1]]*decay[twostars[k,2]])
      } # end for k
      
    } # end if
  } # end i
  return(outstars)
}



# simulate a citation network of dimension 100

CN<- matrix(0, 100, 100)
rownames(CN)<- 1:100
colnames(CN)<- 1:100

# set coefficients
theta.true<- c(-2, -1, 1)

# count.outstar and count.triangle requires an age vector
# create a vector of years
year.vector<- c(rep(2007:2016, times=1, each=10))


########################################################################
###
###  start simulation study
###
########################################################################

MPLE.results<- matrix(0, 50, 3)
colnames(MPLE.results)<-c("edges", 'twostars', 'triangles')

MCMLE.results<- matrix(0, 50, 3)
colnames(MCMLE.results)<-c("edges", 'twostars', 'triangles')

sim.stat<- matrix(0, 50, 3)
colnames(sim.stat)<- c('edges', "outstars", "triangles")

set.seed(123)

for(u in 1:50){


#####################################################
# use gibbs sampling to simulate citation networks
  if(u %% 1 == 0) cat("Iteration #", u, "\n")
print("simulating a new network")
for(l in 1:10){
  #if(l %% 1 == 0) cat("Starting iteration", l, "\n")
for (i in 2:100){ # each row of matrix CN
  for (j in 1:(i-1)){ # max column that can have a 1 in the citation matrix (lower triangle matrix)
    CN.plus<- CN
    CN.minus<- CN
    CN.plus[i,j]<-1
    CN.minus[i,j]<-0
    
    # age vector
    age<- year.vector[i]-year.vector[1:i]
    
    # change statistic
    triangles.plus<- count.triangles.sim(CN.plus,age=age,0.025, i=i)
    triangles.minus<- count.triangles.sim(CN.minus,age=age,0.025, i=i)
    
    outstar.plus<- count.outstars.sim(CN.plus,age=age,0.025, i=i)
    outstar.minus<- count.outstars.sim(CN.minus,age=age,0.025, i=i)
    
    pi<- exp(theta.true[1]*1+theta.true[2]*(outstar.plus-outstar.minus)+theta.true[3]*(triangles.plus-triangles.minus))/
      (1+ exp(theta.true[1]*1+theta.true[2]*(outstar.plus-outstar.minus)+theta.true[3]*(triangles.plus-triangles.minus)))
    
    # draw one sample from Bin (1,pi)
    Z= rbinom(1,1,pi)
    
    # change vector
    if(Z==1){CN[i,j]<- 1}
    if(Z==0){CN[i,j]<- 0}
    
  } # end j
} #end i
 # print(sum(CN))
} #end l
sim.stat[u,1]<- sum(CN)
sim.stat[u,2]<- count.citation.outstars.sim(CN,age=age,0.025 )
sim.stat[u,3]<- count.citation.triangles.sim(CN,age=age,0.025 )

# library(network)
# plot(as.network.matrix(CN))

############################################
## MPLE Estimation

A<- CN

print("MPLE")

# create matrix to store results, such that we can fit a logistic regression model; edges is intercept -> no column for edges

change.mat<- matrix(0, 0.5*(100*100-100), 3) # 3 statistics (edges, outstars, triangles)
colnames(change.mat)<- c("citation", "outstar", "triangle")

# read the values of the lower triangle into the first column of change.mat
change.mat[,1]<- A[lower.tri(A)]

# create empty matrix for outstar and triangle change statistics
triangle.change<- matrix(0,100, 100)
outstar.change<- matrix(0,100,100)

for (i in 2:100){ # each row of matrix CN
  for (j in 1:(i-1)){ 
    A.plus<- A
    A.minus<- A
    A.plus[i,j]<-1
    A.minus[i,j]<-0
    
    triangles.plus<- count.triangles.sim(A.plus,age=age,0.025, i=i)
    triangles.minus<- count.triangles.sim(A.minus,age=age,0.025, i=i)
    triangle.change[i,j]<- triangles.plus-triangles.minus # add value into right row in change.mat
    
    outstar.plus<- count.outstars.sim(A.plus,age=age,0.025, i=i)
    outstar.minus<- count.outstars.sim(A.minus,age=age,0.025, i=i)
    outstar.change[i,j]<- outstar.plus-outstar.minus # add value into right row in change.mat
    
  }
}

# read change statistics from triangle.change and outstar.change into change.mat
change.mat[,2]<- outstar.change[lower.tri(outstar.change)]
change.mat[,3]<- triangle.change[lower.tri(triangle.change)]


#save as data.frame
change.dat<- as.data.frame(change.mat)

# fit logistic regression

modelMPLE<- glm(citation~outstar+triangle, data=change.dat, family="binomial")
MPLE.results[u,]<-summary(modelMPLE)$coefficient[,1] # good results
print(summary(modelMPLE)$coefficient[,1])

#####################################################
#### MCMLE
#####################################################


# estimation of theta
A<- CN
A.obs<- CN

print("MCMLE")

# fix theta_0 as the MPLE estimate
theta_0 <- summary(modelMPLE)$coef[,1]

theta<- summary(modelMPLE)$coef[,1]

# set sum of absolute differences for while loop
imp<-1
iter<- 1
##########################################
while(imp > 0.1 & iter<11 ){
  if(iter %% 1 == 0) cat("MCMLE iteration #", iter, "\n")
# list for sampled citation networks
sampled.network.list<- list()

# first simulate m networks
m<- 50

for(l in 1:m){
  #if(l %% 1 == 0) cat("Simulating network #", l, "\n")
  for (i in 2:100){ # each row of matrix CN
    for (j in 1:(i-1)){ # max column that can have a 1 in the citation matrix (lower triangle matrix)
      A.plus<- A
      A.minus<- A
      A.plus[i,j]<-1
      A.minus[i,j]<-0
      
      # age vector
      age<- year.vector[i]-year.vector[1:i]
      
      # change statistic
      triangles.plus<- count.triangles.sim(A.plus,age=age,0.025, i=i)
      triangles.minus<- count.triangles.sim(A.minus,age=age,0.025, i=i)
      
      outstar.plus<- count.outstars.sim(A.plus,age=age,0.025, i=i)
      outstar.minus<- count.outstars.sim(A.minus,age=age,0.025, i=i)
      
      pi<- exp(theta_0[1]*1+theta_0[2]*(outstar.plus-outstar.minus)+theta_0[3]*(triangles.plus-triangles.minus))/
        (1+ exp(theta_0[1]*1+theta_0[2]*(outstar.plus-outstar.minus)+theta_0[3]*(triangles.plus-triangles.minus)))
      
      # draw one sample from Bin (1,pi)
      Z= rbinom(1,1,pi)
      
      # change vector
      if(Z==1){A[i,j]<- 1}
      if(Z==0){A[i,j]<- 0}
      
    } # end j
  } #end i
  sampled.network.list[[l]]<- A
} #end l


###############################################
## create arrays for optimization, gamma_m includes the network statistcs for every time t for every simulated network m

column.names <- c("edges","outstars","triangles")
row.names <- 1:m
matrix.names <- 2:100

gamma_m<- array(0,dim=c(m,3,99) ,dimnames = list(row.names,column.names,matrix.names))

# fill array
for(i in 2:100){
for(j in 1:m){
  N<- sampled.network.list[[j]]
  N<- N[1:i, 1:i]
  gamma_m[j,1,i-1]<- sum(N)
  gamma_m[j,2,i-1]<- count.citation.outstars.sim(N,age=age[1:i],0.025)
  gamma_m[j,3,i-1]<- count.citation.triangles.sim(N,age=age[1:i],0.025)
}
}

# gamma_N: statistics of the observed network at every timepoint t

# create vector Gamma_N with values of the observed network
gamma_N<- matrix(0, 99, 3)
colnames(gamma_N)<- c("edges", "outstars", "triangles")
rownames(gamma_N)<- 2:100

for(i in 2:100){
A.obs.cut<- A.obs[1:i, 1:i]  
gamma_N[i-1,1]<- sum(A.obs.cut)
gamma_N[i-1,2]<- count.citation.outstars.sim(A.obs.cut,age=age[1:i],0.025)
gamma_N[i-1,3]<- count.citation.triangles.sim(A.obs.cut,age=age[1:i],0.025)

}

##################################################################
## likelihood function for optim

ercm_iter<- function(theta, gamma_m, gamma_N, theta_0){

  ll<- 0
  for (i in 2:100){
  ll<- ll +sum(c(theta[1],theta[2],theta[3])*gamma_N[i-1,])- log(sum(exp(gamma_m[,,i-1]%*%(c(theta[1],theta[2],theta[3])- theta_0))))
  }
  -ll
}

###################################################################
## optim
theta.old<- theta
theta<- try(optim(par=theta, fn= ercm_iter, gamma_N=gamma_N, gamma_m=gamma_m, theta_0=theta_0, method="BFGS")$par)
print(theta)
theta_0<- theta

MCMLE.results[u,]<- theta

## calculate improvement
imp<- try(sum(abs(theta-theta.old)))
if(class(imp)=='try-error'){
  imp<- 0.001
}
cat("MCMLE improved by", imp, "\n")

# iteration +1

iter<- iter +1


} # end while loop

print(theta)

} # end simulation study loop







A.plus<- CN
A.minus<- CN
A.plus[i,j]<-1
A.minus[i,j]<-0

triangles.plus<- count.triangles.sim(A.plus,age=age,0.025, i=i)
triangles.minus<- count.triangles.sim(A.minus,age=age,0.025, i=i)
triangles.plus-triangles.minus

triangles.plus<- count.citation.triangles.sim(A.plus,age=age,0.025)
triangles.minus<- count.citation.triangles.sim(A.minus,age=age,0.025)
triangles.plus-triangles.minus


outstar.plus<- count.outstars.sim(A.plus,age=age,0.025, i=i)
outstar.minus<- count.outstars.sim(A.minus,age=age,0.025, i=i)
outstar.plus-outstar.minus

outstar.plus<- count.citation.outstars.sim(A.plus,age=age,0.025 )
outstar.minus<- count.citation.outstars.sim(A.minus,age=age,0.025)
outstar.plus-outstar.minus



################################################
## Assessing the Gibbs sampling method

# MPLE
sim.stat<- matrix(0, 50, 3)
colnames(sim.stat)<- c('edges', "outstars", "triangles")

# simulate a citation network of dimension 100

CN<- matrix(0, 100, 100)
rownames(CN)<- 1:100
colnames(CN)<- 1:100

MPLE.results<- matrix(0, 50, 3)
colnames(MPLE.results)<-c("edges", 'twostars', 'triangles')

# set coefficients
theta.true<- c(-2, -1, 1)

# count.outstar and count.triangle requires an age vector
# create a vector of years
year.vector<- c(rep(2007:2016, times=1, each=10))

set.seed(123)
for(l in 1:50){
  if(l %% 1 == 0) cat("Starting iteration", l, "\n")
  for (i in 2:100){ # each row of matrix CN
    for (j in 1:(i-1)){ # max column that can have a 1 in the citation matrix (lower triangle matrix)
      CN.plus<- CN
      CN.minus<- CN
      CN.plus[i,j]<-1
      CN.minus[i,j]<-0
      
      # age vector
      age<- year.vector[i]-year.vector[1:i]
      
      # change statistic
      triangles.plus<- count.triangles.sim(CN.plus,age=age,0.025, i=i)
      triangles.minus<- count.triangles.sim(CN.minus,age=age,0.025, i=i)
      
      outstar.plus<- count.outstars.sim(CN.plus,age=age,0.025, i=i)
      outstar.minus<- count.outstars.sim(CN.minus,age=age,0.025, i=i)
      
      pi<- exp(theta.true[1]*1+theta.true[2]*(outstar.plus-outstar.minus)+theta.true[3]*(triangles.plus-triangles.minus))/
        (1+ exp(theta.true[1]*1+theta.true[2]*(outstar.plus-outstar.minus)+theta.true[3]*(triangles.plus-triangles.minus)))
      
      # draw one sample from Bin (1,pi)
      Z= rbinom(1,1,pi)
      
      # change vector
      if(Z==1){CN[i,j]<- 1}
      if(Z==0){CN[i,j]<- 0}
      
    } # end j
  } #end i
  sim.stat[l,1]<- sum(CN)
  sim.stat[l,2]<- count.citation.outstars.sim(CN,age=age,0.025 )
  sim.stat[l,3]<- count.citation.triangles.sim(CN,age=age,0.025 )
  # print(sum(CN))
  
 ###################################  
 ### MPLE 
  A<- CN
  
  print("MPLE")
  
  # create matrix to store results, such that we can fit a logistic regression model; edges is intercept -> no column for edges
  
  change.mat<- matrix(0, 0.5*(100*100-100), 3) # 3 statistics (edges, outstars, triangles)
  colnames(change.mat)<- c("citation", "outstar", "triangle")
  
  # read the values of the lower triangle into the first column of change.mat
  change.mat[,1]<- A[lower.tri(A)]
  
  # create empty matrix for outstar and triangle change statistics
  triangle.change<- matrix(0,100, 100)
  outstar.change<- matrix(0,100,100)
  
  for (i in 2:100){ # each row of matrix CN
    for (j in 1:(i-1)){ 
      A.plus<- A
      A.minus<- A
      A.plus[i,j]<-1
      A.minus[i,j]<-0
      
      triangles.plus<- count.triangles.sim(A.plus,age=age,0.025, i=i)
      triangles.minus<- count.triangles.sim(A.minus,age=age,0.025, i=i)
      triangle.change[i,j]<- triangles.plus-triangles.minus # add value into right row in change.mat
      
      outstar.plus<- count.outstars.sim(A.plus,age=age,0.025, i=i)
      outstar.minus<- count.outstars.sim(A.minus,age=age,0.025, i=i)
      outstar.change[i,j]<- outstar.plus-outstar.minus # add value into right row in change.mat
      
    }
  }
  
  # read change statistics from triangle.change and outstar.change into change.mat
  change.mat[,2]<- outstar.change[lower.tri(outstar.change)]
  change.mat[,3]<- triangle.change[lower.tri(triangle.change)]
  
  
  
  #save as data.frame
  change.dat<- as.data.frame(change.mat)
  
  # fit logistic regression
  
  modelMPLE<- glm(citation~outstar+triangle, data=change.dat, family="binomial")
  MPLE.results[l,]<-summary(modelMPLE)$coefficient[,1] # good results
} #end l


# traceplots statistics
par(mfrow=c(2,2))
plot(sim.stat[,1], type="l", main="edges, theta=-2,-1,1")
plot(sim.stat[,2], type="l", main="outstars, theta=-2,-1,1")
plot(sim.stat[,3], type="l", main="triangles, theta=-2,-1,1")

# traceplots coefficients
par(mfrow=c(2,2))
plot(MPLE.results[,1], type="l", main="edges, theta=-2,-1,1")
abline(h=-2, col="red")
plot(MPLE.results[,2], type="l", main="outstars, theta=-2,-1,1")
abline(h=-1, col="red")
plot(MPLE.results[,3], type="l", main="triangles, theta=-2,-1,1")
abline(h=1, col="red")



### adding MCMLE
