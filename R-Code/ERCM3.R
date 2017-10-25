# requires scc_data_merging
library(combinat)


scc1<- scc


# create empty adjacency matrix
memory.limit(30000)
adjacency.matrix<- matrix(0, 26803, 26803)

# fill adjacency matrix
for(i in 1:209946){
  #print(i)
  if(i %% 1000 == 0) cat("Starting iteration", i, "\n")
  # which line can the information of case edgelist[i,1] be found in?
  l<- which(edgelist[i,1]==scc1[,1])
  # which line can the information of case edgelist[i,2] be found in? used to count how often a case is being cited
  q<- which(edgelist[i,2]==scc1[,1])
  # indicate tie in adjacency matrix
  adjacency.matrix[l,q]<- 1
}

# only keep the part of the adjacency matrix for which we have Martin Quinn scores
# 17987 is the first row in scc and the first row id of a case for which we were able to calculate the mean MQ score
adjacency.matrix<- adjacency.matrix[17987:26803, 17987:26803]


# write function that cuts out the necessary adjacency matrix for a time t

#AM.t<- function(time.t){ # first id with MQ scores is 3136, last 5251
  # what is the id of the case/cases that entered the network at time t
#  latest.case<- max(which(time.t==scc1[, 71]))
#  AM<- adjacency.matrix[1:(latest.case-17986), 1:(latest.case-17986)]
#  return(AM)
#}


#### statistics
time.t<- 3500
latest.case<- max(which(time.t==scc1[, 71]))
year.latest.case <- scc1[latest.case, 4]

decayFun <- function(age,beta){2/(1+exp(beta*age))}
par(las=1)
age = 0:100
plot(age,decayFun(age,1),type="l",lwd=2,ylab="weight")
lines(age,decayFun(age,0.25),col="red",lwd=2)
lines(age,decayFun(age,0.1),col="green",lwd=2)
lines(age,decayFun(age,0.05),col="blue",lwd=2)
lines(age,decayFun(age,0.025),col="grey60",lwd=2)
legend("topright",legend=c("1","0.25","0.10","0.05","0.025"),lwd=2,col=c("black","red","green","blue","grey60"),title="Beta")

#######################################
# adjust Am.t such that it can also cut mq.matrix into the right shape

# mod=1 adjacency matrix, mod=2, Marting Quinn score matrix, mod=3 Issue are, mod=4, Year difference matrix
AM.t<- function(time.t, mod){ # first id with MQ scores is 3136, last 5251
  # what is the id of the case/cases that entered the network at time t
  latest.case<- max(which(time.t==scc1[, 71]))
  AM<- adjacency.matrix[1:(latest.case-17986), 1:(latest.case-17986)]
  MQ<- mq.matrix[1:(latest.case-17986), 1:(latest.case-17986)]
  IA <- same.issue.area[1:(latest.case-17986), 1:(latest.case-17986)]
  YD <- year.diff.matrix[1:(latest.case-17986), 1:(latest.case-17986)]
  if(mod==1){
    return(AM)}
  if(mod==2){
    return(MQ)}
  if(mod==3){
    return(IA)}
  if(mod==4){
    return(YD)}
}



A<- AM.t(time.t, 1)
##################################
### triangles

# transpose matrix before applying to this function
count.citation.triangles <- function(A,age,beta){
  # Input is an adjacency matrix
  # weight edge by the age of the sender
  ageMat <- matrix(decayFun(age,beta),nrow(A),nrow(A),byrow=T)
  A <- t(A)*ageMat
  B <- t(A)%*%A # this step takes a long time for large matrices
  sum(A*B) 
}

# age is been calculated as the year of the latest case minus the year of the previous case, years can be found in column 4 of scc1
count.citation.triangles(A, year.latest.case-scc1[17987:latest.case, 4], 0.025)


# alternativ function to count weighted citation triangles
count.triangles<- function(A, age, beta){
  # decay vector
  decay<- decayFun(age, 0.025)
  # set number of triangles = 0
  triangles <-0
  # loop through all the cases that entered the network at time.t
  for(i in (latest.case-17986-number.cases.at.t):(latest.case-17986)){
    # which cases are cited by case i
    ones<- which(A[i,]==1)
    # what are all the twostars that case i has formed
    if(length(ones)>= 2){
    twostars<- t(combn(ones,2))
    l<- dim(twostars)[1]
    # loop through twostars to count triangles
    for(k in 1:l){
      #add triangles
      triangles<- triangles +A[twostars[k,1],twostars[k,2]]*sqrt(decay[twostars[k,1]]*decay[twostars[k,2]])
    } # end for k
    
    } # end if
  } # end for i
  
  return(triangles)
} # end function

count.triangles(A, age, 0.025)


##################################
### outstar

count.citation.outstars <- function(A, age, beta){
  # Input is an adjacency matrix
  # weight edge by the age of the sender
  ageMat <- matrix(decayFun(age,beta),nrow(A),nrow(A),byrow=T)
  A <- t(A)*ageMat
  B <- t(A)%*%A
  # calculate number of weighted outstars
  (sum(B)-sum(diag(B)))/2
}

# test
count.citation.outstars(A, year.latest.case-scc1[17987:latest.case, 4], 0.025)



# alternativ function to count weighted citation outstars
count.outstars<- function(A, age, beta){
  # decay vector
  decay<- decayFun(age, 0.025)
  # set number of triangles = 0
  outstars <-0
  # loop through all the cases that entered the network at time.t
  for(i in (latest.case-17986-number.cases.at.t):(latest.case-17986)){
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
  } # end for i
  
  return(outstars)
} # end function

count.outstars(A, age, 0.025)


####################################
### edges


# number of edges statistic
edges<- function(A){
  sum(A)
}


#########################################
### issue area
# 8817 is dimension of adjaceny matrix
# create matrix where 1 indicates that two cases have same issue area, 0 ow
same.issue.area<- matrix(0, 8817, 8817)
for(i in 17987:26803){ # calculation takes a while
  if(i %% 100 == 0) cat("Starting iteration", i, "\n")
  # issue area for sender
  issue.area.sender <- scc1[i, 57]
  for(j in 17987:26803){
    # issue area for receiver
    issue.area.receiver <- scc1[j, 57]
    # if sender and receiver issue area is the same and non is NA then enter 1 into matrix to indicate same issue area
    if(is.na(issue.area.receiver)==FALSE & is.na(issue.area.sender)==FALSE & issue.area.sender==issue.area.receiver){
      same.issue.area[i-17986,j-17986]<-1
    }
  }
}

issue.area.stat <- function(A, IA){
  sum(A*IA)
}

issue.area.stat(AM.t(3300,1), AM.t(3300,3))

#########################################
## Martin Quinn score
# 8817 is dimension of adjaceny matrix
mq.matrix <- matrix(rep(scc1[17987:26803,72], 8817), 8817, 8817)


mq.stat <- function(A, MQ){
  sum(A*MQ, na.rm=TRUE)
}

mq.stat(AM.t(3300,1), AM.t(3300,2))

############################################################
## time difference
year.diff.matrix<- matrix(0, 8817, 8817)

for(i in 17987:26803){ # calculation takes a while
  if(i %% 100 == 0) cat("Starting iteration", i, "\n")
  # year for sender
  year.sender <- scc1[i, 4]
  for(j in 17987:26803){
    # year for receiver
    year.receiver <- scc1[j, 4]
    year.diff.matrix[i-17986,j-17986]<- abs(year.sender- year.receiver)
  }
}

year.stat <- function(A, YD){
  sum(A*YD, na.rm=TRUE)
}

year.stat(AM.t(3300,1), AM.t(3300,4))

#############################################
#### Estimation
#############################################

############### Pseudolikelihood



# set time point, range [3136, 5251]
time.t<- 5000
number.cases.at.t<- length(which(time.t==scc1[, 71]))
latest.case<- max(which(time.t==scc1[, 71]))
year.latest.case <- scc1[latest.case, 4]

# get adjacency matrix for this given year
A <- AM.t(time.t, 1)
# issue Area Matrix
IA<- AM.t(time.t, 3)
# MQ score matrix
MQ<- AM.t(time.t, 2)
# year diff matrix
YD<- AM.t(time.t, 4)

# fill in right values into change.mat. change.mat will play the role of a dataframe for glm
d<- dim(A)[1]

# create matrix to store results, such that we can fit a logistic regression model; edges is intercept -> no column for edges

change.mat<- matrix(0, number.cases.at.t*d, 6) # 3 statistics (edges, outstars, triangles)
colnames(change.mat)<- c("citation", "outstar", "triangle", "issuearea", "MQscore", "yeardiff")


change.mat[,1]<- c(A[(d-number.cases.at.t+1):d,]) # enters particular vaues of matrix row wise

age<- year.latest.case-scc1[17987:latest.case, 4]
# change statistic matrix
for(j in (d-number.cases.at.t+1):d){
  print(j)
  # calculate change statistics
  for(i in 1:d){
    #print(i)
    #if(i %% 250 == 0) cat("Starting iteration", i, "\n")
    A.plus<- A
    A.minus<- A
    A.plus[i,j]<-1
    A.minus[i,j]<-0
    
    triangles.plus<- count.triangles(A.plus,age=age,0.025)
    triangles.minus<- count.triangles(A.minus,age=age,0.025)
    change.mat[(j-(d-number.cases.at.t+1))*d+i,3]<- triangles.plus-triangles.minus # add value into right row in change.mat
    
    outstar.plus<- count.outstars(A.plus,age=age,0.025)
    outstar.minus<- count.outstars(A.minus,age=age,0.025)
    change.mat[(j-(d-number.cases.at.t+1))*d+i,2]<- outstar.plus-outstar.minus # add value into right row in change.mat
    
    issue.plus <- issue.area.stat(A.plus, IA )
    issue.minus <- issue.area.stat(A.minus, IA )
    change.mat[(j-(d-number.cases.at.t+1))*d+i,4]<- issue.plus-issue.minus
    
    mq.plus<- mq.stat(A.plus, MQ)
    mq.minus<- mq.stat(A.minus, MQ)
    change.mat[(j-(d-number.cases.at.t+1))*d+i,5]<- mq.plus-mq.minus
    
    year.diff.plus<- year.stat(A.plus, YD)
    year.diff.minus<- year.stat(A.minus, YD)
    change.mat[(j-(d-number.cases.at.t+1))*d+i,6]<- year.diff.plus-year.diff.minus
    
    # loops are not considered
    if(i==j){change.mat[(j-(d-number.cases.at.t+1))*d+i,2:6]<- 0}
    
  }
}

#save as data.frame
change.dat<- as.data.frame(change.mat)

# fit logistic regression

modelMPLE<- glm(citation~outstar+triangle+issuearea+MQscore+yeardiff, data=change.dat, family="binomial")
summary(modelMPLE)

################################################
################################################
## MCMLE
################################################
################################################

# set time point, range [3136, 5251]
time.t<- 3180
number.cases.at.t<- length(which(time.t==scc1[, 71]))
latest.case<- max(which(time.t==scc1[, 71]))
year.latest.case <- scc1[latest.case, 4]

# get adjacency matrix for this given year
A <- AM.t(time.t, 1)
# dimension of A 
d<- dim(A)[1]

# indicate that A is the observed matrix
A.obs <- A
# matrix is everything but the rows that belong to the cases that entered the network at time.t
A.core<-A[-((d-number.cases.at.t+1):d),]

# issue Area Matrix
IA<- AM.t(time.t, 3)
# MQ score matrix
MQ<- AM.t(time.t, 2)
# year diff matrix
YD<- AM.t(time.t, 4)

##### Gibbs sampling

# if MPLE has been calculated
#theta.old<- modelMPLE$coefficients
theta<- modelMPLE$coefficients

# if MPLE wasn't calculated, set vector to 0
theta<- c(0,0,0, 0, 0, 0)
theta.old<- c(0,0,0, 0, 0, 0) # for optim, this is theta_0

# age
age<- year.latest.case-scc1[17987:latest.case, 4]

# draw m networks
m<-2

# create empty list to save vectors; just save vectors for memory issues, and attach vectors to core matrix A.core later
sampled.vector.list<- list()

for(j in 1:m){
  if(j %% 1 == 0) cat("Simulating network number", j, "\n")
  for(k in (d-number.cases.at.t+1):d){
    if(k %% 1 == 0) cat("Iteration k", k, "\n")
    # calculate change statistics
    for(i in 1:d){
      #print(i)
      #if(i %% 250 == 0) cat("Starting iteration", i, "\n")
      #print(i)
      # define A.plus and A.minus
      A.plus<- A
      A.minus<- A
      A.plus[k,i]<-1
      A.minus[k,i]<-0
      
      triangles.plus<- count.citation.triangles(A.plus,age=age,0.025)
      triangles.minus<- count.citation.triangles(A.minus,age=age,0.025)
      
      outstar.plus<- count.citation.outstars(A.minus,age=age,0.025)
      outstar.minus<- count.citation.outstars(A.minus,age=age,0.025)
      
      issue.plus <- issue.area.stat(A.plus, IA )
      issue.minus <- issue.area.stat(A.minus, IA )
      
      mq.plus<- mq.stat(A.plus, MQ)
      mq.minus<- mq.stat(A.minus, MQ)
      
      year.diff.plus<- year.stat(A.plus, YD)
      year.diff.minus<- year.stat(A.minus, YD)
      
      
      pi<- exp(theta[1]*1+theta[2]*(outstar.plus-outstar.minus)+theta[3]*(triangles.plus-triangles.minus)+ theta[4]*(issue.plus-issue.minus)+
                 theta[5]*(mq.plus-mq.minus)+theta[6]*(year.diff.plus-year.diff.minus))/(1+
                 exp(theta[1]*1+theta[2]*(outstar.plus-outstar.minus)+theta[3]*(triangles.plus-triangles.minus)+ theta[4]*(issue.plus-issue.minus)+
                       theta[5]*(mq.plus-mq.minus)+theta[6]*(year.diff.plus-year.diff.minus)))
      
      # draw one sample from Bin (1,pi)
      Z= rbinom(1,1,pi)
      
      # change vector
      if(Z==1){A[k,i]<- 1}
      if(Z==0){A[k,i]<- 0}
      
    }
    sampled.vector.list[[j]]<- A[(d-number.cases.at.t+1):d,] # just save the rows that can change
  }
}

# end gibbs sampling
###############################################################


###################################################################
# optimzie theta

# create the normalizing constant
xna<- paste("(par[1]-theta.old[1])*edges(cbind(A.core, sampled.vector.list[[", 1:m,"]]))+
            (par[2]-theta.old[2])*count.citation.outstars(cbind(A.core, sampled.vector.list[[",1:m, "]]), age, 0.025)+ 
            (par[3]-theta.old[3])*count.citation.triangles(cbind(A.core, sampled.vector.list[[", 1:m,"]]), age, 0.025)+
            (par[4]-theta.old[4])*issue.area.stat(cbind(A.core, sampled.vector.list[[", 1:m, "]]))+
            (par[5]-theta.old[5])*mq.stat(cbind(A.core, sampled.vector.list[[", 1:m, "]]))+
            (par[6]-theta.old[6])*year.stat(cbind(A.core, sampled.vector.list[[", 1:m, "]]))", sep="", collapse="+" )
fmla <- as.formula(paste("y ~", paste(xna, collapse= "+")))
# fmla[[3]] #right hand side of formula


hat.kappa.theta <- function(data, par){
  
  -(par[1]*edges(A.obs)+ par[2]*count.citation.outstars(A.obs, age, 0.025)+par[3]*count.citation.triangles(A.obs, age, 0.025)+
      par[4]*issue.area.stat(A.obs, IA)+ par[5]*mq.stat(A.obs, MQ)+ par[6]*year.stat(A.obs, YD)-
      log(as.expression(fmla[[3]])) )    
}


optim(par=theta, hat.kappa.theta, data=sampled.vector.list)


#####################################################################
#####################################################################
########## 2nd try
#####################################################################
#####################################################################


# set time point, range [3136, 5251]
time.t<- 3240
number.cases.at.t<- length(which(time.t==scc1[, 71]))
latest.case<- max(which(time.t==scc1[, 71]))
year.latest.case <- scc1[latest.case, 4]

# get adjacency matrix for this given year
A <- AM.t(time.t, 1)
# dimension of A 
d<- dim(A)[1]

# indicate that A is the observed matrix
A.obs <- A
# matrix is everything but the rows that belong to the cases that entered the network at time.t
A.core<-A[-((d-number.cases.at.t+1):d),]

# issue Area Matrix
IA<- AM.t(time.t, 3)
# MQ score matrix
MQ<- AM.t(time.t, 2)
# year diff matrix
YD<- AM.t(time.t, 4)

#################
# MCMLE

# initialize theta_0
theta_0 <- c(-5.2, -3.5, -3, 0.5, -0.28, -0.19)

# age
age<- year.latest.case-scc1[17987:latest.case, 4]

# draw m networks using Gibbs sampling
m <-2

# create empty list to save vectors; just save vectors for memory issues, and attach vectors to core matrix A.core later
sampled.vector.list<- list()

for(j in 1:m){
  if(j %% 1 == 0) cat("Simulating network number", j, "\n")
  for(k in (d-number.cases.at.t+1):d){
    if(k %% 1 == 0) cat("Iteration k", k, "\n")
    # calculate change statistics
    for(i in 1:d){
      #print(i)
      #if(i %% 250 == 0) cat("Starting iteration", i, "\n")
      #print(i)
      # define A.plus and A.minus
      A.plus<- A
      A.minus<- A
      A.plus[k,i]<-1
      A.minus[k,i]<-0
      
      triangles.plus<- count.triangles(A.plus,age=age,0.025)
      triangles.minus<- count.triangles(A.minus,age=age,0.025)
      
      outstar.plus<- count.outstars(A.minus,age=age,0.025)
      outstar.minus<- count.outstars(A.minus,age=age,0.025)
      
      issue.plus <- issue.area.stat(A.plus, IA )
      issue.minus <- issue.area.stat(A.minus, IA )
      
      mq.plus<- mq.stat(A.plus, MQ)
      mq.minus<- mq.stat(A.minus, MQ)
      
      year.diff.plus<- year.stat(A.plus, YD)
      year.diff.minus<- year.stat(A.minus, YD)
      
      
      pi<- exp(theta_0[1]*1+theta_0[2]*(outstar.plus-outstar.minus)+theta_0[3]*(triangles.plus-triangles.minus)+ theta_0[4]*(issue.plus-issue.minus)+
                 theta_0[5]*(mq.plus-mq.minus)+theta_0[6]*(year.diff.plus-year.diff.minus))/
        (1+ exp(theta_0[1]*1+theta_0[2]*(outstar.plus-outstar.minus)+theta_0[3]*(triangles.plus-triangles.minus)+ 
                  theta_0[4]*(issue.plus-issue.minus)+theta_0[5]*(mq.plus-mq.minus)+theta_0[6]*(year.diff.plus-year.diff.minus)))
      
      # draw one sample from Bin (1,pi)
      Z= rbinom(1,1,pi)
      
      # change vector
      if(Z==1){A[k,i]<- 1}
      if(Z==0){A[k,i]<- 0}
      
    }
    sampled.vector.list[[j]]<- A[(d-number.cases.at.t+1):d,] # just save the rows that can change
  }
}

# end gibbs sampling

###############
# Calculation of Gamma_m, a m x 6 matrix of network statistics for the simulated networks

# create Gamma_m
gamma_m<- matrix(0, m, 6)
colnames(gamma_m)<- c("edges", "outstars", "triangles", "issue.are", "martin.quinn", "year")
rownames(gamma_m)<- 1:m

# fill gamma_m with values
for(i in 1:m){
  N<- rbind(A.core, sampled.vector.list[[i]])
  gamma_m[i,1]<- sum(N)
  gamma_m[i,2]<- count.outstars(N,age=age,0.025)
  gamma_m[i,3]<- count.triangles(N,age=age,0.025)
  gamma_m[i,4]<- issue.area.stat(N, IA )
  gamma_m[i,5]<- mq.stat(N, MQ)
  gamma_m[i,6]<- year.stat(N, YD)
}

# create vector Gamma_N with values of the observed network
gamma_N<- matrix(0, 1, 6)
colnames(gamma_N)<- c("edges", "outstars", "triangles", "issue.are", "martin.quinn", "year")

gamma_N[1,1]<- sum(A.obs)
gamma_N[1,2]<- count.outstars(A.obs,age=age,0.025)
gamma_N[1,3]<- count.triangles(A.obs,age=age,0.025)
gamma_N[1,4]<- issue.area.stat(A.obs, IA )
gamma_N[1,5]<- mq.stat(A.obs, MQ)
gamma_N[1,6]<- year.stat(A.obs, YD)


# function for optim

ercm_iter<- function(theta, gamma_m, gamma_N, theta_0){
  -sum(c(theta[1],theta[2],theta[3],theta[4],theta[5], theta[6])*gamma_N)+log(sum(exp(gamma_m%*%(c(theta[1],theta[2],theta[3],theta[4],theta[5], theta[6])- theta_0))))
}

optim(par=theta, fn= ercm_iter, gamma_N=gamma_N, gamma_m=gamma_m, theta_0=theta_0, method="BFGS")
