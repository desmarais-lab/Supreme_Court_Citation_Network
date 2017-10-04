# requires scc_data_merging

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

AM.t<- function(time.t){ # first id with MQ scores is 3136, last 5251
  # what is the id of the case/cases that entered the network at time t
  latest.case<- max(which(time.t==scc1[, 71]))
  AM<- adjacency.matrix[1:(latest.case-17986), 1:(latest.case-17986)]
  return(AM)
}


#### statistics
time.t<- 5251
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


A<- AM.t(time.t)
##################################
### triangles

# transpose matrix before applying to this function
count.citation.triangles <- function(A,age,beta){
  # Input is an adjacency matrix
  # weight edge by the age of the sender
  ageMat <- matrix(decayFun(age,beta),nrow(A),nrow(A),byrow=T)
  A <- t(A)*ageMat
  B <- t(A)%*%A
  sum(A*B) 
}

# age is been calculated as the year of the latest case minus the year of the previous case, years can be found in column 4 of scc1
count.citation.triangles(A, year.latest.case-scc1[17987:latest.case, 4], 0.025)


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


####################################
### edges


# number of edges statistic
edges<- function(A){
  sum(A)
}

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

# estimate coefficients for time t=1431 | condition on years before 1869

# get vector c1431
time.t<- 1000
number.cases.at.t<- length(which(time.t==scc1[, 71]))
latest.case<- max(which(time.t==scc1[, 71]))
year.latest.case <- scc1[latest.case, 4]

# get adjacency matrix for this given year
A <- AM.t(time.t)

# create matrix to store results, such that we can fit a logistic regression model; edges is intercept -> no column for edges

change.mat<- matrix(0, number.cases.at.t*latest.case, 3) # 3 statistics (edges, outstars, triangles)
colnames(change.mat)<- c("citation", "outstar", "triangle")

# fill in right values
d<- dim(A)[1]

change.mat[,1]<- c(A[,(d-number.cases.at.t+1):d])

age<- year.latest.case-scc1[1:latest.case, 4]
# change statistic matrix
for(j in (d-number.cases.at.t+1):d){
  # calculate change statistics
  for(i in 1:latest.case){
    #print(i)
    if(i %% 250 == 0) cat("Starting iteration", i, "\n")
    #print(i)
    A.plus<- A
    A.minus<- A
    A.plus[i,j]<-1
    A.minus[i,j]<-0
    
    triangles.plus<- count.citation.triangles(A.plus,age=age,0.025)
    triangles.minus<- count.citation.triangles(A.minus,age=age,0.025)
    change.mat[(j-1)*latest.case+i,3]<- triangles.plus-triangles.minus
    
    outstar.plus<- count.citation.outstars(A.minus,age=age,0.025)
    outstar.minus<- count.citation.outstars(A.minus,age=age,0.025)
    change.mat[(j-1)*latest.case+i,2]<- outstar.plus-outstar.minus
    
    # loops are not considered
    if(i==j){change.mat[(j-1)*latest.case+i,2:3]<- 0}
    
  }
}

#save as data.frame
change.dat<- as.data.frame(change.mat)

# fit logistic regression

modelMPLE<- glm(citation~outstar+triangle, data=change.dat, family="binomial")
summary(modelMPLE)


################################################
## MCMLE



time.t<- 200
number.cases.at.t<- length(which(time.t==scc1[, 71]))
latest.case<- max(which(time.t==scc1[, 71]))
year.latest.case <- scc1[latest.case, 4]

# get adjacency matrix for this given year
A <- AM.t(time.t)
A.obs <- A
A.core<-A[,-((d-number.cases.at.t+1):d)]

# dimension of A 
d<- dim(A)[1]

##### Gibbs sampling

# set theta vector
#theta<- rep(0,3)
# if MPLE has been calculated
#theta.old<- modelMPLE$coefficients
theta<- modelMPLE$coefficients

# ow
theta<- c(0,0,0)
theta.old<- c(0,0,0) # for optim, this is theta_0

# age
age<- year.latest.case-scc1[1:latest.case, 4]

# draw m networks
m<-2

# create empty list to save vectors; just save vectors for memory issues, and attach vectors to adjacency matrix later
sampled.vector.list<- list()

for(j in 1:m){
  if(j %% 1 == 0) cat("Iteration j", j, "\n")
  for(k in (d-number.cases.at.t+1):d){
    if(k %% 1 == 0) cat("Iteration k", k, "\n")
    # calculate change statistics
    for(i in 1:latest.case){
      print(i)
      if(i %% 250 == 0) cat("Starting iteration", i, "\n")
      #print(i)
      A.plus<- A
      A.minus<- A
      A.plus[i,k]<-1
      A.minus[i,k]<-0
      
      triangles.plus<- count.citation.triangles(A.plus,age=age,0.025)
      triangles.minus<- count.citation.triangles(A.minus,age=age,0.025)
      
      outstar.plus<- count.citation.outstars(A.minus,age=age,0.025)
      outstar.minus<- count.citation.outstars(A.minus,age=age,0.025)
      
      pi<- exp(theta[1]*1+theta[2]*(outstar.plus-outstar.minus)+theta[3]*(triangles.plus-triangles.minus))/(1+exp(theta[1]*1+theta[2]*(outstar.plus-outstar.minus)+theta[3]*(triangles.plus-triangles.minus)))
      
      # draw from Bin (1,pi)
      Z= rbinom(1,1,pi)
      
      # change vector
      if(Z==1){A[i,k]<- 1}
      if(Z==0){A[i,k]<- 0}
      
    }
    sampled.vector.list[[j]]<- A[,(d-number.cases.at.t+1):d]
  }
}

# end gibbs sampling
###############################################################


###################################################################
# optimzie theta


hat.kappa.theta <- function(data, par){
  
  -(par[1]*edges(A.obs)+ par[2]*count.citation.outstars(A.obs, age, 0.025)+par[3]*count.citation.triangles(A.obs, age, 0.025)-
      log(exp((par[1]-theta.old[1])*edges(cbind(A.core, sampled.vector.list[[1]]))+(par[2]-theta.old[2])*count.citation.outstars(cbind(A.core, sampled.vector.list[[1]]), age, 0.025)+
                (par[3]-theta.old[3])*count.citation.triangles(cbind(A.core, sampled.vector.list[[1]]), age, 0.025))+
            exp((par[1]-theta.old[1])*edges(cbind(A.core, sampled.vector.list[[2]]))+(par[2]-theta.old[2])*count.citation.outstars(cbind(A.core, sampled.vector.list[[2]]), age, 0.025)+
                  (par[3]-theta.old[3])*count.citation.triangles(cbind(A.core, sampled.vector.list[[2]]), age, 0.025))) )    
}


optim(par=theta, hat.kappa.theta, data=sampled.vector.list)



