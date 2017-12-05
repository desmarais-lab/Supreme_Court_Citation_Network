# ERCM bootstrapped MPLE


# requires scc1 from scc_data_merging


########################################################
# network statistics
########################################################


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
##############################################################

memory.limit(30000)
##############################################################
# MQ score

mq.matrix <- matrix(rep(scc1[,72], 8817), 8817, 8817)

mq.stat <- function(A, MQ){
  sum(A*MQ, na.rm=TRUE)
}


##############################################################
# issue area

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

#########################################################
### year difference

year.diff.matrix<- matrix(0, 8817, 8817)

for(i in 17987:26803){ # calculation takes a while
  if(i %% 100 == 0) cat("Starting iteration", i, "\n")
  # year for sender
  year.sender <- scc[i, 4]
  for(j in 17987:26803){
    # year for receiver
    year.receiver <- scc[j, 4]
    year.diff.matrix[i-17986,j-17986]<- abs(year.sender- year.receiver)
  }
}

year.stat <- function(A, YD){
  sum(A*YD, na.rm=TRUE)
}

###########################################################
### calculate matrices for supreme court justices, e.g. (ph(I(i="Hughes)))
 #C.E. Hughes is baseline

# H.F.Stone
HFStone<- mq.matrix
for(i in 1:8817){
  if(scc1[i,74]==0){
    HFStone[i,]<- 0
  }
}

# F.M. Vinson
FMVinson<-mq.matrix
for(i in 1:8817){
  if(scc1[i,75]==0){
    FMVinson[i,]<- 0
  }
}

# E. Warren
EWarren<- mq.matrix
for(i in 1:8817){
  if(scc1[i,76]==0){
    EWarren[i,]<- 0
  }
}
# W.E. Burger
WEBurger<- mq.matrix
for(i in 1:8817){
  if(scc1[i,77]==0){
    WEBurger[i,]<- 0
  }
}

# W.Rehnquist
WRehnquist<-mq.matrix
for(i in 1:8817){
  if(scc1[i,78]==0){
    WRehnquist[i,]<- 0
  }
}

############################################################
###   Citation Network
############################################################

# get adjacency.matrix from ERCM 3

# I decided to condition on the 10 years from 1937-1946 when Hughes and Stone were chief justices: lines 1:1395 in scc1, id's 1:213

A<- adjacency.matrix

# matrix to save estimated coefficients and CI
Results<- list()


###########################################################
# calculation of matrix of change statistics

# first for the core matrix (cases/years that are conditioned on, see line 124)
change.mat<- matrix(0,1,12)
colnames(change.mat)<- c("citation", "outstar", "triangle", "mqscore", "issuearea", "yeardiff",
                         "HFStone", "FMVinson", "EWarren", "WEBurger", "WRehnquist", "time.t")

for(u in 91:150){ # 1:213 <- time points that are conditioned on
  print(u)
  time.t<- u
  number.cases.at.t<- length(which(time.t==scc1[, 71]))
  latest.case<- max(which(time.t==scc1[, 71]))
  year.latest.case <- scc1[latest.case, 4]
  
  # age vector for yearly decay in outstar and triangle statistic
  age<- year.vector[latest.case]-year.vector[1:latest.case]
  
  # create empty matrices to store change statistics
  triangle.change<- matrix(0, number.cases.at.t, latest.case)
  outstar.change<- matrix(0, number.cases.at.t, latest.case)
  mq.score.change<- matrix(0, number.cases.at.t, latest.case)
  issue.area.change<- matrix(0, number.cases.at.t, latest.case)
  year.diff.change<-matrix(0, number.cases.at.t, latest.case)
  hfstone.change<- matrix(0, number.cases.at.t, latest.case)
  fmvinson.change<- matrix(0, number.cases.at.t, latest.case)
  ewarren.change<- matrix(0, number.cases.at.t, latest.case)
  weburger.change<- matrix(0, number.cases.at.t, latest.case)
  wrehnquist.change<- matrix(0, number.cases.at.t, latest.case)
  
  # cut out corresponding entries in adjacency.matrix
  if(number.cases.at.t==1){
  AM<- t(as.matrix(adjacency.matrix[((latest.case-number.cases.at.t+1):latest.case), 1:latest.case]))
  } else{
  AM<- adjacency.matrix[((latest.case-number.cases.at.t+1):latest.case), 1:latest.case]
  }
  
  # calculate and save change statistics 
  for(i in (latest.case-number.cases.at.t+1):latest.case){
  for (j in 1:latest.case){ 
    
    if(i!=j){
    A.plus<- A[1:latest.case, 1:latest.case]
    A.minus<- A[1:latest.case, 1:latest.case]
    A.plus[i,j]<-1
    A.minus[i,j]<-0
    
    # MQscore Matrix with same dimensions as A.plus and A.minus
    MQ<- mq.matrix[1:latest.case, 1:latest.case]
    
    # Issue Area Matrix with same dimensions as A.plus and A.minus
    IA<- same.issue.area[1:latest.case, 1:latest.case]
    
    # Issue Area Matrix with same dimensions as A.plus and A.minus
    YD<- year.diff.matrix[1:latest.case, 1:latest.case]
    
    # chief justice matrices with same dimensions as A.plus and A.minus
    HFS<- HFStone[1:latest.case, 1:latest.case]
    FMV<- FMVinson[1:latest.case, 1:latest.case]
    EW<- EWarren[1:latest.case, 1:latest.case]
    WEB<- WEBurger[1:latest.case, 1:latest.case]
    WR<- WRehnquist[1:latest.case, 1:latest.case]
    
    # year diff
    age<- year.vector[latest.case]-year.vector[1:latest.case]
    
    # change statistic triangles
    triangles.plus<- count.triangles.sim(A.plus,age=age,0.025, i=i)
    triangles.minus<- count.triangles.sim(A.minus,age=age,0.025, i=i)
    triangle.change[i-latest.case+number.cases.at.t,j]<- triangles.plus-triangles.minus # add value into right row in change.mat
    
    # change statistic outstar
    outstar.plus<- count.outstars.sim(A.plus,age=age,0.025, i=i)
    outstar.minus<- count.outstars.sim(A.minus,age=age,0.025, i=i)
    outstar.change[i-latest.case+number.cases.at.t,j]<- outstar.plus-outstar.minus # add value into right row in change.mat
    
    # change statistic MQ score
    mq.plus<- mq.stat(A.plus, MQ)
    mq.minus<- mq.stat(A.minus, MQ)
    mq.score.change[i-latest.case+number.cases.at.t,j]<- mq.plus-mq.minus
    
    # change.statistic Issue Area
    ia.plus<- issue.area.stat(A.plus, IA)
    ia.minus<- issue.area.stat(A.minus, IA)
    issue.area.change[i-latest.case+number.cases.at.t,j]<- ia.plus-ia.minus
    
    # change.statistic Year Diff
    yd.plus<- year.stat(A.plus, YD)
    yd.minus<- year.stat(A.minus, YD)
    year.diff.change[i-latest.case+number.cases.at.t,j]<- yd.plus-yd.minus
    
    # change statistic for chief justices
    stone.plus<- mq.stat(A.plus, HFS)
    stone.minus<- mq.stat(A.minus, HFS)
    hfstone.change[i-latest.case+number.cases.at.t,j]<- stone.plus-stone.minus
    
    vinson.plus<- mq.stat(A.plus, FMV)
    vinson.minus<- mq.stat(A.minus, FMV)
    fmvinson.change[i-latest.case+number.cases.at.t,j]<- vinson.plus-vinson.minus
    
    warren.plus<- mq.stat(A.plus, EW)
    warren.minus<- mq.stat(A.minus, EW)
    ewarren.change[i-latest.case+number.cases.at.t,j]<- warren.plus-warren.minus
    
    burger.plus<- mq.stat(A.plus, WEB)
    burger.minus<- mq.stat(A.minus, WEB)
    weburger.change[i-latest.case+number.cases.at.t,j]<- burger.plus-burger.minus
    
    rehnquist.plus<- mq.stat(A.plus, WR)
    rehnquist.minus<- mq.stat(A.minus, WR)
    wrehnquist.change[i-latest.case+number.cases.at.t,j]<- rehnquist.plus-rehnquist.minus
    
    } # end if
    else{
      triangle.change[i-latest.case+number.cases.at.t,j]<- NA
      outstar.change[i-latest.case+number.cases.at.t,j]<- NA
      AM[i-latest.case+number.cases.at.t,j] <- NA
      mq.score.change[i-latest.case+number.cases.at.t,j]<- NA
      issue.area.change[i-latest.case+number.cases.at.t,j]<- NA
      year.diff.change[i-latest.case+number.cases.at.t,j]<- NA
      hfstone.change[i-latest.case+number.cases.at.t,j]<- NA
      fmvinson.change[i-latest.case+number.cases.at.t,j]<- NA
      ewarren.change[i-latest.case+number.cases.at.t,j]<- NA
      weburger.change[i-latest.case+number.cases.at.t,j]<- NA
      wrehnquist.change[i-latest.case+number.cases.at.t,j]<- NA
        }
  } # end j
  } # end i
  
  # delete loops
  AM.vec<- as.vector(t(AM))
  AM.vec <- AM.vec[!is.na(AM.vec)]
  
  triangle.change.vec<- as.vector(t(triangle.change))
  triangle.change.vec<- triangle.change.vec[!is.na(triangle.change.vec)]
  
  outstar.change.vec<- as.vector(t(outstar.change))
  outstar.change.vec<- outstar.change.vec[!is.na(outstar.change.vec)]
  
  mq.score.change.vec<- as.vector(t(mq.score.change))
  mq.score.change.vec<- mq.score.change.vec[!is.na(mq.score.change.vec)]
  
  issue.area.change.vec<- as.vector(t(issue.area.change))
  issue.area.change.vec<- issue.area.change.vec[!is.na(issue.area.change.vec)]
  
  year.diff.change.vec<- as.vector(t(year.diff.change))
  year.diff.change.vec<- year.diff.change.vec[!is.na(year.diff.change.vec)]
  
  hfstone.change.vec<- as.vector(t(hfstone.change))
  hfstone.change.vec<- hfstone.change.vec[!is.na(hfstone.change.vec)]
  
  fmvinson.change.vec<- as.vector(t(fmvinson.change))
  fmvinson.change.vec<- fmvinson.change.vec[!is.na(fmvinson.change.vec)]
  
  ewarren.change.vec<- as.vector(t(ewarren.change))
  ewarren.change.vec<- ewarren.change.vec[!is.na(ewarren.change.vec)]
  
  weburger.change.vec<- as.vector(t(weburger.change))
  weburger.change.vec<- weburger.change.vec[!is.na(weburger.change.vec)]
  
  wrehnquist.change.vec<- as.vector(t(wrehnquist.change))
  wrehnquist.change.vec<- wrehnquist.change.vec[!is.na(wrehnquist.change.vec)]
  
  # length of vector
  l<- length(triangle.change.vec)
  
  # auxiliary matrix to combine with change.mat
  aux.mat <- matrix(0, l, 12)
  
  aux.mat[,1]<- AM.vec
  aux.mat[,3]<- triangle.change.vec
  aux.mat[,2]<- outstar.change.vec
  aux.mat[,4]<- mq.score.change.vec
  aux.mat[,5]<- issue.area.change.vec
  aux.mat[,6]<- year.diff.change.vec
  aux.mat[,7]<- hfstone.change.vec
  aux.mat[,8]<- fmvinson.change.vec
  aux.mat[,9]<- ewarren.change.vec
  aux.mat[,10]<- weburger.change.vec
  aux.mat[,11]<- wrehnquist.change.vec
  aux.mat[,12]<- u
  
  change.mat<- rbind(change.mat, aux.mat)
  
  #delete first initial row
  if(u==1){
    change.mat<- change.mat[-1,]
  }
  write.csv(change.mat, file = "changemat.csv")
  }


#save as data.frame
change.dat<- as.data.frame(change.mat)

# fit logistic regression

modelMPLE<- glm(citation~outstar+triangle+issuearea+mqscore+yeardiff, data=change.dat, family="binomial")
summary(modelMPLE)
Results[[u]]<- summary(modelMPLE)

##### preparation for non parametric bootstrapping

# nonparametric bootstrap resampling
nboot <- 100 # this number should be 200 or higher
ncoef <- length(coef(modelMPLE))
boot.results <- matrix(NA,nboot,ncoef)
# need to weight by the number of potential edges in a time period
n.per.time <- table(change.mat[,12])
# get unique sender times, over which to weight
unique.sender.times <- as.numeric(names(n.per.time))
# calculate weights for weighted resampling of times
time.weights <- n.per.time/sum(n.per.time)

set.seed(444)
# loop over bootstrap iterations (easily paralellize)
for(b in 1:nboot){
  print(b)
  # sample time periods with replacement
  time.periods.b <- sample(unique.sender.times,length(unique.sender.times),prob=time.weights,rep=T)
  # construct the data for iteration b
  data.b <- list()
  for(k in 1:length(time.periods.b)){
    t<- time.periods.b[k]
    data.b[[k]] <- subset(change.mat,change.mat[,12]==t)
  }
  # collapse into a matrix
  data.b <- do.call('rbind',data.b)
  # convert to data frame
  data.b <- data.frame(data.b)
  # run mple
  mple.est.b <- glm(citation~outstar+triangle+issuearea+mqscore+yeardiff, data=data.b, family="binomial")
  # store results
  boot.results[b,] <- coef(mple.est.b)
  
}


### take percentiles
quantile(boot.results[,1], probs=c(0.975, 0.025))
quantile(boot.results[,2], probs=c(0.975, 0.025))
quantile(boot.results[,3], probs=c(0.975, 0.025))
quantile(boot.results[,4], probs=c(0.975, 0.025))
quantile(boot.results[,5], probs=c(0.975, 0.025))
quantile(boot.results[,6], probs=c(0.975, 0.025))
