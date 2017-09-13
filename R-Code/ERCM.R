library(lubridate)

scc1<-scc
# first sort data frame by decision date
scc1<-scc1[order(as.Date(scc1$dateDecision, format="%m/%d/%Y")),]

# time is day-discrete -> turn date into number of days that have passed since the first case

x <- scc1$dateDecision
#transform date into yyyy-mm-dd
date <- mdy(x)

days <-as.numeric(date)+64970
scc1$time_t<- days

# assign same date the same number, save in column 'id'
scc1 <- transform(scc1,id=as.numeric(factor(time_t)))


# create a large matrix where every column refers to a time t and every row refers to a scc case
citation.matrix<- matrix(0, 26803, 5251) # matrix for only the first case, we add the columns one by one
colnames(citation.matrix)<-1:5251

for(i in 1:209946){
  if(i %% 1000 == 0) cat("Starting iteration", i, "\n")
  # which line can the information of case edgelist[i,1] be found in?
  l<- which(edgelist[i,1]==scc1[,1])
  # which line can the information of case edgelist[i,2] be found in? used to count how often a case is being cited
  q<- which(edgelist[i,2]==scc1[,1])
  # which time_t was citation made?
  t<- scc1[l, 71]
  citation.matrix[q,t]<-1
}

#### Statistics

## down-weighted triangle statistic 1787

triangles<- function(c_t, time_t){
  # get right colum of time t
  #col.is.a.match <- apply(citation.matrix, 2, identical, c_t)
  #time_t<- which(col.is.a.match)
  
  # which rows have an entry 1
  citation.row<- which(c_t == 1)
  length.citation.row <- length(citation.row)
  
  #set tri.count =0
  tri.count<- 0
  # count number of triangles
  for(i in 1:(length.citation.row-1)){
    for(j in (i+1):length.citation.row){
      # what is time_t of later citation?
      late_cit<- scc1[citation.row[j], 71]
      #### year difference between cases
      # which row is case in
      year.row<- which(scc1[,71]==time_t)
      # save dates
      year.c_t <- scc1[year.row[1], 4]
      year.c_t_a <- scc1[citation.row[j],4]
      year.c_t_a_b <- scc1[citation.row[i],4]
      # does j cite i?
      tri.count<- tri.count+ (citation.matrix[citation.row[i],late_cit])*sqrt(((year.c_t-year.c_t_a+1)*(year.c_t-year.c_t_a_b+1)/year.c_t^2))
      }
    }
  return(tri.count)
}

# number of edges statistic
edges<- function(c_t){
  number.edges<- sum(c_t)
  return(number.edges)
}

# down-weighted outstar statistic
outstar<- function(c_t, time_t){
  #col.is.a.match <- apply(citation.matrix, 2, identical, c_t)
  #time_t<- which(col.is.a.match)
  
  # which rows have an entry 1
  citation.row<- which(c_t == 1)
  length.citation.row <- length(citation.row)
  
  #set ost.count =0
  ost.count<- 0
  for(i in 1:(length.citation.row-1)){
    for(j in (i+1):length.citation.row){
      #### year difference between cases
      # which row is case in
      year.row<- which(scc1[,71]==time_t)
      # save dates
      year.c_t <- scc1[year.row[1], 4]
      year.c_t_a <- scc1[citation.row[j],4]
      year.c_t_a_b <- scc1[citation.row[i],4]
      
      ost.count <- ost.count + ((year.c_t-year.c_t_a+1)*sqrt((year.c_t-year.c_t_a_b+1)/year.c_t^2))
      
      }
  }
  return(ost.count)
}

############### Estimation

# Pseudolikelihood

# estimate coefficients for time t=1431 | condition on years before 1869

# get vector c1431
time.t<- 5000

cit.mat.row<- which(scc1[,71]==time.t) # could be multiple rows
#c1431<- citation.matrix[1:max(cit.mat.row), 1431] 
ct<- citation.matrix[,time.t]

# create matrix to store results, such that we can fit a logistic regression model; edges is intercept -> no column for edges

change.mat<- matrix(0, max(cit.mat.row), 3)
colnames(change.mat)<- c("citation", "outstar", "triangle")

# fill in right values

change.mat[,1]<- ct[1:max(cit.mat.row)]

# calculate change statistics
for(i in 1:max(cit.mat.row)){
  if(i %% 250 == 0) cat("Starting iteration", i, "\n")
  #print(i)
  c_t.plus<- ct
  c_t.minus<- ct
  c_t.plus[i]<-1
  c_t.minus[i]<-0
  
  triangles.plus<- triangles(c_t.plus, time.t)
  triangles.minus<- triangles(c_t.minus, time.t)
  change.mat[i,3]<- triangles.plus-triangles.minus
  
  outstar.plus<- outstar(c_t.plus, time.t)
  outstar.minus<- outstar(c_t.minus, time.t)
  change.mat[i,2]<- outstar.plus-outstar.minus
  
}

# exclude the last entry (can't cite yourself) if only one case enters network at time t
if(length(cit.mat.row)==1){
change.mat<- change.mat[-cit.mat.row,]
}


#save as data.frame
change.dat<- as.data.frame(change.mat)

# fit logistic regression

model5000<- glm(citation~outstar+triangle, data=change.dat, family="binomial")
summary(model5000)
