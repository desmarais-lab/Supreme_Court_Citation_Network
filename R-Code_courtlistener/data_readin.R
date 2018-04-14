memory.limit(30000)


# reading and preprocessing all the data takes a couple of hours (4-5h)


#####################################################
#### read in Washington law data
#####################################################

wash.law1<- read.csv("SCDB1.csv", stringsAsFactors = F)
wash.law2<- read.csv("SCDB2.csv", stringsAsFactors = F)
# combine data sets

wash.law<- rbind(wash.law1, wash.law2)

#####################################################
#### read in courtlistener citation csv
#####################################################

library(readr)
citations <- read_csv("cl_citations.csv")
citations<- as.data.frame(citations)

#######################################################
#### read in Courtlistener scotus opinion data
#######################################################

library(rjson)
# takes about 5 mins
# filenames_op <- list.files("C:/Users/Geiler Typ/Desktop/PSU/Bruce Desmarais/Supreme Court Citation Project/scotus_opinion", pattern="*.json", full.names=TRUE) # this should give you a character vector, with each file name represented by an entry
# takes about 30 mins
# myJSON_opinion <- lapply(filenames_op, function(x) fromJSON(file=x)) # a list in which each element is one of the original JSON files, takes a while ~ 30 min

######################################################
#### read in courtlistener scotus cluster-based data

filenames_cluster <- list.files("C:/Users/Geiler Typ/Desktop/PSU/Bruce Desmarais/Supreme Court Citation Project/R-Code_courtlistener_data/all_cluster/scotus", pattern="*.json", full.names=TRUE) # this should give you a character vector, with each file name represented by an entry
# takes about 30 mins
myJSON_cluster <- lapply(filenames_cluster, function(x) fromJSON(file=x)) # a list in which each element is one of the original JSON files, takes a while ~ 30 min

l.cluster<- length(myJSON_cluster) # 63967

#########################
# create matrix that takes the important information from the JSON cluster files

cluster.mat <- matrix(NA, l.cluster, 7)
colnames(cluster.mat)<- c("resource_uri", "docket", "date_filed", "citation_id", "federal_cite_one", "scdb_id", "sub_opinions")

# turn into data frame
cluster.mat <- as.data.frame(cluster.mat)

# read in data into matrix
for(i in 1:l.cluster){
  #print(i)
  if(i %% 1000 == 0) cat("Starting iteration", i, "\n")
  l<- length(myJSON_cluster[[i]])
  for(j in 1:l){
    #print(j)
    w<- which(names(myJSON_cluster[[i]])[j]== colnames(cluster.mat)) 
    if(myJSON_cluster[[i]][j]!="list()" & myJSON_cluster[[i]][j]!="NULL"){
      cluster.mat[i,w]<- myJSON_cluster[[i]][j]
    }
  } 
}

rm(i,j,l,w)

# extract opinion_id and docket number from 'resource_uri' and 'docket'
cluster.mat[,1]<- gsub("http://www.courtlistener.com/api/rest/v3/clusters/", "", cluster.mat[,1], fixed = TRUE)
cluster.mat[,1]<- gsub("/", "", cluster.mat[,1], fixed = TRUE)

cluster.mat[,2]<- gsub("http://www.courtlistener.com/api/rest/v3/dockets/", "", cluster.mat[,2], fixed = TRUE)
cluster.mat[,2]<- gsub("/", "", cluster.mat[,2], fixed = TRUE)

cluster.mat[,7]<- gsub("http://www.courtlistener.com/api/rest/v3/opinions/", "", cluster.mat[,7], fixed = TRUE)
cluster.mat[,7]<- gsub("/", "", cluster.mat[,7], fixed = TRUE)

# indicate entries as integer and not character
cluster.mat[,1]<- as.integer(cluster.mat[,1])
cluster.mat[,2]<- as.integer(cluster.mat[,2])
cluster.mat[,4]<- as.integer(cluster.mat[,4])
cluster.mat[,7]<- as.integer(cluster.mat[,7])


#######################################
### add  days and time.id to data such that we can indicate which cases enter the network on which day
library(lubridate)

scc<- wash.law
scc1<-scc
# first sort data frame by decision date
scc1<-scc1[order(as.Date(scc1$dateDecision, format="%m/%d/%Y")),]

# time is day-discrete -> turn date into number of days that have passed since the first case

x <- scc1$dateDecision
#transform date into yyyy-mm-dd
date <- mdy(x)

#start days at 1
days <-as.numeric(date)+64970
scc1$time_t<- days

# assign same date the same number, save in column 'id'
scc1 <- transform(scc1,id=as.numeric(factor(time_t)))

# create a new column with year of decision
scc1$year<- as.integer(substr(date, 1,4))

scc<- scc1
rm(scc1, days, date, x)


###################################################
######## MQ scores
#### read in justice centered data set

library(readr)
SCDB1_justice <- read_csv("SCDB1_justice.csv")
SCDB2_justice <- read_csv("SCDB2_justice.csv")

SCDB1_justice<- as.data.frame(SCDB1_justice)
SCDB2_justice<- as.data.frame(SCDB2_justice)

# remove space and dots in "1 U.S. 239" to match it with citeid in node_attributes
uscite1<- gsub(" ", "", SCDB1_justice[,7], fixed = TRUE)
uscite1<- gsub(".", "", uscite1, fixed=TRUE)

uscite2<- gsub(" ", "", SCDB2_justice[,7], fixed = TRUE)
uscite2<- gsub(".", "", uscite2, fixed=TRUE)

SCDB1_justice[,7]<- uscite1
SCDB2_justice[,7]<- uscite2

# combine SCDB1_justice and SCDB2_justice into one data set

SCDB_justice <- rbind(SCDB2_justice, SCDB1_justice)
rm(SCDB1_justice, SCDB2_justice)

# set NA==0 (doesn't do any harm, data is merely used to merge MQ scores)
SCDB_justice[is.na(SCDB_justice)]<-0

# read in Martin Quinn scores

MQ.scores <- as.data.frame(read_csv("justices.csv"))


######
# add a column with the median MS score of the judges of the majority vote
#####

scc1<- scc

# Let scc be the full data set and scc1 be the data from term 1937 on


# delete all entries prior 1937
last.1937<- max(which(scc1$term==1936))
scc1<- scc1[-(1:last.1937),]
# MQ scores end at 2015, delete cases after 2015 term
last.2016<- max(which(scc1$term==2016))
first.2016<- min(which(scc1$term==2016))
scc1<- scc1[-(first.2016:last.2016),]





scc1$medianMSscores<- 0
d<- dim(scc1)[1]
rownames(scc1)<- 1:d

set.pro <- c(1,3,4,5,8) # 8 is split vote. this means for split votes, we simply take the MQ average

for ( i in 1:d){ # row 18437 is when term 1937 begins (1937 is beginning of MQ score data)
  if(i %% 1000 == 0) cat("Starting Iteration", i, "\n")
  # which columns relate to case i in the 
  case.rows<- which(scc1[i,1]==SCDB_justice[,1])
  number.judges<- length(case.rows)
  
  # create empty vector to calculate the median MS score of majority judges
  MQ.score<- c() # save MQ scores in vector -> take median later
  for(j in 1:number.judges){
    
    if(is.element(SCDB_justice[case.rows[j], 56], set.pro)){ # 1 indicates voted pro
      # get id of the justice
      justice.id <- SCDB_justice[case.rows[j],54] # column 54 is justice id
      #print(justice.id)
      # get year of term
      case.year <- scc1[i,11] # column 11 is term
      #print(case.year)
      # get row from which to take MS.score based on year and ID
      row.with.MQ.score <- which(MQ.scores[,1]==case.year & MQ.scores[,2]==justice.id)
      #print(row.with.MQ.score)
      if(length(row.with.MQ.score)==1){
        MQ.score[j]<- MQ.scores[row.with.MQ.score,4]
        #print(total.MQ.score)
      }
    }
    
  }
  # save mean MQ score in scc1
  scc1$medianMSscores[i]<- median(MQ.score, na.rm=TRUE)
  
}

#### for split votes calculate the MQ mean of all judges that voted

# create indicator columns for chief justices, e.g. column Rehnquist -> 1 if Rehnquist was chief justice for a certain case, 0 ow
scc1$Hughes<-0
scc1$Stone<-0
scc1$Vinson<- 0
scc1$Warren<-0
scc1$Burger<-0
scc1$Rehnquist<-0
scc1$Roberts<- 0

for(i in 1:dim(scc1)[1]){
  if(scc1[i,13]=="Hughes"){scc1[i,58]<-1}
  if(scc1[i,13]=="Stone"){scc1[i,59]<-1}
  if(scc1[i,13]=="Vinson"){scc1[i,60]<-1}
  if(scc1[i,13]=="Warren"){scc1[i,61]<-1}
  if(scc1[i,13]=="Burger"){scc1[i,62]<-1}
  if(scc1[i,13]=="Rehnquist"){scc1[i,63]<-1}
  if(scc1[i,13]=="Roberts"){scc1[i,64]<-1}
}

# rename id column to 1-2645
scc1 <- transform(scc1,id=as.numeric(factor(time_t)))
rownames(scc1)<- 1:dim(scc1)[1]
d<-dim(scc1)[1]


############################################################################################
# add a new row to scc1 for the absolute difference of the maximum and minimum MQ score judge that supported a case

scc<- scc1
scc$MQ.score.diff <- 0

set.pro.minus.split <- c(1,3,4,5)

for ( i in 1:d){ # row 17987 is when term 1937 begins
  if(i %% 1000 == 0) cat("Starting Iteration", i, "\n")
  # which columns relate to case i in the 
  case.rows<- which(scc[i,1]==SCDB_justice[,1])
  number.judges<- length(case.rows)
  mq.vector<- c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)
  # create empty vector to calculate the mean MS score of majority judges
  total.MQ.score<- 0
  number.counts <- 0 # for how many judges do we have the MQ score
  for(j in 1:number.judges){
    
    if(is.element(SCDB_justice[case.rows[j], 56], set.pro.minus.split)){
      # get id of the justice
      justice.id <- SCDB_justice[case.rows[j],54]
      #print(justice.id)
      # get year of term
      case.year <- scc1[i,11]
      #print(case.year)
      # get row from which to take MS.score based on year and ID
      row.with.MQ.score <- which(MQ.scores[,1]==case.year & MQ.scores[,2]==justice.id)
      #print(row.with.MQ.score)
      if(length(row.with.MQ.score)==1){
        mq.vector[j]<- MQ.scores[row.with.MQ.score,4]
      }
    }
    
  }
  # save mean MQ score in scc1
  if(is.infinite(min(mq.vector, na.rm=TRUE))==FALSE){
  scc$MQ.score.diff[i]<- abs(max(mq.vector, na.rm=TRUE)- min(mq.vector, na.rm=TRUE))
  }
}

scc1<- scc
rm(wash.law, wash.law1, wash.law2, a, case.rows, case.year, d, i,j,justice.id, l.cluster, mq.vector, number.counts, number.judges,
   row.with.MQ.score, total.MQ.score, uscite1, uscite2, w, first.2016, last.2016, last.1937)

##################### merge SCDB date (scc1) and courtlistener data

# new column: opinion_id
scc$opinion_id <- 0

for(i in 1:dim(cluster.mat)[1]){
  w<- which(cluster.mat[i,6]==scc[,1])
  scc$opinion_id[w]<- cluster.mat[i,7]
}

####
## create csv file with all cases that could not be matched

# which(scc$opinion_id==0)
# [1]   14  171  224  338  354  389  396  504  597  598  599  600  601  604  605  606  646  730  731  752  761  764  811
# [24]  812  935  971  978  983  990 1004 1005 1032 1055 1096 1118 1167 1210 1330 1342 1343 1424 1505 1650 1687 1703 1723
# [47] 1759 1762 1880 1913 1920 1934 1985 2016 2048 2055 2078 2090 2118 2119 2120 2154 2207 2234 2255 2256 2263 2268 2269
# [70] 2280 2281 2282 2294 2329 2345 2357 2358 2359 2360 2361 2365 2369 2370 2376 2387 2388 2393 2394 2395 2426 2427 2428
# [93] 2429 2433 2441 2453 2454 2479 2482 2500 2533 2555 2577 2579 2582 2583 2584 2597 2598 3035 3278 3424 3425 3431 3448
# [116] 3449 3849 4010 4260 4275 4491 4578 7717 8280 8320 8478 8572 8599 8704 8708 8729 8731 8780 8790 8791 8792 8793 8794
# [139] 8796 8892 9394 9479 9520 9617 9778

no.op.id.mat <- scc

for ( i in dim(scc)[1]:1){
  if(scc$opinion_id[i]!=0){
    no.op.id.mat<- no.op.id.mat[-i,]
  }
}

write.csv(no.op.id.mat, file="cases_wo_opinion_id.csv")

# scc1 -> delete cases without an opinion id, scc -> cases with opinion ids that are 0
scc1<- scc
d<- dim(scc1)[1]
# delete cases without an opinion id
k<-0
for(i in d:1){
  if(scc1$opinion_id[i]==0){
    scc1<- scc1[-i,]
    k<- k+1
    print(k)
    }
}

rm(k,d,i,w)
rm(filenames_cluster, filenames_op, myJSON_cluster, myJSON_opinion)

############################################
############################################
### create adjacency matrix
############################################
############################################

library(combinat)
d<- dim(scc1)[1]
l<- dim(citations)[1]

# create empty adjacency matrix (full AM for all cases from the beginning)
memory.limit(30000)
adjacency.matrix<- matrix(0, d, d)
rownames(adjacency.matrix)<- 1:d
colnames(adjacency.matrix)<- 1:d

# fill adjacency matrix
for(i in 1:l){ # takes about 2 hrs
  #print(i)
  if(i %% 1000000 == 0) cat("Starting iteration", i, "\n")
  # which line can the information of case edgelist[i,1] be found in?
  l<- which(citations[i,1]==scc1[,66])
  # which line can the information of case edgelist[i,2] be found in? used to count how often a case is being cited
  q<- which(citations[i,2]==scc1[,66])
  # indicate tie in adjacency matrix
  adjacency.matrix[l,q]<- 1
}


rm(d,i,l,q)

#############################################################################
### read in overruled cases and delete overruled citations from adjacency network
#############################################################################

AM<- adjacency.matrix
overruled_cases_supremecourt <- read_csv("overruled_cases_supremecourt.csv")
# View(overruled_cases_supremecourt)
overruled<- as.data.frame(overruled_cases_supremecourt)

for(i in 1: dim(overruled)[1]){
  sender<- which(overruled[i,1]==scc1[,7])
  receiver <-  which(overruled[i,2]==scc1[,7])
  if(sum(sender)>0 & sum(receiver)>0){
  AM[sender, receiver]<- 0
  }
}

# 121 edges were deleted

adjacency.matrix <- AM


#############################################################################
## matrix that indicates the difference in years
#############################################################################
d<- dim(scc1)[1]


year.diff.matrix<- matrix(0, d, d)

for(i in 1:d){ # calculation takes a while
  if(i %% 500 == 0) cat("Starting iteration", i, "\n")
  # year for sender
  year.sender <- scc1[i, 11] # term column
  for(j in i:d){
    # year for receiver
    year.receiver <- scc1[j, 11] # term column
    year.diff.matrix[i,j]<- abs(year.sender- year.receiver)
    year.diff.matrix[j,i]<- abs(year.sender- year.receiver)
  }
}


#############################################################################
## same issue area matrix, 1  same issue area, 0 not
#############################################################################


same.issue.area<- matrix(0, d, d)
for(i in 1:d){ # calculation takes a while, ca 15 mins
  if(i %% 500 == 0) cat("Starting iteration", i, "\n")
  # issue area for sender
  issue.area.sender <- scc1[i, 41]
  for(j in i:d){
    # issue area for receiver
    issue.area.receiver <- scc1[j, 41]
    # if sender and receiver issue area is the same and non is NA then enter 1 into matrix to indicate same issue area
    if(is.na(issue.area.receiver)==FALSE & is.na(issue.area.sender)==FALSE & issue.area.sender==issue.area.receiver){
      same.issue.area[i,j]<-1
      same.issue.area[j,i]<-1
    }
  }
}


#############################################################################
## same Majority opinion writer matrix, 1  same opinion writer, 0 not
#############################################################################


same.opinion.writer<- matrix(0, d, d)
for(i in 1:d){ # calculation takes a while, ca 15 mins
  if(i %% 500 == 0) cat("Starting iteration", i, "\n")
  # opinion writer for sender
  opinion.writer.sender <- scc1[i, 49]
  for(j in i:d){
    # opinion writer for receiver
    opinion.writer.receiver <- scc1[j, 49]
    # if sender and receiver opinion writer is the same and non is NA then enter 1 into matrix to indicate same issue area
    if(is.na(opinion.writer.receiver)==FALSE & is.na(opinion.writer.sender)==FALSE & opinion.writer.sender==opinion.writer.receiver){
      same.opinion.writer[i,j]<-1
      same.opinion.writer[j,i]<-1
    }
  }
}

##############################################################################
## Absolue Difference of Median MQ score of sender
##############################################################################

mq.matrix <- matrix(0, d, d)

for(i in 1:d){ # calculation takes a while
  if(i %% 500 == 0) cat("Starting iteration", i, "\n")
  # mq for sender
  mq.sender <- scc1[i, 57] # term column
  for(j in i:d){
    # mq for receiver
    mq.receiver <- scc1[j, 57] # term column
    mq.matrix[i,j]<- abs(mq.sender- mq.receiver)
    mq.matrix[j,i]<- abs(mq.sender- mq.receiver)
  }
}



##############################################################################
##### add a column that counts how often a case was overruled in the past
##############################################################################


scc1$overruled <- 0

for(i in 1: dim(overruled)[1]){
  receiver <-  which(overruled[i,2]==scc1[,7])
  if(sum(receiver)>0){
    scc1$overruled[receiver]<-scc1$overruled[receiver] + 1
  }
}


#############################################################################
### create a matrix for changing overruled covariate
#############################################################################

max.id <- max(scc1$id)
Overruled.matrix<- matrix(0,d,max.id)

for(i in 1:dim(overruled)[1]){
  receiver.id <- which(overruled[i,2]==scc1[,7]) 
  sender.id <- which(overruled[i,1]==scc1[,7]) 
  # time point receiver case was overruled
  time.overruled <- scc1[sender.id, 55] # 55 is id
  ### add overruled cases for right time points
  if(sum(sender.id)>0 & sum(receiver.id)>0){
  for(j in time.overruled:max.id){
    Overruled.matrix[receiver.id, j] <-  Overruled.matrix[receiver.id, j] +1
  }
}
}



# delete 
rm(breaks, burger, cols, counts, hughes, i, issue.area.receiver, issue.area.sender, j,k,max.case, min.case,number.cases,
   number.citations, receiver, rehnquist, roberts, sender, set.pro, set.pro.minus.split, stone, vals, vinson, warren,
   year.receiver, year.sender, max.id, receiver.id, sender.id, time.overruled)
rm(AM, citations, MQ.scores, no.op.id.mat, overruled, overruled_cases_supremecourt, scc.save, scc1.save)

rm(mq.receiver, MQ.score, mq.sender, opinion.writer.receiver, opinion.writer.sender)



##############################################################################
########## EDA
##############################################################################

# Number of new cases every year

counts<- table(scc1$year)
barplot(counts, main="Number of new Cases every Year", xlab="Year", ylab="Frequency")

################# outdegree distribution
library(ggplot2)
outdegree.plot<- qplot(rowSums(adjacency.matrix), geom="histogram", binwidth = 1,  
      main = "Outdegree Distribution", 
      xlab = "Outdegree", ylab="Frequency",  
      fill=I("lightblue"), 
      col=I("black"), ylim=c(0,1000),
      xlim=c(0,50)) + theme(axis.text=element_text(size=24),axis.title=element_text(size=26),
                            plot.title=element_text(size=28, face='bold', hjust=0.5))

max(rowSums(adjacency.matrix))

################# indegree distribution
indegree.plot<- qplot(colSums(adjacency.matrix), geom="histogram", binwidth = 1,  
      main = "Indegree Distribution", 
      xlab = "Indegree", ylab="Frequency",  
      fill=I("lightblue"), 
      col=I("black"), ylim=c(0,1000),
      xlim=c(0,50)) + theme(axis.text=element_text(size=24),axis.title=element_text(size=26),
                            plot.title=element_text(size=28, face='bold', hjust=0.5))

max(colSums(adjacency.matrix))

library(gridExtra)
grid.arrange(indegree.plot, outdegree.plot, ncol=1, nrow=2)


## number of ties
sum(adjacency.matrix) # 111986

# timepoints
max(scc1$id) # 2619

# number cases
dim(adjacency.matrix)[1] #9945

# number mutual ties
sum(adjacency.matrix*t(adjacency.matrix)) #56


### number triangles
library(statnet)
summary(adjacency.matrix ~ triangle) # 250717


########################
## cases in each era

### hughes
hughes<- sum(scc1$Hughes==1) # 628

# cases per year
hughes/5   # 125.6


### stone
stone<- sum(scc1$Stone==1) # 756

# cases per year
stone/5  #151.2

### vinson
vinson<- sum(scc1$Vinson==1) #789

# cases per year
vinson/8 #98.25

### warren
warren<- sum(scc1$Warren==1)  #2149

# cases per year
warren/17  # 126.41

### burger
burger<- sum(scc1$Burger==1) #2805

burger/18   # 155.06

### rehnquist
rehnquist<- sum(scc1$Rehnquist==1) # 2004

rehnquist/19 #105.47

### roberts
roberts<- sum(scc1$Roberts==1) # 789

roberts/10 #78.9


###############################
# cases each term

number.cases<- c()
for(i in 1937:2015){
  a<- length(which(scc1$term==i))
  number.cases[i-1936]<-a
}
par(mfrow=c(1,1))
names(number.cases)<- 1937:2015
vals <- 1937:2015
breaks <- c(-Inf, 1941, 1946, 1953, 1969, 1986,2006, Inf)
cols=c("blue", "grey", "red", "green", "orange", "black", "lightblue")[findInterval(vals, vec=breaks)]
barplot(number.cases, main="Number of Cases in Each Term", cex.lab=1.8, cex.main=3, cex.axis=1.5, xlab = "Year", ylab="Count", col=cols)

# number of citations for each term

number.citations<- c()
for(i in 1937:2015){
  min.case<- min(which(scc1$term==i))
  max.case<- max(which(scc1$term==i))
  number.citations[i-1936]<-sum(adjacency.matrix[min.case:max.case, ])
}
names(number.citations)<- 1937:2015
vals <- 1937:2015
breaks <- c(-Inf, 1941, 1946, 1953, 1969, 1986, 2006, Inf)
cols=c("blue", "grey", "red", "green", "orange", "black", "lightblue")[findInterval(vals, vec=breaks)]
barplot(number.citations, main="Number of Citations in Each Term", cex.lab=1.8, cex.main=3, cex.axis=1.5, xlab = "Year", ylab="Count", col=cols)


rm(a, breaks, burger, cols, counts, hughes, i, indegree.plot, max.case, min.case, number.cases, number.citations, outdegree.plot , 
   rehnquist, roberts, stone, vals, vinson, warren)
