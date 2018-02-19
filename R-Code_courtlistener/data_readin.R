memory.limit(30000)

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
filenames_op <- list.files("C:/Users/Geiler Typ/Desktop/PSU/Bruce Desmarais/Supreme Court Citation Project/scotus_opinion", pattern="*.json", full.names=TRUE) # this should give you a character vector, with each file name represented by an entry
myJSON_opinion <- lapply(filenames_op, function(x) fromJSON(file=x)) # a list in which each element is one of the original JSON files, takes a while ~ 30 min

######################################################
#### read in courtlistener scotus cluster-based data

filenames_cluster <- list.files("C:/Users/Geiler Typ/Desktop/PSU/Bruce Desmarais/Supreme Court Citation Project/R-Code_courtlistener_data/all_cluster/scotus", pattern="*.json", full.names=TRUE) # this should give you a character vector, with each file name represented by an entry
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

######################### delete
#check which column aligns with the opinion ids
w<- rep(NA, 100000)
for(i in 1:100000){
  if(i %% 10000 == 0) cat("Starting iteration", i, "\n")
  a<- which(cluster.mat[,7]==citations[i,2])
  if(sum(a)>0){
   w[i]<- a 
  }
}
# set everything greater 0 to 1 in order to count the number of citations that canbe matched
w[w>0] <-1
sum(w, na.rm=T) # 987 for first column, 167 for 2nd column, 1262 for 4th column, i in 1:10000
                # 10738 for 1st column, 1629 for 2nd columnm, 13420 for 4th column, i in 1:100000
                # column 1 and 7 seem to be the same
######################################


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
# add a column with the mean MS score of the judges of the majority vote
#####

scc1<- scc

# Let scc be the full data set and scc1 be the data from term 1937 on
# delete all entries prior 1937
scc1<- scc1[-(1:18436),]
# MQ scores end at 2015, delete cases after 2015 term
scc1<- scc1[-(10091:10234),]

scc1$meanMSscores<- 0
d<- dim(scc1)[1]
rownames(scc1)<- 1:d

for ( i in 1:d){ # row 18437 is when term 1937 begins (1937 is beginning of MQ score data)
  if(i %% 1000 == 0) cat("Starting Iteration", i, "\n")
  # which columns relate to case i in the 
  case.rows<- which(scc1[i,1]==SCDB_justice[,1])
  number.judges<- length(case.rows)
  
  # create empty vector to calculate the mean MS score of majority judges
  total.MQ.score<- 0
  number.counts <- 0 # for how many judges do we have the MQ score
  for(j in 1:number.judges){
    
    if(SCDB_justice[case.rows[j], 56]==1){ # 1 indicates voted pro
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
        total.MQ.score<- total.MQ.score + MQ.scores[row.with.MQ.score,4]
        number.counts<- number.counts +1
        #print(total.MQ.score)
      }
    }
    
  }
  # save mean MQ score in scc1
  scc1[i,56]<- total.MQ.score/number.counts
  
}

# get rid of split vote cases
for(i in d:1){
  if(is.nan(scc1[i,56])){
    scc1<- scc1[-i,]
  }
}

# create indicator columns for chief justices, e.g. column Rehnquist -> 1 if Rehnquist was chief justice for a certain case, 0 ow
scc1$Hughes<-0
scc1$Stone<-0
scc1$Vinson<- 0
scc1$Warren<-0
scc1$Burger<-0
scc1$Rehnquist<-0
scc1$Roberts<- 0

for(i in 1:dim(scc1)[1]){
  if(scc1[i,13]=="Hughes"){scc1[i,57]<-1}
  if(scc1[i,13]=="Stone"){scc1[i,58]<-1}
  if(scc1[i,13]=="Vinson"){scc1[i,59]<-1}
  if(scc1[i,13]=="Warren"){scc1[i,60]<-1}
  if(scc1[i,13]=="Burger"){scc1[i,61]<-1}
  if(scc1[i,13]=="Rehnquist"){scc1[i,62]<-1}
  if(scc1[i,13]=="Roberts"){scc1[i,63]<-1}
}

# rename id column to 1-2116
scc1 <- transform(scc1,id=as.numeric(factor(time_t)))
rownames(scc1)<- 1:dim(scc1)[1]
d<-dim(scc1)[1]


############################################################################################
# add a new row to scc1 for the absolute difference of the maximum and minimum MQ score judge that supported a case

scc<- scc1
scc$MQ.score.diff <- 0

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
    
    if(SCDB_justice[case.rows[j], 56]==1){
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
  scc[i,65]<- abs(max(mq.vector, na.rm=TRUE)- min(mq.vector, na.rm=TRUE))
  #if(scc[i,79]==Inf){print(mq.vector)
  # print(i)}
  
}

scc1<- scc
rm(wash.law, wash.law1, wash.law2, a, case.rows, case.year, d, i,j,justice.id, l.cluster, mq.vector, number.counts, number.judges,
   row.with.MQ.score, total.MQ.score, uscite1, uscite2, w)

##################### merge SCDB date (scc1) and courtlistener data

# new column: opinion_id
scc$opinion_id <- 0

for(i in 1:dim(cluster.mat)[1]){
  w<- which(cluster.mat[i,6]==scc[,1])
  scc$opinion_id[w]<- cluster.mat[i,7]
}

# scc1 -> delete cases without an opinion id, scc -> cases with opinion ids
scc1<- scc
d<- dim(scc1)[1]
# delete cases without an opinion id
for(i in d:1){
  if(scc1$opinion_id[i]==0){
    scc1<- scc1[-i,]
  }
}

rm(d,i,w)
rm(filenames_cluster, filenames_op, myJSON_cluster, myJSON_opinion)



########################################
### create adjacency matrix


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



##############################################################################
########## EDA
##############################################################################

# Number of new cases every year

counts<- table(scc$year)
barplot(counts, main="Number of new Cases every Year", xlab="Year", ylab="Frequency")

################# outdegree distribution
hist(rowSums(adjacency.matrix), breaks=-0.5:165.5, main="Outdegree Distribution", xlab="Outdegree")

################# indegree distribution
hist(colSums(adjacency.matrix), breaks=-0.5:228.5, main="Indegree Distribution", xlab="Indegree")

## number of ties
sum(adjacency.matrix) # 110742

# timepoints
max(scc1$id) # 2598

# number cases
dim(adjacency.matrix)[1] #9841

# number mutual ties
sum(adjacency.matrix*t(adjacency.matrix)) #54


### number triangles
library(statnet)
summary(adjacency.matrix ~ triangle) # 246134 


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
vinson<- sum(scc1$Vinson==1) #786

# cases per year
vinson/8 #98.25

### warren
warren<- sum(scc1$Warren==1)  #2123

# cases per year
warren/17  # 124.88

### burger
burger<- sum(scc1$Burger==1) #2755

burger/18   # 153.06

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
