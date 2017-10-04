### Edgelist gives case ids of citing and cited cases
edgelist <- read.table("allcites.txt",header=F)
## Need to add characters to ids so that they are not used as integers by network functions
edgelist[,1] <- paste("c",edgelist[,1],sep="")
edgelist[,2] <- paste("c",edgelist[,2],sep="")

### Read in node attributes
node_attributes <- read.csv("judicial.csv",stringsAsFactors=F)
node_attributes[,1] <- paste("c",node_attributes$caseid,sep="")

wash.law1<- read.csv("SCDB1.csv", stringsAsFactors = F)
wash.law2<- read.csv("SCDB2.csv", stringsAsFactors = F)

# remove space and dots in "1 U.S. 239" to match it with citeid in node_attributes
uscite1<- gsub(" ", "", wash.law1[,7], fixed = TRUE)
uscite1<- gsub(".", "", uscite1, fixed=TRUE)

uscite2<- gsub(" ", "", wash.law2[,7], fixed = TRUE)
uscite2<- gsub(".", "", uscite2, fixed=TRUE)

wash.law1[,7]<- uscite1
wash.law2[,7]<- uscite2

# merge both data sets together

# create empty matrix, so we can merge data from all three sets together
scc<- cbind(node_attributes, matrix(0, 30288, 53))
colnames(scc)<- c(colnames(node_attributes), colnames(wash.law1))

# loop that merges data from wash.law1 into scc

# merge wash.law1
for(i in 1:19861){
  if(i %% 1000 == 0) cat("Starting iteration", i, "\n")
  scc.row<- which(wash.law1[i,7]==scc[,2])
  scc[scc.row,17:69] <- wash.law1[i,]
}

# loop that merges data from wash.law2 into scc

# merge wash.law2
for(i in 1:8809){
  if(i %% 1000 == 0) cat("Starting iteration", i, "\n")
  scc.row<- which(wash.law2[i,7]==scc[,2])
  scc[scc.row,17:69] <- wash.law2[i,]
}


# delete all cases where we don't have data
for(i in 30188:1){
  if(i %% 1000 == 0) cat("Less than", i, "\n")
  if(scc[i, 23]==0){
    scc<- scc[-i,]
  }
}

# delete edges that involve cases that can't be cited
for(i in 216738:1){
  if(i %% 1000 == 0) cat("Less than", i, "\n")
  if(sum(edgelist[i,1]==scc[,1])==0 | sum(edgelist[i,2]==scc[,1])==0 ){
   edgelist<- edgelist[-i,]
  }
}
dim(edgelist)[1]
# number edges=209946


dim(scc)[1]
# number of nodes: 26803


#######################################
### add  days and time.id to data grame such that we can indicate which cases enter the network on which day
library(lubridate)

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

scc<- scc1
rm(scc1, days, date, x)


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

# set NA==0 (doesn't do any harm)
SCDB_justice[is.na(SCDB_justice)]<-0

# read in Martin Quinn scores

MQ.scores <- as.data.frame(read_csv("justices.csv"))



######
# add a column with the mean MS score of the judges of the majority vote
#####

scc1<- scc
scc1$meanMSscores<- 0

for ( i in 17987:26803){
  if(i %% 1000 == 0) cat("Starting Iteration", i, "\n")
  # which columns relate to case i in the 
  case.rows<- which(scc1[i,2]==SCDB_justice[,7])
  number.judges<- length(case.rows)
  
  # create empty vector to calculate the mean MS score of majority judges
  total.MQ.score<- 0
  number.counts <- 0 # for how many judges do we have the MQ score
  for(j in 1:number.judges){
    
    if(SCDB_justice[case.rows[j], 56]==1){
      # get id of the justice
      justice.id <- SCDB_justice[case.rows[j],54]
      #print(justice.id)
      # get year of case
      case.year <- scc1[i,4]
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
  scc1[i,72]<- total.MQ.score/number.counts
  
}

scc<- scc1
rm(scc1)


##########################################################################
##### EDA
##########################################################################



# are cases getting cited several times a day?
edgelist$date<-"empty"
edgelist$times.cited<- "empty"
for(i in 1:209946){
  if(i %% 1000 == 0) cat("Starting iteration", i, "\n")
  # which line can the information of case edgelist[i,1] be found in?
  l<- which(edgelist[i,1]==scc[,1])
  # which line can the information of case edgelist[i,2] be found in? used to count how often a case is being cited
  q<- which(edgelist[i,2]==scc[,1])
  # issue date sender
  edgelist[i,3]<- scc[l, 21]
  # issue date receiver
  edgelist[i,4]<- scc[q, 21]
}

# paste case and date together
case.and.date<- paste(edgelist[,2], edgelist[,3], sep="")

hist(table(case.and.date), main="How often a day does a case gets cited?", xlim=c(0.5, 6.5), breaks=0.5:26.5)
hist(table(case.and.date), main="How often a day does a case gets cited?", xlim=c(0.5, 6.5), breaks=0.5:26.5)$counts

# how often is a case cited?
number.citations <- paste(edgelist[,2], edgelist[,4], sep="")

hist(table(number.citations), main="Number of Citations for a Case", xlim=c(0.5, 20.5), breaks=0.5:200.5)
hist(table(number.citations), main="Number of Citations for a Case", xlim=c(0.5, 20.5), breaks=0.5:200.5)$counts


# Number of new cases every year

counts<- table(scc$year)
barplot(counts, main="Number of new Cases every Year", xlab="Year", ylab="Frequency")

################# outdegree distribution
hist(table(edgelist[,1]),  xlim=c(0.5, 30.5), breaks=0.5:200.5, main="Outdegree Distribution", xlab="Outdegree")

#How many total cases? (to calculate the 0's)
total <-sum(hist(table(edgelist[,1]),  xlim=c(0.5, 30.5), breaks=0.5:500.5, main="Indegree Distribution", xlab="intdegree")$counts)
# 18028

dim(scc)[1]-total
# 8775 cases do not cite



################### indegree distribution
hist(table(edgelist[,2]),  xlim=c(0.5, 30.5), breaks=0.5:200.5, main="Indegree Distribution", xlab="Indegree")$counts

#How many total cases? (to calculate the 0's)
total <-sum(hist(table(edgelist[,2]),  xlim=c(0.5, 30.5), breaks=0.5:200.5, main="Indegree Distribution", xlab="intdegree")$counts)
# 22238

dim(scc)[1]-total
# 4565 cases have no citation

################## year difference between citations
difference <- c()
for(i in 1:209946){
  if(i %% 1000 == 0) cat("Starting iteration", i, "\n")
  s<- which(edgelist[i,1]==scc[,1])
  r<- which(edgelist[i,2]==scc[,1])
  year.s<- scc[s, 4]
  year.r <- scc[r, 4]
  difference[i]<- year.s-year.r 
}

hist(difference, main="Years Difference Between Sender and Receiver",  xlim=c(-0.5, 100.5), breaks=-0.5:200.5)

#### do cases that occur at the same time cite each other?
cases.same.time<-0

for(i in 1:209946){
  if(i %% 1000 == 0) cat("Starting iteration", i, "\n")
  s<- which(edgelist[i,1]==scc[,1])
  r<- which(edgelist[i,2]==scc[,1])
  s.time<- scc1[s,71]
  r.time<- scc1[r,71]
  if(s.time==r.time){
    cases.same.time<- cases.same.time+1
  }
  
}

# cases.same.time 1327 out of 209946

