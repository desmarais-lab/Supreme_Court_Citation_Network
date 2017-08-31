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
# number edges=209946


dim(scc)[1]
# number of nodes: 26803



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

