### Edgelist gives case ids of citing and cited cases
edgelist <- read.table("allcites.txt",header=F)
## Need to add characters to ids so that they are not used as integers by network functions
edgelist[,1] <- paste("c",edgelist[,1],sep="")
edgelist[,2] <- paste("c",edgelist[,2],sep="")

### Read in node attributes
node_attributes <- read.csv("judicial.csv",stringsAsFactors=F)
node_attributes[,1] <- paste("c",node_attributes$caseid,sep="")

### Create a netowrk object
## The following sequence assures that edges are matched to the correct nodes
library(network)
## this is a directed acyclic graph, so it makes most senese to analyze it as undirected (i.e., since there can be no cycling)
scnetwork <- network.initialize(nrow(node_attributes),dir=F)
## set name to case ids, which will make it easy to add edges
network.vertex.names(scnetwork) <- node_attributes[,1]

# convert edgelist to a matrix
edgelist <- as.matrix(edgelist)

# match node indices to numeric place
edge_row <- match(edgelist[,1],network.vertex.names(scnetwork))
edge_col <- match(edgelist[,2],network.vertex.names(scnetwork))

# estimate timing
test_time <- system.time( for(i in 2:200){ scnetwork[cbind(edge_row[(i-1):i], edge_col[(i-1):i])] <- 1} )

# convert to hours
estimated_hours <- test_time/199*length(edge_row)/60/60

# view estimated timing
print(estimated_hours)

# run all rows to store edges
for(i in 98500:length(edge_row)){ #stopped at 98500
  if(i %% 250 == 0) cat("Starting iteration", i, "\n")
  scnetwork[cbind(edge_row[(i-1):i], edge_col[(i-1):i])] <- 1}

### Lets use the age of the case and the salience indicator
set.vertex.attribute(scnetwork,c("year","salience"),node_attributes[,c("year","oxford")])


# only use vertexes created after 1953
sc<- scnetwork

scnetwork54<- delete.vertices(sc, 1:21065)
rm(sc)


# fit mple
library(statnet)
mple <- try(ergm(scnetwork54~edges+ nodecov('salience')+ nodecov('year.center') + absdiff('year.center') + absdiff('year.center.square') + dsp(0) + esp(0), estimate = "MPLE" )) #
summary(mple)