##############################################################################
########## EDA
##############################################################################


load("ERCM_MPLE_cl.RData")

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


###################################################################
### Table 1 in Paper


## number of ties
sum(adjacency.matrix) # 112939

# timepoints
max(scc1$id) # 2645

# number cases
dim(adjacency.matrix)[1] #10020

# number mutual ties
sum(adjacency.matrix*t(adjacency.matrix)) #56


### number triangles
library(statnet)
summary(adjacency.matrix ~ triangle) # 252544


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
rehnquist<- sum(scc1$Rehnquist==1) # 2022

rehnquist/19 #106.42

### roberts
roberts<- sum(scc1$Roberts==1) # 871

roberts/10 #87.1


# Some additional exploration
cid <- which(scc1$usCite == '542 U.S. 507') # Getting row/col for specific case (this one is Hamdi v Rumsfeld)
mqs <- mq.matrix[which(adjacency.matrix[cid,] == 1), cid] # Getting all the ideological distances to the cited opinions
range(mqs)

# Change first argument to subset data (currently rehnquist court); change function to get statistic of interest (currently range)
mqdist <- sapply(which(scc1$Rehnquist == 1), function(x){range(mq.matrix[which(adjacency.matrix[x,] == 1), x])})
plot(density(mqdist, na.rm = T)) # Plotting
abline(v = range(mqs))

