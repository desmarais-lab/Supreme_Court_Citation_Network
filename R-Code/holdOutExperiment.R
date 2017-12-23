scdata <- read.csv("mpledata_all.csv",stringsAsFactors=F)

set.seed(123)

niter = 10
predict.results <- matrix(0,niter,6)

for(i in 1:niter){

train.obs <- sample(1:nrow(scdata),round(0.8*nrow(scdata)))

train.data <- scdata[train.obs,]

library(biglm)

# MPLE
mple.est <- bigglm(edgeij ~ istar2+ostar2+mutual+triangle+ edgecov.mq.t+ edgecov.same.issue.area.t+ edgecov.year.diff.t +edgecov.year.diff.square.t +  
                nodeicov.AbsDiffMQscores+ nodeicov.NumberJusticesPro+ nodeocov.sender.time + nodeifactor.SameIssueArea.2+ nodeifactor.SameIssueArea.3+ 
                nodeifactor.SameIssueArea.4+nodeifactor.SameIssueArea.5+ nodeifactor.SameIssueArea.6+ nodeifactor.SameIssueArea.7+ 
                nodeifactor.SameIssueArea.8+nodeifactor.SameIssueArea.9+  nodeifactor.SameIssueArea.10+
                nodeifactor.SameIssueArea.11+ nodeifactor.SameIssueArea.12+ nodeifactor.SameIssueArea.13+nodeifactor.SameIssueArea.14+ 
                nodeofactor.SameIssueArea.2+ nodeofactor.SameIssueArea.3+ nodeofactor.SameIssueArea.4+nodeofactor.SameIssueArea.5+ 
                nodeofactor.SameIssueArea.6+nodeofactor.SameIssueArea.7+
                nodeofactor.SameIssueArea.8+nodeofactor.SameIssueArea.9+nodeofactor.SameIssueArea.10+ nodeofactor.SameIssueArea.11+ 
                nodeofactor.SameIssueArea.12+ nodeofactor.SameIssueArea.13+nodeofactor.SameIssueArea.14+
                instar2_sendertime+outstar2_sendertime+ mutual_sendertime+ triangle_sendertime+mq_sendertime +sameissuearea_sendertime+
                yeardiff_sendertime + yeardiffsquare_sendertime+ AbsDiffMQscores_sendertime + NumberJusticesPro_sendertime,family=binomial(logit),data=train.data)
summary(mple.est)

save(list="mple.est",file="training_full.RData")


# MPLE
mple.est.independent <- bigglm(edgeij ~ edgecov.mq.t+ edgecov.same.issue.area.t+ edgecov.year.diff.t +edgecov.year.diff.square.t +  
                nodeicov.AbsDiffMQscores+ nodeicov.NumberJusticesPro+ nodeocov.sender.time + nodeifactor.SameIssueArea.2+ nodeifactor.SameIssueArea.3+ 
                nodeifactor.SameIssueArea.4+nodeifactor.SameIssueArea.5+ nodeifactor.SameIssueArea.6+ nodeifactor.SameIssueArea.7+ 
                nodeifactor.SameIssueArea.8+nodeifactor.SameIssueArea.9+  nodeifactor.SameIssueArea.10+
                nodeifactor.SameIssueArea.11+ nodeifactor.SameIssueArea.12+ nodeifactor.SameIssueArea.13+nodeifactor.SameIssueArea.14+ 
                nodeofactor.SameIssueArea.2+ nodeofactor.SameIssueArea.3+ nodeofactor.SameIssueArea.4+nodeofactor.SameIssueArea.5+ 
                nodeofactor.SameIssueArea.6+nodeofactor.SameIssueArea.7+
                nodeofactor.SameIssueArea.8+nodeofactor.SameIssueArea.9+nodeofactor.SameIssueArea.10+ nodeofactor.SameIssueArea.11+ 
                nodeofactor.SameIssueArea.12+ nodeofactor.SameIssueArea.13+nodeofactor.SameIssueArea.14+
                mq_sendertime +sameissuearea_sendertime+
                yeardiff_sendertime + yeardiffsquare_sendertime+ AbsDiffMQscores_sendertime + NumberJusticesPro_sendertime,family=binomial(logit),train.data)
summary(mple.est.independent)

save(list="mple.est.independent",file="training_independent.RData")

test.data <- scdata[-train.obs,]

predict.full <- c(predict(mple.est,newdata=test.data,type="response"))

predict.independent <- c(predict(mple.est.independent,newdata=test.data,type="response"))

library(MLmetrics)

true.y <- test.data$edgeij
pred.full <- as.numeric(predict.full>0.5)
pred.ind <- as.numeric(predict.independent>0.5)

precision <- function(true.y,pred.y){
	mean(true.y[which(pred.y==1)])
}

recall <- function(true.y,pred.y){
	mean(pred.y[which(true.y==1)])
}

prec.full <- precision(true.y,pred.full)
prec.ind <- precision(true.y,pred.ind)

rec.full <- recall(true.y,pred.full)
rec.ind <- recall(true.y,pred.ind)

f1.full <- F1_Score(true.y,pred.full,pos=1)
f1.ind <- F1_Score(true.y,pred.ind,pos=1)

predict.results[i,] <- c(prec.full,prec.ind,rec.full,rec.ind,f1.full,f1.ind)

print(i)

}

save(list="predict.results",file="prediction.performance.RData")

mean.range <- function(x){
  c(round(mean(x),dig=4),paste("(",round(min(x),dig=4),", ",round(max(x),dig=4),")",sep=""))
}

results.full <- t(apply(predict.results[,c(1,3,5)],2,mean.range))
results.ind <- t(apply(predict.results[,c(2,4,6)],2,mean.range))

library(xtable)

results.table <- cbind(results.ind,results.full)

rownames(results.table) <- c("precision","recall","F1 score")

colnames(results.table) <- c("mean","range","mean","range")

tex.table <- xtable(results.table)

print(tex.table,file="prediction_table.tex")



