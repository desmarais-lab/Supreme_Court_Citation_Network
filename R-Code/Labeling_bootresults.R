
library(readr)

mpledata <- read_csv("bootresults.csv")
mpledata<- mpledata[,-1]
d<- c("edges","istar2","ostar2","mutual","triangle", "edgecov.mq.t", "edgecov.same.issue.area.t", "edgecov.year.diff.t" ,"edgecov.year.diff.square.t" ,  
                   "nodeicov.AbsDiffMQscores", "nodeicov.NumberJusticesPro", "nodeocov.sender.time" , "nodeifactor.SameIssueArea.2", "nodeifactor.SameIssueArea.3",
                      "nodeifactor.SameIssueArea.4","nodeifactor.SameIssueArea.5", "nodeifactor.SameIssueArea.6", "nodeifactor.SameIssueArea.7", 
                      "nodeifactor.SameIssueArea.8","nodeifactor.SameIssueArea.9",  "nodeifactor.SameIssueArea.10",
                      "nodeifactor.SameIssueArea.11", "nodeifactor.SameIssueArea.12", "nodeifactor.SameIssueArea.13","nodeifactor.SameIssueArea.14", 
                      "nodeofactor.SameIssueArea.2", "nodeofactor.SameIssueArea.3", "nodeofactor.SameIssueArea.4","nodeofactor.SameIssueArea.5", 
                      "nodeofactor.SameIssueArea.6","nodeofactor.SameIssueArea.7",
                      "nodeofactor.SameIssueArea.8","nodeofactor.SameIssueArea.9","nodeofactor.SameIssueArea.10", "nodeofactor.SameIssueArea.11", 
                      "nodeofactor.SameIssueArea.12", "nodeofactor.SameIssueArea.13","nodeofactor.SameIssueArea.14",
                      "instar2_sendertime","outstar2_sendertime", "mutual_sendertime", "triangle_sendertime","mq_sendertime" ,"sameissuearea_sendertime",
                      "yeardiff_sendertime" , "yeardiffsquare_sendertime", "AbsDiffMQscores_sendertime" , "NumberJusticesPro_sendertime")
colnames(mpledata)<- d


write.csv(mpledata, file = "bootresults.csv")

########################################
##### results for model without endogenous statistics

mpledata <- read_csv("bootresults_noEndo.csv")
mpledata<- mpledata[,-1]
d<- c("edges", "edgecov.mq.t", "edgecov.same.issue.area.t", "edgecov.year.diff.t" ,"edgecov.year.diff.square.t" ,  
      "nodeicov.AbsDiffMQscores", "nodeicov.NumberJusticesPro", "nodeocov.sender.time" , "nodeifactor.SameIssueArea.2", "nodeifactor.SameIssueArea.3",
      "nodeifactor.SameIssueArea.4","nodeifactor.SameIssueArea.5", "nodeifactor.SameIssueArea.6", "nodeifactor.SameIssueArea.7", 
      "nodeifactor.SameIssueArea.8","nodeifactor.SameIssueArea.9",  "nodeifactor.SameIssueArea.10",
      "nodeifactor.SameIssueArea.11", "nodeifactor.SameIssueArea.12", "nodeifactor.SameIssueArea.13","nodeifactor.SameIssueArea.14", 
      "nodeofactor.SameIssueArea.2", "nodeofactor.SameIssueArea.3", "nodeofactor.SameIssueArea.4","nodeofactor.SameIssueArea.5", 
      "nodeofactor.SameIssueArea.6","nodeofactor.SameIssueArea.7",
      "nodeofactor.SameIssueArea.8","nodeofactor.SameIssueArea.9","nodeofactor.SameIssueArea.10", "nodeofactor.SameIssueArea.11", 
      "nodeofactor.SameIssueArea.12", "nodeofactor.SameIssueArea.13","nodeofactor.SameIssueArea.14",
      "mq_sendertime" ,"sameissuearea_sendertime",
      "yeardiff_sendertime" , "yeardiffsquare_sendertime", "AbsDiffMQscores_sendertime" , "NumberJusticesPro_sendertime")
colnames(mpledata)<- d


write.csv(mpledata, file = "bootresults_noEndo.csv")
