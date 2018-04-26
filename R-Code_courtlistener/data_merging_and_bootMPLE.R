# merge together csv data that was obtained from the aci.ics cluster
 
# rm(list= ls()[!(ls() %in% c('scc1','adjacency.matrix', 'mq.matrix', 'same.issue.area', 'year.diff.matrix'))])
memory.limit(30000)

library(readr)

mpledata1 <- read_csv("mpledata1.csv")
mpledata2 <- read_csv("mpledata2.csv")
mpledata3 <- read_csv("mpledata3.csv")
mpledata4 <- read_csv("mpledata4.csv")
mpledata5 <- read_csv("mpledata5.csv")

mpledata1<- as.data.frame(mpledata1)
mpledata2<- as.data.frame(mpledata2)
mpledata3<- as.data.frame(mpledata3)
mpledata4<- as.data.frame(mpledata4)
mpledata5<- as.data.frame(mpledata5)

mpledata<- rbind(mpledata1, mpledata2, mpledata3, mpledata4, mpledata5)
mpledata<- mpledata[,-1]

rm(mpledata1, mpledata2, mpledata3, mpledata4, mpledata5)


# next, I create new columns to add interaction effects

# first degree
mpledata$instar2_sendertime<- mpledata[,3]*mpledata[,14]
mpledata$mutual_sendertime<- mpledata[,4]*mpledata[,14]
mpledata$outstar2_sendertime<- mpledata[,5]*mpledata[,14]
mpledata$triangle_sendertime<- mpledata[,6]*mpledata[,14]
mpledata$mq_sendertime<- mpledata[,7]*mpledata[,14]
mpledata$sameissuearea_sendertime<- mpledata[,8]*mpledata[,14]
mpledata$yeardiff_sendertime<- mpledata[,9]*mpledata[,14]
mpledata$yeardiffsquare_sendertime<- mpledata[,10]*mpledata[,14]
mpledata$AbsDiffMQscores_sendertime<- mpledata[,16]*mpledata[,14]
mpledata$NumberJusticesPro_sendertime<- mpledata[,17]*mpledata[,14]
mpledata$Overruled_sendertime<- mpledata[,44]*mpledata[,14]
mpledata$jusitcehomophily_sendertime<- mpledata[,45]*mpledata[,14] 

# 2nd degree
mpledata$instar2_sendertime2<- mpledata[,3]*mpledata[,14]*mpledata[,14]
mpledata$mutual_sendertime2<- mpledata[,4]*mpledata[,14]*mpledata[,14]
mpledata$outstar2_sendertime2<- mpledata[,5]*mpledata[,14]*mpledata[,14]
mpledata$triangle_sendertime2<- mpledata[,6]*mpledata[,14]*mpledata[,14]
mpledata$mq_sendertime2<- mpledata[,7]*mpledata[,14]*mpledata[,14]
mpledata$sameissuearea_sendertime2<- mpledata[,8]*mpledata[,14]*mpledata[,14]
mpledata$yeardiff_sendertime2<- mpledata[,9]*mpledata[,14]*mpledata[,14]
mpledata$yeardiffsquare_sendertime2<- mpledata[,10]*mpledata[,14]*mpledata[,14]
mpledata$AbsDiffMQscores_sendertime2<- mpledata[,16]*mpledata[,14]*mpledata[,14]
mpledata$NumberJusticesPro_sendertime2<- mpledata[,17]*mpledata[,14]*mpledata[,14]
mpledata$Overruled_sendertime2<- mpledata[,44]*mpledata[,14]*mpledata[,14]
mpledata$jusitcehomophily_sendertime2<- mpledata[,45]*mpledata[,14]*mpledata[,14] 

# 3rd degree, as.numeric() is used to prevent integer overflow
mpledata$instar2_sendertime3<- mpledata[,3]*mpledata[,14]*mpledata[,14]*mpledata[,14]
mpledata$mutual_sendertime3<- mpledata[,4]*mpledata[,14]*mpledata[,14]*mpledata[,14]
mpledata$outstar2_sendertime3<- mpledata[,5]*mpledata[,14]*mpledata[,14]*mpledata[,14]
mpledata$triangle_sendertime3<- mpledata[,6]*mpledata[,14]*mpledata[,14]*mpledata[,14]
mpledata$mq_sendertime3<- mpledata[,7]*mpledata[,14]*mpledata[,14]*mpledata[,14]
mpledata$sameissuearea_sendertime3<- mpledata[,8]*mpledata[,14]*mpledata[,14]*mpledata[,14]
mpledata$yeardiff_sendertime3<- mpledata[,9]*mpledata[,14]*mpledata[,14]*mpledata[,14]
mpledata$yeardiffsquare_sendertime3<- as.numeric(mpledata[,10])*as.numeric(mpledata[,14])*as.numeric(mpledata[,14])*as.numeric(mpledata[,14])
mpledata$AbsDiffMQscores_sendertime3<- mpledata[,16]*mpledata[,14]*mpledata[,14]*mpledata[,14]
mpledata$NumberJusticesPro_sendertime3<- mpledata[,17]*mpledata[,14]*mpledata[,14]*mpledata[,14]
mpledata$Overruled_sendertime3<- mpledata[,44]*mpledata[,14]*mpledata[,14]*mpledata[,14]
mpledata$jusitcehomophily_sendertime3<- mpledata[,45]*mpledata[,14]*mpledata[,14]*mpledata[,14] 

# 4th degree
mpledata$instar2_sendertime4<- as.numeric(mpledata[,3])*as.numeric(mpledata[,14])*as.numeric(mpledata[,14])*as.numeric(mpledata[,14])*as.numeric(mpledata[,14])
mpledata$mutual_sendertime4<- mpledata[,4]*mpledata[,14]*mpledata[,14]*mpledata[,14]*mpledata[,14]
mpledata$outstar2_sendertime4<- as.numeric(mpledata[,5])*as.numeric(mpledata[,14])*as.numeric(mpledata[,14])*as.numeric(mpledata[,14])*as.numeric(mpledata[,14])
mpledata$triangle_sendertime4<- mpledata[,6]*mpledata[,14]*mpledata[,14]*mpledata[,14]*mpledata[,14]
mpledata$mq_sendertime4<- mpledata[,7]*mpledata[,14]*mpledata[,14]*mpledata[,14]*mpledata[,14]
mpledata$sameissuearea_sendertime4<- mpledata[,8]*mpledata[,14]*mpledata[,14]*mpledata[,14]*mpledata[,14]
mpledata$yeardiff_sendertime4<- as.numeric(mpledata[,9])*as.numeric(mpledata[,14])*as.numeric(mpledata[,14])*as.numeric(mpledata[,14])*as.numeric(mpledata[,14])
mpledata$yeardiffsquare_sendertime4<- as.numeric(mpledata[,10])*as.numeric(mpledata[,14])*as.numeric(mpledata[,14])*as.numeric(mpledata[,14])*as.numeric(mpledata[,14])
mpledata$AbsDiffMQscores_sendertime4<- mpledata[,16]*mpledata[,14]*mpledata[,14]*mpledata[,14]*mpledata[,14]
mpledata$NumberJusticesPro_sendertime4<- mpledata[,17]*mpledata[,14]*mpledata[,14]*mpledata[,14]*mpledata[,14]
mpledata$Overruled_sendertime4<- mpledata[,44]*mpledata[,14]*mpledata[,14]*mpledata[,14]*mpledata[,14]
mpledata$jusitcehomophily_sendertime4<- mpledata[,45]*mpledata[,14]*mpledata[,14] *mpledata[,14] *mpledata[,14]  

# sender_time year
mpledata$edgecov.year.t2<- mpledata$edgecov.year.t^2
mpledata$edgecov.year.t3<- mpledata$edgecov.year.t^3
mpledata$edgecov.year.t4<- mpledata$edgecov.year.t^4

# sender_time day
mpledata$nodeocov.sender.time2<- mpledata$nodeocov.sender.time^2
mpledata$nodeocov.sender.time3<- mpledata$nodeocov.sender.time^3
mpledata$nodeocov.sender.time4<- mpledata$nodeocov.sender.time^4

# full
write.csv(mpledata, file = "mpledata_all.csv")



#mpledata$yeardiffsquare_sendertime3<- as.numeric(mpledata[,10])*as.numeric(mpledata[,14])*as.numeric(mpledata[,14])*as.numeric(mpledata[,14])




#### testing bigglm

library(biglm)

mple.small <- mpledata[1:100000,]


mple.est <- bigglm(edgeij ~ istar2+ostar2+mutual+triangle+ edgecov.mq.t+ edgecov.same.issue.area.t+ edgecov.year.diff.t +edgecov.year.diff.square.t +  
                     nodeicov.AbsDiffMQscores+ nodeicov.NumberJusticesPro+ edgecov.year.t + nodeifactor.SameIssueArea.2+ nodeifactor.SameIssueArea.3+ 
                     nodeifactor.SameIssueArea.4+nodeifactor.SameIssueArea.5+ nodeifactor.SameIssueArea.6+ nodeifactor.SameIssueArea.7+ 
                     nodeifactor.SameIssueArea.8+nodeifactor.SameIssueArea.9+  nodeifactor.SameIssueArea.10+
                     nodeifactor.SameIssueArea.11+ nodeifactor.SameIssueArea.12+ nodeifactor.SameIssueArea.13+nodeifactor.SameIssueArea.14+ 
                     nodeofactor.SameIssueArea.2+ nodeofactor.SameIssueArea.3+ nodeofactor.SameIssueArea.4+nodeofactor.SameIssueArea.5+ 
                     nodeofactor.SameIssueArea.6+nodeofactor.SameIssueArea.7+
                     nodeofactor.SameIssueArea.8+nodeofactor.SameIssueArea.9+nodeofactor.SameIssueArea.10+ nodeofactor.SameIssueArea.11+ 
                     nodeofactor.SameIssueArea.12+ nodeofactor.SameIssueArea.13+nodeofactor.SameIssueArea.14+ nodeicov.Overruled +
                     edgecov.same.opinion.writer.t+ edgecov.same.opinion.writer.t + nodeifactor.MajOpWriter.68 + nodeifactor.MajOpWriter.71+
                     nodeifactor.MajOpWriter.72+ nodeifactor.MajOpWriter.74+ nodeifactor.MajOpWriter.75+ nodeifactor.MajOpWriter.76+ nodeifactor.MajOpWriter.77 +nodeifactor.MajOpWriter.78+
                     nodeifactor.MajOpWriter.79+ nodeifactor.MajOpWriter.80+ nodeifactor.MajOpWriter.81+ nodeifactor.MajOpWriter.82+ nodeifactor.MajOpWriter.83+ nodeifactor.MajOpWriter.84+
                     nodeifactor.MajOpWriter.85+ nodeifactor.MajOpWriter.86+ nodeifactor.MajOpWriter.87+ nodeifactor.MajOpWriter.88+ nodeifactor.MajOpWriter.89+ nodeifactor.MajOpWriter.90+
                     nodeifactor.MajOpWriter.91+ nodeifactor.MajOpWriter.92+ nodeifactor.MajOpWriter.93+ nodeifactor.MajOpWriter.94+ nodeifactor.MajOpWriter.95+ nodeifactor.MajOpWriter.96+
                     nodeifactor.MajOpWriter.97+ nodeifactor.MajOpWriter.98+ nodeifactor.MajOpWriter.99+ nodeifactor.MajOpWriter.100+ nodeifactor.MajOpWriter.101+ nodeifactor.MajOpWriter.102+
                     nodeifactor.MajOpWriter.103+ nodeifactor.MajOpWriter.104+ nodeifactor.MajOpWriter.105+ nodeifactor.MajOpWriter.106+ nodeifactor.MajOpWriter.107+ nodeifactor.MajOpWriter.108+
                     nodeifactor.MajOpWriter.109+ nodeifactor.MajOpWriter.110+ nodeifactor.MajOpWriter.111+ nodeifactor.MajOpWriter.112+ nodeifactor.MajOpWriter.113+ nodeifactor.MajOpWriter.114+
                     nodeofactor.MajOpWriter.68+ nodeofactor.MajOpWriter.71+
                     nodeofactor.MajOpWriter.72+ nodeofactor.MajOpWriter.74+ nodeofactor.MajOpWriter.75+ nodeofactor.MajOpWriter.76+ nodeofactor.MajOpWriter.77+ nodeofactor.MajOpWriter.78+
                     nodeofactor.MajOpWriter.79+ nodeofactor.MajOpWriter.80+ nodeofactor.MajOpWriter.81+ nodeofactor.MajOpWriter.82+ nodeofactor.MajOpWriter.83+ nodeofactor.MajOpWriter.84+
                     nodeofactor.MajOpWriter.85+ nodeofactor.MajOpWriter.86+ nodeofactor.MajOpWriter.87+ nodeofactor.MajOpWriter.88+ nodeofactor.MajOpWriter.89+ nodeofactor.MajOpWriter.90+
                     nodeofactor.MajOpWriter.91+ nodeofactor.MajOpWriter.92+ nodeofactor.MajOpWriter.93+ nodeofactor.MajOpWriter.94+ nodeofactor.MajOpWriter.95+ nodeofactor.MajOpWriter.96+
                     nodeofactor.MajOpWriter.97+ nodeofactor.MajOpWriter.98+ nodeofactor.MajOpWriter.99+ nodeofactor.MajOpWriter.100+ nodeofactor.MajOpWriter.101+ nodeofactor.MajOpWriter.102+
                     nodeofactor.MajOpWriter.103+ nodeofactor.MajOpWriter.104+ nodeofactor.MajOpWriter.105+ nodeofactor.MajOpWriter.106+ nodeofactor.MajOpWriter.107+ nodeofactor.MajOpWriter.108+
                     nodeofactor.MajOpWriter.109+ nodeofactor.MajOpWriter.110+ nodeofactor.MajOpWriter.111+ nodeofactor.MajOpWriter.112+ nodeofactor.MajOpWriter.113+ nodeofactor.MajOpWriter.114+
                     mix.SameIssueArea.1.1+ mix.SameIssueArea.1.2+ mix.SameIssueArea.1.3+ mix.SameIssueArea.1.4+ mix.SameIssueArea.1.5+ mix.SameIssueArea.1.6+ mix.SameIssueArea.1.7+ mix.SameIssueArea.1.8+
                     mix.SameIssueArea.1.9+ mix.SameIssueArea.1.10+ mix.SameIssueArea.1.11+ mix.SameIssueArea.1.12+ mix.SameIssueArea.1.13+ mix.SameIssueArea.1.14+ mix.SameIssueArea.2.1+ mix.SameIssueArea.2.2+
                     mix.SameIssueArea.2.3+mix.SameIssueArea.2.4+mix.SameIssueArea.2.5+mix.SameIssueArea.2.6+mix.SameIssueArea.2.7+mix.SameIssueArea.2.8+mix.SameIssueArea.2.9+mix.SameIssueArea.2.10+
                     mix.SameIssueArea.2.11+mix.SameIssueArea.2.12+mix.SameIssueArea.2.13+mix.SameIssueArea.2.14+mix.SameIssueArea.3.1+mix.SameIssueArea.3.2+mix.SameIssueArea.3.3+mix.SameIssueArea.3.4+
                     mix.SameIssueArea.3.5+mix.SameIssueArea.3.6+mix.SameIssueArea.3.7+mix.SameIssueArea.3.8+mix.SameIssueArea.3.9+mix.SameIssueArea.3.10+mix.SameIssueArea.3.11+mix.SameIssueArea.3.12+
                     mix.SameIssueArea.3.13+mix.SameIssueArea.3.14+mix.SameIssueArea.4.1+mix.SameIssueArea.4.2+mix.SameIssueArea.4.3+mix.SameIssueArea.4.4+mix.SameIssueArea.4.5+mix.SameIssueArea.4.6+
                     mix.SameIssueArea.4.7+mix.SameIssueArea.4.8+mix.SameIssueArea.4.9+mix.SameIssueArea.4.10+mix.SameIssueArea.4.11+mix.SameIssueArea.4.12+mix.SameIssueArea.4.13+mix.SameIssueArea.4.14+
                     mix.SameIssueArea.5.1+mix.SameIssueArea.5.2+mix.SameIssueArea.5.3+mix.SameIssueArea.5.4+mix.SameIssueArea.5.5+mix.SameIssueArea.5.6+mix.SameIssueArea.5.7+mix.SameIssueArea.5.8+
                     mix.SameIssueArea.5.9+mix.SameIssueArea.5.10+mix.SameIssueArea.5.11+mix.SameIssueArea.5.12+mix.SameIssueArea.5.13+mix.SameIssueArea.5.14+mix.SameIssueArea.6.1+mix.SameIssueArea.6.2+
                     mix.SameIssueArea.6.3+mix.SameIssueArea.6.4+mix.SameIssueArea.6.5+mix.SameIssueArea.6.6+mix.SameIssueArea.6.7+mix.SameIssueArea.6.8+mix.SameIssueArea.6.9+mix.SameIssueArea.6.10+
                     mix.SameIssueArea.6.11+mix.SameIssueArea.6.12+mix.SameIssueArea.6.13+mix.SameIssueArea.6.14+mix.SameIssueArea.7.1+mix.SameIssueArea.7.2+mix.SameIssueArea.7.3+mix.SameIssueArea.7.4+
                     mix.SameIssueArea.7.5+mix.SameIssueArea.7.6+mix.SameIssueArea.7.7+mix.SameIssueArea.7.8+mix.SameIssueArea.7.9+mix.SameIssueArea.7.10+mix.SameIssueArea.7.11+mix.SameIssueArea.7.12+
                     mix.SameIssueArea.7.13+mix.SameIssueArea.7.14+mix.SameIssueArea.8.1+mix.SameIssueArea.8.2+mix.SameIssueArea.8.3+mix.SameIssueArea.8.4+mix.SameIssueArea.8.5+mix.SameIssueArea.8.6+
                     mix.SameIssueArea.8.7+mix.SameIssueArea.8.8+mix.SameIssueArea.8.9+mix.SameIssueArea.8.10+mix.SameIssueArea.8.11+mix.SameIssueArea.8.12+mix.SameIssueArea.8.13+mix.SameIssueArea.8.14+
                     mix.SameIssueArea.9.1+mix.SameIssueArea.9.2+mix.SameIssueArea.9.3+mix.SameIssueArea.9.4+mix.SameIssueArea.9.5+mix.SameIssueArea.9.6+mix.SameIssueArea.9.7+mix.SameIssueArea.9.8+
                     mix.SameIssueArea.9.9+mix.SameIssueArea.9.10+mix.SameIssueArea.9.11+mix.SameIssueArea.9.12+mix.SameIssueArea.9.13+mix.SameIssueArea.9.14+mix.SameIssueArea.10.1+mix.SameIssueArea.10.2+
                     mix.SameIssueArea.10.3+mix.SameIssueArea.10.4+mix.SameIssueArea.10.5+mix.SameIssueArea.10.6+mix.SameIssueArea.10.7+mix.SameIssueArea.10.8+mix.SameIssueArea.10.9+mix.SameIssueArea.10.10+                     
                     mix.SameIssueArea.10.11+mix.SameIssueArea.10.12+mix.SameIssueArea.10.13+mix.SameIssueArea.10.14+mix.SameIssueArea.11.1+mix.SameIssueArea.11.2+mix.SameIssueArea.11.3+mix.SameIssueArea.11.4+
                     mix.SameIssueArea.11.5+mix.SameIssueArea.11.6+mix.SameIssueArea.11.7+mix.SameIssueArea.11.8+mix.SameIssueArea.11.9+mix.SameIssueArea.11.10+mix.SameIssueArea.11.11+mix.SameIssueArea.11.12+
                     mix.SameIssueArea.11.13+mix.SameIssueArea.11.14+mix.SameIssueArea.12.1+mix.SameIssueArea.12.2+mix.SameIssueArea.12.3+mix.SameIssueArea.12.4+mix.SameIssueArea.12.5+mix.SameIssueArea.12.6+
                     mix.SameIssueArea.12.7+mix.SameIssueArea.12.8+mix.SameIssueArea.12.9+mix.SameIssueArea.12.10+mix.SameIssueArea.12.11+mix.SameIssueArea.12.12+mix.SameIssueArea.12.13+mix.SameIssueArea.12.14+
                     mix.SameIssueArea.13.1+mix.SameIssueArea.13.2+mix.SameIssueArea.13.3+mix.SameIssueArea.13.4+mix.SameIssueArea.13.5+mix.SameIssueArea.13.6+mix.SameIssueArea.13.7+mix.SameIssueArea.13.8+
                     mix.SameIssueArea.13.9+mix.SameIssueArea.13.10+mix.SameIssueArea.13.11+mix.SameIssueArea.13.12+mix.SameIssueArea.13.13+mix.SameIssueArea.13.14+mix.SameIssueArea.14.1+mix.SameIssueArea.14.2+
                     mix.SameIssueArea.14.3+mix.SameIssueArea.14.4+mix.SameIssueArea.14.5+mix.SameIssueArea.14.6+mix.SameIssueArea.14.7+mix.SameIssueArea.14.8+mix.SameIssueArea.14.9+mix.SameIssueArea.14.10+
                     mix.SameIssueArea.14.11+mix.SameIssueArea.14.12+#mix.SameIssueArea.14.13+mix.SameIssueArea.14.14+
                     instar2_sendertime+outstar2_sendertime+ mutual_sendertime+ triangle_sendertime+mq_sendertime +sameissuearea_sendertime+
                     yeardiff_sendertime + yeardiffsquare_sendertime+ AbsDiffMQscores_sendertime + NumberJusticesPro_sendertime+ Overruled_sendertime+
                     jusitcehomophily_sendertime ,family=binomial(logit),data=mple.small)




mple.est <- glm(edgeij ~ istar2+ostar2+mutual+triangle+ edgecov.mq.t+ edgecov.same.issue.area.t+ edgecov.year.diff.t +edgecov.year.diff.square.t +  
                     nodeicov.AbsDiffMQscores+ nodeicov.NumberJusticesPro+ edgecov.year.t + nodeifactor.SameIssueArea.2+ nodeifactor.SameIssueArea.3+ 
                     nodeifactor.SameIssueArea.4+nodeifactor.SameIssueArea.5+ nodeifactor.SameIssueArea.6+ nodeifactor.SameIssueArea.7+ 
                     nodeifactor.SameIssueArea.8+nodeifactor.SameIssueArea.9+  nodeifactor.SameIssueArea.10+
                     nodeifactor.SameIssueArea.11+ nodeifactor.SameIssueArea.12+ nodeifactor.SameIssueArea.13+nodeifactor.SameIssueArea.14+ 
                     nodeofactor.SameIssueArea.2+ nodeofactor.SameIssueArea.3+ nodeofactor.SameIssueArea.4+nodeofactor.SameIssueArea.5+ 
                     nodeofactor.SameIssueArea.6+nodeofactor.SameIssueArea.7+
                     nodeofactor.SameIssueArea.8+nodeofactor.SameIssueArea.9+nodeofactor.SameIssueArea.10+ nodeofactor.SameIssueArea.11+ 
                     nodeofactor.SameIssueArea.12+ nodeofactor.SameIssueArea.13+nodeofactor.SameIssueArea.14+ nodeicov.Overruled +
                     edgecov.same.opinion.writer.t+ edgecov.same.opinion.writer.t + nodeifactor.MajOpWriter.68 + nodeifactor.MajOpWriter.71+
                     nodeifactor.MajOpWriter.72+ nodeifactor.MajOpWriter.74+ nodeifactor.MajOpWriter.75+ nodeifactor.MajOpWriter.76+ nodeifactor.MajOpWriter.77 +nodeifactor.MajOpWriter.78+
                     nodeifactor.MajOpWriter.79+ nodeifactor.MajOpWriter.80+ nodeifactor.MajOpWriter.81+ nodeifactor.MajOpWriter.82+ nodeifactor.MajOpWriter.83+ nodeifactor.MajOpWriter.84+
                     nodeifactor.MajOpWriter.85+ nodeifactor.MajOpWriter.86+ nodeifactor.MajOpWriter.87+ nodeifactor.MajOpWriter.88+ nodeifactor.MajOpWriter.89+ nodeifactor.MajOpWriter.90+
                     nodeifactor.MajOpWriter.91+ nodeifactor.MajOpWriter.92+ nodeifactor.MajOpWriter.93+ nodeifactor.MajOpWriter.94+ nodeifactor.MajOpWriter.95+ nodeifactor.MajOpWriter.96+
                     nodeifactor.MajOpWriter.97+ nodeifactor.MajOpWriter.98+ nodeifactor.MajOpWriter.99+ nodeifactor.MajOpWriter.100+ nodeifactor.MajOpWriter.101+ nodeifactor.MajOpWriter.102+
                     nodeifactor.MajOpWriter.103+ nodeifactor.MajOpWriter.104+ nodeifactor.MajOpWriter.105+ nodeifactor.MajOpWriter.106+ nodeifactor.MajOpWriter.107+ nodeifactor.MajOpWriter.108+
                     nodeifactor.MajOpWriter.109+ nodeifactor.MajOpWriter.110+ nodeifactor.MajOpWriter.111+ nodeifactor.MajOpWriter.112+ nodeifactor.MajOpWriter.113+ nodeifactor.MajOpWriter.114+
                     nodeofactor.MajOpWriter.68+ nodeofactor.MajOpWriter.71+
                     nodeofactor.MajOpWriter.72+ nodeofactor.MajOpWriter.74+ nodeofactor.MajOpWriter.75+ nodeofactor.MajOpWriter.76+ nodeofactor.MajOpWriter.77+ nodeofactor.MajOpWriter.78+
                     nodeofactor.MajOpWriter.79+ nodeofactor.MajOpWriter.80+ nodeofactor.MajOpWriter.81+ nodeofactor.MajOpWriter.82+ nodeofactor.MajOpWriter.83+ nodeofactor.MajOpWriter.84+
                     nodeofactor.MajOpWriter.85+ nodeofactor.MajOpWriter.86+ nodeofactor.MajOpWriter.87+ nodeofactor.MajOpWriter.88+ nodeofactor.MajOpWriter.89+ nodeofactor.MajOpWriter.90+
                     nodeofactor.MajOpWriter.91+ nodeofactor.MajOpWriter.92+ nodeofactor.MajOpWriter.93+ nodeofactor.MajOpWriter.94+ nodeofactor.MajOpWriter.95+ nodeofactor.MajOpWriter.96+
                     nodeofactor.MajOpWriter.97+ nodeofactor.MajOpWriter.98+ nodeofactor.MajOpWriter.99+ nodeofactor.MajOpWriter.100+ nodeofactor.MajOpWriter.101+ nodeofactor.MajOpWriter.102+
                     nodeofactor.MajOpWriter.103+ nodeofactor.MajOpWriter.104+ nodeofactor.MajOpWriter.105+ nodeofactor.MajOpWriter.106+ nodeofactor.MajOpWriter.107+ nodeofactor.MajOpWriter.108+
                     nodeofactor.MajOpWriter.109+ nodeofactor.MajOpWriter.110+ nodeofactor.MajOpWriter.111+ nodeofactor.MajOpWriter.112+ nodeofactor.MajOpWriter.113+ nodeofactor.MajOpWriter.114+
                     mix.SameIssueArea.1.1+ mix.SameIssueArea.1.2+ mix.SameIssueArea.1.3+ mix.SameIssueArea.1.4+ mix.SameIssueArea.1.5+ mix.SameIssueArea.1.6+ mix.SameIssueArea.1.7+ mix.SameIssueArea.1.8+
                     mix.SameIssueArea.1.9+ mix.SameIssueArea.1.10+ mix.SameIssueArea.1.11+ mix.SameIssueArea.1.12+ mix.SameIssueArea.1.13+ mix.SameIssueArea.1.14+ mix.SameIssueArea.2.1+ mix.SameIssueArea.2.2+
                     mix.SameIssueArea.2.3+mix.SameIssueArea.2.4+mix.SameIssueArea.2.5+mix.SameIssueArea.2.6+mix.SameIssueArea.2.7+mix.SameIssueArea.2.8+mix.SameIssueArea.2.9+mix.SameIssueArea.2.10+
                     mix.SameIssueArea.2.11+mix.SameIssueArea.2.12+mix.SameIssueArea.2.13+mix.SameIssueArea.2.14+mix.SameIssueArea.3.1+mix.SameIssueArea.3.2+mix.SameIssueArea.3.3+mix.SameIssueArea.3.4+
                     mix.SameIssueArea.3.5+mix.SameIssueArea.3.6+mix.SameIssueArea.3.7+mix.SameIssueArea.3.8+mix.SameIssueArea.3.9+mix.SameIssueArea.3.10+mix.SameIssueArea.3.11+mix.SameIssueArea.3.12+
                     mix.SameIssueArea.3.13+mix.SameIssueArea.3.14+mix.SameIssueArea.4.1+mix.SameIssueArea.4.2+mix.SameIssueArea.4.3+mix.SameIssueArea.4.4+mix.SameIssueArea.4.5+mix.SameIssueArea.4.6+
                     mix.SameIssueArea.4.7+mix.SameIssueArea.4.8+mix.SameIssueArea.4.9+mix.SameIssueArea.4.10+mix.SameIssueArea.4.11+mix.SameIssueArea.4.12+mix.SameIssueArea.4.13+mix.SameIssueArea.4.14+
                     mix.SameIssueArea.5.1+mix.SameIssueArea.5.2+mix.SameIssueArea.5.3+mix.SameIssueArea.5.4+mix.SameIssueArea.5.5+mix.SameIssueArea.5.6+mix.SameIssueArea.5.7+mix.SameIssueArea.5.8+
                     mix.SameIssueArea.5.9+mix.SameIssueArea.5.10+mix.SameIssueArea.5.11+mix.SameIssueArea.5.12+mix.SameIssueArea.5.13+mix.SameIssueArea.5.14+mix.SameIssueArea.6.1+mix.SameIssueArea.6.2+
                     mix.SameIssueArea.6.3+mix.SameIssueArea.6.4+mix.SameIssueArea.6.5+mix.SameIssueArea.6.6+mix.SameIssueArea.6.7+mix.SameIssueArea.6.8+mix.SameIssueArea.6.9+mix.SameIssueArea.6.10+
                     mix.SameIssueArea.6.11+mix.SameIssueArea.6.12+mix.SameIssueArea.6.13+mix.SameIssueArea.6.14+mix.SameIssueArea.7.1+mix.SameIssueArea.7.2+mix.SameIssueArea.7.3+mix.SameIssueArea.7.4+
                     mix.SameIssueArea.7.5+mix.SameIssueArea.7.6+mix.SameIssueArea.7.7+mix.SameIssueArea.7.8+mix.SameIssueArea.7.9+mix.SameIssueArea.7.10+mix.SameIssueArea.7.11+mix.SameIssueArea.7.12+
                     mix.SameIssueArea.7.13+mix.SameIssueArea.7.14+mix.SameIssueArea.8.1+mix.SameIssueArea.8.2+mix.SameIssueArea.8.3+mix.SameIssueArea.8.4+mix.SameIssueArea.8.5+mix.SameIssueArea.8.6+
                     mix.SameIssueArea.8.7+mix.SameIssueArea.8.8+mix.SameIssueArea.8.9+mix.SameIssueArea.8.10+mix.SameIssueArea.8.11+mix.SameIssueArea.8.12+mix.SameIssueArea.8.13+mix.SameIssueArea.8.14+
                     mix.SameIssueArea.9.1+mix.SameIssueArea.9.2+mix.SameIssueArea.9.3+mix.SameIssueArea.9.4+mix.SameIssueArea.9.5+mix.SameIssueArea.9.6+mix.SameIssueArea.9.7+mix.SameIssueArea.9.8+
                     mix.SameIssueArea.9.9+mix.SameIssueArea.9.10+mix.SameIssueArea.9.11+mix.SameIssueArea.9.12+mix.SameIssueArea.9.13+mix.SameIssueArea.9.14+mix.SameIssueArea.10.1+mix.SameIssueArea.10.2+
                     mix.SameIssueArea.10.3+mix.SameIssueArea.10.4+mix.SameIssueArea.10.5+mix.SameIssueArea.10.6+mix.SameIssueArea.10.7+mix.SameIssueArea.10.8+mix.SameIssueArea.10.9+mix.SameIssueArea.10.10+                     
                     mix.SameIssueArea.10.11+mix.SameIssueArea.10.12+mix.SameIssueArea.10.13+mix.SameIssueArea.10.14+mix.SameIssueArea.11.1+mix.SameIssueArea.11.2+mix.SameIssueArea.11.3+mix.SameIssueArea.11.4+
                     mix.SameIssueArea.11.5+mix.SameIssueArea.11.6+mix.SameIssueArea.11.7+mix.SameIssueArea.11.8+mix.SameIssueArea.11.9+mix.SameIssueArea.11.10+mix.SameIssueArea.11.11+mix.SameIssueArea.11.12+
                     mix.SameIssueArea.11.13+mix.SameIssueArea.11.14+mix.SameIssueArea.12.1+mix.SameIssueArea.12.2+mix.SameIssueArea.12.3+mix.SameIssueArea.12.4+mix.SameIssueArea.12.5+mix.SameIssueArea.12.6+
                     mix.SameIssueArea.12.7+mix.SameIssueArea.12.8+mix.SameIssueArea.12.9+mix.SameIssueArea.12.10+mix.SameIssueArea.12.11+mix.SameIssueArea.12.12+mix.SameIssueArea.12.13+mix.SameIssueArea.12.14+
                     mix.SameIssueArea.13.1+mix.SameIssueArea.13.2+mix.SameIssueArea.13.3+mix.SameIssueArea.13.4+mix.SameIssueArea.13.5+mix.SameIssueArea.13.6+mix.SameIssueArea.13.7+mix.SameIssueArea.13.8+
                     mix.SameIssueArea.13.9+mix.SameIssueArea.13.10+mix.SameIssueArea.13.11+mix.SameIssueArea.13.12+mix.SameIssueArea.13.13+mix.SameIssueArea.13.14+mix.SameIssueArea.14.1+mix.SameIssueArea.14.2+
                     mix.SameIssueArea.14.3+mix.SameIssueArea.14.4+mix.SameIssueArea.14.5+mix.SameIssueArea.14.6+mix.SameIssueArea.14.7+mix.SameIssueArea.14.8+mix.SameIssueArea.14.9+mix.SameIssueArea.14.10+
                     mix.SameIssueArea.14.11+mix.SameIssueArea.14.12+#mix.SameIssueArea.14.13+mix.SameIssueArea.14.14+
                     instar2_sendertime+outstar2_sendertime+ mutual_sendertime+ triangle_sendertime+mq_sendertime +sameissuearea_sendertime+
                     yeardiff_sendertime + yeardiffsquare_sendertime+ AbsDiffMQscores_sendertime + NumberJusticesPro_sendertime+ Overruled_sendertime+
                     jusitcehomophily_sendertime ,family=binomial,data=mple.small)
