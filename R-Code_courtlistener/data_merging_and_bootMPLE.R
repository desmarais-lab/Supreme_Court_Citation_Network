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



mpledata$yeardiffsquare_sendertime3<- as.numeric(mpledata[,10])*as.numeric(mpledata[,14])*as.numeric(mpledata[,14])*as.numeric(mpledata[,14])
