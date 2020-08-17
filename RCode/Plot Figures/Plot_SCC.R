


############################################
#### Plot SCC Results
############################################

load(file="1950_san.RData")
load(file="1951_san.RData")
load(file="1952_san.RData")
load(file="1953_san.RData")
load(file="1954_san.RData")
load(file="1955_san.RData")
load(file="1956_san.RData")
load(file="1957_san.RData")
load(file="1958_san.RData")
load(file="1959_san.RData")
load(file="1960_san.RData")
load(file="1961_san.RData")
load(file="1962_san.RData")
load(file="1963_san.RData")
load(file="1964_san.RData")
load(file="1965_san.RData")
load(file="1966_san.RData")
load(file="1967_san.RData")
load(file="1968_san.RData")
load(file="1969_san.RData")
load(file="1970_san.RData")
load(file="1971_san.RData")
load(file="1972_san.RData")
load(file="1973_san.RData")
load(file="1974_san.RData")
load(file="1975_san.RData")
load(file="1976_san.RData")
load(file="1977_san.RData")
load(file="1978_san.RData")
load(file="1979_san.RData")
load(file="1980_san.RData")
load(file="1981_san.RData")
load(file="1982_san.RData")
load(file="1983_san.RData")
load(file="1984_san.RData")
load(file="1985_san.RData")
load(file="1986_san.RData")
load(file="1987_san.RData")
load(file="1988_san.RData")
load(file="1989_san.RData")
load(file="1990_san.RData")
load(file="1991_san.RData")
load(file="1992_san.RData")
load(file="1993_san.RData")
load(file="1994_san.RData")
load(file="1995_san.RData")
load(file="1996_san.RData")
load(file="1997_san.RData")
load(file="1998_san.RData")
load(file="1999_san.RData")
load(file="2000_san.RData")
load(file="2001_san.RData")
load(file="2002_san.RData")
load(file="2003_san.RData")
load(file="2004_san.RData")
load(file="2005_san.RData")
load(file="2006_san.RData")
load(file="2007_san.RData")
load(file="2008_san.RData")
load(file="2009_san.RData")
load(file="2010_san.RData")
load(file="2011_san.RData")
load(file="2012_san.RData")
load(file="2013_san.RData")
load(file="2014_san.RData")
load(file="2015_san.RData")

## loop to create a matrix with all coefficients, matrix of std.errors and matrix of p-values

AICfull <- c()
BICfull <- c()
AICind <- c()
BICind <- c()

for(i in 50:115)  {
  Object <- get(paste0("li", i))
  
  AICfull[i-49] <- Object$AICm
  BICfull[i-49] <- Object$BICm
  AICind[i-49] <- Object$AICi
  BICind[i-49] <- Object$BICi
}


library(ggplot2)
library(reshape)
library(gridExtra)

Year<- 1950:2015

#################
### AIC


AICdata <- cbind(Year, AICfull, AICind)
AICdata <- as.data.frame(AICdata)
colnames(AICdata) <- c("Year", "Full Model", "Independent Model")

AICmelt <- reshape2::melt(AICdata, id.var='Year')
colnames(AICmelt) <- c("Year", "variable", "AIC")


p1<- ggplot(data = AICmelt, aes(x = Year, y = AIC, col=variable)) +ggtitle('AIC')+
  geom_line(lwd=2, aes(linetype=variable))
p1<-p1+theme(legend.position='none', axis.text=element_text(size=17),axis.title=element_text(size=19),
             plot.title=element_text(size=21, face='bold'))
          
  #geom_point(aes(colour = factor(pi)), size=1.5) 
p1


#####################
##### BIC

BICdata <- cbind(Year, BICfull, BICind)
BICdata <- as.data.frame(BICdata)
colnames(BICdata) <- c("Year", "Full Model", "Independent Model")

BICmelt <- reshape2::melt(BICdata, id.var='Year')
colnames(BICmelt) <- c("Year", "variable", "BIC")


p2<- ggplot(data = BICmelt, aes(x = Year, y = BIC, col=variable)) +ggtitle('BIC')+
  geom_line(lwd=2, aes(linetype=variable))
p2<-p2+theme( axis.text=element_text(size=17),axis.title=element_text(size=19),
              plot.title=element_text(size=21, face='bold'), legend.title=element_blank(),
              legend.position = c(0.73, 0.05), legend.text=element_text(size=18))

#geom_point(aes(colour = factor(pi)), size=1.5) 
p2


#my_layout<- matrix(c(1,1,2,2,2),1,5)
grid.arrange(p1, p2, ncol=2, nrow=1)




##################################################
##################################################
## Plot Coefficients, Std.error and P-values
##################################################
##################################################
# Defining Colour
orng.c<-"#fcae91FF"
purp.c<-"#7828a0FF"
gren.c<-"#46aa96FF"
lgry.c<-"#dcdcdcFF"
mgry.c<-"#8c8c8cFF"
dgry.c<-"#333333FF"

mkrzywinski = c('#006E82FF', 
                '#8214A0FF',
                '#005AC8FF',
                '#00A0FAFF', 
                '#FA78FAFF',
                '#14D2DCFF',
                '#AA0A3CFF',
                '#FA7850FF',
                '#0AB45AFF',
                '#F0F032FF',
                '#A0FA82FF',
                '#FAE6BEFF')

mkrzywinski[c(1, 8, 7)] <- c('#0098b3', '#f85320', '#910833')

## loop to create a matrix with all coefficients, matrix of std.errors and matrix of p-values

Object <- get(paste0("li", 50))
coM <- Object[[1]]
pvM <- Object[[2]]
steM<- Object[[3]]


for(i in 51:115)  {
  Object <- get(paste0("li", i))
  
  co <- Object[[1]]
  coM <- cbind(coM, co)
  
  pv <- Object[[2]]
  pvM <- cbind(pvM, pv)
  
  ste<- Object[[3]]
  steM <- cbind(steM, ste)
}


# reciprocity NA
coM[2, c(3:6, 8:13, 15:17, 19, 20,22, 23, 25, 27, 28, 30:35, 37, 38, 40:54, 56, 58:66)]<- NA
coM[12,c(3,5,6,7)]<- NA



library(ggplot2)
library(reshape)
library(gridExtra)

Year<- 1950:2015


P <- pvM
P[P> 0.1] <- 22
P[P< 0.1 & P>0.05] <- 24
P[P< 0.05 ] <- 21
P[is.na(P)]<- 23

steM[is.na(steM)] <- 0

# Old plotting colors toggle
# mkrzywinski[c(1, 8, 7)] <- c('springgreen4', 'orange1', 'red')

data<- data.frame(Year, coM[1,], P[1,])
colnames(data)<- c('Year', 'Theta', 'pi')
eb <- aes(ymax = coM[1,]+1.96*steM[1,], ymin = coM[1,]-1.96*steM[1,])
p1<- ggplot(data = data, aes(x = Year, y = Theta,  group=1)) +ggtitle('Edges')+
  annotate("rect", fill = grey(0.5), alpha = 0.15, 
           xmin = c(1953, 1986) + 0.5, xmax = c(1969, 2005) + 0.5,
           ymin = -Inf, ymax = Inf)+
  geom_ribbon(eb, fill = grey(0.7))+
  geom_line() + 
  geom_hline(yintercept = 0, color = 'black', size = 1)+
  geom_point(aes(fill = factor(pi), color = factor(pi), shape=factor(pi), size = 3), size=2)+
  scale_shape_manual(values=c('21' = 21, '24' = 22, '22' = 24))+
  scale_fill_manual(values=c('21'=mkrzywinski[1], '24'=mkrzywinski[8], '22'=mkrzywinski[7], '23'='black'))+
  scale_color_manual(values=c('21'=mkrzywinski[1], '24'=mkrzywinski[8], '22'=mkrzywinski[7], '23'='black'))

p1<-p1+theme_minimal() + theme(legend.position='none', axis.text=element_text(size=10),axis.title=element_text(size=14),
                               plot.title=element_text(size=16, face='bold'), plot.margin = unit(c(0.1,0,0.1,0.1), "cm"))
p1


# remove NA years
coM2 <- coM[2,-c(3:6, 8:13, 15:17, 19, 20,22, 23, 25, 27, 28, 30:35, 37, 38, 40:54, 56, 58:66)]
Year <- Year[-c(3:6, 8:13, 15:17, 19, 20,22, 23, 25, 27, 28, 30:35, 37, 38, 40:54, 56, 58:66)]
P2 <- P[2,-c(3:6, 8:13, 15:17, 19, 20,22, 23, 25, 27, 28, 30:35, 37, 38, 40:54, 56, 58:66)]
steM2 <- steM[2,-c(3:6, 8:13, 15:17, 19, 20,22, 23, 25, 27, 28, 30:35, 37, 38, 40:54, 56, 58:66)]

data<- data.frame(Year, coM2, P2)
colnames(data)<- c('Year', 'Theta', 'pi')
eb <- aes(ymax = coM2+1.96*steM2, ymin = coM2-1.96*steM2)
p2<- ggplot(data = data, aes(x = Year, y = Theta,  group=1)) +ggtitle('Reciprocity')+
  annotate("rect", fill = grey(0.5), alpha = 0.15, 
           xmin = c(1953, 1986) + 0.5, xmax = c(1969, 2005) + 0.5,
           ymin = -Inf, ymax = Inf)+
  geom_ribbon(eb, fill = grey(0.7))+
  geom_line() + 
  geom_hline(yintercept = 0, color = 'black', size = 1)+
  geom_vline(xintercept = 2015, alpha = 0)+
  geom_point(aes(fill = factor(pi), color = factor(pi), shape=factor(pi), size = 3), size=2)+
  scale_shape_manual(values=c('21' = 21, '24' = 22, '22' = 24, '23'=1))+
  scale_fill_manual(values=c('21'=mkrzywinski[1], '24'=mkrzywinski[8], '22'=mkrzywinski[7], '23'='black'))+
  scale_color_manual(values=c('21'=mkrzywinski[1], '24'=mkrzywinski[8], '22'=mkrzywinski[7], '23'='black'))

p2<-p2+theme_minimal() + theme(legend.position='none', axis.text=element_text(size=10),axis.title=element_text(size=14),
                               plot.title=element_text(size=16, face='bold'), plot.margin = unit(c(0.1,0,0.1,0.1), "cm"))
p2


Year<- 1950:2015

data<- data.frame(Year, coM[3,], P[3,])
colnames(data)<- c('Year', 'Theta', 'pi')
eb <- aes(ymax = coM[3,]+1.96*steM[3,], ymin = coM[3,]-1.96*steM[3,])
p3<- ggplot(data = data, aes(x = Year, y = Theta,  group=1)) +ggtitle('Receiver Outdegree')+
  annotate("rect", fill = grey(0.5), alpha = 0.15, 
           xmin = c(1953, 1986) + 0.5, xmax = c(1969, 2005) + 0.5,
           ymin = -Inf, ymax = Inf)+
  geom_ribbon(eb, fill = grey(0.7))+
  geom_line() + 
  geom_hline(yintercept = 0, color = 'black', size = 1)+
  geom_point(aes(fill = factor(pi), color = factor(pi), shape=factor(pi), size = 3), size=2)+
  scale_shape_manual(values=c('21' = 21, '24' = 22, '22' = 24))+
  scale_fill_manual(values=c('21'=mkrzywinski[1], '24'=mkrzywinski[8], '22'=mkrzywinski[7], '23'='black'))+
  scale_color_manual(values=c('21'=mkrzywinski[1], '24'=mkrzywinski[8], '22'=mkrzywinski[7], '23'='black'))

p3<-p3+theme_minimal() + theme(legend.position='none', axis.text=element_text(size=10),axis.title=element_text(size=14),
                               plot.title=element_text(size=16, face='bold'), plot.margin = unit(c(0.1,0,0.1,0.1), "cm"))
p3

data<- data.frame(Year, coM[4,], P[4,])
colnames(data)<- c('Year', 'Theta', 'pi')
eb <- aes(ymax = coM[4,]+1.96*steM[4,], ymin = coM[4,]-1.96*steM[4,])
p4<- ggplot(data = data, aes(x = Year, y = Theta,  group=1)) +ggtitle('Different Term Transitivity')+
  annotate("rect", fill = grey(0.5), alpha = 0.15, 
           xmin = c(1953, 1986) + 0.5, xmax = c(1969, 2005) + 0.5,
           ymin = -Inf, ymax = Inf)+
  geom_ribbon(eb, fill = grey(0.7))+
  geom_line() + 
  geom_hline(yintercept = 0, color = 'black', size = 1)+
  geom_point(aes(fill = factor(pi), color = factor(pi), shape=factor(pi), size = 3), size=2)+
  scale_shape_manual(values=c('21' = 21, '24' = 22, '22' = 24))+
  scale_fill_manual(values=c('21'=mkrzywinski[1], '24'=mkrzywinski[8], '22'=mkrzywinski[7], '23'='black'))+
  scale_color_manual(values=c('21'=mkrzywinski[1], '24'=mkrzywinski[8], '22'=mkrzywinski[7], '23'='black'))

p4<-p4+theme_minimal() + theme(legend.position='none', axis.text=element_text(size=10),axis.title=element_text(size=14),
                               plot.title=element_text(size=16, face='bold'), plot.margin = unit(c(0.1,0,0.1,0.1), "cm"))
p4


data<- data.frame(Year, coM[5,], P[5,])
colnames(data)<- c('Year', 'Theta', 'pi')
eb <- aes(ymax = coM[5,]+1.96*steM[5,], ymin = coM[5,]-1.96*steM[5,])
p5<- ggplot(data = data, aes(x = Year, y = Theta,  group=1)) +ggtitle('GWIdegree')+
  annotate("rect", fill = grey(0.5), alpha = 0.15, 
           xmin = c(1953, 1986) + 0.5, xmax = c(1969, 2005) + 0.5,
           ymin = -Inf, ymax = Inf)+
  geom_ribbon(eb, fill = grey(0.7))+
  geom_line() + 
  geom_hline(yintercept = 0, color = 'black', size = 1)+
  geom_point(aes(fill = factor(pi), color = factor(pi), shape=factor(pi), size = 3), size=2)+
  scale_shape_manual(values=c('21' = 21, '24' = 22, '22' = 24))+
  scale_fill_manual(values=c('21'=mkrzywinski[1], '24'=mkrzywinski[8], '22'=mkrzywinski[7], '23'='black'))+
  scale_color_manual(values=c('21'=mkrzywinski[1], '24'=mkrzywinski[8], '22'=mkrzywinski[7], '23'='black'))

p5<-p5+theme_minimal() + theme(legend.position='none', axis.text=element_text(size=10),axis.title=element_text(size=14),
                               plot.title=element_text(size=16, face='bold'), plot.margin = unit(c(0.1,0,0.1,0.1), "cm"))
p5



data<- data.frame(Year, coM[6,], P[6,])
colnames(data)<- c('Year', 'Theta', 'pi')
eb <- aes(ymax = coM[6,]+1.96*steM[6,], ymin = coM[6,]-1.96*steM[6,])
p6<- ggplot(data = data, aes(x = Year, y = Theta,  group=1)) +ggtitle('GWESP')+
  annotate("rect", fill = grey(0.5), alpha = 0.15, 
           xmin = c(1953, 1986) + 0.5, xmax = c(1969, 2005) + 0.5,
           ymin = -Inf, ymax = Inf)+
  geom_ribbon(eb, fill = grey(0.7))+
  geom_line() + 
  geom_hline(yintercept = 0, color = 'black', size = 1)+
  geom_point(aes(fill = factor(pi), color = factor(pi), shape=factor(pi), size = 3), size=2)+
  scale_shape_manual(values=c('21' = 21, '24' = 22, '22' = 24))+
  scale_fill_manual(values=c('21'=mkrzywinski[1], '24'=mkrzywinski[8], '22'=mkrzywinski[7], '23'='black'))+
  scale_color_manual(values=c('21'=mkrzywinski[1], '24'=mkrzywinski[8], '22'=mkrzywinski[7], '23'='black'))

p6<-p6+theme_minimal() + theme(legend.position='none', axis.text=element_text(size=10),axis.title=element_text(size=14),
                               plot.title=element_text(size=16, face='bold'), plot.margin = unit(c(0.1,0,0.1,0.1), "cm"))
p6


data<- data.frame(Year, coM[7,], P[7,])
colnames(data)<- c('Year', 'Theta', 'pi')
eb <- aes(ymax = coM[7,]+1.96*steM[7,], ymin = coM[7,]-1.96*steM[7,])
p7<- ggplot(data = data, aes(x = Year, y = Theta,  group=1)) +ggtitle('Ideological Distance')+
  annotate("rect", fill = grey(0.5), alpha = 0.15, 
           xmin = c(1953, 1986) + 0.5, xmax = c(1969, 2005) + 0.5,
           ymin = -Inf, ymax = Inf)+
  geom_ribbon(eb, fill = grey(0.7))+
  geom_line() + 
  geom_hline(yintercept = 0, color = 'black', size = 1)+
  geom_point(aes(fill = factor(pi), color = factor(pi), shape=factor(pi), size = 3), size=2)+
  scale_shape_manual(values=c('21' = 21, '24' = 22, '22' = 24))+
  scale_fill_manual(values=c('21'=mkrzywinski[1], '24'=mkrzywinski[8], '22'=mkrzywinski[7], '23'='black'))+
  scale_color_manual(values=c('21'=mkrzywinski[1], '24'=mkrzywinski[8], '22'=mkrzywinski[7], '23'='black'))

p7<-p7+theme_minimal() + theme(legend.position='none', axis.text=element_text(size=10),axis.title=element_text(size=14),
                               plot.title=element_text(size=16, face='bold'), plot.margin = unit(c(0.1,0,0.1,0.1), "cm"))
p7


data<- data.frame(Year, coM[8,], P[8,])
colnames(data)<- c('Year', 'Theta', 'pi')
eb <- aes(ymax = coM[8,]+1.96*steM[8,], ymin = coM[8,]-1.96*steM[8,])
p8<- ggplot(data = data, aes(x = Year, y = Theta,  group=1)) +ggtitle('Year Difference')+
  annotate("rect", fill = grey(0.5), alpha = 0.15, 
           xmin = c(1953, 1986) + 0.5, xmax = c(1969, 2005) + 0.5,
           ymin = -Inf, ymax = Inf)+
  geom_ribbon(eb, fill = grey(0.7))+
  geom_line() + 
  geom_hline(yintercept = 0, color = 'black', size = 1)+
  geom_point(aes(fill = factor(pi), color = factor(pi), shape=factor(pi), size = 3), size=2)+
  scale_shape_manual(values=c('21' = 21, '24' = 22, '22' = 24))+
  scale_fill_manual(values=c('21'=mkrzywinski[1], '24'=mkrzywinski[8], '22'=mkrzywinski[7], '23'='black'))+
  scale_color_manual(values=c('21'=mkrzywinski[1], '24'=mkrzywinski[8], '22'=mkrzywinski[7], '23'='black'))

p8<-p8+theme_minimal() + theme(legend.position='none', axis.text=element_text(size=10),axis.title=element_text(size=14),
                               plot.title=element_text(size=16, face='bold'), plot.margin = unit(c(0.1,0,0.1,0.1), "cm"))
p8


data<- data.frame(Year, coM[9,], P[9,])
colnames(data)<- c('Year', 'Theta', 'pi')
eb <- aes(ymax = coM[9,]+1.96*steM[9,], ymin = coM[9,]-1.96*steM[9,])
p9<- ggplot(data = data, aes(x = Year, y = Theta,  group=1)) +ggtitle('Year Difference Squared')+
  annotate("rect", fill = grey(0.5), alpha = 0.15, 
           xmin = c(1953, 1986) + 0.5, xmax = c(1969, 2005) + 0.5,
           ymin = -Inf, ymax = Inf)+
  geom_ribbon(eb, fill = grey(0.7))+
  geom_line() + 
  geom_hline(yintercept = 0, color = 'black', size = 1)+
  geom_point(aes(fill = factor(pi), color = factor(pi), shape=factor(pi), size = 3), size=2)+
  scale_shape_manual(values=c('21' = 21, '24' = 22, '22' = 24))+
  scale_fill_manual(values=c('21'=mkrzywinski[1], '24'=mkrzywinski[8], '22'=mkrzywinski[7], '23'='black'))+
  scale_color_manual(values=c('21'=mkrzywinski[1], '24'=mkrzywinski[8], '22'=mkrzywinski[7], '23'='black'))

p9<-p9+theme_minimal() + theme(legend.position='none', axis.text=element_text(size=10),axis.title=element_text(size=14),
                               plot.title=element_text(size=16, face='bold'), plot.margin = unit(c(0.1,0,0.1,0.1), "cm"))
p9


data<- data.frame(Year, coM[10,], P[10,])
colnames(data)<- c('Year', 'Theta', 'pi')
eb <- aes(ymax = coM[10,]+1.96*steM[10,], ymin = coM[10,]-1.96*steM[10,])
p10<- ggplot(data = data, aes(x = Year, y = Theta,  group=1)) +ggtitle('Ideological Breadth')+
  annotate("rect", fill = grey(0.5), alpha = 0.15, 
           xmin = c(1953, 1986) + 0.5, xmax = c(1969, 2005) + 0.5,
           ymin = -Inf, ymax = Inf)+
  geom_ribbon(eb, fill = grey(0.7))+
  geom_line() + 
  geom_hline(yintercept = 0, color = 'black', size = 1)+
  geom_point(aes(fill = factor(pi), color = factor(pi), shape=factor(pi), size = 3), size=2)+
  scale_shape_manual(values=c('21' = 21, '24' = 22, '22' = 24))+
  scale_fill_manual(values=c('21'=mkrzywinski[1], '24'=mkrzywinski[8], '22'=mkrzywinski[7], '23'='black'))+
  scale_color_manual(values=c('21'=mkrzywinski[1], '24'=mkrzywinski[8], '22'=mkrzywinski[7], '23'='black'))

p10<-p10+theme_minimal() + theme(legend.position='none', axis.text=element_text(size=10),axis.title=element_text(size=14),
                               plot.title=element_text(size=16, face='bold'), plot.margin = unit(c(0.1,0,0.1,0.1), "cm"))
p10



data<- data.frame(Year, coM[11,], P[11,])
colnames(data)<- c('Year', 'Theta', 'pi')
eb <- aes(ymax = coM[11,]+1.96*steM[11,], ymin = coM[11,]-1.96*steM[11,])
p11<- ggplot(data = data, aes(x = Year, y = Theta,  group=1)) +ggtitle('Number of Justices in Majority')+
  annotate("rect", fill = grey(0.5), alpha = 0.15, 
           xmin = c(1953, 1986) + 0.5, xmax = c(1969, 2005) + 0.5,
           ymin = -Inf, ymax = Inf)+
  geom_ribbon(eb, fill = grey(0.7))+
  geom_line() + 
  geom_hline(yintercept = 0, color = 'black', size = 1)+
  geom_point(aes(fill = factor(pi), color = factor(pi), shape=factor(pi), size = 3), size=2)+
  scale_shape_manual(values=c('21' = 21, '24' = 22, '22' = 24))+
  scale_fill_manual(values=c('21'=mkrzywinski[1], '24'=mkrzywinski[8], '22'=mkrzywinski[7], '23'='black'))+
  scale_color_manual(values=c('21'=mkrzywinski[1], '24'=mkrzywinski[8], '22'=mkrzywinski[7], '23'='black'))

p11<-p11+theme_minimal() + theme(legend.position='none', axis.text=element_text(size=10),axis.title=element_text(size=14),
                                 plot.title=element_text(size=16, face='bold'), plot.margin = unit(c(0.1,0,0.1,0.1), "cm"))
p11

# remove NA years
coM3 <- coM[12,-c(3,5,6,7)]
Year <- Year[-c(3,5,6,7)]
P3 <- P[12,-c(3,5,6,7)]
steM3 <- steM[12,-c(3,5,6,7)]


data<- data.frame(Year, coM3, P3)
colnames(data)<- c('Year', 'Theta', 'pi')
eb <- aes(ymax = coM3+1.96*steM3, ymin = coM3-1.96*steM3)
p12<- ggplot(data = data, aes(x = Year, y = Theta,  group=1)) +ggtitle('Overruled Cases')+
  annotate("rect", fill = grey(0.5), alpha = 0.15, 
           xmin = c(1953, 1986) + 0.5, xmax = c(1969, 2005) + 0.5,
           ymin = -Inf, ymax = Inf)+
  geom_ribbon(eb, fill = grey(0.7))+
  geom_line() + 
  geom_hline(yintercept = 0, color = 'black', size = 1)+
  geom_point(aes(fill = factor(pi), color = factor(pi), shape=factor(pi), size = 3), size=2)+
  scale_shape_manual(values=c('21' = 21, '24' = 22, '22' = 24))+
  scale_fill_manual(values=c('21'=mkrzywinski[1], '24'=mkrzywinski[8], '22'=mkrzywinski[7], '23'='black'))+
  scale_color_manual(values=c('21'=mkrzywinski[1], '24'=mkrzywinski[8], '22'=mkrzywinski[7], '23'='black'))

p12<-p12+theme_minimal() + theme(legend.position='none', axis.text=element_text(size=10),axis.title=element_text(size=14),
                                 plot.title=element_text(size=16, face='bold'), plot.margin = unit(c(0.1,0,0.1,0.1), "cm"))
p12

Year<- 1950:2015


data<- data.frame(Year, coM[13,], P[13,])
colnames(data)<- c('Year', 'Theta', 'pi')
eb <- aes(ymax = coM[13,]+1.96*steM[13,], ymin = coM[13,]-1.96*steM[13,])
p13<- ggplot(data = data, aes(x = Year, y = Theta,  group=1)) +ggtitle('Same Opinion Writer')+
  annotate("rect", fill = grey(0.5), alpha = 0.15, 
           xmin = c(1953, 1986) + 0.5, xmax = c(1969, 2005) + 0.5,
           ymin = -Inf, ymax = Inf)+
  geom_ribbon(eb, fill = grey(0.7))+
  geom_line() + 
  geom_hline(yintercept = 0, color = 'black', size = 1)+
  geom_point(aes(fill = factor(pi), color = factor(pi), shape=factor(pi), size = 3), size=2)+
  scale_shape_manual(values=c('21' = 21, '24' = 22, '22' = 24))+
  scale_fill_manual(values=c('21'=mkrzywinski[1], '24'=mkrzywinski[8], '22'=mkrzywinski[7], '23'='black'))+
  scale_color_manual(values=c('21'=mkrzywinski[1], '24'=mkrzywinski[8], '22'=mkrzywinski[7], '23'='black'))

p13<-p13+theme_minimal() + theme(legend.position='none', axis.text=element_text(size=10),axis.title=element_text(size=14),
                                 plot.title=element_text(size=16, face='bold'), plot.margin = unit(c(0.1,0,0.1,0.1), "cm"))
p13

data<- data.frame(Year, coM[14,], P[14,])
colnames(data)<- c('Year', 'Theta', 'pi')
eb <- aes(ymax = coM[14,]+1.96*steM[14,], ymin = coM[14,]-1.96*steM[14,])
p14<- ggplot(data = data, aes(x = Year, y = Theta,  group=1)) +ggtitle('Same Issue Area')+
  annotate("rect", fill = grey(0.5), alpha = 0.15, 
           xmin = c(1953, 1986) + 0.5, xmax = c(1969, 2005) + 0.5,
           ymin = -Inf, ymax = Inf)+
  geom_ribbon(eb, fill = grey(0.7))+
  geom_line() + 
  geom_hline(yintercept = 0, color = 'black', size = 1)+
  geom_point(aes(fill = factor(pi), color = factor(pi), shape=factor(pi), size = 3), size=2)+
  scale_shape_manual(values=c('21' = 21, '24' = 22, '22' = 24))+
  scale_fill_manual(values=c('21'=mkrzywinski[1], '24'=mkrzywinski[8], '22'=mkrzywinski[7], '23'='black'))+
  scale_color_manual(values=c('21'=mkrzywinski[1], '24'=mkrzywinski[8], '22'=mkrzywinski[7], '23'='black'))

p14<-p14+theme_minimal() + theme(legend.position='none', axis.text=element_text(size=10),axis.title=element_text(size=14),
                                 plot.title=element_text(size=16, face='bold'), plot.margin = unit(c(0.1,0,0.1,0.1), "cm"))
p14


pdf('SCC_results_1.pdf', height = 11.69, width = 8.27)
cowplot::plot_grid(p1, p2, p3, p5, p4, p6, ncol=2, nrow=3, align = 'v')
dev.off()

pdf('SCC_results_2.pdf', height = 11.69, width = 8.27)
grid.arrange(p7, p10, p8, p9, p11, p12, p13, p14, ncol=2, nrow=4)
dev.off()
#

