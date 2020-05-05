


############################################
#### Plot SCC Results
############################################

load(file="1950_2.RData")
load(file="1951_2.RData")
load(file="1952_2.RData")
load(file="1953_2.RData")
load(file="1954_2.RData")
load(file="1955_2.RData")
load(file="1956_2.RData")
load(file="1957_2.RData")
load(file="1958_2.RData")
load(file="1959_2.RData")
load(file="1960_2.RData")
load(file="1961_2.RData")
load(file="1962_2.RData")
load(file="1963_2.RData")
load(file="1964_2.RData")
load(file="1965_2.RData")
load(file="1966_2.RData")
load(file="1967_2.RData")
load(file="1968_2.RData")
load(file="1969_2.RData")
load(file="1970_2.RData")
load(file="1971_2.RData")
load(file="1972_2.RData")
load(file="1973_2.RData")
load(file="1974_2.RData")
load(file="1975_2.RData")
load(file="1976_2.RData")
load(file="1977_2.RData")
load(file="1978_2.RData")
load(file="1979_2.RData")
load(file="1980_2.RData")
load(file="1981_2.RData")
load(file="1982_2.RData")
load(file="1983_2.RData")
load(file="1984_2.RData")
load(file="1985_2.RData")
load(file="1986_2.RData")
load(file="1987_2.RData")
load(file="1988_2.RData")
load(file="1989_2.RData")
load(file="1990_2.RData")
load(file="1991_2.RData")
load(file="1992_2.RData")
load(file="1993_2.RData")
load(file="1994_2.RData")
load(file="1995_2.RData")
load(file="1996_2.RData")
load(file="1997_2.RData")
load(file="1998_2.RData")
load(file="1999_2.RData")
load(file="2000_2.RData")
load(file="2001_san.RData")
load(file="2002_2.RData")
load(file="2003_2.RData")
load(file="2004_san.RData")
load(file="2005_2.RData")
load(file="2006_san.RData")
load(file="2007_2.RData")
load(file="2008_2.RData")
load(file="2009_2.RData")
load(file="2010_2.RData")
load(file="2011_san.RData")
load(file="2012_2.RData")
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


data<- data.frame(Year, coM[1,], P[1,])
colnames(data)<- c('Year', 'Theta', 'pi')
eb <- aes(ymax = coM[1,]+1.96*steM[1,], ymin = coM[1,]-1.96*steM[1,])
p1<- ggplot(data = data, aes(x = Year, y = Theta, shape=factor(pi), group=1)) +ggtitle('Edges')+
  geom_line() + 
  geom_ribbon(eb, alpha = 0.3)
p1<-p1+scale_colour_manual(values=c('21'='springgreen4', '24'='orange1', '22'='red', '23'='black'))
p1<-p1+theme(legend.position='none', axis.text=element_text(size=12),axis.title=element_text(size=14),
             plot.title=element_text(size=16, face='bold'))+ geom_point(aes(colour = factor(pi), size = 3), size=2.5) #+
#geom_point(aes(colour = factor(pi)), size=1.5) 
p1


# remove NA years
coM2 <- coM[2,-c(3:6, 8:13, 15:17, 19, 20,22, 23, 25, 27, 28, 30:35, 37, 38, 40:54, 56, 58:66)]
Y2 <- Year[-c(3:6, 8:13, 15:17, 19, 20,22, 23, 25, 27, 28, 30:35, 37, 38, 40:54, 56, 58:66)]
P2 <- P[2,-c(3:6, 8:13, 15:17, 19, 20,22, 23, 25, 27, 28, 30:35, 37, 38, 40:54, 56, 58:66)]
steM2 <- steM[2,-c(3:6, 8:13, 15:17, 19, 20,22, 23, 25, 27, 28, 30:35, 37, 38, 40:54, 56, 58:66)]

data<- data.frame(Y2, coM2, P2)
colnames(data)<- c('Year', 'Theta', 'pi')
eb <- aes(ymax = coM2+1.96*steM2, ymin = coM2-1.96*steM2)
p2<- ggplot(data = data, aes(x = Y2, y = Theta, shape=factor(pi), group=1)) +ggtitle('Reciprocity')+
  geom_line() + 
  geom_ribbon(eb, alpha = 0.3)
p2<-p2+scale_colour_manual(values=c('21'='springgreen4', '24'='orange1', '22'='red', '23'='black'))
p2<-p2+theme(legend.position='none', axis.text=element_text(size=12),axis.title=element_text(size=14),
             plot.title=element_text(size=16, face='bold'))+ geom_point(aes(colour = factor(pi), size = 3), size=2.5) #+
#geom_point(aes(colour = factor(pi)), size=1.5) 
p2



data<- data.frame(Year, coM[3,], P[3,])
colnames(data)<- c('Year', 'Theta', 'pi')
eb <- aes(ymax = coM[3,]+1.96*steM[3,], ymin = coM[3,]-1.96*steM[3,])
p3<- ggplot(data = data, aes(x = Year, y = Theta, shape=factor(pi), group=1)) +ggtitle('Receiver Outdegree')+
  geom_line() + 
  geom_ribbon(eb, alpha = 0.3)
p3<-p3+scale_colour_manual(values=c('21'='springgreen4', '24'='orange1', '22'='red', '23'='black'))
p3<-p3+theme(legend.position='none', axis.text=element_text(size=12),axis.title=element_text(size=14),
             plot.title=element_text(size=16, face='bold'))+ geom_point(aes(colour = factor(pi), size = 3), size=2.5) #+
#geom_point(aes(colour = factor(pi)), size=1.5) 
p3



data<- data.frame(Year, coM[4,], P[4,])
colnames(data)<- c('Year', 'Theta', 'pi')
eb <- aes(ymax = coM[4,]+1.96*steM[4,], ymin = coM[4,]-1.96*steM[4,])
p4<- ggplot(data = data, aes(x = Year, y = Theta, shape=factor(pi), group=1)) +ggtitle('Different Term Transitivity')+
  geom_line() + 
  geom_ribbon(eb, alpha = 0.3)
p4<-p4+scale_colour_manual(values=c('21'='springgreen4', '24'='orange1', '22'='red', '23'='black'))
p4<-p4+theme(legend.position='none', axis.text=element_text(size=12),axis.title=element_text(size=14),
             plot.title=element_text(size=16, face='bold'))+ geom_point(aes(colour = factor(pi), size = 3), size=2.5) #+
#geom_point(aes(colour = factor(pi)), size=1.5) 
p4


data<- data.frame(Year, coM[5,], P[5,])
colnames(data)<- c('Year', 'Theta', 'pi')
eb <- aes(ymax = coM[5,]+1.96*steM[5,], ymin = coM[5,]-1.96*steM[5,])
p5<- ggplot(data = data, aes(x = Year, y = Theta, shape=factor(pi), group=1)) +ggtitle('GWIdegree')+
  geom_line() + 
  geom_ribbon(eb, alpha = 0.3)
p5<-p5+scale_colour_manual(values=c('21'='springgreen4', '24'='orange1', '22'='red', '23'='black'))
p5<-p5+theme(legend.position='none', axis.text=element_text(size=12),axis.title=element_text(size=14),
             plot.title=element_text(size=16, face='bold'))+ geom_point(aes(colour = factor(pi), size = 3), size=2.5) #+
#geom_point(aes(colour = factor(pi)), size=1.5) 
p5



data<- data.frame(Year, coM[6,], P[6,])
colnames(data)<- c('Year', 'Theta', 'pi')
eb <- aes(ymax = coM[6,]+1.96*steM[6,], ymin = coM[6,]-1.96*steM[6,])
p6<- ggplot(data = data, aes(x = Year, y = Theta, shape=factor(pi), group=1)) +ggtitle('GWESP')+
  geom_line() + 
  geom_ribbon(eb, alpha = 0.3)
p6<-p6+scale_colour_manual(values=c('21'='springgreen4', '24'='orange1', '22'='red', '23'='black'))
p6<-p6+theme(legend.position='none', axis.text=element_text(size=12),axis.title=element_text(size=14),
             plot.title=element_text(size=16, face='bold'))+ geom_point(aes(colour = factor(pi), size = 3), size=2.5) #+
#geom_point(aes(colour = factor(pi)), size=1.5) 
p6



data<- data.frame(Year, coM[7,], P[7,])
colnames(data)<- c('Year', 'Theta', 'pi')
eb <- aes(ymax = coM[7,]+1.96*steM[7,], ymin = coM[7,]-1.96*steM[7,])
p7<- ggplot(data = data, aes(x = Year, y = Theta, shape=factor(pi), group=1)) +ggtitle('Ideological Distance')+
  geom_line() + 
  geom_ribbon(eb, alpha = 0.3)
p7<-p7+scale_colour_manual(values=c('21'='springgreen4', '24'='orange1', '22'='red', '23'='black'))
p7<-p7+theme(legend.position='none', axis.text=element_text(size=12),axis.title=element_text(size=14),
             plot.title=element_text(size=16, face='bold'))+ geom_point(aes(colour = factor(pi), size = 3), size=2.5) #+
#geom_point(aes(colour = factor(pi)), size=1.5) 
p7



data<- data.frame(Year, coM[8,], P[8,])
colnames(data)<- c('Year', 'Theta', 'pi')
eb <- aes(ymax = coM[8,]+1.96*steM[8,], ymin = coM[8,]-1.96*steM[8,])
p8<- ggplot(data = data, aes(x = Year, y = Theta, shape=factor(pi), group=1)) +ggtitle('Year Difference')+
  geom_line() + 
  geom_ribbon(eb, alpha = 0.3)
p8<-p8+scale_colour_manual(values=c('21'='springgreen4', '24'='orange1', '22'='red', '23'='black'))
p8<-p8+theme(legend.position='none', axis.text=element_text(size=12),axis.title=element_text(size=14),
             plot.title=element_text(size=16, face='bold'))+ geom_point(aes(colour = factor(pi), size = 3), size=2.5) #+
#geom_point(aes(colour = factor(pi)), size=1.5) 
p8


data<- data.frame(Year, coM[9,], P[9,])
colnames(data)<- c('Year', 'Theta', 'pi')
eb <- aes(ymax = coM[9,]+1.96*steM[9,], ymin = coM[9,]-1.96*steM[9,])
p9<- ggplot(data = data, aes(x = Year, y = Theta, shape=factor(pi), group=1)) +ggtitle('Year Difference Squared')+
  geom_line() + 
  geom_ribbon(eb, alpha = 0.3)
p9<-p9+scale_colour_manual(values=c('21'='springgreen4', '24'='orange1', '22'='red', '23'='black'))
p9<-p9+theme(legend.position='none', axis.text=element_text(size=12),axis.title=element_text(size=14),
             plot.title=element_text(size=16, face='bold'))+ geom_point(aes(colour = factor(pi), size = 3), size=2.5) #+
#geom_point(aes(colour = factor(pi)), size=1.5) 
p9



data<- data.frame(Year, coM[10,], P[10,])
colnames(data)<- c('Year', 'Theta', 'pi')
eb <- aes(ymax = coM[10,]+1.96*steM[10,], ymin = coM[10,]-1.96*steM[10,])
p10<- ggplot(data = data, aes(x = Year, y = Theta, shape=factor(pi), group=1)) +ggtitle('Ideological Breadth')+
  geom_line() + 
  geom_ribbon(eb, alpha = 0.3)
p10<-p10+scale_colour_manual(values=c('21'='springgreen4', '24'='orange1', '22'='red', '23'='black'))
p10<-p10+theme(legend.position='none', axis.text=element_text(size=12),axis.title=element_text(size=14),
               plot.title=element_text(size=16, face='bold'))+ geom_point(aes(colour = factor(pi), size = 3), size=2.5) #+
#geom_point(aes(colour = factor(pi)), size=1.5) 
p10



data<- data.frame(Year, coM[11,], P[11,])
colnames(data)<- c('Year', 'Theta', 'pi')
eb <- aes(ymax = coM[11,]+1.96*steM[11,], ymin = coM[11,]-1.96*steM[11,])
p11<- ggplot(data = data, aes(x = Year, y = Theta, shape=factor(pi), group=1)) +ggtitle('Number of Justices in Majority')+
  geom_line() + 
  geom_ribbon(eb, alpha = 0.3)
p11<-p11+scale_colour_manual(values=c('21'='springgreen4', '24'='orange1', '22'='red', '23'='black'))
p11<-p11+theme(legend.position='none', axis.text=element_text(size=12),axis.title=element_text(size=14),
               plot.title=element_text(size=16, face='bold'))+ geom_point(aes(colour = factor(pi), size = 3), size=2.5) #+
#geom_point(aes(colour = factor(pi)), size=1.5) 
p11


# remove NA years
coM3 <- coM[12,-c(3,5,6,7)]
Y3 <- Year[-c(3,5,6,7)]
P3 <- P[12,-c(3,5,6,7)]
steM3 <- steM[12,-c(3,5,6,7)]


data<- data.frame(Y3, coM3, P3)
colnames(data)<- c('Year', 'Theta', 'pi')
eb <- aes(ymax = coM3+1.96*steM3, ymin = coM3-1.96*steM3)
p12<- ggplot(data = data, aes(x = Y3, y = Theta, shape=factor(pi), group=1)) +ggtitle('Overruled Cases')+
  geom_line() + 
  geom_ribbon(eb, alpha = 0.3)
p12<-p12+scale_colour_manual(values=c('21'='springgreen4', '24'='orange1', '22'='red', '23'='black'))
p12<-p12+theme(legend.position='none', axis.text=element_text(size=12),axis.title=element_text(size=14),
             plot.title=element_text(size=16, face='bold'))+ geom_point(aes(colour = factor(pi), size = 3), size=2.5) #+
#geom_point(aes(colour = factor(pi)), size=1.5) 
p12


data<- data.frame(Year, coM[13,], P[13,])
colnames(data)<- c('Year', 'Theta', 'pi')
eb <- aes(ymax = coM[13,]+1.96*steM[13,], ymin = coM[13,]-1.96*steM[13,])
p13<- ggplot(data = data, aes(x = Year, y = Theta, shape=factor(pi), group=1)) +ggtitle('Same Opinion Writer')+
  geom_line() + 
  geom_ribbon(eb, alpha = 0.3)
p13<-p13+scale_colour_manual(values=c('21'='springgreen4', '24'='orange1', '22'='red', '23'='black'))
p13<-p13+theme(legend.position='none', axis.text=element_text(size=12),axis.title=element_text(size=14),
               plot.title=element_text(size=16, face='bold'))+ geom_point(aes(colour = factor(pi), size = 3), size=2.5) #+
#geom_point(aes(colour = factor(pi)), size=1.5) 
p13




data<- data.frame(Year, coM[14,], P[14,])
colnames(data)<- c('Year', 'Theta', 'pi')
eb <- aes(ymax = coM[14,]+1.96*steM[14,], ymin = coM[14,]-1.96*steM[14,])
p14<- ggplot(data = data, aes(x = Year, y = Theta, shape=factor(pi), group=1)) +ggtitle('Same Issue Area')+
  geom_line() + 
  geom_ribbon(eb, alpha = 0.3)
p14<-p14+scale_colour_manual(values=c('21'='springgreen4', '24'='orange1', '22'='red', '23'='black'))
p14<-p14+theme(legend.position='none', axis.text=element_text(size=12),axis.title=element_text(size=14),
               plot.title=element_text(size=16, face='bold'))+ geom_point(aes(colour = factor(pi), size = 3), size=2.5) #+
#geom_point(aes(colour = factor(pi)), size=1.5) 
p14


grid.arrange(p1, p2, p3, p5, p4, p6, ncol=2, nrow=3)
grid.arrange(p7, p8, p9, p10, p11, p12, p13, p14, ncol=2, nrow=4)

