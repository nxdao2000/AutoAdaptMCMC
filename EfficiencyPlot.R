# Load plyr so we can use ddply() to create the example data set
library(plyr)
library(ggplot2)
library(reshape2)
load("litter3e+05Auto.rda")
m1=m1[which(m1$mcmc!='crossLevel'),]
m1=m1[which(m1$mcmc!='blockAB'),]
m1$mcmc=strtrim(m1$mcmc,8)
m2=m1[which(m1$mcmc=='Auto-Blo'),]
print(mean(m2$Efficiency))
print(mean(m2$C)*32/6)

mylitters<-m1[,c(1,3:5)]
colnames(mylitters)<-c("methods","ESS", "Time", "Efficiency")
ddply(mylitters, .(methods), summarize,  ESS=mean(ESS), Efficiency=mean(Efficiency), Time=mean(Time))

pdf('litterESS.pdf')
dfmelt<-melt(mylitters, measure.vars = 2) 
ggplot(mylitters, aes(factor(methods), ESS))+
  stat_boxplot(geom ='errorbar') +geom_boxplot(aes(fill = factor(methods)))+scale_x_discrete(name ="Methods")+scale_fill_discrete(name = "methods")  
dev.off()

pdf('litterTime.pdf')
dfmelt<-melt(mylitters, measure.vars = 3) 
ggplot(mylitters, aes(factor(methods), Time))+
  stat_boxplot(geom ='errorbar') +geom_boxplot(aes(fill = factor(methods)))+scale_x_discrete(name ="Methods")+scale_fill_discrete(name = "methods")   
dev.off()

pdf('litterAutoBlockEfficiency.pdf')
dfmelt<-melt(mylitters, measure.vars = 4) 
ggplot(mylitters, aes(factor(methods), Efficiency))+
  stat_boxplot(geom ='errorbar') +geom_boxplot(aes(fill = factor(methods)))+scale_x_discrete(name ="Methods")+scale_fill_discrete(name = "methods") + ylim(0, 50) 
dev.off()



###########################################################


# Load plyr so we can use ddply() to create the example data set
library(plyr)
library(ggplot2)
library(reshape2)
load("GLMM50000Auto.rda")
m1=m1[which(m1$mcmc!='crossLevel'),]
m1$mcmc=strtrim(m1$mcmc,8)
m2=m1[which(m1$mcmc=='Auto-Blo'),]
print(mean(m2$Efficiency))
print(mean(m2$C)*32)

myGLMMs<-m1[,c(1,3:5)]
colnames(myGLMMs)<-c("methods","ESS", "Time", "Efficiency")
ddply(myGLMMs, .(methods), summarize,  ESS=mean(ESS), Efficiency=mean(Efficiency), Time=mean(Time))

pdf('GLMMESS.pdf')
dfmelt<-melt(myGLMMs, measure.vars = 2) 
ggplot(myGLMMs, aes(factor(methods), ESS))+
    stat_boxplot(geom ='errorbar') +geom_boxplot(aes(fill = factor(methods)))+scale_x_discrete(name ="Methods")+scale_fill_discrete(name = "methods")  
dev.off()

pdf('GLMMTime.pdf')
dfmelt<-melt(myGLMMs, measure.vars = 3) 
ggplot(myGLMMs, aes(factor(methods), Time))+
    stat_boxplot(geom ='errorbar') +geom_boxplot(aes(fill = factor(methods)))+scale_x_discrete(name ="Methods")+scale_fill_discrete(name = "methods")   
dev.off()

pdf('GLMMAutoBlockEfficiency.pdf')
dfmelt<-melt(myGLMMs, measure.vars = 4) 
ggplot(myGLMMs, aes(factor(methods), Efficiency))+
    stat_boxplot(geom ='errorbar') +geom_boxplot(aes(fill = factor(methods)))+scale_x_discrete(name ="Methods")+scale_fill_discrete(name = "methods")+theme(axis.text.x=element_text(angle = -45, hjust = 0)) + ylim(0, 1.6)  
dev.off()



###########################################################

# Load plyr so we can use ddply() to create the example data set
library(plyr)
library(ggplot2)
library(reshape2)
load("spatial50000Auto.rda")
m1$mcmc=strtrim(m1$mcmc,8)
m2=m1[which(m1$mcmc=='Auto-Blo'),]
print(mean(m2$Efficiency))
print(mean(m2$C)*32)

myspatials<-m1[,c(1,3:5)]
colnames(myspatials)<-c("methods","ESS", "Time", "Efficiency")
ddply(myspatials, .(methods), summarize,  ESS=mean(ESS), Efficiency=mean(Efficiency), Time=mean(Time))

pdf('spatialESS.pdf')
dfmelt<-melt(myspatials, measure.vars = 2) 
ggplot(myspatials, aes(factor(methods), ESS))+
    stat_boxplot(geom ='errorbar') +geom_boxplot(aes(fill = factor(methods)))+scale_x_discrete(name ="Methods")+scale_fill_discrete(name = "methods")  
dev.off()

pdf('spatialTime.pdf')
dfmelt<-melt(myspatials, measure.vars = 3) 
ggplot(myspatials, aes(factor(methods), Time))+
    stat_boxplot(geom ='errorbar') +geom_boxplot(aes(fill = factor(methods)))+scale_x_discrete(name ="Methods")+scale_fill_discrete(name = "methods")   
dev.off()

pdf('spatialAutoBlockEfficiency.pdf')
dfmelt<-melt(myspatials, measure.vars = 4) 
ggplot(myspatials, aes(factor(methods), Efficiency))+
    stat_boxplot(geom ='errorbar') +geom_boxplot(aes(fill = factor(methods)))+scale_x_discrete(name ="Methods")+scale_fill_discrete(name = "methods") + theme(axis.text.x=element_text(angle = -45, hjust = 0)) +ylim(0, 1.0)  
dev.off()