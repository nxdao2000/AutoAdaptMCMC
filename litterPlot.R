# Load plyr so we can use ddply() to create the example data set
library(plyr)
library(ggplot2)
library(reshape2)

model='litter'
niter=5000

load(paste(model,niter,'.rda', sep=''))
time<-m1[,31:60]
efficiencies<-m1[,1:30]
Time=rep(1:20,30)

tg <- data.frame(Time,as.vector(time),as.vector(efficiencies))
colnames(tg)<-c('chain', 'time','MCMC_Efficiency')

mean(tg$time)
mean(tg$MCMC_Efficiency)


pdf(paste(model,niter,'.pdf', sep=''))
ggplot(data=tg, aes(x=time, y=MCMC_Efficiency, group=as.factor(chain),colour = as.factor(chain))) + geom_line()+ geom_point( size=1, shape=21, fill="white")+ labs(colour = "chain")+ xlim(0, 40)+ ylim(0, 120)+theme(axis.text.x=element_text(angle = 0, hjust = 0)) 
dev.off()

model='litter'
niter=10000

load(paste(model,niter,'.rda', sep=''))
time<-m1[,31:60]
efficiencies<-m1[,1:30]
Time=rep(1:20,30)

tg <- data.frame(Time,as.vector(time),as.vector(efficiencies))
colnames(tg)<-c('chain', 'time','MCMC_Efficiency')
mean(tg$time)
mean(tg$MCMC_Efficiency)

pdf(paste(model,niter,'.pdf', sep=''))
ggplot(data=tg, aes(x=time, y=MCMC_Efficiency, group=as.factor(chain),colour = as.factor(chain))) + geom_line()+ geom_point( size=1, shape=21, fill="white")+ labs(colour = "chain")+ xlim(0, 110)+ ylim(0, 100) +theme(axis.text.x=element_text(angle = 0, hjust = 0))
dev.off()

model='litter'
niter=20000

load(paste(model,niter,'.rda', sep=''))
time<-m1[,31:60]
efficiencies<-m1[,1:30]
Time=rep(1:20,30)

tg <- data.frame(Time,as.vector(time),as.vector(efficiencies))
colnames(tg)<-c('chain', 'time','MCMC_Efficiency')
mean(tg$time)
mean(tg$MCMC_Efficiency)

pdf(paste(model,niter,'.pdf', sep=''))
ggplot(data=tg, aes(x=time, y=MCMC_Efficiency, group=as.factor(chain),colour = as.factor(chain))) + geom_line()+ geom_point( size=1, shape=21, fill="white")+ labs(colour = "chain")+ xlim(0, 110)+ ylim(0, 100) +theme(axis.text.x=element_text(angle = 0, hjust = 0))
dev.off()

library(plyr)
library(ggplot2)
library(reshape2)

model='litter'
niter=10000
load(paste(model,niter,'.rda', sep=''))
time<-m1[,31:60]
Time<-apply(time, 2, mean)
efficiencies<-m1[,1:30]
tg <- data.frame(round(Time,0),t(efficiencies))
colnames(tg)[1]='Time'
melted = melt(tg, id.vars="Time")
print(Time)
apply(efficiencies,2,mean)

colnames(melted)<-c('Time', 'chain','MCMC_Efficiency')
pdf(paste(model,niter,'BoxplotEfficiency.pdf', sep=''))

ggplot(melted, aes(factor(Time), MCMC_Efficiency ))+
  stat_boxplot(geom ='errorbar') +geom_boxplot(aes(fill = (Time)))+scale_x_discrete(name ="Time(s)")+theme(axis.text.x=element_text(angle = 0, hjust = 0)) +ylim(0, 120) 
dev.off()

model='litter'
niter=20000
load(paste(model,niter,'.rda', sep=''))
time<-m1[,31:60]
Time<-apply(time, 2, mean)
efficiencies<-m1[,1:30]
tg <- data.frame(round(Time,0),t(efficiencies))
colnames(tg)[1]='Time'
melted = melt(tg, id.vars="Time")
print(Time)
apply(efficiencies,2,mean)

colnames(melted)<-c('Time', 'chain','MCMC_Efficiency')
pdf(paste(model,niter,'BoxplotEfficiency.pdf', sep=''))

ggplot(melted, aes(factor(Time), MCMC_Efficiency ))+
  stat_boxplot(geom ='errorbar') +geom_boxplot(aes(fill = (Time)))+scale_x_discrete(name ="Time(s)")+theme(axis.text.x=element_text(angle = 0, hjust = 0)) +ylim(0, 120) 
dev.off()


model='litter'
niter=50000
load(paste(model,niter,'.rda', sep=''))
time<-m1[,31:60]
Time<-apply(time, 2, mean)
efficiencies<-m1[,1:30]
tg <- data.frame(round(Time,0),t(efficiencies))
colnames(tg)[1]='Time'
melted = melt(tg, id.vars="Time")
print(Time)
apply(efficiencies,2,mean)

colnames(melted)<-c('Time', 'chain','MCMC_Efficiency')
pdf(paste(model,niter,'BoxplotEfficiency.pdf', sep=''))

ggplot(melted, aes(factor(Time), MCMC_Efficiency ))+
  stat_boxplot(geom ='errorbar') +geom_boxplot(aes(fill = (Time)))+scale_x_discrete(name ="Time(s)")+theme(axis.text.x=element_text(angle = 0, hjust = 0)) +ylim(0, 50) 
dev.off()
############################################################################

# Load plyr so we can use ddply() to create the example data set
library(plyr)
library(ggplot2)
library(reshape2)


model='GLMM'
niter=10000

load(paste(model,niter,'.rda', sep=''))
time<-m1[,31:60]
efficiencies<-m1[,1:30]
Time=rep(1:17,30)

tg <- data.frame(Time,as.vector(time),as.vector(efficiencies))
colnames(tg)<-c('chain', 'time','MCMC_Efficiency')
mean(tg$time)
mean(tg$MCMC_Efficiency)

pdf(paste(model,niter,'.pdf', sep=''))
ggplot(data=tg, aes(x=time, y=MCMC_Efficiency, group=as.factor(chain),colour = as.factor(chain))) + geom_line()+ geom_point( size=1, shape=21, fill="white")+ labs(colour = "chain") +xlim(0, 6000)+ ylim(0, 2.5)
dev.off()

model='GLMM'
niter=20000

load(paste(model,niter,'.rda', sep=''))
time<-m1[,31:60]
efficiencies<-m1[,1:30]
Time=rep(1:9,30)

tg <- data.frame(Time,as.vector(time),as.vector(efficiencies))
colnames(tg)<-c('chain', 'time','MCMC_Efficiency')
mean(tg$time)
mean(tg$MCMC_Efficiency)

pdf(paste(model,niter,'.pdf', sep=''))
ggplot(data=tg, aes(x=time, y=MCMC_Efficiency, group=as.factor(chain),colour = as.factor(chain))) + geom_line()+ geom_point( size=1, shape=21, fill="white")+ labs(colour = "chain") +xlim(0, 6000)+ ylim(0, 2.5)
dev.off()

library(plyr)
library(ggplot2)
library(reshape2)

model='GLMM'
niter=5000
load(paste(model,niter,'.rda', sep=''))
time<-m1[,31:60]
Time<-apply(time, 2, mean)
efficiencies<-m1[,1:30]
tg <- data.frame(round(Time,2),t(efficiencies))
colnames(tg)[1]='Time'
melted = melt(tg, id.vars="Time")

Time<-apply(time, 2, mean)
efficiencies<-m1[,1:30]
tg <- data.frame(round(Time,0),t(efficiencies))
colnames(tg)[1]='Time'
melted = melt(tg, id.vars="Time")
print(Time)
apply(efficiencies,2,mean)

colnames(melted)<-c('Time', 'chain','MCMC_Efficiency')
pdf(paste(model,niter,'BoxplotEfficiency.pdf', sep=''))

ggplot(melted, aes(factor(Time), MCMC_Efficiency ))+
  stat_boxplot(geom ='errorbar') +geom_boxplot(aes(fill = (Time)))+scale_x_discrete(name ="Time(s)")+theme(axis.text.x=element_text(angle = -45, hjust = 0,vjust=0.5)) + ylim(0, 1.6)  
dev.off()


model='GLMM'
niter=10000
load(paste(model,niter,'.rda', sep=''))
time<-m1[,31:60]
Time<-apply(time, 2, mean)
efficiencies<-m1[,1:30]
tg <- data.frame(round(Time,2),t(efficiencies))
colnames(tg)[1]='Time'
melted = melt(tg, id.vars="Time")

Time<-apply(time, 2, mean)
efficiencies<-m1[,1:30]
tg <- data.frame(round(Time,0),t(efficiencies))
colnames(tg)[1]='Time'
melted = melt(tg, id.vars="Time")
print(Time)
apply(efficiencies,2,mean)

colnames(melted)<-c('Time', 'chain','MCMC_Efficiency')
pdf(paste(model,niter,'BoxplotEfficiency.pdf', sep=''))

ggplot(melted, aes(factor(Time), MCMC_Efficiency ))+
    stat_boxplot(geom ='errorbar') +geom_boxplot(aes(fill = (Time)))+scale_x_discrete(name ="Time(s)")+theme(axis.text.x=element_text(angle = -45, hjust = 0,vjust=0.5)) + ylim(0, 1.6)  
dev.off()


model='GLMM'
niter=20000
load(paste(model,niter,'.rda', sep=''))
time<-m1[,31:60]
Time<-apply(time, 2, mean)
efficiencies<-m1[,1:30]
tg <- data.frame(round(Time,2),t(efficiencies))
colnames(tg)[1]='Time'
melted = melt(tg, id.vars="Time")

Time<-apply(time, 2, mean)
efficiencies<-m1[,1:30]
tg <- data.frame(round(Time,2),t(efficiencies))
colnames(tg)[1]='Time'
melted = melt(tg, id.vars="Time")
print(Time)
apply(efficiencies,2,mean)

colnames(melted)<-c('Time', 'chain','MCMC_Efficiency')
pdf(paste(model,niter,'BoxplotEfficiency.pdf', sep=''))

ggplot(melted, aes(factor(Time), MCMC_Efficiency ))+
  stat_boxplot(geom ='errorbar') +geom_boxplot(aes(fill = (Time)))+scale_x_discrete(name ="Time(s)")+theme(axis.text.x=element_text(angle = -45, hjust = 0)) + ylim(0, 1.6)  
dev.off()



############################################################################

# Load plyr so we can use ddply() to create the example data set
library(plyr)
library(ggplot2)
library(reshape2)

model='spatial'
niter=5000

load(paste(model,niter,'.rda', sep=''))
time<-m1[,31:60]
efficiencies<-m1[,1:30]
Time=rep(1:18,30)

tg <- data.frame(Time,as.vector(time),as.vector(efficiencies))
colnames(tg)<-c('chain', 'time','MCMC_Efficiency')
mean(tg$time)
mean(tg$MCMC_Efficiency)

pdf(paste(model,niter,'.pdf', sep=''))
ggplot(data=tg, aes(x=time, y=MCMC_Efficiency, group=as.factor(chain),colour = as.factor(chain))) + geom_line()+ geom_point( size=1, shape=21, fill="white")+ labs(colour = "chain")+ xlim(0, 42000)+ ylim(0, 1) 
dev.off()

model='spatial'
niter=10000

load(paste(model,niter,'.rda', sep=''))
time<-m1[,31:60]
efficiencies<-m1[,1:30]
Time=rep(1:19,30)

tg <- data.frame(Time,as.vector(time),as.vector(efficiencies))
colnames(tg)<-c('chain', 'time','MCMC_Efficiency')
mean(tg$time)
mean(tg$MCMC_Efficiency)

pdf(paste(model,niter,'.pdf', sep=''))
ggplot(data=tg, aes(x=time, y=MCMC_Efficiency, group=as.factor(chain),colour = as.factor(chain))) + geom_line()+ geom_point( size=1, shape=21, fill="white")+ labs(colour = "chain")+ xlim(0, 42000)+ ylim(0, 1) 
dev.off()


library(plyr)
library(ggplot2)
library(reshape2)


model='spatial'
niter=5000
load(paste(model,niter,'.rda', sep=''))
time<-m1[,31:60]
Time<-apply(time, 2, mean)
efficiencies<-m1[,1:30]
tg <- data.frame(round(Time,2),t(efficiencies))
colnames(tg)[1]='Time'
melted = melt(tg, id.vars="Time")

Time<-apply(time, 2, mean)
efficiencies<-m1[,1:30]
tg <- data.frame(round(Time,2),t(efficiencies))
colnames(tg)[1]='Time'
melted = melt(tg, id.vars="Time")
print(Time)
apply(efficiencies,2,mean)

colnames(melted)<-c('Time', 'chain','MCMC_Efficiency')
pdf(paste(model,niter,'BoxplotEfficiency.pdf', sep=''))

ggplot(melted, aes(factor(Time), MCMC_Efficiency ))+
  stat_boxplot(geom ='errorbar') +geom_boxplot(aes(fill = (Time)))+scale_x_discrete(name ="Time(s)")+theme(axis.text.x=element_text(angle = -45, hjust = 0)) + ylim(0, 1.0)  
dev.off()



############################################################################
