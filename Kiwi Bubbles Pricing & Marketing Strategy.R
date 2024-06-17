library("AER")
library("plotly")
library('RColorBrewer')
library("data.table")
library("mlogit")
library("gmnl")

data=fread("kiwi_bubbles_P2.csv",stringsAsFactors = F)
demo=fread("demo_P2.csv",stringsAsFactors = F)

data=data[!(data$price.KB==99),]
data=data[!(data$price.KR==99),]
data=data[!(data$price.MB==99),]


avg_price_KB = mean(data$price.KB) 
avg_price_KR = mean(data$price.KR) 
avg_price_MB = mean(data$price.MB) 
avg_price_all=c(avg_price_KB,avg_price_KR,avg_price_MB)


mlogitdata = mlogit.data(data, id = 'id', varying = 4:7,choice = 'choice', shape = 'wide')

mle_noseg = gmnl(choice ~ price, data = mlogitdata)
summary(mle_noseg) 

beta1=-3.73793
beta0KB = 4.25316
beta0KR = 4.36240
beta0MB = 4.20440

parameter_1=c(beta0KB,beta0KR,beta0MB,beta1)

demand_a=function(p1,p2,parameter_1)
{  
    prob=exp(parameter_1[1]+parameter_1[4]*p1)/(1+exp(parameter_1[1]+parameter_1[4]*p1)+exp(parameter_1[2]+parameter_1[4]*p2))
    return(prob)
}


demand_b=function(pKB,pKR,pMB,parameter_1){
    prob=exp(parameter_1[1]+parameter_1[4]*pKB)/(1+exp(parameter_1[1]+parameter_1[4]*pKB)+exp(parameter_1[2]+parameter_1[4]*pKR)+exp(parameter_1[3]+parameter_1[4]*pMB))
    return(prob)
}


aggregate_choice_b=function(pKB,pKR, own, rival) {  
    m=c(own+1,rival+1,5)
    aggregate_choice=seg.share[1]*demand_a(pKB,pKR,as.numeric(coef.est[1,m]))+
        seg.share[2]*demand_a(pKB,pKR,as.numeric(coef.est[2,m]))+
        seg.share[3]*demand_a(pKB,pKR,as.numeric(coef.est[3,m]))+
        seg.share[4]*demand_a(pKB,pKR,as.numeric(coef.est[4,m]))+
        seg.share[5]*demand_a(pKB,pKR,as.numeric(coef.est[5,m]))+ 
        seg.share[6]*demand_a(pKB,pKR,as.numeric(coef.est[6,m]))+
        seg.share[7]*demand_a(pKB,pKR,as.numeric(coef.est[7,m]))+
        seg.share[8]*demand_a(pKB,pKR,as.numeric(coef.est[8,m]))+
        seg.share[9]*demand_a(pKB,pKR,as.numeric(coef.est[9,m]))
    return(aggregate_choice)
}


aggregate_choice=function(pKB,pKR,pMB) {
    
    aggregate_choice=seg.share[1]*demand_b(pKB,pKR,pMB,as.numeric(coef.est[1,2:5]))+
        seg.share[2]*demand_b(pKB,pKR,pMB,as.numeric(coef.est[2,2:5]))+
        seg.share[3]*demand_b(pKB,pKR,pMB,as.numeric(coef.est[3,2:5]))+
        seg.share[4]*demand_b(pKB,pKR,pMB,as.numeric(coef.est[4,2:5]))+
        seg.share[5]*demand_b(pKB,pKR,pMB,as.numeric(coef.est[5,2:5]))+ 
        seg.share[6]*demand_b(pKB,pKR,pMB,as.numeric(coef.est[6,2:5]))+
        seg.share[7]*demand_b(pKB,pKR,pMB,as.numeric(coef.est[7,2:5]))+
        seg.share[8]*demand_b(pKB,pKR,pMB,as.numeric(coef.est[8,2:5]))+
        seg.share[9]*demand_b(pKB,pKR,pMB,as.numeric(coef.est[9,2:5]))
    
    return(aggregate_choice)
}


demand_KB_KR=function(pKB,pKR,pMB,parameter_1){
    probability_KB=exp(parameter_1[1]+parameter_1[4]*pKB)/(1+exp(parameter_1[1]+parameter_1[4]*pKB)+exp(parameter_1[2]+parameter_1[4]*pKR)+exp(parameter_1[3]+parameter_1[4]*pMB))
    probability_KR=exp(parameter_1[2]+parameter_1[4]*pKR)/(1+exp(parameter_1[1]+parameter_1[4]*pKB)+exp(parameter_1[2]+parameter_1[4]*pKR)+exp(parameter_1[3]+parameter_1[4]*pMB))
    return(cbind(probability_KB,probability_KR))
}
demand_KB_KR_MB=function(pKB,pKR,pMB,parameter_1){
    probability_KB=exp(parameter_1[1]+parameter_1[4]*pKB)/(1+exp(parameter_1[1]+parameter_1[4]*pKB)+exp(parameter_1[2]+parameter_1[4]*pKR)+exp(parameter_1[3]+parameter_1[4]*pMB))
    probability_KR=exp(parameter_1[2]+parameter_1[4]*pKR)/(1+exp(parameter_1[1]+parameter_1[4]*pKB)+exp(parameter_1[2]+parameter_1[4]*pKR)+exp(parameter_1[3]+parameter_1[4]*pMB))
    probability_MB=exp(parameter_1[3]+parameter_1[4]*pKR)/(1+exp(parameter_1[1]+parameter_1[4]*pKB)+exp(parameter_1[2]+parameter_1[4]*pKR)+exp(parameter_1[3]+parameter_1[4]*pMB))
    return(cbind(probability_KB,probability_KR,probability_MB))
}

profit_KB_KR=function(pKB,pKR, pMB,parameter_1){
    profitKB=1000*(demand_KB_KR(pKB,pKR, pMB,parameter_1)[,1]*(pKB-0.5))
    profitKR=1000*(demand_KB_KR(pKB,pKR, pMB,parameter_1)[,2]*(pKR-0.5))
    return(cbind(profitKB,profitKR))
}

avg_price_KB = mean(data$price.KB) 
avg_price_KR = mean(data$price.KR) 
avg_price_MB = mean(data$price.MB) 
avg_price_all=c(avg_price_KB,avg_price_KR,avg_price_MB)


demand_KB = demand_b(avg_price_KB,avg_price_KR,avg_price_MB,parameter_1) 
demand_KR = demand_b(avg_price_KR,avg_price_KB,avg_price_MB,parameter_1[c(2,1,3,4)])  
demand_MB = demand_b(avg_price_MB,avg_price_KB,avg_price_KR,parameter_1[c(3,1,2,4)]) 

own_elasticity_KB = -(beta1)*avg_price_KB*(1-demand_KB) 

own_elasticity_KR = -(beta1)*avg_price_KR*(1-demand_KR) 

own_elasticity_MB = -(beta1)*avg_price_MB*(1-demand_MB) 


cross_elasticity_KR = -(beta1)*avg_price_KR*demand_KR 
cross_elasticity_MB = -(beta1)*avg_price_MB*demand_MB 
cross_elasticity_KB = -(beta1)*avg_price_KB*demand_KB 

elasticity_KB_KR_MB <- data.frame("Product"=c("KB","KR","MB"), "Own Elasticity"=c(own_elasticity_KB,own_elasticity_KR,own_elasticity_MB), 
                                  "Cross Price Elasticity"=c(cross_elasticity_KB,cross_elasticity_KR,cross_elasticity_MB))
elasticity_KB_KR_MB


uc=0.5
pMB=1.43

demand_KB_KR=function(pKB,pKR,pMB,parameter_1){
    probability_KB=exp(parameter_1[1]+parameter_1[4]*pKB)/(1+exp(parameter_1[1]+parameter_1[4]*pKB)+exp(parameter_1[2]+parameter_1[4]*pKR)+exp(parameter_1[3]+parameter_1[4]*pMB))
    probability_KR=exp(parameter_1[2]+parameter_1[4]*pKR)/(1+exp(parameter_1[1]+parameter_1[4]*pKB)+exp(parameter_1[2]+parameter_1[4]*pKR)+exp(parameter_1[3]+parameter_1[4]*pMB))
    return(cbind(probability_KB,probability_KR))
}


profit_KB_KR=function(pKB,pKR, pMB,parameter_1){
    profitKB=1000*(demand_KB_KR(pKB,pKR, pMB,parameter_1)[,1]*(pKB-0.5))
    profitKR=1000*(demand_KB_KR(pKB,pKR, pMB,parameter_1)[,2]*(pKR-0.5))
    return(cbind(profitKB,profitKR))
}


aux=seq(1,3,0.01)

pricespace=expand.grid(aux,aux)

profitmat=matrix(0L,nrow(pricespace),1)

for (i in 1:nrow(pricespace)){
    profitmat[i]=sum(profit_KB_KR(pricespace[i,1],pricespace[i,2],1.43,parameter_1))  
}



scatterplot3d(pricespace[, 1], pricespace[, 2], profitmat,
              xlab = "P^{KB}", ylab = "P^{KR}", zlab = "Profit",
              color = adjustcolor("cyan", alpha.f = 0.6), pch = 16)


max(profitmat)

infoTable = data.frame(maxProfit = max(profitmat), 
                       pKR = pricespace[profitmat==max(profitmat)][1],
                       pKB = pricespace[profitmat==max(profitmat)][2])
infoTable

N=283
set.seed(0)
data_copy = data

segment_list = list()

for(i in 1:15){
    demo_cluster = kmeans(x=demo[, 2:18], centers = i, nstart = 1000)
    data_copy=data
    
    
    cluster_id = data.frame(id = demo$id)
    cluster_id$cluster = demo_cluster$cluster
    data_copy = merge(data_copy, cluster_id, by = "id", all.x = T)
    
    data_copy$cluster[is.na(data_copy$cluster)] = i+1
    
    seg.share = c( table(demo_cluster$cluster),N - sum(table(demo_cluster$cluster))) / N
    
    
    segment_list[[i]] = seg.share
    
    
    data_copy = data_copy[,1:8]
}
segment_list

demo_cluster = kmeans(x=demo[, 2:18], centers = 8, nstart = 1000)
summary(demo_cluster)


cluster_id = data.frame(id = demo$id)
cluster_id$cluster = demo_cluster$cluster
data = merge(data, cluster_id, by = "id", all.x = T)


data$cluster[is.na(data$cluster)] = 9


seg.share = c( table(demo_cluster$cluster),N - sum(table(demo_cluster$cluster))) / N


coef.est = data.frame(segment = 1:9, intercept.KB = NA, intercept.KR = NA, 
                      intercept.MB = NA, price.coef = NA) 

for (seg in 1:9) {
    
    data.sub = subset(data, cluster == seg)
    
    
    mlogitdata = mlogit.data(data.sub,id="id",varying=4:7,choice="choice",shape="wide")
    
    mle = gmnl(choice ~  price, data = mlogitdata)
    mle
    
    coef.est[seg, 2:5] = mle$coefficients
}
coef.est

demand_b=function(pKB,pKR,pMB,parameter_1){
    probability_KB=exp(parameter_1[1]+parameter_1[4]*pKB)/(1+exp(parameter_1[1]+parameter_1[4]*pKB)+exp(parameter_1[2]+parameter_1[4]*pKR)+exp(parameter_1[3]+parameter_1[4]*pMB))
    probability_KR=exp(parameter_1[2]+parameter_1[4]*pKR)/(1+exp(parameter_1[1]+parameter_1[4]*pKB)+exp(parameter_1[2]+parameter_1[4]*pKR)+exp(parameter_1[3]+parameter_1[4]*pMB))
    probability_MB=exp(parameter_1[3]+parameter_1[4]*pKR)/(1+exp(parameter_1[1]+parameter_1[4]*pKB)+exp(parameter_1[2]+parameter_1[4]*pKR)+exp(parameter_1[3]+parameter_1[4]*pMB))
    return(cbind(probability_KB,probability_KR,probability_MB))
}
aggregate_choice=function(pKB,pKR,pMB) {
    
    aggregate_choice=seg.share[1]*demand_b(pKB,pKR,pMB,as.numeric(coef.est[1,2:5]))+
        seg.share[2]*demand_b(pKB,pKR,pMB,as.numeric(coef.est[2,2:5]))+
        seg.share[3]*demand_b(pKB,pKR,pMB,as.numeric(coef.est[3,2:5]))+
        seg.share[4]*demand_b(pKB,pKR,pMB,as.numeric(coef.est[4,2:5]))+
        seg.share[5]*demand_b(pKB,pKR,pMB,as.numeric(coef.est[5,2:5]))+ 
        seg.share[6]*demand_b(pKB,pKR,pMB,as.numeric(coef.est[6,2:5]))+
        seg.share[7]*demand_b(pKB,pKR,pMB,as.numeric(coef.est[7,2:5]))+
        seg.share[8]*demand_b(pKB,pKR,pMB,as.numeric(coef.est[8,2:5]))+
        seg.share[9]*demand_b(pKB,pKR,pMB,as.numeric(coef.est[9,2:5]))
    return(aggregate_choice)
}

average_price_vector=list()
for (i in 1:3){
    avg_price_KB = mean(data$price.KB[data$cluster==i]) 
    avg_price_KR = mean(data$price.KR[data$cluster==i]) 
    avg_price_MB = mean(data$price.MB[data$cluster==i]) 
    average_price_vector[[i]]=c(avg_price_KB,avg_price_KR,avg_price_MB)
}


parameter_2=c(beta0KR,beta0MB,beta1)
demand_a=function(p1,p2,parameter_2){  
    prob1=exp(parameter_2[1]+parameter_2[3]*p1)/(1+exp(parameter_2[1]+parameter_2[3]*p1)+exp(parameter_2[2]+parameter_2[3]*p2))
    prob2=exp(parameter_2[2]+parameter_2[3]*p1)/(1+exp(parameter_2[1]+parameter_2[3]*p1)+exp(parameter_2[2]+parameter_2[3]*p2))
    return(cbind(prob1,prob2))
}


aggregate_choice_b=function(pKB,pKR, own, rival) {  
    m=c(own+1,rival+1,5)
    aggregate_choice=seg.share[1]*demand_a(pKB,pKR,as.numeric(coef.est[1,m]))+
        seg.share[2]*demand_a(pKB,pKR,as.numeric(coef.est[2,m]))+
        seg.share[3]*demand_a(pKB,pKR,as.numeric(coef.est[3,m]))+
        seg.share[4]*demand_a(pKB,pKR,as.numeric(coef.est[4,m]))+
        seg.share[5]*demand_a(pKB,pKR,as.numeric(coef.est[5,m]))+ 
        seg.share[6]*demand_a(pKB,pKR,as.numeric(coef.est[6,m]))+
        seg.share[7]*demand_a(pKB,pKR,as.numeric(coef.est[7,m]))+
        seg.share[8]*demand_a(pKB,pKR,as.numeric(coef.est[8,m]))+
        seg.share[9]*demand_a(pKB,pKR,as.numeric(coef.est[9,m]))
    return(aggregate_choice)
}


elasticity_own=function(avg_price_all,own){
    coef2=c(1,2,3)[c(1,2,3)!=own][1]
    coef3=c(1,2,3)[c(1,2,3)!=own][2]
    aggregate_choice=aggregate_choice(avg_price_all[own],avg_price_all[coef2],avg_price_all[coef3])
    new_aggregate_choice=aggregate_choice(avg_price_all[own]*1.01,avg_price_all[coef2],avg_price_all[coef3])
    (new_aggregate_choice-aggregate_choice)/aggregate_choice
    
} 
KB_elasticity=(elasticity_own(avg_price_all,1)[1])*-100  
KR_elasticity=(elasticity_own(avg_price_all,2)[1])*-100 
MB_elasticity=(elasticity_own(avg_price_all,3)[1])*-100 

Own_elasticity_all <- data.frame("Product"=c("KB","KR","MB"), "Own Elasticity"=c(KB_elasticity,KR_elasticity,MB_elasticity))
Own_elasticity_all 


cross_elasticity=function(avg_price_all,own){
    ce=vector()
    coef2=c(1,2,3)[c(1,2,3)!=own][1]
    coef3=c(1,2,3)[c(1,2,3)!=own][2]
    aggregate_choice=aggregate_choice(avg_price_all[own],avg_price_all[coef2],avg_price_all[coef3])[1]
    new_agg_choice1=aggregate_choice(avg_price_all[own],avg_price_all[coef2]*1.01,avg_price_all[coef3])[1]
    new_agg_choice2=aggregate_choice(avg_price_all[own],avg_price_all[coef2],avg_price_all[coef3]*1.01)[1]
    
    ce=append(ce, (new_agg_choice1-aggregate_choice)/aggregate_choice)
    ce=append(ce, (new_agg_choice2-aggregate_choice)/aggregate_choice)
    ce
    
} 
Crosselas_KB_KR_MB=(cross_elasticity(avg_price_all, 1))*100  
Crosselas_KB_KR_MB


Crosselas_KR_KB_MB=(cross_elasticity(avg_price_all, 2))*100 
Crosselas_KR_KB_MB


Crosselas_MB_KB_KR=(cross_elasticity(avg_price_all, 3))*100  
Crosselas_MB_KB_KR


pop_comparison = data.frame(segments = 1:9, 
                            difference_KB_KR = coef.est$intercept.KB-coef.est$intercept.KR, 
                            difference_KB_MB = coef.est$intercept.KB-coef.est$intercept.MB,
                            difference_MB_KR = coef.est$intercept.MB-coef.est$intercept.KR)
pop_comparison

difference_comparison = data.frame(avg_difference_KB_KR=mean(pop_comparison$difference_KB_KR),
                                   avg_difference_KB_MB=mean(pop_comparison$difference_KB_MB))
difference_comparison


plot(pop_comparison$difference_KB_KR, coef.est[,5], 
     main = ("Preference Comparison between KB and KR"),
     xlab="Preference of KB - Preference of KR",ylab=("Price Sensitivity"),
     xlim=c(-1.5,2),ylim=c(-6,-1),
     type='n')
points(pop_comparison$difference_KB_KR[4],coef.est$price.coef[4],cex=20*seg.share[4],col='orange',pch=16)
points(pop_comparison$difference_KB_KR[6],coef.est$price.coef[6],cex=20*seg.share[6],col='orange',pch=16)
points(pop_comparison$difference_KB_KR[7],coef.est$price.coef[7],cex=20*seg.share[7],col='orange',pch=16)
points(pop_comparison$difference_KB_KR[8],coef.est$price.coef[8],cex=20*seg.share[8],col='orange',pch=16)
points(pop_comparison$difference_KB_KR[1],coef.est$price.coef[1],cex=20*seg.share[1],col='green',pch=16)
points(pop_comparison$difference_KB_KR[3],coef.est$price.coef[3],cex=20*seg.share[3],col='green',pch=16)
points(pop_comparison$difference_KB_KR[5],coef.est$price.coef[5],cex=20*seg.share[5],col='green',pch=16)
points(pop_comparison$difference_KB_KR[2],coef.est$price.coef[2],cex=20*seg.share[2],col='green',pch=16)
points(pop_comparison$difference_KB_KR[9],coef.est$price.coef[9],cex=20*seg.share[9],col='green',pch=16)

plot(pop_comparison$difference_MB_KR, coef.est[,5],
     main = ("Preference Comparison between MB and KR"),
     xlab="Preference of MB - Preference of KR",ylab=("Price Sensitivity"),
     xlim=c(-1.5,2),ylim=c(-6,-1), 
     type='n')
points(pop_comparison$difference_MB_KR[4],coef.est$price.coef[4],cex=20*seg.share[4],col='orange',pch=16)
points(pop_comparison$difference_MB_KR[6],coef.est$price.coef[6],cex=20*seg.share[6],col='orange',pch=16)
points(pop_comparison$difference_MB_KR[7],coef.est$price.coef[7],cex=20*seg.share[7],col='orange',pch=16)
points(pop_comparison$difference_MB_KR[8],coef.est$price.coef[8],cex=20*seg.share[8],col='orange',pch=16)
points(pop_comparison$difference_MB_KR[2],coef.est$price.coef[2],cex=20*seg.share[2],col='green',pch=16)
points(pop_comparison$difference_MB_KR[1],coef.est$price.coef[1],cex=20*seg.share[1],col='green',pch=16)
points(pop_comparison$difference_MB_KR[3],coef.est$price.coef[3],cex=20*seg.share[3],col='green',pch=16)
points(pop_comparison$difference_MB_KR[5],coef.est$price.coef[5],cex=20*seg.share[5],col='green',pch=16)
points(pop_comparison$difference_MB_KR[9],coef.est$price.coef[9],cex=20*seg.share[9],col='green',pch=16)

pricespace=seq(1,3,0.01)
uc=0.5
profit=1000*aggregate_choice_b(pricespace,1.43,2,3)[,1]*(pricespace-uc)
price_KR_Best=pricespace[profit==max(profit)] 
price_KR_Best

max(profit)

pricespace[profit==max(profit)]

profit_MB=1000*aggregate_choice_b(1.43,price_KR_Best,3,2)[,1]*(1.43-uc)   
profit_MB

parameter_1=c(beta0KB,beta0KR,beta0MB,beta1)
profit_KB_KR_seg=function(pKB,pKR, pMB,parameter_1){
    profitKB=1000*(aggregate_choice(pKB,pKR, pMB)[,1]*(pKB-0.5))
    profitKR=1000*(aggregate_choice(pKB,pKR, pMB)[,2]*(pKR-0.5))
    return(cbind(profitKB,profitKR))
}

aux=seq(1,3,0.01)

pricespace=expand.grid(aux,aux)

profitmat=matrix(0L,nrow(pricespace),1)
for (i in 1:nrow(pricespace)){
    profitmat[i]=sum(profit_KB_KR_seg(pricespace[i,1],pricespace[i,2],1.43,parameter_1))  
}

max(profitmat) 

pKB_KRBest=pricespace[profitmat==max(profitmat)] 
pKB_KRBest

profit_MB2=1000*aggregate_choice(1.43,pKB_KRBest,pKB_KRBest)[2]*(1.43-uc) 
profit_MB2 

pricespace=seq(0,2,0.01)
uc=0.5
profit=1000*aggregate_choice(pricespace,1.15,1.19)[,1]*(pricespace-uc)

max(profit)  

pricespace[profit==max(profit)] 


aux=seq(1,3,0.01)

pricespace=expand.grid(aux,aux)
profitmat=matrix(0L,nrow(pricespace),1)
for (i in 1:nrow(pricespace)){
    profitmat[i]=sum(profit_KB_KR_seg(pricespace[i,1],pricespace[i,2],0.96,parameter_1))  
}

max(profitmat) 

pricespace=seq(0,2,0.01)
uc=0.5
profit=1000*aggregate_choice(pricespace,1.01,1.09)[,1]*(pricespace-uc)

max(profit)  

pricespace[profit==max(profit)] 


aux=seq(1,3,0.01)

pricespace=expand.grid(aux,aux)
profitmat=matrix(0L,nrow(pricespace),1)
for (i in 1:nrow(pricespace)){
    profitmat[i]=sum(profit_KB_KR_seg(pricespace[i,1],pricespace[i,2],0.92,parameter_1))  
}
max(profitmat)

pricespace[profitmat==max(profitmat)] 


pricespace=seq(0,2,0.01)
uc=0.5
profit=1000*aggregate_choice(pricespace,1.00,1.08)[,1]*(pricespace-uc)

max(profit)  

pricespace[profit==max(profit)] 

aux=seq(1,3,0.01)

pricespace=expand.grid(aux,aux)
profitmat=matrix(0L,nrow(pricespace),1)
for (i in 1:nrow(pricespace)){
    profitmat[i]=sum(profit_KB_KR_seg(pricespace[i,1],pricespace[i,2],0.92,parameter_1))  
}

max(profitmat) 
pricespace[profitmat==max(profitmat)] 