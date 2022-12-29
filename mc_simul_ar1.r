library(MSwM)
library(readxl)
library(e1071)
library(rpart)
mc=function(N){
  matriz_acertos=matrix(NA,ncol = 3,nrow = N)
  matriz_resul_pred=matrix(NA,ncol = 3,nrow = N)
  for(n in 1:N){
#Ex 1
#################################################
#####################CASO AR1####################
#################################################
#Algoritmo EM para maximização de Log Verossimilhanças (caso AR(1))
#set.seed(43985)
n_states=2 #define num estados
TT=150 #define quantos observações de yt temos
y_1=vector()
y_2=vector()
phi_1=0.5
phi_2=-0.6
c1=0
c2=0
y_1[1]=c1
for (j in 2:TT){
  y_1[j]=c1+phi_1*y_1[j-1]+rnorm(1,0.1,2.1)
}
y_2[1]=c2
for (j in 2:TT){
  y_2[j]=c2+phi_2*y_2[j-1]+rnorm(1,0.1,1.1)
}
y=vector()
y_state=vector()
init_state=1
y[1]=y_1[1]
y_state[1]=init_state
p11=1/4
p22=3/4
p12=1-p11
p21=1-p22
P=matrix(data = c(p11,p12,p21,p22),ncol = 2)
P

#simulando dados
for (t in 2:TT){
  if(y_state[t-1]==1) ind_state=rbinom(1,1,p11) #define estado 1 como sucesso em uma dist de bernoulli
  if(y_state[t-1]==2) ind_state=rbinom(1,1,p21)
  
  if(ind_state==1){y[t]=y_1[t];y_state[t]=1}
  if(ind_state==0){y[t]=y_2[t];y_state[t]=2}
}
par(mfrow=c(1,1))
#plot(y,col = ifelse(y_state == 1,'red','blue'))
#lines(y)
#plot(y,col = ifelse(y_state == 1,'red','blue'))
#plot(y_state,col = ifelse(y_state == 1,'red','blue'))
#sum(y_state==1)/TT

#estimando parametros
#sabendo que existem 2 estados e sendo que var resposta para ambos vem de uma dist normal
mu_space=seq(-5,5,0.2)
sigma_space=seq(-5,5,0.2)
pi_space=seq(0,1,0.01)
mu_hat=rep(0,n_states)
sigma_hat=rep(0,n_states)
p_hat=rep(0,n_states)
pi_hat=rep(0,n_states)
sd_y=sd(y)
mean_y=mean(y)
#inicializando probabilidades de cada estado
epsolon_tt_hat=matrix(data = NA,ncol = n_states,nrow = TT)
epsolon_tt_hat[1,]=t(c(0.5,0.5))
ni=pnorm(y,)
L=rep(-Inf,n_states)
L1=vector()
L2=vector()
state_hat=vector()

#supondo que sabemos o vetor de parâmetros real e a distribuição da variável resposta em cada estado
p1_hat=dnorm(y,mean = c1/(1-phi_1),sd = 1.1/sqrt(1-phi_1^2))
p2_hat=dnorm(y,mean = c2/(1-phi_2),sd = 1.1/sqrt(1-phi_2^2))
epsolon_tt_hat=matrix(NA,ncol = n_states+1,nrow = TT)
epsolon_tt_hat[,1]=p1_hat
epsolon_tt_hat[,2]=p2_hat
epsolon_tt_hat[,3]=2
y_state_hat=vector(length = TT)
epsolon_tt_hat[epsolon_tt_hat[,1]>=epsolon_tt_hat[,2],3]=1
par(mfrow=c(3,1))
#par(mfrow=c(3,1))
#plot(y,col = ifelse(y_state == 1,'red','blue'),main = "Plot variável resposta y")
#plot(y_state,col = ifelse(y_state == 1,'red','blue'),main = "Plot dos estados da var resposta y")
#plot(epsolon_tt_hat[,3],col = ifelse(epsolon_tt_hat[,3] == 1,'red','blue'),main = "Plot dos estados estimados da var resposta y")
#sum(epsolon_tt_hat[,3]==y_state)/TT



#pacf(y)
y_2=y[1:(TT-2)]
#y_4=y[1:(TT-4)]
d2=as.data.frame(cbind(y[-c(1,2)],y_2+rnorm(length(y_2),0,.000001)))
#d4=as.data.frame(cbind(y[-c(1,2,3,4)],y_4+rnorm(length(y_4),0,.000001)))
#d24=as.data.frame(cbind(y[-c(1,2,3,4)],y_4+rnorm(length(y_4),0,.000001),y_2[-c(1:2)]+rnorm(length(y_2[-c(1:2)]),0,.000001)))
lm_2=lm(d2$V1~d2$V2,d2)
#lm_4=lm(d4$V1~d4$V2,d4)
#lm_2_4=lm(d24$V1~d24$V2+d24$V3,d24)
#summary(lm_2_4)



##################################
#####MARKOV#######################
##################################
#lag 2
mod.mswm=msmFit(lm_2, k=2, p=0, sw=c(T,T,T))
#summary(mod.mswm)
#plotProb(mod.mswm, which=1)
msmResid(mod.mswm,1)
y_state_est_markov=rep(2,TT-2)
y_state_est_markov_temp1=vector()
y_state_est_markov_temp1[1:2]=y_state[1:2]
y_state_est_markov[mod.mswm@Fit@filtProb[,1]<mod.mswm@Fit@filtProb[,2]]=1
y_state_est_markov=c(y_state_est_markov_temp1,y_state_est_markov)
#par(mfrow=c(3,1))
#plot(y_state,col = ifelse(y_state == 1,'red','blue'),main = "Plot dos estados da var resposta y")
#plot(y_state_est_markov,col = ifelse(y_state_est_markov == 1,'red','blue'),main = "Plot dos estados estimados da var resposta y")
aceros_markov_2=sum(y_state_est_markov==y_state)/TT

#lag 4
#mod.mswm=msmFit(lm_4, k=2, p=0, sw=c(T,T,T))
#summary(mod.mswm)
#plotProb(mod.mswm, which=1)
#msmResid(mod.mswm,1)
#y_state_est_markov=rep(1,TT-4)
#y_state_est_markov_temp1=vector()
#y_state_est_markov_temp1[1:4]=y_state[1:4]
#y_state_est_markov[mod.mswm@Fit@filtProb[,1]<mod.mswm@Fit@filtProb[,2]]=2
#y_state_est_markov=c(y_state_est_markov_temp1,y_state_est_markov)
#par(mfrow=c(3,1))
#plot(y_state,col = ifelse(y_state == 1,'red','blue'),main = "Plot dos estados da var resposta y")
#plot(y_state_est_markov,col = ifelse(y_state_est_markov == 1,'red','blue'),main = "Plot dos estados estimados da var resposta y")
#aceros_markov_4=sum(y_state_est_markov==y_state)/TT


#lag 4 e 2
#mod.mswm=msmFit(lm_2_4, k=2, p=0, sw=c(T,T,T,T))
#summary(mod.mswm)
#plotProb(mod.mswm, which=1)
#msmResid(mod.mswm,1)
#y_state_est_markov=rep(1,TT-4)
#y_state_est_markov_temp1=vector()
#y_state_est_markov_temp1[1:4]=y_state[1:4]
#y_state_est_markov[mod.mswm@Fit@filtProb[,1]<mod.mswm@Fit@filtProb[,2]]=2
#y_state_est_markov=c(y_state_est_markov_temp1,y_state_est_markov)
#par(mfrow=c(3,1))
#plot(y_state,col = ifelse(y_state == 1,'red','blue'),main = "Plot dos estados da var resposta y")
#plot(y_state_est_markov,col = ifelse(y_state_est_markov == 1,'red','blue'),main = "Plot dos estados estimados da var resposta y")
#aceros_markov=sum(y_state_est_markov==y_state)/TT
matriz_acertos[n,1]=aceros_markov_2


##############################################################
#linear

#Separando dados para teste
dataset_svm_train=as.data.frame(y[1:length(y)])
dataset_svm_test=data.frame(y[121:150]) #selecionar dados do grupo 1 para a base de teste (30observações)

#################################################
####################SVM##########################
#################################################
#dataset_svm_train=dataset_svm_train[order(dataset_svm_train$V1),]
classifier = svm(y =  y_state[1:length(y)] , x = dataset_svm_train, type = 'C-classification', kernel = 'linear') #roda svm tentando prver var 31 (variavel binaria de grupo) a partir dos dados
#da série de tempo
#summary(classifier) #aqui a gente vê quantos dados realmente o modelo svm usou para classificar os dados (63 dos 800 para classe 0 e 65 dos 200 para classe 1)
par(mfrow=c(3,1))
y_fitted=as.vector(classifier$fitted)
#plot(y,col = ifelse(y_state == 1,'red','blue'),main = "Plot variável resposta y")
#lines(y)
#plot(y_state,col = ifelse(y_state == 1,'red','blue'),main = "Plot dos estados da variável resposta y")
#plot(y_fitted,col = ifelse(classifier$fitted == 1,'red','blue'),main = "Plot dos estados estimados da variável resposta y")
acerto_classificacao=sum(y_fitted==y_state[1:length(y)])/length(y)

#matriz_acertos[n,2]=acerto_classificacao

###############################################################
#radial
#Separando dados para teste
dataset_svm_train=as.data.frame(y[1:length(y)])
dataset_svm_test=data.frame(y[121:150]) #selecionar dados do grupo 1 para a base de teste (30observações)

#################################################
####################SVM##########################
#################################################
#dataset_svm_train=dataset_svm_train[order(dataset_svm_train$V1),]
classifier = svm(y =  y_state[1:length(y)] , x = dataset_svm_train, type = 'C-classification', kernel = 'radial',gamma = 0.1) #roda svm tentando prver var 31 (variavel binaria de grupo) a partir dos dados
#da série de tempo
#summary(classifier) #aqui a gente vê quantos dados realmente o modelo svm usou para classificar os dados (63 dos 800 para classe 0 e 65 dos 200 para classe 1)
par(mfrow=c(3,1))
y_fitted=as.vector(classifier$fitted)
#plot(y,col = ifelse(y_state == 1,'red','blue'),main = "Plot variável resposta y")
#lines(y)
#plot(y_state,col = ifelse(y_state == 1,'red','blue'),main = "Plot dos estados da variável resposta y")
#plot(y_fitted,col = ifelse(classifier$fitted == 1,'red','blue'),main = "Plot dos estados estimados da variável resposta y")
acerto_classificacao=sum(y_fitted==y_state[1:length(y)])/length(y)

matriz_acertos[n,2]=acerto_classificacao
}
return(matriz_acertos)
}



##############################################
##########RESULTADOS##########################
##############################################
#MCMC
resultados=mc(100)
#resultado markov
summary(resultados[,1])
sd(resultados[,1])
#resultado svm radial
summary(resultados[,2])
sd(resultados[,2])
