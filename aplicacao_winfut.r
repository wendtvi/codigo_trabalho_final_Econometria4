library(readxl)
library(e1071)
library(rpart)

##################################################
###############APLICAÇÃO##########################
##################################################
setwd("C:/Users/vitor/OneDrive/Área de Trabalho/Bases Contratos W/ARQ WIN/DATA")
mar2022=read_excel("C:/Users/vitor/OneDrive/Área de Trabalho/Bases Contratos W/ARQ WIN/DATA/MAR2022.xlsx") #dados de mini contrato futuro do bovespa (periodicidade 2 min)
fev2022=read_excel("C:/Users/vitor/OneDrive/Área de Trabalho/Bases Contratos W/ARQ WIN/DATA/FEB2022.xlsx")
jan2022=read_excel("C:/Users/vitor/OneDrive/Área de Trabalho/Bases Contratos W/ARQ WIN/DATA/JAN2022.xlsx")
dec2021=read_excel("C:/Users/vitor/OneDrive/Área de Trabalho/Bases Contratos W/ARQ WIN/DATA/DEC2021.xlsx")
nov2021=read_excel("C:/Users/vitor/OneDrive/Área de Trabalho/Bases Contratos W/ARQ WIN/DATA/NOV2021.xlsx")
out2021=read_excel("C:/Users/vitor/OneDrive/Área de Trabalho/Bases Contratos W/ARQ WIN/DATA/OUT2021.xlsx")

historico_data=c(out2021$Data,nov2021$Data,dec2021$Data,jan2022$Data,fev2022$Data)
historico_fechamento=c(out2021$Fechamento,nov2021$Fechamento,dec2021$Fechamento,jan2022$Fechamento,fev2022$Fechamento) #dados de fechamento
historico_minima=c(out2021$Mínima,nov2021$Mínima,dec2021$Mínima,jan2022$Mínima,fev2022$Mínima) #minima
historico_maxima=c(out2021$Máxima,nov2021$Máxima,dec2021$Máxima,jan2022$Máxima,fev2022$Máxima) #max
historico_abertura=c(out2021$Abertura,nov2021$Abertura,dec2021$Abertura,jan2022$Abertura,fev2022$Abertura) #abertura
historico_ma=c(out2021$`Média Móvel A [100]`,nov2021$`Média Móvel A [100]`,dec2021$`Média Móvel A [100]`,jan2022$`Média Móvel A [100]`,fev2022$`Média Móvel A [100]`)

Data=data.frame(historico_fechamento,historico_minima,historico_maxima,historico_abertura,historico_ma,historico_data)

Data$historico_data=format(Data$historico_data, "%d-%m-%Y %H:%M:%S")
Data=Data[order(as.Date(Data$historico_data,format = "%d-%m-%Y %H:%M:%S"),decreasing = F),]

Data$Retorno=c(0,Data$historico_fechamento[2:length(historico_fechamento)]-Data$historico_fechamento[1:(length(historico_fechamento)-1)]) #calcula vetor de retorno

plot(Data$historico_fechamento,type = 'l',main="Série de fechamento do WINFUT de out/21 até mar/22 (2 min)")
plot(Data$Retorno,type = 'l',main="Série de retorno do WINFUT de out/21 até mar/22 (2 min)")

#Calcula vetor de volatilidade do retorno por x candle de 2 min
x=30 #volatilidade em 1 hr se x=30
p=31
i=0
vetor_volatil=vector()
while (p <= length(Data$historico_fechamento)){
  i=i+1
  vetor_volatil[i]=sqrt(var(Data$Retorno[(p-30):(p-1)]))
  p=p+(30)
}
plot(vetor_volatil,col=ifelse(vetor_volatil<200, 'red', 'blue'))
lines(vetor_volatil)
vetor_volatil_final=vetor_volatil[vetor_volatil<200]
vetor_volatil_final_index=which(vetor_volatil<200)

plot(vetor_volatil_final)

#Calcula distancia entre fechamento da janela anterior e o max e min na janela de x candles de 2 min (se dist for maior pro max selecionala max, se nao min)
max_retorno_janela=vector()
p=31
c=0
max_retorno_janela_index=vector()
for (i in vetor_volatil_final_index){
  c=c+1
  c_index=vetor_volatil_final_index[i]
  max_retorno_janela[c]=max(Data$historico_abertura[c_index]-min(Data$historico_minima[c_index:(c_index+(p-1))]),
                            max(Data$historico_maxima[c_index:(c_index+(p-1))])-Data$historico_abertura[c_index])
  max_retorno_janela_index[c]=c_index
}
plot(max_retorno_janela,col=ifelse(max_retorno_janela>=600, 'red', 'blue'))
lines(max_retorno_janela)
vetor_mx_retorno_final_index=max_retorno_janela_index[max_retorno_janela>600]
par(mfrow=c(5,2))
for (i in 11:20){
  c_index=vetor_mx_retorno_final_index[i*10]
  plot(Data$historico_fechamento[(c_index-1):(c_index+30-1)],type='l', ylab="y")
}



#Indices dos periodos de interesse
indices=vector()
indices=rep(0,length(vetor_volatil))
indices=seq(0,length(vetor_volatil)-1)*30+1
indices_estado=vector()
indices_estado=rep(1,length(vetor_volatil))
indices_estado[max_retorno_janela<600]=2
indices_estado[vetor_volatil>=200]=2
indices_sucesso=indices[indices_estado==1]

estados_orig=vector()
estados_orig=rep(2,length(Data$historico_fechamento))
#2 é nao sucesso
estados_orig[indices_sucesso]=1
par(mfrow=c(3,1))
plot(estados_orig*100000,col=ifelse(estados_orig==1, 'red', 'white'),ylim=c(100000,120000))
lines(Data$historico_fechamento)


#Separando dados para teste
dataset_svm_train=cbind(Data$historico_ma,estados_orig)
dataset_svm_train=as.data.frame(dataset_svm_train)

#################################################
####################SVM##########################
#################################################
classifier = svm(y =  estados_orig,
                 x = dataset_svm_train$V1,
                 type = 'C-classification',
                 kernel = 'radial',gamma=0.1)
summary(classifier)
par(mfrow=c(3,1))
plot(classifier$fitted)
plot(as.numeric(classifier$fitted),main =  "Classificação dos dados da base de treinamento",col = ifelse(as.numeric(classifier$fitted) == 1,'red','blue'))
plot(classifier$decision.values,col = ifelse(as.numeric(classifier$fitted) == 1,'red','blue'),main = "Plot dos estados da variável resposta y")
sum(as.numeric(classifier$fitted)==dataset_svm_train$estados_orig)/length(dataset_svm_train$estados_orig)
sum(as.numeric(classifier$fitted)==estados_orig)/TT
sum(as.numeric(classifier$fitted)[estados_orig==1]==estados_orig[estados_orig==1])/TT

#################################################
####################MSM##########################
#################################################
TT=length(Data$Retorno)
y=Data$Retorno
y_state=estados_orig
n_states=2
#estimando parametros
#sabendo que existem 2 estados e sendo que var resposta para ambos vem de uma dist normal
#inicializando probabilidades de cada estado
epsolon_tt_hat=matrix(data = NA,ncol = n_states,nrow = TT)
epsolon_tt_hat[1,]=t(c(0.5,0.5))
ni=pnorm(y,)
L=rep(-Inf,n_states)
L1=vector()
L2=vector()
state_hat=vector()


#supondo que sei apenas a distribuição da var resposta y (Inferência ótima)
init_mu1=-1000
init_sd1=200
init_mu2=1000
init_sd2=200
pi1=sum(dnorm(y,mean = init_mu1,sd = init_sd1))/TT
pi2=sum(dnorm(y,mean = init_mu2,sd = init_sd2))/TT
init_state=1
n_max_test=100
teta_hat=matrix(NA,ncol = 3*n_states+1,nrow = n_max_test)
teta_hat[1,]=c(init_mu1,init_mu2,init_sd1,init_sd2,pi1,pi2,init_state)
epsolon_tt_hat1=vector()
epsolon_tt_hat2=vector()
L=matrix(NA,nrow = n_max_test,ncol = n_states)
L[1,]=c(0,0)


#########smoothed inference
for (k in 2:n_max_test){
  epsolon_tt_hat1=dnorm(y,mean = teta_hat[k-1,1],sd = teta_hat[k-1,3])
  epsolon_tt_hat2=dnorm(y,mean = teta_hat[k-1,2],sd = teta_hat[k-1,4])
  epsolon_tt_hat1=epsolon_tt_hat1/(epsolon_tt_hat1+epsolon_tt_hat2)
  epsolon_tt_hat2=epsolon_tt_hat2/(epsolon_tt_hat1+epsolon_tt_hat2)
  teta_hat[k,1]=sum(y*epsolon_tt_hat1)/sum(epsolon_tt_hat1)
  teta_hat[k,2]=sum(y*epsolon_tt_hat2)/sum(epsolon_tt_hat2)
  teta_hat[k,3]=sum(((y-teta_hat[k,1])^2)*epsolon_tt_hat1)/sum(epsolon_tt_hat1)
  teta_hat[k,4]=sum(((y-teta_hat[k,2])^2)*epsolon_tt_hat2)/sum(epsolon_tt_hat2)
  teta_hat[k,5]=sum(epsolon_tt_hat1)/sum(epsolon_tt_hat1+epsolon_tt_hat2)
  teta_hat[k,6]=sum(epsolon_tt_hat2)/sum(epsolon_tt_hat1+epsolon_tt_hat2)
  
  L[k,1]=-sum(log(teta_hat[k,5]/sqrt(2*pi*sqrt(teta_hat[k,3])))) + sum((y-teta_hat[k,1])^2)/(2*teta_hat[k,1]^2)
  L[k,2]=-sum(log(teta_hat[k,6]/sqrt(2*pi*sqrt(teta_hat[k,4])))) + sum((y-teta_hat[k,2])^2)/(2*teta_hat[k,2]^2)
}

p1_hat=dnorm(y,mean = -2.875247,sd = sqrt(0.00001))
p2_hat=dnorm(y,mean = 0.9999141738,sd = sqrt(0.00001))
epsolon_tt_hat=matrix(NA,ncol = n_states+1,nrow = TT)
epsolon_tt_hat[,1]=p1_hat
epsolon_tt_hat[,2]=p2_hat
epsolon_tt_hat[,3]=2
y_state_hat=vector(length = TT)
epsolon_tt_hat[epsolon_tt_hat[,1]>=epsolon_tt_hat[,2],3]=2
par(mfrow=c(3,1))
plot(y,main = "Plot variável resposta y")
lines(y)
plot(y_state,col = ifelse(y_state == 1,'red','blue'),main = "Plot dos estados da variável resposta y")
plot(epsolon_tt_hat[,3],col = ifelse(epsolon_tt_hat[,3] == 1,'red','blue'),main = "Plot dos estados estimados da variável resposta y")
acerto_classificacao_markov=sum(epsolon_tt_hat[,3]==y_state)/TT


count_11_hat=0
count_22_hat=0
for(l in 2:TT){
  if(epsolon_tt_hat[l,3]==epsolon_tt_hat[l-1,3] && epsolon_tt_hat[l,3]==1)count_11_hat=count_11_hat+1
  if(epsolon_tt_hat[l,3]==epsolon_tt_hat[l-1,3] && epsolon_tt_hat[l,3]==2)count_22_hat=count_22_hat+1
}
p11_hat=count_11_hat/sum(epsolon_tt_hat[,3]==1)
p22_hat=count_22_hat/sum(epsolon_tt_hat[,3]==2)
P_hat=matrix(data = c(p11_hat,1-p11_hat,1-p22_hat,p22_hat),ncol = 2)
