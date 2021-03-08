#Limpar
rm(list=ls())


#Pacotes
library(data.table)
library(dplyr)
library(tidyr)
library(xtable)
library(stargazer)
library(ggplot2)


#Diretório
setwd("C:/Users/Caio Azevedo/Documents/Documentos Caio/Github/Dissertacao/data")


#Carregamento e configuração dos dados----

base <- read.table("base_window_008.csv", header = TRUE,sep = ",",dec=".",
                   fileEncoding = "latin1")

base<-base %>% 
  mutate("home_match"=ifelse(condicao=="mandante",1,0)) %>% 
  select(-di,-pts_4) %>% 
  mutate("m"=as.numeric(m)) 

base<-base[complete.cases(base),]



#Selecionando as observações da janela----


base<-base %>% 
  filter(periodo==1)

k<-8                      #definir o tamanho da janela

x<-c(seq(k,nrow(base),k))
temporada<-matrix(nrow=length(x))
time<-matrix(nrow=length(x))
rodada<-matrix(nrow=length(x))
cum_surpresa<-matrix(nrow = length(x))
tratado<-matrix(nrow = length(x))
m<-matrix(nrow = length(x))



for(i in x){
  temporada[[i]]<-c(base$temporada[i])
}

for(i in x){
  time[[i]]<-c(base$time[i])
}

for(i in x){
  rodada[[i]]<-c(base$rodada[i])
}

for(i in x){
  cum_surpresa[[i]]<-c(base$cum_surpresa[i])
}

for(i in x){
  tratado[[i]]<-c(base$tratado[i])
}

for(i in x){
  m[[i]]<-c(base$m[i])
}


temporada<-temporada[complete.cases(temporada)]
time<-time[complete.cases(time)]
rodada<-rodada[complete.cases(rodada)]
cum_surpresa<-cum_surpresa[complete.cases(cum_surpresa)]
tratado<-tratado[complete.cases(tratado)]
m<-m[complete.cases(m)]

dados<-cbind.data.frame(temporada,time,rodada, cum_surpresa,tratado,m)


dados_controle<-dados %>% 
  filter(tratado==0)

dados_tratamento<-dados %>% 
  filter(tratado==1)

dados<-left_join(dados_tratamento,dados_controle,c("m","time"))

#Gráfico

library(ggplot2)

cleanup = theme(panel.grid.major = element_blank(),
                panel.grid.minor = element_line(color = "white"),
                panel.background = element_blank(),
                legend.position = "bottom",
                axis.line = element_line(color = "black"),
                axis.title = element_text(size = 18),
                legend.text = element_text(size = 14),
                axis.text = element_text(size = 14))

attach(dados)

num<-cbind(cum_surpresa.y,cum_surpresa.x)

setwd("C:/Users/Caio Azevedo/Documents/Documentos Caio/Github/Dissertacao/Figuras")


postscript("caso.eps", horizontal = FALSE, onefile = FALSE, 
           paper = "special", height = 2,75591, width = 3,93701)
plot(num, xlim = c(-15,15), ylim=c(-15,15), 
     col = ifelse(num %in% num[c(18,29)], 'red', 'black'), 
     pch = ifelse(num %in% num[29], 17, ifelse(num %in% num[18], 15, 19)),
     cex.lab=1.4, cex.axis=1.4, cex=1.5,
     xlab = "Surpresa acumulada - Grupo de Controle " ,
     ylab = "Surpresa acumulada - Grupo de Tratamento") + abline(coef = c(0,1))   
            



#dev.copy(pdf,"caso.pdf")
dev.off()
