# DEFININDO O PROJETO
# --------------------------------
# Pergunta: O que afeta a qualidade do ar? E como isso afeta?
# --------------------------------

#install.packages("Ecdat") # se necessário
library(Ecdat) # carregando
data(Airq) # carregando o banco de dados do pacote
names(Airq) # exibe os nomes das variáveis


# --------------------------------
# DESCREVENDO AS VARIÁVEIS
# --------------------------------
# airq: indice de qualidade do ar (quanto menor, melhor)
# vala: valor das empresas nas cidades (em milhares de dólares)
# rain: quantidade de chuva (em polegadas)
# coas: posição costeira da cidade (sim ou não)
# dens: densidade populacional (em milha quadrada)
# medi: renda média per capita (em dólares)

# --------------------------------
# ANÁLISE DESCRITIVA OU EXPLORATÓRIA DOS DADOS
# --------------------------------
summary(Airq) # sumário das variáveis

# as variáveis podem ser contínuas ou categóricas
# a variável resposta é a qualidade do ar (airq) - quanto menor, melhor a qualidade do ar

plot(airq~vala, data=Airq) 
#ao plotar um gráfico, a primeira variável será a variável resposta
#após a variável resposta indicar ~(variável a ser analisada)
#indicar o banco de dados que será utilizado a partir de data=xxx

# --------------------------------
# CRIANDO UM MODELO ESTATÍSTICO
# --------------------------------
# y (resposta) ~ x (explicativa)
# y ~ x1 + x2 + x3...
# ex.: airq ~ vala + coas + rain
# --------------------------------

# --------------------------------
# MONTANDO O MODELO
# --------------------------------
# Primeiro deve-se inserir o nome do modelo (no exemplo abaixo, m1)
# LM - comando de modelo linear
# variável resposta em função da variável explicativa (airq~vala)
# p-value indica a significância do modelo ou da variável
# se o p-value for menor do que 0.05 a variávelé significativa, ou seja, tem efeito
# se o p-value for maior do que 0.05 não existe o efeito esperado
# summary do modelo - mostra informações sobre a análise estatística
# plot de regressão linear - plot(y~x, data=z)
# alguns dados podem não ser lineares


# --------------------------------
# PRIMEIRO MODELO
# --------------------------------
# O valor das empresas da região afeta a qualidade do ar?
# A variável "vala" afeta a variável "airq"?
m1<-lm(airq~vala, data=Airq)
summary(m1) # para saber a significância do modelo
plot(airq~vala, data=Airq) # plot de regressão linear
# Não.A variável "vala" não influenciou a qualidade do ar nas cidades ("airq")


# --------------------------------
# SEGUNDO MODELO
# --------------------------------
# A posição costeira afeta a qualidade do ar?
m2<-lm(airq~coas, data=Airq)
summary(m2)
# Sim, A posição costeira da cidade influencia a qualidade do ar das cidades
plot(airq~coas, data=Airq)
# As cidades costeiras apresentam uma melhor qualidade do ar.


# --------------------------------
# TERCEIRO MODELO
# --------------------------------
# A renda média per capita afeta a qualidade do ar?
m3<-lm(airq~medi, data=Airq)
summary(m3)
plot(airq~medi, data=Airq)
# A variável de renda per capita não afetou a qualidade do ar


# --------------------------------
# QUARTO MODELO
# --------------------------------
# A quantidade de chuva afeta a qualidade do ar?
m4<-lm(airq~rain, data=Airq)
summary(m4)
# A quantidade de chuva não afeta a qualidade do ar


# --------------------------------
# QUINTO MODELO
# --------------------------------
# A densidade populacional afeta a qualidade do ar?
m5<-lm(airq~dens, data=Airq)
summary(m5)
plot(airq~dens, data=Airq)
# A densidade populacional não afeta a qualidade do ar

# A única variável que explica a qualidade do ar nas cidades é a posição costeira

# --------------------------------
# --------------------------------
# termos: anova(variável contínua ~ de uma variável categórica)
# regressão (variável contínua ~ variável contínua)
# regressão múltipla (variável contínua ~ variáveis contínuas ou não)
# --------------------------------
# --------------------------------


# --------------------------------
# RETAS NOS GRÁFICOS
# COLOCANDO UMA RETA NO GRÁFICO DE REGRESSÃO
# --------------------------------
# Retas de modelos não significativos são opcionais nos gráficos
# Equação da reta: y=a+b*x
# a = intercepto (onde a reta vai tocar no eixo y)
# b = inclinação reta
# x = variável explicativa
# modelos lineares - a inclinação (curve) será uma reta
# modelos não lineares - a inclinação(curve) será uma curva
plot(airq~medi, data=Airq)
curve(9.936e+01+5.638e-04*x, add=TRUE) #True - plota o gráfico no mesmo arquivo


# --------------------------------
# MELHORANDO O GRÁFICO
# --------------------------------
# pch=1 - pontos brancos no gráfico
# pch=16 - pontos pretos no gráfico
# col="blue" - deixa a cor azul
# cex.lab - mudar o tamanho do eixo
# lwd - large width - muda a espessura da inclinação
# lty - muda o tipo de linha
# main - título do gráfico
# ylim - y limit - muda a escala do eixo
# c - concatenate
plot(airq~medi,data=Airq, xlab="Renda média per capita", ylab="Qualidade do ar", 
     pch=1,col="blue", cex.lab=1.3, main="Renda Média - 2010")
curve(9.936e+01+5.638e-04*x, add=TRUE, col="darkblue", lwd=2,lty=2)


# --------------------------------
# Modelo m1
# --------------------------------
plot(airq~vala, data=Airq, xlab="Valor das empresas ($)", ylab="Qualidade do ar",
     pch=16, col="blue", cex=1.5)
curve(96.451419+0.001969*x, add=TRUE, col="darkblue", lwd=2,lty=2)


# --------------------------------
# Modelo m2
# --------------------------------
plot(airq~coas, data=Airq, xlab="Posição costeira", ylab="Qualidade do ar",
     pch=16, col="lightblue", cex=1.5, ylim=c(50,170), cex.lab=1.3,
     main="Análise da qualidade do ar")


# --------------------------------
# REGRESSÃO MÚLTIPLA
# --------------------------------
# Uma variável pode exercer controle sobre outra variável

mRM1<-lm(airq~vala+coas, data=Airq)
summary(mRM1)
# Então existe um efeito da posição costeira e do valor das empresas na qualidade do ar


# --------------------------------
#GRÁFICO REGRESSÃO MÚLTIPLA
# --------------------------------
#legend - bottomright, topright
#bty - tira a box da legenda

plot(airq~vala, data=Airq, xlab="Valor das empresas ($)",
     ylab="Qualidade do ar")
curve(1.171e+02+1.999e-03*x,add=TRUE) # cidade não costeira
curve(1.171e+02+1.999e-03*x+-2.968e+01, lty=2, add=TRUE) # cidade costeira
legend("bottomright", c("Não costeiras", "Costeiras"), pch=16, 
       lty=c(1,2), bty="n")
# A qualidade do ar das cidades é afetada tanto pelo valor das empresas quanto pela posição costeira das cidades.
# Quanto maior o valor das empresas, pior a qualidade do ar das cidades.
# Além disso, as cidades não-costeiras apresentam qualidade do ar pior do que as cidades costeiras.

mRM2<-lm(airq~ vala + coas + dens, data=Airq)
summary(mRM2)
# O valor das empresas e a posição costeira continua afetando a qualidade do ar
# Mas a densidade populacional não afeta a qualidade do ar mesmo combinada com outras variáveis


# --------------------------------
# CONTRASTES DE MODELOS
# --------------------------------
# Comparar um modelo completo com um modelo sem a variável em questão
modelocompleto<-lm(airq~vala+coas+dens, data=Airq)
modeloincompleto<-lm(airq~vala+coas, data=Airq)
# Os modelos são iguais?
# Se p>0.05 não existe diferença entre os modelos. Neste caso, fica-se com o modelo mais simples
# Se p<0.05 os modelos são diferentes e a variável não deve ser retirada do modelo
anova(modelocompleto, modeloincompleto)
# Não existe diferença entre os modelos
# A variável densidade não tem significância no modelo e pode ser retirada.
