#************************ Aplicação - Modelo de regressão Poisson *******************#

# Os dados a seguir são de um estudo prospectivo com 100 indivíduos de pelo menos 65 anos de idade em boas condições físicas.
# O objetivo do estudo é tentar relacionar o número médio de quedas num período de seis meses com algumas variáveis explicativas.
# Os dados estão descritos na seguinte ordem:  quedas (número de quedas no período), intervenção (=0 educação somente, =1 educação
# e exercícios físicos), sexo (=0, feminino, =1 masculino), balanço (escore) e força (escore). Para as variáveis 
# balanço e força quanto maior o valor maior o balanço e a força do indivíduo, respectivamente.  

# Faça uma análise descritiva dos dados através de gráficos e medidas-resumo. Usando todas as covariáveis, ajuste um modelo de
# regressão Poisson. Interprete o modelo ajustado e faça uma análise de diagnóstico. 

# Verifique se existem afastamentos sérios das suposições do modelo e se existem pontos atípicos que influenciam no ajuste.
# Em seguida, ajuste um novo modelo Poisson levando em conta a seleção de covariáveis via o método do AIC (use a função stepAIC) 
# e considerações feitas na análise de diagnóstico. Faça a análise de diagnóstico do modelo final e apresente todas as interpretações
# relevantes.


geriatra <- scan("geriatra.txt", list(quedas=0, intervencao=0, sexo=0, balanco=0, forca=0))
attach(geriatra)

# Informe previamente as variáveis que são fatores:
sexo <- as.factor(sexo); intervencao <- as.factor(intervencao)


plot(sexo,quedas, xlab="sexo", ylab="número de quedas", main="Boxplot do número de quedas segundo sexo")
plot(balanco,quedas, pch=16, ylab="número de quedas", xlab="balanço")
plot(forca,quedas, pch=16, ylab="número de quedas", xlab="força")
#identify(forca,quedas)
boxplot(quedas~intervencao, xlab="intervenção", ylab="número de quedas", main="Boxplot do número de quedas segundo intervenção")


# ** Ajuste via MLG Poisson **#

fit <- glm(quedas ~ sexo+intervencao+balanco+forca,
 family=poisson(link = "log"));summary(fit)

MASS::stepAIC(fit)

fit1 <- glm(quedas ~ intervencao+balanco+forca,
 family=poisson(link = "log"));summary(fit1)


#** Diagnostico **#
fit.model <- fit1
source("envel_pois.R")
source("diag_pois.R")
par(mfrow=c(1,1))
glmtoolbox::envelope(fit1, rep=100, conf=0.95, type="quantile") #repita
glmtoolbox::envelope(fit1, rep=100, conf=0.95, type="deviance", standardized=T)
glmtoolbox::envelope(fit1, rep=100, conf=0.95, type="pearson", standardized=T)

# O ajuste via Poisson está razoável? 

# Existem pontos influentes no ajuste deste MLG Poisson?

# Criando os graficos individualmente:

X <- model.matrix(fit.model)
n <- nrow(X)
p <- ncol(X)
w <- fit.model$weights
W <- diag(w)
H <- solve(t(X)%*%W%*%X)
H <- sqrt(W)%*%X%*%H%*%t(X)%*%sqrt(W)
h <- diag(H)
ts <- resid(fit.model,type="pearson")/sqrt(1-h)
td <- resid(fit.model,type="deviance")/sqrt(1-h)
di <- (h/(1-h))*(ts^2)

a <- min(td)
b <- max(td)

# Grafico das medidas de alavancagem versus os valores ajustados

plot(fitted(fit.model), h,xlab="Valor Ajustado", ylab="Medida h",
pch=16)
#identify(fitted(fit.model), h, n=1) #Obs. 62

# Grafico das medidas de influência de Cook versus os indices

plot(di,xlab="Índice", ylab="Distância de Cook", pch=16)
#identify(di, n=3) #Obs. 52, 37, 42

# Grafico dos resíduos desvio versus os indices

plot(td,xlab="Índice", ylab="Resíduo Componente do Desvio",
ylim=c(a-1,b+1), pch=16)
abline(2,0,lty=2)
abline(-2,0,lty=2)
#identify(td, n=1) 

# Grafico dos resíduos desvio versus os valores ajustados

plot(fitted(fit.model), td,xlab="Valor Ajustado", ylab="Resíduo Componente do Desvio",
ylim=c(a-1,b+1), pch=16)
abline(2,0,lty=2)
abline(-2,0,lty=2)
#identify(td, n=1) 
#

# Refazendo os graficos usando o resíduo quantilico

resq <- statmod::qresid(fit.model)
a <- max(resq)
b <- min(resq)
par(mfrow=c(1,2))

plot(resq,xlab="Índice", ylab="Resíduo Quantílico",ylim=c(b-1,a+1), pch=16)
abline(2,0,lty=2)
abline(-2,0,lty=2)
#identify(resq, n=1)
#
plot(fitted(fit.model),resq,xlab="Valor Ajustado", 
ylab="Resíduo Quantílico", ylim=c(b-1,a+1), pch=16)
abline(2,0,lty=2)
abline(-2,0,lty=2)
#identify(fitted(fit1),resq, n=2)


# Grafico da resposta transformada z versus o preditor linear

w <- fit.model$weights
eta <- predict(fit.model)
z <- eta + resid(fit.model, type="pearson")/sqrt(w)
plot(predict(fit.model),z,xlab="Preditor Linear", 
ylab="Variavel z", pch=16)
lines(smooth.spline(predict(fit.model), z, df=2))
#identify(predict(fit.model),z, n=2) #Obs. 42 e 37

# Existem pontos influentes no ajuste deste MLG Poisson?

# Os pontos identificados foram 52, 42, 37

# Mensurar a mudança relativas nos coeficientes estimados:

#********************* Medidas de influência *********************#

# Calculo da mudança relativa
MR <- function(a,b) cat("Mudança relativa = ", round(as.numeric((b/a-1)*100),2), "%","\n")

#exemplos:
MR(10,5)
MR(2,6)

#Pontos destacados sob fit1: 52, 42, 37

fit1 <- glm(quedas ~ intervencao+balanco+forca,
 family=poisson(link = "log"));summary(fit1)

fit1_w52 <- glm(quedas ~ intervencao+balanco+forca,
 family=poisson(link = "log"), subset=-c(52));summary(fit1_w52)

fit1_w42 <- glm(quedas ~ intervencao+balanco+forca,
 family=poisson(link = "log"), subset=-c(42));summary(fit1_w42)

fit1_w37 <- glm(quedas ~ intervencao+balanco+forca,
 family=poisson(link = "log"), subset=-c(37));summary(fit1_w37)

fit1_w <- glm(quedas ~ intervencao+balanco+forca,
 family=poisson(link = "log"), subset=-c(52,42,37));summary(fit1_w)

#***mudança nas estimativas***#
MR(coef(fit1), coef(fit1_w52)) # ao retirar obs. 52
MR(coef(fit1), coef(fit1_w42)) # ao retirar obs. 42
MR(coef(fit1), coef(fit1_w37)) # ao retirar obs. 37
MR(coef(fit1), coef(fit1_w)) # ao retirar todas


# Existem pontos influentes sob o ajuste Poisson com todos os dados (fit1)?



#************************ Aplicação - Modelo de regressão linear normal *******************#


# Dados de um estudo sobre expectativa de vida em 1969-70
#de 50 estados norte-americanos. Os dados estao disponibilizados em
#https://www.ime.usp.br/˜giapaula/reg3.txt

ev <- scan("reg3.txt", list(estado="  ", pop=0, percap=0, analf=0, expvida=0, crime=0, estud=0, ndias=0, area=0))
attach(ev)
dens <- pop/area # definindo nova covariável

# Ajuste final com modelo normal linear que tinhamos escolhido:

fit3_N <- lm(expvida ~ crime+estud+ndias)
summary(fit3_N)

# Realize a análise de diagnóstico e com base nesta, escolha o modelo final.
fit.model <- fit3_N
source("envel_norm.R")
source("diag_norm.R")

#------------------------------------------------------------#
X <- model.matrix(fit.model)
n <- nrow(X)
p <- ncol(X)
H <- X%*%solve(t(X)%*%X)%*%t(X)
h <- diag(H)
lms <- summary(fit.model)
s <- lms$sigma
r <- resid(lms)
ts <- r/(s*sqrt(1-h))
di <- (1/p)*(h/(1-h))*(ts^2)
si <- lm.influence(fit.model)$sigma
tsi <- r/(si*sqrt(1-h))
a <- max(tsi)
b <- min(tsi)
par(mfrow=c(2,2))
plot(h,xlab="Índice", ylab="Medida h", pch=16, ylim=c(0,1))
cut <- 2*p/n
abline(cut,0,lty=2)
#identify(h, n=1)
#title(sub="(a)")
#
plot(di,xlab="Índice", ylab="Distância de Cook", pch=16)
#identify(di, n=2)
#
plot(tsi,xlab="Índice", ylab="Resíduo Padronizado",
ylim=c(b-1,a+1), pch=16)
abline(2,0,lty=2)
abline(-2,0,lty=2)
#identify(tsi, n=1)
#
plot(fitted(fit.model),tsi,xlab="Valor Ajustado", 
ylab="Resíduo Padronizado", ylim=c(b-1,a+1), pch=16)
abline(2,0,lty=2)
abline(-2,0,lty=2)
#identify(fitted(fit.model),tsi, n=1)
par(mfrow=c(1,1))


# Avaliando efeito da obs. 11

fit3_Nw11 <- lm(expvida ~ crime+estud+ndias, subset=-c(11));summary(fit3_Nw11)


#***mudança nas estimativas***#
MR(coef(fit3_N), coef(fit3_Nw11)) # ao retirar obs. 11


#************************ Aplicação - Modelo de regressão normal, gama e normal inverso *******************#

# Para uma amostra de 64 ovelhas merino castradas (machos) em pastagem,
# os requisitos energéticos diários e o peso foram registrados.

#install.packages("GLMsData")
require(GLMsData)
data(sheep); attach(sheep)
?sheep
plot((Energy)~(Weight), pch=16, ylab="energia diária necessária", xlab="peso da ovelha",cex=1.2,cex.lab=1.5, 
cex.axis=1.5)
identify(Weight, Energy) #ovelha 11

# ----->  O objetivo é explicar a energia diária necessária em função do peso

#**** Ajuste inicial com modelo normal linear homoscedastico *****#


# Ajuste via lm

fitN1 <- lm((Energy) ~ Weight, family=gaussian(link = "identity"));summary(fitN1)
fit.model <- fitN1
source("envel_norm.R")
source("diag_norm.R")

# EXERCÍCIO: Encontre o melhor ajuste possível via MLGs.
#tentar ligação, tentar outras distribuições

fitNI <- glm((Energy) ~ Weight, family=inverse.gaussian(link = "log"));summary(fitNI)
fit.model <- fitNI
source("envel_ninv.R")
source("diag_ninv.R")


#PROFESSORA FEZ A GAMA EM SALA COM FUNÇÃO DE LIGAÇÃO LOG















