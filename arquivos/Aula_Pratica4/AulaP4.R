
#************************ Aplicação - Modelo de regressão Poisson e Binomial Negativa *******************#

# No arquivo tvcabo.txt os dados seguem a seguinte ordem: 

#Para cada área:

# Nass: numero de assinantes (em milhares) de TV a cabo
# Domic: numero de domicilios (em milhares) na área
# Percap: renda per capita (em USD) por domicílio com TV a cabo
# Taxa: taxa de instalacao de TV a cabo (em USD)
# Custo: custo medio mensal de manutencao de TV a cabo (em USD)
# Ncabo: numero de canais a cabo disponiveis na area
# Ntv: numero de canais nao pagos com sinal de boa qualidade disponiveis na area

# Vamos considerar um conjunto de dados sobre a demanda de TV a cabo em 40 áreas metropolitanas
# dos EUA (Ramanathan, 1993).

# O objetivo do estudo é explicar o número de assinantes (Nass) de TV a cabo dadas algumas variáveis 
# explicativas observadas em cada área.

TVCabo <- scan("tvcabo.txt", list(Nass=0, Domic=0, Percap=0,Taxa=0, Custo=0, Ncabo=0, Ntv=0))
attach(TVCabo)

# ** Ajuste via MLG Poisson **#

fit1 <- glm(Nass ~ Domic+Percap+Taxa+Custo+Ncabo+Ntv, family=poisson(link = "log"));summary(fit1)

#** Diagnostico **#
fit.model <- fit1
source("envel_pois.R")
source("diag_pois.R")

# Graficos de probabilidade normal dos resíduos com envelope simulado
glmtoolbox::envelope(fit1, rep=100, conf=0.95, type="quantile")
glmtoolbox::envelope(fit1, rep=100, conf=0.95, type="deviance", standardized=T)
glmtoolbox::envelope(fit1, rep=100, conf=0.95, type="pearson", standardized=T)

# O ajuste via Poisson está razoável? O que pode ser feito? Envelope tem indicativo
# de superdispersão, logo a poisson não é indicada. 

# ** Ajuste via Regressão Binomial Negativa **#

?glm.nb # função para ajustar regressão binomial negativa

fit2 <- MASS::glm.nb(Nass ~ Domic+Percap+Taxa+Custo+Ncabo+Ntv, link = "log");summary(fit2)

MASS::stepAIC(fit2) #selecao de covariaveis 

#O metodo automatizado sugere retirar o Percap
fit2 <- MASS::glm.nb(Nass ~ Domic+Taxa+Custo+Ncabo+Ntv, link = "log");summary(fit2)


Domic_c <- Domic- mean(Domic)
Taxa_c <- Taxa - mean(Taxa)
Custo_c <- Custo - mean(Custo)
Ncabo_c <- Ncabo - mean(Ncabo)
Ntv_c <- Ntv - mean(Ntv) 

#*** Diagnóstico ***#
require(MASS)
fit.model <- fit2
source("envel_nbin.R")
source("diag_nbin.R")

# Exercício: Existem pontos influentes? Baseado neste análise, escolha o modelo final.



#******************* AJUSTES VIA MLGS DUPLOS *******************#

#install.packages("dglm")
require(dglm) #pacote para ajuste de MLGs duplos

#install.packages("car")
require(car)


#********************* APLICACAO 1 *********************#

require(GLMsData)

# Um estudo com 654 jovens investigou as relações entre a capacidade pulmonar 
# (medida pelo volume expiratório forçado,
# ou fev, em litros) e o status de tabagismo (Smoke), idade, altura e gênero

data(lungcap); attach(lungcap)
?lungcap
head(lungcap)

Smoke <- as.factor(Smoke)

require(car)
scatterplot(FEV~Age, regLine = T, smooth=F, boxplots=F, pch=16, xlab="idade",
 ylab="capacidade pulmonar",cex=1.2,cex.lab=1.5, cex.axis=1.5)

scatterplot(FEV~Ht, regLine = T, smooth=F, boxplots=F, pch=16, xlab="altura",
 ylab="capacidade pulmonar",cex=1.2,cex.lab=1.5, cex.axis=1.5)

plot(Smoke, FEV)
plot(Gender, FEV)


# Ajuste via lm

fitN1 <- lm(FEV~Ht+Age+Smoke+Gender);summary(fitN1)
fit.model <- fitN1
source("envel_norm.R")
source("diag_norm.R")

#**** Ajuste com modelo normal linear heteroscedastico *****#
              #Para a media         , #Para dispersão
fitN2 <- dglm(FEV~Ht+Age+Smoke+Gender, ~Ht+Age+Smoke+Gender, family=gaussian(link = "identity"));summary(fitN2)
fit.model <- fitN2



source("envel_norm_dglm.R") # Avalia o ajuste do submodelo da media
source("envel_norm_dglm_disp.R") # Avalia o ajuste do submodelo da precisao

source("diag_norm_dglm.R")  # Diagnostico do ajuste do submodelo da media
source("diag_norm_dglm_disp.R")  # Diagnostico do ajuste do submodelo da precisao/dispersao


# Este modelo está bem ajustados aos dados? Caso não esteja, o que você propõe?

#Ajustar uma gama "simples" e uma gama dupla

#**** Ajuste com modelo gama que é naturalmente heteroscedastico *****#
fitG1 <- dglm(FEV~Ht+Age+Smoke+Gender, ~Ht+Age+Smoke+Gender, family=Gamma(link = "log"));summary(fitG1)
fit.model <- fitG1




#********************* APLICACAO 2 *********************#

#Os dados do arquivo UN11 contêm diversas variáveis, incluindo
# ppgdp: o produto nacional bruto por pessoa em dólares americanos,
# fertility: o número de filhos por mulher;
# lifeExpF: Expectativa de vida para mulheres em anos

# Os dados são para 199 localidades, principalmente países membros da ONU,
#mas também outras áreas, como Hong Kong, que não são países independentes.
#Referência - WEISBERG, S.. Applied Linear Regression. John Wiley & Sons, Inc., 2014.


#install.packages("alr4"); 
#?UN11

require(alr4); data(UN11); attach(UN11)

scatterplot(fertility~log(ppgdp), regLine = T, smooth=F, boxplots=F, pch=16, xlab="produto nacional bruto por pessoa",
 ylab="taxa de fertilidade",cex=1.2,cex.lab=1.5, cex.axis=1.5)

scatterplot(fertility~lifeExpF, regLine = T, smooth=F, boxplots=F, pch=16, xlab="expectativa de vida feminina",
 ylab="Taxa de fertilidade",cex=1.2,cex.lab=1.5, cex.axis=1.5)


# Ajuste via lm

fitN1 <- lm(fertility~lifeExpF);summary(fitN1)
fit.model <- fitN1
source("envel_norm.R")
source("diag_norm.R")

#**** Ajuste com modelo normal linear heteroscedastico *****#

fitN2 <- dglm(fertility~lifeExpF, ~lifeExpF, family=gaussian(link = "identity"));summary(fitN2)
fit.model <- fitN2

source("envel_norm_dglm.R") # Avalia o ajuste do submodelo da media
source("envel_norm_dglm_disp.R") # Avalia o ajuste do submodelo da precisao
# Pontos das pontas que saem pela mesma direção é um indicativo de assimetria 


source("diag_norm_dglm.R")  # Diagnostico do ajuste do submodelo da media
source("diag_norm_dglm_disp.R")  # Diagnostico do ajuste do submodelo da precisao/dispersao

# Exercício: Ajuste MLGs duplos gama e Normal inversa. 


#********************* APLICACAO 3 *********************#


# Para uma amostra de 64 ovelhas merino castradas (machos) em pastagem,
# os requisitos energéticos diários e o peso foram registrados.

require(GLMsData)
data(sheep); attach(sheep)
?sheep

scatterplot(Energy~Weight, regLine = T, smooth=F, boxplots=F, pch=16, ylab="energia diária necessária",
 xlab="peso da ovelha",cex=1.2,cex.lab=1.5, cex.axis=1.5)
identify(Weight, Energy) #ovelha 11


# ----->  O objetivo é explicar a energia diária necessária em função do peso

#**** Ajuste inicial com modelo normal linear homoscedastico *****#

# Ajuste via lm

fitN1 <- lm(Energy ~ Weight, family=gaussian(link = "identity"));summary(fitN1)
fit.model <- fitN1
source("envel_norm.R")
source("diag_norm.R")


#**** Ajuste com modelo normal linear heteroscedastico *****#

fitN2 <- dglm(Energy ~ Weight, ~Weight, family=gaussian(link = "identity"));summary(fitN2)
fit.model <- fitN2

# Graficos de envelope baseado no residuo desvio
source("envel_norm_dglm.R") # Avalia o ajuste do submodelo da media
source("envel_norm_dglm_disp.R") # Avalia o ajuste do submodelo da precisao

#Graficos de diagnostico
source("diag_norm_dglm.R")  # Diagnostico do ajuste do submodelo da media
source("diag_norm_dglm_disp.R")  # Diagnostico do ajuste do submodelo da precisao/dispersao


# O ajuste via MLG duplo normal produz um melhor ajuste aos dados. 
# Entretanto, ainda não é um ajuste ideal. 


#*********** Ajuste via MLG gama - Modelando apenas media *************#

fitG1 <- glm(Energy ~ Weight, family=Gamma(link = "identity"))
summary(fitG1)
fit.model <- fitG1
source("envel_gama_lig_id.R") 
source("diag_gama.R")

# O ajuste com um MLG gama com ligação identidade produz um ajuste muito parecido
# ao ajuste duplo normal.















