
# Pacotes 
require(MASS)
require(glmtoolbox)#pacote para gerar graficos de envelope


#*********************** APLICACAO - DADOS EXPECTATIVA DE VIDA ****************#


#**************** Ajuste via MLGs normal e gama *******************#

#Dados de um estudo sobre expectativa de vida em 1969-70
#de 50 estados norte-americanos. Os dados estao disponibilizados em
#https://www.ime.usp.br/˜giapaula/reg3.txt

ev <- scan("reg3.txt", list(estado="  ", pop=0, percap=0, analf=0, expvida=0, crime=0, estud=0, ndias=0, area=0))
attach(ev)
dens <- pop/area # definindo nova covariável

# Ajuste final com modelo normal linear que tinhamos escolhido:

fit_N <- lm(expvida ~ crime+estud+ndias);summary(fit_N)

#*************Diagnostico************#

fit.model <- fit_N

#Via scripts Prof. Gilberto A. Paula - Site: https://www.ime.usp.br/~giapaula/textoregressao.htm
source("envel_norm.R")
source("diag_norm.R") 


# Usando o pacote glmtoolbox

envelope(fit_N, rep=100, conf=0.95, type ="external") #resíduo studentizado externo
# Mesmo grafico usando o residuo studentizado com fit3_N obtido via função lm


#  Ajustando via glm

fit_N2 <- glm(expvida ~ crime+estud+ndias); summary(fit_N2) 
# adapta para pegar o sigma2 do mínimos quadrados ao invés da maxima verossimilhança
# Null Deviance é o desvio nulo só com intercpeto
# Residual Deviance é desvio com todos os parametros 


# Cálculo de pseudo R2

# Desvio não escalonado:
fit_N2$deviance 

# R^2 baseado na funcao desvio: R2 explicado no slide 32 da unidade II, na normal ele vai coincidir com o R2 da normal 

R2 <- 1 - fit_N2$deviance/fit_N2$null; R2

# R^2 baseado na correlação linear entre preditor linear ajustado e g(y)
#Slide 32 da Unidade II 
R_2p <- cor(fit_N2$linear.predictors, expvida)^2; R_2p

# Neste casos, os dois pseudo R2 coincidiram. Pq?

# R^2 = SQREG/SQT = Cor(y, y^)^2, como g(mui) = etai -> g(yi) = etai^
#Por causa que o preditor linear é a identidade (??)
# Nos outros vai ficar próximo porque não vai ser a entidade


# qqplots dos residuos com envelope:

envelope(fit_N2, rep=100, conf=0.95, type="quantile")

#Deviance tem distribuição mais próxima de 1 (?)
envelope(fit_N2, rep=100, conf=0.95, type="deviance", standardized=T)


envelope(fit_N2, rep=100, conf=0.95, type="pearson", standardized	=T)

#********************* Graficos de diagnostico separadamente usando diag_norm.R ******************#

fit.model <- lm(expvida ~ crime+estud+ndias)

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

#********* Identificação de pontos de alavanca *****#
plot(h,xlab="Índice", ylab="Medida h", pch=16, ylim=c(0,1))
cut <- 2*p/n
abline(cut,0,lty=2)
#identify(h, n=1)

#********* Identificação de pontos influentes *****#
plot(di,xlab="Índice", ylab="Distância de Cook", pch=16)
#identify(di, n=2)


#********* Verificar adequação de independência das respostas*****#
plot(tsi,xlab="Índice", ylab="Resíduo Padronizado",ylim=c(b-1,a+1), pch=16)
abline(2,0,lty=2)
abline(-2,0,lty=2)
#identify(tsi, n=1)#obs. 19

#*************Verificar adequação da homoscedasticidade****#
plot(fitted(fit.model),tsi,xlab="Valor Ajustado", 
ylab="Resíduo Padronizado", ylim=c(b-1,a+1), pch=16)
abline(2,0,lty=2)
abline(-2,0,lty=2)
#identify(fitted(fit.model),tsi, n=1) # Obs. 19
par(mfrow=c(1,1))


#------->  Ajustando via glm gama

fit_G <- glm(expvida ~ crime+estud+ndias, family =Gamma(link = "log")); summary(fit_G) 


# Cálculo de pseudo R2

# Desvio não escalonado:
fit_G$deviance 

# R^2 baseado na funcao desvio:

R2 <- 1 - fit_G$deviance/fit_G$null; R2

# R^2 baseado na correlação linear entre preditor linear ajustado e g(y)

R_2p <- cor(fit_G$linear.predictors, log(expvida))^2; R_2p

# Neste casos, apesar de proximos, os dois pseudo R2 não coincidiram. 

# qqplots dos residuos com envelope:

#Quando a regressão for discretar rodar o quantile mais de uma vez e tomar uma decisão
# depois de avaliar pelo menos uns 10, se indicarem pro mesmo resultado então, decidir
envelope(fit_G, rep=100, conf=0.95, type="quantile", identify = 2)
envelope(fit_G, rep=100, conf=0.95, type="deviance", standardized=T)
envelope(fit_G, rep=100, conf=0.95, type="pearson", standardized	=T)


#Via scripts Prof. Gilberto A. Paula - Site: https://www.ime.usp.br/~giapaula/textoregressao.htm

fit.model <- fit_G

source("envel_gama.R")
source("diag_gama.R") 
# aparece a variável Z que é o que tá descrito na unidade 1 slide (??)
# espera-se que a variável z x pelo preditor linear seja próximo de uma reta


# Testes de hipoteses para o caso que phi é desconhecido:

# Comparacao entre modelos encaixados:

fit0 <- glm(expvida ~ 1, family =Gamma(link = "log")); summary(fit0)

fit1 <- glm(expvida ~ crime+estud+ndias, family =Gamma(link = "log")); summary(fit1)

#Tabela de análise de variância:

anova(fit0, fit1, test="F")
#O p-valor é assintótico, então ele só vale se o modelo COMPLETO estiver bem ajustado 

# Calculando a estatistica: Estatística F do slide 37 unidade II 

n <- length(expvida); p <- length(coef(fit1)); q <- p - 1

#Calculando a estatistica como vimos em aula:

F0 <- ((fit0$deviance - fit1$deviance)/q)/(fit1$deviance/(n-p)); F0

# Note que este valor não coincide exatamente com o valor da tabela.
# Isto ocorre pq usamos a forma do estimador de máxima verossimilhança para
# normal e normal inversa; o estimador de máxima verossimilhança no caso 
# gama não possui forma fechada.

# A estimativa de momentos para phi (precisao) é (formula da unidade I, slide 95)

phiM <- (n-p)/(sum((expvida-fitted(fit1))^2/(fitted(fit1)^2))); phiM

1/phiM # estimativa da dispersao 

#Note que a estimativa acima coincide com a saída de summary(fit1)

# Portanto, usando a estimativa de momentos (que é consistente) para phi no denominador da estatística F:

F0 <- ((fit0$deviance - fit1$deviance)/q)/(1/phiM); F0
# Tem que adptar a formula (PRESTAR ATENÇÃO E ESTUDAR ISSO MELHOR)

# Agora a estatística coincide com a saída da função anova

#Calculo do p-valor assintótico:

pf(F0, df1=q, df2=n-p, lower.tail=F)
#Tudo aqui é assintótico, tudo é apróximado então tomar cuidado com as decisões

#Para o caso das distribuições normal e normal inversa, essa diferença na estatística F
# não deve ocorrer.

# Para o caso com phi conhecido (Poisson e binomial), deve-se usar o TRV que possui
# distribuição qui-quadrado assintoticamente.

# Para usar o teste TRV use anova(fit0, fit1, test="LRT").
# Amostra aqui é considerada pequena!!

#---------------->Fazer mesmo procedimento com ajuste normal inversa.


#*********************** APLICACAO - DADOS FERTILIDADE ****************#

# Os dados do arquivo UN11 contêm diversas variáveis, incluindo
# ppgdp: o produto nacional bruto por pessoa em dólares americanos,
# fertility: o número de filhos por mulher;
# lifeExpF: Expectativa de vida para mulheres em anos

# Os dados são para 199 localidades, principalmente países membros da ONU,
# mas também outras áreas, como Hong Kong, que não são países independentes.
# Referência - WEISBERG, S.. Applied Linear Regression. John Wiley & Sons, Inc., 2014.


#install.packages("alr4"); 
#?UN11

require(alr4); data(UN11); attach(UN11)

#NÃO TRANSFORMA RESPOSTA, SÓ A COVARIÁVEL
scatterplot((fertility)~log(ppgdp), regLine = T, smooth=F, boxplots=F, pch=16, xlab="produto nacional bruto por pessoa",
 ylab="taxa de fertilidade",cex=1.2,cex.lab=1.5, cex.axis=1.5)

scatterplot(fertility~lifeExpF, regLine = T, smooth=F, boxplots=F, pch=16, xlab="expectativa de vida feminina",
 ylab="Taxa de fertilidade",cex=1.2,cex.lab=1.5, cex.axis=1.5)


# Ajuste MLGs para este dados e encontre o melhor ajuste com base nos resíduos.

# Usando regressao gama:

fit1 <- glm(fertility ~ log(ppgdp)+lifeExpF, family =Gamma(link = "log")); summary(fit1)
fit2 <- glm(fertility ~ log(ppgdp)+lifeExpF, family =Gamma(link = "inverse")); summary(fit2)
fit3 <- glm(fertility ~ log(ppgdp)+lifeExpF, family =Gamma(link = "identity")); summary(fit3)


# qqplots dos residuos com envelope:

envelope(fit1, rep=100, conf=0.95, type="quantile")
envelope(fit1, rep=100, conf=0.95, type="deviance", standardized=T)
envelope(fit1, rep=100, conf=0.95, type="pearson", standardized	=T)

envelope(fit2, rep=100, conf=0.95, type="quantile")
envelope(fit2, rep=100, conf=0.95, type="deviance", standardized=T)
envelope(fit2, rep=100, conf=0.95, type="pearson", standardized	=T)

envelope(fit3, rep=100, conf=0.95, type="quantile")
envelope(fit3, rep=100, conf=0.95, type="deviance", standardized=T)
envelope(fit3, rep=100, conf=0.95, type="pearson", standardized	=T)


#************************ Aplicação - Modelo de regressão Bernoulli *******************#

# No arquivo pulso.txt são descritas as variáveis pulsação em repouso (1: normal, 2: alta),
# hábito de fumar (1: sim, 2: não) e peso (em kg) de 92 adultos do sexo masculino.

# Faça uma análise descritiva dos dados. Ajuste MLGs Bernoulli considerando diversas funções de ligação
# para explicar a probabilidade de pulsação alta dadas as demais variáveis. 

# Escolha o melhor modelo ajustado e verifique se existem desvios da suposição do modelo. 

# Interprete os resultados de acordo com a função de ligação escolhida. 


#Dados:

pulso <- scan("pulso.txt", list(pulsacao=0, habitof=0, peso=0))
attach(pulso)

# A mudança abaixo não é necessária, mas pode ser feita.
 # pulsacao[pulsacao==1] <- 0
 # pulsacao[pulsacao==2] <- 1

pulsacao <- as.factor(pulsacao);
habitof <- as.factor(habitof)


fit1 <- glm(pulsacao~habitof+peso, family= binomial(link = "logit")); summary(fit1)

#Como interpretamos? Vale a pena trocar a função de ligação?


# R^2 baseado na funcao desvio:

R2 <- 1 - fit1$deviance/fit1$null; R2

require(MASS)
stepAIC(fit1) #Seleção automatica - passo atrás


#** Diagnostico **#

# TEM QUE GERAR MAIS DE UM GRÁFICO POIS A DISTRIBUIÇÃO É DISCRETA
envelope(fit1, rep=100, conf=0.95, type="quantile") #gere mais de uma vez
#O resíduo quantilico está próximo da normal, pela visualiação do gráfico, ou por ser um resultado
# esperado ?
envelope(fit1, rep=100, conf=0.95, type="deviance", standardized=T)
envelope(fit1, rep=100, conf=0.95, type="pearson", standardized=T)


# Outra forma de obter resíduos quantílicos:

#install.packages("statmod")
require(statmod)

resq <- qresid(fit1) #gere mais de uma vez
a <- max(resq)
b <- min(resq)
par(mfrow=c(1,2))

plot(resq,xlab="Índice", ylab="Resíduo Quantílico",ylim=c(b-1,a+1), pch=16)
abline(2,0,lty=2)
abline(-2,0,lty=2)
#identify(resq, n=1)
#
plot(fitted(fit1),resq,xlab="Valor Ajustado", 
ylab="Resíduo Quantílico", ylim=c(b-1,a+1), pch=16)
abline(2,0,lty=2)
abline(-2,0,lty=2)
#identify(fitted(fit1),resq, n=1)


#** Diagnostico - Scripts Prof. Gilberto **#
fit.model <- fit1
source("envel_bino.R")
source("diag_bino.R")

# O que você acha destes graficos de diagnostico?


#************************ APLICACAO - AJUSTE POISSON *******************#

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
#head(TVCabo)
#names(TVCabo)

# ** Ajuste via MLG Poisson **#

fit1 <- glm(Nass ~ Domic+Percap+Taxa+Custo+Ncabo+Ntv, family=poisson(link = "log"));summary(fit1)

stepAIC(fit1) #selecao de covariaveis

#** Diagnostico **#
fit.model <- fit1
source("envel_pois.R")
source("diag_pois.R")


# Graficos de probabilidades normal dos resíduos com envelope simulado

envelope(fit1, rep=100, conf=0.95, type="quantile") 
envelope(fit1, rep=100, conf=0.95, type="deviance", standardized=T)
envelope(fit1, rep=100, conf=0.95, type="pearson", standardized=T)

# O ajuste via Poisson está razoável?


# Ajuste um modelo de regressão normal e compare via diagnósticos.

