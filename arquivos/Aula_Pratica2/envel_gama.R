#------------------------------------------------------------#
# Para rodar este programa deixe no objeto fit.model a saida      
# do ajuste da regressao com erros gama e ligacao log.  Deixe     
# tambem os dados disponiveis atraves do comando attach(...).    
# Depois use o comando source(...) no R ou S-Plus para        
# executar o programa. A sequencia de comandos e a seguinte:               
#
#       fit.model <- ajuste
#       attach(dados)
#       source("envel_gama")                                      
#
# A saida sera o grafico de envelope para o residuo componente 
# do desvio padronizado. Para colocar um titulo no grafico use
# o comando title("..."). Para  usar  outras  ligacoes mudar 
# no programa abaixo o termo family=Gamma(link=log) para 
# family=Gamma no caso de ligacao  reciproca ou por 
# family= Gamma(link=identity) no caso de ligacao identidade.
#------------------------------------------------------------#
par(mfrow=c(1,1))
X <- model.matrix(fit.model)
n <- nrow(X)
p <- ncol(X)
w <- fit.model$weights
W <- diag(w)
H <- solve(t(X)%*%W%*%X)
H <- sqrt(W)%*%X%*%H%*%t(X)%*%sqrt(W)
h <- diag(H)
ro <- resid(fit.model,type="response")
fi <- (n-p)/sum((ro/(fitted(fit.model)))^ 2)
td <- resid(fit.model,type="deviance")*sqrt(fi/(1-h))
#
e <- matrix(0,n,100)
#
for(i in 1:100){
resp <- rgamma(n,fi)
resp <- (fitted(fit.model)/fi)*resp
fit <- glm(resp ~ X, family=Gamma(link=log))
w <- fit$weights
W <- diag(w)
H <- solve(t(X)%*%W%*%X)
H <- sqrt(W)%*%X%*%H%*%t(X)%*%sqrt(W)
h <- diag(H)
ro <- resid(fit,type="response")
phi <- (n-p)/sum((ro/(fitted(fit)))^ 2)
e[,i] <- sort(resid(fit,type="deviance")*sqrt(phi/(1-h)))}
#
e1 <- numeric(n)
e2 <- numeric(n)
#
for(i in 1:n){
  eo <- sort(e[i,])
e1[i] <- (eo[2]+eo[3])/2
e2[i] <- (eo[97]+eo[98])/2}
#
med <- apply(e,1,mean)
faixa <- range(td,e1,e2)
par(pty="s")
qqnorm(td, xlab="Percentil da N(0,1)",
ylab="Componente do Desvio", ylim=faixa, pch=16, main="")
par(new=TRUE)
#
qqnorm(e1,axes=F,xlab="",ylab="",type="l",ylim=faixa,lty=1, main="")
par(new=TRUE)
qqnorm(e2,axes=F,xlab="",ylab="", type="l",ylim=faixa,lty=1, main="")
par(new=TRUE)
qqnorm(med,axes=F,xlab="", ylab="", type="l",ylim=faixa,lty=2, main="")
#------------------------------------------------------------#                      
