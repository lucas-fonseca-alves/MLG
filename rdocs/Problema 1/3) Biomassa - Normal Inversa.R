source(here::here("rdocs/Pacotes", "Pacotes.R"))

# Dados
data(lime); 
lime <- lime %>%
  mutate(Origin = fct_reorder(Origin, Foliage, .fun = median))
attach(lime)

# Não utilizar DBH e Age juntas, pois, tem alta multicolinearidade
#------------------------------------------------------------------------------#---- 
#                 Modelo Normal Inversa - Ligação Log     (fit 3, levemente melhor no evenlope, e entre o 3 e 7 a homocedasticidade não muda muito)                     #
#------------------------------------------------------------------------------#----

fit1_NIL <- glm(Foliage ~ DBH + Origin , 
               family=inverse.gaussian(link="log"))
summary(fit1_NIL)

fit2_NIL <- glm(Foliage ~ Age + Origin , 
                family=inverse.gaussian(link="log"))
summary(fit2_NIL)

fit3_NIL <- glm(Foliage ~ log(DBH) + Origin , 
                family=inverse.gaussian(link="log"))
summary(fit3_NIL)

# fit4_NIL <- glm(Foliage ~ log(Age) + Origin , 
#                 family=inverse.gaussian(link="log"))
# summary(fit4_NIL)

fit5_NIL <- glm(Foliage ~ DBH , 
                family=inverse.gaussian(link="log"))
summary(fit5_NIL)

fit6_NIL <- glm(Foliage ~ Age, 
                family=inverse.gaussian(link="log"))
summary(fit6_NIL)

fit7_NIL <- glm(Foliage ~ log(DBH), 
                family=inverse.gaussian(link="log"))
summary(fit7_NIL)

fit8_NIL <- glm(Foliage ~ log(Age), 
                family=inverse.gaussian(link="log"))
summary(fit8_NIL)

#**********************************Diagnóstico**********************************#----

#*** Avaliando a suposicao de distribuicao Normal da resposta com Ligação Log ***#
envelope(fit1_NIL, rep=100, conf=0.95, type="quantile")
envelope(fit1_NIL, rep=100, conf=0.95, type="deviance")
envelope(fit1_NIL, rep=100, conf=0.95, type="pearson")

fit.model <- fit1_NIL
source(here::here("rdocs/Pacotes", "diag_ninv.R"))

envelope(fit2_NIL, rep=100, conf=0.95, type="quantile")
envelope(fit2_NIL, rep=100, conf=0.95, type="deviance")
envelope(fit2_NIL, rep=100, conf=0.95, type="pearson")

fit.model <- fit2_NIL
source(here::here("rdocs/Pacotes", "diag_ninv.R"))


envelope(fit3_NIL, rep=100, conf=0.95, type="quantile")
envelope(fit3_NIL, rep=100, conf=0.95, type="deviance")
envelope(fit3_NIL, rep=100, conf=0.95, type="pearson")

#Gráficos de homogeneidade (mas ainda enxergo o homogeneidade como um funil) e Variável z parecem ok 
fit.model <- fit3_NIL
source(here::here("rdocs/Pacotes", "diag_ninv.R"))
# 15  20  24 144 210
# 20, 24
# 20  21  22  23  24  31  32  75  76 202 209 210 257
# 20  21  22  23  24  32  75  76 202 209 254 257
#  22 23

#20, 21, 22, 24, 

fit3_NIL_w20 <- glm(Foliage ~ log(DBH) + Origin ,
                 family=inverse.gaussian(link = "log"),
                 data = lime,
                 subset=-c(20));summary(fit3_NIL_w20)

fit3_NIL_w21 <- glm(Foliage ~ log(DBH) + Origin ,
                    family=inverse.gaussian(link = "log"),
                    data = lime,
                    subset=-c(21));summary(fit3_NIL_w21)

fit3_NIL_w22 <- glm(Foliage ~ log(DBH) + Origin ,
                    family=inverse.gaussian(link = "log"),
                    data = lime,
                    subset=-c(22));summary(fit3_NIL_w22)

fit3_NIL_w24 <- glm(Foliage ~ log(DBH) + Origin ,
                    family=inverse.gaussian(link = "log"),
                    data = lime,
                    subset=-c(24));summary(fit3_NIL_w24)

fit3_NIL_w <- glm(Foliage ~ log(DBH) + Origin ,
                    family=inverse.gaussian(link = "log"),
                    data = lime,
                    subset=-c(20, 21, 22, 24));summary(fit3_NIL_w)

MR(coef(fit3_NIL), coef(fit3_NIL_w20)) # 0.83 0.76 % 
MR(coef(fit3_NIL), coef(fit3_NIL_w21)) # 0.83 0.76 % 
MR(coef(fit3_NIL), coef(fit3_NIL_w22)) # 0.83 0.76 % 
MR(coef(fit3_NIL), coef(fit3_NIL_w24)) # 0.83 0.76 % 
MR(coef(fit3_NIL), coef(fit3_NIL_w)) # 0.83 0.76 % 

# envelope(fit4_NIL, rep=100, conf=0.95, type="quantile")
# envelope(fit4_NIL, rep=100, conf=0.95, type="deviance")
# envelope(fit4_NIL, rep=100, conf=0.95, type="pearson")

envelope(fit5_NIL, rep=100, conf=0.95, type="quantile")
envelope(fit5_NIL, rep=100, conf=0.95, type="deviance")
envelope(fit5_NIL, rep=100, conf=0.95, type="pearson")

#Linearização fica boa, mas o homogeneidade ainda me parece funil
fit.model <- fit5_NIL
source(here::here("rdocs/Pacotes", "diag_ninv.R"))

#O 6 parece o menos pior
envelope(fit6_NIL, rep=100, conf=0.95, type="quantile")
envelope(fit6_NIL, rep=100, conf=0.95, type="deviance")
envelope(fit6_NIL, rep=100, conf=0.95, type="pearson")

fit.model <- fit6_NIL
source(here::here("rdocs/Pacotes", "diag_ninv.R"))


envelope(fit7_NIL, rep=100, conf=0.95, type="quantile")
envelope(fit7_NIL, rep=100, conf=0.95, type="deviance")
envelope(fit7_NIL, rep=100, conf=0.95, type="pearson")

fit.model <- fit7_NIL
source(here::here("rdocs/Pacotes", "diag_ninv.R"))
# 20 24
# 20 24
# 20 21 22 23  24  31  32  75  76 190 193 209 210 255 256 257
# 20  21  22  23  24  32  75  76 190 193 209 210 254 255 257
# 22, 23

fit7_NIL_w20 <- glm(Foliage ~ log(DBH),   
                    family=inverse.gaussian(link = "log"),
                    data = lime,
                    subset=-c(20));summary(fit7_NIL_w20)

fit7_NIL_w21 <- glm(Foliage ~ log(DBH), 
                    family=inverse.gaussian(link = "log"),
                    data = lime,
                    subset=-c(21));summary(fit7_NIL_w21)

fit7_NIL_w22 <- glm(Foliage ~ log(DBH), 
                    family=inverse.gaussian(link = "log"),
                    data = lime,
                    subset=-c(22));summary(fit7_NIL_w22)

fit7_NIL_w24 <- glm(Foliage ~ log(DBH), 
                    family=inverse.gaussian(link = "log"),
                    data = lime,
                    subset=-c(24));summary(fit7_NIL_w24)
fit7_NIL_w257 <- glm(Foliage ~ log(DBH), 
                    family=inverse.gaussian(link = "log"),
                    data = lime,
                    subset=-c(257));summary(fit7_NIL_w257)


fit7_NIL_w <- glm(Foliage ~ log(DBH), 
                  family=inverse.gaussian(link = "log"),
                  data = lime,
                  subset=-c(20, 21, 22, 24, 257));summary(fit7_NIL_w)

MR(coef(fit7_NIL), coef(fit7_NIL_w20)) # 0.83 0.76 % 
MR(coef(fit7_NIL), coef(fit7_NIL_w21)) # 0.83 0.76 % 
MR(coef(fit7_NIL), coef(fit7_NIL_w22)) # 0.83 0.76 % 
MR(coef(fit7_NIL), coef(fit7_NIL_w24)) # 0.83 0.76 % 
MR(coef(fit7_NIL), coef(fit7_NIL_w257)) # 0.83 0.76 % 
MR(coef(fit7_NIL), coef(fit7_NIL_w)) # 0.83 0.76 % 


#Aplicar o Log não fez muita diferença no Envelope
envelope(fit8_NIL, rep=100, conf=0.95, type="quantile")
envelope(fit8_NIL, rep=100, conf=0.95, type="deviance")
envelope(fit8_NIL, rep=100, conf=0.95, type="pearson")

fit.model <- fit8_NIL
source(here::here("rdocs/Pacotes", "diag_ninv.R"))
