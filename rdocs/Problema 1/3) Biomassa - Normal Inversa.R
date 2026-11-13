source(here::here("rdocs/Pacotes", "Pacotes.R"))

# Dados
data(lime); 
lime <- lime %>%
  mutate(Origin = fct_reorder(Origin, Foliage, .fun = median))
attach(lime)

# Não utilizar DBH e Age juntas, pois, tem alta multicolinearidade
#------------------------------------------------------------------------------#---- 
#                 Modelo Normal Inversa - Ligação Log                          #
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


#Aplicar o Log não fez muita diferença no Envelope
envelope(fit8_NIL, rep=100, conf=0.95, type="quantile")
envelope(fit8_NIL, rep=100, conf=0.95, type="deviance")
envelope(fit8_NIL, rep=100, conf=0.95, type="pearson")

fit.model <- fit8_NIL
source(here::here("rdocs/Pacotes", "diag_ninv.R"))
