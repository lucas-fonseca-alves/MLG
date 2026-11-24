source(here::here("rdocs/Pacotes", "Pacotes.R"))

# Dados
data(lime)

lime <- lime %>%
  mutate(Origin = fct_reorder(Origin, Foliage, .fun = median))

attach(lime)
# Não utilizar DBH e Age juntas, pois, tem alta multicolinearidade
#------------------------------------------------------------------------------#---- 
#                     Modelo Gama - Ligação Log (fit 7 venceu)                               #
#------------------------------------------------------------------------------#----
#*** Ajustes ***#
fit1_GL <- glm(Foliage ~ DBH + Origin , 
               family=Gamma(link = "log"))
summary(fit1_GL)

fit2_GL <- glm(Foliage ~ Age + Origin , 
               family=Gamma(link = "log"))
summary(fit2_GL)

fit3_GL <- glm(Foliage ~ DBH, 
               family=Gamma(link = "log"))
summary(fit3_GL)

fit4_GL <- glm(Foliage ~ Age, 
               family=Gamma(link = "log"))
summary(fit4_GL)

fit5_GL <- glm(Foliage ~ log(DBH) + Origin , 
               family=Gamma(link = "log"))
summary(fit5_GL)

fit6_GL <- glm(Foliage ~ log(Age) + Origin , 
               family=Gamma(link = "log"))
summary(fit6_GL)

fit7_GL <- glm(Foliage ~ log(DBH), 
               family=Gamma(link = "log"))
summary(fit7_GL)

fit8_GL <- glm(Foliage ~ log(Age), 
               family=Gamma(link = "log"))
summary(fit8_GL)

#-------------------------------------------------------------------------------#----

#**********************************Diagnóstico**********************************#----

#*** Avaliando a suposicao de distribuicao Gamma da resposta com Ligação Log ***#
envelope(fit1_GL, rep=100, conf=0.95, type="quantile")
envelope(fit1_GL, rep=100, conf=0.95, type="deviance")
envelope(fit1_GL, rep=100, conf=0.95, type="pearson")

fit.model <- fit1_GL
source(here::here("rdocs/Pacotes", "diag_gama.R"))

envelope(fit2_GL, rep=100, conf=0.95, type="quantile")
envelope(fit2_GL, rep=100, conf=0.95, type="deviance")
envelope(fit2_GL, rep=100, conf=0.95, type="pearson")

fit.model <- fit2_GL
source(here::here("rdocs/Pacotes", "diag_gama.R"))


envelope(fit3_GL, rep=100, conf=0.95, type="quantile")
envelope(fit3_GL, rep=100, conf=0.95, type="deviance")
envelope(fit3_GL, rep=100, conf=0.95, type="pearson")

fit.model <- fit3_GL
source(here::here("rdocs/Pacotes", "diag_gama.R"))


envelope(fit4_GL, rep=100, conf=0.95, type="quantile")
envelope(fit4_GL, rep=100, conf=0.95, type="deviance")
envelope(fit4_GL, rep=100, conf=0.95, type="pearson")

fit.model <- fit4_GL
source(here::here("rdocs/Pacotes", "diag_gama.R"))


envelope(fit5_GL, rep=100, conf=0.95, type="quantile")
envelope(fit5_GL, rep=100, conf=0.95, type="deviance")
envelope(fit5_GL, rep=100, conf=0.95, type="pearson")

fit.model <- fit5_GL
source(here::here("rdocs/Pacotes", "diag_gama.R"))

envelope(fit6_GL, rep=100, conf=0.95, type="quantile")
envelope(fit6_GL, rep=100, conf=0.95, type="deviance")
envelope(fit6_GL, rep=100, conf=0.95, type="pearson")

fit.model <- fit6_GL
source(here::here("rdocs/Pacotes", "diag_gama.R"))
#22, 23
#15, 22, 23
#15, 22, 23


fit6_GL_w15 <- glm(Foliage ~ log(Age) + Origin , 
                       family=Gamma(link = "log"),
                       data = lime,
                       subset=-c(15));summary(fit6_GL_w15)

fit6_GL_w22 <- glm(Foliage ~ log(Age) + Origin , 
                   family=Gamma(link = "log"),
                   data = lime,
                   subset=-c(22));summary(fit6_GL_w22)

fit6_GL_w23 <- glm(Foliage ~ log(Age) + Origin , 
                   family=Gamma(link = "log"),
                   data = lime,
                   subset=-c(23));summary(fit6_GL_w23)

fit6_GL_w <- glm(Foliage ~ log(Age) + Origin , 
                   family=Gamma(link = "log"),
                   data = lime,
                   subset=-c(15, 22, 23));summary(fit6_GL_w)

MR(coef(fit6_GL), coef(fit6_GL_w15)) # -0.84 -1.11 3.44 2 %
MR(coef(fit6_GL), coef(fit6_GL_w22)) # 2.36 1.94 6.39 3.04 % 
MR(coef(fit6_GL), coef(fit6_GL_w23)) # 6.18 5.09 16.9 7.97 %
MR(coef(fit6_GL), coef(fit6_GL_w)) # 8.32 6.44 28.33 13.71 % 



envelope(fit7_GL, rep = 100, conf = 0.95, type = "quantile", identify = 3)

envelope(fit7_GL, rep=100, conf=0.95, type="deviance")
envelope(fit7_GL, rep=100, conf=0.95, type="pearson")

fit.model <- fit7_GL
source(here::here("rdocs/Pacotes", "diag_gama.R"))
# 20, 24
# 21, 22, 23, 377
# 22, 23, 32, 226, 344, 377
# 22, 23, 377



fit7_GL_w20 <- glm(Foliage ~ log(DBH), 
                   family=Gamma(link = "log"),
                   data = lime,
                   subset=-c(20));summary(fit7_GL_w20)

fit7_GL_w21 <- glm(Foliage ~ log(DBH), 
                   family=Gamma(link = "log"),
                   data = lime,
                   subset=-c(21));summary(fit7_GL_w21)

fit7_GL_w22 <- glm(Foliage ~ log(DBH), 
                   family=Gamma(link = "log"),
                   data = lime,
                   subset=-c(22));summary(fit7_GL_w22)

fit7_GL_w23 <- glm(Foliage ~ log(DBH), 
                   family=Gamma(link = "log"),
                   data = lime,
                   subset=-c(23));summary(fit7_GL_w23)

fit7_GL_w32 <- glm(Foliage ~ log(DBH), 
                   family=Gamma(link = "log"),
                   data = lime,
                   subset=-c(32));summary(fit7_GL_w32)

fit7_GL_w226 <- glm(Foliage ~ log(DBH), 
                   family=Gamma(link = "log"),
                   data = lime,
                   subset=-c(226));summary(fit7_GL_w226)

fit7_GL_w344 <- glm(Foliage ~ log(DBH), 
                    family=Gamma(link = "log"),
                    data = lime,
                    subset=-c(344));summary(fit7_GL_w344)

fit7_GL_w377 <- glm(Foliage ~ log(DBH), 
                   family=Gamma(link = "log"),
                   data = lime,
                   subset=-c(377));summary(fit7_GL_w377)

fit7_GL_w <- glm(Foliage ~ log(DBH), 
                    family=Gamma(link = "log"),
                    data = lime,
                    subset=-c(20, 21, 22, 23, 32, 226, 344, 377));summary(fit7_GL_w)



MR(coef(fit7_GL), coef(fit7_GL_w20)) # 0.83 0.76 % 
MR(coef(fit7_GL), coef(fit7_GL_w21)) # 0.83 0.76 % 
MR(coef(fit7_GL), coef(fit7_GL_w22)) # 2.5 2.1 % 
MR(coef(fit7_GL), coef(fit7_GL_w23)) # 2.34 1.85 % 
MR(coef(fit7_GL), coef(fit7_GL_w32)) # 2.34 1.85 % 
MR(coef(fit7_GL), coef(fit7_GL_w226)) # 2.34 1.85 % 
MR(coef(fit7_GL), coef(fit7_GL_w344)) # 2.34 1.85 % 
MR(coef(fit7_GL), coef(fit7_GL_w377)) # -0.61 -0.75 %
MR(coef(fit7_GL), coef(fit7_GL_w)) # 5.55 4.38 % 


envelope(fit8_GL, rep=100, conf=0.95, type="quantile")
envelope(fit8_GL, rep=100, conf=0.95, type="deviance")
envelope(fit8_GL, rep=100, conf=0.95, type="pearson")

fit.model <- fit8_GL
source(here::here("rdocs/Pacotes", "diag_gama.R"))


#------------------------------------------------------------------------------#----