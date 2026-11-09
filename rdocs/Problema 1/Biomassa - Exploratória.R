source(here::here("rdocs", "Pacotes.R"))

# Dados
data(lime);attach(lime)

lime <- lime %>%
  mutate(Origin = fct_reorder(Origin, Foliage, .fun = median))

#------------------------------------------------------------------------------#---- 
#               Correlação e medidas resumo gerais                             #
#------------------------------------------------------------------------------#----

mat_cor <- cor(lime[,-4])
# DBH tem correlação moderada com Foliage. (Bom)
# Age e DBH tem alta correlação (Ruim)

ggcorrplot(mat_cor,
           method = "square",       # ou "circle"
           #type   = "lower",        # mostra só o triângulo inferior
           lab    = TRUE,           # mostra os valores
           lab_size = 3,
           colors = c("firebrick", "white", "steelblue"),
           title = "Matriz de correlação",
           ggtheme = theme_minimal())


#------------------------------------------------------------------------------#---- 
#                         Variáveis individuais                                #
#------------------------------------------------------------------------------#----

#*** Foliage (Resposta)***#
psych::describe(Foliage)

summary(Foliage)

ggplot(lime ,aes(x = Foliage)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "white") +
  labs(x = 'Biomassa da Folhagem', y = "Frequência") +
  theme_minimal()
#Curva bem assimétrica á direita. Então, pensar em distribuições assimétricas contínuas
# Função gama com ligação log parece ser uma boa opção 

ggplot(lime, aes(x = "", y = Foliage)) +
  geom_boxplot(fill = "steelblue", alpha = 0.7) +
  stat_summary(fun = mean, geom = "point",
               shape = 18, size = 4, color = "white") +
  labs(x = NULL, y = "Biomassa da Foliagem") +
  theme_minimal()
# Muitos Outliers 

#*** DBH ***#
psych::describe(DBH)

summary(DBH)

ggplot(lime ,aes(x = DBH)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "white") +
  labs(x = 'Diâmetro do tronco à altura do peito (cm)', y = "Frequência") +
  theme_minimal()

ggplot(lime, aes(x = "", y = DBH)) +
  geom_boxplot(fill = "steelblue", alpha = 0.7) +
  stat_summary(fun = mean, geom = "point",
               shape = 18, size = 4, color = "white") +
  labs(x = NULL, y = "Diâmetro do tronco à altura do peito (cm)") +
  theme_minimal()


#*** Age ***# 
psych::describe(log(Age))

summary(log(Age))

ggplot(lime ,aes(x = Age)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "white") +
  labs(x = 'Idade da árvore (anos)', y = "Frequência") +
  theme_minimal()

ggplot(lime ,aes(x = log(Age))) +
  geom_histogram(bins = 30, fill = "steelblue", color = "white") +
  labs(x = 'Idade da árvore (anos)', y = "Frequência") +
  theme_minimal()
#Log parece retirar a assimetria 

ggplot(lime, aes(x = "", y = Age)) +
  geom_boxplot(fill = "steelblue", alpha = 0.7) +
  stat_summary(fun = mean, geom = "point",
               shape = 18, size = 4, color = "white") +
  labs(x = NULL, y = "Idade da árvore (anos)") +
  theme_minimal()

# o skew: ele passou de +0.64 (direita) para -0.61 (esquerda), com magnitude praticamente idêntica. Isso não é coincidência — é um diagnóstico clássico e muito informativo.
# 
# 🔍 O que o "flip" do skew indica
# Quando o log inverte a assimetria mantendo a magnitude, isso significa que o log é uma transformação forte demais para esses dados. Ele não só corrigiu a cauda direita como criou uma cauda esquerda igualmente pronunciada. Em outras palavras:
#   
# 1) A distribuição não é log-normal. Se fosse, o log aproximaria o skew de zero, não o jogaria para o lado oposto.
# 
# 2) A transformação "correta" está entre a identidade e o log. O Box-Cox formalizaria isso: o λ ótimo provavelmente está entre 0 (log) e 1 (identidade), talvez em torno de 0.3–0.5 (algo como Age^0.5, ou seja, raiz quadrada).
# 
# 3) A "assimetria não tão forte" que você percebeu está correta. Skew de 0.64 é moderado, não severo. Muitos autores consideram valores entre -1 e +1 como "aceitáveis" para a maioria dos métodos. A média e a mediana próximas confirmam isso: a cauda existe, mas não domina a distribuição.

#------------------------------------------------------------------------------#---- 
#                         Relação entre as Variáveis                           #
#------------------------------------------------------------------------------#----

#*** Foliage x DBH ***#
ggplot(lime, aes(x = DBH, y = Foliage, color = Origin)) +
  geom_point(size = 2.5) +
  labs(x = "Diâmetro do tronco à altura do peito (cm)",
       y = "Biomassa da Folhagem",
       color = "Origem") +
  theme_minimal()
# Parece uma relação nada linear, uma cara bem exponencial. 
# Também não parece ter relação com o tipo de origem

scatterplot((log(Foliage))~log(DBH), regLine = T, smooth=F, boxplots=F, pch=16, xlab="Diâmetro do tronco à altura do peito (cm)",
            ylab="Biomassa da Folhagem",cex=1.2,cex.lab=1.5, cex.axis=1.5)

#*** Foliage x Age ***#
ggplot(lime, aes(x = Age, y = Foliage, color = Origin)) +
  geom_point(size = 2.5) +
  labs(x = "Idade da árvore (anos)",
       y = "Biomassa da Folhagem",
       color = "Origem") +
  theme_minimal()
# Parece nada linear também

scatterplot((log(Foliage))~log(Age), regLine = T, smooth=F, boxplots=F, pch=16, xlab="Idade da árvore (anos)",
            ylab="Biomassa da Folhagem",cex=1.2,cex.lab=1.5, cex.axis=1.5)


#*** Foliage x Origin ***#
ggplot(lime, aes(x = reorder(Origin, Foliage, FUN = median),
                 y = Foliage,
                 fill = reorder(Origin, Foliage, FUN = median))) +
  geom_boxplot() +
  stat_summary(fun = mean, geom = "point",
               shape = 18, size = 3, color = "white") +
  labs(x = "Origem",
       y = "Biomassa da Folhagem",
       fill = "Origem") +
  theme_minimal()

describeBy(lime$Foliage, group = lime$Origin, mat = TRUE)
tapply(lime$Foliage, lime$Origin, summary)

#Todas as relações são assime´tricas, e com outliers
# Planted é a que tem maior variancia, natural é a que mais compacta
#Copiece é a mais próxima de uma simetria
# Mediana de coppice e planted são bem próximas. 

