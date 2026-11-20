source(here::here("rdocs/Pacotes", "Pacotes.R"))

# Dados
data(lime)

lime <- lime %>%
  mutate(Origin = fct_reorder(Origin, Foliage, .fun = median))

attach(lime)

caminho_Problema1 <- "resultados/Problema 1/Biomassa - Exploratória"

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
ggsave("Correlacao_Biomassa.pdf",path = caminho_Problema1, width = 158, height = 93, units = "mm")

#------------------------------------------------------------------------------#---- 
#                         Variáveis individuais                                #
#------------------------------------------------------------------------------#----

#*** Foliage (Resposta)***#
psych::describe(Foliage)

summary(Foliage)

ggplot(lime ,aes(x = Foliage)) +
  geom_histogram(bins = 15, fill = "steelblue", color = "white") +
  labs(x = 'Biomassa da Folhagem', y = "Frequência") +
  theme_minimal()
ggsave("Histograma_Biomassa.pdf",path = caminho_Problema1, width = 158, height = 93, units = "mm")

#Curva bem assimétrica á direita. Então, pensar em distribuições assimétricas contínuas
# Função gama com ligação log parece ser uma boa opção 

ggplot(lime, aes(x = "", y = Foliage)) +
  geom_boxplot(fill = "steelblue", alpha = 0.7) +
  stat_summary(fun = mean, geom = "point",
               shape = 18, size = 4, color = "white") +
  labs(x = NULL, y = "Biomassa da Foliagem") +
  theme_minimal()
ggsave("Boxplot_Biomassa.pdf",path = caminho_Problema1, width = 158, height = 93, units = "mm")

# Muitos Outliers 

#*** DBH ***#
psych::describe(DBH)

summary(DBH)

ggplot(lime ,aes(x = DBH)) +
  geom_histogram(bins = 15, fill = "steelblue", color = "white") +
  labs(x = 'Diâmetro do tronco à altura do peito (cm)', y = "Frequência") +
  theme_minimal()
ggsave("Histograma_DBH.pdf",path = caminho_Problema1, width = 158, height = 93, units = "mm")


ggplot(lime, aes(x = "", y = DBH)) +
  geom_boxplot(fill = "steelblue", alpha = 0.7) +
  stat_summary(fun = mean, geom = "point",
               shape = 18, size = 4, color = "white") +
  labs(x = NULL, y = "Diâmetro do tronco à altura do peito (cm)") +
  theme_minimal()
ggsave("Boxplot_DBH.pdf",path = caminho_Problema1, width = 158, height = 93, units = "mm")


#*** Age ***# 
psych::describe(Age)

summary(Age)

ggplot(lime ,aes(x = Age)) +
  geom_histogram(bins = 15, fill = "steelblue", color = "white") +
  labs(x = 'Idade da árvore (anos)', y = "Frequência") +
  theme_minimal()
ggsave("Histograma_Age.pdf",path = caminho_Problema1, width = 158, height = 93, units = "mm")

ggplot(lime, aes(x = "", y = Age)) +
  geom_boxplot(fill = "steelblue", alpha = 0.7) +
  stat_summary(fun = mean, geom = "point",
               shape = 18, size = 4, color = "white") +
  labs(x = NULL, y = "Idade da árvore (anos)") +
  theme_minimal()
ggsave("Boxplot_Age.pdf",path = caminho_Problema1, width = 158, height = 93, units = "mm")

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

p1 <- ggplot(lime, aes(x = DBH, y = Foliage, color = Origin)) +
  geom_point(size = 2.5) +
  labs(x = "Diâmetro do tronco à altura do peito (cm)",
       y = "Biomassa da Folhagem",
       color = "Origem") +
  theme_minimal()

p2 <- ggplot(lime, aes(x = log(DBH), y = Foliage, color = Origin)) +
  geom_point(size = 2.5) +
  labs(x = "Log do Diâmetro do tronco à altura do peito (cm)",
       y = "Biomassa da Folhagem",
       color = "Origem") +
  theme_minimal()

p3 <- ggplot(lime, aes(x = DBH, y = log(Foliage), color = Origin)) +
  geom_point(size = 2.5) +
  labs(x = "Diâmetro do tronco à altura do peito (cm)",
       y = "Log da Biomassa da Folhagem",
       color = "Origem") +
  theme_minimal()

p4 <- ggplot(lime, aes(x = log(DBH), y = log(Foliage), color = Origin)) +
  geom_point(size = 2.5) +
  labs(x = "Log Diâmetro do tronco à altura do peito (cm)",
       y = "Log Biomassa da Folhagem",
       color = "Origem") +
  theme_minimal()

cor(log(lime$DBH), log(lime$Foliage))


#Usando o pacote patchwork
# 2. Ajuste o layout para dar mais espaço entre os gráficos e controlar as 
# margens
#O plot_spacer() cria uma faixa vazia entre a linha de cima e a de baixo, e 
# o heights = c(1, 0.15, 1) define quanto essa faixa ocupa. Isso dá um 
# "respiro" sem tocar nos gráficos.

combinado <- (p1 + p2) / plot_spacer() / (p3 + p4) +
  plot_layout(guides = "collect", heights = c(1, 0.12, 1)) &
  theme(
    axis.title = element_text(size = rel(0.7)),  # <-- reduz o título do eixo
    plot.margin = margin(3, 3, 3, 3, "pt")  # margem menor para sobrar espaço
  )


# 3. Salve o objeto 'combinado', não os gráficos individuais
ggsave(
  filename = "Dispersão_Biomassa_DBH.pdf",
  plot = combinado,  # <-- Ponto crucial: especificar o objeto combinado
  path = caminho_Problema1,
  width = 158,
  height = 93,
  units = "mm"
)

# Parece uma relação nada linear, uma cara bem exponencial. 
# Também não parece ter relação com o tipo de origem

scatterplot((log(Foliage))~log(DBH), regLine = T, smooth=F, boxplots=F, pch=16, xlab="Diâmetro do tronco à altura do peito (cm)",
            ylab="Biomassa da Folhagem",cex=1.2,cex.lab=1.5, cex.axis=1.5)

#*** Foliage x Age ***#

scatterplot((log(Foliage))~log(Age), regLine = T, smooth=F, boxplots=F, pch=16, xlab="Idade da árvore (anos)",
            ylab="Biomassa da Folhagem",cex=1.2,cex.lab=1.5, cex.axis=1.5)


p5 <- ggplot(lime, aes(x = Age, y = Foliage, color = Origin)) +
  geom_point(size = 2.5) +
  labs(x = "Idade da árvore (anos)",
       y = "Biomassa da Folhagem",
       color = "Origem") +
  theme_minimal()
# Parece nada linear também

p6 <- ggplot(lime, aes(x = log(Age), y = Foliage, color = Origin)) +
  geom_point(size = 2.5) +
  labs(x = "Log da Idade da árvore (anos)",
       y = "Biomassa da Folhagem",
       color = "Origem") +
  theme_minimal()

p7 <- ggplot(lime, aes(x = Age, y = log(Foliage), color = Origin)) +
  geom_point(size = 2.5) +
  labs(x = "Idade da árvore (anos)",
       y = "Log da Biomassa da Folhagem",
       color = "Origem") +
  theme_minimal()

p8 <- ggplot(lime, aes(x = log(Age), y = log(Foliage), color = Origin)) +
  geom_point(size = 2.5) +
  labs(x = "Log da Idade da árvore (anos)",
       y = "Log da Biomassa da Folhagem",
       color = "Origem") +
  theme_minimal()

#Usando o pacote patchwork
# 2. Ajuste o layout para dar mais espaço entre os gráficos e controlar as 
# margens
#O plot_spacer() cria uma faixa vazia entre a linha de cima e a de baixo, e 
# o heights = c(1, 0.15, 1) define quanto essa faixa ocupa. Isso dá um 
# "respiro" sem tocar nos gráficos.

combinado <- (p5 + p6) / plot_spacer() / (p7 + p8) +
  plot_layout(guides = "collect", heights = c(1, 0.12, 1)) &
  theme(
    axis.title = element_text(size = rel(0.7)),  # <-- reduz o título do eixo
    plot.margin = margin(3, 3, 3, 3, "pt")  # margem menor para sobrar espaço
  )

cor(log(lime$Age), log(lime$Foliage))


# 3. Salve o objeto 'combinado', não os gráficos individuais
ggsave(
  filename = "Dispersão_Biomassa_Age.pdf",
  plot = combinado,  # <-- Ponto crucial: especificar o objeto combinado
  path = caminho_Problema1,
  width = 158,
  height = 93,
  units = "mm"
)


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

ggsave("Boxplot_Biomassa_Origin.pdf",path = caminho_Problema1, width = 158, height = 93, units = "mm")

describeBy(lime$Foliage, group = lime$Origin, mat = TRUE)
tapply(lime$Foliage, lime$Origin, summary)

table(lime$Origin)
#Todas as relações são assime´tricas, e com outliers
# Planted é a que tem maior variancia, natural é a que mais compacta
#Copiece é a mais próxima de uma simetria
# Mediana de coppice e planted são bem próximas. 

