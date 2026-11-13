#LEIA O README

#Pacote necessário para rodar o source
library(here)

#Pacote onde os dados estão
library(GLMsData)

#Pacotes necessários para as análises
library(tidyverse)
library(psych)
library(ggcorrplot)


#Para os modelos de MLG
library(MASS)
library(glmtoolbox)#envelopes
library(alr4)

#Função de mudança relativa:
# Calculo da mudança relativa
MR <- function(a,b) cat("Mudança relativa = ", round(as.numeric((b/a-1)*100),2), "%","\n")
