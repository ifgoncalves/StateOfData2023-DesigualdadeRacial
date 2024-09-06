

# Importando pacotes necessários para a análise (REVISAR)
library(dplyr)
library(openxlsx)
library(tidyverse)
library(ggplot2)
library(readr)
library(readxl)
library(stringr)


## FUNÇÕES:
# Criando função de ajustar o título simples
formatar_cabecalhos_simples <- function(cabecalho) {
        cabecalho <- trimws(gsub("[^[:alnum:] ]", "_", cabecalho))
        cabecalho <- gsub(" ", "_", cabecalho)
        cabecalho <- gsub("__+", "_", cabecalho)
        cabecalho <- gsub("^_+|_+$", "", cabecalho) 
        cabecalho <- toupper(iconv(cabecalho, to = "ASCII//TRANSLIT"))
        return(cabecalho)
}


# Criando função de ajustar o título quando este for lista
formatar_cabecalhos_lista <- function(cabecalho) {
        partes <- unlist(strsplit(cabecalho, ","))
        segunda_parte <- trimws(gsub("[^[:alnum:] ]", "_", partes[2]))
        segunda_parte <- gsub(" ", "_", segunda_parte)
        segunda_parte <- gsub("^_+|_+$", "", segunda_parte) 
        segunda_parte <- toupper(iconv(segunda_parte, to = "ASCII//TRANSLIT"))
        return(segunda_parte)
}



