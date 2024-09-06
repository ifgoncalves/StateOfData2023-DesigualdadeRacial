


# Importando pacotes necessários para a análise REVISAR))

library(kaggler)
library(openxlsx)
library(readr)
library(readxl)

# Primeira Etapa: Download dos dados que serão utilizados para as análises

# Base de dados 1: Pesquisa State of Data Brazil 2023, disponível no Kaggle


## IMPORTANTE: O kaggle.json -> Necessário baixar no kaggle, com suas credenciais
## para login e download da base

# Autenticando via json
kgl_auth(creds_file = 'kaggle.json')

# Conectando na base de dados que será baixada
response <- kgl_datasets_download_all(
        owner_dataset = "datahackers/state-of-data-brazil-2023")

# Diretório onde o arquivo será salvo
diretorio_state_of_data <- "fonte_dados_pesquisa_state_of_data"

# Criar o diretório se não existir
if (!dir.exists(diretorio_state_of_data)) {
        dir.create(diretorio_state_of_data, recursive = TRUE)
}

# Baixando o arquivo
download.file(response[["url"]],
              "fonte_dados_pesquisa_state_of_data/state_of_data_br_2023.zip",
              mode="wb")



# Base de dados 2 e 3: Dados Estatísticos do Censo 2022 do IBGE

# Caminho do diretório onde o arquivo será salvo
diretorio_ibge_censo <- "fonte_dados_IBGE/censo_2022"

# Criar o diretório se não existir
if (!dir.exists(diretorio_ibge_censo)) {
        dir.create(diretorio_ibge_censo, recursive = TRUE)
}


# URL do ibge para download direto
url_ibge_cor_ou_raca <- "https://ftp.ibge.gov.br/Censos/Censo_Demografico_2022/Populacao_por_cor_ou_raca_Resultados_do_universo/Tabelas_selecionadas/xlsx/Tabela_02_Pop_resid_por_cor_ou_raca_e_pessoas_indigenas_2022_BR_GR.xlsx"
url_ibge_razao_generos <- "https://ftp.ibge.gov.br/Censos/Censo_Demografico_2022/Populacao_por_cor_ou_raca_Resultados_do_universo/Tabelas_selecionadas/xlsx/Tabela_05_Razao_de_sexos_por_cor_ou_raca_e_das_pessoas_ind%c3%adigenas_2022_BR_GR.xlsx"


# Caminho completo do arquivo de destino
arq_destino_cor_ou_raca  <- file.path(diretorio_ibge_censo,
                               "Tabela_02_cor_ou_raca.xlsx")

arq_destino_ibge_razao_generos <- file.path(diretorio_ibge_censo,
                               "Tabela_05_genero_por_cor_ou_raca.xlsx")

# Download do arquivo
download.file(url_ibge_cor_ou_raca, arq_destino_cor_ou_raca , method = "curl", mode = "wb")

download.file(url_ibge_razao_generos, arq_destino_ibge_razao_generos, method = "curl", mode = "wb")




# Base de dados 4, 5 e 6: Dados Estatísticos sobre Genero - 2022 IBGE

# Caminho do diretório onde o arquivo será salvo
diretorio_ibge_ec <- "fonte_dados_IBGE/estatisticas_genero"

# Criar o diretório se não existir
if (!dir.exists(diretorio_ibge_ec)) {
        dir.create(diretorio_ibge_ec, recursive = TRUE)
}


# URL do ibge para download direto
url_ibge_empoderamento_econo <- "https://ftp.ibge.gov.br/Estatisticas_de_Genero/Indicadores_sociais_das_mulheres_no_Brasil_3a_edicao/xls/1_Empoderamento_Economico_xls.zip"
url_ibge_educacao_genero <- "https://ftp.ibge.gov.br/Estatisticas_de_Genero/Indicadores_sociais_das_mulheres_no_Brasil_3a_edicao/xls/2_Educacao_xls.zip"
url_ibge_vida_publica <- "https://ftp.ibge.gov.br/Estatisticas_de_Genero/Indicadores_sociais_das_mulheres_no_Brasil_3a_edicao/xls/4_Vida_Publica_xls.zip"


# Caminho completo do arquivo de destino
arq_destino_empoderamento_econo  <- file.path(diretorio_ibge_ec,
                               "1_Empoderamento_Economico_xls.zip")

arq_destino_educacao_genero <- file.path(diretorio_ibge_ec,
                               "2_Educacao_xls.zip")

arq_destino_vida_publica <- file.path(diretorio_ibge_ec,
                                  "4_Vida_Publica_xls.zip")

# Download do arquivo
download.file(url_ibge_empoderamento_econo, arq_destino_empoderamento_econo,
              method = "curl", mode = "wb")

download.file(url_ibge_educacao_genero, arq_destino_educacao_genero,
              method = "curl", mode = "wb")

download.file(url_ibge_vida_publica, arq_destino_vida_publica,
              method = "curl", mode = "wb")




# Base de dados 2 e 4: Dados Estatísticos sobre Raça - 2022 IBGE

# Caminho do diretório onde o arquivo será salvo
diretorio_ibge_er <- "fonte_dados_IBGE/estatisticas_raca"

# Criar o diretório se não existir
if (!dir.exists(diretorio_ibge_er)) {
        dir.create(diretorio_ibge_er, recursive = TRUE)
}


# URL do ibge para download direto
url_ibge_trabalho <- "https://ftp.ibge.gov.br/Indicadores_Sociais/Desigualdades_por_Cor_ou_Raca_2a_edicao/xls/2_Trabalho_Renda_xls.zip"
url_ibge_educacao_raca <- "https://ftp.ibge.gov.br/Indicadores_Sociais/Desigualdades_por_Cor_ou_Raca_2a_edicao/xls/4_Educacao_xls.zip"


# Caminho completo do arquivo de destino
arq_destino_trabalho  <- file.path(diretorio_ibge_er,
                                              "2_Trabalho_Renda_xls.zip")

arq_destino_educacao_raca <- file.path(diretorio_ibge_er,
                                  "4_Educacao_xls.zip")


# Download do arquivo
download.file(url_ibge_trabalho, arq_destino_trabalho,
              method = "curl", mode = "wb")

download.file(url_ibge_educacao_raca, arq_destino_educacao_raca,
              method = "curl", mode = "wb")



