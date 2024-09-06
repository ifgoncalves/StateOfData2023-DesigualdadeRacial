


# Importando os pacotes e rodando as funções que serão utilizadas nesse script

source("script_funcoes.R")


# Importando a base de dados da pesquisa da Data Hackers para o R
# Acessando o csv baixado, que está dentro da pasta ZIP

df_state_of_data_imp <- read.csv(unz(
        "fonte_dados_pesquisa_state_of_data/state_of_data_br_2023.zip",
        "State_of_data_BR_2023_Kaggle - df_survey_2023.csv"),
        check.names=FALSE,
        stringsAsFactors = FALSE,
        na.strings = "")


# Criando Tabela de Perguntas, para entendimento e documentação da pesquisa

aux_perguntas_colunas <- names(df_state_of_data_imp)[-1] %>%
        strsplit("',")


for (i in seq_along(aux_perguntas_colunas)) {
        
        # Acessando o primeiro elemento da lista i
        
        primeiro_elemento <- aux_perguntas_colunas[[i]][[1]]
        
        
        # Separando o texto por "_"
        
        primeiro_elemento_sep <- strsplit(primeiro_elemento, "_")[[1]]
        
        # Salvando o texto separado na lista
        
        aux_perguntas_colunas[[i]] <- c(primeiro_elemento_sep,
                                        aux_perguntas_colunas[[i]][-1])
        
        
        # Se a lista tiver apenas 3 elementos (Perguntas "Líderes"), inserir 0
        
        if (length(aux_perguntas_colunas[[i]]) == 3) {
                aux_perguntas_colunas[[i]] <- append(aux_perguntas_colunas[[i]],
                                                     values = 0, after = 2)
        }
}


#Transformando a lista em uma tabela

df_perguntas <- as.data.frame(do.call(rbind, aux_perguntas_colunas),
                              stringsAsFactors = FALSE)


# Tratando os caracteres "indesejados" dessa tabela

df_perguntas <- as.data.frame(lapply(df_perguntas,
                                     function(x) {gsub("[()']", "", x)
                                     }),stringsAsFactors = FALSE)


# Nomeando as colunas da tabela

colnames(df_perguntas) <- c("SECAO_CODIGO",
                            "SECAO_PERGUNTA",
                            "PERGUNTA_OPCAO",
                            "PERGUNTA_DESCRITIVO")


# Adequando as colunas para o tipo correto de dado, e removendo espaços

df_perguntas$PERGUNTA_OPCAO <- as.numeric(df_perguntas$PERGUNTA_OPCAO)
df_perguntas$SECAO_PERGUNTA <- str_trim(df_perguntas$SECAO_PERGUNTA)
df_perguntas$PERGUNTA_DESCRITIVO <- str_trim(df_perguntas$PERGUNTA_DESCRITIVO)


# Criando a tabela com o descritivo de cada Seção de perguntas. (Informação
# coletada na página do Kaggle sobre a pesquisa), para cruzamento

aux_df_perguntas_descr <- data.frame(
        SECAO_CODIGO = c("P1", "P2", "P3", "P4",
                         "P5", "P6", "P7", "P8"),
        SECAO_DESCRITIVO = c("Dados demográficos",
                             "Dados sobre carreira",
                             "Desafios dos gestores de times de dados",
                             "Conhecimentos na área de dados",
                             "Objetivos na área de dados",
                             "Conhecimentos em Engenharia de Dados/DE",
                             "Conhecimentos em Análise de Dados/DA",
                             "Conhecimentos em Ciências de Dados/DS"))


# Consolidando as tabelas

df_perguntas <- merge(df_perguntas, aux_df_perguntas_descr,
                      by = "SECAO_CODIGO")


# Adicionando mais uma coluna, para evidenciar se é uma pergunta ou uma# opção

df_perguntas <- df_perguntas %>% mutate(PERGUNTA_OPCAO_DESCRITIVO = 
                                                ifelse(PERGUNTA_OPCAO == 0,
                                                       'PERGUNTA',
                                                       'OPÇÃO'))


# Criar o diretório (se não existir) para salvar os arquivos gerados na análise

if (!dir.exists("resultados_analise")) {
        dir.create("resultados_analise", recursive = TRUE)
}

# Salvando as perguntas em um excel, para definir quais análises poderão ser
# feitas, e qual caminho irei seguir
write.xlsx(df_perguntas, "resultados_analise/perguntas_pesquisa.xlsx")


# Criando dataframe para manipular e realizar as análises (Tratar o nome
# das colunas e deixar a importação sem alterações)

df_analise_exploracao <- df_state_of_data_imp


# Renomeando as colunas para facilitar a manipulação / boas práticas

names(df_analise_exploracao) <- sapply(names(df_analise_exploracao), 
                                       formatar_cabecalhos_lista)

names(df_analise_exploracao) <- make.unique(names(df_analise_exploracao))


# Criando colunas que serão utilizadas em diversas análises

df_analise_exploracao <- df_analise_exploracao %>% 
        mutate(COR_RACA_ETNIA_AGRUP = 
                       case_when(COR_RACA_ETNIA %in% c("Amarela", 
                                                       "Indígena",
                                                       "Outra",
                                                       "Prefiro não informar") 
                                 ~ "Outras",
                                 TRUE ~ COR_RACA_ETNIA
                       ),
               GENERO_E_RACA = paste0(COR_RACA_ETNIA_AGRUP, " (", GENERO, ")"),
               NIVEL_TRAT = ifelse(GESTOR == 1, 'Gestor', NIVEL))


## Gráfico 01: Participação Racial

# Retirando quem não informou uma raça/cor, calculando QTD e PCT

df_raca <- df_analise_exploracao %>%
        filter(COR_RACA_ETNIA != "Prefiro não informar",
               GENERO %in% c("Feminino", "Masculino")) %>%
        group_by(COR_RACA_ETNIA_AGRUP) %>%
        summarise(QTD = n()) %>%
        mutate(PCT = (QTD/ sum(QTD)) * 100,
               FONTE = "PESQUISA_DADOS")


# Arquivo dos números gerais do CENSO 2022 sobre cor e raça
# Entrando no ZIP, baixado do site do censo e importando o csv específico

df_censo_2022_raca <- read_excel("fonte_dados_IBGE/censo_2022/Tabela_02_cor_ou_raca.xlsx",
                                 range = "D10:I10",
                                 col_names = c(
                                         'Branca',
                                         'Preta',
                                         'Amarela',
                                         'Parda',
                                         'Indígena',
                                         'Ignorados'
                                 )) %>% stack()

colnames(df_censo_2022_raca) <- c("QTD", "COR_RACA_ETNIA")

df_censo_2022_raca$QTD <- as.numeric(gsub(" ", "", df_censo_2022_raca$QTD))


# Tratando para padronizar com as análises que serão realizadas

df_censo_2022_raca <- 
        df_censo_2022_raca %>% 
        filter(COR_RACA_ETNIA != "Ignorados") %>%
        mutate(PCT = (QTD/ sum(QTD)) * 100,
               FONTE = "CENSO_2022")


# Armazenando em tabela com as qtds para posterior consulta

aux_qtd_por_raca <- df_censo_2022_raca


# Padronizando a tabela do censo de acordo com os critérios da visão

df_censo_2022_raca <- df_censo_2022_raca %>%
        mutate(COR_RACA_ETNIA_AGRUP = case_when(COR_RACA_ETNIA %in% 
                                                        c("Amarela", 
                                                          "Indígena") ~ "Outras",
                                                TRUE ~ COR_RACA_ETNIA)) %>%
        group_by(COR_RACA_ETNIA_AGRUP, FONTE) %>%
        summarise(QTD = sum(QTD), PCT = sum(PCT), .groups = "drop") %>%
        select(COR_RACA_ETNIA_AGRUP, QTD, PCT, FONTE)


# Juntando os dois df's

df_raca_e_censo <- rbind(df_raca, df_censo_2022_raca)


# Ordenando a exibição da "Fonte"

df_raca_e_censo$FONTE <- factor(df_raca_e_censo$FONTE, 
                                levels = c("PESQUISA_DADOS", "CENSO_2022"))


# Criando o gráfico

plot01 <- ggplot(df_raca_e_censo, aes(x = reorder(COR_RACA_ETNIA_AGRUP, -PCT),
                                      y = PCT,
                                      fill = FONTE)) +
        geom_bar(stat = "identity", position = "dodge") +
        geom_text(aes(label = sprintf("%.2f%%", PCT)),
                  position = position_dodge(width = 0.9),
                  vjust = -0.5,
                  hjust = 0.5,
                  size = 6,
                  fontface = "bold") + 
        
        labs(x = "Cor/Raça",
             y = "Percentual",
             title = "Gráfico 1 - Distribuição por Cor ou Raça",
             subtitle = "Pesquisa State of Data Brazil 2023 e Censo IBGE 2022",
             caption = "Nota: Para os dados da pesquisa, desconsidera respondentes que não informaram cor/raça ou gênero binário (masculino ou feminino)."
        ) +
        
        ylim(0, max(df_raca$PCT) * 1.2) +
        
        theme_minimal() +
        theme(
                plot.title = element_text(size = 18, hjust = 0.5, 
                                          margin = margin(t = 5),
                                          face = "bold"), 
                plot.subtitle = element_text(size = 16, hjust = 0.5, 
                                             margin = margin(t = 3)),
                
                axis.text.y = element_blank(),
                axis.text.x = element_text(size = 15, margin = margin(t = -5)),
                axis.ticks.y = element_blank(),
                axis.title.x = element_text(size = 15, margin = margin(t = 15),
                                            face = "bold"),
                axis.title.y = element_text(size = 15, margin = margin(r = 10),
                                            face = "bold"),
                legend.text = element_text(size = 13),
                legend.position = "bottom",
                legend.title = element_blank(),
                plot.caption = element_text(size = 12, hjust = 0,
                                            margin = margin(t = 15)),
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank()
        ) +
        scale_fill_manual(values = c("PESQUISA_DADOS" =  "#111D4A",
                                     "CENSO_2022" = "#D05167"),
                          labels = c("Pesquisa State of Data Brazil 2023",
                                     "Censo IBGE 2022"))


ggsave(filename = "resultados_analise/plot01.png",
       plot = plot01,
       width = 12, height = 6, dpi = 300, bg = "white")




## Gráfico 02: Participação de gênero

# Calculando a intersecção de raça e gênero na pesquisa state of data

df_genero <- df_analise_exploracao %>%
        filter(COR_RACA_ETNIA != "Prefiro não informar",
               GENERO %in% c("Feminino", "Masculino")) %>%
        group_by(COR_RACA_ETNIA_AGRUP, GENERO) %>%
        summarise(QTD = n(), .groups = "drop") %>%
        mutate(PCT = (QTD/ sum(QTD)) * 100,
               FONTE = "PESQUISA_DADOS")


# Arquivo dos números gerais do CENSO 2022 sobre genero

df_censo_2022_genero <- read_excel("fonte_dados_IBGE/censo_2022/Tabela_05_genero_por_cor_ou_raca.xlsx",
                                   range = "D7:H8") %>% stack()

colnames(df_censo_2022_genero) <- c("RAZAO", "COR_RACA_ETNIA")

df_censo_2022_genero$RAZAO <- as.numeric(gsub(" ", "", df_censo_2022_genero$RAZAO))


# Padronizando de acordo com as outras análises e cruzando para obter o número
# absoluto por cor/raça

df_censo_2022_genero <- df_censo_2022_genero %>%
        mutate(COR_RACA_ETNIA_AGRUP = 
                       case_when(COR_RACA_ETNIA %in% c("Amarela",
                                                       "Indígena") ~ "Outras",
                                 TRUE ~ COR_RACA_ETNIA)) %>%
        left_join(aux_qtd_por_raca %>% select(COR_RACA_ETNIA, 
                                              QTD),
                  by = "COR_RACA_ETNIA")


# Calculando a quantidade por gênero através da RAZAO

df_censo_2022_genero <- df_censo_2022_genero %>%
        mutate(Feminino = (QTD * 100) / (RAZAO + 100),
               Masculino = QTD - Feminino) %>%
        pivot_longer(cols = c(Masculino, Feminino), 
                     names_to = "GENERO", 
                     values_to = "QTD_GENERO")


# Agrupando Amarelo e Indígena em 'Outras'

df_censo_2022_genero <- df_censo_2022_genero %>%
        mutate(PCT = (QTD_GENERO/ sum(QTD_GENERO)) * 100,
               FONTE = "CENSO_2022") %>%
        group_by(COR_RACA_ETNIA_AGRUP, GENERO, FONTE) %>%
        summarise(QTD = sum(QTD),
                  PCT = sum(PCT), .groups = 'drop') %>%
        select(COR_RACA_ETNIA_AGRUP, GENERO, QTD, PCT, FONTE)


# Juntando os dataframes da pesquisa e do censo

df_genero_e_censo <- rbind(df_genero, df_censo_2022_genero) %>%
        mutate(GENERO_E_RACA = paste0(COR_RACA_ETNIA_AGRUP, " (", GENERO, ")"))


# Visão geral de gênero comparando pesquisa e IBGE

df_genero_e_censo_geral <- 
        df_genero_e_censo %>%
        group_by(GENERO, FONTE) %>%
        summarise(PCT = sum(PCT), .groups = "drop")


# Ordenando o gênero

df_genero_e_censo_geral$GENERO <- factor(df_genero_e_censo_geral$GENERO,
                                         levels = c("Masculino",
                                                    "Feminino"))


# Ordenando a "Fonte"

df_genero_e_censo_geral$FONTE <- factor(df_genero_e_censo_geral$FONTE,
                                        levels = c("PESQUISA_DADOS",
                                                   "CENSO_2022"))


# Criando o gráfico

plot02 <- ggplot(df_genero_e_censo_geral, aes(x = GENERO,
                                              y = PCT,
                                              fill = FONTE)) +
        geom_bar(stat = "identity", position = "dodge",
                 width = 0.5) +
        geom_text(aes(label = sprintf("%.2f%%", PCT)), 
                  position = position_dodge(width = 0.5),
                  vjust = -0.5,
                  hjust = 0.5,
                  size = 6,
                  fontface = "bold") +
        
        labs(x = "Gênero",
             y = "Percentual",
             title = "Gráfico 2 - Distribuição por Gênero",
             subtitle = "Pesquisa State of Data Brazil 2023 e Censo IBGE 2022",
             caption = "Nota: Para os dados da pesquisa, desconsidera respondentes que não informaram cor/raça ou gênero binário (masculino ou feminino).") +
        
        ylim(0, max(df_genero_e_censo_geral$PCT) * 1.2) +
        theme_minimal() +
        theme(
                plot.title = element_text(size = 18, hjust = 0.5, 
                                          margin = margin(t = 5),
                                          face = "bold"), 
                plot.subtitle = element_text(size = 16, hjust = 0.5, 
                                             margin = margin(t = 3)),
                
                axis.text.y = element_blank(),
                axis.text.x = element_text(size = 15, margin = margin(t = -5)),
                axis.ticks.y = element_blank(),
                axis.title.x = element_text(size = 15, margin = margin(t = 15),
                                            face = "bold"),
                axis.title.y = element_text(size = 15, margin = margin(r = 10),
                                            face = "bold"),
                legend.text = element_text(size = 13),
                legend.position = "bottom",
                legend.title = element_blank(),
                plot.caption = element_text(size = 12, hjust = 0,
                                            margin = margin(t = 15)),
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank()
                
        ) +
        scale_fill_manual(values = c("PESQUISA_DADOS" = "#111D4A",
                                     "CENSO_2022" = "#D05167"),
                          labels = c("Pesquisa State of Data Brazil 2023",
                                     "Censo IBGE 2022"))


ggsave(filename = "resultados_analise/plot02.png",
       plot = plot02,
       width = 12, height = 6, dpi = 300, bg = "white")




## Gráfico 03: Tratar sobre a interseccionalidade entre raça e gênero

# Criando o df para essa análise

df_genero_e_censo_detalhado <- df_genero_e_censo


# Df com o cálculo do Total para ordenar a visão

df_genero_e_censo_ordem <- df_genero_e_censo %>%
        group_by(GENERO_E_RACA) %>%
        summarise(TOTAL_ORDENAR = sum(PCT)) %>%
        arrange(desc(TOTAL_ORDENAR))


# Ordenando o df pela quantidade total do genero e raça

df_genero_e_censo_detalhado$GENERO_E_RACA <-
        factor(df_genero_e_censo_detalhado$GENERO_E_RACA, 
               levels = df_genero_e_censo_ordem$GENERO_E_RACA)


# Ordenando a "Fonte"

df_genero_e_censo_detalhado$FONTE <-
        factor(df_genero_e_censo_detalhado$FONTE, 
               levels = c("PESQUISA_DADOS", "CENSO_2022"))


# Criando o gráfico

plot03 <- ggplot(filter(df_genero_e_censo_detalhado,
                        !grepl("Outras", GENERO_E_RACA)),
                 aes(x = GENERO_E_RACA,
                     y = PCT,
                     fill = FONTE)) +
        geom_bar(stat = "identity", position = "dodge") +
        geom_text(aes(label = sprintf("%.2f%%", PCT)), 
                  position = position_dodge(width = 1),
                  vjust = -0.5,
                  hjust = 0.5,
                  size = 5,
                  fontface = "bold") +
        
        labs(x = "Cor/Raça e Gênero",
             y = "Percentual",
             title = "Gráfico 3 - Distribuição por Cor/Raça e Gênero",
             subtitle = "Pesquisa State of Data Brazil 2023 e Censo IBGE 2022",
             caption = "Nota: Para os dados da pesquisa, desconsidera respondentes que não informaram cor/raça ou gênero binário (masculino ou feminino).") +
        
        ylim(0, max(df_genero_e_censo_detalhado$PCT) * 1.2) +
        theme_minimal() +
        theme(
                plot.title = element_text(size = 18, hjust = 0.5, 
                                          margin = margin(t = 5),
                                          face = "bold"), 
                plot.subtitle = element_text(size = 16, hjust = 0.5, 
                                             margin = margin(t = 3)),
                
                axis.text.y = element_blank(),
                axis.text.x = element_text(size = 14, margin = margin(t = -5)),
                axis.ticks.y = element_blank(),
                axis.title.x = element_text(size = 15, margin = margin(t = 15),
                                            face = "bold"),
                axis.title.y = element_text(size = 15, margin = margin(r = 10),
                                            face = "bold"),
                legend.text = element_text(size = 13),
                legend.position = "bottom",
                legend.title = element_blank(),
                plot.caption = element_text(size = 12, hjust = 0,
                                            margin = margin(t = 15)),
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
        ) +
        scale_fill_manual(values = c("PESQUISA_DADOS" =  "#111D4A",
                                     "CENSO_2022" = "#D05167"),
                          labels = c("Pesquisa State of Data Brazil 2023",
                                     "Censo IBGE 2022"))


ggsave(filename = "resultados_analise/plot03.png",
       plot = plot03,
       width = 12, height = 6, dpi = 300, bg = "white")




## Gráfico 04: Nível de Formação - Gênero Feminino

# Criando df, filtrando para refletir os respondentes em todos âmbitos

df_ensino <- df_analise_exploracao %>%
        filter(NIVEL_DE_ENSINO != "Prefiro não informar",
               GENERO %in% c("Feminino", "Masculino"),
               COR_RACA_ETNIA != "Prefiro não informar") %>%
        group_by(NIVEL_DE_ENSINO, GENERO, COR_RACA_ETNIA_AGRUP) %>%
        summarise(QTD = n(), .groups = "drop") %>%
        group_by(NIVEL_DE_ENSINO) %>%
        mutate(PCT_ENSINO = (QTD/ sum(QTD)) * 100) %>%
        ungroup()


# Renomeando para diminuir o texto no gráfico

df_ensino <- df_ensino %>%
        mutate(NIVEL_DE_ENSINO = recode(NIVEL_DE_ENSINO,
                                        "Não tenho graduação formal" =
                                                "Sem Graduação",
                                        "Graduação/Bacharelado" = 
                                                "Graduação"))


# Ordenando o nível de ensino

df_ensino$NIVEL_DE_ENSINO <- factor(df_ensino$NIVEL_DE_ENSINO,
                                    levels = c("Sem Graduação",
                                               "Estudante de Graduação",
                                               "Graduação",
                                               "Pós-graduação",
                                               "Mestrado",
                                               "Doutorado ou Phd"))


# Ordenando por Cor/Raça

df_ensino$COR_RACA_ETNIA_AGRUP <- factor(df_ensino$COR_RACA_ETNIA_AGRUP,
                                         levels = c("Branca",
                                                    "Parda",
                                                    "Preta",
                                                    "Outras"))


# Adicionando a coluna de cor do texto ao dataframe

df_ensino$CORES_TEXTO <- ifelse(df_ensino$COR_RACA_ETNIA_AGRUP == "Parda",
                                "#FFFFFF", "#000000")


# Filtro e total para o gênero feminino 

df_ensino_feminino <- df_ensino %>%
        filter(GENERO == "Feminino") %>%
        group_by(NIVEL_DE_ENSINO) %>%
        mutate(TOTAL = sum(PCT_ENSINO))


# Cores das barras que envolvem cor/raça

plot_cores_raca <- c("Branca" = "#D05167",  # Azul
                     "Parda" = "#111D4A",   # Laranja
                     "Preta" = "#5299D3",    # Verde
                     "Outras" = "#827EAB")    # Vermelho


# Criando o gráfico

plot04 <- ggplot(df_ensino_feminino, aes(x = NIVEL_DE_ENSINO,
                                         y = PCT_ENSINO,
                                         fill = COR_RACA_ETNIA_AGRUP)) +
        
        geom_bar(stat = "identity", width = 0.6) +
        
        geom_text(aes(label = sprintf("%.2f%%", PCT_ENSINO)),
                  position = position_stack(vjust = 0.5), 
                  size = 5,
                  fontface = "bold",
                  color = df_ensino_feminino$CORES_TEXTO) +
        
        geom_text(data = df_ensino_feminino, aes(x = NIVEL_DE_ENSINO,
                                                 y = TOTAL,
                                                 label = sprintf("Total: %.2f%%",
                                                                 TOTAL)),
                  vjust = -0.5,
                  size = 5, 
                  color = "#8A817C") +
        
        
        labs(x = "Nível de Formação",
             y = "Percentual",
             title = "Gráfico 4 - Distribuição de Cor/Raça por Nível de Formação - Gênero Feminino",
             subtitle = "Pesquisa State of Data Brazil 2023",
             caption = "Nota: Desconsidera respondentes que não informaram nível de formação, cor/raça ou gênero binário (masculino ou feminino).") +
        
        scale_fill_manual(values = plot_cores_raca) +
        
        scale_y_continuous(limits = c(0, df_ensino_feminino$TOTAL * 1.05)) +
        
        theme_minimal() +
        
        theme(
                plot.title = element_text(size = 18, hjust = 0.5, 
                                          margin = margin(t = 5),
                                          face = "bold"), 
                plot.subtitle = element_text(size = 16, hjust = 0.5, 
                                             margin = margin(t = 3)),
               
                axis.text.y = element_blank(),
                axis.text.x = element_text(size = 13, margin = margin(t = -5)),
                axis.ticks.y = element_blank(),
                axis.title.x = element_text(size = 15, margin = margin(t = 15),
                                            face = "bold"),
                axis.title.y = element_text(size = 15, margin = margin(r = 10),
                                            face = "bold"),
                
                legend.text = element_text(size = 13),
                legend.position = "bottom",
                legend.title = element_blank(),
                plot.caption = element_text(size = 12, hjust = 0,
                                            margin = margin(t = 15)),
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank())


ggsave(filename = "resultados_analise/plot04.png",
       plot = plot04,
       width = 12, height = 6, dpi = 300, bg = "white")




## Gráfico 05: Nível de Formação - Gênero Masculino

# Filtro e total para o gênero masculino 

df_ensino_masculino <- df_ensino %>%
        filter(GENERO == "Masculino") %>%
        group_by(NIVEL_DE_ENSINO) %>%
        mutate(TOTAL = sum(PCT_ENSINO))


# Criando o gráfico

plot05 <- ggplot(df_ensino_masculino, aes(x = NIVEL_DE_ENSINO,
                                          y = PCT_ENSINO,
                                          fill = COR_RACA_ETNIA_AGRUP)) +
        
        geom_bar(stat = "identity", width = 0.6) +
        
        geom_text(aes(label = sprintf("%.2f%%", PCT_ENSINO)),
                  position = position_stack(vjust = 0.5), 
                  size = 5,
                  fontface = "bold",
                  color = df_ensino_masculino$CORES_TEXTO) +
        
        geom_text(data = df_ensino_masculino, aes(x = NIVEL_DE_ENSINO,
                                                  y = TOTAL,
                                                  label = sprintf("Total: %.2f%%",
                                                                  TOTAL)),
                  vjust = -0.5,
                  size = 5, 
                  color = "#8A817C") +
        
        
        labs(x = "Nível de Formação",
             y = "Percentual",
             title = "Gráfico 5 - Distribuição de Cor/Raça por Nível de Formação - Gênero Masculino",
             subtitle = "Pesquisa State of Data Brazil 2023",
             caption = "Nota: Desconsidera respondentes que não informaram nível de formação, cor/raça ou gênero binário (masculino ou feminino).") +
        
        scale_fill_manual(values = plot_cores_raca) +
        
        scale_y_continuous(limits = c(0, df_ensino_masculino$TOTAL * 1.3)) + 
        
        theme_minimal() +
        
        theme(
                plot.title = element_text(size = 18, hjust = 0.5, 
                                          margin = margin(t = 5),
                                          face = "bold"), 
                plot.subtitle = element_text(size = 16, hjust = 0.5, 
                                             margin = margin(t = 3)),
                
                axis.text.y = element_blank(),
                axis.text.x = element_text(size = 13, margin = margin(t = -5)),
                axis.ticks.y = element_blank(),
                axis.title.x = element_text(size = 15, margin = margin(t = 15),
                                            face = "bold"),
                axis.title.y = element_text(size = 15, margin = margin(r = 10),
                                            face = "bold"),
                
                legend.text = element_text(size = 13),
                legend.position = "bottom",
                legend.title = element_blank(),
                plot.caption = element_text(size = 12, hjust = 0,
                                            margin = margin(t = 15)),
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank())


ggsave(filename = "resultados_analise/plot05.png",
       plot = plot05,
       width = 12, height = 6, dpi = 300, bg = "white")




## Gráfico 06: Frequência Escolar no Ensino Superior

# Dados do IBGE, da PNAD

# Extrair o arquivo .xls para um diretório temporário

unzip("fonte_dados_IBGE/estatisticas_genero/2_Educacao_xls.zip",
      files = "Tabela 2.2.4.xls",
      exdir = tempdir())


# Construir o caminho completo do arquivo extraído

xls_path <- file.path(tempdir(), "Tabela 2.2.4.xls")


# Ler o arquivo .xls

df_dados_ibge_formacao <- read_excel(xls_path,
                                     range = "D8:H8",
                                     col_names = c(
                                             'Branca (Masculino)',
                                             'Negra (Masculino)',
                                             'Excluir',
                                             'Branca (Feminino)',
                                             'Negra (Feminino)'
                                     ))


# Excluindo a coluna de Total, que não será utilizada

df_dados_ibge_formacao <- df_dados_ibge_formacao %>%
        select(-Excluir)


# Pivotar a tabela, transformando as colunas em linhas

df_dados_ibge_formacao <- df_dados_ibge_formacao %>%
        pivot_longer(
                cols = everything(),    # Seleciona todas as colunas para transformar
                names_to = "COR_RACA_ETNIA",    # Nome da nova coluna para os nomes originais das colunas
                values_to = "TAXA_FREQUENCIA" # Nome da nova coluna para os valores originais das colunas
        ) %>%
        arrange(TAXA_FREQUENCIA) # Ordenando


# Ordenando por Cor/Raça

df_dados_ibge_formacao$COR_RACA_ETNIA <- factor(
        df_dados_ibge_formacao$COR_RACA_ETNIA,
        levels = df_dados_ibge_formacao$COR_RACA_ETNIA)


# Criando o gráfico

plot06 <- ggplot(df_dados_ibge_formacao, aes(x = TAXA_FREQUENCIA,
                                             y = COR_RACA_ETNIA)) +
        
        geom_bar(stat = "identity", width = 0.6, fill = "#223C6D") +
        
        geom_text(aes(label = sprintf("%.2f%%", TAXA_FREQUENCIA)), 
                  position = position_dodge(width = 0.9),
                  vjust = 0.5,
                  hjust = -0.1,
                  size = 6,
                  fontface = "bold") +
        
        
        labs(x = "Percentual",
             y = "Cor/Raça e Gênero",
             title = "Gráfico 6 - Taxa Ajustada de Frequência Escolar Líquida no Ensino Superior",
             subtitle = "Pesquisa Nacional por Amostra de Domicílios Contínua de 2022") +
        
        xlim(0, max(df_dados_ibge_formacao$TAXA_FREQUENCIA) * 1.2) +
        
        theme_minimal() +
        
        theme(
                plot.title = element_text(size = 18, hjust = 0.5, 
                                          margin = margin(t = 5),
                                          face = "bold"), 
                plot.subtitle = element_text(size = 16, hjust = 0.5, 
                                             margin = margin(t = 3)),
                
                axis.text.y = element_text(size = 13, margin = margin(l = 15,
                                                                      r = -30)),
                axis.text.x = element_blank(),
                axis.ticks.y = element_blank(),
                axis.title.x = element_text(size = 15, margin = margin(t = 15),
                                            face = "bold"),
                axis.title.y = element_text(size = 15, margin = margin(r = 10),
                                            face = "bold"),
                legend.position = "none",
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank())


ggsave(filename = "resultados_analise/plot06.png",
       plot = plot06,
       width = 12, height = 6, dpi = 300, bg = "white")




## Gráfico 07: Percentual que se sente prejudicado devido a cor/raça

df_aspectos_prejudicados_geral <- df_analise_exploracao %>%
        filter(GENERO %in% c('Masculino', 'Feminino'),
               COR_RACA_ETNIA != "Prefiro não informar",
               !QUAL_SUA_SITUACAO_ATUAL_DE_TRABALHO %in% 
                       c("Desempregado e não estou buscando recolocação", 
                         "Somente Estudante (pós-graduação)",
                         "Somente Estudante (graduação)",
                         "Prefiro não informar",
                         "Freelancer")) %>%
        
        group_by(GENERO_E_RACA) %>%
        
        summarise(QTD_GRUPO = n(),
                  PREJUDICADO_COR = sum(EXPERIENCIA_PREJUDICADA_DEVIDO_A_MINHA_COR_RACA_ETNIA,
                                        na.rm = T) 
                  / QTD_GRUPO,
                  PREJUDICADO_GENERO = sum(EXPERIENCIA_PREJUDICADA_DEVIDO_A_MINHA_IDENTIDADE_DE_GENERO,
                                           na.rm = T)
                  / QTD_GRUPO)


# Ordenando o dataframe pela coluna 'PREJUDICADO_COR' em ordem descendente

df_aspectos_prejudicados_geral <- df_aspectos_prejudicados_geral %>%
        arrange(desc(PREJUDICADO_COR))


# Convertendo a coluna 'GENERO_E_RACA' em fator com os níveis na ordem de 'PREJUDICADO_COR'

df_aspectos_prejudicados_geral <- df_aspectos_prejudicados_geral %>%
        mutate(GENERO_E_RACA = factor(GENERO_E_RACA, levels = unique(GENERO_E_RACA)))


# Criando o gráfico

plot07 <- ggplot(filter(df_aspectos_prejudicados_geral,
                        !grepl("Branca", GENERO_E_RACA)),
                 
                 aes(x = GENERO_E_RACA, y = PREJUDICADO_COR)) +
        
        geom_bar(stat = "identity", width = 0.6, fill = "#223C6D") +
        
        geom_text(aes(label = paste0(round(PREJUDICADO_COR * 100, 1), "%")), 
                  position = position_dodge(width = 0.9),
                  vjust = -0.5,
                  hjust = 0.5,
                  size = 6,
                  fontface = "bold") +
        
        
        labs(x = "Cor/Raça e Gênero",
             y = "Percentual",
             title = "Gráfico 7 -  Experiência Profissional Prejudicada Devido a Cor/Raça",
             subtitle = "Pesquisa State of Data Brazil 2023",
             caption = "Nota: Desconsidera freelancers, estudantes, desempregados que não buscam recolocação e pessoas que não informaram situação trabalhista, cor/raça ou
           gênero binário (masculino ou feminino).") +
        
        ylim(0, max(df_aspectos_prejudicados_geral$PREJUDICADO_COR) * 1.2) +
        
        theme_minimal() +
        
        theme(
                plot.title = element_text(size = 18, hjust = 0.5, 
                                          margin = margin(t = 5),
                                          face = "bold"), 
                plot.subtitle = element_text(size = 16, hjust = 0.5, 
                                             margin = margin(t = 3)),
                
                axis.text.x = element_text(size = 13, margin = margin(t = -5)),
                axis.text.y = element_blank(),
                axis.ticks.y = element_blank(),
                axis.title.x = element_text(size = 15, margin = margin(t = 15),
                                            face = "bold"),
                axis.title.y = element_text(size = 15, margin = margin(r = 10),
                                            face = "bold"),
                legend.position = "none",
                plot.caption = element_text(size = 12, hjust = 0,
                                            margin = margin(t = 15)),
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank())


ggsave(filename = "resultados_analise/plot07.png",
       plot = plot07,
       width = 12, height = 6, dpi = 300, bg = "white")




## Gráfico 08 - Aspectos Prejudicados devido a cor/raça

# Criando a tabela de aspectos prejudicados devido a cor/raça
# Renomeando as colunas para rotular o gráfico melhor

df_negros_prejudicados_cor <- df_analise_exploracao %>%
        filter(GENERO %in% c('Masculino', 'Feminino'),
               COR_RACA_ETNIA_AGRUP %in% c('Parda', 'Preta'),
               EXPERIENCIA_PREJUDICADA_DEVIDO_A_MINHA_COR_RACA_ETNIA == 1,
               !QUAL_SUA_SITUACAO_ATUAL_DE_TRABALHO %in% 
                       c("Desempregado e não estou buscando recolocação", 
                         "Somente Estudante (pós-graduação)",
                         "Somente Estudante (graduação)",
                         "Prefiro não informar",
                         "Freelancer")) %>%
        group_by(GENERO_E_RACA) %>%
        summarise(QTD = n(),
                  'Quantidade de vagas recebidas'
                  = sum(QUANTIDADE_DE_OPORTUNIDADES_DE_EMPREGO_VAGAS_RECEBIDAS, 
                        na.rm = T) / QTD,
                  'Senioridade das vagas em relação a experiência' = sum(SENIORIDADE_DAS_VAGAS_RECEBIDAS_EM_RELACAO_A_SUA_EXPERIENCIA,
                                                                         na.rm = T) / QTD,
                  'Aprovação em processo seletivos' = sum(APROVACAO_EM_PROCESSOS_SELETIVOS_ENTREVISTAS,
                                                          na.rm = T) / QTD,
                  'Progressão de carreira' = sum(OPORTUNIDADES_DE_PROGRESSAO_DE_CARREIRA,
                                                 na.rm = T) / QTD,
                  'Velocidade de progressão de carreira' = sum(VELOCIDADE_DE_PROGRESSAO_DE_CARREIRA,
                                                               na.rm = T) / QTD,
                  'Cobrança no Trabalho' = sum(NIVEL_DE_COBRANCA_NO_TRABALHO_STRESS_NO_TRABALHO,
                                               na.rm = T) / QTD,
                  'Atenção diante das opiniões' = sum(ATENCAO_DADA_DIANTE_DAS_MINHAS_OPINIOES_E_IDEIAS,
                                                      na.rm = T) / QTD,
                  'Relação com membros da empresa' = sum(RELACAO_COM_OUTROS_MEMBROS_DA_EMPRESA,
                                                         na.rm = T) / QTD,
                  .groups = "drop")


# Pivotando essa tabela

df_negros_prejudicados_cor <- df_negros_prejudicados_cor %>%
        select(-QTD) %>%
        pivot_longer(
                cols = -GENERO_E_RACA, # Todas as colunas exceto 'GENERO_E_RACA'
                names_to = "ASPECTO", # Nome da nova coluna que conterá os nomes das variáveis
                values_to = "PCT_PREJUDICADO") # Nome da nova coluna que conterá os valores


# Criando uma variável para a escala de cor de cada Gênero e Raça

df_negros_prejudicados_cor <- df_negros_prejudicados_cor %>%
        group_by(GENERO_E_RACA) %>%
        mutate(PCT_FILL = scales::rescale(PCT_PREJUDICADO, to = c(0, 1)))


# Criando o gráfico

plot08 <- ggplot(df_negros_prejudicados_cor, aes(x = str_wrap(ASPECTO, width = 16),
                                                 y = PCT_PREJUDICADO,
                                                 fill = PCT_FILL)) +
        geom_bar(stat = "identity") +
        geom_text(aes(label = paste0(round(PCT_PREJUDICADO * 100, 1), "%")), 
                  position = position_dodge(width = 0.9),
                  vjust = -0.5, 
                  hjust = 0.6,
                  size = 5, 
                  fontface = "bold") + 
        
        facet_wrap(~ GENERO_E_RACA, ncol = 1, scales = "free") +
        
        scale_fill_gradient(low = "#CCD9E8", high = "#223C6D") + 
        
        labs(x = "Aspecto Prejudicado", 
             y = "Percentual", 
             title = "Gráfico 8 - Aspectos da Experiência Profissional Prejudicados Devido a Cor/Raça", 
             subtitle = "Pesquisa State of Data Brazil 2023",
             caption = "Nota: Desconsidera freelancers, estudantes, desempregados que não buscam recolocação e pessoas que não informaram situação trabalhista, cor/raça ou
           gênero binário (masculino ou feminino).") +
        
        theme_minimal() +
        
        theme(
                plot.title = element_text(size = 18, hjust = 0.5, 
                                          margin = margin(t = 5),
                                          face = "bold"), 
                plot.subtitle = element_text(size = 16, hjust = 0.5, 
                                             margin = margin(t = 3,
                                                             b = 5)),
                axis.text.y = element_blank(), 
                axis.text.x = element_text(size = 12, margin = margin(t = 0)),
                axis.ticks.y = element_blank(), 
                axis.title.x = element_text(size = 15, margin = margin(t = 10),
                                            face = "bold"),
                axis.title.y = element_text(size = 15, margin = margin(r = 10),
                                            face = "bold"), 
                legend.position = "none",
                plot.caption = element_text(size = 12, hjust = 0,
                                            margin = margin(t = 10)),
                panel.grid.major = element_blank(), 
                panel.grid.minor = element_blank(),
                strip.text = element_text(size = 15, color = "#111D4A", 
                                          face = "bold",
                                          margin = margin(t = 0)),
                strip.text.x = element_text(size = 12),
                strip.background = element_blank(), 
                panel.spacing = unit(1, "lines")) +  
        expand_limits(y = max(df_negros_prejudicados_cor$PCT_PREJUDICADO) * 1.2)


ggsave(filename = "resultados_analise/plot08.png",
       plot = plot08,
       width = 12, height = 9, dpi = 300, bg = "white")




## Gráfico 09: Cargos por Raça/Cor

# Analisando o cargo de pessoas CLT que informaram cor/raça.

df_nivel_cargo <- df_analise_exploracao %>%
        filter(QUAL_SUA_SITUACAO_ATUAL_DE_TRABALHO == 'Empregado (CLT)',
               COR_RACA_ETNIA != "Prefiro não informar",
               GENERO %in% c("Feminino", "Masculino")) %>%
        group_by(COR_RACA_ETNIA_AGRUP, NIVEL_TRAT) %>%
        summarise(QTD = n(), .groups = 'drop')


# Calculando o total por nível do cargo

df_totais_nivel <- df_nivel_cargo %>%
        group_by(NIVEL_TRAT) %>%
        summarise(TOTAL_NIVEL = sum(QTD), .groups = 'drop')


# Calculando o percentual da raça por nível do cargo

df_nivel_cargo <- df_nivel_cargo %>%
        left_join(df_totais_nivel, by = "NIVEL_TRAT") %>%
        mutate(PCT_NIVEL = (QTD / TOTAL_NIVEL) * 100)


# Adicionando a coluna de cor do texto ao dataframe

df_nivel_cargo$CORES_TEXTO <- ifelse(df_nivel_cargo$COR_RACA_ETNIA_AGRUP == "Branca",
                                     "#000000", "#000000")


# Ordenando por nível do cargo

df_nivel_cargo$NIVEL_TRAT <- factor(df_nivel_cargo$NIVEL_TRAT,
                                    levels = c("Júnior", "Pleno",
                                               "Sênior", "Gestor"))


# Ordenando por cor/raça

df_nivel_cargo$COR_RACA_ETNIA_AGRUP <-
        factor(df_nivel_cargo$COR_RACA_ETNIA_AGRUP,
               levels = c("Branca", "Parda", "Preta", "Outras"))


# Adicionando a coluna de cor do texto ao dataframe

df_nivel_cargo$CORES_TEXTO <- ifelse(df_nivel_cargo$COR_RACA_ETNIA_AGRUP == "Parda",
                                     "#FFFFFF", "#000000")


# Criando o gráfico

plot09 <- ggplot(df_nivel_cargo, aes(x = NIVEL_TRAT,
                                     y = PCT_NIVEL,
                                     fill = COR_RACA_ETNIA_AGRUP)) +
        
        geom_bar(stat = "identity", width = 0.6) +
        
        geom_text(aes(label = sprintf("%.2f%%", PCT_NIVEL)),
                  position = position_stack(vjust = 0.5), 
                  size = 5,
                  fontface = "bold",
                  color = df_nivel_cargo$CORES_TEXTO) + 
        
        labs(x = "Nível do Cargo",
             y = "Percentual",
             title = "Gráfico 9 - Distribuição de Cor/Raça por Nível do Cargo",
             subtitle = "Pesquisa State of Data Brazil 2023",
             caption = "Nota: Desconsidera pessoas que não estão na situação de 'Empregado (CLT)', não informaram cor/raça ou gênero binário (masculino ou feminino)."
        ) +
        
        scale_fill_manual(values = plot_cores_raca) +
        
        scale_y_continuous(limits = c(0, df_nivel_cargo$PCT_NIVEL * 1.45)) + 
        
        theme_minimal() +
        
        theme(
                plot.title = element_text(size = 18, hjust = 0.5, 
                                          margin = margin(t = 5),
                                          face = "bold"), 
                plot.subtitle = element_text(size = 16, hjust = 0.5, 
                                             margin = margin(t = 3)),
                
                axis.text.y = element_blank(),
                axis.text.x = element_text(size = 13, margin = margin(t = -5)),
                axis.ticks.y = element_blank(),
                axis.title.x = element_text(size = 15, margin = margin(t = 15),
                                            face = "bold"),
                axis.title.y = element_text(size = 15, margin = margin(r = 10),
                                            face = "bold"),
                
                legend.text = element_text(size = 13),
                legend.position = "bottom",
                legend.title = element_blank(),
                plot.caption = element_text(size = 12, hjust = 0,
                                            margin = margin(t = 15)),
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank())

ggsave(filename = "resultados_analise/plot09.png",
       plot = plot09,
       width = 12, height = 6, dpi = 300, bg = "white")




## Gráfico 10: Liderança por cor/raça comparando com o IBGE

# Extrai o arquivo ZIP para o diretório temporário

unzip("fonte_dados_IBGE/estatisticas_raca/2_Trabalho_Renda_xls.zip",
      exdir = tempdir())


# Selecionando a tabela que será importada

diretorio_ibge_cargos<- grep("Tabela 2.8",
                             list.files(tempdir(), full.names = TRUE),
                             value = TRUE)

# Lendo o excel

df_ibge_cargos_gestao <- read_excel(diretorio_ibge_cargos,
                                    range = "G15:J16",
                                    col_names = c('TOTAL_GENERO',
                                                  'Branca',
                                                  'Preta',
                                                  'Parda')) %>%
        mutate(Outras = 100 - (Branca + Preta + Parda),
               GENERO = c('Masculino', 'Feminino'))


# Pivotando o dataframe

df_ibge_cargos_gestao <- df_ibge_cargos_gestao %>%
        pivot_longer(cols = c('Branca', 'Preta', 'Parda', 'Outras'),
                     names_to = "RACA",
                     values_to = "PCT_GENERO")


# Criando a coluna de Gênero e Raça, e calculando os percentuais por grupo
# Df de cargos de gestão do IBGE

df_ibge_cargos_gestao <- df_ibge_cargos_gestao %>%
        mutate(PCT_GENERO = PCT_GENERO / 100,
               GENERO_E_RACA = paste0(RACA, " (", GENERO, ")"),
               QTD = PCT_GENERO * TOTAL_GENERO,
               PCT = QTD / sum(QTD),
               FONTE = 'IBGE') %>%
        select(GENERO_E_RACA, QTD, PCT, FONTE)


# Df de cargos de gestão da PESQUISA

df_pesquisa_cargos_gestao <- df_analise_exploracao %>%
        filter(QUAL_SUA_SITUACAO_ATUAL_DE_TRABALHO == 'Empregado (CLT)',
               GENERO %in% c('Feminino', 'Masculino'),
               COR_RACA_ETNIA != "Prefiro não informar",
               GESTOR == 1) %>%
        group_by(GENERO_E_RACA) %>%
        summarise(QTD = n(), .groups = 'drop') %>%
        mutate(PCT = QTD / sum(QTD),
               FONTE = 'PESQUISA_DADOS')


# Juntando os df's para comparar os percentuais

df_cargos_gestao <- rbind(df_pesquisa_cargos_gestao, df_ibge_cargos_gestao)


# Ordenando a fonte

df_cargos_gestao$FONTE <- factor(df_cargos_gestao$FONTE,
                                 levels = c('PESQUISA_DADOS', 'IBGE'))


# Ordenando por Gênero e Raça
df_cargos_gestao <- df_cargos_gestao %>%
        arrange(FONTE, desc(PCT))


df_cargos_gestao <- df_cargos_gestao %>%
        mutate(GENERO_E_RACA = factor(GENERO_E_RACA,
                                      levels = unique(GENERO_E_RACA)))

# Criando o gráfico

plot10 <- ggplot(filter(df_cargos_gestao,
                        !grepl("Outras", GENERO_E_RACA)),
                 aes(x = GENERO_E_RACA,
                     y = PCT,
                     fill = FONTE)) +
        geom_bar(stat = "identity", position = "dodge") +
        geom_text(aes(label = sprintf("%.2f%%", PCT * 100)), 
                  position = position_dodge(width = 1),
                  vjust = -0.5,
                  hjust = 0.5,
                  size = 5,
                  fontface = "bold") +
        
        labs(x = "Cor/Raça e Gênero",
             y = "Percentual",
             title = "Gráfico 10 - Distribuição de Cargos Gerenciais por Cor/Raça e Gênero",
             subtitle = "Pesquisa State of Data Brazil 2023 e Dados de Desigualdade IBGE 2021",
             caption = "Nota: Para os dados da pesquisa, desconsidera pessoas que não estão na situação de 'Empregado (CLT)', não informaram cor/raça ou gênero binário
             (masculino ou feminino).") +
        
        ylim(0, max(df_cargos_gestao$PCT) * 1.1) +
        theme_minimal() +
        theme(
                plot.title = element_text(size = 18, hjust = 0.5, 
                                          margin = margin(t = 5),
                                          face = "bold"), 
                plot.subtitle = element_text(size = 16, hjust = 0.5, 
                                             margin = margin(t = 3)),
                
                axis.text.y = element_blank(),
                axis.text.x = element_text(size = 13, margin = margin(t = -5)),
                axis.ticks.y = element_blank(),
                axis.title.x = element_text(size = 15, margin = margin(t = 15),
                                            face = "bold"),
                axis.title.y = element_text(size = 15, margin = margin(r = 10),
                                            face = "bold"),
                legend.text = element_text(size = 13),
                legend.position = "bottom",
                legend.title = element_blank(),
                plot.caption = element_text(size = 12, hjust = 0,
                                            margin = margin(t = 15)),
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
        ) +
        scale_fill_manual(values = c("PESQUISA_DADOS" = "#111D4A",
                                     "IBGE" = "#D05167"),
                          labels = c("Pesquisa State of Data Brazil 2023",
                                     "IBGE"))

ggsave(filename = "resultados_analise/plot10.png",
       plot = plot10,
       width = 12, height = 6, dpi = 300, bg = "white")



## Gráfico 11: Faixa Salarial

# Agrupando as faixas salariais e reescrevendo para melhor leitura do gráfico
# APlicando os filtros necessários e calculando QTD e PCT

df_salario <- df_analise_exploracao %>%
        filter(QUAL_SUA_SITUACAO_ATUAL_DE_TRABALHO == 'Empregado (CLT)',
               GENERO %in% c('Feminino', 'Masculino'),
               COR_RACA_ETNIA_AGRUP != 'Outras') %>%
        
        mutate(FAIXA_SALARIAL_TRAT = coalesce(FAIXA_SALARIAL, ""),
               FAIXA_SALARIAL_AGRUP = 
                       case_when(
                               str_detect(FAIXA_SALARIAL_TRAT,
                                          " 1.000| 2.000") ~ '< R$ 2k/mês',
                               str_detect(FAIXA_SALARIAL_TRAT,
                                          "3.000|4.000") ~ 'R$ 2k/mês - R$ 4k/mês',
                               str_detect(FAIXA_SALARIAL_TRAT,
                                          " 6.000") ~ 'R$ 4k/mês - R$ 6k/mês',
                               str_detect(FAIXA_SALARIAL_TRAT,
                                          " 8.000") ~ 'R$ 6k/mês - R$ 8k/mês',
                               str_detect(FAIXA_SALARIAL_TRAT,
                                          " 12.000") ~ 'R$ 8k/mês - R$ 12k/mês',
                               str_detect(FAIXA_SALARIAL_TRAT,
                                          " 16.000") ~ 'R$ 12k/mês - R$ 16k/mês',
                               TRUE ~ '> 16k/mês'
                       )) %>%
        group_by(GENERO_E_RACA, FAIXA_SALARIAL_AGRUP) %>%
        summarise(QTD = n(), .groups = 'drop') %>%
        group_by(GENERO_E_RACA) %>%
        mutate(PCT_GENERO_RACA = QTD / sum(QTD)) %>%
        ungroup()


# Ordenando a faixa salarial

df_salario$FAIXA_SALARIAL_AGRUP <- factor(df_salario$FAIXA_SALARIAL_AGRUP,
                                          levels = c('> 16k/mês',
                                                     'R$ 12k/mês - R$ 16k/mês',
                                                     'R$ 8k/mês - R$ 12k/mês',
                                                     'R$ 6k/mês - R$ 8k/mês',
                                                     'R$ 4k/mês - R$ 6k/mês',
                                                     'R$ 2k/mês - R$ 4k/mês',
                                                     '< R$ 2k/mês'))

# Ordenando a cor/gênero

df_salario$GENERO_E_RACA <- factor(df_salario$GENERO_E_RACA,
                                   levels = c('Branca (Masculino)',
                                              'Parda (Masculino)',
                                              'Preta (Masculino)',
                                              'Branca (Feminino)',
                                              'Parda (Feminino)',
                                              'Preta (Feminino)'))


# Definindo as cores de cada faixa

plot11_cores_barras <- c("> 16k/mês" = "#111D4A",
                         "R$ 12k/mês - R$ 16k/mês" = "#325B8F",   
                         "R$ 8k/mês - R$ 12k/mês" = "#5299D3",    
                         "R$ 6k/mês - R$ 8k/mês" = "#6A8CBF",
                         "R$ 4k/mês - R$ 6k/mês" = "#827EAB",
                         "R$ 2k/mês - R$ 4k/mês" = "#B16382",
                         "< R$ 2k/mês" = '#F93943')


# Adicionando a coluna de cor do texto ao dataframe

df_salario$CORES_TEXTO <- ifelse(
        df_salario$FAIXA_SALARIAL_AGRUP == "> 16k/mês" |
                df_salario$FAIXA_SALARIAL_AGRUP == "R$ 12k/mês - R$ 16k/mês",
        "#FFFFFF", "#000000")


# Criando o gráfico

plot11 <- ggplot(df_salario, aes(x = GENERO_E_RACA,
                                 y = PCT_GENERO_RACA,
                                 fill = FAIXA_SALARIAL_AGRUP)) +
        
        geom_bar(stat = "identity", width = 0.6) +
        
        geom_text(aes(label = sprintf("%.1f%%", PCT_GENERO_RACA * 100)),
                  position = position_stack(vjust = 0.5), 
                  size = 5,
                  fontface = "bold",
                  color = df_salario$CORES_TEXTO) +
        
        labs(x = "Cor/Raça e Gênero",
             y = "Percentual",
             title = "Gráfico 11 - Distribuição de Faixa Salarial por Cor/Raça",
             subtitle = "Pesquisa State of Data Brazil 2023",
             caption = "Nota: Desconsidera pessoas que não estão na situação de 'Empregado (CLT)'.") +
        
        scale_fill_manual(values = plot11_cores_barras,
                          breaks = rev(levels(df_salario$FAIXA_SALARIAL_AGRUP))) +
        
        theme_minimal() +
        
        theme(
                plot.title = element_text(size = 18, hjust = 0.5, 
                                          margin = margin(t = 5),
                                          face = "bold"), 
                plot.subtitle = element_text(size = 16, hjust = 0.5, 
                                             margin = margin(t = 3,
                                                             b = 7)),
                
                axis.text.y = element_blank(),
                axis.text.x = element_text(size = 13, margin = margin(t = -5)),
                axis.ticks.y = element_blank(),
                axis.title.x = element_text(size = 15, margin = margin(t = 15),
                                            face = "bold"),
                axis.title.y = element_text(size = 15, margin = margin(r = 10),
                                            face = "bold"),
                
                legend.text = element_text(size = 13),
                legend.position = "bottom",
                legend.title = element_blank(),
                plot.caption = element_text(size = 12, hjust = 0,
                                            margin = margin(t = 10)),
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank())

ggsave(filename = "resultados_analise/plot11.png",
       plot = plot11,
       width = 12, height = 6, dpi = 300, bg = "white")





