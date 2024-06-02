#_______________________ Análise de Desempenho Scooby-Doo _________________________#
#_______________________ André Rodrigues ___________________________________#

#____ Analises: ----
#' 1) Número de lançamentos a cada década por formato de lançamento;
#' 2) Variação da nota IMDB por temporada dos episódios;
#' 3) Top 3 terrenos mais frequentes pela ativação da armadilha;
#' 4) Relação entre as notas IMDB e engajamento;
#' 5) Variação da nota de engajamento pelo personagem que conseguiu capturar o monstro.
 
#____ Pacotes ----
library(readr)
library(ggplot2)
library(tidyr)
library(tidyverse)
library(DescTools)
#setwd('C:/Users/André/OneDrive/Documentos/Documents/Trabalho/Projeto-Fantasma')

#____ Bancos ----

banco <- read.csv("C:/Users/André/OneDrive/Documentos/Documents/Trabalho/Projeto-Fantasma/banco/banco_final.csv")

#____ Tema Estat ----
estat_colors <- c(
  "#A11D21", "#003366", "#CC9900",
  "#663333", "#FF6600", "#CC9966",
  "#999966", "#006606", "#008091", 
  "#041835", "#666666" )

theme_estat <- function(...) {
  theme <- ggplot2::theme_bw() +
    ggplot2::theme(
      axis.title.y = ggplot2::element_text(colour = "black", size = 12),
      axis.title.x = ggplot2::element_text(colour = "black", size = 12),
      axis.text = ggplot2::element_text(colour = "black", size = 9.5),
      panel.border = ggplot2::element_blank(),
      axis.line = ggplot2::element_line(colour = "black"),
      legend.position = "top",
      ...
    )
  
  return(
    list(
      theme,
      scale_fill_manual(values = estat_colors),
      scale_colour_manual(values = estat_colors)
    )
  )
}

caminho_andre <- 'resultados'

print_quadro_resumo <- function(data, var_name, title="Medidas resumo da(o) [nome da variável]", label="quad:quadro_resumo1")
{
  var_name <- substitute(var_name)
  data <- data %>%
    summarize(`Média` = round(mean(!!sym(var_name)),2),
              `Desvio Padrão` = round(sd(!!sym(var_name)),2),
              `Variância` = round(var(!!sym(var_name)),2),
              `Mínimo` = round(min(!!sym(var_name)),2),
              `1º Quartil` = round(quantile(!!sym(var_name), probs = .25),2),
              `Mediana` = round(quantile(!!sym(var_name), probs = .5),2),
              `3º Quartil` = round(quantile(!!sym(var_name), probs = .75),2),
              `Máximo` = round(max(!!sym(var_name)),2)) %>%
    t() %>% 
    as.data.frame() %>%
    rownames_to_column()
  
  latex <- str_c("\\begin{quadro}[H]
\t\\caption{", title, "}
\t\\centering
\t\\begin{adjustbox}{max width=\\textwidth}
\t\\begin{tabular}{", sep="")
  
  col_count <- ncol(data)
  row_count <- nrow(data)
  latex <- str_c(latex, "| l |\n", sep=" ")
  for (i in seq(2, col_count))
  {
    numCount <- data[i, -c(1)] %>%
      as.numeric() %>%
      {floor(log10(.)) + 1} %>%
      max()
    latex <- str_c(latex, "\t\t\tS[table-format = ", numCount ,".2]\n", sep="")
  }
  
  
  latex <- str_c(latex, "\t\t\t|}\n\t\\toprule\n\t\t", sep="")
  if (col_count > 2)
  {
    for (i in seq(1,col_count))
    {
      if (i == 1)
        latex <- str_c(latex, "\\textbf{Estatística}", sep="")
      else
        latex <- str_c(latex, " \\textbf{", data[1, i], "}", sep="")
      
      if (i < col_count)
        latex <- str_c(latex, "&", sep=" ")
      else
        latex <- str_c(latex, "\\\\\n", sep=" ")
    }
  }
  else
  {
    latex <- str_c(latex, "\\textbf{Estatística} & \\textbf{Valor} \\\\\n", sep="")  
  }
  
  latex <- str_c(latex, "\t\t\\midrule\n", sep="")
  
  if (col_count > 2)
    starting_number <- 2
  else
    starting_number <- 1
  
  for (i in seq(starting_number, row_count))
  {
    latex <- str_c(latex, "\t\t", str_flatten(t(data[i,]), collapse = " & "), " \\\\\n")
  }
  latex <- str_c(latex, "\t\\bottomrule
\t\\end{tabular}
\t\\label{", label, "}
\t\\end{adjustbox}
\\end{quadro}", sep="")
  
  writeLines(latex)
}

### 1) Numero de Lançamentos ----
# Filtrando o data set
data_e_lançamentos <- banco %>% select(date_aired,format ) %>% mutate(year(date_aired))


#Renomeando A coluna para tornar mais facil
names(data_e_lançamentos)[names(data_e_lançamentos) == 'year(date_aired)'] <- 'Launch_Year'
data_e_lançamentos <- subset(data_e_lançamentos, select = -c(date_aired))
names(data_e_lançamentos)[names(data_e_lançamentos) == 'format'] <- 'Formatos'

#Definindo as décadas 
data_e_lançamentos <- data_e_lançamentos %>% mutate(Decada = case_when(
  Launch_Year >= 1960 & Launch_Year <1970 ~ "60",
  Launch_Year >= 1970 & Launch_Year <1980 ~ "70",
  Launch_Year >= 1980 & Launch_Year <1990 ~ "80",
  Launch_Year >= 1990 & Launch_Year <2000 ~ "90",
  Launch_Year >= 2000 & Launch_Year <2010 ~ "2000",
  Launch_Year >= 2010 & Launch_Year <2020 ~ "2010",
  Launch_Year >= 2020 ~ "2020",
))
data_e_lançamentos <- subset(data_e_lançamentos, select = -c(Launch_Year))

# Recolhendo dados iniciais da quantidade de lançamentos
data_e_lançamentos_Tbl <- table(data_e_lançamentos)

# Agrupando dados iguais para o gráfico
data_e_lançamentos <- data_e_lançamentos %>%
  group_by(Formatos, Decada) %>%
  summarise(n = n())

#Mudando nome das variáveis para português
data_e_lançamentos <- data_e_lançamentos %>%
  mutate(Formatos = recode(Formatos, Movie = 'Filme', CrossOver = 'Crossover', Serie = 'Série'))

#Colocando ordem nos elementos
level_order1 <- c('60', '70', '80', '90', '2000', '2010', '2020')

#Gerando o gráfico final
ggplot(data_e_lançamentos) +
  aes(x = factor(Decada, level = level_order1), y = n , group = Formatos, colour = Formatos) +
  geom_line(size = 1) +
  geom_line(size = 1) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(x = "Décadas", y = "Frequência") +
  theme_estat()
ggsave(file.path(caminho_andre, "numero_de_lançamentos.pdf"), width = 158, height = 93, units = "mm")     

### 2) Variação IMDB ----
#Colocar eixo x episodios y imdb e colocar em grupos na tag das temporadas

df_imdbeTemp <- banco %>% filter(season %in% c(1,2,3,4)) %>% select(season, imdb);

# Mudando nome das variaveis
df_imdbeTemp <- df_imdbeTemp %>%
  mutate(season = recode(season,Special = 'Especial'))
names(df_imdbeTemp)[names(df_imdbeTemp) == 'season'] <- 'Temporada'
df_imdbeTemp <- df_imdbeTemp %>% mutate(Temporada = recode(Temporada, '1' = '1°', '2' = '2°', '3' = '3°', '4' = '4°'))

#Ordenando as variáveis
level_order <- c('1°', '2°', '3°', '4°')

#Blox pot
 ggplot(df_imdbeTemp) +
  aes(x = factor(reorder(Temporada, imdb, FUN = median), level = level_order), y = imdb) +
  geom_boxplot(fill = c("#A11D21"), width = 0.5) +
  stat_summary(
    fun = "mean", geom = "point", shape = 23, size = 3, fill = "#FFFFFF"
  ) +
  labs(x = "Temporada", y = "Nota Imdb") +
  theme_estat()
ggsave(file.path(caminho_andre, "temporada_por_Imdb.pdf"), width = 158, height = 93, units = "mm")

#Vendo variação
ContCoef(df_imdbeTemp$imdb, df_imdbeTemp$Temporada, correct = TRUE)

#Dados para medida resumo
df_imdbeTemp %>% 
  group_by(Temporada) %>%
  print_quadro_resumo(var_name = imdb)

### 3)Top 3 terrenos mais frequentes pela ativação da armadilha ----
#Definindo os dados desejados
terreno_e_armadilha <- banco %>% select(trap_work_first, setting_terrain) %>% 
   filter(trap_work_first == 'True' | trap_work_first == 'False')
table(terreno_e_armadilha)
terreno_e_armadilha <- terreno_e_armadilha %>%
  mutate(setting_terrain = recode(setting_terrain, 'Urban' = 'Urbano','Forest' = 'Floresta', 'Rural' = 'Rural', 'Snow' = 'Neve', 'Island' = 'Ilha', 
                'Swamp' = 'Pântano', 'Coast' = 'Costa', 'Desert' = 'Deserto', 'Cave' = 'Caverna', 'Ocean' = 'Oceano', 'Jungle' = 'Selva', 
                'Mountain' = 'Montanha', 'Air' = 'Aéreo', 'Space' = 'Espaço'))
#Criando gráfico 1 sobre os mais frequentes 
classes <- terreno_e_armadilha %>%
  count (setting_terrain) %>%
  mutate(
    freq = n,
    relative_freq = round((freq / sum(freq)) * 100, 2),
    freq = gsub("\\.", ",", relative_freq) %>% paste("%", sep = ""),
    label = str_c(n, " (", freq, ")") %>% str_squish()
  )
round(classes$relative_freq, 2)
ggplot(classes) +
  aes(x = n, label = label, y = fct_reorder(setting_terrain, n, .desc=F)) +
  geom_bar(stat = "identity", fill = "#A11D21", width = 0.7) +
  geom_text(
    position = position_dodge(width = .9),
    vjust = 0.45, hjust = -0.005, angle = 0,
    size = 2.5
  ) + 
  expand_limits(x = 135) +
  labs(x = "Frequência", y = "Terrenos") +
  theme_estat()
  ggsave(file.path(caminho_andre, "colunas_freqTerreno.pdf") , width = 158, height = 93, units = "mm")

# Filtrando os trêS mais usados em true e false e alterando nome de variaveis
terreno_e_armadilha <- terreno_e_armadilha %>% 
  filter(setting_terrain == 'Urbano' | setting_terrain == 'Rural' | setting_terrain == 'Floresta')
names(terreno_e_armadilha)[names(terreno_e_armadilha) == 'trap_work_first'] <- 'Armadilha_Funcionou' 
terreno_e_armadilha <- terreno_e_armadilha %>%
  mutate(Armadilha_Funcionou = recode(Armadilha_Funcionou, 'True' = 'Sim', 'False' = 'Não'))

#Criando Gráfico das Armadilhas
trans_drv <- terreno_e_armadilha %>%
  mutate(setting_terrain = case_when(
    setting_terrain %>% str_detect("Urbano") ~ 'Urbano',
    setting_terrain %>% str_detect("Rural") ~ "Rural",
    setting_terrain %>% str_detect('Floresta') ~ 'Floresta'
  )) %>%
  group_by(setting_terrain, Armadilha_Funcionou) %>%
  summarise(freq = n()) %>%
  mutate(
    freq_relativa = round(freq / sum(freq) * 100,1)
  )
porcentagens <- str_c(trans_drv$freq_relativa, "%") %>% str_replace("
\\.", ",")

legendas <- str_squish(str_c(trans_drv$freq, " (", porcentagens, ")")
)
ggplot(trans_drv) +
  aes(
    x = fct_reorder(setting_terrain, freq, .desc = T), y = freq,
    fill = Armadilha_Funcionou, label = legendas
  ) +
  geom_col(position = position_dodge2(preserve = "single", padding =
                                        0)) +
  guides(fill = guide_legend(title = 'Armadilha funcionou?')) +
  geom_text(
    position = position_dodge(width = .9),
    vjust = -0.5, hjust = 0.5,
    size = 3
  ) +
  expand_limits(y = 70) +
  labs(x = "Terrenos", y = "Frequência") +
  theme_estat()
ggsave(file.path(caminho_andre, "colunasSimNao.pdf"), width = 158, height = 93, units = "mm")

### 4) Relação entre as notas IMDB e engajamento ----
#Definindo Data Frame
engajamento_imdb <- banco %>% select(engagement, imdb)

#Gráfico de dispersão
ggplot(engajamento_imdb) +
  aes(x = engagement , y = imdb) +
  geom_jitter(width = 0.5, height = 0.5, colour = '#A11D21', alpha = 0.7)+
  labs(
    x = "Classificação de engajamento",
    y = "Nota Imdb"
  ) +
  theme_estat()
ggsave(file.path(caminho_andre, "distribuicao_imdb_engajamento.pdf"), width = 158, height = 93, units = "mm")

#Quadros
engajamento_imdb %>% 
  print_quadro_resumo(var_name = 'engagement')

engajamento_imdb %>% 
  print_quadro_resumo(var_name = 'imdb')

#Coeficiente de pearson
cor(engajamento_imdb$engagement, engajamento_imdb$imdb)



### 5) Variação da nota de engajamento pelo personagem que conseguiu capturar o monstro ----
#Definindo Data Frame
engajamento_captura <- banco %>% select(engagement, caught_daphnie,caught_fred,caught_other,caught_velma,caught_shaggy,caught_scooby, caught_not)
engajamento_captura <- engajamento_captura %>% pivot_longer(cols = c('caught_fred', 'caught_daphnie', 'caught_velma', 'caught_shaggy', 
                                                                     'caught_scooby', 'caught_other', 'caught_not'),
                                                            names_to = 'Quem Capturou',
                                                            values_to = 'Valor')
#Filtrando
engajamento_captura <- engajamento_captura %>% 
  mutate(`Quem Capturou` = recode(`Quem Capturou`, 'caught_daphnie' = 'Daphnie', 'caught_velma' = 'Velma', 'caught_fred' = 'Fred', 
                                  'caught_shaggy' = 'Salsicha', 'caught_scooby' = 'Scooby', 'caught_other' = 'Outros', 'caught_not' = 'Não houve' ))
engajamento_captura <- engajamento_captura %>% 
  filter(Valor != 'False' ) %>%
  filter(Valor != '')

level_order2 = c('Daphnie', 'Fred', 'Salsicha', 'Scooby', 'Velma', 'Outros','Não houve')
# Gráfico BoxPlot multivariado
ggplot(engajamento_captura) +
  aes(x = factor(reorder(`Quem Capturou`, engagement, FUN = median), level = level_order2), y = engagement) +
  geom_boxplot(fill = c("#A11D21"), width = 0.5) +
  stat_summary(
    fun = "mean", geom = "point", shape = 23, size = 3, fill = "white"
  ) +
  labs(x = "Personagem que capturou", y = "Engajamento") +
  theme_estat()
ggsave(file.path(caminho_andre, "boxplot_engajamento_personagem.pdf"), width = 158, height = 93, units = "mm")

engajamento_captura %>% 
  group_by(`Quem Capturou`) %>%
  print_quadro_resumo(var_name = 'engagement')
