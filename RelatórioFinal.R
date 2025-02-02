---
  title: "O que brasileiros e brasileiras casados pensam sobre os trabalhos 
                  dom�stico e de cuidado n�o remunerado? Uma an�lise 
                    feita a partir de dados do Changing Family
                            and Gender Values (ISSP) 
                                    de 2002"
  author: "Maria Elisa Rocha Couto Gomes"
  date: "26 de outubro de 2021"
  output:
    prettydoc::html_pretty:
    theme: cayman
---
    
# Changing Family and Gender Values 2002 ----------------------------------

# Relat�rio final

# R para Ci�ncia de Dados I


# Extra��o dos dados ------------------------------------------------------

# 1� - ativando os principais pacotes cujas fun��es ser�o utilizadas:

library(haven)
library(tidyverse)
library(foreign)
library(ggplot2)
library(patchwork)
library(dplyr)

# 2� - extraindo os dados do arquivo .dta original (mantendo os valores das 
# labels):

ZA3880_v1_1_0 <- 
  foreign::read.dta("C:/Users/elisa/Downloads/changing.dta", 
                                   convert.dates = TRUE, 
                                  convert.factors = TRUE, 
                                   missing.type = FALSE,
                                  convert.underscore = FALSE, 
                                   warn.missing.labels = TRUE)

# N�o procurar outro pacote, trocar manualmente. naif dplyr



# Organizando a base de dados ---------------------------------------------

# 1� -  filtrando a base para manter apenas os casos de entrevistados brasileiros
# que estavam na amostra da pesquisa em 2002:

brasil <- ZA3880_v1_1_0 %>% 
  group_by(COUNTRY) %>% 
  filter(COUNTRY == "Brazil (BR)")

# 2� - selecionando as vari�veis de interesse para minha an�lise. Isto �, todas 
# as que est�o relacionadas com trabalho dom�stico e de cuidado n�o remunerado 
# e algumas referentes �s caracter�sticas sociodemogr�ficas dos respondentes:

brasil_vs <- brasil %>% 
  select(v3, COUNTRY, v7, v8, v11, v12, v13, v18, 
         v19, v20, v24, v25, v26, v27, v28, v30, 
         v31, v32, v33, v34, v35, v36, v37, v38, 
         v39, v40, v44, v45, v48, v50, v51, v54, 
         v66, v67, v69, v70, v71, v201, v200, 
         v202, v204, v239, v240, v246, 
         v249, v250, v359, v361) %>% 
  mutate(v200teste = na_if(v200, "Na, refused"))

count(brasil_vs, v200, v200teste)

# 3� - filtrando os casos para que apenas os respondentes casados ou que 
# coabitam com seus parceiros permane�am em minha amostra.

brasil_casados_vs <- brasil_vs %>% 
  group_by(v202) %>% 
  filter(v202 == "Marr,liv as mar")


# Organizando as vari�veis: renomeando e traduzindo as labels -------------

# 1� - Renomenando as vari�veis:

names(brasil_casados_vs)[names(brasil_casados_vs) == "v3"] <- "identificador"
names(brasil_casados_vs)[names(brasil_casados_vs) == "COUNTRY"] <- "pa�s"
names(brasil_casados_vs)[names(brasil_casados_vs) == "v7"] <- "mulher_casa_filhos"
names(brasil_casados_vs)[names(brasil_casados_vs) == "v8"] <- "casa_realiza��o" 
names(brasil_casados_vs)[names(brasil_casados_vs) == "v11"] <- "trab_homem_mulher" 
names(brasil_casados_vs)[names(brasil_casados_vs) ==  "v12"] <- "homem_dom�sticas"
names(brasil_casados_vs)[names(brasil_casados_vs) == "v13"] <- "homem_cuidado" 
names(brasil_casados_vs)[names(brasil_casados_vs) == "v18"] <- "casamento_feliz" 
names(brasil_casados_vs)[names(brasil_casados_vs) == "v19"] <- "casamento_ruim"
names(brasil_casados_vs)[names(brasil_casados_vs) == "v20"] <- "casamento_filhos"
names(brasil_casados_vs)[names(brasil_casados_vs) == "v24"] <- "div�rcio" 
names(brasil_casados_vs)[names(brasil_casados_vs) == "v25"] <- "filhos_felicidade" 
names(brasil_casados_vs)[names(brasil_casados_vs) == "v26"] <- "filhos_vazio"
names(brasil_casados_vs)[names(brasil_casados_vs) == "v27"] <- "licen�a_maternidade"
names(brasil_casados_vs)[names(brasil_casados_vs) == "v28"] <- "pais_benef�cios"
names(brasil_casados_vs)[names(brasil_casados_vs) == "v30"] <- "lavar_roupa"
names(brasil_casados_vs)[names(brasil_casados_vs) == "v31"] <- "pequenos_reparos"
names(brasil_casados_vs)[names(brasil_casados_vs) == "v32"] <- "doen�a_parentes"
names(brasil_casados_vs)[names(brasil_casados_vs) == "v33"] <- "compras" 
names(brasil_casados_vs)[names(brasil_casados_vs) == "v34"] <- "limpeza" 
names(brasil_casados_vs)[names(brasil_casados_vs) == "v35"] <- "comida"
names(brasil_casados_vs)[names(brasil_casados_vs) == "v36"] <- "horas_trab_dom" 
names(brasil_casados_vs)[names(brasil_casados_vs) =="v37"] <- "horas_dom_parceiro" 
names(brasil_casados_vs)[names(brasil_casados_vs) == "v38"] <- "divis�o_tar_dom" 
names(brasil_casados_vs)[names(brasil_casados_vs) == "v39"] <- "conflito_tar_dom" 
names(brasil_casados_vs)[names(brasil_casados_vs) == "v40"] <- "decis�es_crian�as"
names(brasil_casados_vs)[names(brasil_casados_vs) == "v44"] <- "sobrecarga_tar_dom"
names(brasil_casados_vs)[names(brasil_casados_vs) == "v45"] <- "stress_casa"
names(brasil_casados_vs)[names(brasil_casados_vs) == "v48"] <- "cansa�o_tar_dom" 
names(brasil_casados_vs)[names(brasil_casados_vs) == "v50"] <- "cansa�o_dom_trab"
names(brasil_casados_vs)[names(brasil_casados_vs) == "v51"] <- "concentra��o_trab"
names(brasil_casados_vs)[names(brasil_casados_vs) == "v54"] <- "familia_satisfa��o"
names(brasil_casados_vs)[names(brasil_casados_vs) == "v66"] <- "crian�as_adolescentes"
names(brasil_casados_vs)[names(brasil_casados_vs) == "v67"] <- "crian�as_pequenas"
names(brasil_casados_vs)[names(brasil_casados_vs) == "v69"] <- "total_crian�as_adolescentes" 
names(brasil_casados_vs)[names(brasil_casados_vs) == "v70"]  <- "edu_parceiro" 
names(brasil_casados_vs)[names(brasil_casados_vs) == "v71"] <- "horas_trab_parceiro"
names(brasil_casados_vs)[names(brasil_casados_vs) == "v201"] <- "idade"
names(brasil_casados_vs)[names(brasil_casados_vs) == "v200"] <- "sexo"
names(brasil_casados_vs)[names(brasil_casados_vs) == "v202"] <- "status_marital"
names(brasil_casados_vs)[names(brasil_casados_vs) == "v204"] <- "anos_escolaridade" 
names(brasil_casados_vs)[names(brasil_casados_vs) == "v239"] <- "emprego_status"
names(brasil_casados_vs)[names(brasil_casados_vs) == "v240"] <- "horas_trab" 
names(brasil_casados_vs)[names(brasil_casados_vs) == "v246"] <- "emprego_status_parceiro" 
names(brasil_casados_vs)[names(brasil_casados_vs) == "v249"] <- "rendimento"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      
names(brasil_casados_vs)[names(brasil_casados_vs) == "v250"] <- "renda_familiar" 
names(brasil_casados_vs)[names(brasil_casados_vs) == "v359"] <- "identidade_etnica"
names(brasil_casados_vs)[names(brasil_casados_vs) == "v361"] <- "peso"

# Estat�sticas descritivas ------------------------------------------------
# N�o usar acento no nome das variaveis 
#1� gr�fico - N�mero de homens e mulheres entrevistados na amostra brasileira

sexo_gr�fico <- brasil_casados_vs %>% 
  group_by(sexo) %>% 
  summarise(num_sexo = n()) %>% 
  ungroup() %>% 
  ggplot() +
  geom_col(aes(x = sexo, y = num_sexo, fill = sexo), show.legend = TRUE) +
  scale_fill_viridis_d(labels = c("Masculino", "Femino")) +
  labs(y = "N�mero de entrevistados", fill = "Sexo",
       title = "N�mero de entrevistados de acordo com seu sexo",
      caption = "Changing Family and Gender Values, 2002, International
      Social Programme (ISSP)") +
  theme_minimal()

sexo_gr�fico <- sexo_gr�fico + theme(axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank())

print(sexo_gr�fico)

# 2� gr�fico - Rendimento m�dio por sexo dos entrevistados
  
rendimento_m�dio_gr�fico <- brasil_casados_vs %>% 
  group_by(sexo) %>%
  summarize(rendimento_medio = mean(rendimento, na.rm = TRUE)) %>% 
  ggplot() +
  geom_col(aes(x = sexo, y = rendimento_medio, fill = sexo), show.legend = TRUE) +
  scale_fill_viridis_d(labels = c("Masculino", "Femino")) +
  labs(y = "Rendimento m�dio em R$", fill = "Sexo",
       title = "Rendimento m�dio em R$ por sexo dos entrevistados",
       caption = "Changing Family and Gender Values, 2002, Internacional
       Social Survey Programme (ISSP)")+
  theme_minimal()

rendimento_m�dio_gr�fico <- rendimento_m�dio_gr�fico +
  theme(axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank())

print(rendimento_m�dio_gr�fico)

  
sexo_gr�fico + rendimento_m�dio_gr�fico


