rm(list = ls())

#=============================================================================
# A��es iniciais
#=============================================================================

pacman::p_load(tidylog, haven, Amelia, Rcpp, dplyr)
#Carregando pactoes utilizados

#1) Pacman � um pacote que permite o carregamento e instala��o de pacotes no R
## ao mesmo tempo.

#2) Tidylog � uma vers�o do tidyverse com mais feedback.

#3) dplyr � similar a linguagem SQL e � �til para data wrangling

#4) Amelia � o principal pacote utilizado que detecta missings e os preenche por meio
## de imputa��o m�ltipla.

#5) Rcpp � auxiliar ao Amelia.

#=============================================================================
# Caracter�sticas do banco de dados
#=============================================================================

load("data_geral_oficial1.RData")
# Carregando o banco geral da tese que � produto da jun��o de diversos outros bancos.

# Os dados s�o compostas por 190 colunas e 1380 observa��es.

# Al�m disso, est� estruturado em forma de Time series cross-sectional, isto �, como
## diversas linhas para cada pa�s em determinado per�odo de tempo.

#Na tese enfoquei pa�ses do Leste Asi�tico e da Am�rica Latina.

table(data_geralf1$country) #comando para demonstar a frequ�ncia de categorias.

# Output:
# Argentina         Bolivia             Brazil 
# 60                 60                 60 
# Chile              China           Colombia 
# 60                 60                 60 
# Costa Rica         Dominican Republic Ecuador 
# 60                 60                 60 
# El Salvador        Honduras          Hong Kong 
# 60                 60                 60 
# Indonesia         Japan              Korea 
# 60                 60                 60 
# Malaysia           Mexico           Peru 
# 60                 60                 60 
# Philippines        Singapore        Thailand 
# 60                 60                 60 
# Uruguay            Vietnam 
# 60                 60 

table(data_geralf1$year) #Vamos ver a cobertura temporal dos dados

#Output
# 1960 1961 1962 1963 1964 1965 1966 1967 1968 1969 1970 1971 1972 
# 23   23   23   23   23   23   23   23   23   23   23   23   23 
# 1973 1974 1975 1976 1977 1978 1979 1980 1981 1982 1983 1984 1985 
# 23   23   23   23   23   23   23   23   23   23   23   23   23 
# 1986 1987 1988 1989 1990 1991 1992 1993 1994 1995 1996 1997 1998 
# 23   23   23   23   23   23   23   23   23   23   23   23   23 
# 1999 2000 2001 2002 2003 2004 2005 2006 2007 2008 2009 2010 2011 
# 23   23   23   23   23   23   23   23   23   23   23   23   23 
# 2012 2013 2014 2015 2016 2017 2018 2019 
# 23   23   23   23   23   23   23   23 

#Isto �, de 1960 a 2019.

#S�o 4 vari�veis dependentes:
## Gasto p�blico em Seguridade Social (% PIB) - segurpib
## Gasto p�blico em Sa�de (% PIB) - saudpib
## Gasto p�blico em Educa��o (% PIB) - educpib
## Redistribui��o (diferen�a entre Gini Dispon�vel e Gini de Mercado) - redis

#=============================================================================
# Checando os missings
#=============================================================================

# Para um banco time series cross-sectional, os missing afetam muito, pois na maioria
## das vezes � preciso usar lags (sobretudo para corrigir problemas de autocorrela��o serial) o que resulta em perda de observa��es. 

# Por isso, diversos metod�logos recomendam utilizar m�todos de preenchimento como
## interporla��o ou imputa��o m�ltipla. No entanto, estes m�todos t�m limites.

# � recomenadado o preenchimento de valores missings com no m�ximo de intervalo de
## dois anos. Mais do que isso, os algoritmos podem gerar vi�s.


#Vamos usar a fun��o missmap do pacote Amelia que fornece uma visualiza��o dos missings


missmap(data_geralf1)

#A primeira visualiza��o demonstra que poucas vari�veis poucos pa�ses tem dados
# para todo o recorte temoral. Os missings aumentam enquanto a linha temporal regressa.
# A fun��o conta com 43% e 57% valores preenchidos

# Na verdade, no processo de constru��o do banco eu j� havia identificado que pouqu�ssimos pa�ses
# possuiam s�ries hist�ricas da mesma fonte para vari�vel dependente cobrindo antes
# dos anos 1990.

# Em especial, sobretudo para os pa�seas asi�ticos, os dados dispon�veis at� no m�ximo
# 1995. E o recorte temporal da minha pesquisa vai at� 2015.

# Portanto, vou filtrar o banco por este recorte.
df <- data_geralf1 %>% 
  filter(year %in% c(1995:2015))

#
missmap(df, tsvar = "year") #houve uma redu��o para 17% de missings

#O map mostra, no entanto, que muitas vari�veis est�o repletas de missings.
## Foi preciso repetir esse processo de plotagem e exclus�o de vari�veis algumas, pois
## muitas vari�vels estavam inapatas a serem modeladas.

#tirando vari�veis com muitos missings
panel <- df %>% 
  filter(year %in% c(1990:2015)) %>% 
  select(-cover_social_assisten, -cover_family_benefits, -cover_unempinsu,
         -coverage_penions, -tc, -educ_atta, -average_earn_hour, -inf_emp_rate, -inf_emp_rate2,
         -educ_cap_tertiary, -qtl_decile_01, -qtl_decile_02,
         -qtl_decile_03, -qtl_decile_04, -qtl_decile_05, -qtl_decile_06, 
         -qtl_decile_07, -qtl_decile_08,-qtl_decile_10, 
         -mean, -cover_unempins_pop,-low_pay, -laborrights_MU.x,
         -labour_income_gdp,ud_s, -debt_cgov_WDI, -poverty_t_5usd, 
         -wopol_CR, -eco_glob_KOF.x, -eco_glob_KOF.y,-emp_protec, -union_density_ilo,
         -union_density_i,
         -extdebt_WDI,-literacy_rate, -tertiarypib, -constsusp_IDC,
         -tech_rnd_WDI, -educpib_WDI,-temporar_emp, -working_poverty,
         -qtl_decile_10,-coord, -poli_glob_KOF.x,-educgov, -soc_glob_KOF.x,
         -patents_aplications, -high_tech_exports_wdi, -und_unem_time_rate,
         -almp_gdp, -execrlc_DPI.x, -reer_WDI, -avh,-r_d_expend,
         -enroll_tertiary_wdi, -ud_s, -tax_rev_pib, -coletive_bargaint,
         -qtl_decile_09, -unemp_65, -timetoturn_CBG, -laborrights_MU.x,
         -labor_market_reg_fraser, -overallGlob_index_KOF.x, 
         -checks_DPI, -v2x_feduni_VDEM.x, -polity_P4, -numgov_DPI, -KOFPoGIdj,
         -KOFPoGIdj, -v2xlg_legcon_VDEM, -v2xps_party_VDEM,
         -v2xeg_eqdr_VDEM, -labsh, -manuf_export, -v2x_genpp_VDEM,
         -yrcurnt_DPI, -v2x_polyarchy_VDEM, -capformraw_WDI, -foreign_bank,
         -concent_bank, -labsh_PW, -v2x_api_VDEM, -v2x_mpi_VDEM,
         -govt_consump_WDI, -hc_PW, -mean2011PPP, -ka_open_norm, -iso2c) 


#interporla��o


# Outras vari�veis tiveram que ser extra�das, pois expressaram multicolineariedade
## E a fun��o de interpola��o m�ltipa do Amelia n�o roda com multicolineriedade.

panel <- panel %>% 
  select(-manuf_gdp, -surveyyears, -redis, -ka_open_norm)


panel_int <- amelia(panel, m= 3, #comando interpola��o
                     ts = "year", cs = "ccode", idvars = c("pais", "country", "region"),
                     ords= c("v2pasoctie_ord", "checks", "state", "v2elparlel"),
                     noms = c("party_gov")) 
#Esta � a fun��o.
#ts � para indicar a vari�vel temporal. 
#cs s�o as unidades, no caso pa�ses,
#m � o n�mero de interpola��es geradas. Ele vai gerar tr�s vers�es diferentes de banco interpolados.

#E os demais comandos s�o para discriminar vari�veis ordinais e nominais. Do contr�rio,
## a fun��o n�o roda.

write.amelia(obj= panel_int, file.stem= "panel_int", format = "csv") 
##Salvando em csv, os bancos interpolados. 

