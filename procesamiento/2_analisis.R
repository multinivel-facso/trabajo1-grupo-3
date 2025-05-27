# ******************************************************************************
# 
#                         Universidad de Chile
#                     Facultad de Ciencias Sociales
#                    Modelos Multinivel 2025
#
#                  Plantilla procesamiento trabajo final curso
#
# ******************************************************************************
# Carga Librerías --------------------------------------------------------------

library(pacman)
pacman::p_load(lme4,
               reghelper,
               haven,
               stargazer,
               ggplot2, # gráficos
               texreg, # tablas de regresion (screenreg)
               dplyr # manipulacion de datos
)

rm(list = ls()) # para limpiar el entorno de trabajo

#----------------Establecer directorio-----------------------------------------

setwd("Colocar directorio")           # ajústalo a tu ruta
getwd()


#Cargar base de datos trabaja anteriormente (cansen_educ)

casen_educ <- readRDS("output/casen_educ.rds")



##Descriptivos------------------------------------------------------------------

stargazer(as.data.frame(casen_educ), title="Estadísticos descriptivos", type = "text")


##Modelo 0 ICC -----------------------------------------------------------------

modelo_icc = lmer(esc ~ 1 + (1 | comuna), data = casen_educ)

    # 6. Tabla icc

reghelper::ICC(modelo_icc)

screenreg(modelo_icc) # de library texreg


#Modelo 1: Predictores de nivel individual--------------------------------------

results_1 = lmer(esc ~ 1 + female + nse_numerico + part_social_num +(1 | comuna), data = casen_educ)
screenreg(results_1, naive=TRUE)

#Modelo 2: Predictores nivel 2

results_2 = lmer(esc ~ 1 + prop_empleo + n_escuelas + (1 | comuna), data = casen_educ)
screenreg(results_2)

#Modelo 3: Predictores individuales y grupales----------------------------------
results_3 = lmer(esc ~ 1 + female + nse_numerico + part_social_num + prop_empleo + n_escuelas + (1 | comuna), data = casen_educ)
screenreg(results_3)
