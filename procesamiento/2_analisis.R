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
               dplyr,# manipulacion de datos
               foreign,
               lattice,
               sjPlot,
               ggeffects, 
               here
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


#tabla de modelos 

tab_model(
  modelo_icc,      # Modelo 0
  results_1,       # Modelo 1: individuales
  results_2,       # Modelo 2: contextuales
  results_3,       # Modelo 3: completos
  show.ci = FALSE,
  show.se = TRUE,
  collapse.se = TRUE,
  show.p = FALSE,
  p.style = "scientific_stars",
  dv.labels = c("Modelo 0: Intercepto", 
                "Modelo 1: Individuales", 
                "Modelo 2: Contextuales", 
                "Modelo 3: Completo")

  
###############################################################################
#Aleatorios para participacion
  
#Modelo con intercepto + pendiente ALEATORIA ------------------------------
  
  mod_aleat <- lmer(
    esc ~ part_social_num + female + nse_numerico +          # efectos fijos
      (1 + part_social_num | comuna),                    # intercepto y pendiente aleatoria
    data = casen_educ
  )
  
#Ver componentes aleatorios ----------------------------------------------
head(ranef(mod_aleat)$comuna)   # U0j y U1j para cada comuna
  
# head(coef(mod_aleat)$comuna)  # γ00 + U0j   |   β1 + U1j
  
# Predicciones por comuna (pendientes aleatorias) --------------------------
  plt <- ggpredict(
    mod_aleat,
    terms = c("part_social_num", "comuna [sample=9]"),  # 9 comunas al azar
    type  = "random"                                    # usa U0j y U1j
  )
  
  plot(plt) +
    labs(title = "Variación comunal del efecto de la carencia de participación social")
  
# Tabla resumida -----------------------------------------------------------
screenreg(mod_aleat)
  
 
###############################################################################

# Modelo con intercepto y pendiente aleatoria para 'female'
mod_female_aleat <- lmer(
    esc ~ female + part_social_num + nse_numerico +           # efectos fijos
      (1 + female | comuna),                                  # intercepto y pendiente aleatoria
    data = casen_educ
  )
  
# Ver componentes aleatorios por comuna (U0j y U1j)
head(ranef(mod_female_aleat)$comuna)
  
# Predicciones para 9 comunas al azar
plt_female <- ggpredict(
    mod_female_aleat,
    terms = c("female", "comuna [sample=9]"),
    type = "random"
  )
  
# Gráfico del efecto del género según comuna
plot(plt_female) +
    labs(
      title = "Variación comunal del efecto de ser mujer",
      x = "Género (0 = hombre, 1 = mujer)",
      y = "Años esperados de escolaridad"
    )
  
# Tabla resumida del modelo
  screenreg(mod_female_aleat)
