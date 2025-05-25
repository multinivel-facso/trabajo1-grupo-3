# ******************************************************************************
# 
#                         Universidad de Chile
#                     Facultad de Ciencias Sociales
#                    Modelos Multinivel 2025
#
#             Plantilla procesamiento trabajo final curso
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

# ----------------Establecer directorio-----------------------------------------

#Colocar Directorio del proyecto 
setwd("Colocar directorio")

# Confirmar directorio/ evitar problema de compatibilidad. 
getwd()

# Carga datos ------------------------------------------------------------------

# 1. Leer las bases desde tus rutas
casen2022 <- read_sav("D:/U pruebas/TDR_1/Prueba2/trabajo1-grupo-3/input/data/Base de datos Casen 2022 SPSS_18 marzo 2024.sav")
comunas <- read_sav("D:/U pruebas/TDR_1/Prueba2/trabajo1-grupo-3/input/data/Base de datos provincia y comuna Casen 2022 SPSS.sav")


## Unir bases ------------------------------------------------------------------

# 2. Unir base de datos por folio e id_persona
casen_completa <- left_join(
  casen2022,
  comunas %>% select(folio, id_persona, comuna),
  by = c("folio", "id_persona")
)


## Filtrar por comunas de santiago ---------------------------------------------

# 3. Crear vector con códigos y etiquetar los nombres de las 32 comunas del Gran Santiago
comunas_santiago <- tibble::tibble(
  codigo = c(
    13101, 13102, 13103, 13104, 13105, 13106, 13107, 13108,
    13109, 13110, 13111, 13112, 13113, 13114, 13115, 13116,
    13117, 13118, 13119, 13120, 13121, 13122, 13123, 13124,
    13125, 13126, 13127, 13128, 13129, 13130, 13131, 13132
  ),
  nombre = c(
    "Santiago", "Cerrillos", "Cerro Navia", "Conchalí", "El Bosque", "Estación Central",
    "Huechuraba", "Independencia", "La Cisterna", "La Florida", "La Granja", "La Pintana",
    "La Reina", "Las Condes", "Lo Barnechea", "Lo Espejo", "Lo Prado", "Macul", "Maipú",
    "Ñuñoa", "Pedro Aguirre Cerda", "Peñalolén", "Providencia", "Pudahuel", "Quilicura",
    "Quinta Normal", "Recoleta", "Renca", "San Joaquín", "San Miguel", "San Ramón", "Vitacura"
  )
)


# 4. Filtrar casos solo de esas comunas y volver factor a comuna.
casen_santiago <- casen_completa %>%
  filter(comuna %in% comunas_santiago$codigo) %>%
  mutate(comuna = factor(comuna, levels = comunas_santiago$codigo, labels = comunas_santiago$nombre))

#### Filtrar variables utilizadas y limpieza -----------------------------------

casen_educ <- casen_santiago %>%
  select(esc, comuna, "agregar las otras variables que utlizaremos") %>%
  filter(!is.na(esc), !is.na(comuna), "XXXX")

##ICC --------------------------------------------------------------------------

# 5. Ajustar modelo nulo para ICC
modelo_icc_santiago = lmer(esc ~ 1 + (1 | comuna), data = casen_educ)
summary(modelo_icc_santiago)

# 6. Tabla icc

reghelper::ICC(modelo_icc_santiago)

screenreg(modelo_icc_santiago) # de library texreg



##Volver Base de datos ---------------------------------------------------------

save(data,file="output/casen_educ.RData")

