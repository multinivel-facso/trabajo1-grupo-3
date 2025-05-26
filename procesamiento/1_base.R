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

#----------------Establecer directorio-----------------------------------------

setwd("Colocar directorio")           # ajústalo a tu ruta
getwd()


#Carga datos ------------------------------------------------------------------

#Leer las bases desde tus rutas
casen2022 <- read_sav("D:/U pruebas/TDR_1/Prueba2/trabajo1-grupo-3/input/data/Base de datos Casen 2022 SPSS_18 marzo 2024.sav")
comunas <- read_sav("D:/U pruebas/TDR_1/Prueba2/trabajo1-grupo-3/input/data/Base de datos provincia y comuna Casen 2022 SPSS.sav")


#Unir bases por folio + id_persona  (añade código de comuna)------------------

casen_completa <- left_join(
  casen2022,
  comunas %>% select(folio, id_persona, comuna),
  by = c("folio", "id_persona")
)


##Filtrar por comunas de santiago ---------------------------------------------


#Quedarse solo con las 32 comunas del núcleo urbano de Santiago

comunas_santiago <- tibble::tibble(
  codigo = c(
    13101:13132      # rango contiguo desde 13101 a 13132
  ),
  nombre = c(
    "Santiago","Cerrillos","Cerro Navia","Conchalí","El Bosque","Estación Central",
    "Huechuraba","Independencia","La Cisterna","La Florida","La Granja","La Pintana",
    "La Reina","Las Condes","Lo Barnechea","Lo Espejo","Lo Prado","Macul","Maipú",
    "Ñuñoa","Pedro Aguirre Cerda","Peñalolén","Providencia","Pudahuel","Quilicura",
    "Quinta Normal","Recoleta","Renca","San Joaquín","San Miguel","San Ramón","Vitacura"
  )
)

#filtro casen de comuna de santiago---------------------------------------------

casen_santiago <- casen_completa %>% 
  filter(comuna %in% comunas_santiago$codigo) %>% 
  mutate(comuna = factor(comuna,
                         levels = comunas_santiago$codigo,
                         labels = comunas_santiago$nombre))

#Ajuste de variable sexo y NSE--------------------------------------------------
                                    #sexo: 1 = hombre, 2 = mujer  → dummy female                                                          # 1= mujer, 0 = hombr

casen_santiago <- casen_santiago %>% 
  mutate(
      # dummy female (1 = mujer, 0 = hombre)
    female = ifelse(sexo == 2, 1, 0))
    

#Nse

casen_santiago <- casen_santiago %>% 
      mutate(
        # factor ordenado (categorías 1-7 en sentido ascendente)
        nse_factor = factor(
          nse,
          levels = c(1, 4, 5, 6, 2, 7, 3),
          labels = c("Bajo",
                     "Bajo-medio",
                     "Bajo-alto",
                     "Bajo-medio-alto",
                     "Medio",
                     "Medio-alto",
                     "Alto"),
          ordered = TRUE
        ),
        
        # versión numérica 1–7 que respeta ese orden
        nse_numerico = as.numeric(nse_factor)
      )

#Ajuste variable hogar carente de participacion social (hh_d_part)--------------

casen_santiago <- casen_santiago %>% 
  mutate(
    # 1) Dummy clara: 1 = hogar carente, 0 = no carente
    part_social = ifelse(hh_d_part == 1, 1, 0),
    
    # 2) Factor con etiquetas legibles (opción para tablas/modelos)
    part_social = factor(
      hh_d_part,
      levels = c(0, 1),
      labels = c("No carente", "Carente")
    )
  )


##Crear la variable contextual: prop_empleo (tasa de empleo efectivo)------------

                  # trabajó ≥ 1 hora la semana pasada    1= trabajo // 0= no trabajo

casen_santiago <- casen_santiago %>% 
  mutate(emp_efect = o1 == 1)                          # TRUE / FALSE

prop_empleo <- casen_santiago %>% 
  group_by(comuna) %>% 
  summarise(prop_empleo = mean(emp_efect, na.rm = TRUE))   # 0–1

casen_santiago <- casen_santiago %>%
  left_join(prop_empleo, by = "comuna")   # ← ahora prop_empleo es columna numérica



# Crear tabla con número de establecimientos educacionales por comuna(dato 2022)
# ------------------------------------------------------------------------------

escuelas_tbl <- tibble::tribble(
  ~comuna,                 ~n_escuelas,
  "Lo Barnechea",            31,
  "Quilicura",               59,
  "Huechuraba",              27,
  "Conchalí",                58,
  "Vitacura",                19,
  "Renca",                   58,
  "Recoleta",                68,
  "Independencia",           36,
  "Las Condes",              59,
  "Cerro Navia",             49,
  "Quinta Normal",           69,
  "Providencia",             43,
  "Pudahuel",                64,
  "La Reina",                43,
  "Lo Prado",                30,
  "Santiago",               128,
  "Ñuñoa",                   80,
  "Estación Central",        59,
  "Peñalolén",               69,
  "Macul",                   40,
  "Pedro Aguirre Cerda",     50,
  "San Joaquín",             40,
  "San Miguel",              63,
  "Cerrillos",               32,
  "Maipú",                  199,
  "Lo Espejo",               38,
  "La Florida",             184,
  "La Cisterna",             63,
  "La Granja",               54,
  "San Ramón",               38,
  "El Bosque",               92,
  "La Pintana",              70
)


# Unir a casen_santiago (la columna comuna ya es factor con nombres)
casen_santiago <- casen_santiago %>%
  left_join(escuelas_tbl, by = "comuna")


#base final (casen_educ) incluyendo n_escuelas
# ------------------------------------------------------------------------------

casen_educ <- casen_santiago %>% 
  select(
    esc, comuna,                       # dependiente + clúster
    female, nse_factor, nse_numerico,   # individuales ajustados
    part_social,                         # indicador de hogar sin cotizar (si lo usas)
    prop_empleo, n_escuelas            # contextuales ya unidos
  ) %>% 
  filter(
    !is.na(esc),
    !is.na(nse_numerico),
    !is.na(n_escuelas),
    !is.na(prop_empleo)
  )

# Guardar datos ----------------------------------------------------------------
saveRDS(casen_educ, file = "output/casen_educ.rds")
