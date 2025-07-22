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
casen2022 <- read_sav("D:/U/Multivariada/trabajo1-grupo-3/input/data/Base de datos Casen 2022 SPSS_18 marzo 2024.sav")
comunas <- read_sav("D:/U/Multivariada//trabajo1-grupo-3/input/data/Base de datos provincia y comuna Casen 2022 SPSS.sav")


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
    # Dummy numérica: 1 = No carente, 0 = Carente
    part_social_num = ifelse(hh_d_part == 0, 1, 0),
    
    # Factor con etiquetas legibles
    part_social_fac = factor(
      part_social_num,
      levels = c(0, 1),
      labels = c("Carente", "No carente")
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

#
#Crear tabla con número de personas por comuna
# ------------------------------------------------------------------------------
poblacion_tbl <- tibble::tribble(
  ~comuna,                 ~n_poblacion,
  "Lo Barnechea",            112.620,
  "Quilicura",               205.624,
  "Huechuraba",              101.808,
  "Conchalí",                121.587,
  "Vitacura",                86.420,
  "Renca",                   143.622,
  "Recoleta",                154.615,
  "Independencia",           116.943,
  "Las Condes",              296.134,
  "Cerro Navia",             127.250,
  "Quinta Normal",           129.351,
  "Providencia",             143.974,
  "Pudahuel",                227.820,
  "La Reina",                89.870,
  "Lo Prado",                91.290,
  "Santiago",                438.856,
  "Ñuñoa",                   241.467,
  "Estación Central",        181.049,
  "Peñalolén",               236.478,
  "Macul",                   123.800,
  "Pedro Aguirre Cerda",     96.062,
  "San Joaquín",             95.602,
  "San Miguel",              150.829,
  "Cerrillos",               85.041,
  "Maipú",                   503.635,
  "Lo Espejo",               87.295,
  "La Florida",              374.836,
  "La Cisterna",             103.157,
  "La Granja",               112.022,
  "San Ramón",               76.002,
  "El Bosque",               155.257,
  "La Pintana",              175.421)

# Unir a casen_santiago (la columna comuna ya es factor con nombres)
casen_santiago <- casen_santiago %>%
  left_join(poblacion_tbl, by = "comuna")

# Crear tabla de indice de desarrollo  humano por comuna
# ------------------------------------------------------------------------------
idh_comuna_tbl <- tibble::tribble(
  ~comuna,                 ~idh_comuna,
  "Lo Barnechea",            0.909,
  "Quilicura",               0.656,
  "Huechuraba",              0.750,
  "Conchalí",                0.587,
  "Vitacura",                0.961,
  "Renca",                   0.585,
  "Recoleta",                0.590,
  "Independencia",           0.576,
  "Las Condes",              0.936,
  "Cerro Navia",             0.521,
  "Quinta Normal",           0.604,
  "Providencia",             0.886,
  "Pudahuel",                0.644,
  "La Reina",                0.862,
  "Lo Prado",                0.580,
  "Santiago",                0.662,
  "Ñuñoa",                   0.858,
  "Estación Central",        0.594,
  "Peñalolén",               0.702,
  "Macul",                   0.732,
  "Pedro Aguirre Cerda",     0.564,
  "San Joaquín",             0.611,
  "San Miguel",              0.727,
  "Cerrillos",               0.633,
  "Maipú",                   0.7,
  "Lo Espejo",               0.5,
  "La Florida",              0.701,
  "La Cisterna",             0.660,
  "La Granja",               0.568,
  "San Ramón",               0.540,
  "El Bosque",               0.572,
  "La Pintana",              0.498)

# Unir a casen_santiago (la columna comuna ya es factor con nombres)
casen_santiago <- casen_santiago %>%
  left_join(idh_comuna_tbl, by = "comuna")

#-------------------------------------------------------------------------------

#Ajuste de los decimales de la variable n de personas
casen_santiago <- casen_santiago %>% 
  mutate(
    n_poblacion = if_else(
      n_poblacion < 10000,      # usa 10000 o 1e4, sin guión bajo
      n_poblacion * 1000,       # corrige escala
      n_poblacion
    )
  )


summary(casen_santiago$n_poblacion)

#crear variable que establece la proporicion 
casen_santiago <- casen_santiago %>% 
  mutate(prop_pob_esc   = (n_escuelas / n_poblacion) * 10000)


#base final (casen_educ) incluyendo n_escuelas
# ------------------------------------------------------------------------------

casen_educ <- casen_santiago %>% 
  select(
    esc, comuna,                       # dependiente + clúster
    female, nse_factor, nse_numerico,   # individuales ajustados
    part_social_num, part_social_fac,                         # indicador de hogar sin cotizar (si lo usas)
    prop_empleo, idh_comuna, prop_pob_esc, n_escuelas        # contextuales ya unidos
  ) %>% 
  filter(
    !is.na(esc),
    !is.na(nse_numerico),
    !is.na(prop_empleo),
    !is.na(prop_pob_esc),
    !is.na(part_social_num))

sum(is.na(casen_educ))

# Guardar datos ----------------------------------------------------------------
saveRDS(casen_educ, file = "output/casen_educ.rds")
