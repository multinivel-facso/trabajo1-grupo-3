# ******************************************************************************
# 
#                         Universidad de Chile
#                     Facultad de Ciencias Sociales
#                        Modelos Multinivel 2025
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
               here, 
               summarytools
)


rm(list = ls()) # para limpiar el entorno de trabajo

#----------------Establecer directorio-----------------------------------------

setwd("Colocar directorio")           # ajústalo a tu ruta
getwd()


#Cargar base de datos trabaja anteriormente (cansen_educ)

casen_educ <- readRDS("output/casen_educ.rds")


# GRAFICOS BIVARIADOS NIVEL 2 --------------------------------------------------

# Scatterplot: relación entre promedio comunal de escolaridad y tasa de empleo

casen_educ %>%
  group_by(comuna) %>%
  summarise(
    esc_prom = mean(esc, na.rm = TRUE),
    prop_empleo = first(prop_empleo)
  ) %>%
  ggplot(aes(x = prop_empleo, y = esc_prom)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "steelblue") +
  labs(
    title = "Relación entre tasa de empleo comunal y escolaridad promedio",
    x = "Tasa de empleo (proporción)",
    y = "Años promedio de escolaridad"
  ) +
  theme_minimal()


# Boxplot: escolaridad individual según cuartiles de escuelas comunales

casen_educ %>%
  mutate(
    cuartil_escuelas = ntile(n_escuelas, 4)  # divide en 4 cuartiles
  ) %>%
  ggplot(aes(x = factor(cuartil_escuelas), y = esc)) +
  geom_boxplot(fill = "lightgray", color = "black") +
  labs(
    title = "Distribución de escolaridad según cantidad de escuelas en la comuna",
    x = "Cuartil de cantidad de escuelas",
    y = "Años de escolaridad"
  ) +
  theme_minimal()

# CENTRADO DE VARIABLES --------------------------------------------------------
# ─────────────────  CMC para NSE individual  ─────────────────
casen_educ <- casen_educ %>% 
  group_by(comuna) %>% 
  mutate(
    mean_nse_comuna = mean(nse_numerico, na.rm = TRUE),
    nse_cmc         = nse_numerico - mean_nse_comuna
  ) %>% 
  ungroup()
# ─────────────────  GMC para variables comunales  ─────────────────
casen_educ <- casen_educ %>% 
  mutate(
    prop_empleo_gmc = prop_empleo - mean(prop_empleo, na.rm = TRUE),
    n_escuelas_gmc  = n_escuelas  - mean(n_escuelas,  na.rm = TRUE)
  )


###############################################################################
# Modelo 0: nulo / ICC ---------------------------------------------------------
m0 <- lmer(esc ~ 1 + (1 | comuna), data = casen_educ)
ICC(m0)

# Modelo 1: solo predictores nivel 1 ------------------------------------------
m1 <- lmer(
  esc ~ 1 + female + nse_cmc + part_social_num +
    (1 | comuna),
  data = casen_educ
)

# Modelo 2: solo predictores nivel 2 ------------------------------------------
m2 <- lmer(
  esc ~ 1 + prop_empleo_gmc + n_escuelas_gmc +
    (1 | comuna),
  data = casen_educ
)

# Modelo 3: fijos L1 + L2 ------------------------------------------------------
m3 <- lmer(
  esc ~ 1 + female + nse_cmc + part_social_num +
    prop_empleo_gmc + n_escuelas_gmc +
    (1 | comuna),
  data = casen_educ
)


# Modelo 4: pendiente aleatoria para NSE (ya estimado) -------------------------
m4 <- lmer(
  esc ~ 1 + female + nse_cmc + part_social_num +
    prop_empleo_gmc + n_escuelas_gmc +          # efectos fijos
    (1 + nse_cmc | comuna),                     # intercepto + pendiente NSE
  data = casen_educ
)
anova(m4, m3)   # test de devianza (m3 = fijos L1+L2)

# Modelo 5-H6: interacción L1 × L2 SOLO para H6 -------------------------------
m5_H6 <- lmer(
  esc ~ 1 +
    nse_cmc         * prop_empleo_gmc +   # H6 (se mantiene)
    part_social_num +                     # efecto principal (sin interacción)
    n_escuelas_gmc +                      # efecto principal contextual
    female +
    (1 + nse_cmc | comuna),
  data = casen_educ
)

# (opcional) comparar m5_H6 con m4
anova(m5_H6, m4)

# Tabla rápida de resumen ------------------------------------------------------
screenreg(
  list("Pend. NSE" = m4, "Interacción H6" = m5_H6),
  stars  = c(0.001, 0.01, 0.05),
  digits = 3
)

# ------------------------------------------------------------------------------
#  ––– útil si entregarás un documento HTML o Quarto –––
#  Deja «todo» pero permite desplazamiento horizontal.
library(sjPlot)

cat('<div style="overflow-x: auto; width: 100%;">')

tab_model(
  m0,        # nulo / ICC
  m1,        # L1 centrado
  m2,        # L2 centrado
  m3,        # L1 + L2 (pendientes fijas)
  m4,        # + pendiente aleatoria NSE
  m5_H6,     # + interacción H6 (NSE × empleo)   ← nuevo modelo final
  show.ci   = FALSE,
  show.se   = TRUE,
  p.style   = "stars",
  digits    = 3,
  dv.labels = c(
    "Nulo (ICC)",
    "L1 (centrado)",
    "L2 (centrado)",
    "L1 + L2",
    "+ Pend. NSE",
    "+ Interacción H6"
  )
)

cat('</div>')

# ─────────────────────────  H6: NSE × Tasa de empleo  ─────────────────────────
int_h6 <- plot_model(
  m5,
  type  = "int",
  terms = c("nse_cmc", "prop_empleo_gmc")
)

# el ggplot está en la posición 1 de la lista
p_h6 <- int_h6[[1]] +                     # ⬅️  aquí el cambio
  labs(
    title  = "Interacción H6: NSE individual × Tasa de empleo comunal",
    x      = "NSE centrado en la comuna (CMC)",
    y      = "Años esperados de escolaridad",
    colour = "Tasa de empleo comunal\n(GMC; −1 DE / Media / +1 DE)"
  ) +
  theme_minimal(base_size = 12)

p_h6
# ─────────────────────────  Cálculo de R² multinivel ─────────────────────────

# 0. Librería para R² de Bryk & Raudenbush -------------------------------------
pacman::p_load(misty,purrr)        # multilevel.r2()

rb_r2 <- function(null_mod, mod, label){
  ## Extraer componentes σ² y τ00
  get_vars <- function(m){
    vc <- as.data.frame(VarCorr(m))
    sigma2 <- vc$vcov[vc$grp == "Residual"]
    tau00  <- vc$vcov[vc$grp != "Residual" & vc$var1 == "(Intercept)"]
    c(sigma2 = sigma2, tau00 = tau00)
  }
  v0 <- get_vars(null_mod)
  vf <- get_vars(mod)
  
  r2_l1 <- (v0["sigma2"] - vf["sigma2"]) / v0["sigma2"]
  r2_l2 <- (v0["tau00"]  - vf["tau00"])  / v0["tau00"]
  
  tibble(Modelo = label,
         R2_L1  = round(r2_l1, 3),
         R2_L2  = round(r2_l2, 3))
}

#tabla ----------------------------------------------
tabla_rb <- purrr::map2_dfr(          # ← prefijo explícito
  .x = list(m1, m2, m3, m4, m5_H6),
  .y = c("L1", "L2", "L1+L2", "+ Pend. NSE", "+ H6"),
  ~ rb_r2(null_mod = m0, mod = .x, label = .y)
)

print(tabla_rb)
#-------------------------------------------------------------------------------
saveRDS(casen_educ, file = "output/casen_educ_cen.rds")