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
               summarytools,
               ggrepel, 
               tidyverse
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
    esc_prom    = mean(esc, na.rm = TRUE),
    prop_empleo = first(prop_empleo)
  ) %>% 
  ggplot(aes(x = prop_empleo, y = esc_prom)) +
  geom_point() +
  geom_text_repel(aes(label = comuna), size = 2) +   # tamaño reducido
  geom_smooth(method = "lm", se = FALSE, color = "steelblue") +
  labs(
    title = "Relación entre tasa de empleo comunal y escolaridad promedio",
    x     = "Tasa de empleo (proporción)",
    y     = "Años promedio de escolaridad"
  ) +
  theme_minimal()




#Scatterplot: relación entre promedio comunal de escolaridad y IDH

casen_educ %>% 
  group_by(comuna) %>% 
  summarise(
    esc_prom    = mean(esc, na.rm = TRUE),
    idh_comuna = first(idh_comuna)
  ) %>% 
  ggplot(aes(x = idh_comuna, y = esc_prom)) +
  geom_point() +
  geom_text_repel(aes(label = comuna), size = 2) +   # tamaño reducido
  geom_smooth(method = "lm", se = FALSE, color = "steelblue") +
  labs(
    title = "Relación entre IDH y escolaridad promedio",
    x     = "Indice de Desarollo Humano ",
    y     = "Años promedio de escolaridad"
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
    prop_pob_esc_gmc  = prop_pob_esc  - mean(prop_pob_esc,  na.rm = TRUE),
    idh_gmc  = idh_comuna  - mean(idh_comuna,  na.rm = TRUE),
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
  esc ~ 1 + prop_empleo_gmc + prop_pob_esc_gmc + idh_gmc +
    (1 | comuna),
  data = casen_educ
)

screenreg(
  list("Conextuales" = m2),
  stars  = c(0.001, 0.01, 0.05),
  digits = 3
)

# Modelo 3: fijos L1 + L2 ------------------------------------------------------
m3 <- lmer(
  esc ~ 1 + female + nse_cmc + part_social_num +
    prop_empleo_gmc + prop_pob_esc_gmc + idh_gmc +
    (1 | comuna),
  data = casen_educ
)

screenreg(
  list("ambas" = m3),
  stars  = c(0.001, 0.01, 0.05),
  digits = 3
)
# Modelo 4: pendiente aleatoria para NSE (ya estimado) -------------------------
m4 <- lmer(
  esc ~ 1 + female + nse_cmc + part_social_num +
    prop_empleo_gmc + prop_pob_esc_gmc + idh_gmc +         # efectos fijos
    (1 + nse_cmc | comuna),                     # intercepto + pendiente NSE
  data = casen_educ
)
anova(m4, m3)   # test de devianza (m3 = fijos L1+L2)


# Modelo 5-H6: interacción L1 × L2 SOLO para H6 -------------------------------
m5_H6 <- lmer(
  esc ~ 1 +
    nse_cmc         * prop_empleo_gmc +    # H6 (se mantiene)
    part_social_num +                     # efecto principal (sin interacción)
    prop_pob_esc_gmc +                      # efecto principal contextual
    female +
    (1 + nse_cmc | comuna),
  data = casen_educ
)


screenreg(
  list("Interreacion h6" = m5_H6),
  stars  = c(0.001, 0.01, 0.05),
  digits = 3
)
# Modelo 6-H7: interacción L1 × L2 SOLO para H7 -------------------------------

m6_H7 <- lmer(
  esc ~ 1 +
    nse_cmc         * idh_gmc +           # ← NUEVA interacción H7
    part_social_num +                     # efecto principal (sin interacción)
    prop_empleo_gmc +                     # efecto contextual
    prop_pob_esc_gmc +                      # efecto contextual
    female +
    (1 + nse_cmc | comuna),               # pendiente aleatoria para NSE
  data = casen_educ,
  control = lmerControl(
    optimizer = "bobyqa",
    optCtrl   = list(maxfun = 1e5)        # para estabilidad si fuera necesario
  )
)

screenreg(
  list("Interreacion" = m6_H7),
  stars  = c(0.001, 0.01, 0.05),
  digits = 3
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
  m5_H6, 
  m6_H7,# + interacción H6 (NSE × empleo)   ← nuevo modelo final
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
    "+ Interacción H7",
    "+ Interacción H8"
  )
)

cat('</div>')

# ─────────────────────────  H6: NSE × Tasa de empleo  ─────────────────────────
p_h6 <- plot_model(
  m5_H6,
  type  = "int",
  terms = c("nse_cmc", "prop_empleo_gmc")
) +
  labs(
    title  = "Interacción H6: NSE individual × Tasa de empleo comunal",
    x      = "NSE centrado en la comuna (CMC)",
    y      = "Años esperados de escolaridad",
    colour = "Tasa de empleo comunal\n(GMC; −1 DE / Media / +1 DE)"
  ) +
  theme_minimal(base_size = 12)

print(p_h6)

# ─────────────────────────  H7: NSE × IDH ─────────────────────────
p_h8 <- plot_model(
  m6_H7,
  type  = "int",
  terms = c("nse_cmc", "idh_gmc")   # usa el nombre real de tu var. IDH
) +
  labs(
    title  = "Interacción H8: NSE individual × IDH comunal",
    x      = "NSE centrado en la comuna (CMC)",
    y      = "Años esperados de escolaridad",
    colour = "IDH comunal\n(GMC; −1 DE / Media / +1 DE)"
  ) +
  theme_minimal(base_size = 12)

print(p_h8)

# ─────────────────────────  Cálculo de R² multinivel ─────────────────────────


library(lme4)
library(misty)
library(purrr)
library(dplyr)
library(knitr)

# ── 1. Ajustar modelos en ML y SIN pendientes aleatorias ────────────────
# ── 1. Modelos refiteados en ML, SOLO intercepto aleatorio ──────────────

mods <- list(
  m0  = update(m0,        REML = FALSE),
  m1  = update(m1,        REML = FALSE),
  m2  = update(m2,        REML = FALSE),
  m3  = update(m3,        REML = FALSE),
  m4  = update(m4,        REML = FALSE),
  m5  = update(m5_H6,     REML = FALSE),
  m6  = update(m6_H7,     REML = FALSE)
)


# ── 2. Varianzas del modelo nulo (referencia) ───────────────────────────
sigma0  <- sigma(mods[[1]])^2
tau00_0 <- as.numeric(VarCorr(mods[[1]])$comuna[1])

# ── 3. Función para calcular R2 RB manualmente ──────────────────────────
calc_rb <- function(mod) {
  sig2  <- sigma(mod)^2
  tau00 <- as.numeric(VarCorr(mod)$comuna[1])
  tibble(
    R2_L1    = round(1 - sig2              / sigma0,               3),
    R2_L2    = round(1 - tau00            / tau00_0,              3),
    R2_Total = round(1 - (sig2 + tau00)  / (sigma0 + tau00_0),    3)
  )
}

# ── 4. Construir tabla completa ─────────────────────────────────────────
r2_tabla <- imap_dfr(mods, ~calc_rb(.x) %>% mutate(Modelo = .y)) %>%
  relocate(Modelo)

# ── 5. Mostrar tabla scrollable ─────────────────────────────────────────
cat('<div class="scroll-table">')
kable(
  r2_tabla,
  caption = "Coeficiente $R^{2}$ de Raudenbush–Bryk (comparado con modelo nulo)",
  digits  = 3, align = "lccc", row.names = FALSE
)
cat('</div>')

#-------------------------------------------------------------------------------
saveRDS(casen_educ, file = "output/casen_educ_cen.rds")

