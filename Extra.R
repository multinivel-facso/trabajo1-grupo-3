# ******************************************************************************
#  Universidad de Chile · Modelos Multinivel 2025
#  GENERAR ECACIONES EN MathJax PARA TODOS LOS MODELOS (m0 … m5_H6)
#  Cada ecuación se imprime lista para copiar-pegar en Quarto/HTML.
# ******************************************************************************

# ───────────────── 1. FUNCIÓN CORREGIDA (siempre 1 string) ──────────────────
# ******************************************************************************
#      Universidad de Chile · Modelos Multinivel 2025
#      Generar ecuaciones MathJax para m0 – m5_H6 (casen_educ)
# ******************************************************************************

if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
pacman::p_load(lme4, stringr, purrr)

# ───────────────────────── FUNCIÓN ROBUSTA ───────────────────────────────────
lmer_to_mathjax <- function(model,
                            resp      = "\\text{esc}_{ij}",
                            level2_id = NULL) {
  # 1. Detectar agrupación
  if (is.null(level2_id)) {
    grp <- lme4::findbars(formula(model)[[3]])[[1]][[3]]
    level2_id <- deparse(grp)              # p.e. "comuna"
  }
  
  # 2. Parte fija
  fixed_terms <- attr(terms(model), "term.labels")
  pretty <- function(x) str_c("\\text{", x, "}")
  gamma_fixed <- str_c("\\gamma_{0", seq_along(fixed_terms), "} ",
                       pretty(fixed_terms))
  fixed_part  <- str_c(c("\\gamma_{00}", gamma_fixed), collapse = " + ")
  
  # 3. Parte aleatoria
  rand_components <- lme4::findbars(formula(model)[[3]]) |>
    map(function(re) {
      vars <- attr(terms(as.formula(paste("~", deparse(re[[2]])))), "term.labels")
      if (length(vars) == 0) {
        str_c("u_{0", level2_id, "}")
      } else {
        c(str_c("u_{0", level2_id, "}"),
          str_c("u_{", seq_along(vars), level2_id, "} ", pretty(vars)))
      }
    }) |>
    unlist() |>
    unique()
  rand_part <- str_c(rand_components, collapse = " + ")
  
  # 4. Error
  eps <- "r_{ij}"
  
  # 5. Ecuación completa (colapsar a 1 string)
  paste(
    "$$\n",
    resp, " = ", fixed_part,
    if (rand_part != "") str_c(" + ", rand_part),
    " + ", eps, "\n$$",
    collapse = ""
  )
}

# ───────────────────────── MODELOS EN LISTA ───────────────────────────────────
models <- list(
  m0    = m0,
  m1    = m1,
  m2    = m2,
  m3    = m3,
  m4    = m4,
  m5_H6 = m5_H6
)

# ───────────────────────── GENERAR ECUACIONES ─────────────────────────────────
mathjax_eqs <- purrr::imap_chr(
  models,
  function(mod, nom) {
    paste0(
      "### Modelo ", nom, "\n",
      lmer_to_mathjax(mod), "\n"
    )
  }
)

# ───────────────────────── MOSTRAR RESULTADO ──────────────────────────────────
cat(paste(mathjax_eqs, collapse = "\n"))


library(kableExtra)

library(knitr)
library(kableExtra)

kable(lrt,
      format   = "html",
      digits   = 3,
      caption  = "Test de devianza: ¿pendiente aleatoria de NSE mejora el modelo?") |>
  kable_styling(full_width = FALSE,
                bootstrap_options = c("striped", "hover", "condensed")) |>
  add_header_above(c(" " = 6, "Likelihood-ratio Test" = 3)) |>   # 6 + 3 = 9
  add_footnote("χ² significativo (p < 0.001) indica que la pendiente aleatoria de NSE mejora el ajuste.",
               notation = "none")

lrt <- as.data.frame(anova(m4, m3))
names(lrt)


#_____________________________________________________________________________

int_h6 <- plot_model(
  m5_H6,
  type  = "int",
  terms = c("nse_cmc", "prop_empleo_gmc")   # pred, moderador
  # si quisieras especificar los niveles del moderador:
  # terms = c("nse_cmc", "prop_empleo_gmc [-1 +1]")
)

is.ggplot(int_h6)
#> TRUE   ← entonces usa int_h6 directamente

p_h6 <- int_h6 +
  labs(
    title  = "Interacción H6: NSE individual × Tasa de empleo comunal",
    x      = "NSE centrado en la comuna (CMC)",
    y      = "Años esperados de escolaridad",
    colour = "Tasa de empleo comunal\n(GMC; −1 DE / Media / +1 DE)"
  ) +
  theme_minimal(base_size = 12)

print(p_h6)      

#_______________________________________________________________________________

# Cargar librería necesaria
library(influence.ME)

# Calcular DFBETAS para el modelo m4
infl_m4 <- influence(m4, group = "comuna")
dfb_m4   <- as.data.frame(dfbetas(infl_m4))

# Revisar nombres de columnas (coeficientes)
names(dfb_m4)
# Por ejemplo: (Intercept), female, nse_cmc, part_social_num, etc.

# Calcular punto de corte sugerido
cutoff <- 2 / sqrt(nrow(dfb_m4))  # n ≈ número de grupos
print(cutoff)

# Filtrar observaciones influyentes (por encima del umbral en al menos un coeficiente clave)
dfb_influyentes <- dfb_m4 %>%
  filter(
    abs(nse_cmc) > cutoff |
      abs(part_social_num) > cutoff
  )

# Ver las observaciones influyentes
print(dfb_influyentes)

# Agregar IDs de comuna
dfb_influyentes$comuna <- rownames(dfb_influyentes)

# Graficar DFBETAS para dos parámetros clave
plot(infl_m4, which = "dfbetas",
     parameters = c("nse_cmc", "part_social_num"),
     xlab = "DFBETAS", ylab = "Comuna")
¿Qué hace cada parte?