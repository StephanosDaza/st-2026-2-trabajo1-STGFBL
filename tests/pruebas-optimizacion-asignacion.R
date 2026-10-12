source("R/04-metodos.R")
source("R/03-dependencia.R")
source("R/05-evaluacion.R")

y <- c(10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21)
rejilla_mm <- data.frame(k = 2:5)
opt_mm <- optimizar(y, ajustar_mm, rejilla_mm)
mse_manual <- vapply(rejilla_mm$k, function(k) mse_un_paso(ajustar_mm(y, k), y), numeric(1))
stopifnot(
  identical(names(opt_mm), c("modelo", "parametros", "mse", "superficie", "fijos")),
  opt_mm$parametros$k == rejilla_mm$k[which.min(mse_manual)],
  isTRUE(all.equal(opt_mm$mse, min(mse_manual))),
  isTRUE(all.equal(opt_mm$superficie$mse, mse_manual)),
  inherits(graficar_optimizacion(opt_mm), "ggplot")
)

rejilla_holt <- expand.grid(alpha = c(0.2, 0.8), beta = c(0.2, 0.8))
opt_holt <- optimizar(y, ajustar_holt, rejilla_holt)
stopifnot(
  nrow(opt_holt$superficie) == 4L,
  all(opt_holt$superficie$valido),
  inherits(graficar_optimizacion(opt_holt), "ggplot")
)

rejilla_hw <- expand.grid(alpha = c(0.2, 0.8), beta = c(0.2, 0.8), gamma = c(0.2, 0.8))
y_hw <- 20 + 0.3 * seq_len(24) + rep(c(-2, -1, 1, 2), 6)
opt_hw <- optimizar(y_hw, ajustar_hw, rejilla_hw, fijos = list(s = 4))
stopifnot(
  nrow(opt_hw$superficie) == 8L,
  inherits(graficar_optimizacion(opt_hw), "ggplot")
)

catalogo <- crear_catalogo_metodos(12)
asignacion <- definir_asignacion_metodos(catalogo)
stopifnot(
  length(catalogo) == 13L,
  nrow(asignacion) == 39L,
  all(!is.na(asignacion$evidencia) & nzchar(asignacion$evidencia)),
  length(unique(asignacion$evidencia)) == 39L,
  sum(asignacion$decision) == 15L,
  identical(
    asignacion$metodo_id[asignacion$serie == "serie1" & asignacion$decision],
    c("ingenuo_estacional", "tendencia_estacional", "hw_aditivo",
      "hw_multiplicativo")
  ),
  all(vapply(names(catalogo), function(id) {
    any(asignacion$metodo_id == id & asignacion$decision)
  }, logical(1))),
  all(vapply(c("serie1", "serie3"), function(serie) {
    any(asignacion$serie == serie & asignacion$rol == "Contraste")
  }, logical(1))),
  asignacion$rol[asignacion$serie == "serie2" &
                   asignacion$metodo_id == "ingenuo_estacional"] == "No aplicado",
  all(vapply(unique(asignacion$serie), function(serie) {
    sum(asignacion$serie == serie & asignacion$rol == "Referente") == 1L
  }, logical(1)))
)

ajuste_directo <- ajustar_especificacion(y, catalogo$media)
stopifnot(
  is.null(ajuste_directo$superficie),
  is.finite(ajuste_directo$mse)
)

stopifnot(
  inherits(try(optimizar(y, ajustar_mm, data.frame(k = character())), silent = TRUE),
           "try-error"),
  inherits(try(crear_catalogo_metodos(1), silent = TRUE), "try-error")
)

t <- seq_len(60)
y_reg <- 5 + 0.4 * t + rep(c(-1.2, 0.4, 0.8), length.out = length(t))
modelo_reg <- ajustar_tendencia(y_reg, "lineal")
diagnostico <- diagnosticar_regresion(modelo_reg, m = 10)
stopifnot(
  diagnostico$escala == "original",
  diagnostico$ljung_box$grados_libertad == 10L,
  diagnostico$jarque_bera$grados_libertad == 2L,
  isTRUE(all.equal(
    diagnostico$durbin_watson$estadistico,
    sum(diff(diagnostico$residuos)^2) / sum(diagnostico$residuos^2)
  )),
  isTRUE(all.equal(
    diagnostico$durbin_watson$aproximacion,
    2 * (1 - diagnostico$durbin_watson$r1)
  ))
)

modelo_exp <- ajustar_tendencia(exp(1 + 0.02 * t + 0.01 * sin(t)), "exponencial")
stopifnot(diagnosticar_regresion(modelo_exp, m = 10)$escala == "logaritmica")

errores_om <- origen_movil(1:10, n0 = 5, H = 3, ajustar = ajustar_ingenuo)
metricas_om <- medidas(errores_om, c(1, 3))
errores_prueba <- c(-2, -1, 0, 1, 3)
validacion_om <- validar_errores(errores_prueba, m = 1L)
datos_prueba <- tibble::tibble(
  fecha = seq(as.Date("2026-01-01"), by = "day", length.out = 10),
  y = 1:10
)
attr(datos_prueba, "frecuencia") <- 365
attr(datos_prueba, "unidad") <- "unidades"
final_prueba <- pronostico_final(
  datos_prueba, H = 3L, ajustar = ajustar_ingenuo,
  errores_origen = errores_om
)
stopifnot(
  nrow(errores_om) == 12L,
  sum(errores_om$horizonte == 1L) == 5L,
  sum(errores_om$horizonte == 3L) == 3L,
  all(errores_om$error[errores_om$horizonte == 1L] == 1),
  all(errores_om$error[errores_om$horizonte == 3L] == 3),
  metricas_om$n_errores[metricas_om$horizonte == 1L] == 5L,
  metricas_om$RMSE[metricas_om$horizonte == 1L] == 1,
  metricas_om$MAD[metricas_om$horizonte == 3L] == 3,
  inherits(try(origen_movil(1:10, 10, 1, ajustar_ingenuo), silent = TRUE), "try-error"),
  inherits(try(origen_movil(1:10, 5, 6, ajustar_ingenuo), silent = TRUE), "try-error"),
  validacion_om$n == 5L,
  validacion_om$m == 1L,
  validacion_om$media_cero$grados_libertad == 4L,
  isTRUE(all.equal(
    validacion_om$media_cero$estadistico,
    mean(errores_prueba) / (sd(errores_prueba) / sqrt(5))
  )),
  inherits(graficar_errores(errores_om[errores_om$horizonte == 1L, ]), "ggplot"),
  identical(final_prueba$tabla$pronostico, rep(10, 3)),
  identical(final_prueba$tabla$fecha,
            as.Date(c("2026-01-11", "2026-01-12", "2026-01-13"))),
  all(is.finite(final_prueba$tabla$limite_inferior)),
  inherits(graficar_pronostico_final(datos_prueba, final_prueba$tabla), "ggplot")
)

message("Optimizacion y asignacion: todas las pruebas finalizaron correctamente.")
