source("R/04-metodos.R")
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
  all(vapply(names(catalogo), function(id) {
    any(asignacion$metodo_id == id & asignacion$decision)
  }, logical(1))),
  all(vapply(unique(asignacion$serie), function(serie) {
    any(asignacion$serie == serie & asignacion$rol == "Contraste")
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

message("Optimizacion y asignacion: todas las pruebas finalizaron correctamente.")
