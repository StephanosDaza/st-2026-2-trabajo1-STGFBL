source("R/04-metodos.R")

verificar_contrato <- function(modelo, y, h = 8L) {
  stopifnot(
    identical(sort(names(modelo)), sort(c("yhat", "pronosticar", "parametros"))),
    length(modelo$yhat) == length(y),
    is.function(modelo$pronosticar),
    length(modelo$pronosticar(h)) == h,
    all(is.finite(modelo$pronosticar(h))),
    is.list(modelo$parametros)
  )
}

y <- as.numeric(1:16)
ingenuo <- ajustar_ingenuo(y)
ingenuo_estacional <- ajustar_ingenuo_estacional(y, 4)
verificar_contrato(ingenuo, y)
verificar_contrato(ingenuo_estacional, y)
stopifnot(
  is.na(ingenuo$yhat[1]),
  identical(ingenuo$yhat[2:16], y[1:15]),
  identical(ingenuo$pronosticar(3), rep(16, 3)),
  all(is.na(ingenuo_estacional$yhat[1:4])),
  identical(ingenuo_estacional$yhat[5:16], y[1:12]),
  isTRUE(all.equal(ingenuo_estacional$pronosticar(6), c(13:16, 13:14)))
)

s <- 4L
t <- seq_len(32)
efectos <- c(-3, -1, 1, 3)
y_te <- 10 + 0.5 * t + efectos[(t - 1L) %% s + 1L]
tendencia_estacional <- ajustar_tendencia_estacional(y_te, s)
verificar_contrato(tendencia_estacional, y_te)
stopifnot(
  isTRUE(all.equal(sum(tendencia_estacional$parametros$efectos_estacionales), 0,
                   tolerance = 1e-12)),
  isTRUE(all.equal(tendencia_estacional$pronosticar(8),
                   10 + 0.5 * (33:40) + efectos[((33:40) - 1L) %% s + 1L],
                   tolerance = 1e-9)),
  all(is.na(tendencia_estacional$yhat[1:(2 * s)])),
  all(is.finite(tendencia_estacional$yhat[(2 * s + 1L):length(y_te)]))
)

y_aditivo <- 30 + 0.4 * t + rep(c(-4, -1, 2, 3), length.out = length(t))
hw_aditivo <- ajustar_hw(y_aditivo, s, 0.3, 0.2, 0.4)
verificar_contrato(hw_aditivo, y_aditivo)
stopifnot(
  all(is.na(hw_aditivo$yhat[1:(2 * s)])),
  all(is.finite(hw_aditivo$yhat[(2 * s + 1L):length(y_aditivo)])),
  isTRUE(all.equal(mean(hw_aditivo$parametros$estacional_inicial), 0,
                   tolerance = 1e-12))
)

y_multiplicativo <- (40 + 0.5 * t) * rep(c(0.8, 0.95, 1.05, 1.2),
                                         length.out = length(t))
hw_multiplicativo <- ajustar_hw_mult(y_multiplicativo, s, 0.3, 0.2, 0.4)
verificar_contrato(hw_multiplicativo, y_multiplicativo)
stopifnot(
  all(is.na(hw_multiplicativo$yhat[1:(2 * s)])),
  all(is.finite(hw_multiplicativo$yhat[(2 * s + 1L):length(y_multiplicativo)])),
  isTRUE(all.equal(mean(hw_multiplicativo$parametros$estacional_inicial), 1,
                   tolerance = 1e-12)),
  all(hw_multiplicativo$pronosticar(8) > 0)
)

stopifnot(
  is.finite(mse_un_paso(ingenuo, y)),
  is.finite(mse_un_paso(ingenuo_estacional, y)),
  is.finite(mse_un_paso(tendencia_estacional, y_te)),
  is.finite(mse_un_paso(hw_aditivo, y_aditivo)),
  is.finite(mse_un_paso(hw_multiplicativo, y_multiplicativo))
)

stopifnot(
  inherits(try(ajustar_ingenuo_estacional(y, 1), silent = TRUE), "try-error"),
  inherits(try(ajustar_tendencia_estacional(y[1:7], 4), silent = TRUE), "try-error"),
  inherits(try(ajustar_hw(y[1:7], 4, 0.3, 0.2, 0.4), silent = TRUE), "try-error"),
  inherits(try(ajustar_hw_mult(c(rep(1, 15), 0), 4, 0.3, 0.2, 0.4), silent = TRUE),
           "try-error")
)

message("Metodos faltantes: todas las pruebas finalizaron correctamente.")
