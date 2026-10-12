source("R/04-metodos.R")

verificar_contrato <- function(modelo, y, h = 4L) {
  stopifnot(
    is.list(modelo),
    identical(sort(names(modelo)), sort(c("yhat", "pronosticar", "parametros"))),
    length(modelo$yhat) == length(y),
    is.function(modelo$pronosticar),
    length(modelo$pronosticar(h)) == h,
    all(is.finite(modelo$pronosticar(h))),
    is.list(modelo$parametros)
  )
}

y <- as.numeric(1:12)

media <- ajustar_media(y)
verificar_contrato(media, y)
stopifnot(
  is.na(media$yhat[1]),
  isTRUE(all.equal(media$yhat[2:12], cumsum(y)[1:11] / 1:11)),
  identical(media$pronosticar(3), rep(mean(y), 3))
)

mm <- ajustar_mm(y, 3)
verificar_contrato(mm, y)
stopifnot(
  all(is.na(mm$yhat[1:3])),
  mm$yhat[4] == mean(1:3),
  identical(mm$pronosticar(3), rep(mean(10:12), 3))
)

ses <- ajustar_ses(y, 0.5)
verificar_contrato(ses, y)
stopifnot(
  ses$yhat[2] == 1,
  ses$yhat[3] == 1.5,
  length(unique(ses$pronosticar(4))) == 1L
)

dmm <- ajustar_dmm(y, 3)
verificar_contrato(dmm, y)
stopifnot(
  all(is.na(dmm$yhat[1:5])),
  isTRUE(all.equal(dmm$yhat[6:12], y[6:12])),
  isTRUE(all.equal(dmm$pronosticar(4), 13:16))
)

t <- seq_len(20)
lineal <- ajustar_tendencia(3 + 2 * t, "lineal")
cuadratica <- ajustar_tendencia(1 + 0.5 * t + 0.2 * t^2, "cuadratica")
exponencial <- ajustar_tendencia(exp(1 + 0.1 * t), "exponencial")

for (modelo in list(lineal, cuadratica, exponencial)) verificar_contrato(modelo, t)
stopifnot(
  isTRUE(all.equal(lineal$pronosticar(3), 3 + 2 * (21:23), tolerance = 1e-10)),
  isTRUE(all.equal(cuadratica$pronosticar(3), 1 + 0.5 * (21:23) + 0.2 * (21:23)^2,
                   tolerance = 1e-9)),
  isTRUE(all.equal(exponencial$pronosticar(3), exp(1 + 0.1 * (21:23)),
                   tolerance = 1e-10))
)

y_ruido <- 3 + 0.4 * t + rep(c(-0.8, 0.2, 0.5, -0.1), length.out = length(t))
regresion <- ajustar_tendencia(y_ruido, "lineal")
tabla <- regresion$parametros$coeficientes
stopifnot(
  isTRUE(all.equal(tabla$t_robusto, tabla$estimacion / tabla$se_hac)),
  all(tabla$grados_libertad == length(y_ruido) - 2L),
  all(tabla$valor_critico == qt(0.975, df = length(y_ruido) - 2L)),
  identical(tabla$decision,
            ifelse(tabla$p_valor_robusto < 0.05, "Rechazar H0", "No rechazar H0"))
)

holt <- ajustar_holt(rep(5, 12), 0.3, 0.2)
verificar_contrato(holt, rep(5, 12))
stopifnot(
  all(holt$yhat[2:12] == 5),
  all(holt$pronosticar(4) == 5)
)

modelos <- list(media, mm, ses, dmm, lineal, cuadratica, exponencial, holt)
stopifnot(all(vapply(modelos, function(modelo) {
  is.finite(mse_un_paso(modelo, if (length(modelo$yhat) == length(y)) y else 3 + 2 * t))
}, logical(1))))

stopifnot(
  inherits(try(ajustar_mm(y, 2.5), silent = TRUE), "try-error"),
  inherits(try(ajustar_ses(y, 1), silent = TRUE), "try-error"),
  inherits(try(ajustar_holt(y, 0.3, 0), silent = TRUE), "try-error"),
  inherits(try(ajustar_tendencia(c(1, 2, 0, 4, 5), "exponencial"), silent = TRUE),
           "try-error")
)

message("Catalogo reciclable: todas las pruebas finalizaron correctamente.")
